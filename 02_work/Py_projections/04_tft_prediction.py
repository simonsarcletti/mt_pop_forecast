import torch

# Check if CUDA is available (for NVIDIA GPUs)
cuda_available = torch.cuda.is_available()
print(f"CUDA Available: {cuda_available}")

device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
print(f"Using device: {device}")

if cuda_available:
    # Get the name of the current GPU
    gpu_name = torch.cuda.get_device_name(0)
    print(f"GPU Name: {gpu_name}")

    # Get the number of available GPUs
    gpu_count = torch.cuda.device_count()
    print(f"Number of GPUs: {gpu_count}")

    # Check CUDA version
    cuda_version = torch.version.cuda
    print(f"CUDA Version: {cuda_version}")

import copy
from pathlib import Path
import warnings
from pyreadr import read_r

import lightning.pytorch as pl
from lightning.pytorch.callbacks import EarlyStopping, LearningRateMonitor
from lightning.pytorch.loggers import TensorBoardLogger
import numpy as np
import pandas as pd
import torch

np.Inf = np.inf


from pytorch_forecasting import Baseline, TemporalFusionTransformer, TimeSeriesDataSet
from pytorch_forecasting.data import EncoderNormalizer
from pytorch_forecasting.metrics import MAE, SMAPE, PoissonLoss, QuantileLoss
from pytorch_forecasting.models.temporal_fusion_transformer.tuning import (
    optimize_hyperparameters,
)

data_path = r"/data/simon/"
#data_path = r"C:\Users\simon.sarcletti\OneDrive - FH JOANNEUM\FH Joanneum - DAT\XX_Masterarbeit\05_Empirical_work\01_data\02_work"

all_munip_pop = read_r(data_path + r"all_municipalities_population.RData")[
    "all_munip_pop"
]

all_munip_pop["municipality_code"] = all_munip_pop["municipality_code"].astype("int64")

static_data_path = r"/data/simon/"
#static_data_path = r"C:\Users\simon.sarcletti\OneDrive - FH JOANNEUM\FH Joanneum - DAT\XX_Masterarbeit\05_Empirical_work\01_data\01_original"

static_metadata = pd.read_csv(
    static_data_path + r"static_variables.csv",
    encoding="latin-1",
    sep=";",
    decimal=",",
)
merged_data = pd.merge(
    all_munip_pop,
    static_metadata,
    how="left",
    left_on="municipality_code",
    right_on="ID",
)

# create an index col
merged_data["index"] = (
    merged_data["municipality_code"].astype(str)
    + "_"
    + merged_data["sex"].round(0).astype(str)
    + "_"
    + merged_data["coarse_age_group"]
)
# remove unnecessary columns
merged_data = merged_data.drop(
    columns=[
        "Name",
        "ID",
        "municipality_code",
        "reg_code",
        "municipality",
        "sex",
        "population",
    ]
) # maybe remove "Jahresbruttobezug_2023" as well

merged_data = merged_data[merged_data["year"] >= 2004].copy()

# create a new column with first three digits of index
merged_data["reg_code"] = merged_data["index"].str[:3]

merged_data = merged_data.rename(
    columns={"smoothed_population": "population", "coarse_age_group": "age_group"}
)
merged_data["year"] = pd.to_numeric(merged_data["year"], downcast="integer")

static_categoricals = ['Urban-Rural-Typologie',
       'klassifikation_palme95', 'OeV-Güteklassen', 'Bezirkshauptstadt',
       'schulen_ue250', 'umkreis_schulen', 'haltestelle_IbIII',
       'haltestelle_umkreis', 'autobahnauffahrt', 'autobahnauffahrt_umkreis','umkreis_einpendler', 'reg_code',]

static_reals = ['Index_Pendlersaldos_2022','anteil_ue75_2014',
       'anteil_ue75_2024', 'durchschnittsalter', 'Jahresbruttobezug_2023',
       'anteil_frauen_1534_gesamtbevölkerung',
       'verkehrsleistung_personenkilometer_energiemosaik',
       'handelsgebaeude_1000ew_gwr', 'kulturgebaeude_1000ew_gwr',]

for col in static_categoricals:
    merged_data[col] = merged_data[col].astype(str)


max_prediction_length = 5
max_encoder_length = 18
training_cutoff = 2019  # training data ends in 2019, validation starts in 2020

training = TimeSeriesDataSet(
    merged_data[lambda x: x.year <= training_cutoff],
    time_idx="year",
    target="population",
    group_ids=["index"],
    min_encoder_length=max_encoder_length
    // 2,  # keep encoder length long (as it is in the validation set)
    max_encoder_length=max_encoder_length,
    min_prediction_length=5,
    max_prediction_length=max_prediction_length,
    static_categoricals=static_categoricals,
    static_reals =static_reals,
    time_varying_unknown_reals=[
        "population",
    ],
    target_normalizer=EncoderNormalizer(),
    add_relative_time_idx=True,
    add_target_scales=True,
    add_encoder_length=True,
)

validation = TimeSeriesDataSet.from_dataset(
    training,  # copy all the encoders/normalizers/etc.
    merged_data,
    predict=True,
    stop_randomization=True,
    min_prediction_idx=2020,  # prediction windows begin in 2019
    max_prediction_length=5,  # prediction windows are 5 years long
)

batch_size = 64  # set this between 32 to 128
train_dataloader = training.to_dataloader(
    train=True, batch_size=batch_size, num_workers=8
)
val_dataloader = validation.to_dataloader(
    train=False, batch_size=batch_size, num_workers=8
)

# configure network and trainer
pl.seed_everything(42)
trainer = pl.Trainer(
    accelerator="gpu",
    # clipping gradients is a hyperparameter and important to prevent divergance
    # of the gradient for recurrent neural networks
    gradient_clip_val=0.1,
)


tft = TemporalFusionTransformer.from_dataset(
    training,
    # not meaningful for finding the learning rate but otherwise very important
    learning_rate=0.03,
    hidden_size=8,  # most important hyperparameter apart from learning rate
    attention_head_size=2, # number of attention heads. Set to up to 4 for large datasets
    dropout=0.1,  # between 0.1 and 0.3 are good values
    hidden_continuous_size=4,  # set to <= hidden_size
    loss=QuantileLoss(),
    optimizer="adam",
    # reduce learning rate if no improvement in validation loss after x epochs
    # reduce_on_plateau_patience=1000,
)
print(f"Number of parameters in network: {tft.size() / 1e3:.1f}k")

# configure network and trainer
early_stop_callback = EarlyStopping(
    monitor="val_loss", min_delta=1e-4, patience=10, verbose=False, mode="min"
)
lr_logger = LearningRateMonitor()  # log the learning rate
logger = TensorBoardLogger("lightning_logs")  # logging results to a tensorboard

trainer = pl.Trainer(
    max_epochs=50,
    accelerator="gpu",
    enable_model_summary=True,
    gradient_clip_val=0.1,
    limit_train_batches=50,  # coment in for training, running valiation every 30 batches
    # fast_dev_run=True,  # comment in to check that networkor dataset has no serious bugs
    callbacks=[lr_logger, early_stop_callback],
    logger=logger,
)

tft = TemporalFusionTransformer.from_dataset(
    training,
    learning_rate=0.03,
    hidden_size=16,
    attention_head_size=2,
    dropout=0.1,
    hidden_continuous_size=8,
    loss=QuantileLoss(),
    log_interval=10,  # uncomment for learning rate finder and otherwise, e.g. to 10 for logging every 10 batches
    optimizer="adam",
    reduce_on_plateau_patience=4,
)
print(f"Number of parameters in network: {tft.size() / 1e3:.1f}k")

# fit network
trainer.fit(
    tft,
    train_dataloaders=train_dataloader,
    val_dataloaders=val_dataloader,
)

import pickle

from pytorch_forecasting.models.temporal_fusion_transformer.tuning import (
    optimize_hyperparameters,
)

# create study
study = optimize_hyperparameters(
    train_dataloader,
    val_dataloader,
    model_path="/home/v18y97/mt_pop_forecast/tft_for_prediction_tuning",
    n_trials=200,
    max_epochs=50,
    gradient_clip_val_range=(0.01, 1.0),
    hidden_size_range=(8, 128),
    hidden_continuous_size_range=(8, 128),
    attention_head_size_range=(1, 4),
    learning_rate_range=(0.001, 0.1),
    dropout_range=(0.1, 0.3),
    trainer_kwargs=dict(limit_train_batches=30),
    reduce_on_plateau_patience=4,
    use_learning_rate_finder=True,  # use Optuna to find ideal learning rate or use in-built learning rate finder
)

# save study results - also we can resume tuning at a later point in time
with open("/home/v18y97/mt_pop_forecast/prediction_study.pkl", "wb") as fout:
    pickle.dump(study, fout)

# show best hyperparameters
print(study.best_trial.params)

# load the best model according to the validation loss
# (given that we use early stopping, this is not necessarily the last epoch)
best_model_path = trainer.checkpoint_callback.best_model_path
best_tft = TemporalFusionTransformer.load_from_checkpoint(best_model_path)

print(f"Best model path: {best_model_path}")


max_encoder_length = 25
max_prediction_length = 11

# 1) grab the last 24 months of history
encoder_data = merged_data[lambda df: df.year > df.year.max() - max_encoder_length]

last_data = merged_data[lambda x: x.year == x.year.max()]


decoder_data = pd.concat(
    [
        last_data.assign(year=lambda x, i=i: x['year'] + i)
        for i in range(1, max_prediction_length + 1)
    ],
    ignore_index=True,
)
# combine encoder and decoder data
new_prediction_data = pd.concat([encoder_data, decoder_data], ignore_index=True)

static_categoricals = ['Urban-Rural-Typologie',
       'klassifikation_palme95', 'OeV-Güteklassen', 'Bezirkshauptstadt',
       'schulen_ue250', 'umkreis_schulen', 'haltestelle_IbIII',
       'haltestelle_umkreis', 'autobahnauffahrt', 'autobahnauffahrt_umkreis','umkreis_einpendler', 'reg_code',]

static_reals = ['Index_Pendlersaldos_2022','anteil_ue75_2014',
       'anteil_ue75_2024', 'durchschnittsalter', 'Jahresbruttobezug_2023',
       'anteil_frauen_1534_gesamtbevölkerung',
       'verkehrsleistung_personenkilometer_energiemosaik',
       'handelsgebaeude_1000ew_gwr', 'kulturgebaeude_1000ew_gwr',]

for col in static_categoricals:
    new_prediction_data[col] = new_prediction_data[col].astype(str)

new_raw_predictions = best_tft.predict(
    new_prediction_data,
    mode="raw",
    return_x=True,
    return_index=True,
    trainer_kwargs=dict(accelerator="gpu"),
)

arr = new_raw_predictions.output.prediction.detach().cpu().numpy()

# Transpose if needed: ensure shape is (samples, steps, quantiles)
#if arr.shape[2] == 3:  # then it's (samples, quantiles, steps)
#    arr = arr.transpose(0, 2, 1)

n_samples, n_steps, n_quantiles = arr.shape  # (33840, 3, 7) if 3 years, 7 quantiles
# Repeat each sample index
original_index = np.repeat(new_raw_predictions.index["index"], n_quantiles * n_steps)

# Repeat quantiles and years
quantiles = ["0.01", "0.1", "0.25", "0.5", "0.75", "0.9", "0.99"]
quantile_column = np.tile(np.repeat(quantiles, n_steps), n_samples)
year_column = np.tile(list(range(2025, 2025 + n_steps)), n_samples * n_quantiles)

# Flatten prediction
prediction_column = arr.flatten()

# Build DataFrame
df_long = pd.DataFrame({
    "original_index": original_index,
    "quantile": quantile_column,
    "year": year_column,
    "prediction": prediction_column
})

df_long.to_csv("/home/v18y97/mt_pop_forecast/tft_prediction_2025-2035.csv", index=False)