############################################################################## #
# Filename
#    01_GCE_algorithm.R
#
# Description
#   GCE algorithm
#
# Project   OEROK_Evaluierung und Dekomposition
# Author(s) Simon Sarcletti
# Date      20xx-xx-xx
#
# Copyright JOANNEUM RESEARCH, 2025
############################################################################## #


#' Generate Support Vectors for Each Element in a Matrix with Middle Element Preserved
#'
#' For every element in matrix A, this function generates M candidate values (support vectors)
#' by drawing M-1 random numbers uniformly from an interval around the element and ensuring
#' the original element is included as the middle value. The interval is defined as:
#' \[ \left[A[i,j] - 0.1 \times A[i,j], \; A[i,j] + 0.1 \times A[i,j]\right] \]
#' The sampled values are then rounded to one decimal place.
#'
#' @param A A numeric matrix.
#' @param M An integer specifying the number of support vectors (default is 5).
#'
#' @return A 3-dimensional array with dimensions (nrow(A), ncol(A), M) containing the support vectors,
#'         where the middle element of the support vectors for each element in A is the
#'         original element itself.
generate_support_vectors <- function(A, M = 5) {
  T <- nrow(A)
  K <- ncol(A)
  support_vectors <- array(0, dim = c(T, K, M))
  
  for (i in 1:T) {
    for (j in 1:K) {
      original_value <- A[i, j]
      lower_bound    <- original_value - 0.1 * original_value
      upper_bound    <- original_value + 0.1 * original_value
      
      # how many below vs above
      n_rand <- M - 1
      n_below <- floor(n_rand / 2)
      n_above <- n_rand - n_below
      
      # generate them separately
      vals_below <- if (n_below > 0) round(runif(n_below, lower_bound, original_value), 1) else numeric(0)
      vals_above <- if (n_above > 0) round(runif(n_above, original_value, upper_bound), 1) else numeric(0)
      
      # assemble support vector, placing original in the middle
      sv <- numeric(M)
      mid <- ceiling(M / 2)
      sv[mid] <- original_value
      
      # fill below
      if (n_below > 0) {
        idx_below <- seq_len(n_below)
        sv[idx_below] <- vals_below
      }
      # fill above
      if (n_above > 0) {
        idx_above <- (M - n_above + 1):M
        sv[idx_above] <- vals_above
      }
      
      support_vectors[i, j, ] <- sv
    }
  }
  support_vectors
}


#' Initialize Probability Distribution Over Support Vectors
#'
#' This function initializes a probability distribution for each element of matrix A over its
#' M support vectors. It supports two types of priors:
#' - 'uniform': All candidates have equal probability (1/M).
#' - 'spike': The candidate at the middle index gets a high probability (0.95) and the rest of the probability (0.05)
#'            is equally distributed over the other candidates.
#'
#' @param T Number of rows of matrix A.
#' @param K Number of columns of matrix A.
#' @param M Number of support vectors per cell.
#' @param prior A character string specifying the prior type ('uniform' or 'spike').
#'
#' @return A 3-dimensional array (T x K x M) with the initial probabilities.
initialize_probabilities <- function(T, K, M, prior = 'uniform') {
  if (M == 1) {
    return(array(1, dim = c(T, K, M)))    
  } else if (prior == 'uniform') {
    return(array(1 / M, dim = c(T, K, M)))
  } else if (prior == 'spike') {
    spike_prob <- 0.60
    non_spike_prob <- (1 - spike_prob) / (M - 1)
    # Create an array filled with the non-spike probability.
    probs <- array(non_spike_prob, dim = c(T, K, M))
    # Assume the "middle" candidate is at index ceiling(M/2)
    mid <- ceiling(M / 2)
    probs[, , mid] <- spike_prob
    return(probs)
  } else {
    stop("Invalid prior specification")
  }
}



#' Compute the KL Divergence Objective Function
#'
#' This function computes the Kullback-Leibler divergence between the current probability vector p and
#' the prior distribution Q.
#'
#' @param p A numeric vector representing the current probabilities (flattened).
#' @param Q A numeric vector representing the prior probabilities (flattened).
#'
#' @return A numeric value of the KL divergence.
objective_function <- function(p, Q) {
  p <- pmax(p, 1e-10)  # Avoid taking log(0)
  return(sum(p * log(p / Q)))
}


#' Evaluate the Gradient of the Objective Function
#'
#' This function computes the gradient (first derivative) of the objective function
#' with respect to p. The derivative is given by log(p / Q) + 1.
#'
#' @param p A numeric vector representing the current probabilities.
#'
#' @return A numeric vector representing the gradient.
eval_grad_f <- function(p) {
  p <- pmax(p, 1e-10)
  # Q is assumed to be defined in the global environment (set in balance_matrix)
  grad <- log(p / as.vector(Q)) + 1
  return(grad)
}


#' Balance a Matrix with Row and Column Constraints Using Nonlinear Optimization
#'
#' This function balances the matrix A by adjusting probabilities over candidate support values,
#' so that the weighted combination of the support vectors produces a matrix that satisfies:
#' - The row sums equal the target vector u.
#' - The column sums lie within the specified lower (v_lower) and upper (v_upper) bounds.
#' - For each cell, the probabilities over the M support vectors sum to 1.
#'
#' @param A A numeric matrix whose cells are to be adjusted.
#' @param u A numeric vector specifying the exact target row sums.
#' @param v_lower A numeric vector specifying the lower bounds for the column sums.
#' @param v_upper A numeric vector specifying the upper bounds for the column sums.
#' @param support_vectors A 3-dimensional array (nrow(A) x ncol(A) x M) containing candidate values.
#' @param M An integer for the number of support vectors (should match the third dimension of support_vectors).
#' @param prior A character specifying the prior type for the probabilities ('uniform' or 'spike').
#'
#' @return A list with two elements:
#'   \item{X_estimated}{The balanced matrix produced as a weighted sum of support vectors.}
#'   \item{P_estimated}{The optimized probabilities (as a T x K x M array).}
balance_matrix <- function(A, u, v_lower, v_upper, support_vectors, M = 15, prior = 'uniform') {
  T <- nrow(A)
  K <- ncol(A)
  
  # Initialize the prior probability distribution over the support vectors.
  # Assign Q globally so that it is available in eval_grad_f.
  Q <<- initialize_probabilities(T, K, M, prior)
  
  # Vectorize the initial probability distribution.
  P_init <- as.vector(Q)
  
  
  # --- Equality Constraints ---
  
  # Row constraints: The weighted sums across each row must equal the target row sums.
  row_constraints <- function(p) {
    P_matrix <- array(p, dim = c(T, K, M))
    # Calculate the estimated matrix by summing over the support vectors weighted by their probabilities.
    X_est <- apply(P_matrix * support_vectors, c(1, 2), sum)
    return(as.vector(rowSums(X_est) - u))
  }
  
  # Probability constraints: For each cell, the sum of probabilities over support vectors must equal 1.
  probability_constraints <- function(p) {
    P_matrix <- array(p, dim = c(T, K, M))
    prob_sum <- rowSums(P_matrix, dims = 2)
    return(as.vector(prob_sum - 1))
  }
  
  # Combine both sets of equality constraints.
  eval_g_eq <- function(p) {
    return(c(row_constraints(p), probability_constraints(p)))
  }
  
  
  # --- Jacobian for Equality Constraints ---
  # The Jacobian matrix has (T + T*K) rows and T*K*M columns.
  eval_jac_g_eq <- function(p) {
    nvar <- length(p)        # Total number of variables: T * K * M
    neq <- T + T * K         # Total number of equality constraints
    jac_eq <- matrix(0, nrow = neq, ncol = nvar)
    
    # For row constraints: derivative with respect to p_{ijm} is support_vectors[i,j,m].
    for (m in 1:M) {
      for (j in 1:K) {
        for (i in 1:T) {
          col_index <- (m - 1) * (T * K) + (j - 1) * T + i
          row_index <- i  # Row constraint for row i.
          jac_eq[row_index, col_index] <- support_vectors[i, j, m]
        }
      }
    }
    
    # For probability constraints: derivative with respect to p_{ijm} is 1.
    for (j in 1:K) {
      for (i in 1:T) {
        eq_index <- T + (j - 1) * T + i  # Constraint for cell (i,j)
        for (m in 1:M) {
          col_index <- (m - 1) * (T * K) + (j - 1) * T + i
          jac_eq[eq_index, col_index] <- 1
        }
      }
    }
    return(jac_eq)
  }
  
  
  # --- Inequality Constraints (Column Sum Bounds) ---
  # For each column j:
  #   g_lower_j(p) = v_lower[j] - (column j sum) <= 0, and
  #   g_upper_j(p) = (column j sum) - v_upper[j] <= 0.
  col_constraints_ineq <- function(p) {
    P_matrix <- array(p, dim = c(T, K, M))
    X_est <- apply(P_matrix * support_vectors, c(1, 2), sum)
    col_sums <- colSums(X_est)
    lower_diff <- v_lower - col_sums  # Should be <= 0.
    upper_diff <- col_sums - v_upper  # Should be <= 0.
    return(c(lower_diff, upper_diff))
  }
  
  # Jacobian for inequality constraints.
  eval_jac_g_ineq <- function(p) {
    nvar <- length(p)
    jac <- matrix(0, nrow = 2 * K, ncol = nvar)
    # The vectorization order is: for (m in 1:M) for (j in 1:K) for (i in 1:T)
    for (m in 1:M) {
      for (j in 1:K) {
        for (i in 1:T) {
          col_index <- (m - 1) * (T * K) + (j - 1) * T + i
          # For the lower bound constraint, derivative is -support_vectors[i,j,m]
          jac[j, col_index] <- -support_vectors[i, j, m]
          # For the upper bound constraint, derivative is +support_vectors[i,j,m]
          jac[K + j, col_index] <- support_vectors[i, j, m]
        }
      }
    }
    return(jac)
  }
  
  # Wrap the inequality constraints.
  eval_g_ineq <- function(p) {
    return(col_constraints_ineq(p))
  }
  
  
  # --- Objective Function Wrapper ---
  f_obj <- function(p) {
    return(objective_function(p, as.vector(Q)))
  }
  
  
  # --- Optimization Call ---
  result <- nloptr(x0 = P_init, 
                   eval_f = f_obj,
                   eval_grad_f = eval_grad_f,
                   eval_g_eq = eval_g_eq,
                   eval_jac_g_eq = eval_jac_g_eq,
                   eval_g_ineq = eval_g_ineq,
                   eval_jac_g_ineq = eval_jac_g_ineq,
                   opts = list("algorithm" = "NLOPT_LD_SLSQP", 
                               "print_level" = 0,
                               "xtol_rel" = 1.0e-1,
                               #"ftol_abs" = 1.0e-6,
                               "maxeval" = 10000))
  
  if (result$status < 0) {
    stop("Optimization did not converge: ", result$message)
  }
  
  # Reshape the optimized probabilities into a T x K x M array.
  P_opt <- array(result$solution, dim = c(T, K, M))
  # Compute the balanced matrix as the weighted sum of the support vectors.
  X_est <- apply(P_opt * support_vectors, c(1, 2), sum)
  
  return(list(X_estimated = X_est, P_estimated = P_opt))
}

