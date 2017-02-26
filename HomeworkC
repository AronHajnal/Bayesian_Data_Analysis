get_likelihood <- function(flips, prior){
  pseq = 0
  k = sum(flips)
  for(p in prior){
    pseq = pseq + choose(8,k)*p^k*(1-p)^(8-k)/length(prior)
  }
  return(pseq)
}

#The get_likelihood function calculates the probability of a given sequence,
#given a prior distribution on the paramater space of a model. In both of our models the
#prior is a uniform distribution and the likelihood function is the binomial distribution
#i.e. the probability of observing k heads given the model parameter.

priorA = c(0.5,0.7)

priorB = seq(0.1,0.9,0.1)

flips1 = c(0,1,0,0,1,0,0)

flips2 = c(0,1,1,1,0,1,1,1)

pflips1A = get_likelihood(flips1, priorA)

pflips1B = get_likelihood(flips1, priorB)

pflips2A = get_likelihood(flips2, priorA)

pflips2B = get_likelihood(flips2, priorB)

bayes_factor = (pflips1A*pflips2A)/(pflips1B*pflips2B)

#As we observe two independent sequences, we can calculate
#the probability of sequence 1 AND sequence 2 happening by multiplying
#the respective probabilities.
