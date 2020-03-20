data {
  int<lower=0> N;
  int<lower=0> dia0;
  int<lower=0> pop;
  vector[N] dia;
  vector[N] defs;
}

parameters {
  real<lower = 0, upper = 1000> casos_0; 
  vector<lower = 0>[N] contagios;
  real<lower = 0, upper = 3> r0;  //priori cutre sobre r0, infectados diarios que genera un caso
  real<lower = 0, upper = .1> letalidad;
  real<lower = 0> sigma_contagios;
  real<lower = 0> sigma_defs;
}

model {
  vector[N] casos;
  real contagiadores;
  
  contagios[1] ~ normal(casos_0 * r0, sigma_contagios);
  casos[1] = casos_0 + contagios[1];
  
  // TODO: add upper limit in the number of cases given the population size
  for (i in 2:N) {
    
    contagiadores = 0;
    for (j in max(1, i - 27):i) {
      contagiadores = contagiadores + contagios[j];
    }
    
    contagios[i] ~ normal((1 - casos[i-1] / pop) * contagiadores * r0, sigma_contagios);
    casos[i] = casos[i-1] + contagios[i];
  }
  
  // only people catching coronavirus 22 days ago can die
  for (i in dia0:N) {
    defs[i] ~ normal(letalidad * contagios[i - 22], sigma_defs);
  }
}

