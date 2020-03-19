data {
  int<lower=0> N;
  int<lower=0> dia0;
  vector[N] dia;
  vector[N] defs;
}

parameters {
  real<lower = 0, upper = 1000> casos_0; 
  vector[N] contagios;
  real<lower = 0, upper = 3> r0;  //priori cutre sobre r0, infectados diarios que genera un caso
  real<lower = 0, upper = .02> letalidad;
}

model {
  vector[N] casos;
  
  contagios[1] ~ normal(casos_0 * r0, 1);
  casos[1] = casos_0 + contagios[1];
  
  
  for (i in 2:N){
    contagios[i] ~ normal(casos[i-1] * r0, 1);
    casos[i] = casos[i-1] + contagios[i];
  }
  
  for (i in dia0:N) {
    defs[i] ~ normal(letalidad * casos[i - 22], 1);
  }
}

