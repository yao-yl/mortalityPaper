data {
  int<lower=0> N_age;
  int<lower=0> N_month;
  int cases[N_age,2,2,N_month];
  int exposures[N_age,2,2,N_month];
  int covid_start;
}


parameters {
  real age_eff[N_age];
  real month_eff[11];
  real edu_eff;
  real gender_eff;
  real covid_eff[N_age];
  real covid_male;
  real covid_edu;
  real<lower=0> sigma_age;
  real<lower=0> sigma_age2;
  real<lower=0> sigma_month;
}

transformed parameters {
  real time_eff[N_month];
  real rate[N_age,2,2,N_month];
  time_eff[1]=0;
  time_eff[2:12]=month_eff;
  time_eff[(12+1):N_month]=time_eff[1:(N_month-12)];
  for(i1 in 1:N_age)
    for(i2 in 1:2)
        for(i3 in 1:2)
          for(i4 in 1:N_month){
              rate[i1, i2, i3, i4] = age_eff[i1]+time_eff[i4];
              if(i2==2)
              rate[i1, i2, i3, i4] += gender_eff;
               if(i3==2)
              rate[i1, i2, i3, i4] += edu_eff;
              if(i4>=12+covid_start){
              rate[i1, i2, i3, i4] += covid_eff[i1];
               if(i2==2)
              rate[i1, i2, i3, i4] += covid_male;
               if(i3==2)
              rate[i1, i2, i3, i4] += covid_edu;
              }
          }
  
}


model {
  for(i1 in 1:N_age)
    for(i2 in 1:2)
        for(i3 in 1:2)
          for(i4 in 1:N_month)
              cases[i1, i2, i3, i4]~ binomial_logit(exposures[i1, i2, i3, i4],  rate[i1, i2, i3, i4] );
   for(i in 2:N_age)
      target+= -0.5*(  age_eff[i]-age_eff[i-1])^2/sigma_age;
   age_eff[1]~normal(-8,4);
   for(i in 2:N_age)
      target+= -0.5*(covid_eff[i]-covid_eff[i-1])^2/sigma_age2; 
   for(i in 2:11)
      target+= -0.5*(month_eff[i]-month_eff[i-1])^2/sigma_month;
   target+= -(0-month_eff[11])^2/sigma_month;   
   edu_eff~std_normal();
   gender_eff~std_normal();
   covid_eff~std_normal();
   sigma_month~normal(0,1);
   sigma_age~std_normal();
   sigma_age2~normal(0,1);
   covid_edu~normal(0,1);
   covid_male~normal(0,1);
}

