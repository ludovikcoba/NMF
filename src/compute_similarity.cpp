#include <Rcpp.h>
#include <iostream>
#include <fstream>

using namespace Rcpp;

const int OBSERVATION = 0;
const int INTERACTION = 1;
const int WEIGHT = 2;

// [[Rcpp::export]]
DataFrame compute_similarity(
    NumericVector observation, 
    NumericVector interaction,
    NumericVector weight,
    int dim,
    int damp,
    int shrinkage
) {
  
  Rcpp::Rcout << "Computing similarity."<<std::endl;
  
  std::ofstream outfile;
  outfile.open("simil_rrecsys.csv");
 
  std::vector<int> A;
  std::vector<int> B;
  std::vector<float> simil;
  
  int num_rows = observation.size();
  int observation_u, observation_v, i, j;
  float s = 0, s_u = 0, s_v = 0;
  float d_factor;
  bool go_on = true;
  NumericVector row_pointer(dim+1);
  
  
  observation_u = -1;
  int cnt = 0;
  
  for(int l = 0; l < num_rows; l++){
    if(observation[l] != observation_u){
      row_pointer[cnt] = l;
      cnt++;
      observation_u = observation[l];
    }
  }
  
  row_pointer[dim] = - 1;
  
  observation_u = 0; //we set u to point to the first observation.
  observation_v = 1; //we set v to point to the the second observation.
  i = row_pointer[observation_u];// pointer on the scores of observation u;
  j = row_pointer[observation_v];// pointer on the scores of observation v;
  cnt = 0;
  
  
  
  while(go_on){
    
    if(interaction[i] == interaction[j]){
      s += (float) weight[i] * weight[j];
      s_u += (float) weight[i] * weight[i];
      s_v += (float) weight[j] * weight[j];
      i++;
      j++;
      cnt++;
    }else if(interaction[i] > interaction[j]){
      j++;
    }else if(interaction[i] < interaction[j]){
      i++;
    }
    
    if(j == row_pointer[observation_v + 1] || i == row_pointer[observation_u + 1] || j == num_rows){
      if((s_u != 0) && (s_v != 0)){
        //
        A.push_back(observation[row_pointer[observation_u]]);
        B.push_back(observation[row_pointer[observation_v]]);
        outfile << observation[row_pointer[observation_u]] << ";" << observation[row_pointer[observation_v]] << ";";
        
        if(damp != 0){
          d_factor = std::max(cnt,damp) /damp;
          simil.push_back( d_factor * s/sqrt(s_u * s_v) );
          outfile <<  d_factor * s/sqrt(s_u * s_v) << "\n";
        }
        else if(shrinkage != 0){
          d_factor = (cnt - 1);
          d_factor /= shrinkage + cnt - 1;
          simil.push_back(  d_factor * s/sqrt(s_u * s_v) );
          outfile << d_factor * s/sqrt(s_u * s_v) << "\n";
        }
        else
          {
          //in case damp is 0 we return the standard similarity measurement
          simil.push_back(s/sqrt(s_u * s_v));
          outfile << s/sqrt(s_u * s_v) << "\n";
        }
        
      }
      
      s = 0;
      s_u = 0;
      s_v = 0;
      cnt = 0;
      
      observation_u++; //we set u to point to the first observation.
      if(observation_u == observation_v){
        observation_u = 0;
        observation_v ++;
        if (observation_v % 10000 == 0){
          Rcpp::Rcout << "Progress = " << observation_v << " observations out of "<< dim << "." <<std::endl;
        }

        
      }
      
      i = row_pointer[observation_u];// pointer on the scores of observation u;
      j = row_pointer[observation_v];// pointer on the scores of observation v;
      
      if(j == -1) go_on = false;
      
    }
    
    
  }
  Rcpp::Rcout << "Similarity computed."<<std::endl;
  
  outfile.close();
  
  return DataFrame::create(_["A"]= A, _["B"]= B, _["sim"] = simil);;
}
