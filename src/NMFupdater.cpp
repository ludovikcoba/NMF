// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
#include <unordered_map>

using namespace Rcpp;
const int USER = 0;
const int ITEM = 1;
const int SCORE = 2;
const int NVL = 3;

template <class T>
inline int
  sgn(T v) {
    return (v > T(0)) - (v < T(0));
  }

// [[Rcpp::export]]
List NSVDupdater(
    NumericMatrix sparseRatingMat,
    double learningRate, 
    double regCoef,
    double regCoefNovelty,
    int nrfeat, // the total number of features.
    int steps,
    int reg // 1 MF, 2 L2 regulariztion, 3 L1 regularization
)
{
  
  int max_uID = -1;
  int max_iID = -1;
  
  for(int i =0; i < sparseRatingMat.nrow(); i++){
    if(sparseRatingMat(i,USER) > max_uID) max_uID = sparseRatingMat(i,USER);
    if(sparseRatingMat(i,ITEM) > max_iID) max_iID = sparseRatingMat(i,ITEM);
  }
  
  
  NumericMatrix U(max_uID, nrfeat);
  NumericMatrix V(max_iID, nrfeat);
  
  double error;
  
  for(int i = 0; i < max_uID; i++){
    for(int j = 0; j < nrfeat ; j++){
      U(i,j) = R::runif(0,1) * sqrt(0.5f/nrfeat);
    }
  }
  
  for(int i = 0; i <  max_iID; i++){
    for(int j = 0; j < nrfeat ; j++){
      V(i,j) = R::runif(0,1) * sqrt(0.5f/nrfeat);
    }
  }
  
  double eij, deltaUif, deltaVjf, pred, novelty_ij;
  NumericVector uID(max_uID), iID(max_iID);
  
  int i, j;
  
  for(int ss = 0; ss < steps; ss++){
    error = 0;
    //for every rating
    for(int k = 0; k < sparseRatingMat.nrow(); k++){
      
      pred = 0;

      //user index.
      i = (int) sparseRatingMat(k,USER) % max_uID;
      //item index.
      j = (int) sparseRatingMat(k,ITEM) % max_iID;
      
      uID(i) = sparseRatingMat(k,USER);
      iID(j) = sparseRatingMat(k,ITEM);
      
      novelty_ij = sparseRatingMat(k,NVL);
      
      //compute paiwise multiplication on the features.
      for(int l = 0; l < nrfeat; l++){
        pred += U(i,l) * V(j,l);
      }
      
      
      //compute error
      eij = sparseRatingMat(k,SCORE) - pred;
      
      error += abs(eij);
      
      for(int feat = 0; feat < nrfeat; feat++){
        
        //item feature 
        deltaVjf = 2 * eij * U(i,feat);
        deltaVjf -= regCoef * V(j,feat);
        
        deltaUif = 2 * eij * V(j,feat);
        deltaUif -= regCoef * U(i,feat);
        
        
        if(reg == 2){
          //expl & novelty on items' feature 
          deltaVjf -= (U(i,feat) - V(j,feat)) * regCoefNovelty * novelty_ij;
          //expl & novelty on users' feature 
          deltaUif -= (U(i,feat) - V(j,feat)) * regCoefNovelty * novelty_ij;

        }else if(reg == 3){
          
          //expl & novelty on item feature 
          deltaVjf -= sgn(U(i,feat) - V(j,feat)) * (regCoefNovelty * novelty_ij);
          
          //expl & novelty on users' feature  
          deltaUif -= sgn(U(i,feat) - V(j,feat)) * (regCoefNovelty * novelty_ij);
          
          
        }
        //update
        V(j,feat) += learningRate * (deltaVjf);
        U(i,feat) += learningRate * (deltaUif);
      }
      
    }
    
    if((ss % 10) == 0) {
      Rcout << "At step:" << ss << " the objective error is: "<< error <<"\n";
    }
  }
  
  
  
  
  List ret;
  ret["uID"] = uID;
  ret["U"] = U;
  ret["iID"] = iID;
  ret["V"] = V;
  
  
  return ret;
  
}




