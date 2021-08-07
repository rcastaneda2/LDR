
# n = number of persons
# p = number of items
# a = IRT slope
# b = IRT b
# g = IRT c (guessing)
# theta = theta values

#data generating functions
generate<-function(n,p,a,b,g,theta){
  responses = matrix(0, nrow=n, ncol=p)
  for(i in 1:n) {
    for(j in 1:p) {
      prob =g[j]+(1-g[j])*((exp(a[j]*(theta[i]-b[j])))/(1+exp(a[j]*(theta[i]-b[j]))))
      responses[i,j] = (runif(1) < prob)
    }
  }
  return(responses)
}



# n = number of persons
# p = number of items
# rho = LD strength
# LD = 'first' only the first two items have LD
# LD = 'cluster' the first n items have LD with each other


#Previously called genitems
item_sim<-function(n,p,rho,LD='first'){

  if (LD=='cluster'){
            p1=floor(p*.2)
            p2=ceiling(p*.8)

  }else if (LD=='first'){
        #Only first two have LD
        p1=p-(p-2)
        p2=p-p1

    }else( print ("ERROR!!"))

  #sample n number of times from normal distribution
  x1=rnorm(n)
  x2=rnorm(n)

  mu1=0
  mu2=0
  s1=1
  s2=1
  rho=rho

  #Generates correlated thetas
  theta1=mu1+s1*x1
  theta2=mu2+s2*((rho*x1)+sqrt(1-rho^2)*x2)


  #FIRST SET OF ITEMS **********************
  a1=rlnorm(p1,0,.5)
  b1=rnorm(p1)
  g1=rlogitnorm(mu=-1.1,sigma=.5,p1)
  responses1=generate(n,p1,a1,b1,g1,theta1)

  #SECOND SET OF ITEMS **********************
  a2=rlnorm(p2,0,.5)
  b2=rnorm(p2)
  g2=rlogitnorm(mu=-1.1,sigma=.5,p2)

  responses2=generate(n,p2,a2,b2,g2,theta2)
  responses=cbind(responses1,responses2)

  #Calculate IRT Parameters of generated data************

      dat<-responses #[sample(1:n, 1000, replace=TRUE), ]
      res=mirt(dat,1,itemtype='2PL',optimizer='L-BFGS-B',verbose=FALSE)
      temp3=residuals(res,type="Q3",verbose=FALSE)
      theta=fscores(res,full.scores=TRUE)

      sample1=4000
      sample2=1000
      sample3=800
      sample4=500
      sample5=200

      #temp4=est.q3(dat,res,theta,sample)
      samp1=est.q3(dat,res,theta,sample1)
      samp2=est.q3(dat,res,theta,sample2)
      samp3=est.q3(dat,res,theta,sample3)
      samp4=est.q3(dat,res,theta,sample4)
      samp5=est.q3(dat,res,theta,sample5)

      q3=matrix(temp3,ncol=p,nrow=p)


      #Set up matrix of Q3 to use in meta-analysis
      newq3=matrix(c(q3[upper.tri(q3,diag=FALSE)]),nrow=1)
      newq3=as.vector(newq3)

      ####Sample 1
      q3.boot1=matrix(samp1,ncol=p,nrow=p)
      newq3.boot1=matrix(c(q3.boot1[upper.tri(q3.boot1,diag=FALSE)]),nrow=1)
      newq3.boot1=as.vector(newq3.boot1)
      ####Sample 2
      q3.boot2=matrix(samp2,ncol=p,nrow=p)
      newq3.boot2=matrix(c(q3.boot2[upper.tri(q3.boot2,diag=FALSE)]),nrow=1)
      newq3.boot2=as.vector(newq3.boot2)
      ####Sample 3
      q3.boot3=matrix(samp3,ncol=p,nrow=p)
      newq3.boot3=matrix(c(q3.boot3[upper.tri(q3.boot3,diag=FALSE)]),nrow=1)
      newq3.boot3=as.vector(newq3.boot3)
      ####Sample 4
      q3.boot4=matrix(samp4,ncol=p,nrow=p)
      newq3.boot4=matrix(c(q3.boot4[upper.tri(q3.boot4,diag=FALSE)]),nrow=1)
      newq3.boot4=as.vector(newq3.boot4)
      ####Sample 5
      q3.boot5=matrix(samp5,ncol=p,nrow=p)
      newq3.boot5=matrix(c(q3.boot5[upper.tri(q3.boot5,diag=FALSE)]),nrow=1)
      newq3.boot5=as.vector(newq3.boot5)


  final=list(dat=dat,q3=newq3,q3.boot1=newq3.boot1,q3.boot2=newq3.boot2,q3.boot3=newq3.boot3,q3.boot4=newq3.boot4,q3.boot5=newq3.boot5)
  return(final)

}

#debug(genitems)
#debug(genitems)

#gen1<-genitems(4000,12,.2,LD="first")



