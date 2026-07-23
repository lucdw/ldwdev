#' Creates a pdf file of an R source file
#'
#' Creates a pdf file of an R source file e.g. to print on paper.
#'
#' @param source_file The path to the R file.
#' @param dest_file The path to the file to create.
#'                  If this file already exists it will be overwritten.
#' @param show_pdf Logical. If TRUE the PDF will be shown after completion.
#'
#' @return NULL (invisible).
#'
#' @examples
#' \dontrun{
#'    r2pdf("R/dev_util.R", "~/dev_util.pdf", TRUE)
#' }
#'
#' @author Luc De Wilde
#' @name r2pdf
#' @rdname r2pdf
#' @export
r2pdf <- function(source_file, dest_file, show_pdf = FALSE) {
  src <- normalizePath(source_file, mustWork = TRUE)
  dst <- normalizePath(dest_file, mustWork = FALSE)
  tmpfile <- tempfile(fileext = ".tex")
  tmp <- file(tmpfile, open = "wt")
  tmppdf <- gsub(".tex", ".pdf", basename(tmpfile))
  writeLines(c(                                                      # nolint start
  "\\documentclass[a4paper, twoside, openright, usecolor]{article}",
  "\\title{PDF from R-file.}",
  "\\author{r2pdf}",
  "\\usepackage{colortbl}",
  "\\usepackage[a4paper]{geometry}",
  "\\usepackage{listings} % see https://texdoc.org/serve/listings.pdf/0 for users guide and reference guide",
  "\\geometry{top={30mm}, left={20mm}, right={20mm}}",
  "\\definecolor{mygreen}{rgb}{0,0.6,0}",
  "\\definecolor{darkred}{rgb}{0.5,0,0}",
  "\\lstdefinelanguage{Rldw}",
  "  {keywords={abbreviate,abline,abs,acos,acosh,action,add1,add,%",
  "      aggregate,alias,Alias,alist,all,anova,any,aov,aperm,append,apply,%",
  "      approx,approxfun,apropos,Arg,args,array,arrows,as,asin,asinh,%",
  "      atan,atan2,atanh,attach,attr,attributes,autoload,autoloader,ave,%",
  "      axis,backsolve,barplot,basename,besselI,besselJ,besselK,besselY,%",
  "      beta,binomial,body,box,boxplot,break,browser,bug,builtins,bxp,by,%",
  "      c,C,call,Call,case,cat,category,cbind,ceiling,character,char,%",
  "      charmatch,check,chol,chol2inv,choose,chull,class,close,cm,codes,%",
  "      coef,coefficients,co,col,colnames,colors,colours,commandArgs,%",
  "      comment,complete,complex,conflicts,Conj,contents,contour,%",
  "      contrasts,contr,control,helmert,contrib,convolve,cooks,coords,%",
  "      distance,coplot,cor,cos,cosh,count,fields,cov,covratio,wt,CRAN,%",
  "      create,crossprod,cummax,cummin,cumprod,cumsum,curve,cut,cycle,D,%",
  "      data,dataentry,date,dbeta,dbinom,dcauchy,dchisq,de,debug,%",
  "      debugger,Defunct,default,delay,delete,deltat,demo,de,density,%",
  "      deparse,dependencies,Deprecated,deriv,description,detach,%",
  "      dev2bitmap,dev,cur,deviance,off,prev,,dexp,df,dfbetas,dffits,%",
  "      dgamma,dgeom,dget,dhyper,diag,diff,digamma,dim,dimnames,dir,%",
  "      dirname,dlnorm,dlogis,dnbinom,dnchisq,dnorm,do,dotplot,double,%",
  "      download,dpois,dput,drop,drop1,dsignrank,dt,dummy,dump,dunif,%",
  "      duplicated,dweibull,dwilcox,dyn,edit,eff,effects,eigen,else,%",
  "      emacs,end,environment,env,erase,eval,equal,evalq,example,exists,%",
  "      exit,exp,expand,expression,External,extract,extractAIC,factor,%",
  "      fail,family,fft,file,filled,find,fitted,fivenum,fix,floor,for,%",
  "      For,formals,format,formatC,formula,Fortran,forwardsolve,frame,%",
  "      frequency,ftable,ftable2table,function,gamma,Gamma,gammaCody,%",
  "      gaussian,gc,gcinfo,gctorture,get,getenv,geterrmessage,getOption,%",
  "      getwd,gl,glm,globalenv,gnome,GNOME,graphics,gray,grep,grey,grid,%",
  "      gsub,hasTsp,hat,heat,help,hist,home,hsv,httpclient,I,identify,if,%",
  "      ifelse,Im,image,\\%in\\%,index,influence,measures,inherits,install,%",
  "      installed,integer,interaction,interactive,Internal,intersect,%",
  "      inverse,invisible,IQR,is,jitter,kappa,kronecker,labels,lapply,%",
  "      layout,lbeta,lchoose,lcm,legend,length,levels,lgamma,library,%",
  "      licence,license,lines,list,lm,load,local,locator,log,log10,log1p,%",
  "      log2,logical,loglin,lower,lowess,ls,lsfit,lsf,ls,machine,Machine,%",
  "      mad,mahalanobis,make,link,margin,match,Math,matlines,mat,matplot,%",
  "      matpoints,matrix,max,mean,median,memory,menu,merge,methods,min,%",
  "      missing,Mod,mode,model,response,mosaicplot,mtext,mvfft,na,nan,%",
  "      names,omit,nargs,nchar,ncol,NCOL,new,next,NextMethod,nextn,%",
  "      nlevels,nlm,noquote,NotYetImplemented,NotYetUsed,nrow,NROW,null,%",
  "      numeric,\\%o\\%,objects,offset,old,on,Ops,optim,optimise,optimize,%",
  "      options,or,order,ordered,outer,package,packages,page,pairlist,%",
  "      pairs,palette,panel,par,parent,parse,paste,path,pbeta,pbinom,%",
  "      pcauchy,pchisq,pentagamma,persp,pexp,pf,pgamma,pgeom,phyper,pico,%",
  "      pictex,piechart,Platform,plnorm,plogis,plot,pmatch,pmax,pmin,%",
  "      pnbinom,pnchisq,pnorm,points,poisson,poly,polygon,polyroot,pos,%",
  "      postscript,power,ppoints,ppois,predict,preplot,pretty,Primitive,%",
  "      print,prmatrix,proc,prod,profile,proj,prompt,prop,provide,%",
  "      psignrank,ps,pt,ptukey,punif,pweibull,pwilcox,q,qbeta,qbinom,%",
  "      qcauchy,qchisq,qexp,qf,qgamma,qgeom,qhyper,qlnorm,qlogis,qnbinom,%",
  "      qnchisq,qnorm,qpois,qqline,qqnorm,qqplot,qr,Q,qty,qy,qsignrank,%",
  "      qt,qtukey,quantile,quasi,quit,qunif,quote,qweibull,qwilcox,%",
  "      rainbow,range,rank,rbeta,rbind,rbinom,rcauchy,rchisq,Re,read,csv,%",
  "      csv2,fwf,readline,socket,real,Recall,rect,reformulate,regexpr,%",
  "      relevel,remove,rep,repeat,replace,replications,report,require,%",
  "      resid,residuals,restart,return,rev,rexp,rf,rgamma,rgb,rgeom,R,%",
  "      rhyper,rle,rlnorm,rlogis,rm,rnbinom,RNGkind,rnorm,round,row,%",
  "      rownames,rowsum,rpois,rsignrank,rstandard,rstudent,rt,rug,runif,%",
  "      rweibull,rwilcox,sample,sapply,save,scale,scan,scan,screen,sd,se,%",
  "      search,searchpaths,segments,seq,sequence,setdiff,setequal,set,%",
  "      setwd,show,sign,signif,sin,single,sinh,sink,solve,sort,source,%",
  "      spline,splinefun,split,sqrt,stars,start,stat,stem,step,stop,%",
  "      storage,strstrheight,stripplot,strsplit,structure,strwidth,sub,%",
  "      subset,substitute,substr,substring,sum,summary,sunflowerplot,svd,%",
  "      sweep,switch,symbol,symbols,symnum,sys,status,system,t,table,%",
  "      tabulate,tan,tanh,tapply,tempfile,terms,terrain,tetragamma,text,%",
  "      time,title,topo,trace,traceback,transform,tri,trigamma,trunc,try,%",
  "      ts,tsp,typeof,unclass,undebug,undoc,union,unique,uniroot,unix,%",
  "      unlink,unlist,unname,untrace,update,upper,url,UseMethod,var,%",
  "      variable,vector,Version,vi,warning,warnings,weighted,weights,%",
  "      which,while,window,write,\\%x\\%,x11,X11,xedit,xemacs,xinch,xor,%",
  "      xpdrows,xy,xyinch,yinch,zapsmall,zip},%",
  "   otherkeywords={!,!=,~,$,*,\\&,\\%/\\%,\\%*\\%,\\%\\%,<-,<<-,_,/},%",
  "   alsoother={._$},%",
  "   sensitive,%",
  "   morecomment=[l]\\#,%",
  "   morestring=[b]\",%",
  "   morestring=[b]', % 2001 Robert Denham",
  "  alsoletter={.}",
  "  }%",
  "",
  "\\lstset{ %",
  "	language=Rldw,                % choose the language of the code",
  "	basicstyle=\\footnotesize\\ttfamily,  % the size of the fonts that are used for the code",
  "	numbers=left,              % where to put the line-numbers",
  "	numberstyle=\\scriptsize,   % the size of the fonts that are used for the line-numbers",
  "	stepnumber=1,              % the step between two line-numbers. If it is 1 each line will be numbered",
  "	numbersep=5pt,             % how far the line-numbers are from the code",
  "	backgroundcolor=\\color{white},  % choose the background color. You must add \\usepackage{color}",
  "	showspaces=false,           % show spaces adding particular underscores",
  "	showstringspaces=false,     % underline spaces within strings",
  "	showtabs=false,             % show tabs within strings adding particular underscores",
  "	frame=none,                 % adds a frame around the code",
  "	tabsize=2,                  % sets default tabsize to 2 spaces",
  "	captionpos=b,               % sets the caption-position to bottom",
  "	breaklines=true,            % sets automatic line breaking",
  "	breakatwhitespace=false,    % sets if automatic breaks should only happen at whitespace",
  "	xleftmargin=15pt,",
  "	xrightmargin=5pt,",
  "	commentstyle=\\color{mygreen}, %teal",
  "	keywordstyle=\\color{blue},",
  "	stringstyle=\\color{darkred}    % if you want to add LaTeX within your code",
  "}",
  "%-------------------------------------------------------------------------------",
  "% The actual document",
  "%",
  "\\begin{document}",
  paste0("\\lstinputlisting{", src, "}"),
  "\\end{document}",
  ""), tmp)                                                             # nolint end
  close(tmp)
  tools::texi2dvi(tmpfile, pdf = TRUE, clean = TRUE)
  file.copy(tmppdf, dst, TRUE)
  file.remove(tmppdf)
  if (show_pdf) open_pdf(dst)
  invisible(NULL)
}
