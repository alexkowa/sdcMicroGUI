#should be in zzz.R
#require(gWidgetsRGtk2)localSupp1_tmp
#sdcGUIenv <- new.env() starts in gui function

# just for test case while not in sdcMicro package
#require(sdcMicro)

sdcGUI <- function() {
  if(!is.null(options("quitRwithsdcGUI")[[1]]))#If started via windows binarybuild, auto start update
    updates2()
  updates22 <- function(...)updates2(restart=TRUE)
#Tooltip main window und select variables
  tt_selVar <- "Summary of the selected variables and their assignment"
  tt_print <- "Frequency print"
  tt_summary <- "Frequency summary"
  tt_ir <- "measure_risk Output"
  tt_vc <- "Configure your key variables"
  tt_ls1 <- "Local Suppression"
  tt_ld1 <- "Compute l-Diversity"
  tt_man <- "Run additional R commands"
  tt_pir <- "Histogram and ECDF of the individual risks"
  tt_noi <- "Add noise"
  tt_shuffle <- "Use model-based shuffling for generate anonym data"
  tt_topcoding <- "Extreme values are replaced by one value to reduce the disclosure risk"
  tt_ma <- "Microaggregation of numeric variables"
  tt_rr <- "Recalculate Risk"
  tt_slider1 <- "Paramter k for risk computation"
  tt_slider2 <- "Paramter k2 for risk computation"
  tt_nmr <- "Numerical method risk"
  tt_pram1 <- "PRAM is a probabilistic, perturbative method which can be applied on categorical variables"
  tt_pram2 <- "View the saved PRAM output."
  tt_genstrat <- "Generate a strata variable"
#
  mosaic_check <- function(formX){
    xtmp <- ActiveSdcVars("manipKeyVars")
    ft <- as.data.frame(ftable(xtmp[,formX,drop=FALSE]))
    ft <- ft[ft$Freq!=0,]
    if(nrow(ft)>40){ 
      plot(1,main="Two many classes for a nice mosaic plot!")
    }else{
      mosaic(as.formula(paste("~",paste(formX,collapse="+"),sep="")),data=xtmp,shade=FALSE)
    }
  }
  #data(free1)
  #data(testdata)
  # 
  
  .findHelpPage <- function(topic, package=NULL) {
    l <- list(topic=topic)
    if(!is.null(package))
      l$package <- package
    out <- do.call("help", l)
    if(length(out) == 0) return(NULL)
    
    pkgname <-  basename(dirname(dirname(out)))
    
    ## thanks to Josef L for this
    help.txt <- "" ## keep R CMD check happy  
    help.con <- textConnection("help.txt", "w", local = TRUE)
    Rd2txt(.getHelpFile_sdcMicroGUI(out), out=help.con, package=pkgname,
        width=80L)
    close(help.con)
    
    return(list(x=help.txt,topic=topic, package=pkgname))
  }
  .insertHelpPage <- function(obj, x) {
    isSlow <- obj@toolkit@toolkit == "tcltk" || obj@toolkit@toolkit == "RGtk2"
    dispose(obj)       # clear
    
    out <- c()
    for(i in x) {
      if(grepl("^_\b",i)) {
        if(isSlow)
          out <- c(out, gsub("_\b","",i))
        else
          insert(obj, gsub("_\b","",i), font.attr=c(weight="bold"))
      } else {
        if(isSlow)
          out <- c(out,i)
        else
          insert(obj, i,font.attr=c(weight="normal"))
      }
    }
    if(isSlow)
      svalue(obj) <- out
    else
      insert(obj, "", do.newline=FALSE, where="beginning")              
  }
  helpR <- function(topic){
    print(help(topic))
  }
  
  
  # Script
  #
  Script <- function(name, ...) {
    if( missing(name) ) {
      getd("activeScript")
    } else { 
      putd("activeScript", name)
    }
  }
  
  Script.new <- function(...) {
    xtmp <- list(cmd=c())
    putd("activeScript", xtmp)
    putd("activescript.file", "Untitled Script")
    cmd.seed <- paste(paste("set.seed(",round(runif(1)*10e5),")"),
    paste("# sdcMicro:",packageVersion("sdcMicro"),", sdcMicroGUI",packageVersion("sdcMicroGUI")))
    eval(parse(text=cmd.seed))
    Script.add(cmd.seed)
  }
  
  Script.add <- function(cmd, ...) {
    xtmp <- Script()
    xtmp$cmd[length(xtmp$cmd)+1] = cmd
    Script(xtmp)
    if(exists("leftFrame") && existd("leftgdf")) {
      leftgdf <- getd("leftgdf")
      delete(leftFrame, leftgdf)
      leftgdf <- gdf(Script()$cmd, expand=TRUE, container=leftFrame)
      add3rdmousepopupmenu(leftgdf, leftgdflist)
      putd("leftgdf", leftgdf)
    }
  }
  
  Script.run <- function(xscr, ...) {
    if( missing(xscr) ) {
      xcmd <- Script()
      xcmd <- xcmd$cmd
    } else {
      xcmd <- xscr
    }
    xprogress = gwindow("please wait", width=180, height=40, parent=window)
    glabel("... script running ...", container=xprogress)
    activedataset <- ""
    sdcObject <- ""
    for( i in 1:length(xcmd) ) {
      trycatch <- try(eval(parse(text=xcmd[i])))
      if(class(trycatch)=="try-error"){
        dispose(xprogress)
        msg <- paste("Running the script was not possible due to the following error:\n",attributes(trycatch)$condition$message)
        gmessage(msg, title="Attention", icon="error", parent=window)
        rmd(listd()) # cleans up the Environment
        stop(msg)
      }
      #xtmp <- function() { eval(parse(text=ytmp)) }
      #do.call(xtmp, list(), envir=sdcGUIenv)
    }
    putd("activeDataSet", activedataset)
    putd("dataSetName", "activedataset")
    ActiveSdcObject(sdcObject)
    writeVars(ActiveSdcVarsStr("keyVars"),ActiveSdcVarsStr("numVars"), ActiveSdcVarsStr("weightVar"),
        ActiveSdcVarsStr("hhId"),ActiveSdcVarsStr("strataVar"))
    dispose(xprogress)
    updateRightFrame()
  }
  
  viewkanon2anonymity <- function(){
    fk <- ActiveSdcVars("risk")$individual[,2]
    TFfk <- fk<2
    if(any(TFfk)){
      orig <- ActiveSdcVars("origData")
      kV <- ActiveSdcVars("manipKeyVars")
      nV <- ActiveSdcVars("manipNumVars")
      orig <- orig[,!colnames(orig)%in%c(colnames(kV),colnames(nV)),drop=FALSE]
      d <- orig
      if(!is.null(kV))
        d <- cbind(kV,orig)
      if(!is.null(nV))
        d <- cbind(nV,orig)
      xtmp <- cbind(fk[TFfk],d[TFfk,])
      colnames(xtmp)[1] <- c("fk")
      xtmp <- xtmp[order(xtmp[,1]),]
      win = gwindow("Observations violating 2-anoymity", parent=window)
      mainGroup1 = ggroup(container=win, horizontal=FALSE)
      vkT <- gtable(xtmp)
      size(vkT) <- c(800,600)
      add(mainGroup1, vkT)
    }else
      gmessage("No observations violating 2-anonymity", title="Information", icon="info", parent=window)
  }

  viewkanon <- function(){
    fk <- ActiveSdcVars("risk")$individual[,2]
    TFfk <- fk<3
    if(any(TFfk)){
      orig <- ActiveSdcVars("origData")
      kV <- ActiveSdcVars("manipKeyVars")
      nV <- ActiveSdcVars("manipNumVars")
      orig <- orig[,!colnames(orig)%in%c(colnames(kV),colnames(nV)),drop=FALSE]
      d <- orig
      if(!is.null(kV))
        d <- cbind(kV,orig)
      if(!is.null(nV))
        d <- cbind(nV,orig)
      xtmp <- cbind(fk[TFfk],d[TFfk,])
      colnames(xtmp)[1] <- c("fk")
      xtmp <- xtmp[order(xtmp[,1]),]
      win = gwindow("Observations violating 3-anoymity", parent=window)
      mainGroup1 = ggroup(container=win, horizontal=FALSE)
      vkT <- gtable(data.frame(apply(xtmp,2,function(x)as.character(x)),stringsAsFactors=FALSE))
      size(vkT) <- c(800,600)
      add(mainGroup1, vkT)
    }else
      gmessage("No observations violating 3-anonymity", title="Information", icon="info", parent=window)
  }
  viewldiv <- function(){
    ldiv <- ActiveSdcVars("risk")$ldiversity
    ldiv <- ldiv[,grep("_Distinct_Ldiversity",colnames(ldiv)),drop=FALSE]
    fk <- ActiveSdcVars("risk")$individual[,2]
    TFfk <- apply(ldiv,1,function(x)any(x<3))
    if(any(TFfk)){
      orig <- ActiveSdcVars("origData")
      kV <- ActiveSdcVars("manipKeyVars")
      nV <- ActiveSdcVars("manipNumVars")
      orig <- orig[,!colnames(orig)%in%c(colnames(kV),colnames(nV)),drop=FALSE]
      d <- orig
      if(!is.null(kV))
        d <- cbind(kV,orig)
      if(!is.null(nV))
        d <- cbind(nV,orig)
      xtmp <- cbind(ldiv[TFfk,],fk[TFfk],d[TFfk,])
      colnames(xtmp)[1:ncol(ldiv)] <- colnames(ldiv)
      colnames(xtmp)[ncol(ldiv)+1] <- "fk"
      xtmp <- xtmp[order(xtmp[,1]),]
      win = gwindow("Observations violating 2 l-diversity", parent=window)
      mainGroup1 = ggroup(container=win, horizontal=FALSE)
      vkT <- gtable(data.frame(apply(xtmp,2,function(x)as.character(x)),stringsAsFactors=FALSE))
      size(vkT) <- c(800,600)
      add(mainGroup1, vkT)
    }else
      gmessage("No observations violating 2 l-diversity", title="Information", icon="info", parent=window)
  }
  viewhigh <- function(){
    rk <- ActiveSdcVars("risk")$individual[,1]
    rko <- order(rk,decreasing = TRUE)[1:20]
    fk <- ActiveSdcVars("risk")$individual[,2]
    orig <- ActiveSdcVars("origData")
    kV <- ActiveSdcVars("manipKeyVars")
    nV <- ActiveSdcVars("manipNumVars")
    orig <- orig[,!colnames(orig)%in%c(colnames(kV),colnames(nV)),drop=FALSE]
    d <- orig
    if(!is.null(nV))
      d <- cbind(nV,d)
    if(!is.null(kV))
      d <- cbind(kV,d)
    xtmp <- cbind(fk[rko],rk[rko],d[rko,])
    colnames(xtmp) <- c("fk","risk",colnames(d))
    xtmp <- xtmp[order(xtmp[,2],decreasing=TRUE),]
    win = gwindow("Observations with highest risk", parent=window)
    mainGroup1 = ggroup(container=win, horizontal=FALSE)
    vkT <- gtable(data.frame(apply(xtmp,2,function(x)as.character(x)),stringsAsFactors=FALSE))
    size(vkT) <- c(800,600)
    add(mainGroup1, vkT)
  }  
  # function for button ir_button (plotIndivRisk)
  # indivRiskGroup function
  # x ... object of class indivRisk
  # y ... object of class freqCalc
  plotIndivRisk <- function(...) {
    method = "histogram"
    putd("method","histogram")
    m1 <- ActiveSdcVars("risk")
    mu <- m1$global$threshold
    rk <-  m1$individual[,1]
    fk <-  m1$individual[,2]
    if(is.na(mu))
      mu <- quantile(rk,.9, na.rm=TRUE)
#    sd <- 1/length(rk) * (sum(fk[rk < mu] * rk[rk < mu]) + mu*sum(fk[rk>mu])) * 100
    s2 <- length(which(rk > mu))
    mu.old <- mu
#    sd.old <- sd
    s2.old <- s2
#    maxsd <- 1/length(rk) * (sum(fk * rk)) *100
    knames <- ActiveSdcVarsStr()
    n1 <- knames[1]    ## next, the plot of column names of keys
    if( length(knames) > 1 ){
      for(i in 2:length(knames)){
        n1 <- paste(n1, "x", knames[i])
      }
    }
    norm.refresh <- function(...) {
      method = getd("method")
      mu <- as.numeric(evalq(svalue(smu)))
#      sd <- as.numeric(evalq(svalue(ssd)))
      s2 <- as.numeric(evalq(svalue(ss2)))
      if (mu != mu.old) {
        s2 <- round(length(which(rk > mu)))
#        sd <- 1/length(rk) * (sum(fk[rk < mu] * rk[rk < mu]) + mu*sum(fk[rk>mu])) * 100
#        try(svalue(ssd)<-sd)
        try(svalue(ss2)<-s2)
#        sd.old <<- sd
        s2.old <<- s2
      }
#      if (sd != sd.old) {
#        sd <- as.numeric(evalq(tclvalue(s2)))#, envir = slider.env))
#        s2 <- length(which(rk > mu))
#        try(svalue(ssd)<-sd)
#        try(svalue(ss2)<-s2)
#        sd.old <<- sd
#        s2.old <<- s2
#      }
      if (s2 != s2.old) {
        s2 <- as.numeric(evalq(tclvalue(s2)))#, envir = slider.env))
#        sd <- 1/length(rk) * (sum(fk * rk) + 0.02*sum(fk))
#        try(svalue(ssd)<-sd)
#        sd.old <<- sd
        s2.old <<- length(which(rk > mu))
      }
      if( method == "histogram" ){
        hist(rk, main=n1,freq=TRUE, xlab="individual risk", col="yellow")
        abline(v=mu, col="blue", lwd=2)
      }
      if( method == "ecdf" ){
        plot(ecdf(rk), main="ecdf of individual risk", xlab="individual risk")
        abline(v=mu, col="blue", lwd=2)
      }
    }
    plot1 <- function(method){
      if( method == "histogram" ){
        putd("method","histogram")
        hist(rk, main=n1,freq=TRUE, xlab="individual risk", col="yellow")
        abline(v=mu, col="blue", lwd=2)
      }
      if( method == "ecdf" ){
        putd("method","ecdf")
        plot(ecdf(rk), main="ecdf of individual risk", xlab="individual risk")
        abline(v=as.numeric(evalq(svalue(smu))), col="blue", lwd=2)
      }
    }
    win = gwindow("Individual Risk Adjustments", parent=window)
    mainGroup1 = ggroup(container=win, horizontal=FALSE)
    method = "histogram"
    sliderGroup = ggroup(container=mainGroup1, horizontal=FALSE)
    tmp = gframe('<span weight="bold" size="medium">Individual Risk Threshold</span>',
        container=sliderGroup,markup=TRUE)
    mustart <- round(mu/0.001)*0.001
    tostart <- round(max(rk)/0.001)*0.001+0.001
    smu = gslider(from=0, to=tostart, by=0.001, value=mustart, handler=norm.refresh)
    add(tmp, smu, expand=TRUE)
#    tmp = gframe('<span weight="bold" size="medium">Re-identification Rate</span>',
#        container=sliderGroup,markup=TRUE)
#    sdstart <- round(sd/0.01)*0.01
#    to2start=round(maxsd/0.01)*0.01+0.01
#    ssd = gslider(from=0, to=to2start, by=0.01, value=sdstart, handler=norm.refresh)
#    add(tmp, ssd, expand=TRUE)
    tmp = gframe('<span weight="bold" size="medium">Unsafe Records</span>',
        container=sliderGroup,markup=TRUE)
    s2start <- round(s2)
    ss2 = gslider(from=0, to=length(rk), by=1, value=s2start, handler=norm.refresh)
    add(tmp, ss2, expand=TRUE)
    gbutton("Show ecdf", container=mainGroup1, handler=function(x,...) plot1("ecdf"))
    gbutton("Show histogram", container=mainGroup1, handler=function(x,...) plot1("histogram"))
    gbutton("Suppress above threshold", container=mainGroup1, handler=function(x,...){
          smuval=as.numeric(svalue(smu))
          dispose(win)
          localSupp_tmp(threshold=smuval)
        })
    add(mainGroup1, ggraphics())
    if( method == "histogram" ){
      try(hist(rk, main=n1,freq=TRUE, xlab="individual risk", col="yellow"), silent=TRUE)
      try(abline(v=mu, col="blue", lwd=2), silent=TRUE)
    }
  }
  
  # FreqCalc and indivRisk calculation - freqCalc()
  #                                    - indivRisk()
  # TODO: not needed - save freqCalcIndivRisk for script/history
  freqCalcIndivRisk <- function(...) {
    xprogressFQ = gwindow("please wait", width=250, height=140, parent=window)
    glabel("... calculating ...", container=xprogressFQ)
    # freqCalc
    ActiveSdcObject(measure_risk(ActiveSdcObject()))
    tmp <- capture.output(printFrequenciesComp(ActiveSdcObject()))
    risk <- ActiveSdcObject()@risk
    originalRisk <- ActiveSdcObject()@originalRisk
    currenthighrisk <- sum((risk$individual[,1] > median(risk$individual[,1])+2*mad(risk$individual[,1])) & (risk$indiviual[,1] > 0.1))
    current2violate <- sum(risk$individual[,2]<2)
    current3violate <- sum(risk$individual[,2]<3)
    currentexpectedrisk <- round(risk$global$risk_ER,2)
    
    orighighrisk <- sum((originalRisk$individual[,1] > median(originalRisk$individual[,1])+2*mad(originalRisk$individual[,1])) & (originalRisk$indiviual[,1] > 0.1))
    orig2violate <- sum(originalRisk$individual[,2]<2)
    orig3violate <- sum(originalRisk$individual[,2]<3)
    origexpectedrisk <- round(originalRisk$global$risk_ER,2)
    CurrentCount = 0
    if(!is.null(risk$global$hier_risk_ER)) {
      CurrentCount = round(risk$global$hier_risk_ER,2)
    }
    CurrentPercentage = 0
    if(!is.null(risk$global$hier_risk_pct)) {
      CurrentPercentage = round(risk$global$hier_risk_pct,2)
    }
    OrigCount = 0
    if(!is.null(originalRisk$global$hier_risk_ER)) {
      OrigCount = round(originalRisk$global$hier_risk_ER,2)
    }
    OrigPercentage = 0
    if(!is.null(originalRisk$global$hier_risk_ER)) {
      OrigPercentage = round(originalRisk$global$hier_risk_pct,2)
    }
    n <- nrow(ActiveSdcObject()@origData)
    svalue(nbMain) <- 1
    keyvariablerisktable[] = data.frame(c("R1", "R2", "R3", "R4", "R5"), 
                                        c("violating 2-anonymity", "violating 3-anonymity", "risk-higher than the benchmark", "Re-indentification, global risk", "Re-indentification, hierarchical risk"), 
                                        c(current2violate, current3violate, currenthighrisk, currentexpectedrisk, CurrentCount), 
                                        c(orig2violate, orig3violate, orighighrisk, origexpectedrisk, OrigCount),
                                        c(paste(round(current2violate/n * 100, 2), "%", SEP=""), paste(round(current3violate/n * 100, 2), "%", SEP=""), paste(round(currenthighrisk/n * 100, 2), "%", SEP=""), paste(round(currentexpectedrisk/n * 100, 2), "%", SEP=""),paste(CurrentPercentage, "%", SEP="")), 
                                        c(paste(round(orig2violate/n * 100, 2), "%", SEP=""), paste(round(orig3violate/n * 100, 2), "%", SEP=""), paste(round(orighighrisk/n * 100, 2), "%", SEP=""), paste(round(origexpectedrisk/n * 100, 2), "%", SEP=""),paste(OrigPercentage, "%", SEP="")),stringsAsFactors=FALSE)
    visible(keyvariableriskgraph) <- TRUE
    counts <- c(round(current2violate/n * 100, 2), round(orig2violate/n * 100, 2), 
                round(current3violate/n * 100, 2), round(orig3violate/n * 100, 2),
                round(currenthighrisk/n * 100, 2), round(orighighrisk/n * 100, 2),
                round(currentexpectedrisk/n * 100, 2),round(origexpectedrisk/n * 100, 2),
                CurrentPercentage, OrigPercentage
    )
    barplot(matrix(counts, nrow = 2, ncol = 5), beside = TRUE, main="Percentage of observations at risk", names.arg=c("R1", "R2","R3", "R4", "R5"))
    m1 <- ActiveSdcVars("risk")$individual
    xtmp <- ActiveSdcVars("manipKeyVars")
    tabDat <- cbind(xtmp,m1)
    ind <- !duplicated(apply(xtmp,1,function(x)paste(x,collapse="_")))
    tabDat <- tabDat[ind,]
    tabDat$risk <- round(tabDat$risk,5)
    tabDat <- tabDat[order(as.numeric(tabDat$risk),decreasing=TRUE),]
    if(nrow(tabDat) > 10) {
      tabDat <- tabDat[1:10,]
    }
    delete(keyvariableFreqGroup, getd("keyvariableFreqTable"))
    putd("keyvariableFreqTable",gtable(data.frame(apply(tabDat,2,function(x)as.character(x)),stringsAsFactors=FALSE), container=keyvariableFreqGroup,expand=TRUE))
    svalue(nbMain) <- 2
    keyVars <- colnames(ActiveSdcObject()@manipKeyVars)
    if(is.null(ActiveSdcObject()@localSuppression))
      lsup <- list(rep(0,length(keyVars)))
    else
      lsup <- ActiveSdcObject()@localSuppression
    keyvariablerecodetable[] = data.frame(keyVars, c(do.call("cbind", lsup)), round(100 * c(do.call("cbind", lsup))/n,3),stringsAsFactors=FALSE)
    visible(keyvariablerecodegraph) <- TRUE
    barplot(c(do.call("cbind", lsup)), beside = TRUE, main="Suppressions", names.arg=keyVars, xlab="categorical key variables")
    recode_summary[,] <- returnRecode(ActiveSdcObject())
    svalue(nbMain) <- 1
    #Measure Risk Funktion
    dispose(xprogressFQ)
  }
  
  # TODO: var to factor tmp
  varToFactor_tmp <- function(var){
    xtmp <- get.sdcMicroObj(ActiveSdcObject(), type="manipKeyVars")
    tmp <- var[var %in% names(xtmp)]
    if(length(tmp) > 0 && !is.factor(tmp)) {
    Script.add(paste("sdcObject <- varToFactor(sdcObject,var=", 
            parseVarStr(tmp), 
            ")", sep=""))  
    ActiveSdcObject(varToFactor(ActiveSdcObject(),var=tmp))
    updateOutput(Script()$cmd[length(Script()$cmd)])
    }
  }
  varToNumeric_tmp <- function(var){
    xtmp <- get.sdcMicroObj(ActiveSdcObject(), type="manipKeyVars")
    suppressWarnings(tmpvar <- as.numeric(as.character(xtmp[,var])))
    ret <- TRUE
    if(sum(is.na(tmpvar))>sum(is.na(xtmp[,var]))){
      if(existd("rb")){
        rb <- getd("rb")
        ind <- 1
        svalue(rb[[ind]]) <- "Categorical"
        gr1_window <- getd("gr1_window")
        gmessage("Variable cannot be changed to numeric!", title="Information", icon="info", parent=gr1_window)
        ret <- FALSE
      }
    }else{
      ActiveSdcObject(varToNumeric(ActiveSdcObject(),var=var))
      Script.add(paste("sdcObject <- varToNumeric(sdcObject,var=", 
              parseVarStr(var), 
              ")", sep=""))
      #hideLevels(var)
    }
#	print(head(xtmp))
    ret
  }
  pram_tmp <- function(var,strata_var=NULL){
    xprogress = gwindow("please wait", width=180, height=40)
    glabel("... script running ...", container=xprogress)
    if(length(strata_var)>0){
      strata_var <- parseVarStr(strata_var)
      Script.add(paste("sdcObject <- pram(sdcObject,variables=", 
              parseVarStr(var),",strata_variables=",strata_var, 
              "",")", sep="")) 
      ActiveSdcObject(pram(ActiveSdcObject(),variables=var,strata_variables=strata_var))
      updateOutput(paste("sdcObject <- pram(sdcObject,variables=", 
                         parseVarStr(var),",strata_variables=",strata_var, 
                         "",")", sep=""))
    }else{
      strata_var <- parseVarStr(strata_var)
      Script.add(paste("sdcObject <- pram(sdcObject,variables=", 
              parseVarStr(var),"",")", sep="")) 
      ActiveSdcObject(pram(ActiveSdcObject(),variables=var))
      updateOutput(paste("sdcObject <- pram(sdcObject,variables=", 
                         parseVarStr(var),"",")", sep="")) 
    } 
    freqCalcIndivRisk()
    dispose(xprogress)
  }
  #LocalSuppression
  localSuppression_tmp <- function(k, importance) {
    Script.add(paste("sdcObject <- localSuppression(sdcObject,k=", parseVar(k), ",importance=", parseVar(importance), ")", sep=""))
    xprogress = gwindow("please wait", width=180, height=40)
    glabel("... script running ...", container=xprogress)
    importance <- importance
    ActiveSdcObject(localSuppression(obj=ActiveSdcObject(), k=k, importance=importance))
    updateOutput(paste("sdcObject <- localSuppression(sdcObject,k=", parseVar(k), ",importance=", parseVar(importance), ")", sep=""))
    freqCalcIndivRisk()
    dispose(xprogress)
  }
  
  
  # microaggregation_tmp - microaggregation()
  # TODO: done - save microaggregation for script/history
  microaggregation_tmp <- function(aggr, method, vars,strata_variables=NULL) {
    xprogress = gwindow("please wait", width=180, height=40)
    glabel("... script running ...", container=xprogress)
    if(length(strata_variables)==0){
      Script.add(paste("sdcObject <- microaggregation(sdcObject,aggr=", parseVar(aggr), ", method=",
              parseVarStr(method), ", variables=", parseVarStr(vars), ")", sep=""))
      strata_variables <- NULL
    }else{
      Script.add(paste("sdcObject <- microaggregation(sdcObject,aggr=", parseVar(aggr), ", method=",
              parseVarStr(method), ", variables=", parseVarStr(vars),",strata_variables=",parseVarStr(strata_variables), ")",
              sep=""))
      
    }
    ActiveSdcObject(microaggregation(ActiveSdcObject(), method=method, aggr=aggr,variables=vars,strata_variables=strata_variables))
    updateOutput(Script()$cmd[length(Script()$cmd)])
    freqCalcIndivRisk()
    nm_risk_print_function()
    dispose(xprogress)
  }
  localSupp_tmp <- function(threshold) {
    putd("threshold",threshold)
    nm2_window = gwindow("Suppress above threshold", width=230, parent=window,height=300)
    nb <- gnotebook(container=nm2_window, closebuttons=FALSE)
    #Main
    nm2_windowGroup = ggroup(container=nb, horizontal=FALSE,label="Function")
    #Help
    t <- gtext(container=nb, label="Help ", expand=TRUE)
    l <- .findHelpPage("localSupp", "sdcMicro")
    x <- l$x
    .insertHelpPage(t, x)
    svalue(nb) <- 1
    
    tmp = gframe("key-Variable to supress", container=nm2_windowGroup, horizontal=FALSE)
    VarSel = gdroplist(ActiveSdcVarsStr())
    tt_var <- "For observation with risk above the threshold, this variable will be deleted."
    tooltip(VarSel) <- tt_var
    add(tmp, VarSel)
    gseparator(container=nm2_windowGroup)
    nm2_windowButtonGroup = ggroup(container=nm2_windowGroup)
    addSpring(nm2_windowButtonGroup)
    gbutton("Ok", container=nm2_windowButtonGroup,
        handler=function(h,...) {
          Var=svalue(VarSel)
          xprogress = gwindow("please wait", width=180, height=40)
          glabel("... script running ...", container=xprogress)
          Script.add(paste("sdcObject <- localSupp(sdcObject,threshold=", parseVar(getd("threshold")),",keyVar=",parseVarStr(Var),")",sep=""))
          
          ActiveSdcObject(localSupp(ActiveSdcObject(), threshold=getd("threshold"),keyVar=Var))
          freqCalcIndivRisk()
          dispose(nm2_window)
          dispose(xprogress)
          #plotIndivRisk()
        })
    gbutton("Cancel ", container=nm2_windowButtonGroup, handler=function(h,...) { dispose(nm2_window) })
    gbutton("Help ", container=nm2_windowButtonGroup, handler=function(h,...) { helpR("microaggregation") })
    
    
    
    
    
  }
  # shuffle_tmp - shuffle()
  shuffle_tmp <- function( method,regmethod,covmethod, xvars,yvars) {
    xprogress = gwindow("please wait", width=180, height=40)
    glabel("... script running ...", container=xprogress)
    form <- paste(paste(xvars,collapse="+"),"~",paste(yvars,collapse="+"))
    Script.add(paste("sdcObject <- shuffle(sdcObject,method=", parseVarStr(method), ",regmethod= ",parseVarStr(regmethod), ", covmethod=",parseVarStr(covmethod), ", form=",
            form, ")", sep=""))
    ActiveSdcObject(shuffle(obj=ActiveSdcObject(), form=as.formula(form), method=method, regmethod=regmethod,covmethod=covmethod))
    updateOutput(Script()$cmd[length(Script()$cmd)])
    nm_risk_print_function()
    freqCalcIndivRisk()
    dispose(xprogress)
  }
  ls4 <- function(...){
    nm2_window = gwindow("Local Suppression", width=230, parent=window,height=400)
    nb <- gnotebook(container=nm2_window, closebuttons=FALSE)
    #Main
    ls3_pars = ggroup(container=nb, horizontal=FALSE,label="Function")
    #Help
    t <- gtext(container=nb, label="Help ", expand=TRUE)
    l <- .findHelpPage("localSuppression", "sdcMicro")
    x <- l$x
    .insertHelpPage(t, x)
    svalue(nb) <- 1
    
    tmp = gframe('<span weight="bold" size="medium">k-Anonymity parameter</span>',
        container=ls3_pars,markup=TRUE)
    x = gslider(2, 12, by=1)
    add(tmp, x, expand=TRUE)
    y_tmp <- get.sdcMicroObj(ActiveSdcObject(),"manipKeyVars")
    y <- list()
    xxtmp <- apply(y_tmp, 2, function(x) { length(table(x))})
    importance <- match(xxtmp, sort(xxtmp, decreasing=FALSE))
    y_tmp <- ActiveSdcVarsStr()
    for( i in 1:length(y_tmp) ) {
      fns <- eval(parse(text=paste("
                      function(...){
                      if(existd(\"impslider\")){      
                      ii <- ",i,"
                      y <- getd(\"impslider\")
                      yval <- as.numeric(as.vector(lapply(y,svalue)))
                      valtmp <- c(1:length(y))[-yval[ii]]
                      yval[-ii][order(yval[-ii])] <- valtmp
                      yval[yval[ii]<yval] <- yval[yval[ii]<yval]+1
                      for(i in 1:length(yval)){
                      svalue(y[[i]]) <- yval[i]
                      }
                      } 
                      }
                      ",sep="")))
      y[[i]] <- gslider(from=1, to=length(importance), by=1, value=importance[i],handler=fns)
    }
    putd("impslider",y)
    tmp = gframe('<span weight="bold" size="medium">Importance of keyVars</span>',
        container=ls3_pars, horizontal=FALSE,markup=TRUE)
    for( i in 1:length(y_tmp) ) {
      tmpg = ggroup(container=tmp)
      tmpt = glabel(y_tmp[i])
      add(tmpg, tmpt, expand=TRUE)
      add(tmpg, y[[i]], expand=TRUE)
    }
    gseparator(container=ls3_pars)
    ls3_parsButtonGroup = ggroup(container=ls3_pars)
    addSpring(ls3_parsButtonGroup)
    gbutton("Ok", container=ls3_parsButtonGroup,
        handler=function(h,...) {
          importance <- as.numeric(as.vector(lapply(y,svalue)))
          k <- svalue(x)
          localSuppression_tmp(k, importance)
          dispose(nm2_window)
#          }
        })
    gbutton("Cancel ", container=ls3_parsButtonGroup, handler=function(h,...) { dispose(nm2_window) })
    gbutton("Help ", container=ls3_parsButtonGroup, handler=function(h,...) { helpR("localSuppression") })
    
    
  }
  # function for nm_button2
  # globalRecodeGroup-numericalMethods function
  nm2 <- function(...) {
    #Tooltip Microaggegation
    tt_aggr <- "aggregation level (default=3)"
    tt_method <- "mdav, rmd, pca, clustpppca, influence"
    tt_ltr <- "Add selected variable(s)"
    tt_rtl <- "Remove selected variable(s)" 
    tt_ltr1 <- "Add selected strata variable(s)"
    tt_rtl1 <- "Remove selected strata variable(s)"
    lTOr <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(selTab[])==1 ) {
          if( is.na(selTab[]) ) {
            selTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            selTab[,] <- data.frame(vars=c(selTab[], h), stringsAsFactors=FALSE)
          }
        } else {
          selTab[,] <- data.frame(vars=c(selTab[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(varTab[]) ) {
          varTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(varTab[]) ) {
            for( j in 1:length(h) ) {
              if( varTab[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          varTab[,] <- data.frame(vars=varTab[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    rTOl <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(varTab[])==1 ) {
          if( is.na(varTab[]) ) {
            varTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            varTab[,] <- data.frame(vars=c(varTab[], h), stringsAsFactors=FALSE)
          }
        } else {
          varTab[,] <- data.frame(vars=c(varTab[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(selTab[]) ) {
          selTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(selTab[]) ) {
            for( j in 1:length(h) ) {
              if( selTab[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          selTab[,] <- data.frame(vars=selTab[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    lTOr1 <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(selTab1[])==1 ) {
          if( is.na(selTab1[]) ) {
            selTab1[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            selTab1[,] <- data.frame(vars=c(selTab1[], h), stringsAsFactors=FALSE)
          }
        } else {
          selTab1[,] <- data.frame(vars=c(selTab1[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(sTab[]) ) {
          sTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(sTab[]) ) {
            for( j in 1:length(h) ) {
              if( sTab[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          sTab[,] <- data.frame(vars=sTab[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    rTOl1 <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(sTab[])==1 ) {
          if( is.na(sTab[]) ) {
            sTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            sTab[,] <- data.frame(vars=c(sTab[], h), stringsAsFactors=FALSE)
          }
        } else {
          sTab[,] <- data.frame(vars=c(sTab[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(selTab1[]) ) {
          selTab1[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(selTab1[]) ) {
            for( j in 1:length(h) ) {
              if( selTab1[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          selTab1[,] <- data.frame(vars=selTab1[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    
    nm2_window = gwindow("Microaggregation", width=230, parent=window,height=600)
    nb <- gnotebook(container=nm2_window, closebuttons=FALSE)
    #Main
    nm2_windowGroup = ggroup(container=nb, horizontal=FALSE,label="Function")
    #Help
    t <- gtext(container=nb, label="Help ", expand=TRUE)
    l <- .findHelpPage("microaggregation", "sdcMicro")
    x <- l$x
    .insertHelpPage(t, x)
    svalue(nb) <- 1
    
    tmp = gframe('<span weight="bold" size="medium">Aggregation level (size of the groups)</span>',
        container=nm2_windowGroup, horizontal=FALSE,markup=TRUE)
    ntmp = ggroup(container=tmp)
    aggrSel = gslider(from=2, to=20, by=1)
    tooltip(aggrSel) <- tt_aggr
    svalue(aggrSel) <- 3
    add(ntmp, aggrSel, expand=TRUE)
    tmp = gframe("Method", container=nm2_windowGroup, horizontal=FALSE)
    methodSel = gdroplist(c("mdav","rmd", "pca", "clustpppca", "influence"))
    tooltip(methodSel) <- tt_method
    add(tmp, methodSel)
    tmp = gframe('<span weight="bold" size="medium">Variable selection</span>',
        container=nm2_windowGroup,markup=TRUE)
    numVars <- c()
    # just use all numerical vars
    #for( i in 1:dim(xtmp)[2] ) {
    #	if( is.numeric(xtmp[,i]) & names(xtmp)[i] != ActiveSdcVarsStr("weightVar") ) {
    #		numVars <- c(numVars, names(xtmp)[i])
    #	}
    #}
    numVars <- ActiveSdcVarsStr("numVars")
    varTab = gtable(data.frame(vars=numVars, stringsAsFactors=FALSE), multiple=TRUE)
    size(varTab) <- c(120,200)
    add(tmp, varTab)
    btmp = ggroup(container=tmp, horizontal=FALSE)
    addSpring(btmp)
    b1 <- gbutton(">>", container=btmp, handler=function(h,...) { lTOr(svalue(varTab)) })
    b2 <- gbutton("<<", container=btmp, handler=function(h,...) { rTOl(svalue(selTab)) })
    tooltip(b1) <- tt_ltr
    tooltip(b2) <- tt_rtl
    addSpring(btmp)
    selTab = gtable(data.frame(vars=character(0), stringsAsFactors=FALSE), multiple=TRUE)
    size(selTab) <- c(120,200)
    add(tmp, selTab)
    
    
    tmp = gframe('<span weight="bold" size="medium">Strata Variable selection</span>',
        container=nm2_windowGroup,markup=TRUE)
    sVars <- c()
    # just use all numerical vars
    #for( i in 1:dim(xtmp)[2] ) {
    #	if( is.numeric(xtmp[,i]) & names(xtmp)[i] != ActiveSdcVarsStr("weightVar") ) {
    #		numVars <- c(numVars, names(xtmp)[i])
    #	}
    #}
    sVars <- ActiveSdcVarsStr("strataVar")
    keyVars <- ActiveSdcVarsStr()
    sTab = gtable(data.frame(vars=c(sVars,keyVars), stringsAsFactors=FALSE), multiple=TRUE)
    size(sTab) <- c(120,200)
    add(tmp, sTab)
    btmp = ggroup(container=tmp, horizontal=FALSE)
    addSpring(btmp)
    b1 <- gbutton(">>", container=btmp, handler=function(h,...) { lTOr1(svalue(sTab)) })
    b2 <- gbutton("<<", container=btmp, handler=function(h,...) { rTOl1(svalue(selTab1)) })
    tooltip(b1) <- tt_ltr1
    tooltip(b2) <- tt_rtl1
    addSpring(btmp)
    selTab1 = gtable(data.frame(vars=character(0), stringsAsFactors=FALSE), multiple=TRUE)
    size(selTab1) <- c(120,200)
    add(tmp, selTab1)
    
    
    gseparator(container=nm2_windowGroup)
    nm2_windowButtonGroup = ggroup(container=nm2_windowGroup)
    addSpring(nm2_windowButtonGroup)
    gbutton("Ok", container=nm2_windowButtonGroup,
        handler=function(h,...) {
          aggrVal <- as.numeric(svalue(aggrSel))
          if( length(selTab[])<1 | any(is.na(selTab[])) ) {
            gmessage("You need to select at least 1 variable!", title="Information", icon="info", parent=nm2_window)
          } else {
            microaggregation_tmp(aggrVal, svalue(methodSel), vars=selTab[],strata_variables=selTab1[])
            svalue(nbMain) <- 2
            dispose(nm2_window)
          }
        })
    gbutton("Cancel ", container=nm2_windowButtonGroup, handler=function(h,...) { dispose(nm2_window) })
    gbutton("Help ", container=nm2_windowButtonGroup, handler=function(h,...) { helpR("microaggregation") })
  }
  ldiv1 <- function(...) {
    tt_ltr <- "Add selected variable(s)"
    tt_rtl <- "Remove selected variable(s)" 
    tt_slider1 <- "l_recurs_c Parameter"
    lTOr <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(selTab[])==1 ) {
          if( is.na(selTab[]) ) {
            selTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            selTab[,] <- data.frame(vars=c(selTab[], h), stringsAsFactors=FALSE)
          }
        } else {
          selTab[,] <- data.frame(vars=c(selTab[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(varTab[]) ) {
          varTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(varTab[]) ) {
            for( j in 1:length(h) ) {
              if( varTab[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          varTab[,] <- data.frame(vars=varTab[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    rTOl <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(varTab[])==1 ) {
          if( is.na(varTab[]) ) {
            varTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            varTab[,] <- data.frame(vars=c(varTab[], h), stringsAsFactors=FALSE)
          }
        } else {
          varTab[,] <- data.frame(vars=c(varTab[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(selTab[]) ) {
          selTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(selTab[]) ) {
            for( j in 1:length(h) ) {
              if( selTab[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          selTab[,] <- data.frame(vars=selTab[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    
    nm2_window = gwindow("l-diversity", width=230, parent=window,height=600)
    nb <- gnotebook(container=nm2_window, closebuttons=FALSE)
    #Main
    nm2_windowGroup = ggroup(container=nb, horizontal=FALSE,label="Function")
    #Help
    t <- gtext(container=nb, label="Help ", expand=TRUE)
    l <- .findHelpPage("measure_risk", "sdcMicro")
    x <- l$x
    .insertHelpPage(t, x)
    svalue(nb) <- 1
    tmp = gframe('<span weight="bold" size="medium">l Recursive Constant</span>',
        container=nm2_windowGroup, horizontal=FALSE,markup=TRUE)
    recconst = gslider(from=1, to=10, by=1, value=2)
    tooltip(recconst) <- tt_slider1
    enabled(recconst) = TRUE
    add(tmp, recconst, expand=TRUE)
    
    tmp = gframe('<span weight="bold" size="medium">Choose sensitive variable(s)</span>',
        container=nm2_windowGroup, horizontal=FALSE,markup=TRUE)
    
    xtmp <- ActiveSdcObject()@origData
    numVars <- ActiveSdcVarsStr("numVars")
    keyVars <- ActiveSdcVarsStr()
    hVars <- ActiveSdcVarsStr("hhId")
    wVars <- ActiveSdcVarsStr("weightVar")
    sVars <- ActiveSdcVarsStr("strataVar")
    posssensVars <- colnames(xtmp)[!colnames(xtmp)%in%c(numVars,keyVars,hVars,wVars,sVars)]
    
    varTab = gtable(data.frame(vars=posssensVars, stringsAsFactors=FALSE), multiple=TRUE)
    size(varTab) <- c(220,200)
    add(tmp, varTab)
    btmp = ggroup(container=tmp, horizontal=TRUE)
    addSpring(btmp)
    b1 <- gbutton(">>", container=btmp, handler=function(h,...) { lTOr(svalue(varTab)) })
    b2 <- gbutton("<<", container=btmp, handler=function(h,...) { rTOl(svalue(selTab)) })
    tooltip(b1) <- tt_ltr
    tooltip(b2) <- tt_rtl
    addSpring(btmp)
    selTab = gtable(data.frame(vars=character(0), stringsAsFactors=FALSE), multiple=TRUE)
    size(selTab) <- c(220,200)
    add(tmp, selTab)
    
    gseparator(container=nm2_windowGroup)
    nm2_windowButtonGroup = ggroup(container=nm2_windowGroup)
    addSpring(nm2_windowButtonGroup)
    gbutton("Ok", container=nm2_windowButtonGroup,
        handler=function(h,...) {
          if( length(selTab[])<1 | any(is.na(selTab[])) ) {
            gmessage("You need to select at least 1 variables!", title="Information", icon="info", parent=nm2_window)
          } else {
            ActiveSdcObject(ldiversity(ActiveSdcObject(),ldiv_index=selTab[],l_recurs_c=svalue(recconst)))
            updateOutput(paste("ldiversity", toString(selTab[]), svalue(recconst), sep=":"));
            dispose(nm2_window)
            ldiverg_window = gwindow("l-diversity", width=520, parent=window,height=400)
            nb <- gnotebook(container=ldiverg_window, closebuttons=FALSE)
            #Main
            nm2_windowGroup = ggroup(container=nb, horizontal=FALSE,label="Function")
            #Help
            t <- gtext(container=nb, label="Help ", expand=TRUE)
            l <- .findHelpPage("ldiversity", "sdcMicro")
            x <- l$x
            .insertHelpPage(t, x)
            svalue(nb) <- 1
            tmp = gframe("Output", container=nm2_windowGroup, horizontal=FALSE)
            gte <- gtext("", container=tmp, height=250, width=500)
            vk_button = gbutton("View Observations violating 2 l-diversity", container=tmp,
                handler=function(h, ...) viewldiv())
            
            
            svalue(gte) <- capture.output(print(ActiveSdcObject()@risk$ldiversity),append=FALSE)
            gseparator(container=nm2_windowGroup)
            nm2_windowButtonGroup = ggroup(container=nm2_windowGroup)
            addSpring(nm2_windowButtonGroup)
            gbutton("Ok", container=nm2_windowButtonGroup,handler=function(h,...)dispose(ldiverg_window))
            gbutton("Help ", container=nm2_windowButtonGroup, handler=function(h,...) { helpR("ldiversity") })
          }
        })
    gbutton("Cancel ", container=nm2_windowButtonGroup, handler=function(h,...) { dispose(nm2_window) })
    gbutton("Help ", container=nm2_windowButtonGroup, handler=function(h,...) { helpR("ldiversity") })
  }
  
  # addNoise_tmp - addNoise()
  # TODO: done - save addNoise for script/history
  addNoise_tmp <- function(noise, method, vars) {
    xprogress = gwindow("please wait", width=180, height=40)
    glabel("... script running ...", container=xprogress)
    Script.add(paste("sdcObject <- addNoise(sdcObject,noise=", parseVar(noise), ",method= ",
            parseVarStr(method), ",variables= ", parseVarStr(vars), ")", sep=""))
    ActiveSdcObject(addNoise(ActiveSdcObject(),noise=noise,method=method,variables=vars))
    updateOutput(Script()$cmd[length(Script()$cmd)])
    freqCalcIndivRisk()
    nm_risk_print_function()
    dispose(xprogress)
  }
  
  # function for nm_button1
  nm1 <- function(...) {
    #ToolTip Addnoise Window
    tt_noise <- "amount of noise (in percentages)"
    tt_method <- "choose between additive and correlated2"
    tt_ltr <- "Add selected variable(s)"
    tt_rtl <- "Remove selected variable(s)"
    lTOr <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(selTab[])==1 ) {
          if( is.na(selTab[]) ) {
            selTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            selTab[,] <- data.frame(vars=c(selTab[], h), stringsAsFactors=FALSE)
          }
        } else {
          selTab[,] <- data.frame(vars=c(selTab[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(varTab[]) ) {
          varTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(varTab[]) ) {
            for( j in 1:length(h) ) {
              if( varTab[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          varTab[,] <- data.frame(vars=varTab[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    rTOl <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(varTab[])==1 ) {
          if( is.na(varTab[]) ) {
            varTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            varTab[,] <- data.frame(vars=c(varTab[], h), stringsAsFactors=FALSE)
          }
        } else {
          varTab[,] <- data.frame(vars=c(varTab[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(selTab[]) ) {
          selTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(selTab[]) ) {
            for( j in 1:length(h) ) {
              if( selTab[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          selTab[,] <- data.frame(vars=selTab[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    nm1_window = gwindow("Add noise", width=230, parent=window)
    nb <- gnotebook(container=nm1_window, closebuttons=FALSE)
    #Main
    nm1_windowGroup = ggroup(container=nb, horizontal=FALSE,label="Function")
    tmp = gframe('<span weight="bold" size="medium">Noise</span>',
        container=nm1_windowGroup, horizontal=FALSE,markup=TRUE)
    #Help
    t <- gtext(container=nb, label="Help ", expand=TRUE)
    l <- .findHelpPage("addNoise", "sdcMicro")
    x <- l$x
    .insertHelpPage(t, x)
    svalue(nb) <- 1
    ntmp = ggroup(container=tmp)
    glabel("Value between 0 and 2000", container=ntmp)
    noiseSel = gedit()
    svalue(noiseSel) <- "150"
    tooltip(noiseSel) <- tt_noise
    add(ntmp, noiseSel)
    tmp = gframe('<span weight="bold" size="medium">Method</span>',
        container=nm1_windowGroup, horizontal=FALSE,markup=TRUE)
    methodSel = gdroplist(c("correlated2","additive"))
    tooltip(methodSel) <- tt_method
    add(tmp, methodSel)
    tmp = gframe('<span weight="bold" size="medium">Variable selection</span>',
        container=nm1_windowGroup,markup=TRUE)
    numVars <- c()
    # not all vars, just numerical vars
    #for( i in 1:dim(xtmp)[2] ) {
    #	if( class(xtmp[,i])=="numeric" & names(xtmp)[i] != ActiveSdcVarsStr("weightVar") ) {
    #		numVars <- c(numVars, names(xtmp)[i])
    #	}
    #}
    numVars <- ActiveSdcVarsStr("numVars")
    varTab = gtable(data.frame(vars=numVars, stringsAsFactors=FALSE), multiple=TRUE)
    size(varTab) <- c(120,200)
    add(tmp, varTab)
    btmp = ggroup(container=tmp, horizontal=FALSE)
    addSpring(btmp)
    b1 <- gbutton(">>", container=btmp, handler=function(h,...) { lTOr(svalue(varTab)) })
    b2 <- gbutton("<<", container=btmp, handler=function(h,...) { rTOl(svalue(selTab)) })
    tooltip(b1) <- tt_ltr
    tooltip(b2) <- tt_rtl
    addSpring(btmp)
    selTab = gtable(data.frame(vars=character(0), stringsAsFactors=FALSE), multiple=TRUE)
    size(selTab) <- c(120,200)
    add(tmp, selTab)
    gseparator(container=nm1_windowGroup)
    nm1_windowButtonGroup = ggroup(container=nm1_windowGroup)
    addSpring(nm1_windowButtonGroup)
    gbutton("Ok", container=nm1_windowButtonGroup,
        handler=function(h,...) {
          noise <- as.numeric(svalue(noiseSel))
          if( !is.numeric(noise) | is.na(noise) ) {
            gmessage("Noise needs to be a numeric value!", title="Information", icon="info", parent=nm1_window)
          } else {
            if( length(selTab[])==0 | any(is.na(selTab[])) ) {
              gmessage("You need to select at least 1 variable!", title="Information", icon="info", parent=nm1_window)
            } else {
              addNoise_tmp(noise, svalue(methodSel), selTab[])
              svalue(nbMain) <- 2
              dispose(nm1_window)
            } 
          }
        })
    gbutton("Cancel ", container=nm1_windowButtonGroup, handler=function(h,...) { dispose(nm1_window) })
    gbutton("Help ", container=nm1_windowButtonGroup, handler=function(h,...) { helpR("addNoise") })
  }
  
  # function for shuffle_button1
  shuffle1 <- function(...) {
    #Tooltip SHUFFLE
    tt_method <- "mdav, rmd, pca, clustpppca, influence"
    tt_regmethod <- "lm, MM"
    tt_covmethod <- c("spearman, pearson, mcd")
    tt_ltr <- "Add selected variable(s)"
    tt_rtl <- "Remove selected variable(s)" 
    tt_ltr1 <- "Add selected strata variable(s)"
    tt_rtl1 <- "Remove selected strata variable(s)"
    lTOr <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(selTab[])==1 ) {
          if( is.na(selTab[]) ) {
            selTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            selTab[,] <- data.frame(vars=c(selTab[], h), stringsAsFactors=FALSE)
          }
        } else {
          selTab[,] <- data.frame(vars=c(selTab[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(varTab[]) ) {
          varTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(varTab[]) ) {
            for( j in 1:length(h) ) {
              if( varTab[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          varTab[,] <- data.frame(vars=varTab[-xtmp], stringsAsFactors=FALSE)
        }
        if(any(selTab[,]%in%sTab[,])){
          sTab[,] <- sTab[,][-which(sTab[,]%in%selTab[,])]
        }
        if(any(selTab[,]%in%selTab1[,])){
          selTab1[,] <- selTab1[,][-which(selTab1[,]%in%selTab[,])]
        }
      }
    }
    rTOl <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(varTab[])==1 ) {
          if( is.na(varTab[]) ) {
            varTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            varTab[,] <- data.frame(vars=c(varTab[], h), stringsAsFactors=FALSE)
          }
        } else {
          varTab[,] <- data.frame(vars=c(varTab[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(selTab[]) ) {
          selTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(selTab[]) ) {
            for( j in 1:length(h) ) {
              if( selTab[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          selTab[,] <- data.frame(vars=selTab[-xtmp], stringsAsFactors=FALSE)
        }
        sTab[,] <- data.frame(vars=c(sTab[,], h), stringsAsFactors=FALSE)
      }
    }
    lTOr1 <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(selTab1[])==1 ) {
          if( is.na(selTab1[]) ) {
            selTab1[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            selTab1[,] <- data.frame(vars=c(selTab1[], h), stringsAsFactors=FALSE)
          }
        } else {
          selTab1[,] <- data.frame(vars=c(selTab1[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(sTab[]) ) {
          sTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(sTab[]) ) {
            for( j in 1:length(h) ) {
              if( sTab[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          sTab[,] <- data.frame(vars=sTab[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    rTOl1 <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(sTab[])==1 ) {
          if( is.na(sTab[]) ) {
            sTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            sTab[,] <- data.frame(vars=c(sTab[], h), stringsAsFactors=FALSE)
          }
        } else {
          sTab[,] <- data.frame(vars=c(sTab[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(selTab1[]) ) {
          selTab1[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(selTab1[]) ) {
            for( j in 1:length(h) ) {
              if( selTab1[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          selTab1[,] <- data.frame(vars=selTab1[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    
    nm2_window = gwindow("Shuffling", width=230, parent=window,height=600)
    nb <- gnotebook(container=nm2_window, closebuttons=FALSE)
    #Main
    nm2_windowGroup = ggroup(container=nb, horizontal=FALSE,label="Function")
    #Help
    t <- gtext(container=nb, label="Help ", expand=TRUE)
    l <- .findHelpPage("shuffle", "sdcMicro")
    x <- l$x
    .insertHelpPage(t, x)
    svalue(nb) <- 1
    
    tmp = gframe('<span weight="bold" size="medium">Shuffling Method</span>',
        container=nm2_windowGroup, horizontal=FALSE,markup=TRUE)
    methodSel = gdroplist(c("ds","mvn", "mlm"))
    tooltip(methodSel) <- tt_method
    add(tmp, methodSel)
    tmp = gframe('<span weight="bold" size="medium">Regression Method</span>',
        container=nm2_windowGroup, horizontal=FALSE,markup=TRUE)
    regmethodSel = gdroplist(c("lm","MM"))
    tooltip(regmethodSel) <- tt_regmethod
    add(tmp, regmethodSel)
    tmp = gframe('<span weight="bold" size="medium">Covariance Method</span>',
        container=nm2_windowGroup, horizontal=FALSE,markup=TRUE)
    covmethodSel = gdroplist(c("spearman","pearson","mcd"))
    tooltip(covmethodSel) <- tt_covmethod
    add(tmp, covmethodSel)
    
    
    tmp = gframe('<span weight="bold" size="medium">Numerical variable selection (Responses)</span>',
        container=nm2_windowGroup,markup=TRUE)
    numVars <- c()
    numVars <- ActiveSdcVarsStr("numVars")
    varTab = gtable(data.frame(vars=numVars, stringsAsFactors=FALSE), multiple=TRUE)
    size(varTab) <- c(120,200)
    add(tmp, varTab)
    btmp = ggroup(container=tmp, horizontal=FALSE)
    addSpring(btmp)
    b1 <- gbutton(">>", container=btmp, handler=function(h,...) { lTOr(svalue(varTab)) })
    b2 <- gbutton("<<", container=btmp, handler=function(h,...) { rTOl(svalue(selTab)) })
    tooltip(b1) <- tt_ltr
    tooltip(b2) <- tt_rtl
    addSpring(btmp)
    selTab = gtable(data.frame(vars=character(0), stringsAsFactors=FALSE), multiple=TRUE)
    size(selTab) <- c(120,200)
    add(tmp, selTab)
    
    
    tmp = gframe('<span weight="bold" size="medium">Variable selection (Predictors)</span>',
        container=nm2_windowGroup,markup=TRUE)
    xtmp <- ActiveDataSet()
    sVars <- colnames(xtmp)
    sTab = gtable(data.frame(vars=c(sVars), stringsAsFactors=FALSE), multiple=TRUE)
    size(sTab) <- c(120,200)
    add(tmp, sTab)
    btmp = ggroup(container=tmp, horizontal=FALSE)
    addSpring(btmp)
    b1 <- gbutton(">>", container=btmp, handler=function(h,...) { lTOr1(svalue(sTab)) })
    b2 <- gbutton("<<", container=btmp, handler=function(h,...) { rTOl1(svalue(selTab1)) })
    tooltip(b1) <- tt_ltr1
    tooltip(b2) <- tt_rtl1
    addSpring(btmp)
    selTab1 = gtable(data.frame(vars=character(0), stringsAsFactors=FALSE), multiple=TRUE)
    size(selTab1) <- c(120,200)
    add(tmp, selTab1)
    
    
    gseparator(container=nm2_windowGroup)
    nm2_windowButtonGroup = ggroup(container=nm2_windowGroup)
    addSpring(nm2_windowButtonGroup)
    gbutton("Ok", container=nm2_windowButtonGroup,
        handler=function(h,...) {
          if( length(selTab[])<2 | any(is.na(selTab[])) ) {
            gmessage("You need to select at least 2 numeric variable!", title="Information", icon="info", parent=nm2_window)
          }else if( length(selTab1[])<2 | any(is.na(selTab1[])) ) {
            gmessage("You need to select at least 2 predictor variable!", title="Information", icon="info", parent=nm2_window)
          } else {
            shuffle_tmp(method=svalue(methodSel),regmethod=svalue(regmethodSel),covmethod=svalue(covmethodSel), xvars=selTab[],yvars=selTab1[])
            svalue(nbMain) <- 2
            dispose(nm2_window)
          }
        })
    gbutton("Cancel ", container=nm2_windowButtonGroup, handler=function(h,...) { dispose(nm2_window) })
    gbutton("Help ", container=nm2_windowButtonGroup, handler=function(h,...) { helpR("shuffle") })
    
  }

  # topcoding_tmp - topBotCoding()
  topcoding_tmp <- function(value, replacement, kind = "top",column=NULL) {
  xprogress = gwindow("please wait", width=180, height=40)
  glabel("... script running ...", container=xprogress)
  Script.add(paste("sdcObject <- topBotCoding(sdcObject,value=", value, ",replacement= ",
                   replacement, ",kind= \"", kind, "\",column= \"", column,"\")", sep=""))
  ActiveSdcObject(topBotCoding(ActiveSdcObject(), value=as.numeric(value), replacement=as.numeric(replacement),
                               kind=kind, column=column))
  updateOutput(Script()$cmd[length(Script()$cmd)])
  freqCalcIndivRisk()
  nm_risk_print_function()
  dispose(xprogress)
 }

  # function for topcoding_button
  topcoding <- function(...) {
    topcoding_window = gwindow("Top and Bottom Coding", width=230, parent=window,height=300)
    #Main
    topcoding_windowGroup = ggroup(container=topcoding_window, horizontal=FALSE,label="Function")
    
    tmp = gframe('<span weight="bold" size="medium">Top or Bottom</span>',
                 container=topcoding_windowGroup, horizontal=FALSE,markup=TRUE)
    methodSel = gcombobox(c("top","bottom"), container=tmp)
    
    tmp = gframe('<span weight="bold" size="medium">Limit Value</span>',
                 container=topcoding_windowGroup, horizontal=FALSE,markup=TRUE)
    limitValueTxt = gedit("", container=tmp)
    
    tmp = gframe('<span weight="bold" size="medium">Replacement Value</span>',
                 container=topcoding_windowGroup, horizontal=FALSE,markup=TRUE)
    replaceValueTxt = gedit("", container=tmp)
    
    tmp = gframe('<span weight="bold" size="medium">Selecting Numerical Variable</span>',
                 container=topcoding_windowGroup, horizontal=FALSE,markup=TRUE)
    varList = getNameLabelList(names(ActiveSdcObject()@manipNumVars))
    varSel = gcombobox(c(do.call("cbind", varList)), container=tmp)
    
    gseparator(container=topcoding_windowGroup)
    nm2_windowButtonGroup = ggroup(container=topcoding_windowGroup)
    addSpring(nm2_windowButtonGroup)
    gbutton("Ok", container=nm2_windowButtonGroup,
            handler=function(h,...) {
              limit = svalue(limitValueTxt)
              if(nchar(limit) == 0 || nchar(svalue(replaceValueTxt)) == 0) {
                gmessage("The limit value and replacement value have to be given!", title="Information", icon="info", parent=topcoding_window)
              } else {
                selected = names(varList[match(svalue(varSel), varList)])
                selectedVar = ActiveSdcObject()@manipNumVars[selected]
                if(svalue(methodSel) == "top") {
                  len = length(selectedVar[selectedVar < as.numeric(limit)])
                } else {
                  len = length(selectedVar[selectedVar > as.numeric(limit)])
                }
                if(len == 0) {
                  gmessage("The limit value cannot be min at TOP or max at BOTTOM!", title="Information", icon="info", parent=topcoding_window)
                } else {
                  topcoding_tmp(value=svalue(limitValueTxt), replacement=svalue(replaceValueTxt),
                       kind=svalue(methodSel), column=names(varList[match(svalue(varSel), varList)]))
                  svalue(nbMain) <- 2
                  dispose(topcoding_window)
                }
              }
            })
    gbutton("Cancel ", container=nm2_windowButtonGroup, handler=function(h,...) { dispose(topcoding_window) })
    gbutton("Help ", container=nm2_windowButtonGroup, handler=function(h,...) { helpR("topBotCoding") })
  }

  # needed sub functions
  # TODO: done - save rename for script/history
  renameVars_tmp <- function(v, h, newName, redo=FALSE) {
    if( !redo ) {
      Script.add(paste("sdcObject <- renameVars(sdcObject,var=", parseVarStr(v), ", before=",
              parseVarStr(h), ", after=", parseVarStr(newName), ")", sep=""))
    }
    ActiveSdcObject(renameVars(ActiveSdcObject(),var=v,before=h,after=newName))
  }
  # TODO: done - save group for script/history
  groupVars_tmp <- function(v, h, newName, redo=FALSE) {
    if( !redo ) {
      Script.add(paste("sdcObject <- groupVars(sdcObject,var=", parseVarStr(v), ", before=",
              parseVarStr(h), ", after=", parseVarStr(newName), ")", sep=""))
    }
    ActiveSdcObject(groupVars(ActiveSdcObject(),var=v,before=h,after=newName))
  }
  # group and rename variables
  # globalRecodeGroup function
  
  # globalRecode_tmp - globalRecode()
  # TODO: replace cut with globalRecode as soon as it is corrected
  # TODO: done - save globalRecode for script/history
  globalRecode_tmp <- function(var, breaks, labels, redo=FALSE) {
    if(is.logical(labels))
      labels <- NULL
    if( !redo ) {
      Script.add(paste("sdcObject <- globalRecode(sdcObject,column=", parseVarStr(var), ", breaks=",
              parseVar(breaks), ", labels=", parseVarStr(labels), ")", sep=""))
    }
    ActiveSdcObject(globalRecode(ActiveSdcObject(),column=var,breaks=breaks,labels=labels))
    updateOutput(Script()$cmd[length(Script()$cmd)])
    #freqCalcIndivRisk()
  }
  
  # globalRecodeGroup function
  vc <- function(keyname=NA, ...) {
    if(existd("facTab")) {
      rmd("facTab")
    }
    if(existd("SummaryTab")) {
      rmd("SummaryTab")
    }
    if(existd("SummaryTabFrame")) {
      rmd("SummaryTabFrame")
    }    
    renameFacVar <- function(h, v, ...) {
      gr1_window <- getd("gr1_window")
      if( length(h)< 1 ) {
        gmessage("You need to select at least 1 level.", title="Information", icon="warning")
      } else {
        if( length(h)> 1 ) {
          gmessage("To rename one, you just have to select 1.", title="Information",
              icon="warning", parent=gr1_window)
        } else {
          newName <- ginput("Please enter a new level name.", parent=gr1_window)
          if( !is.na(newName) & newName!="" ) {
            renameVars_tmp(v, h, newName)
            #cat("v:\n")
            #print(v)
            #cat("h:\n")
            #print(h)
            showLevels(v)
            updateSummary(v)
          }
        }
      }
    }
    groupFacVar <- function(h, v, ...) {
      gr1_window <- getd("gr1_window")
      if( length(h)< 2 ) {
        gmessage("You need to select at least 2 levels to group.", title="Information",
            icon="warning", parent=gr1_window)
      } else {
        levName <- h[1]
        for( i in 2:length(h) ) {
          levName <- paste(levName, ";", h[i], sep="")
        }
        newName <- ginput("Please enter a new level name.", text=levName, parent=gr1_window)
        if( !is.na(newName) ) {
          groupVars_tmp(v, h, newName)
          showLevels(v)
          updateSummary(v)
        }
      }
    }
    updateSummary <- function(v=NULL){
      if(!is.null(v)){
        index <- which(keyname==v)
        if(existd("SummaryTab")){
          #gr1_head <- getd("gr1_head")
          #gr1_summary <- getd("gr1_summary")
          SummaryTab <- getd("SummaryTab")
          SummaryTabFrame <- getd("SummaryTabFrame")
          xtmp <- ActiveSdcObject()@manipKeyVars
          var <- xtmp[,v]
          if(isExtant(SummaryTab[[index]])){
            #svalue(gr1_head[[index]]) <- capture.output(print(head(var)),append=FALSE)
            Supdate <- as.data.frame(table(var))
            #colnames(Supdate) <- paste("Cat",1:ncol(Supdate),sep="")
            delete(SummaryTabFrame[[index]],SummaryTab[[index]])
            SummaryTab[[index]] <- gtable(Supdate,container=SummaryTabFrame[[index]])
            size(SummaryTab[[index]]) <- c(500,200)
            putd("SummaryTab",SummaryTab)
          }
          visible(gdev[[index]]) <- TRUE
          if(is.factor(var)){
            try(plot(var,main=v,xlab="Levels",ylab="Frequency"),silent=TRUE)
          }else if(is.numeric(var)){
            try(hist(var,main=v,xlab="Levels",ylab="Frequency"),silent=TRUE)
          }
          if(is.na(keyname)) {
            keyname <- ActiveSdcVarsStr()
          }
          varmoslist <- keyname[unlist(lapply(keyname,function(x)is.factor(xtmp[,x])))]
        }
        freqCalcIndivRisk()
      }else{
        indexAllKeys <- 1:length(keyname)
        if(existd("SummaryTab")){
          
          SummaryTab <- getd("SummaryTab")
          SummaryTabFrame <- getd("SummaryTabFrame")
          xtmp <- ActiveSdcObject()@manipKeyVars
          for(index in indexAllKeys){
            v <- keyname[index]
            var <- xtmp[,v]
            if(isExtant(SummaryTab[[index]])){
              #svalue(gr1_head[[index]]) <- capture.output(print(head(var)),append=FALSE)
              Supdate <- as.data.frame(table(var))
              #colnames(Supdate) <- paste("Cat",1:ncol(Supdate),sep="")
              delete(SummaryTabFrame[[index]],SummaryTab[[index]])
              SummaryTab[[index]] <- gtable(Supdate,container=SummaryTabFrame[[index]])
              size(SummaryTab[[index]]) <- c(500,200)
              putd("SummaryTab",SummaryTab)
            }
            visible(gdev[[index]]) <- TRUE
            if(is.factor(var)){
              try(plot(var,main=v,xlab="Levels",ylab="Frequency"),silent=TRUE)
            }else if(is.numeric(var)){
              try(hist(var,main=v,xlab="Levels",ylab="Frequency"),silent=TRUE)
            }
          }
          if(is.na(keyname)) {
            keyname <- ActiveSdcVarsStr()
          }
          varmoslist <- keyname[unlist(lapply(keyname,function(x)is.factor(xtmp[,x])))]
        }
      }
    }
    showLevels <- function(h, ...) {
#      cat("showLevels - ")
#      print(h)
#      cat(" - \n")
      if(existd("facTab")){
#        cat("done\n")
        facTab <- getd("facTab")
        #i <- which(ActiveSdcVarsStr()==h)
      i <- which(keyname==h)
        x <- facTab[[i]]
        if(isExtant(x)){
          xtmp <- ActiveSdcObject()@manipKeyVars
          x[,] <- levels(xtmp[,h])
          gr3_windowButton1 <- getd("gr3_windowButton1")
          gr3_windowButton2 <- getd("gr3_windowButton2")
          enabled(gr3_windowButton1[[i]]) <- TRUE
          enabled(gr3_windowButton2[[i]]) <- TRUE
        }
      }
    }
    hideLevels <- function(h, ...) {
      facTab <- getd("facTab")
      #i <- which(ActiveSdcVarsStr()==h)
      i <- which(keyname==h)
      x <- facTab[[i]] 
      x[,] <- character(0)
      gr3_windowButton1 <- getd("gr3_windowButton1")
      gr3_windowButton2 <- getd("gr3_windowButton2")
      enabled(gr3_windowButton1[[i]]) <- FALSE
      enabled(gr3_windowButton2[[i]]) <- FALSE
    }
    updateLevels <- function(){
      if(is.na(keyname)) {
        keyname <- ActiveSdcVarsStr()
      }
      rb <- getd("rb")
      for(k in seq_along(keyname)){
        if(is.factor(ActiveSdcObject()@manipKeyVars[,k])){
          svalue(rb[[k]]) <- "Categorical"
          showLevels(keyname[k])
        }else{
          hideLevels(keyname[k])
          svalue(rb[[k]]) <- "Continuous"
        }
      }
      
    }
    if(as.character(keyname) == "NULL" || is.na(keyname)) {
      keyname <- ActiveSdcVarsStr()
    }

    gr1_window = gwindow("globalRecode", width=800, parent=window)
    gr1_main <-  gframe("", container=gr1_window, horizontal=FALSE, expand=TRUE)
    nb <- gnotebook(container=gr1_main, closebuttons=FALSE, expand=TRUE)

    #Main
    xtmp <- ActiveSdcObject()@manipKeyVars
    keyNameLabels <- c(do.call("cbind", getNameLabelList(keyname)))
    groupFacVarFun <- renameFacVarFun <- gdev <- recFactorFun <- breaksInput <- labelsInput <- list()
    facTab <- gr3_windowButton1 <- gr3_windowButton2 <- recButton2 <- rb <- GraphFrame <- SummaryTab <- SummaryTabFrame <- rbfun <- list()

    for(i in 1:length(keyname))
    {
      #Main
      tmp <- ggroup(horizontal=TRUE, container=nb,label=keyNameLabels[i])
      lefttmp <- ggroup(horizontal=FALSE, container=tmp)
      svalue(nb) <- 1
      tmp1 <- gframe(text='<span weight="bold" size="medium">Type:</span>',
          horizonal=TRUE,container=lefttmp,markup=TRUE)
      rb[[i]] <- gradio(c("Continuous","Categorical"), container=tmp1)
      rbfun[[i]] <- eval(parse(text=paste("
                      function(h,...) {
                      index <- ",i,"
                      name <- \"",keyname[i],"\"
                      if(svalue(h$obj)==\"Categorical\"){
                      enabled(recButton2[[index]]) <- FALSE
                      varToFactor_tmp(name)
                      showLevels(name)
                      }else{
                        if(varToNumeric_tmp(name)){
                          hideLevels(name)
                          enabled(recButton2[[index]]) <- TRUE
                        }
                      }
                      var <- ActiveSdcObject()@manipKeyVars[,name]
                      updateSummary(name)
                      }",sep="")))
      
      addHandlerClicked(rb[[i]], handler=rbfun[[i]])
      GraphFrame[[i]] <-  ggroup(container=lefttmp, horizontal=FALSE,width = 500, height= 320)    
      SummaryTabFrame[[i]] <- gframe(text='<span weight="bold" size="medium">Frequencies:</span>',
          horizonal=FALSE,container=lefttmp,markup=TRUE)
      #glabel("Frequencies:",container=tmp1)
      dd_summary <- as.data.frame(table(xtmp[,keyname[i]]))
      colnames(dd_summary)<- as.character(dd_summary[1,])
      dd_summary <- dd_summary[-1,,drop=FALSE]
      Supdate <- as.data.frame(table(xtmp[,keyname[i]]))
      #colnames(Supdate) <- paste("Cat",1:ncol(Supdate),sep="")
      SummaryTab[[i]] <- gtable(Supdate, container=SummaryTabFrame[[i]] , expand=TRUE)
      size(SummaryTab[[i]]) <- c(500,200)
      
      tmp2 <- gframe("", container=tmp, horizontal=FALSE, expand=TRUE)
      #####Recode to Factor
      tmpRecFac <-  gframe('<span weight="bold" size="medium">Recode to categories</span>',
          container=tmp2, horizontal=FALSE,markup=TRUE)
      recFactorFun[[i]] <- eval(parse(text=paste(
                  "function(...){
                      index <- ",i,"
                      name <- \"",keyname[i],'"
                      breaksInput <- getd("breaksInput")
                      labelsInput <- getd("labelsInput")
                      breaks=svalue(breaksInput[[index]])
                      labels=svalue(labelsInput[[index]])
                      breaks <- strsplit(breaks, ",")[[1]]
                      labels <- strsplit(labels, ",")[[1]]
                      allNumeric <- TRUE
                      labelsNumeric <- TRUE
                      gr_do <- TRUE
                      if( length(breaks)==0 ) {
                      allNumeric <- FALSE
                      } else {
                      try(breaks <- as.numeric(breaks), silent=TRUE)
                      for( i in 1:length(breaks) ) {
                      if( is.na(breaks[i]) ) {
                      allNumeric <- FALSE
                      }
                      }
                      }
                      if( allNumeric==FALSE ) {
                      gmessage("Breaks argument is not valid", title="Information", icon="info", parent=gr1_window)
                      gr_do <- FALSE
                      }
                      if( allNumeric ) {
                      if( length(labels)>0 ) {
                      if( length(breaks)==1 ) {
                      if( length(labels)!=breaks) {
                      gmessage(paste("Too many or few labels supplied. ",breaks," labels should be supplied.",sep=""), title="Information", icon="info", parent=gr1_window)
                      gr_do <- FALSE
                      }
                      }
                      if( length(breaks)>1 ) {
                      if( length(labels)!=(length(breaks)-1) ) {
                      gmessage(paste("Too many or few labels supplied. ",(length(breaks)-1)," labels should be supplied.",sep=""), title="Information", icon="info", parent=gr1_window)
                      gr_do <- FALSE
                      }
                      }
                      if( gr_do ) {
                      try(tmp_labels <- as.numeric(labels), silent=TRUE)
                      for( i in 1:length(tmp_labels) ) {
                      if( is.na(tmp_labels[i]) ) {
                      labelsNumeric <- FALSE
                      }
                      }
                      if( labelsNumeric ) {
                      labels <- as.numeric(labels)
                      }
                      if( !labelsNumeric ) {
                      gr_do <- gconfirm("Variable will be of typ factor afterwards", title="Information",
                      icon="warning", parent=gr1_window)
                      }
                      }
                      } else {
                      labels <- FALSE
                      }
                      }        
                      if( gr_do ) {
                      globalRecode_tmp (name, breaks, labels)     
                      #var <- ActiveDataSet()[,name]
                      rb <- getd("rb")
                      blockHandler(rb[[index]])
                      svalue(rb[[index]]) <- "Categorical"
                      unblockHandler(rb[[index]])
                      updateSummary(name)
                      showLevels(name)
                      }
                      }',sep="")))
      recButton2[[i]] <- gbutton("Recode to categories", container=tmpRecFac, handler=recFactorFun[[i]])
      lab <- "BREAKS: Example input: 1,3,5,9 splits var in 3 groups"
      lab <- paste(lab, "\n(1,3],(3,5] and (5,9]. If you just supply")
      lab <- paste(lab, "\n1 number, like 3, the var will be split in")
      lab <- paste(lab, "\n3 equal sized groups.")
      glabel(lab, container=tmpRecFac)
      breaksInput[[i]] = gedit(width=40)
      add(tmpRecFac, breaksInput[[i]], expand=TRUE)
      lab <- "LABELS: Labels are depending on your breaks-input."
      lab <- paste(lab, "\nExample inupt with breaks=1,3,5,9 or breaks=3:")
      lab <- paste(lab, "\n- leave it blank: auto numbering from 1 to 3")
      lab <- paste(lab, "\n- a,b,c: the 3 groups are named a, b and c")
      glabel(lab, container=tmpRecFac)
      labelsInput[[i]] = gedit()
      add(tmpRecFac, labelsInput[[i]] , expand=TRUE)
      
      
      gseparator(container=tmp)
      ##Group/Rename Factor
      tmpGroupFac <-  gframe('<span weight="bold" size="medium">Group categories</span>',
          container=tmp2, horizontal=FALSE,markup=TRUE, expand=TRUE)
      tmpGroupFac2 = gframe("Levels", container=tmpGroupFac, expand=TRUE)
      facTab[[i]] <-  gtable(data.frame(levels=character(0), stringsAsFactors=FALSE),
          multiple=TRUE, expand=TRUE, container= tmpGroupFac2)
      size(facTab[[i]]) <- c(120,250)
      #add(tmpGroupFac2, facTab[[i]])
      btmp = ggroup(container=tmpGroupFac2, horizontal=FALSE, expand=TRUE)
      renameFacVarFun[[i]] <- eval(parse(text=paste('
                      function(h,...){
                      facTab <- getd("facTab")
                      renameFacVar(svalue(facTab[[',i,']]), "',keyname[i],'")
                      }
                      ',sep="")))
      
      
      gr3_windowButton1[[i]] <- gbutton("Rename selected level",
          handler= renameFacVarFun[[i]], container=btmp)
      enabled(gr3_windowButton1[[i]]) <- FALSE
      groupFacVarFun[[i]] <- eval(parse(text=paste('
                      function(h,...) {
                      facTab <- getd("facTab")
                      groupFacVar(svalue(facTab[[',i,']]), "',keyname[i],'") 
                      }
                      ',sep="")))
      
      gr3_windowButton2[[i]] <-  gbutton("Group selected levels",
          handler=groupFacVarFun[[i]], container=btmp)
      enabled(gr3_windowButton2[[i]]) <- FALSE
      gr3_windowButtonGroup = ggroup(container=tmpGroupFac)
      addSpring(gr3_windowButtonGroup)
      #Graphics Fenser

      ##Main
      if(is.factor(xtmp[,keyname[i]])){
        svalue(rb[[i]]) <- "Categorical"
      }else{
        svalue(rb[[i]]) <- "Continuous"
      }
    }
    #Save Input-Fields to the env
    putd("breaksInput",breaksInput)
    putd("labelsInput",labelsInput)
    putd("rb",rb)
    putd("gr1_window",gr1_window)
    putd("facTab",facTab)    
    putd("gr3_windowButton1",gr3_windowButton1)
    putd("gr3_windowButton2",gr3_windowButton2)
    #putd("gr1_head",gr1_head)
    #putd("gr1_summary",gr1_summary)
    putd("SummaryTab",SummaryTab)
    putd("SummaryTabFrame",SummaryTabFrame)
    
    
    #Insert Levels in List for Factor variables
    for(i in 1:length(keyname)){
      if(is.factor(xtmp[,keyname[i]])){
        showLevels(keyname[i])
      }
    }
    
    # First Keyvar-Tab
    svalue(nb) <- 1
    gseparator(container=gr1_main)
    okCancelGroup = ggroup(container=gr1_main)
    addSpring(okCancelGroup)
    gbutton("Ok", container=okCancelGroup,
        handler=function(h,...) {dispose(gr1_window) } )
    #gbutton("Cancel ", container=okCancelGroup, handler=function(h,...) dispose(gr1_window) )
    gbutton("Help ", container=okCancelGroup, handler=function(h,...) helpR("globalRecode") )
    
    #Plot ausfuehren
    for(i in length(keyname):1){
      tmp <- ggraphics(container=GraphFrame[[i]],width = 500, height= 300)
      svalue(nb) <- i
      gdev[[i]] <- tmp
    }
    svalue(nb) <- 1
    updateSummary()
  }
  
  removeDirectID_tmp <- function(var){
    cmd <- paste("sdcObject <- removeDirectID(sdcObject,var=",parseVarStr(var),")",sep="")
    Script.add(cmd)
    ActiveSdcObject(removeDirectID(ActiveSdcObject(),var))
    updateRightFrame()
  }
  removeDirectID_menu<- function(...) {
    tt_var <- "choose variables to be removed"
    tt_ltr <- "Add selected variable(s)"
    tt_rtl <- "Remove selected variable(s)"
    lTOr <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(selTab[])==1 ) {
          if( is.na(selTab[]) ) {
            selTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            selTab[,] <- data.frame(vars=c(selTab[], h), stringsAsFactors=FALSE)
          }
        } else {
          selTab[,] <- data.frame(vars=c(selTab[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(varTab[]) ) {
          varTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(varTab[]) ) {
            for( j in 1:length(h) ) {
              if( varTab[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          varTab[,] <- data.frame(vars=varTab[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    rTOl <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(varTab[])==1 ) {
          if( is.na(varTab[]) ) {
            varTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            varTab[,] <- data.frame(vars=c(varTab[], h), stringsAsFactors=FALSE)
          }
        } else {
          varTab[,] <- data.frame(vars=c(varTab[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(selTab[]) ) {
          selTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(selTab[]) ) {
            for( j in 1:length(h) ) {
              if( selTab[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          selTab[,] <- data.frame(vars=selTab[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    p1_window = gwindow("Remove direct identifiers", width=230, parent=window)
    nb <- gnotebook(container=p1_window, closebuttons=FALSE)
    #Main
    p1_windowGroup = ggroup(container=nb, horizontal=FALSE,label="Function")
    #Help
    t <- gtext(container=nb, label="Help ", expand=TRUE)
    l <- .findHelpPage("removeDirectID", "sdcMicro")
    x <- l$x
    .insertHelpPage(t, x)
    svalue(nb) <- 1
    
    
    tmp = gframe('<span weight="bold" size="medium">Select variables to be removed from data set</span>',
        container=p1_windowGroup,markup=TRUE)
    ###Select categorical variables
    keyVars <- ActiveSdcVarsStr("keyVars")
    numVars <- ActiveSdcVarsStr("numVars")
    wVars <- ActiveSdcVarsStr("weightVar")
    sVars <- ActiveSdcVarsStr("strataVar")
    hVars <- ActiveSdcVarsStr("hhId")
    deletedVars <- ActiveSdcObject()@deletedVars
    o <- ActiveSdcObject()@origData
    allVars <- colnames(o)
    allVars <- allVars[!allVars%in%c(keyVars,numVars,wVars,sVars,hVars,deletedVars)]
    allVars <- allVars[apply(o[,allVars],2,function(x)!all(is.na(x)))]
    varTab = gtable(data.frame(vars=getVarLabels(allVars), stringsAsFactors=FALSE), multiple=TRUE)
    size(varTab) <- c(max(nchar(varTab[]))*7,200)
    add(tmp, varTab)
    btmp = ggroup(container=tmp, horizontal=FALSE)
    addSpring(btmp)
    b1 <- gbutton(">>", container=btmp, handler=function(h,...) { lTOr(svalue(varTab)); size(selTab) <- c(max(nchar(selTab[]))*7,200)})
    b2 <- gbutton("<<", container=btmp, handler=function(h,...) { rTOl(svalue(selTab)) })
    tooltip(b1) <- tt_ltr
    tooltip(b2) <- tt_rtl
    addSpring(btmp)
    selTab = gtable(data.frame(vars=character(0), stringsAsFactors=FALSE), multiple=TRUE)
    size(selTab) <- c(180,200)
    add(tmp, selTab)
    gseparator(container=p1_windowGroup)
    
    
    p1_windowButtonGroup = ggroup(container=p1_windowGroup)
    addSpring(p1_windowButtonGroup)
    gbutton("Ok", container=p1_windowButtonGroup,
        handler=function(h,...) {
          if( length(selTab[])==0 ) {
            gmessage("You need to select at least 1 variable!", title="Information", icon="info", parent=p1_window)
          } else {
            var <- getVarNameFromLabel(selTab[])
            message <- paste("Do you really want to remove the following variables from the data set?\n",paste(var,collapse=","))
            TF <- gconfirm(message, title="Confirm", icon = "question", parent=p1_window)
            
            if(TF){
              removeDirectID_tmp(var)
              dispose(p1_window)
            }
          } 
        })
    gbutton("Cancel ", container=p1_windowButtonGroup, handler=function(h,...) { dispose(p1_window) })
    gbutton("Help ", container=p1_windowButtonGroup, handler=function(h,...) { helpR("removeDirectID") })
  } 
  
  
  pram1 <- function(...) {
    #ToolTip Pram Window
    tt_var <- "choose categorical variables for Pram"
    tt_strat <- "choose variables for stratification"
    tt_ltr <- "Add selected variable(s)"
    tt_rtl <- "Remove selected variable(s)"
    lTOr <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(selTab[])==1 ) {
          if( is.na(selTab[]) ) {
            selTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            selTab[,] <- data.frame(vars=c(selTab[], h), stringsAsFactors=FALSE)
          }
        } else {
          selTab[,] <- data.frame(vars=c(selTab[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(varTab[]) ) {
          varTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(varTab[]) ) {
            for( j in 1:length(h) ) {
              if( varTab[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          varTab[,] <- data.frame(vars=varTab[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    rTOl <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(varTab[])==1 ) {
          if( is.na(varTab[]) ) {
            varTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            varTab[,] <- data.frame(vars=c(varTab[], h), stringsAsFactors=FALSE)
          }
        } else {
          varTab[,] <- data.frame(vars=c(varTab[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(selTab[]) ) {
          selTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(selTab[]) ) {
            for( j in 1:length(h) ) {
              if( selTab[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          selTab[,] <- data.frame(vars=selTab[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    lTOr1 <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(selTab1[])==1 ) {
          if( is.na(selTab1[]) ) {
            selTab1[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            selTab1[,] <- data.frame(vars=c(selTab1[], h), stringsAsFactors=FALSE)
          }
        } else {
          selTab1[,] <- data.frame(vars=c(selTab1[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(sTab[]) ) {
          sTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(sTab[]) ) {
            for( j in 1:length(h) ) {
              if( sTab[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          sTab[,] <- data.frame(vars=sTab[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    rTOl1 <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(sTab[])==1 ) {
          if( is.na(sTab[]) ) {
            sTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            sTab[,] <- data.frame(vars=c(sTab[], h), stringsAsFactors=FALSE)
          }
        } else {
          sTab[,] <- data.frame(vars=c(sTab[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(selTab1[]) ) {
          selTab1[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(selTab1[]) ) {
            for( j in 1:length(h) ) {
              if( selTab1[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          selTab[,] <- data.frame(vars=selTab[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    p1_window = gwindow("Pram", width=230, parent=window)
    nb <- gnotebook(container=p1_window, closebuttons=FALSE)
    #Main
    p1_windowGroup = ggroup(container=nb, horizontal=FALSE,label="Function")
    #Help
    t <- gtext(container=nb, label="Help ", expand=TRUE)
    l <- .findHelpPage("pram", "sdcMicro")
    x <- l$x
    .insertHelpPage(t, x)
    svalue(nb) <- 1
    
    
    tmp = gframe('<span weight="bold" size="medium">Variable Selection</span>',
        container=p1_windowGroup,markup=TRUE)
    ###Select categorical variables
    allVars <- colnames(ActiveSdcObject()@origData)[-c(ActiveSdcVars("numVars"),ActiveSdcVars())]
    uniq <- sapply(ActiveSdcObject()@origData[,allVars,drop=FALSE],function(x)length(unique(x)))
    thr_cat <- max(100,nrow(ActiveSdcObject()@origData)*.05)
    keyVars <- ActiveSdcVarsStr()
    allVars <- c(allVars[uniq<=thr_cat],keyVars)
    
    
    varTab = gtable(data.frame(vars=allVars, stringsAsFactors=FALSE), multiple=TRUE)
    size(varTab) <- c(120,200)
    add(tmp, varTab)
    btmp = ggroup(container=tmp, horizontal=FALSE)
    addSpring(btmp)
    b1 <- gbutton(">>", container=btmp, handler=function(h,...) { lTOr(svalue(varTab)) })
    b2 <- gbutton("<<", container=btmp, handler=function(h,...) { rTOl(svalue(selTab)) })
    tooltip(b1) <- tt_ltr
    tooltip(b2) <- tt_rtl
    addSpring(btmp)
    selTab = gtable(data.frame(vars=character(0), stringsAsFactors=FALSE), multiple=TRUE)
    size(selTab) <- c(120,200)
    add(tmp, selTab)
    gseparator(container=p1_windowGroup)
    
    
    #Select strata_variables
    tmp = gframe('<span weight="bold" size="medium">Strata Variable Selection</span>',
        container=p1_windowGroup,markup=TRUE)
    sVars <- ActiveSdcVarsStr("strataVar")
    sTab = gtable(data.frame(vars=sVars, stringsAsFactors=FALSE), multiple=TRUE)
    size(sTab) <- c(120,200)
    add(tmp, sTab)
    btmp = ggroup(container=tmp, horizontal=FALSE)
    addSpring(btmp)
    b1 <- gbutton(">>", container=btmp, handler=function(h,...) { lTOr1(svalue(sTab)) })
    b2 <- gbutton("<<", container=btmp, handler=function(h,...) { rTOl1(svalue(selTab1)) })
    tooltip(b1) <- tt_ltr
    tooltip(b2) <- tt_rtl
    addSpring(btmp)
    selTab1 = gtable(data.frame(vars=character(0), stringsAsFactors=FALSE), multiple=TRUE)
    size(selTab1) <- c(120,200)
    add(tmp, selTab1)
    gseparator(container=p1_windowGroup)
    
    p1_windowButtonGroup = ggroup(container=p1_windowGroup)
    addSpring(p1_windowButtonGroup)
    gbutton("Ok", container=p1_windowButtonGroup,
        handler=function(h,...) {
          if( length(selTab[])==0 ) {
            gmessage("You need to select at least 1 variable!", title="Information", icon="info", parent=p1_window)
          } else {
            if(any(selTab[]%in%keyVars))
              TFKey <- gconfirm("If a key variable is selected for pram, the risk and frequency
                      calculations are not valid anymore. Are you sure you want to pursue with this action?",
                  title="Warning", icon="warn", parent=p1_window)
            else
              TFKey <- TRUE
            if(TFKey){ 
              var <- selTab[]
              svar <- sTab[]
              if(length(svar)==0) svar <- NULL
              pram_tmp(var, svar)
              
              dispose(p1_window)
              #enabled(pram_button2) <- TRUE
              viewpram1()
              
            }
          } 
        })
    gbutton("Cancel ", container=p1_windowButtonGroup, handler=function(h,...) { dispose(p1_window) })
    gbutton("Help ", container=p1_windowButtonGroup, handler=function(h,...) { helpR("pram") })
  }  
  viewpram1 <- function(...){
    p1_window = gwindow("Pram", width=230, parent=window)
    nb <- gnotebook(container=p1_window, closebuttons=FALSE)
    #Main
    p1_windowGroup = ggroup(container=nb, horizontal=FALSE,label="Function")
    #Help
    t <- gtext(container=nb, label="Help ", expand=TRUE)
    l <- .findHelpPage("pram", "sdcMicro")
    x <- l$x
    .insertHelpPage(t, x)
    svalue(nb) <- 1
    tmp = gframe('<span weight="bold" size="medium">Pram output</span>',
        container=p1_windowGroup,markup=TRUE)
    ps <- ActiveSdcObject()@pram$summary
    
    if(!is.null(ps)) {
      varTab = gtable(ps, multiple=TRUE)
      size(varTab) <- c(400,300)
      add(tmp, varTab)
    }
    
    gbutton("Close", container=p1_windowGroup, handler=function(h,...) { dispose(p1_window) })
    gbutton("Help ", container=p1_windowGroup, handler=function(h,...) { helpR("pram") })
  }
  # function for gr_button2
  # opens script window to execute R commands directly
  # globalRecodeGroup function
  scriptWindow <- function(...) {
    # TODO: auto scroll down needs to be implemented
    scriptEnv = new.env()
    assign("cmdhist", c(), envir=scriptEnv)
    sendCommand <- function(gin, gout, ...) {
      insert(gout, paste(">", svalue(gin)), font.attr=c(color="red", family="monospace"))
      err <- try(res <- capture.output(eval(parse(text=svalue(gin)), envir=scriptEnv), append=FALSE),silent=TRUE)
      if(class(err)!="try-error"){
        if( length(res)>0 ){
          #res <- capture.output(print(res))
          insert(gout, res[1], font.attr=c(family="monospace"))
          if( length(res)>1 ) {
            for( i in 2:length(res) ) {
              insert(gout, res[i], font.attr=c(family="monospace"))
            }
          }
        }
      }else{
        insert(gout, err[1]
            , font.attr=c(family="monospace"))
      }
      if( length(strsplit(svalue(gin), "<-")[[1]])>1 || length(strsplit(svalue(gin), "=")[[1]])>1 ) {
        cmdhist <- get("cmdhist", envir=scriptEnv)
        cmdhist <- c(cmdhist, svalue(gin))
        assign("cmdhist", cmdhist, envir=scriptEnv)
      }
      svalue(gin) <- ""
    }
    saveAds <- function(...) {
      ActiveSdcObject(get("sdc", envir=scriptEnv))
      freqCalcIndivRisk()
      cmdhist <- get("cmdhist", envir=scriptEnv)
      if( length(cmdhist) > 0 ) {
        for( i in 1:length(cmdhist) ) {
          Script.add(cmdhist[i])
        }
      }
      # end save
      freqCalcIndivRisk()
      nm_risk_print_function()
      quitScriptWindow()
    }
    removeWs <- function(...) {
      if( exists("scriptEnv", envir=.GlobalEnv) ) {
        try(rm(scriptEnv, envir=.GlobalEnv), silent=TRUE)
      }
    }
    sureQuit <- function(...) {
      gconfirm("You want to close the window without saving?", icon="question", parent=scriptWindow,
          handler=function(h,...) quitScriptWindow() )
    }
    quitScriptWindow <- function(...) {
      removeWs()
      dispose(scriptWindow)
    }
    loadAds <- function(...) {
      assign("sdc", ActiveSdcObject(), envir=scriptEnv)
      #-- End - summary.freqCalc
    }
    scriptWindow = gwindow("Script window", parent=window)
    scriptWidget = ggroup(horizontal=FALSE)
    scriptInfoGroup = ggroup(container=scriptWidget)
    addSpring(scriptInfoGroup)
    glabel("Active Sdc Object available for modifications as variable: sdc",
        container=scriptInfoGroup)
    gbutton("Reload active data set to sdc", container=scriptInfoGroup,
        handler=function(h,...) loadAds() )
    addSpring(scriptInfoGroup)
    loadAds()
    xout = gtext(text="", width=700, height=400)
    add(scriptWidget, xout)
    scriptSubmit = ggroup(container=scriptWidget)
    glabel(" >", container=scriptSubmit)
    xcom = gedit("", container=scriptSubmit, expand=TRUE)#, handler=function(h, ...) sendCommand(xcom, xout))
    gbutton("submit", container=scriptSubmit, handler=function(h, ...) sendCommand(xcom, xout))
    gseparator(container=scriptWidget)
    saveCancelGroup = ggroup(container=scriptWidget)
    addSpring(saveCancelGroup)
    gbutton("Overwrite ads", container=saveCancelGroup, handler=function(h,...) saveAds() )
    gbutton("Cancel ", container=saveCancelGroup, handler=function(h,...) sureQuit() )
    
    add(scriptWindow, scriptWidget)
    focus(xcom)
  }
  
  # TODO: nm_risk_print_function
  # nm_risk_print output function
  nm_risk_print_function <- function(...) {
    if(length(ActiveSdcVars("numVars"))>0){
      xprogress = gwindow("please wait", width=180, height=40, parent=window)
      glabel("... script running ...", container=xprogress)
      optionss <- ActiveSdcVars("options")
      obj <- ActiveSdcObject()
      ActiveSdcObject(dUtility(dRisk(obj)))
      
      risk <- ActiveSdcVars("risk")
      originalRisk <- ActiveSdcVars("originalRisk")
      utility <- ActiveSdcVars("utility")
      origrisknum = 100
      if(!is.null(obj@originalRisk$numeric)) {
        origrisknum = round(100*originalRisk$numeric,2)
      }
      svalue(nm_util_print) <- paste("Information Loss: Criteria IL1: ", 
          round(utility$il1,2), " (orig: 0)\nDifference in Eigenvalues:  ",round(utility$eigen*100,2)," %",
          " (orig: 0) \n",sep="")
      svalue(continuousvariablerisklabel) <- paste("Disclosure risk for continuous key variables is:", round(100*risk$numeric,2), 
                                                   "% (orig.", origrisknum, "%)", SEP="")
      visible(continuousvariableriskgraph) <- TRUE
      counts <- c(round(100*risk$numeric,2), origrisknum)
      bplt <- barplot(matrix(counts, nrow = 2, ncol = 1), beside = TRUE, main="Disclosure risk", 
              space=4, names.arg=c("curr risk", "orig risk"))
      visible(continuousvariablelossgraph) <- TRUE
      namelist <- names(obj@manipNumVars)
      lossgraphcombo[] <- namelist
      svalue(lossgraphcombo, index=TRUE) <- 1
      dispose(xprogress)
    } else {
      svalue(continuousvariablerisklabel) <- ""
      visible(continuousvariableriskgraph) <- TRUE
      barplot(0, axes = FALSE)
      lossgraphcombo[] <- c("")
      svalue(nm_util_print) <- ""
      visible(continuousvariablelossgraph) <- TRUE
      barplot(0, axes = FALSE)
    }
  }
  
  generateStrata_tmp <- function(stratavars,name){
    putd("sLen", 1)
    putd("sVars",name)
    
    xtmp <- ActiveDataSet()
    strata <- rep("",nrow(xtmp))
    for(i in 1:length(stratavars)){
      strata <- paste(strata,xtmp[,stratavars[i]],sep="")
      if(length(stratavars)>i)
        strata <- paste(strata,"-",sep="")
    }
    xtmp <- cbind(xtmp,strata)
    colnames(xtmp)[length(colnames(xtmp))] <- name
    putd("activeDataSet", xtmp)
    putd("sIndex", getIndex(name))
  }
  selVar <- function(...) {
    putd("keyLen", 0)
    putd("numLen", 0)
    putd("wLen", 0)
    putd("hLen", 0)
    putd("sLen", 0)
    #print(ActiveDataSet())
    ft <- function(f, t, h, var, pm, ...) {
      # pm: 1 for +, 0 for -
      count = getd(var)
      varlist = getd("varTab_selVar_list")
      if( pm == 1 ) {
        count <- count + length(h);
        putd("varTab_selVar_list", varlist[ !varlist %in% h ]) 
      } else {
        count <- count - length(h);
        putd("varTab_selVar_list", append(varlist, h)) 
      }
      putd(var, count)
      if( length(h)>0 ) {
        if( length(f[])==1 ) {
          if( is.na(f[]) ) {
            f[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            f[,] <- data.frame(vars=c(f[], h), stringsAsFactors=FALSE)
          }
        } else {
          f[,] <- data.frame(vars=c(f[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(t[]) ) {
          t[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          d <- t[]
          t[] <- d[ !d %in% h]
        }      
      }
    }
    selVar_window = gwindow("Select variables", width=230, parent=window,height=700)
    selVar_windowGroup = ggroup(container=selVar_window, horizontal=FALSE)
    selVar_main = ggroup(container=selVar_windowGroup)
    mtmp = ggroup(container=selVar_main)
    
    allVars <- names(ActiveDataSet())
    
    
    
    # If it is not the first call to selVar, the previous selection is read
    if(existd("sdcObject")){
      sdcObject <- getd("sdcObject")
      
      keyVars <- ActiveSdcVarsStr("keyVars")
      numVars <- ActiveSdcVarsStr("numVars")
      wVars <- ActiveSdcVarsStr("weightVar")
      sVars <- ActiveSdcVarsStr("strataVar")
      hVars <- ActiveSdcVarsStr("hhId")
      deletedVars <- ActiveSdcObject()@deletedVars
      putd("numLen", length(numVars))
      putd("keyLen", length(keyVars))
      putd("hLen", length(hVars))
      putd("wLen", length(wVars))
      putd("sLen", length(sVars))
    }else{
      deletedVars<-numVars <- keyVars <- hVars <- wVars <- sVars <- character(0)
    }
    #Not selected variables
    nsVars <- allVars[!allVars%in%c(numVars,keyVars,hVars,wVars,sVars,deletedVars)]
    #numVars <- c()
    #xtmp <- ActiveDataSet()
    #for( i in 1:dim(xtmp)[2] ) {
    #	numVars <- c(numVars, names(xtmp)[i])
    #}
    tabletmp = ggroup(container=mtmp, horizontal = FALSE)
    varTextBox = gedit(text = "", coerce.with = NULL, initial.msg = "please input filter letters")
    addHandlerKeystroke(varTextBox, handler = function(h, ...) {
      tab <- getd("varTab_selVar")
      filter <- svalue(varTextBox)
      varlist = getd("varTab_selVar_list")
      if(filter != "") {
        sel <- sapply(varlist, function(x) all(grepl(tolower(filter), tolower(x), fixed=TRUE) > 0))
        tab[] <- varlist[sel]
      } else {
        tab[] <- varlist
      }
    })
    add(tabletmp, varTextBox)
    varTab = gtable(data.frame(vars=getVarLabels(nsVars), stringsAsFactors=FALSE), multiple=TRUE)
    putd("varTab_selVar",varTab)
    putd("varTab_selVar_list", varTab[])
    size(varTab) <- c(max(nchar(varTab[]))*7, 612)
    add(tabletmp, varTab)
    
    rtmp = ggroup(container=mtmp, horizontal=FALSE)
    size(rtmp) <- c(220, 650)    
    tmp = gframe('<span weight="bold" size="medium">Categorical key variables</span>',
        container=rtmp,markup=TRUE)
    btmp = ggroup(container=tmp, horizontal=FALSE)
    addSpring(btmp)
    gbutton(">>", container=btmp, handler=function(h,...) { ft(catTab, varTab, svalue(varTab), "keyLen", 1) })
    gbutton("<<", container=btmp, handler=function(h,...) { ft(varTab, catTab, svalue(catTab), "keyLen", 0) })
    addSpring(btmp)
    catTab = gtable(data.frame(vars=getVarLabels(keyVars), stringsAsFactors=FALSE), multiple=TRUE)
    size(catTab) <- c(180,150)
    add(tmp, catTab)
    
    tmp = gframe('<span weight="bold" size="medium">Continuous key variables</span>',
        container=rtmp,markup=TRUE)
    btmp = ggroup(container=tmp, horizontal=FALSE)
    addSpring(btmp)
    gbutton(">>", container=btmp, handler=function(h,...) { ft(numTab, varTab, svalue(varTab), "numLen", 1) })
    gbutton("<<", container=btmp, handler=function(h,...) { ft(varTab, numTab, svalue(numTab), "numLen", 0) })
    addSpring(btmp)
    numTab = gtable(data.frame(vars=getVarLabels(numVars), stringsAsFactors=FALSE), multiple=TRUE)
    size(numTab) <- c(180,150)
    add(tmp, numTab)
    
    tmp = gframe('<span weight="bold" size="medium">Weight variable</span>',
        container=rtmp,markup=TRUE)
    btmp = ggroup(container=tmp, horizontal=FALSE)
    addSpring(btmp)
    gbutton(">>", container=btmp, handler=function(h,...) { ft(wTab, varTab, svalue(varTab), "wLen", 1) })
    gbutton("<<", container=btmp, handler=function(h,...) { ft(varTab, wTab, svalue(wTab), "wLen", 0) })
    addSpring(btmp)
    wTab = gtable(data.frame(vars=getVarLabels(wVars), stringsAsFactors=FALSE), multiple=TRUE)
    size(wTab) <- c(180,50)
    add(tmp, wTab)
    ##Household Selection
    tmp = gframe('<span weight="bold" size="medium">Cluster ID variable(e.g. household ID)</span>',
        container=rtmp,markup=TRUE)
    btmp = ggroup(container=tmp, horizontal=FALSE)
    addSpring(btmp)
    gbutton(">>", container=btmp, handler=function(h,...) { ft(hTab, varTab, svalue(varTab), "hLen", 1) })
    gbutton("<<", container=btmp, handler=function(h,...) { ft(varTab, hTab, svalue(hTab), "hLen", 0) })
    addSpring(btmp)
    hTab = gtable(data.frame(vars=getVarLabels(hVars), stringsAsFactors=FALSE), multiple=TRUE)
    size(hTab) <- c(180,50)
    add(tmp, hTab)
    
    tmp = gframe('<span weight="bold" size="medium">Strata variable</span>',
        container=rtmp,markup=TRUE)
    btmp = ggroup(container=tmp, horizontal=FALSE)
    addSpring(btmp)
    gbutton(">>", container=btmp, handler=function(h,...) { ft(sTab, varTab, svalue(varTab), "sLen", 1) })
    gbutton("<<", container=btmp, handler=function(h,...) { ft(varTab, sTab, svalue(sTab), "sLen", 0) })
    addSpring(btmp)    
    sTab = gtable(data.frame(vars=getVarLabels(sVars), stringsAsFactors=FALSE), multiple=TRUE)
    size(sTab) <- c(180,100)
    add(tmp, sTab)
    
    
    
    gseparator(container=selVar_windowGroup)
    selVar_windowButtonGroup = ggroup(container=selVar_windowGroup)
    addSpring(selVar_windowButtonGroup)
    b1 <- gbutton("Generate Strata Variable", container=selVar_windowButtonGroup, handler=function(h,...) {
          #confirmSelection_tmp(catTab[], numTab[], wTab[],hTab[],sTab[])
          #confirmSelection_tmp(catTab[], numTab[], wTab[],hTab[],sTab[])    
          stVar_window = gwindow("Generate a strata variable", width=230, parent=selVar_window)
          stVar_windowGroup = ggroup(container=stVar_window, horizontal=FALSE)
          stVar_main = ggroup(container=stVar_windowGroup)
          mtmp = ggroup(container=stVar_main)
          allVars <-colnames(ActiveDataSet())
          rmIndex <- c()
          nro <- nrow(ActiveDataSet())
          for(i in 1:length(allVars)){
            if(nrow(unique(ActiveDataSet()[,allVars[i],drop=FALSE]))>nro*.2)
              rmIndex <- c(rmIndex,i)
          }
          allVars <- allVars[-rmIndex]
          varTab = gtable(data.frame(vars=allVars, stringsAsFactors=FALSE), multiple=TRUE)
          size(varTab) <- c(120,400)
          add(mtmp, varTab)
          rtmp = ggroup(container=mtmp, horizontal=FALSE)
          tmp = gframe('<span weight="bold" size="medium">Generate new strata variable from:</span>',
              container=rtmp,markup=TRUE)
          btmp = ggroup(container=tmp, horizontal=FALSE)
          addSpring(btmp)
          gbutton(">>", container=btmp, handler=function(h,...) { ft(sTab, varTab, svalue(varTab), "sLen", 1) })
          gbutton("<<", container=btmp, handler=function(h,...) { ft(varTab, sTab, svalue(sTab), "sLen", 0) })
          addSpring(btmp)
          sTab = gtable(data.frame(vars=sVars, stringsAsFactors=FALSE), multiple=TRUE)
          size(sTab) <- c(120,400)
          add(tmp, sTab)
          gseparator(container=stVar_windowGroup)
          stVar_windowButtonGroup = ggroup(container=stVar_windowGroup)
          addSpring(stVar_windowButtonGroup)
          gbutton("Ok", container=stVar_windowButtonGroup,
              handler=function(h,...) {
                name <- "sdcMicroStrataVariable"
                sVars <- sTab[]                
                if(length(sVars)==0)
                  gmessage("You have to select at least  one categoric variable to generate a strata variable.",
                      title="Information", icon="warning", parent=window)
                else{
                  name <- paste(paste(sVars,collapse="_"),"_stratavar",sep="")
                  t1 <- paste("c(",paste("\"",sVars,"\"",sep="",collapse=","),")",sep="")
                  Script.add(paste("activedataset <- generateStrata(activedataset,", t1, ", \"",name,"\")",sep=""))
                  generateStrata_tmp(sVars,name)
                  dispose(stVar_window)
                  varTab_selVar=getd("varTab_selVar")
                  varTab_selVar[,] <- c(varTab_selVar[,],name)
                  
                }
                
              })
          gbutton("Cancel ", container=stVar_windowButtonGroup, handler=function(h,...) { dispose(stVar_window) })
          
        })
    tooltip(b1) <- tt_genstrat 
    gbutton("Ok", container=selVar_windowButtonGroup,
        handler=function(h,...) {          
          # check if firstrun - if not reset script and dataset to original one
          #cat(paste(getd("keyLen"), getd("numLen"), getd("wLen"), getd("hLen"), "\n"))
          fr_do <- TRUE
          if( !getd("firstRun") ) {
            fr_do <- gconfirm("If you reselect vars, script and dataset will reset.\nAre you sure?", title="Attention",
                icon="warning", parent=window)
            if( fr_do ) {
              frS <- Script()$cmd[2]
              Script.new()
              if(existd("cmdimp")){
                Script.add(getd("cmdimp"))
                rmd("cmdimp")
              }else if(substring(frS,1,16)=="activedataset <-"){
                Script.add(frS)
              }
              if( existd("oldsdcObject") ) {
                putd("sdcObject", getd("oldsdcObject"))
              }
            }
          } else {
            putd("firstRun", FALSE)
          }
          # check if enough is selected
          if( fr_do ) {
            # min selection must be 1 in each category
            #if( ((getd("keyLen")>=1 || getd("numLen")>=1))&&getd("wLen")%in%c(0,1)&&getd("hLen")%in%c(0,1)) {
            if( getd("keyLen")>=1 &&getd("wLen")%in%c(0,1)&&getd("hLen")%in%c(0,1) ) {
              #keyVars <- catTab[]
              keyVars <- getVarNameFromLabel(catTab[])
              confirmSelection_tmp(getVarNameFromLabel(catTab[]), getVarNameFromLabel(numTab[]), getVarNameFromLabel(wTab[]),getVarNameFromLabel(hTab[]),getVarNameFromLabel(sTab[]))
              dispose(selVar_window)
              updateRightFrame()
              if(getd("keyLen")>=1){
                keyV <- keyVars
                keynofac <- keyV[!as.vector(sapply(keyV,function(x)is.factor(ActiveDataSet()[,x])))]
                if(length(keynofac)>0){
                  keynofac <- paste(keynofac,collapse=",")
                  
                  gmessage(paste("The variables ",keynofac," are selected as categoric but not recognized as being in the correct format.
                              This can be confirmed or changed in the next window, you can reopen this window by clicking \"Recode\"",sep=""),
                      title="Information", parent=window)
                  vc()
                }
              }
            } else {
              gmessage("You have to select at least categoric key variable and optionally one weight variable, one cluster ID variable and/or several strata variables.",
                  title="Information", icon="warning", parent=window)              
            }
          }
        })
    gbutton("Cancel ", container=selVar_windowButtonGroup, handler=function(h,...) { dispose(selVar_window) })
  }
  
  updateWindowTitle <- function() {
    if(existd("importFilename") && getd("dataSetName") != "") {
      if(existd("importFilename"))
        svalue(window) <- paste(getd("importFilename"), " (type:", getd("importFilenameType"), ")", sep="")
      else
        svalue(window) <- getd("dataSetName")
      } else {
        svalue(window) <- "sdcMicro GUI"
      }
  }

  # function for gb1 (confirm selection)
  # needed sub functions
  # TODO: done - save selection for script/history
  writeVars <- function(t1,t2,t3,t4,t5){
    #svalue(dslab) <- paste(getd("dataSetName")," [n=",nrow(ActiveDataSet()),"]",sep="")
    updateWindowTitle()
    #enabled(gb1) <- TRUE
    #enabled(gb2) <- TRUE
    if(length(t1)>0){
      stmp <- ""
      for( i in 1:length(t1) ) {
        stmp <- paste(stmp,t1[i]," [#:",length(unique(ActiveDataSet()[,t1[i]])),"]\n",sep="")
      } 
      #svalue(tab1) <- stmp
      #enabled(ir_button) <- TRUE
      #enabled(vk_button) <- TRUE
      #enabled(vh_button) <- TRUE
      #enabled(ls_button1) <- TRUE
      #enabled(ld_button1) <- TRUE
      #enabled(pram_button1) <- TRUE
      #enabled(ls_button2) <- TRUE
      #enabled(vc_button1) <- TRUE
      #enabled(gr_button2) <- TRUE
    }else{
      #svalue(tab1) <- "not selected\n"
      #enabled(ir_button) <- FALSE
      #enabled(vk_button) <- FALSE
      #enabled(vh_button) <- FALSE
      #enabled(ls_button1) <- FALSE
      #enabled(ld_button1) <- FALSE
      #enabled(pram_button1) <- FALSE
      #enabled(ls_button2) <- TRUE
      #enabled(vc_button1) <- FALSE
      #enabled(gr_button2) <- FALSE
    }
    if(length(t2)>0){
      stmp <- ""
      for( i in 1:length(t2) ) {
        mi <- round(min(ActiveDataSet()[,t2[i]],na.rm=TRUE),1)
        ma <- round(max(ActiveDataSet()[,t2[i]],na.rm=TRUE),1)
        me <- round(median(ActiveDataSet()[,t2[i]],na.rm=TRUE),1)
        stmp <- paste(stmp, t2[i],"[Min:",mi,", Med:",me,", Max:",ma,"]\n")
      }
      #svalue(tab2) <- stmp
      #enabled(nm_button1) <- TRUE
      #enabled(shuffle_button1) <- TRUE
      #enabled(topcoding_button) <- TRUE
      #enabled(nm_button2) <- TRUE
      #enabled(nm_button3) <- TRUE
      #enabled(nm_risk_slider1) <- TRUE
      #enabled(nm_risk_slider2) <- TRUE
    }else{
      #svalue(tab2) <- "not selected\n"
      #enabled(nm_button1) <- FALSE
      #enabled(shuffle_button1) <- FALSE
      #enabled(topcoding_button) <- FALSE
      #enabled(nm_button2) <- FALSE
      #enabled(nm_button3) <- FALSE
      #enabled(nm_risk_slider1) <- FALSE
      #enabled(nm_risk_slider2) <- FALSE
    }
    #if(length(t3)>0){
    #  stmp <- ""
    #  for( i in 1:length(t3) ) {
    #    mi <- round(min(ActiveDataSet()[,t3[i]],na.rm=TRUE),1)
    #    ma <- round(max(ActiveDataSet()[,t3[i]],na.rm=TRUE),1)
    #    me <- round(median(ActiveDataSet()[,t3[i]],na.rm=TRUE),1)
    #    stmp <- paste(stmp, t3[i]," [Min:",mi,", Med:",me,", Max:",ma,"]\n",sep="")
    #  }
      #svalue(tab3) <- stmp
    #}else
      #svalue(tab3) <- "not selected\n"
    #if(length(t4)>0){
    #  stmp <- ""
    #  for( i in 1:length(t4) ) {
    #    me <- round(mean(by(ActiveDataSet()[,t4[i]],ActiveDataSet()[,t4[i]],length),na.rm=TRUE),1)
    #    stmp <- paste(stmp, t4[i]," [Mean size:",me,"]",sep="")
    #  }
      #svalue(tab4) <- stmp
    #}else 
      #svalue(tab4) <- "not selected\n"
    #if(length(t5)>0){
    #  stmp <- ""
    #  for( i in 1:length(t5) ) {
    #    stmp <- paste(stmp,t5[i]," [#:",length(unique(ActiveDataSet()[,t5[i]])),"]\n",sep="")
    #  }
      #svalue(tab5) <- stmp
    #}else 
      #svalue(tab5) <- "not selected\n"
    
    # enable plot indivRisk button
    freqCalcIndivRisk()
    nm_risk_print_function()
  }
  confirmSelection_tmp <- function(t1=character(0), t2=character(0), t3=character(0),t4=character(0),t5=character(0)) {
    selvar <- vector()
    if(length(t1)>0)
      selvar[length(selvar)+1] <- paste("keyVars=",parseVarStr(t1),sep="")
    if(length(t2)>0)
      selvar[length(selvar)+1] <- paste("numVars=",parseVarStr(t2),sep="")
    if(length(t3)>0)
      selvar[length(selvar)+1] <- paste("weightVar=",parseVarStr(t3),sep="")
    if(length(t4)>0)
      selvar[length(selvar)+1] <- paste("hhId=",parseVarStr(t4),sep="")
    if(length(t5)>0)
      selvar[length(selvar)+1] <- paste("strataVar=",parseVarStr(t5),sep="")
    selvar <- paste(selvar,collapse=",")
    if(existd("cmdimp")){
      Script.add(getd("cmdimp"))
      rmd("cmdimp")
    }
    Script.add(paste("sdcObject <- createSdcObj(activedataset,", selvar, ")", sep=""))
    xprogress = gwindow("please wait", width=180, height=40, parent=window)
    glabel("... script running ...", container=xprogress)
    if(existd("importFilename"))
      filename <- getd("importFilename")
    else
      filename <- getd("dataSetName")
    sdcObject <- createSdcObj(ActiveDataSet(),keyVars=t1,numVars=t2,weightVar=t3,hhId=t4,strataVar=t5,options=list(risk_k=0.01,risk_k2=0.05,filename=filename))
    dispose(xprogress)
    ActiveSdcObject(sdcObject)
    writeVars(t1,t2,t3,t4,t5)    
    
  }
  # variableSelectionGroup function
  #       if re-clicked, prompt and ask if you want to reset all work and script done
  # 			this is to be used to set dataset to start format as well as reset script,
  #				because it is not needed to reselect the vars during the work process
  confirmSelection <- function(...) {
    # open selection window
    #if(existd("sdcObject"))
    #  rmd("sdcObject")
    selVar()
  }
  
  ## Menubar Functions
  vign <- function(...) print(vignette("gui_tutorial"))
  vign2 <- function(...) print(vignette("guidelines"))
  paind <- function(...)print(help(package="sdcMicro"))
  
  
  # Data - Load Dataset
  loadDataSet <- function(...) {
    xname <- gfile("Select file to load", parent=window, type="open" ,filter=list("R-Data"=list(patterns=c("*.rda", "*.RData","*.RDA","*.rdata","*.RDATA")), "All files" = list(patterns = c("*"))))
    putd("importFilename",xname)
    putd("importFilenameType","R")
    if( xname != '' ) {
      load(xname, envir=.GlobalEnv)
    }
    setDataSet()
  }
  # Data - Choose Dataset
  setDataSet <- function(...) {
    vardt <- c("testdata","free1",ls(envir = .GlobalEnv, all.names=TRUE))
    vards <- names(which(sapply(vardt, function(.x) is.data.frame(get(.x)))))
    vards <- c(vards,names(which(sapply(vardt, function(.x) is.matrix(get(.x))))))
    if( length(vards)==0 ) {
      gmessage("No datasets loaded.", title="Information", icon="warning",
          parent=window)
    } else {
      gbasicdialog(title="Choose Dataset",
          x<-gdroplist(vards), parent=window,
          handler=function(x, ...) {
            Script.add(paste("activedataset <- ",svalue(x$obj),sep=""))
            
            ActiveDataSet(svalue(x$obj))
          })
      if( existd("activeDataSet") ) {
        if( dim(ActiveDataSet())[1] > 20000 ) {
          gmessage("Large data sets require extensive computation time, so please be patient.", title="Information",
              icon="info", parent=window)
        }
        #svalue(dslab) <- paste(getd("dataSetName")," [n=",nrow(ActiveDataSet()),"]",sep="")
        updateWindowTitle()
        #enabled(gb1) <- TRUE
        #enabled(gb2) <- TRUE
      }
      if(existd("sdcObject"))
        rmd("sdcObject")
      #selVar()
      CreateVariableManager()
    }
  }
  # Data - Save Dataset To - File
  saveToFile <- function(...) {
    saveVar <- function(fileName, ...) {
      xtmp <- sdcGUIoutput()
      save(xtmp, file=paste(fileName,".RData", sep=""))
    }
    if( existd("sdcObject") ) {
      xname <- gfile("Choose a file to save the Dataset", type="save", parent=window)
      if( xname != "" ) {
        saveVar(xname)
      }
    } else {
      gmessage("No active Dataset found.", title="Information", icon="warning",
          parent=window)
    }
  }
  # Data - Save Dataset To - Variable
#  saveToVariable <- function(...) {
#    checkAndSave <- function(parent, varName, ...) {
#      saveVar <- function(varName, ...) {
#        #assign(varName, ActiveDataSet(), envir=sdcGUIenv)
#      }
#      if( exists(varName, envir=.GlobalEnv) ) {
#        gconfirm("Variable already exists, dsetDataSet()o you want to replace it?",
#            title="Information", parent=parent,
#            handler=function(h, ...) { saveVar(varName) } )
#      } else {
#        saveVar(varName)
#      }
#    }
#    if( existd("activeDataSet") ) {
#      xname = ginput("Please enter a Variable name",
#          title="Choose Variable name", icon="question", parent=window,
#          handler=function(h, ...) checkAndSave(h$obj, h$input) )
#    } else {
#      gmessage("No active Dataset found.", title="Information", icon="warning",
#          parent=window)
#    }
#  }
#Saving to an R-Object from the GUI is not possibly due to CRAN policy!?
#TODO: Maybe there is a possible work around.
  
  saveToVariable <- function(...) {
    gmessage("Please use the function 'sdcGUIoutput' to assign the current dataset from the sdcGUI to a R-Object, e.g. 'datX <- sdcGUIoutput()'", title="Information", icon="warning",
        parent=window)
  }
  # Typ Dialog
  typDialog <- function(...){
    testimport <- getd("dframe")
    colclasses <- lapply(testimport, class)
    colname <- colnames(testimport)
    importDialog <- getd("importDialog")
    tDialog <- gwindow("Change Variable Types", width=50, height=50,parent=importDialog)
    tGroup <- ggroup(horizontal=FALSE, container=tDialog)
    tFrame <- gframe("select variable type:")
    gg <- glayout(use.scrollwindow = TRUE)
    comboboxes <- list()
    #maximal colums length
    rn <- 10
    for(i in 1:length(colclasses)){
      s <- 0
# 			  if(colclasses[i]=='numeric')s=1
# 			  else if(colclasses[i]=='factor')s=2
# 			  else if(colclasses[i]=='character')s=3
# 			  else if(colclasses[i]=='integer')s=4
      if(colclasses[i]=='numeric')s=1
      else if(colclasses[i]=='factor')s=2
      else if(colclasses[i]=='character')s=2
      else if(colclasses[i]=='integer')s=1
      #print(paste(colname[i],(i-1%%rn)+1))
      gg[((i-1)%%rn)+1,1+2*ceiling(i/rn), anchor=c(0,0)]<-glabel(colname[i])
# 			  gc <- gcombobox(items=c('numeric','factor', 'character', 'integer'), selected=s)
      gc <- gcombobox(items=c('numeric','factor'), selected=s)
      gg[((i-1)%%rn)+1,2+2*ceiling(i/rn)]<-gc
      comboboxes <- c(comboboxes, gc)
    }
    typeaccept <- gbutton("OK", handler=function(...){
          testimport <- getd("dframe")
          for(i in 1:length(colclasses)){
            colclasses[i]<-svalue(comboboxes[[i]])
          }
          #print(colclasses)
          #putd("dframe", testimport)
          putd("colclasses",colclasses)
          putd("changedTypes", TRUE)
          dispose(tDialog)
        })
    typediscard <- gbutton("Cancel ", handler=function(...){dispose(tDialog)})
# 		  gg[rn+1,1+2*ceiling(length(colclasses)/rn)] <- typeaccept
# 		  gg[rn+1,2+2*ceiling(length(colclasses)/rn)] <- typediscard
    add(tFrame, gg)
    add(tGroup, tFrame)
    tg <- ggroup(horizontal=TRUE)
    addSpring(tg)
    add(tg, typediscard)
    add(tg, typeaccept)
    add(tGroup, tg)
  }
  # Data - View
  viewDataset <- function(...){
    viewDatasetDialog <- gwindow("View Current Data", width=600, height=500)
    putd("viewDatasetDialog",viewDatasetDialog)
    viewDatasetDialogFrame <- ggroup(container=viewDatasetDialog, horizontal=FALSE)
    if(existd("sdcObject")) {
      sdcObject = ActiveSdcObject()
      data <- extractManipData(sdcObject)
      if(!is.null(sdcObject@deletedVars)) {
        data <- data[!names(data) %in% sdcObject@deletedVars]
      }
      svalue(viewDatasetDialog) <- paste("View Current Data:", ncol(data), "variables", nrow(data), "records", sep=" ")
      add(viewDatasetDialogFrame, gtable(data.frame(data)), expand=TRUE)
    } else {
      add(viewDatasetDialogFrame, gtable(data.frame(column="no available Dataset.")), expand=TRUE)
    }
  }

  # Compare Data set
  compareDataset <- function(...){
    compareDatasetDialog <- gwindow("Compare Dataset", width=600, height=500)
    putd("compareDatasetDialog",compareDatasetDialog)
    
    compareDatasetDialogFrame <- ggroup(container=compareDatasetDialog, horizontal=FALSE,expand=TRUE)
    if(existd("sdcObject")) {
      sdcObject = ActiveSdcObject()
      svalue(compareDatasetDialog) <- paste("Dataset:", ncol(sdcObject@origData), "variables", nrow(sdcObject@origData), "records", sep=" ")
      selectedNames <- c(names(sdcObject@manipKeyVars),names(sdcObject@manipNumVars))
      glayout <- glayout(container=compareDatasetDialogFrame,expand=TRUE)
      glayout[1,1] <- onlydiff <- gcheckbox("Only show difference", handler=function(h,...){
        sdcObject = ActiveSdcObject()
        data <- extractManipData(sdcObject)
        if(!is.null(sdcObject@deletedVars)) {
          data <- data[!names(data) %in% sdcObject@deletedVars]
        }
        selected <- svalue(variableList)
        newdf <- data[names(data) %in% selected]
        olddf <- sdcObject@origData[names(sdcObject@origData) %in% selected]
        updateDifference(glayout, newdf, olddf, svalue(h$obj))
        svalue(compareDatasetDialog) <- paste("Dataset:", ncol(sdcObject@origData), "variables", nrow(sdcObject@origData), "records", ", impacted obs:", length(getd("compare.dataset.diff")), sep=" ")
      })
      glayout[1,2] <- gbutton("Prev difference", handler=function(h,...){
        sdcObject = ActiveSdcObject()
        data <- extractManipData(sdcObject)
        if(!is.null(sdcObject@deletedVars)) {
          data <- data[!names(data) %in% sdcObject@deletedVars]
        }
        selected <- svalue(variableList)
        newdf <- data[names(data) %in% selected]
        olddf <- sdcObject@origData[names(sdcObject@origData) %in% selected]
        updateDifference(glayout, newdf, olddf, svalue(onlydiff), -1)
        svalue(compareDatasetDialog) <- paste("Dataset:", ncol(sdcObject@origData), "variables", nrow(sdcObject@origData), "records", ", impacted obs:", length(getd("compare.dataset.diff")), sep=" ")
      })      
      glayout[1,3] <- gbutton("Next difference", handler=function(h,...){
        sdcObject = ActiveSdcObject()
        data <- extractManipData(sdcObject)
        if(!is.null(sdcObject@deletedVars)) {
          data <- data[!names(data) %in% sdcObject@deletedVars]
        }
        selected <- svalue(variableList)
        newdf <- data[names(data) %in% selected]
        olddf <- sdcObject@origData[names(sdcObject@origData) %in% selected]
        updateDifference(glayout, newdf, olddf, svalue(onlydiff), 1)
        svalue(compareDatasetDialog) <- paste("Dataset:", ncol(sdcObject@origData), "variables", nrow(sdcObject@origData), "records", ", impacted obs:", length(getd("compare.dataset.diff")), sep=" ")
      })      
      glayout[2,1] <- glabel("Original Dataset")
      glayout[2,2] <- glabel("Current Dataset")
      glayout[2,3] <- glabel("Variable List")
      glayout[3,1,expand=TRUE] <- gtable(data.frame(numeric(0)))
      glayout[3,2,expand=TRUE] <- gtable(data.frame(numeric(0)))
      tmp <- gframe("Variables")
      variableListNames <- c(names(sdcObject@manipKeyVars), names(sdcObject@manipNumVars), 
                             names(sdcObject@origData)[!names(sdcObject@origData) %in% c(names(sdcObject@manipKeyVars), names(sdcObject@manipNumVars))])
      variableList <- gcheckboxgroup(variableListNames, container=tmp, use.table= TRUE, expand=TRUE, handler=function(h,...){
        sdcObject = ActiveSdcObject()
        data <- extractManipData(sdcObject)
        if(!is.null(sdcObject@deletedVars)) {
          data <- data[!names(data) %in% sdcObject@deletedVars]
        }
        selected <- svalue(variableList)
        newdf <- data[names(data) %in% selected]
        olddf <- sdcObject@origData[names(sdcObject@origData) %in% selected]
        updateDifference(glayout, newdf, olddf, svalue(onlydiff))
        svalue(compareDatasetDialog) <- paste("Dataset:", ncol(sdcObject@origData), "variables", nrow(sdcObject@origData), "records", ", impacted obs:", length(getd("compare.dataset.diff")), sep=" ")
      })
      svalue(variableList) <- names(sdcObject@origData) [names(sdcObject@origData) %in% selectedNames]
      data <- extractManipData(sdcObject)
      if(!is.null(sdcObject@deletedVars)) {
        data <- data[!names(data) %in% sdcObject@deletedVars]
      }
      selected <- svalue(variableList)
      newdf <- data[names(data) %in% selected]
      olddf <- sdcObject@origData[names(sdcObject@origData) %in% selected]
      updateDifference(glayout, newdf, olddf, svalue(onlydiff))
      svalue(compareDatasetDialog) <- paste("Dataset:", ncol(sdcObject@origData), "variables", nrow(sdcObject@origData), "records", ", impacted obs:", length(getd("compare.dataset.diff")), sep=" ")
      glayout[3,3] <- tmp
    } else {
      add(compareDatasetDialogFrame, gtable(data.frame(column="no available Dataset.")), expand=TRUE)
    }
  }
  
  # Compare Data set
  compareDatasetwithsteps <- function(comparedScript, step, ...){
    compareDatasetDialog <- gwindow("Compare Dataset to Original", width=600, height=500)
    putd("compareDatasetDialog",compareDatasetDialog)
  
    compareDatasetDialogFrame <- ggroup(container=compareDatasetDialog, horizontal=FALSE,expand=TRUE)
    if(existd("sdcObject")) {
      sdcObject <- NULL
      for( i in 1:length(comparedScript) ) {
        trycatch <- try(eval(parse(text=comparedScript[i])))
        if(class(trycatch)=="try-error"){
          sdcObject <- NULL
        }
      }
      
      data <- extractManipData(sdcObject)
      if(!is.null(sdcObject@deletedVars)) {
        data <- data[!names(data) %in% sdcObject@deletedVars]
      }
      
      svalue(compareDatasetDialog) <- paste("Dataset:", ncol(sdcObject@origData), "variables", nrow(sdcObject@origData), "records", sep=" ")
      glayout <- glayout(container=compareDatasetDialogFrame,expand=TRUE)
      glayout[1,1] <- onlydiff <- gcheckbox("Only show difference", handler=function(h,...){
        selected <- svalue(variableList)
        newdf <- data[names(data) %in% selected]
        olddf <- sdcObject@origData[names(sdcObject@origData) %in% selected]
        updateDifference(glayout, newdf, olddf, svalue(h$obj))
        svalue(compareDatasetDialog) <- paste("Dataset:", ncol(sdcObject@origData), "variables", nrow(sdcObject@origData), "records", ", impacted obs:", length(getd("compare.dataset.diff")), sep=" ")
      })
      glayout[1,2] <- gbutton("Prev difference", handler=function(h,...){
        selected <- svalue(variableList)
        newdf <- data[names(data) %in% selected]
        olddf <- sdcObject@origData[names(sdcObject@origData) %in% selected]
        updateDifference(glayout, newdf, olddf, svalue(onlydiff), -1)
        svalue(compareDatasetDialog) <- paste("Dataset:", ncol(sdcObject@origData), "variables", nrow(sdcObject@origData), "records", ", impacted obs:", length(getd("compare.dataset.diff")), sep=" ")
      })      
      glayout[1,3] <- gbutton("Next difference", handler=function(h,...){
        selected <- svalue(variableList)
        newdf <- data[names(data) %in% selected]
        olddf <- sdcObject@origData[names(sdcObject@origData) %in% selected]
        updateDifference(glayout, newdf, olddf, svalue(onlydiff), 1)
        svalue(compareDatasetDialog) <- paste("Dataset:", ncol(sdcObject@origData), "variables", nrow(sdcObject@origData), "records", ", impacted obs:", length(getd("compare.dataset.diff")), sep=" ")
      })      
      glayout[2,1] <- glabel("Original Dataset")
      glayout[2,2] <- glabel(paste("Dataset at Script Step ", step, ""))
      glayout[2,3] <- glabel("Variable List")
      glayout[3,1,expand=TRUE] <- gtable(data.frame(numeric(0)))
      glayout[3,2,expand=TRUE] <- gtable(data.frame(numeric(0)))
      tmp <- gframe("Variables")
      variableListNames <- c(names(sdcObject@manipKeyVars), names(sdcObject@manipNumVars), 
                             names(sdcObject@origData)[!names(sdcObject@origData) %in% c(names(sdcObject@manipKeyVars), names(sdcObject@manipNumVars))])
      variableList <- gcheckboxgroup(variableListNames, container=tmp, use.table= TRUE, expand=TRUE, handler=function(h,...){
        selected <- svalue(variableList)
        newdf <- data[names(data) %in% selected]
        olddf <- sdcObject@origData[names(sdcObject@origData) %in% selected]
        updateDifference(glayout, newdf, olddf, svalue(onlydiff))
        svalue(compareDatasetDialog) <- paste("Dataset:", ncol(sdcObject@origData), "variables", nrow(sdcObject@origData), "records", ", impacted obs:", length(getd("compare.dataset.diff")), sep=" ")
      })
      selectedNames <- c(names(sdcObject@manipKeyVars),names(sdcObject@manipNumVars))
      svalue(variableList) <- names(sdcObject@origData)[names(sdcObject@origData) %in% selectedNames]
      newdf <- data[names(data) %in% selectedNames]
      olddf <- sdcObject@origData[names(sdcObject@origData) %in% selectedNames]
      updateDifference(glayout, newdf, olddf, svalue(onlydiff))
      svalue(compareDatasetDialog) <- paste("Dataset:", ncol(sdcObject@origData), "variables", nrow(sdcObject@origData), "records", ", impacted obs:", length(getd("compare.dataset.diff")), sep=" ")
      glayout[3,3] <- tmp
    } else {
      add(compareDatasetDialogFrame, gtable(data.frame(column="no available Dataset.")), expand=TRUE)
    }
  }

  viewRiskandInfoLoss <- function(comparedScript, step, ...){
    viewRiskandInfoLossDialog <- gwindow("View Risk and Info Loss", width=800, height=500)
    putd("viewRiskandInfoLossDialog",viewRiskandInfoLossDialog)
  
    viewRiskandInfoLossDialogFrame <- ggroup(container=viewRiskandInfoLossDialog, horizontal=FALSE,expand=TRUE)
    if(existd("sdcObject")) {
      sdcObject <- NULL
      for( i in 1:length(comparedScript) ) {
        trycatch <- try(eval(parse(text=comparedScript[i])))
        if(class(trycatch)=="try-error"){
          sdcObject <- NULL
        }
      }
      sdcObject <- measure_risk(sdcObject)
      
      data <- extractManipData(sdcObject)
      if(!is.null(sdcObject@deletedVars)) {
        data <- data[!names(data) %in% sdcObject@deletedVars]
      }
      risk <- sdcObject@risk
      originalRisk <- sdcObject@originalRisk
      currenthighrisk <- sum((risk$individual[,1] > median(risk$individual[,1])+2*mad(risk$individual[,1])) & (risk$indiviual[,1] > 0.1))
      current2violate <- sum(risk$individual[,2]<2)
      current3violate <- sum(risk$individual[,2]<3)
      currentexpectedrisk <- round(risk$global$risk_ER,2)
      
      orighighrisk <- sum((originalRisk$individual[,1] > median(originalRisk$individual[,1])+2*mad(originalRisk$individual[,1])) & (originalRisk$indiviual[,1] > 0.1))
      orig2violate <- sum(originalRisk$individual[,2]<2)
      orig3violate <- sum(originalRisk$individual[,2]<3)
      origexpectedrisk <- round(originalRisk$global$risk_ER,2)
      CurrentCount = 0
      if(!is.null(risk$global$hier_risk_ER)) {
        CurrentCount = round(risk$global$hier_risk_ER,2)
      }
      CurrentPercentage = 0
      if(!is.null(risk$global$hier_risk_pct)) {
        CurrentPercentage = round(risk$global$hier_risk_pct,2)
      }
      OrigCount = 0
      if(!is.null(originalRisk$global$hier_risk_ER)) {
        OrigCount = round(originalRisk$global$hier_risk_ER,2)
      }
      OrigPercentage = 0
      if(!is.null(originalRisk$global$hier_risk_ER)) {
        OrigPercentage = round(originalRisk$global$hier_risk_pct,2)
      }
      n <- nrow(sdcObject@origData)
      
      svalue(viewRiskandInfoLossDialog) <- paste("Dataset at Script Step ", step, ":", ncol(sdcObject@origData), "variables", nrow(sdcObject@origData), "records", sep=" ")
      viewRiskandLossnbMain <- gnotebook(container=viewRiskandInfoLossDialogFrame, closebuttons=FALSE, expand=TRUE)
      viewRiskandLossGroupCat = ggroup(container=viewRiskandLossnbMain, horizontal=TRUE,label="Categorical", expand=TRUE)
      viewRiskandLossGroupCont = ggroup(container=viewRiskandLossnbMain, horizontal=TRUE,label="Continuous", expand=TRUE)
      svalue(viewRiskandLossnbMain) <- 1
      
      tmpCR = gframe('<span foreground="blue" size="large" weight="bold">Disclosure Risk</span>', container=viewRiskandLossGroupCat, horizontal=FALSE,markup=TRUE, expand=TRUE)
      tmpCU = gframe('<span foreground="blue" size="large" weight="bold">Information Loss</span>', container=viewRiskandLossGroupCat, horizontal=FALSE,markup=TRUE, expand=TRUE)
      
      fc_tmp = gframe('<span size="medium" weight="bold">Observations at risk of</span>', expand=TRUE,markup=TRUE,container= tmpCR)
      
      tmp = gframe("", container=fc_tmp,horizontal=FALSE, expand=TRUE)
      keyvariablerisktable = gtable(data.frame("number"=c("R1", "R2", "R3", "R4", "R5"), 
                                               "risk.calculations"=c("violating 2-anonymity", "violating 3-anonymity", "risk-higher than the benchmark", "Re-indentification, global risk", "Re-indentification, hierarchical risk"), 
                                               "curr.count"=c(current2violate, current3violate, currenthighrisk, currentexpectedrisk, CurrentCount), 
                                               "orig.count"=c(orig2violate, orig3violate, orighighrisk, origexpectedrisk, OrigCount),
                                               "curr.pct"=c(paste(round(current2violate/n * 100, 2), "%", SEP=""), paste(round(current3violate/n * 100, 2), "%", SEP=""), 
                                                 paste(round(currenthighrisk/n * 100, 2), "%", SEP=""), paste(round(currentexpectedrisk/n * 100, 2), "%", SEP=""),
                                                 paste(CurrentPercentage, "%", SEP="")), 
                                               "orig.pct"=c(paste(round(orig2violate/n * 100, 2), "%", SEP=""), paste(round(orig3violate/n * 100, 2), "%", SEP=""), 
                                                 paste(round(orighighrisk/n * 100, 2), "%", SEP=""), paste(round(origexpectedrisk/n * 100, 2), "%", SEP=""),paste(OrigPercentage, "%", SEP="")),stringsAsFactors=FALSE), container=tmp, width=280, height=250, expand=TRUE)
      keyvariableriskgraph = ggraphics(width=280, height=200)
      add(tmp, keyvariableriskgraph)
      counts <- c(round(current2violate/n * 100, 2), round(orig2violate/n * 100, 2), 
                  round(current3violate/n * 100, 2), round(orig3violate/n * 100, 2),
                  round(currenthighrisk/n * 100, 2), round(orighighrisk/n * 100, 2),
                  round(currentexpectedrisk/n * 100, 2),round(origexpectedrisk/n * 100, 2),
                  CurrentPercentage, OrigPercentage
      )
      
      keyvariableFreqGroup <- gframe('<span size="medium" weight="bold">10 combinations of categories with highest risk</span>',markup=TRUE, container=tmp, width=280, height=250, expand=TRUE,horizontal=FALSE)
      m1 <- sdcObject@risk$individual
      xtmp <- sdcObject@manipKeyVars
      tabDat <- cbind(xtmp,m1)
      ind <- !duplicated(apply(xtmp,1,function(x)paste(x,collapse="_")))
      tabDat <- tabDat[ind,]
      tabDat$risk <- round(tabDat$risk,5)
      tabDat <- tabDat[order(as.numeric(tabDat$risk),decreasing=TRUE),]
      if(nrow(tabDat) > 10) {
        tabDat <- tabDat[1:10,]
      }

      keyVars <- colnames(xtmp)
      if(is.null(sdcObject@localSuppression))
        lsup <- list(rep(0,length(keyVars)))
      else
        lsup <- sdcObject@localSuppression
      keyvariableFreqTable <- gtable(data.frame(apply(tabDat,2,function(x)as.character(x)),stringsAsFactors=FALSE), container=keyvariableFreqGroup, width=280, height=250, expand=TRUE)
      
      tmp = gframe('<span size="medium" weight="bold">Recodings</span>', container=tmpCU,markup=TRUE,horizontal=FALSE, expand=TRUE)
      
      recode_summary <- gtable(returnRecode(sdcObject), container=tmp,expand=TRUE) 
      tooltip(recode_summary)<- "Recoded values"
      
      tmp = gframe('<span size="medium" weight="bold">Suppressions</span>', container=tmpCU,markup=TRUE, expand=TRUE,horizontal=FALSE)
      keyvariablerecodetable = gtable(data.frame("Key Variables"=keyVars, "Suppressions"=c(do.call("cbind", lsup)),"Percent"=round(100 * c(do.call("cbind", lsup))/n,3),stringsAsFactors=FALSE), container=tmp, width=280, height=250, expand=TRUE)
      keyvariablerecodegraph = ggraphics(container=tmp, width=280, height=250)
      
      # Start - Continous Container
      svalue(viewRiskandLossnbMain) <- 2
      tmpR = gframe('<span foreground="blue" size="large" weight="bold">Disclosure Risk</span>', container=viewRiskandLossGroupCont,horizontal=FALSE,markup=TRUE,expand=TRUE)
      tmpU = gframe('<span foreground="blue" size="large" weight="bold">Information Loss</span>', container=viewRiskandLossGroupCont,horizontal=FALSE,markup=TRUE,expand=TRUE)
      
      origrisknum = 100
      if(!is.null(originalRisk$numeric)) {
        origrisknum = round(100*originalRisk$numeric,2)
      }
      currrisknum = 100
      if(!is.null(risk$numeric)) {
        currrisknum = round(100*risk$numeric,2)
      }
      continuousvariablerisklabel = glabel(text = paste("Disclosure risk for continuous key variables is:", currrisknum, 
                                                        "% (orig.", origrisknum, "%)", SEP=""), container=tmpR)
      continuousvariableriskgraph = ggraphics(container=tmpR, width=280, height=200)
      nm_util_print = gtext(text="", width=280, height=60, expand=TRUE, container=tmpU)
      if(!is.null(sdcObject@utility)) {
        svalue(nm_util_print) <- paste("Information Loss: Criteria IL1: ", 
                                       round(sdcObject@utility$il1,2), " (orig: 0)\nDifference in Eigenvalues:  ",
                                       round(sdcObject@utility$eigen*100,2)," %",
                                       " (orig: 0) \n",sep="")
      }
      tooltip(nm_util_print) <- tt_nmr
      visible(continuousvariableriskgraph) <- TRUE
      counts <- c(round(100*risk$numeric,2), origrisknum)
      bplt <- barplot(matrix(counts, nrow = 2, ncol = 1), beside = TRUE, main="Disclosure risk", 
                      space=4, names.arg=c("curr risk", "orig risk"))
      lossgraphcombo <- gcombobox(c(""), container=tmpU, handler=function(h, ...) {
        name <- svalue(h$obj)
        if(!is.null(name) && is.element(name, names(sdcObject@manipNumVars))) {
          visible(continuousvariablelossgraph) <- TRUE
          boxplot(c(sdcObject@manipNumVars[name], sdcObject@origData[name]), na.action= na.exclude, horizontal=TRUE)
        }
      })
      continuousvariablelossgraph = ggraphics(container=tmpU, width=280, height=200, expand=TRUE)
      visible(continuousvariablelossgraph) <- TRUE
      namelist <- names(sdcObject@manipNumVars)
      if(is.null(namelist)) {
        namelist <- c("")
      }
      lossgraphcombo[] <- namelist
      addHandlerChanged(viewRiskandLossnbMain, handler=function(h,...) {
        if(h$pageno == 1) {
          visible(keyvariableriskgraph) <- TRUE
          barplot(matrix(counts, nrow = 2, ncol = 5), beside = TRUE, main="Percentage of observations at risk", names.arg=c("R1", "R2","R3", "R4", "R5"))
          visible(keyvariablerecodegraph) <- TRUE
          barplot(c(do.call("cbind", lsup)), beside = TRUE, main="Suppressions", names.arg=keyVars, xlab="categorical key variables")
        } else {
          svalue(lossgraphcombo, index=TRUE) <- 1
        }
      })
      svalue(viewRiskandLossnbMain) <- 1
    } else {
      add(viewRiskandInfoLossDialogFrame, gtable(data.frame(column="no available Dataset.")), expand=TRUE)
    }
  }

  updateDifference <- function(glayout,newdf, olddf, checked, step=0,...) {
    newrows <- do.call("paste", newdf)
    oldrows <- do.call("paste", olddf)
    diff <- which(c(newrows == oldrows) %in% FALSE)
    putd("compare.dataset.diff",diff)
    if(checked) {
      glayout[3,2,expand=TRUE] <- gtable(data.frame(newdf[c(diff),]))
      glayout[3,1,expand=TRUE] <- gtable(data.frame(olddf[c(diff),]))
    } else {
      glayout[3,2,expand=TRUE] <- newtable <- gtable(data.frame(newdf))
      glayout[3,1,expand=TRUE] <- oldtable <- gtable(data.frame(olddf))
      if(length(diff) > 0) {
        index = 0
        if(existd("compare.dataset.index")) {
          index = getd("compare.dataset.index")
        
          index = index + step
          if( index > length(diff)) {
            index = 0
          } else if(index < 1) {
            index = length(diff)
          }
        }
        svalue(newtable) <- diff[index]
        svalue(oldtable) <- diff[index]
        putd("compare.dataset.index", index)
      }
    }
  }

  # Original Data - View
  viewOriginalDataset <- function(...){
    viewDatasetDialog <- gwindow("View Original Data", width=600, height=500)
    putd("viewDatasetDialog",viewDatasetDialog)
    viewDatasetDialogFrame <- ggroup(container=viewDatasetDialog, horizontal=FALSE)
    if(existd("sdcObject")) {
      sdcObject = ActiveSdcObject()
      svalue(viewDatasetDialog) <- paste("View Original Data:", ncol(sdcObject@origData), "variables", nrow(sdcObject@origData), "records", sep=" ")
      add(viewDatasetDialogFrame, gtable(data.frame(sdcObject@origData)), expand=TRUE)
    } else {
      add(viewDatasetDialogFrame, gtable(data.frame(column="no available Dataset.")), expand=TRUE)
    }
  }

  # Data - Import - Import CSV
  importCSV <- function(...){
    importDialog <- gwindow("Import CSV", parent=window, width=400, height=800)
    putd("importDialog",importDialog)
    putd("dframe", NULL)
    putd("changedTypes", FALSE)
    importDialogFrame <- ggroup(container=importDialog, horizontal=FALSE)
    layout <- glayout()
    csvfilename <- gedit()
    enabled(csvfilename) <- FALSE
    buttonHandler <- function(...){
      openlastcwd()
      gfile(text = "Open CSV File", type = "open", 
          filter=list("CSV files"=list(patterns=c("*.csv", "*.CSV")), "All files" = list(patterns = c("*"))),
          handler=function(h,...){
            svalue(csvfilename) <- h$file
            setlastcwd(dirname(h$file))
            tryCatch({
                  fl <- readLines(svalue(csvfilename), n=2)
                  comma <- sapply(strsplit(as.character(fl[1]), ","), length)
                  semicolon <- sapply(strsplit(as.character(fl[1]), ";"), length)
                  dot <- sum(sapply(strsplit(as.character(fl[2]), "."), length))
                  comma2 <- sum(sapply(strsplit(as.character(fl[2]), ","), length))
                  if(comma > semicolon){
                    svalue(csvseperator) <- ","
                    svalue(csvdecimal) <- "."
                  }else{
                    svalue(csvseperator) <- ";"
                    if(comma2 > dot){
                      svalue(csvdecimal) <- ","
                    }
                    else{
                      svalue(csvdecimal) <- "."
                    }
                  }
                },
                error=function(e){
                  gmessage(paste("There was a problem while preparing your data: '",e,"'"), "Problem",
                      icon="error")       
                })
            previewCSV()
          })
    }
    
    
    
    #creates the actual preview inside the table, also the handler for all gui elements
    #beside the OK-button
    previewCSV <- function(...){
      #testdata <- as.data.frame(matrix(rnorm(100), 10, 10))
      f <- gframe("Preview:")
      g <- ggroup(use.scrollwindow = TRUE)
      testimport <- NULL
      error <- FALSE
      if(svalue(csvfilename)==''){
        testimport <- data.frame(column="preview loading ...")
      }
      else{
        svalue(statusbar) <- "compiling preview!"
        tryCatch({testimport <- read.table(svalue(csvfilename), nrows=10,
                  fill=svalue(csvfill),
                  header=svalue(csvheader),
                  strip.white=svalue(csvstrip.white),
                  stringsAsFactors=svalue(csvstringsAsFactors),
                  blank.lines.skip=svalue(csvblank.lines.skip), 
                  sep=svalue(csvseperator),
                  dec=svalue(csvdecimal),
                  quote=svalue(csvquotes),
                  skip=svalue(csvskip),
                  na.strings=strsplit(svalue(csvnastrings),",")[[1]])
              putd("colclasses",NA)
              putd("changedTypes",FALSE)}, 
            error=function(e){svalue(statusbar) <- "read.table was not successful, please check your settings";
              error<-TRUE})
      }
      if(is.null(testimport)==FALSE){
        svalue(statusbar) <- "preview complete!"
      }
      else{
        testimport <- data.frame(column="preview loading ...")
      }
      add(g, gtable(testimport), expand=TRUE)
      add(f, g, expand=TRUE)
      layout[6:10, 1:7, expand=TRUE] <- f
      putd("dframe",testimport)
      
    }
    
    #setup csv import gui
    statusbar <- gstatusbar("")
    csvfilebutton <- gbutton("...", handler=buttonHandler)
    csvheader <- gcheckbox("header", checked=TRUE, handler=previewCSV)
    csvfill <- gcheckbox("fill", checked=TRUE, handler=previewCSV)
    csvstrip.white <- gcheckbox("strip white", , handler=previewCSV)
    csvblank.lines.skip <- gcheckbox("blank line skip", handler=previewCSV)
    csvstringsAsFactors <- gcheckbox("strings As Factors", handler=previewCSV)
    csvseperator <- gedit(",", handler=previewCSV)
    addHandlerKeystroke(csvseperator, previewCSV)
    csvdecimal <- gedit(".", handler=previewCSV)
    addHandlerKeystroke(csvdecimal, previewCSV)
    csvquotes <- gedit("\"", handler=previewCSV)
    addHandlerKeystroke(csvquotes, previewCSV)
    csvskip <- gedit("0")
    addHandlerKeystroke(csvskip, previewCSV)
    csvnastrings <- gedit("")
    addHandlerKeystroke(csvnastrings, previewCSV)
    csvaccept <- gbutton("OK", handler=function(...){
          ###real CSV import after pressing the accept button
          tryCatch({testimport <- getd("dframe")
                if(getd("changedTypes")==TRUE){
                  colclasses <- getd("colclasses")
                  colclassesSTR <- parseVarStr(colclasses)
                }  
                else{
                  colclasses <- NA
                  colclassesSTR <- "NA"
                }
                wd <- WaitingDialog(Parent=importDialog)
                focus(wd) <- TRUE
                putd("importFilename",svalue(csvfilename))
                putd("importFilenameType","CSV")
                filename=gsub("\\\\","/",svalue(csvfilename))
                df <- read.table(svalue(csvfilename),
                    fill=svalue(csvfill),
                    header=svalue(csvheader),
                    strip.white=svalue(csvstrip.white),
                    stringsAsFactors=svalue(csvstringsAsFactors),
                    blank.lines.skip=svalue(csvblank.lines.skip), 
                    sep=svalue(csvseperator),
                    dec=svalue(csvdecimal),
                    quote=svalue(csvquotes),
                    skip=svalue(csvskip),
                    colClasses=colclasses,
                    na.strings=strsplit(svalue(csvnastrings),",")[[1]])
                dname <- format(Sys.time(), "importedCSV_%H_%M")
                cmdimp <- paste("activedataset <- read.table(\"",filename,"\"",
                    ",fill=",svalue(csvfill), 
                    ",header=",svalue(csvheader),
                    ",strip.white=",svalue(csvstrip.white),
                    ",stringsAsFactors=",svalue(csvstringsAsFactors),
                    ",blank.lines.skip=",svalue(csvblank.lines.skip),
                    ",sep=",parseVarStr(svalue(csvseperator)),
                    ",dec=",parseVarStr(svalue(csvdecimal)),
                    ",quote=\"\\",svalue(csvquotes),"\"",
                    ",skip=",parseVarStr(svalue(csvskip)),
                    ",colClasses=",colclassesSTR,
                    ",na.strings=",parseVarStr(svalue(strsplit(svalue(csvnastrings),",")[[1]])),
                    ")", sep="")
                putd("cmdimp",cmdimp)
                putd("activeDataSet", df)
                putd("dataSetName",dname)
                putd("oldDataSet", ActiveDataSet())
                #svalue(dslab) <- paste(getd("dataSetName")," (n=",nrow(ActiveDataSet()),")",sep="")
                updateWindowTitle()
                #enabled(gb1) <- TRUE
                #enabled(gb2) <- TRUE
                putd("numLen", 0)
                putd("numVars", character(0))
                putd("keyLen", 0)
                putd("keyVars", character(0))
                putd("hLen", 0)
                putd("hVars", character(0))
                putd("wLen", 0)
                putd("wVars", character(0))
                putd("sLen", 0)
                putd("sVars", character(0))
                putd("importFileName", svalue(csvfilename))
                putd("importFilenameType","CSV")
                #save import parameters for later export
                csvimportparams <- list(fill=svalue(csvfill),
                    header=svalue(csvheader),
                    strip.white=svalue(csvstrip.white),
                    stringsAsFactors=svalue(csvstringsAsFactors),
                    blank.lines.skip=svalue(csvblank.lines.skip), 
                    sep=svalue(csvseperator),
                    dec=svalue(csvdecimal),
                    quote=svalue(csvquotes),
                    skip=svalue(csvskip),
                    colClasses=colclasses,
                    na.strings=strsplit(svalue(csvnastrings),",")[[1]])
                putd("csvimportparameters", csvimportparams)
                dispose(wd)
                dispose(importDialog)
                if(existd("sdcObject"))
                  rmd("sdcObject")
                selVar()
              },
              error=function(e){gmessage(paste("There was a problem while importing your data: '",e,"'"), "Problem",
                    icon="error")})
          
        })
    csvdiscard <- gbutton("Cancel ", handler=function(...){dispose(importDialog)})
    csvadjustTypes <- gbutton("Adjust Types", handler=typDialog)
    
    ftop <- gframe("Choose CSV-File:")
    gtop <- ggroup(horizontal=TRUE, container=ftop)
    add(ftop, csvfilename, expand=TRUE)
    add(ftop, csvfilebutton)
    layout[1,1:7] <- ftop
# 	  layout[1,1, anchor=c(0,0)] <- glabel("Choose CSV-File:")
# 	  layout[1,2:6, fill=TRUE] <- csvfilename
# 	  layout[1,7] <- csvfilebutton
    fparams <- gframe("CSV-Parameters:")
    glayout <- glayout(container=fparams)
    glayout[2,1] <- csvheader
    glayout[3,1] <- csvfill
    glayout[4,1] <- csvstrip.white
    glayout[5,1] <- csvstringsAsFactors
    glayout[2,2] <- csvblank.lines.skip
    glayout[2,3, anchor=c(0,0)] <- glabel("seperator:")
    glayout[2,4] <- csvseperator
    glayout[3,3, anchor=c(0,0)] <- glabel("decimal:")
    glayout[3,4] <- csvdecimal
    glayout[4,3, anchor=c(0,0)] <- glabel("quotes:")
    glayout[4,4] <- csvquotes
    glayout[5,3, anchor=c(0,0)] <- glabel("skip:")
    glayout[5,4] <- csvskip
    glayout[2,5, anchor=c(0,0)] <- glabel("NA-strings:")
    glayout[2,6, expand=FALSE] <- csvnastrings
    layout[2:5, 1:7] <- fparams
    previewCSV()
    layout[11,5, expand=FALSE] <- csvadjustTypes
    layout[11,6, expand=FALSE] <- csvaccept
    layout[11,7, expand=FALSE] <- csvdiscard
    add(importDialogFrame, layout, expand=TRUE)
    add(importDialogFrame, statusbar)
    buttonHandler()
    resetcwd()
  }
  
  
  # Data - Export - Export CSV
  exportCSV <- function(...){
    if(existd("sdcObject")  == FALSE){
      gmessage("There is no dataset loaded for export!", "No Dataset!",icon="warning")
    }
    else{
      importDialog <- gwindow("Export CSV", parent=window, width=200, height=200)
      putd("importDialog",importDialog)
      putd("dframe", NULL)
      importDialogFrame <- ggroup(container=importDialog, horizontal=FALSE)
      layout <- glayout()
      csvfilename <- gedit()
      enabled(csvfilename) <- FALSE
      
      buttonHandler <- function(...){
        gfile(text = "Save CSV File", type = "save", filter=list("CSV-Files"=list("*.csv")),handler=function(h,...){
              if(grepl("^.*\\.(csv|CSV)$", h$file)){
                svalue(csvfilename) <- h$file
              }
              else{
                svalue(csvfilename) <- paste(h$file, ".csv", sep="")
              }
            })
      }
      
      #setup csv import gui
      if(existd("csvimportparameters")){
        ip <- getd("csvimportparameters")
        ip$na.strings<-"NA"
      }
      else{
        ip <- list(fill=TRUE,
            header=FALSE,
            strip.white=TRUE,
            stringsAsFactors=TRUE,
            blank.lines.skip=TRUE, 
            sep=",",
            dec=".",
            quote="'",
            skip=0,
            colClasses=NULL,
            na.strings="NA")
      }
      
      statusbar <- gstatusbar("")
      csvfilebutton <- gbutton("...", handler=buttonHandler)
      csvheader <- gcheckbox("row names", checked=ip$header)
      csvseperator <- gedit(ip$sep)
      csvdecimal <- gedit(ip$dec)
      csvnastrings <- gedit(ip$na.strings)
      csvaccept <- gbutton("OK", handler=function(...){
            tryCatch({write.table(sdcGUIoutput(), file=svalue(csvfilename),
                      sep=svalue(csvseperator),
                      na=svalue(csvnastrings),
                      dec=svalue(csvdecimal),
                      row.names=svalue(csvheader))
                  putd("exportFileName", svalue(csvfilename))
                  if(svalue(radio.html, index=TRUE)==2)
                    exportReport()
                },
                error=function(e){gmessage(paste("There was a problem while exporting your data: '",e,"'"), "Problem",
                      icon="error")})
            dispose(importDialog)
          })
      csvdiscard <- gbutton("Cancel ", handler=function(...){dispose(importDialog)})
      
      #record export
      frame.html <- gframe("Generate report?")
      radio.html <- gradio(c("no", "yes"), 
          horizontal=TRUE, container=frame.html)
      
      
      ftop <- gframe("Choose CSV-File:")
      gtop <- ggroup(horizontal=TRUE, container=ftop)
      add(ftop, csvfilename, expand=TRUE)
      add(ftop, csvfilebutton)
      layout[1,1:7] <- ftop
      fparams <- gframe("CSV-Parameters:")
      glayout <- glayout(container=fparams)
      glayout[2,1] <- csvheader
      glayout[2,3, anchor=c(0,0)] <- glabel("seperator:")
      glayout[2,4] <- csvseperator
      glayout[3,3, anchor=c(0,0)] <- glabel("decimal:")
      glayout[3,4] <- csvdecimal
      glayout[2,5, anchor=c(0,0)] <- glabel("NA-strings:")
      glayout[2,6, expand=FALSE] <- csvnastrings
      layout[2:5, 1:7] <- fparams
      layout[8, 1:7] <- frame.html
      layout[9,6, expand=FALSE] <- csvaccept
      layout[9,7, expand=FALSE] <- csvdiscard
      add(importDialogFrame, layout, expand=TRUE)
      #add(importDialogFrame, statusbar)
      
    }
    buttonHandler()
  }
  
  # Data - Import - Import SPSS
  importSPSS <- function(...){
    importDialog <- gwindow("Import SPSS", parent=window, width=100, height=100)
    putd("importDialog",importDialog)
    putd("dframe", NULL)
    importDialogFrame <- ggroup(container=importDialog, horizontal=FALSE)
    layout <- glayout()
    filename <- gedit()
    enabled(filename) <- FALSE
    
    buttonHandler <- function(...){
      openlastcwd()
      gfile(text = "Open SPSS File", type = "open", , 
          filter=list("SPSS files"=list(patterns=c("*.sav", "*.SAV")),"All files" = list(patterns = c("*"))),
          handler=function(h,...){
            svalue(filename) <- h$file
            setlastcwd(dirname(h$file))
          })
    }
    
    #setup SPSS import gui
    statusbar <- gstatusbar("")
    filebutton <- gbutton("...", handler=buttonHandler)
    check.use.value.labels <- gcheckbox("convert value labels to factors")
    check.lowernames <- gcheckbox("convert variable names to lower case")
    check.force.single <- gcheckbox("force storage mode double to single", checked=TRUE)
    check.charfactor <- gcheckbox("convert character variables to factors")
    csvaccept <- gbutton("OK", handler=function(...){
          #try to import spss file, if not message error
          tryCatch({
                wd <- WaitingDialog(Parent=importDialog)
                focus(wd) <- TRUE
                df <- spss.get(svalue(filename),
                    use.value.labels = svalue(check.use.value.labels),
                    lowernames = svalue(check.lowernames),
                    force.single = svalue(check.force.single),
                    charfactor= svalue(check.charfactor),
                    to.data.frame = TRUE)
                putd("importFilename",svalue(filename))
                putd("importFilenameType","SPSS")
                filename=gsub("\\\\","/",svalue(filename))
                cmdimp <- paste("activedataset <- spss.get(\"",filename,"\"",
                    ",use.value.labels=",svalue(check.use.value.labels), 
                    ",lowernames=",svalue(check.lowernames),
                    ",force.single=",svalue(check.force.single),
                    ",charfactor=",svalue(check.charfactor),
                    ",to.data.frame = TRUE)", sep="")
                putd("cmdimp",cmdimp)
                dname <- format(Sys.time(), "importedSPSS_%H_%M")
                putd("activeDataSet", df)
                putd("dataSetName",dname)
                putd("oldDataSet", ActiveDataSet())
                #svalue(dslab) <- paste(getd("dataSetName")," (n=",nrow(ActiveDataSet()),")",sep="")
                updateWindowTitle()
                #enabled(gb1) <- TRUE
                #enabled(gb2) <- TRUE
                putd("numLen", 0)
                putd("numVars", character(0))
                putd("keyLen", 0)
                putd("keyVars", character(0))
                putd("hLen", 0)
                putd("hVars", character(0))
                putd("wLen", 0)
                putd("wVars", character(0))
                putd("sLen", 0)
                putd("sVars", character(0))
                dispose(wd)
                dispose(importDialog)
                if(existd("sdcObject"))
                  rmd("sdcObject")
                selVar()
                
              },error=function(e){
                gmessage(paste("There was a problem while importing your SPSS file: '",e,"'"),"Import Error!",icon="error")
              })
          
        })
    csvdiscard <- gbutton("Cancel ", handler=function(...){dispose(importDialog)})
    csvadjustTypes <- gbutton("Adjust Types", handler=typDialog)
    
    ftop <- gframe("Choose SPSS-File:")
    gtop <- ggroup(horizontal=TRUE, container=ftop)
    add(ftop, filename, expand=TRUE)
    add(ftop, filebutton)
    layout[1,1:7] <- ftop
    fparams <- gframe("SPSS-Parameters:")
    glayout <- glayout(container=fparams)
    glayout[1,1] <- check.use.value.labels
    glayout[1,2] <- check.lowernames
    glayout[2,1] <- check.force.single
    glayout[2,2] <- check.charfactor
    layout[2:3, 1:7] <- fparams
    layout[4,6, expand=FALSE] <- csvaccept
    layout[4,7, expand=FALSE] <- csvdiscard
    add(importDialogFrame, layout, expand=TRUE)
    buttonHandler()
    resetcwd()
  }
  
  # Data - Export - Export SPSS
  exportSPSS <- function(...){
    if(existd("activeDataSet")  == FALSE){
      gmessage("There is no dataset loaded for export!", "No Dataset!",icon="warning")
    }
    else{
      importDialog <- gwindow("Export SPSS", parent=window, width=100, height=100)
      putd("importDialog",importDialog)
      putd("dframe", NULL)
      importDialogFrame <- ggroup(container=importDialog, horizontal=FALSE)
      layout <- glayout()
      datafilename <- gedit()
      codefilename <- gedit()
      enabled(datafilename) <- FALSE
      enabled(codefilename) <- FALSE
      
      databuttonHandler <- function(...){
        gfile(text = "Save Data File", type = "save", handler=function(h,...){
              if(grepl("^.*\\.(dat)$", h$file)){
                svalue(datafilename) <- h$file
              }
              else{
                svalue(datafilename) <- paste(h$file, ".dat", sep="")
              }
            })
      }
      
      codebuttonHandler <- function(...){
        gfile(text = "Save SPS File", type = "save", filter=list(".sps"=list("*.sps")),handler=function(h,...){
              if(grepl("^.*\\.(sps|SPS)$", h$file)){
                svalue(codefilename) <- h$file
              }
              else{
                svalue(codefilename) <- paste(h$file, ".sps", sep="")
              }
            })
      }
      
      #setup sas export gui
      datafilebutton <- gbutton("...", handler=databuttonHandler)
      codefilebutton <- gbutton("...", handler=codebuttonHandler)
      csvaccept <- gbutton("OK", handler=function(...){
            tryCatch({write.foreign(sdcGUIoutput(), datafile=svalue(datafilename),
                      codefile=svalue(codefilename),
                      package = "SPSS")
                  putd("exportFileName", svalue(datafilename))
                  if(svalue(radio.html, index=TRUE)==2)
                    exportReport()
                },
                error=function(e){gmessage(paste("There was a problem while exporting your data: '",e,"'"), "Problem",
                      icon="error")})
            dispose(importDialog)
          })
      csvdiscard <- gbutton("Cancel ", handler=function(...){dispose(importDialog)})
      
      #record export
      frame.html <- gframe("Generate report?")
      radio.html <- gradio(c("no", "yes"), 
          horizontal=TRUE, container=frame.html)
      
      fdata <- gframe("Choose Data-File (Contains exported data as freetext):")
      gdata <- ggroup(horizontal=TRUE, container=fdata)
      add(fdata, datafilename, expand=TRUE)
      add(fdata, datafilebutton)
      layout[1,1:7] <- fdata
      
      fcode <- gframe("Choose Code-File (Contains SPSS Code for import):")
      gcode <- ggroup(horizontal=TRUE, container=fcode)
      add(fcode, codefilename, expand=TRUE)
      add(fcode, codefilebutton)
      layout[2,1:7] <- fcode
      layout[3, 1:7] <- frame.html
      layout[4,6, expand=FALSE] <- csvaccept
      layout[4,7, expand=FALSE] <- csvdiscard
      add(importDialogFrame, layout, expand=TRUE)
      #add(importDialogFrame, statusbar)
      
    }
    
  }

  newDataImportWithoutConfirm <- function(...){
  importDialog <- gwindow("Import Data", parent=window, width=300, height=300)
  putd("importDialog",importDialog)
  putd("dframe", NULL)
  importDialogFrame <- ggroup(container=importDialog, horizontal=FALSE, expand=TRUE)
  
  filename <- gedit()
  enabled(filename) <- FALSE
  buttonHandler <- function(...){
    openlastcwd()
    gfile(text = "Open Data File", 
          filter=list("R"=list(patterns=c("*.rda", "*.RData","*.RDA","*.rdata","*.RDATA")),
                      "STATA"=list(patterns=c("*.dta", "*.DTA")),
                      "CSV"=list(patterns=c("*.csv", "*.CSV")),
                      "SPSS"=list(patterns=c("*.sav", "*.SAV")),
                      "SAS"=list(patterns=c("*.xpt", "*.XPT")),
                      "All files" = list(patterns = c("*"))),
          type ="open", handler=function(h,...){
            visible(ftype) <- TRUE
            svalue(filename) <- h$file
            setlastcwd(dirname(h$file))
            if(file_ext(h$file) %in% c("rda", "RData", "RDA", "rdata", "RDATA")) {
              svalue(gfiletype) <- "R"
            } else if(file_ext(h$file) %in% c("dta", "DTA")) {
              svalue(gfiletype) <- "STATA"
            } else if(file_ext(h$file) %in% c("csv", "CSV")) {
              svalue(gfiletype) <- "CSV"
            }  else if(file_ext(h$file) %in% c("sav", "SAV")) {
              svalue(gfiletype) <- "SPSS"
            } else if(file_ext(h$file) %in% c("xpt", "XPT")) {
              svalue(gfiletype) <- "SAS"
            }
          })
  }
  #setup import gui
  statusbar <- gstatusbar("")
  filebutton <- gbutton("...", handler=buttonHandler)
  doimportaction <- function(...) {
    tryCatch({
      wd <- WaitingDialog(Parent=importDialog)
      focus(wd) <- TRUE
      type <- svalue(gfiletype)
      if(type == "R") {
        xname <- svalue(filename)
        putd("importFilename",xname)
        putd("importFilenameType","R")
        if( xname != '' ) {
          load(xname, envir=.GlobalEnv)
        }
        setDataSet()
        dispose(importDialog)
      } else {
        if(type == "STATA") {
          #start stata        
          putd("activeDataSet", read.dta(svalue(filename), convert.factors = svalue(stata.parameter.label.factors)))
          putd("importFilenameType","STATA")
          putd("dataSetName",format(Sys.time(), "importedSTATA_%H_%M"))        
          cmdimp <- paste("activedataset <- read.dta(\"",gsub("\\\\","/",svalue(filename)),"\"", ",convert.factors=",svalue(stata.parameter.label.factors),")",sep="")
          putd("cmdimp",cmdimp)
          #end stata
        } else if(type == "SPSS") {
          #start SPSS
          putd("activeDataSet",  spss.get(svalue(filename),
                                          use.value.labels = svalue(spss.parameter.labels.to.factors),
                                          lowernames = svalue(spss.parameter.lowernames),
                                          force.single = svalue(spss.parameter.force.single),
                                          charfactor= svalue(spss.parameter.charfactor),
                                          to.data.frame = TRUE))
          putd("importFilenameType","SPSS")
          cmdimp <- paste("activedataset <- spss.get(\"",gsub("\\\\","/",svalue(filename)),"\"",
                          ",use.value.labels=",svalue(spss.parameter.labels.to.factors), 
                          ",lowernames=",svalue(spss.parameter.lowernames),
                          ",force.single=",svalue(spss.parameter.force.single),
                          ",charfactor=",svalue(spss.parameter.charfactor),
                          ",to.data.frame = TRUE)", sep="")
          putd("cmdimp",cmdimp)
          putd("dataSetName",format(Sys.time(), "importedSPSS_%H_%M"))
          #end spss
        } else if(type == "CSV") {
          putd("importFilename",svalue(filename))
          putd("importFilenameType","CSV")
          if(existd("changedTypes") && getd("changedTypes")==TRUE){
            colclasses <- getd("colclasses")
            colclassesSTR <- parseVarStr(colclasses)
          }  
          else{
            colclasses <- NA
            colclassesSTR <- "NA"
          }
          df <- read.table(svalue(filename),
                           fill=svalue(csvfill),
                           header=svalue(csvheader),
                           strip.white=svalue(csvstrip.white),
                           stringsAsFactors=svalue(csvstringsAsFactors),
                           blank.lines.skip=svalue(csvblank.lines.skip), 
                           sep=svalue(csvseperator),
                           dec=svalue(csvdecimal),
                           quote=svalue(csvquotes),
                           skip=svalue(csvskip),
                           colClasses=colclasses,
                           na.strings=strsplit(svalue(csvnastrings),",")[[1]])
          dname <- format(Sys.time(), "importedCSV_%H_%M")
          cmdimp <- paste("activedataset <- read.table(\"",gsub("\\\\","/",svalue(filename)),"\"",
                          ",fill=",svalue(csvfill), 
                          ",header=",svalue(csvheader),
                          ",strip.white=",svalue(csvstrip.white),
                          ",stringsAsFactors=",svalue(csvstringsAsFactors),
                          ",blank.lines.skip=",svalue(csvblank.lines.skip),
                          ",sep=",parseVarStr(svalue(csvseperator)),
                          ",dec=",parseVarStr(svalue(csvdecimal)),
                          ",quote=\"\\",svalue(csvquotes),"\"",
                          ",skip=",parseVarStr(svalue(csvskip)),
                          ",colClasses=",colclassesSTR,
                          ",na.strings=",parseVarStr(svalue(strsplit(svalue(csvnastrings),",")[[1]])),
                          ")", sep="")
          putd("cmdimp",cmdimp)
          putd("activeDataSet", df)
          putd("dataSetName",dname)
        } else {
          #SAS
          putd("activeDataSet", sasxport.get(svalue(filename)))
          putd("importFilename",svalue(filename))
          putd("importFilenameType","SAS")
          putd("cmdimp",paste("activedataset <- sasxport.get(\"",gsub("\\\\","/",svalue(filename)),"\")",sep=""))
          putd("dataSetName",format(Sys.time(), "importedSAS_%H_%M"))
        }
        
        putd("importFilename",svalue(filename))        
        putd("oldDataSet", ActiveDataSet())
        #svalue(dslab) <- paste(getd("dataSetName")," (n=",nrow(ActiveDataSet()),")",sep="")
        updateWindowTitle()
        #enabled(gb1) <- TRUE
        #enabled(gb2) <- TRUE
        putd("numLen", 0)
        putd("numVars", character(0))
        putd("keyLen", 0)
        putd("keyVars", character(0))
        putd("hLen", 0)
        putd("hVars", character(0))
        putd("wLen", 0)
        putd("wVars", character(0))
        putd("sLen", 0)
        putd("sVars", character(0))
        dispose(wd)
        dispose(importDialog)
        if(existd("sdcObject"))
          rmd("sdcObject")
        #selVar()
        CreateVariableManager()
      }
    },error=function(e){
      gmessage(paste("There was a problem while importing your STATA file: ",e,"'"),"Import Error!",icon="error")
      dispose(importDialog)
    })
  }
  okaccept <- gbutton("OK", handler=function(...){
    if(existd("sdcObject")) {
      gconfirm("Do you want to reset the current dataset and import new dataset without saving?", icon="question", parent=window,
               handler=function(h,...) { 
                 Script.new()
                 putd("activescript.file", "Untitled Script")
                 svalue(leftFrameGroupLabel) <- "Untitled Script"
                 doimportaction()
    })
    } else {
      doimportaction()
    }
  })
  canceldiscard <- gbutton("Cancel ", handler=function(...){dispose(importDialog)})
  
  ftop <- gframe("Choose Data File:", expand=TRUE)
  add(ftop, filename, expand=TRUE)
  add(ftop, filebutton)
  ftype <- gframe("File Type:", expand=TRUE)
  
  fparams <- gframe("Data import parameters:", expand=TRUE, horizontal=FALSE)
  add(fparams, ftype)
  visible(ftype) <- FALSE
  fparamsgroup <- ggroup(container=fparams,  horizontal=FALSE, expand=TRUE)
  #stata
  statalayout <- glayout()
  stata.parameter.label.factors <- gcheckbox("convert value labels to factors", checked=TRUE) 
  statalayout[1,1] <- stata.parameter.label.factors
  #SPSS
  spsslayout <- glayout()
  spsslayout[1,1] <- spss.parameter.labels.to.factors <- gcheckbox("convert value labels to factors")
  spsslayout[2,1] <- spss.parameter.lowernames <- gcheckbox("convert variable names to lower case")
  spsslayout[3,1] <- spss.parameter.force.single <- gcheckbox("force storage mode double to single", checked=TRUE)
  spsslayout[4,1] <- spss.parameter.charfactor <- gcheckbox("convert character variables to factors")
  
  #CSV
  csvlayout <- glayout()
  csvlayout[1,1] <- csvheader <- gcheckbox("header", checked=TRUE)
  csvlayout[1,2] <- csvfill <- gcheckbox("fill", checked=TRUE)
  csvlayout[2,1] <- csvstrip.white <- gcheckbox("strip white",)
  csvlayout[2,2] <- csvstringsAsFactors <- gcheckbox("strings As Factors")
  csvlayout[3,1] <- csvblank.lines.skip <- gcheckbox("blank line skip")
  csvlayout[4,1] <- glabel("seperator:")
  csvlayout[4,2] <- csvseperator <- gedit(",")
  csvlayout[5,1] <- glabel("decimal:")
  csvlayout[5,2] <- csvdecimal <- gedit(".")
  csvlayout[6,1] <- glabel("quotes:")
  csvlayout[6,2] <- csvquotes <- gedit("\"")
  csvlayout[7,1] <- glabel("skip:")
  csvlayout[7,2] <- csvskip <- gedit("0")
  csvlayout[8,1] <- glabel("NA-strings:")
  csvlayout[8,2] <- csvnastrings <- gedit("")
  
  gfiletype <- gcombobox(c("R","STATA","SPSS","SAS", "CSV"), container=ftype,
                         handler=function(h, ...){
                           delete(fparamsgroup, statalayout)
                           delete(fparamsgroup, spsslayout)
                           delete(fparamsgroup, csvlayout)
                           delete(fbuttontool, previewCSVButton)
                           if(svalue(h$obj) == "STATA") {
                             add(fparamsgroup, statalayout)
                           } else if (svalue(h$obj) == "SPSS") {
                             add(fparamsgroup, spsslayout)
                           } else if (svalue(h$obj) == "CSV") {
                             add(fparamsgroup, csvlayout)
                             add(fbuttontool, previewCSVButton)
                           }
                         })
  importFileDialogFrame <- ggroup(container=importDialogFrame, horizontal=FALSE)
  add(importFileDialogFrame, ftop)
  #add(importFileDialogFrame, ftype)
  add(importDialogFrame, fparams)
  fbuttontool <- gframe("", container = importDialogFrame, horizontal=TRUE)
  previewCSVButton <-  gbutton("preViewCSV ", handler=function(...){previewCSV()})
  
  previewCSV <- function(...){
    f <- gwindow("Preview Data", parent=importDialog, width=400, height=400)
    g <- ggroup(use.scrollwindow = TRUE, horizontal=FALSE)
    testimport <- NULL
    error <- FALSE
    if(svalue(filename)==''){
      testimport <- data.frame(column="preview loading ...")
    } else{
      svalue(statusbar) <- "compiling preview!"
      tryCatch({testimport <- read.table(svalue(filename), nrows=10,
                                         fill=svalue(csvfill),
                                         header=svalue(csvheader),
                                         strip.white=svalue(csvstrip.white),
                                         stringsAsFactors=svalue(csvstringsAsFactors),
                                         blank.lines.skip=svalue(csvblank.lines.skip), 
                                         sep=svalue(csvseperator),
                                         dec=svalue(csvdecimal),
                                         quote=svalue(csvquotes),
                                         skip=svalue(csvskip),
                                         na.strings=strsplit(svalue(csvnastrings),",")[[1]])
                putd("colclasses",NA)
                putd("changedTypes",FALSE)}, 
               error=function(e){svalue(statusbar) <- "read.table was not successful, please check your settings";
                                 error<-TRUE})
    }
    if(is.null(testimport)==FALSE){
      svalue(statusbar) <- "preview complete!"
    }
    else{
      testimport <- data.frame(column="preview loading ...")
    }
    add(g, gtable(testimport), expand=TRUE)
    csvadjustTypes <- gbutton("Adjust Types", handler=typDialog)
    add(g, csvadjustTypes)
    
    add(f, g, expand=TRUE)
    putd("dframe",testimport)
    putd("importDialog",f)
  }
  
  add(fbuttontool, canceldiscard)
  add(fbuttontool, okaccept)
  resetcwd()
  }

  newDataImport <- function(...){
    newDataImportWithoutConfirm()
  }

  newDataExport <- function(...){
    if(existd("sdcObject")  == FALSE){
      gmessage("There is no dataset loaded for export!", "No Dataset!",icon="warning")
    } else{
      importDialog <- gwindow("Export Data", parent=window, width=100, height=100)
      putd("importDialog",importDialog)
      putd("dframe", NULL)
      importDialogFrame <- ggroup(container=importDialog, horizontal=FALSE)
      layout <- glayout()
      filename <- gedit(handler=function(...){
        delete(fparams, csvlayout)
        delete(fparams, statalayout)
        delete(fparams, spsslayout)
        delete(fparams, saslayout)
        enabled(radio.html) <- TRUE
        if(file_ext(svalue(filename)) %in% c("rda", "RData", "RDA", "rdata", "RDATA")) {
          putd("exportDatafileType", "R")
          enabled(radio.html) <- FALSE
        } else if(file_ext(svalue(filename)) %in% c("dta", "DTA")) {
          putd("exportDatafileType", "STATA")
          add(fparams, statalayout)
        } else if(file_ext(svalue(filename)) %in% c("sav", "SAV")) {
          putd("exportDatafileType",  "SPSS")
          add(fparams, spsslayout)
        } else if(file_ext(svalue(filename)) %in% c("xpt", "XPT")) {
          putd("exportDatafileType",  "SAS")
          add(fparams, saslayout)
        } else {
          putd("exportDatafileType",  "CSV")
          add(fparams, csvlayout)
        }
        svalue(importDialog) <- paste("Export", getd("exportDatafileType"), "Data", sep=" ")
      })
    enabled(filename) <- FALSE
    
    buttonHandler <- function(...){
      gfile(text = "Save AS R(*.rdata), STATA(*.dta), CSV(*.csv), SPSS(*.sav), SAS(*.xpt)", 
            initialfilename = "new file.csv",
            filter=list(),
            type = "save", handler=function(h,...){
              svalue(filename) <- h$file
              setlastcwd(dirname(h$file))
            })
    }
    
    #setup stata export gui
    ftop <- gframe("Save As File:")
    gtop <- ggroup(horizontal=TRUE, container=ftop)
    add(ftop, filename, expand=TRUE)
    add(ftop, gbutton("...", handler=buttonHandler))
    layout[1,1:7] <- ftop
    
    fparams <- gframe("Data export parameters:", expand=TRUE)
    
    csvaccept <- gbutton(  "OK", handler=function(...){
      tryCatch({
        fileType <- getd("exportDatafileType")
        if(fileType == "STATA") {
          write.dta(sdcGUIoutput(), file=svalue(filename),
                    version=as.numeric(svalue(stata.edit.version)),
                    convert.dates=svalue(stata.check.dates),
                    convert.factors=svalue(stata.combo.convert.factors))
          putd("exportFileName", svalue(filename))
          if(svalue(radio.html, index=TRUE)==2) {
            exportReport()
          }
        } else if(fileType == "R") {
          if( existd("sdcObject") ) {
            obj <- sdcGUIoutput()
            save(obj, file=svalue(filename))
          }
        } else if(fileType == "CSV") {
          write.table(sdcGUIoutput(), file=svalue(filename),
                      sep=svalue(csvseperator),
                      na=svalue(csvnastrings),
                      dec=svalue(csvdecimal),
                      row.names=svalue(csvheader))
          putd("exportFileName", svalue(filename))
          if(svalue(radio.html, index=TRUE)==2)
            exportReport()
        } else if(fileType == "SPSS") {
          datafilename <- gsub("(sas|SAS)$", "dat", svalue(filename))
          if(svalue(spss.generate.code.file)) {
            codefilename <- svalue(filename)
          } else {
            codefilename <- ""
          }
          write.foreign(sdcGUIoutput(), datafile=datafilename,
                        codefile=codefilename,
                        package = "SPSS")
          putd("exportFileName", datafilename)
          if(svalue(radio.html, index=TRUE)==2) {
            exportReport()
          }
        } else if(fileType == "SAS") {
          version <- paste("V",substr(svalue(sas.combo.validvarname), 3,3), sep="")
          datafilename <- gsub("(xpt|XPT)$", "dat", svalue(filename))
          if(svalue(sas.generate.code.file)) {
            codefilename <- svalue(filename)
          } else {
            codefilename <- ""
          }
          write.foreign(sdcGUIoutput(), datafile=datafilename,
                        codefile=codefilename,
                        package = "SAS",
                        dataname = svalue(sas.edit.dataname),
                        validvarname = version)
          putd("exportFileName", datafilename)
          if(svalue(radio.html, index=TRUE)==2)
            exportReport()
        }
      },
      error=function(e){gmessage(paste("There was a problem while exporting your data: '",e,"'"), "Problem",icon="error")})
      dispose(importDialog)
    })
    csvdiscard <- gbutton("Cancel ", handler=function(...){dispose(importDialog)})
    
    #record export
    frame.html <- gframe("Generate report?")
    radio.html <- gradio(c("no", "yes"),horizontal=TRUE, container=frame.html)
    
    statalayout <- glayout()
    statalayout[1,1] <- stata.check.dates<- gcheckbox("convert dates to STATA-dates", checked=TRUE)
    statalayout[1,2, anchor=c(0,0)] <- glabel("version:")
    statalayout[1,6, expand=FALSE] <- stata.edit.version <- gedit("7")
    statalayout[2,2, anchor=c(0,0)] <- glabel("handle factors as:")
    statalayout[2,6, expand=FALSE] <- stata.combo.convert.factors<- gcombobox(c("labels","string","numeric","codes"))
    
    csvlayout <- glayout()
    #setup csv import gui
    if(existd("csvimportparameters")){
      ip <- getd("csvimportparameters")
      ip$na.strings<-"NA"
    } else{
      ip <- list(fill=TRUE,
                 header=FALSE,
                 strip.white=TRUE,
                 stringsAsFactors=TRUE,
                 blank.lines.skip=TRUE, 
                 sep=",",
                 dec=".",
                 quote="'",
                 skip=0,
                 colClasses=NULL,
                 na.strings="NA")
    }
    csvlayout[1,1] <- csvheader <- gcheckbox("row names", checked=ip$header)
    csvlayout[2,1, anchor=c(0,0)] <- glabel("seperator:")
    csvlayout[2,2] <- csvseperator <- gedit(ip$sep)
    csvlayout[3,1, anchor=c(0,0)] <- glabel("decimal:")
    csvlayout[3,2] <- csvdecimal <- gedit(ip$dec)
    csvlayout[4,1, anchor=c(0,0)] <- glabel("NA-strings:")
    csvlayout[4,2, expand=FALSE] <- csvnastrings<- gedit(ip$na.strings)
    
    spsslayout <- glayout()
    spsslayout[1,1] <- spss.generate.code.file<- gcheckbox("generate SPSS code file")
    spsslayout[2,1] <- spss.check.use.value.labels<- gcheckbox("convert value labels to factors")
    spsslayout[2,2] <- spss.check.lowernames<- gcheckbox("convert variable names to lower case")
    spsslayout[3,1] <- spss.check.force.single<- gcheckbox("force storage mode double to single", checked=TRUE)
    spsslayout[3,2] <- spss.check.charfactor<- gcheckbox("convert character variables to factors")
    
    saslayout <- glayout()
    saslayout[1,1] <- sas.generate.code.file<- gcheckbox("generate SAS code file")
    saslayout[2,1, anchor=c(-1,0)] <- glabel("future SAS data set name:")
    saslayout[2,2, expand=FALSE] <- sas.edit.dataname  <- gedit("rdata")
    saslayout[3,1, anchor=c(-1,0)] <- glabel("SAS version :")
    saslayout[3,2, expand=FALSE] <- sas.combo.validvarname <- gcombobox(c("<=6",">=7"), selected=2)
    
    layout[2:3, 1:7] <- fparams
    layout[4, 1:7] <- frame.html
    layout[5,6, expand=FALSE] <- csvaccept
    layout[5,7, expand=FALSE] <- csvdiscard      
    add(importDialogFrame, layout, expand=TRUE)
    #add(importDialogFrame, statusbar)
    }
  }

compareDataExport <- function(comparedScript, len, ...){
  if(existd("sdcObject")  == FALSE){
    gmessage("There is no dataset loaded for export!", "No Dataset!",icon="warning")
  } else{
    importDialog <- gwindow("Run until and Export Dataset", parent=window, width=100, height=100)
    putd("importDialog",importDialog)
    putd("dframe", NULL)
    importDialogFrame <- ggroup(container=importDialog, horizontal=FALSE)
    layout <- glayout()
    filename <- gedit(handler=function(...){
      delete(fparams, csvlayout)
      delete(fparams, statalayout)
      delete(fparams, spsslayout)
      delete(fparams, saslayout)
      enabled(radio.html) <- TRUE
      if(file_ext(svalue(filename)) %in% c("rda", "RData", "RDA", "rdata", "RDATA")) {
        putd("exportDatafileType", "R")
        enabled(radio.html) <- FALSE
      } else if(file_ext(svalue(filename)) %in% c("dta", "DTA")) {
        putd("exportDatafileType", "STATA")
        add(fparams, statalayout)
      } else if(file_ext(svalue(filename)) %in% c("sav", "SAV")) {
        putd("exportDatafileType",  "SPSS")
        add(fparams, spsslayout)
      } else if(file_ext(svalue(filename)) %in% c("xpt", "XPT")) {
        putd("exportDatafileType",  "SAS")
        add(fparams, saslayout)
      } else {
        putd("exportDatafileType",  "CSV")
        add(fparams, csvlayout)
      }
      svalue(importDialog) <- paste("Export", getd("exportDatafileType"), "Data", sep=" ")
    })
    enabled(filename) <- FALSE
    
    buttonHandler <- function(...){
      gfile(text = "Save AS R(*.rdata), STATA(*.dta), CSV(*.csv), SPSS(*.sav), SAS(*.xpt)", 
            initialfilename = "new file.csv",
            filter=list(),
            type = "save", handler=function(h,...){
              svalue(filename) <- h$file
              setlastcwd(dirname(h$file))
            })
    }
    activedataset <- ""
    sdcObject <- NULL
    for( i in 1:length(comparedScript) ) {
      trycatch <- try(eval(parse(text=comparedScript[i])))
      if(class(trycatch)=="try-error"){
        sdcObject <- NULL
      }
    }
    
    #setup stata export gui
    ftop <- gframe("Save As File:")
    gtop <- ggroup(horizontal=TRUE, container=ftop)
    add(ftop, filename, expand=TRUE)
    add(ftop, gbutton("...", handler=buttonHandler))
    layout[1,1:7] <- ftop
    
    fparams <- gframe("Data export parameters:", expand=TRUE)
    
    csvaccept <- gbutton(  "OK", handler=function(...){
      tryCatch({
        fileType <- getd("exportDatafileType")
        if(fileType == "STATA") {
          write.dta(sdcGUIoutput(), file=svalue(filename),
                    version=as.numeric(svalue(stata.edit.version)),
                    convert.dates=svalue(stata.check.dates),
                    convert.factors=svalue(stata.combo.convert.factors))
          putd("exportFileName", svalue(filename))
          if(svalue(radio.html, index=TRUE)==2) {
            exportReport()
          }
        } else if(fileType == "R") {
          if( existd("sdcObject") ) {
            obj <- sdcGUIoutput(sdcObject)
            save(obj, file=svalue(filename))
          }
        } else if(fileType == "CSV") {
          write.table(sdcGUIoutput(sdcObject), file=svalue(filename),
                      sep=svalue(csvseperator),
                      na=svalue(csvnastrings),
                      dec=svalue(csvdecimal),
                      row.names=svalue(csvheader))
          putd("exportFileName", svalue(filename))
          if(svalue(radio.html, index=TRUE)==2)
            exportReport()
        } else if(fileType == "SPSS") {
          datafilename <- gsub("(sas|SAS)$", "dat", svalue(filename))
          if(svalue(spss.generate.code.file)) {
            codefilename <- svalue(filename)
          } else {
            codefilename <- ""
          }
          write.foreign(sdcGUIoutput(sdcObject), datafile=datafilename,
                        codefile=codefilename,
                        package = "SPSS")
          putd("exportFileName", datafilename)
          if(svalue(radio.html, index=TRUE)==2) {
            exportReport()
          }
        } else if(fileType == "SAS") {
          version <- paste("V",substr(svalue(sas.combo.validvarname), 3,3), sep="")
          datafilename <- gsub("(xpt|XPT)$", "dat", svalue(filename))
          if(svalue(sas.generate.code.file)) {
            codefilename <- svalue(filename)
          } else {
            codefilename <- ""
          }
          write.foreign(sdcGUIoutput(sdcObject), datafile=datafilename,
                        codefile=codefilename,
                        package = "SAS",
                        dataname = svalue(sas.edit.dataname),
                        validvarname = version)
          putd("exportFileName", datafilename)
          if(svalue(radio.html, index=TRUE)==2)
            exportReport()
        }
      },
      error=function(e){gmessage(paste("There was a problem while exporting your data: '",e,"'"), "Problem",icon="error")})
      dispose(importDialog)
    })
    csvdiscard <- gbutton("Cancel ", handler=function(...){dispose(importDialog)})
    
    #record export
    frame.html <- gframe("Generate report?")
    radio.html <- gradio(c("no", "yes"),horizontal=TRUE, container=frame.html)
    
    statalayout <- glayout()
    statalayout[1,1] <- stata.check.dates<- gcheckbox("convert dates to STATA-dates", checked=TRUE)
    statalayout[1,2, anchor=c(0,0)] <- glabel("version:")
    statalayout[1,6, expand=FALSE] <- stata.edit.version <- gedit("7")
    statalayout[2,2, anchor=c(0,0)] <- glabel("handle factors as:")
    statalayout[2,6, expand=FALSE] <- stata.combo.convert.factors<- gcombobox(c("labels","string","numeric","codes"))
    
    csvlayout <- glayout()
    #setup csv import gui
    if(existd("csvimportparameters")){
      ip <- getd("csvimportparameters")
      ip$na.strings<-"NA"
    } else{
      ip <- list(fill=TRUE,
                 header=FALSE,
                 strip.white=TRUE,
                 stringsAsFactors=TRUE,
                 blank.lines.skip=TRUE, 
                 sep=",",
                 dec=".",
                 quote="'",
                 skip=0,
                 colClasses=NULL,
                 na.strings="NA")
    }
    csvlayout[1,1] <- csvheader <- gcheckbox("row names", checked=ip$header)
    csvlayout[2,1, anchor=c(0,0)] <- glabel("seperator:")
    csvlayout[2,2] <- csvseperator <- gedit(ip$sep)
    csvlayout[3,1, anchor=c(0,0)] <- glabel("decimal:")
    csvlayout[3,2] <- csvdecimal <- gedit(ip$dec)
    csvlayout[4,1, anchor=c(0,0)] <- glabel("NA-strings:")
    csvlayout[4,2, expand=FALSE] <- csvnastrings<- gedit(ip$na.strings)
    
    spsslayout <- glayout()
    spsslayout[1,1] <- spss.generate.code.file<- gcheckbox("generate SPSS code file")
    spsslayout[2,1] <- spss.check.use.value.labels<- gcheckbox("convert value labels to factors")
    spsslayout[2,2] <- spss.check.lowernames<- gcheckbox("convert variable names to lower case")
    spsslayout[3,1] <- spss.check.force.single<- gcheckbox("force storage mode double to single", checked=TRUE)
    spsslayout[3,2] <- spss.check.charfactor<- gcheckbox("convert character variables to factors")
    
    saslayout <- glayout()
    saslayout[1,1] <- sas.generate.code.file<- gcheckbox("generate SAS code file")
    saslayout[2,1, anchor=c(-1,0)] <- glabel("future SAS data set name:")
    saslayout[2,2, expand=FALSE] <- sas.edit.dataname  <- gedit("rdata")
    saslayout[3,1, anchor=c(-1,0)] <- glabel("SAS version :")
    saslayout[3,2, expand=FALSE] <- sas.combo.validvarname <- gcombobox(c("<=6",">=7"), selected=2)
    
    layout[2:3, 1:7] <- fparams
    layout[4, 1:7] <- frame.html
    #layout[5,1, expand=TRUE] <- gbutton("Compare datasets", handler=function(...){compareDataset()})
    #layout[5,2, expand=TRUE] <- gbutton("Compare summaries",
    #                                    handler=function(...){compareSummaries(sdcObject, len)})
    layout[5,6, expand=FALSE] <- csvaccept
    layout[5,7, expand=FALSE] <- csvdiscard
    add(importDialogFrame, layout, expand=TRUE)
  }
}

  # Data - Import - Import STATA
  loadSTATA <- function(...){
    importDialog <- gwindow("Import STATA", parent=window, width=100, height=100)
    putd("importDialog",importDialog)
    putd("dframe", NULL)
    importDialogFrame <- ggroup(container=importDialog, horizontal=FALSE)
    layout <- glayout()
    filename <- gedit()
    enabled(filename) <- FALSE
    buttonHandler <- function(...){
      openlastcwd()
      gfile(text = "Open STATA File", 
          filter=list("STATA files"=list(patterns=c("*.dta", "*.DTA")),"All files" = list(patterns = c("*"))),
          type = "open", handler=function(h,...){
            svalue(filename) <- h$file
            setlastcwd(dirname(h$file))
          })
    }
    #setup STATA import gui
    statusbar <- gstatusbar("")
    filebutton <- gbutton("...", handler=buttonHandler)
    check.use.value.labels <- gcheckbox("convert value labels to factors", checked=TRUE)
    csvaccept <- gbutton("OK", handler=function(...){
          #try to import stata file, if not message error
          tryCatch({
                wd <- WaitingDialog(Parent=importDialog)
                focus(wd) <- TRUE
                
                df <- read.dta(svalue(filename),
                    convert.factors = svalue(check.use.value.labels))
                putd("importFilename",svalue(filename))
                putd("importFilenameType","STATA")
                filename=gsub("\\\\","/",svalue(filename))
                cmdimp <- paste("activedataset <- read.dta(\"",filename,"\"",
                    ",convert.factors=",svalue(check.use.value.labels),")",sep="")
                putd("cmdimp",cmdimp)
                dname <- format(Sys.time(), "importedSTATA_%H_%M")
                putd("activeDataSet", df)
                putd("dataSetName",dname)
                putd("oldDataSet", ActiveDataSet())
                #svalue(dslab) <- paste(getd("dataSetName")," (n=",nrow(ActiveDataSet()),")",sep="")
                updateWindowTitle()
                #enabled(gb1) <- TRUE
                #enabled(gb2) <- TRUE
                putd("numLen", 0)
                putd("numVars", character(0))
                putd("keyLen", 0)
                putd("keyVars", character(0))
                putd("hLen", 0)
                putd("hVars", character(0))
                putd("wLen", 0)
                putd("wVars", character(0))
                putd("sLen", 0)
                putd("sVars", character(0))
                dispose(wd)
                dispose(importDialog)
                if(existd("sdcObject"))
                  rmd("sdcObject")
                selVar()
                
              },error=function(e){
                gmessage(paste("There was a problem while importing your STATA file: ",e,"'"),"Import Error!",icon="error")
              })
          
        })
    csvdiscard <- gbutton("Cancel ", handler=function(...){dispose(importDialog)})
    csvadjustTypes <- gbutton("Adjust Types", handler=typDialog)
    
    ftop <- gframe("Choose STATA-File:")
    gtop <- ggroup(horizontal=TRUE, container=ftop)
    add(ftop, filename, expand=TRUE)
    add(ftop, filebutton)
    layout[1,1:7] <- ftop
    fparams <- gframe("STATA-Parameters:")
    glayout <- glayout(container=fparams)
    glayout[1,1] <- check.use.value.labels
    layout[2, 1:7] <- fparams
    layout[3,6, expand=FALSE] <- csvaccept
    layout[3,7, expand=FALSE] <- csvdiscard
    add(importDialogFrame, layout, expand=TRUE)
    buttonHandler()
    resetcwd()
  }
  
  # Data - Export - Export STATA
  exportSTATA <- function(...){
    if(existd("sdcObject")  == FALSE){
      gmessage("There is no dataset loaded for export!", "No Dataset!",icon="warning")
    }
    else{
      importDialog <- gwindow("Export STATA", parent=window, width=100, height=100)
      putd("importDialog",importDialog)
      putd("dframe", NULL)
      importDialogFrame <- ggroup(container=importDialog, horizontal=FALSE)
      layout <- glayout()
      filename <- gedit()
      enabled(filename) <- FALSE
      edit.version <- gedit("7")
      check.dates <- gcheckbox("convert dates to STATA-dates", checked=TRUE)
      combo.convert.factors <- gcombobox(c("labels","string","numeric","codes"))
      
      buttonHandler <- function(...){
        gfile(text = "Save STATA File", type = "save", filter=list(".dta"=list("*.dta")),handler=function(h,...){
              if(grepl("^.*\\.(dta|DTA)$", h$file)){
                svalue(filename) <- h$file
              }
              else{
                svalue(filename) <- paste(h$file, ".dta", sep="")
              }
            })
      }
      
      #setup stata export gui
      filebutton <- gbutton("...", handler=buttonHandler)
      csvaccept <- gbutton(	"OK", handler=function(...){
            tryCatch({write.dta(sdcGUIoutput(), file=svalue(filename),
                      version=as.numeric(svalue(edit.version)),
                      convert.dates=svalue(check.dates),
                      convert.factors=svalue(combo.convert.factors))
                  putd("exportFileName", svalue(filename))
                  if(svalue(radio.html, index=TRUE)==2)
                    exportReport()
                },
                error=function(e){gmessage(paste("There was a problem while exporting your data: '",e,"'"), "Problem",
                      icon="error")})
            dispose(importDialog)
          })
      csvdiscard <- gbutton("Cancel ", handler=function(...){dispose(importDialog)})
      
      #record export
      frame.html <- gframe("Generate report?")
      radio.html <- gradio(c("no", "yes"), 
          horizontal=TRUE, container=frame.html)
      
      ftop <- gframe("Choose STATA-File:")
      gtop <- ggroup(horizontal=TRUE, container=ftop)
      add(ftop, filename, expand=TRUE)
      add(ftop, filebutton)
      layout[1,1:7] <- ftop
      fparams <- gframe("STATA-Parameters:")
      glayout <- glayout(container=fparams)
      glayout[1,1] <- check.dates
      glayout[1,2, anchor=c(0,0)] <- glabel("version:")
      glayout[1,6, expand=FALSE] <- edit.version
      glayout[2,2, anchor=c(0,0)] <- glabel("handle factors as:")
      glayout[2,6, expand=FALSE] <- combo.convert.factors
      layout[2:3, 1:7] <- fparams
      layout[4, 1:7] <- frame.html
      layout[5,6, expand=FALSE] <- csvaccept
      layout[5,7, expand=FALSE] <- csvdiscard
      add(importDialogFrame, layout, expand=TRUE)
      #add(importDialogFrame, statusbar)
      
    }
    
  }
  
  # Data - Import - Import SAS
  importSAS <- function(...){
    importDialog <- gwindow("Import SAS", parent=window, width=100, height=100)
    putd("importDialog",importDialog)
    putd("dframe", NULL)
    importDialogFrame <- ggroup(container=importDialog, horizontal=FALSE)
    layout <- glayout()
    filename <- gedit()
    enabled(filename) <- FALSE
    
    buttonHandler <- function(...){
      openlastcwd()
      gfile(text = "Open SAS Export File", 
          filter=list("SAS XPORT"=list(patterns=c("*.xpt", "*.XPT")),"All files" = list(patterns = c("*"))),
          type = "open", handler=function(h,...){
            svalue(filename) <- h$file
            setlastcwd(dirname(h$file))
          })
    }
    
    #setup SAS import gui
    statusbar <- gstatusbar("")
    filebutton <- gbutton("...", handler=buttonHandler)
    csvaccept <- gbutton("OK", handler=function(...){
          #try to import sas file, if not message error
          tryCatch({
                wd <- WaitingDialog(Parent=importDialog)
                focus(wd) <- TRUE
                df <- sasxport.get(svalue(filename))
                putd("importFilename",svalue(filename))
                putd("importFilenameType","SAS")
                filename=gsub("\\\\","/",svalue(filename))
                cmdimp <- paste("activedataset <- sasxport.get(\"",filename,"\")",sep="")
                putd("cmdimp",cmdimp)
                
                dname <- format(Sys.time(), "importedSAS_%H_%M")
                putd("activeDataSet", df)
                putd("dataSetName",dname)
                putd("oldDataSet", ActiveDataSet())
                #svalue(dslab) <- paste(getd("dataSetName")," (n=",nrow(ActiveDataSet()),")",sep="")
                updateWindowTitle()
                putd("numLen", 0)
                putd("numVars", character(0))
                putd("keyLen", 0)
                putd("keyVars", character(0))
                putd("hLen", 0)
                putd("hVars", character(0))
                putd("wLen", 0)
                putd("wVars", character(0))
                putd("sLen", 0)
                putd("sVars", character(0))
                dispose(wd)
                dispose(importDialog)
                if(existd("sdcObject"))
                  rmd("sdcObject")
                selVar()
                
              },error=function(e){
                gmessage(paste("There was a problem while importing your SAS file: '",e,"'"),"Import Error!",icon="error")
              })
          
        })
    csvdiscard <- gbutton("Cancel ", handler=function(...){dispose(importDialog)})
    csvadjustTypes <- gbutton("Adjust Types", handler=typDialog)
    
    ftop <- gframe("Choose SAS-File:")
    gtop <- ggroup(horizontal=TRUE, container=ftop)
    add(ftop, filename, expand=TRUE)
    add(ftop, filebutton)
    layout[1,1:7] <- ftop
    layout[2,6, expand=FALSE] <- csvaccept
    layout[2,7, expand=FALSE] <- csvdiscard
    add(importDialogFrame, layout, expand=TRUE)
    buttonHandler()
    resetcwd()
  }
  
  # Data - Export - Export SAS
  exportSAS <- function(...){
    if(existd("sdcObject")  == FALSE){
      gmessage("There is no dataset loaded for export!", "No Dataset!",icon="warning")
    }
    else{
      importDialog <- gwindow("Export SAS", parent=window, width=100, height=100)
      putd("importDialog",importDialog)
      putd("dframe", NULL)
      importDialogFrame <- ggroup(container=importDialog, horizontal=FALSE)
      layout <- glayout()
      datafilename <- gedit()
      codefilename <- gedit()
      enabled(datafilename) <- FALSE
      enabled(codefilename) <- FALSE
      edit.dataname <- gedit("rdata")
      combo.validvarname <- gcombobox(c("<=6",">=7"), selected=2)
      
      databuttonHandler <- function(...){
        gfile(text = "Save Data File", type = "save", handler=function(h,...){
              if(grepl("^.*\\.(dat)$", h$file)){
                svalue(datafilename) <- h$file
              }
              else{
                svalue(datafilename) <- paste(h$file, ".dat", sep="")
              }
            })
      }
      
      codebuttonHandler <- function(...){
        gfile(text = "Save SAS File", type = "save", filter=list(".sas"=list("*.sas")),handler=function(h,...){
              if(grepl("^.*\\.(sas|SAS)$", h$file)){
                svalue(codefilename) <- h$file
              }
              else{
                svalue(codefilename) <- paste(h$file, ".sas", sep="")
              }
            })
      }
      
      #setup sas export gui
      datafilebutton <- gbutton("...", handler=databuttonHandler)
      codefilebutton <- gbutton("...", handler=codebuttonHandler)
      csvaccept <- gbutton("OK", handler=function(...){
            tryCatch({version <- paste("V",substr(svalue(combo.validvarname), 3,3), sep="")
                  write.foreign(sdcGUIoutput(), datafile=svalue(datafilename),
                      codefile=svalue(codefilename),
                      package = "SAS",
                      dataname = svalue(edit.dataname),
                      validvarname = version)
                  putd("exportFileName", svalue(datafilename))
                  if(svalue(radio.html, index=TRUE)==2)
                    exportReport()
                },
                error=function(e){gmessage(paste("There was a problem while exporting your data: '",e,"'"), "Problem",
                      icon="error")})
            dispose(importDialog)
          })
      csvdiscard <- gbutton("Cancel ", handler=function(...){dispose(importDialog)})
      
      #record export
      frame.html <- gframe("Generate report?")
      radio.html <- gradio(c("no", "yes"), 
          horizontal=TRUE, container=frame.html)
      
      fdata <- gframe("Choose Data-File (Contains exported data as freetext):")
      gdata <- ggroup(horizontal=TRUE, container=fdata)
      add(fdata, datafilename, expand=TRUE)
      add(fdata, datafilebutton)
      layout[1,1:7] <- fdata
      
      fcode <- gframe("Choose Code-File (Contains SAS Code for import):")
      gcode <- ggroup(horizontal=TRUE, container=fcode)
      add(fcode, codefilename, expand=TRUE)
      add(fcode, codefilebutton)
      layout[2,1:7] <- fcode
      
      fparams <- gframe("SAS-Parameters:")
      glayout <- glayout(container=fparams)
      glayout[1,1, anchor=c(-1,0)] <- glabel("future SAS data set name:")
      glayout[1,2, expand=FALSE] <- edit.dataname
      glayout[2,1, anchor=c(-1,0)] <- glabel("SAS version :")
      glayout[2,2, expand=FALSE] <- combo.validvarname
      layout[3:4, 1:7] <- fparams
      layout[5, 1:7] <- frame.html
      layout[6,6, expand=FALSE] <- csvaccept
      layout[6,7, expand=FALSE] <- csvdiscard
      add(importDialogFrame, layout, expand=TRUE)
      #add(importDialogFrame, statusbar)
    }
  }
  
  #outdir is the name of the exported data file from the different export dialogs
  exportReport <- function(...){
    if(existd("sdcObject")){
      reportDialog <- gwindow("Generate Report", parent=window, width=400, height=300)
      reportDialogG <- ggroup(container=reportDialog, horizontal=FALSE)
      
      repTitle <- gedit(width=100)
      repType <- gradio(c("internal (detailled) report","external (overview) report"), horizontal=TRUE)
      svalue(repTitle) <- "SDC-Report"
      reportDialogTitleFrame <- gframe("Title:", container=reportDialogG, horizontal=FALSE)
      add(reportDialogTitleFrame,repTitle)
      reportDialogFrame <- gframe("Report Type:", container=reportDialogG, horizontal=FALSE)
      add(reportDialogFrame,repType)
      
      reportDialogOutFrame <- gframe("Output Type:", container=reportDialogG, horizontal=FALSE)
      outputRadio <- gradio(c("HTML", "LATEX", "TEXT"), horizontal=TRUE, container=reportDialogOutFrame)
      okbutton <- gbutton("OK", container=reportDialogG, handler=function(h,...){
            obj <- ActiveSdcObject()
            obj@options$cmd <- getd("activeScript")$cmd
            if(existd("exportFileName")){
              exportFileName <- getd("exportFileName")
            }else{
              if(svalue(outputRadio)=="LATEX") {
                exportFileName <- gfile("Select file to save report to", parent=window, type="save" ,filter=list("LATEX"=list(patterns=c("*.tex")), "All files" = list(patterns = c("*"))))
              } 
              if (svalue(outputRadio)=="HTML") {
                exportFileName <- gfile("Select file to save report to", parent=window, type="save" ,filter=list("HTML"=list(patterns=c("*.html", "*.htm")), "All files" = list(patterns = c("*"))))
              }
              if (svalue(outputRadio)=="TEXT") {
                exportFileName <- gfile("Select file to save report to", parent=window, type="save" ,filter=list("TEXT"=list(patterns=c("*.txt")), "All files" = list(patterns = c("*"))))
              }
            }
            outdir <- dirname(exportFileName)
            filename <- strsplit(basename(exportFileName),"\\.")[[1]][1]
            
            internal <- FALSE
            if ( svalue(repType, index=TRUE) == 1 ) {
              internal <- TRUE
            }
            tryCatch(report(
                obj, 
                outdir=outdir,
                filename=filename,
                format=svalue(outputRadio),
                title=svalue(repTitle),
                internal=internal
            ), error=function(e){
              gmessage(paste("There was a problem while preparing your report: '",e,"'"), "SDCMicro Problem", icon="error")
            });
            dispose(reportDialog)
          })      
    }else{
      gmessage("No sdc object found to generate report.", title="Information", icon="warning", parent=window)
    }
  }
  
  # Waiting Dialog
  WaitingDialog <- function(parent, text="<b><big>Importing Data, Please Wait!</big></b>", 
      header="Importing!", Parent=NULL){
    window <- gwindow(header, parent=Parent, width=100, height=50)
    glabel(text, markup=TRUE,container=window)
    return(window)
  }
  
  # Script - New Script
  newScript <- function(...) {
    ns_do <- gconfirm("A new script will be started.\nAre you sure?", title="Information",
        icon="warning", parent=window)
    if( ns_do ) {
      Script.new()
    }
  }
  
  # Script - Save Script
  saveAsScript <- function(...) {
    saveScriptToFile <- function(fileName, ...) {
      cmdtmp <- Script()$cmd
      if(length(grep("sdcMicroScript",fileName))==0)
        fileName <- paste(fileName,".sdcMicroScript", sep="")
      fo <- file(fileName)
      writeLines(cmdtmp,fo)
      close(fo)
    }
    if( existd("activeScript") ) {
      xname <- gfile("Select file to save Script", type="save", parent=window,
          filter=list("Script files" = list(patterns = c("*.sdcMicroScript")),"All files" = list(patterns = c("*.*"))))
      
      if( xname != "" ) {
        saveScriptToFile(xname)
        putd("activescript.file", xname)
      }
    } else {
      gmessage("No active Script found.", title="Information", icon="warning",
          parent=window)
    }
  }

  # Script - Save Script
  saveScript <- function(...) {
    saveScriptToFile <- function(fileName, ...) {
      cmdtmp <- Script()$cmd
      if(length(grep("sdcMicroScript",fileName))==0)
        fileName <- paste(fileName,".sdcMicroScript", sep="")
      fo <- file(fileName)
      writeLines(cmdtmp,fo)
      close(fo)
    }
    if( existd("activescript.file") ) {
      xname <- getd("activescript.file")
      if( nchar(xname) > 0 && xname != "Untitled Script") {
        saveScriptToFile(xname)
      } else {
        saveAsScript()
        xname <- getd("activescript.file")
        name <- basename(xname)
        svalue(leftFrameGroupLabel) <- substr(name, 0, nchar(name) - nchar(".sdcMicroScript"))
      }
    } else {
      gmessage("No active Script found.", title="Information", icon="warning", parent=window)
    }
  }
  
  # Script - Load Script
  loadScript <- function(...) {
    gconfirm("Do you want to reset the current script and import new script?", icon="question", parent=window,
             handler=function(h,...) { 
    # open file browser and load the needed script
    xname <- gfile("Select script file to open.", parent=window, type="open", 
        filter=
            
            list("Script files" = list(patterns = c("*.sdcMicroScript")),"All files" = list(patterns = c("*.*")))) 
    if( xname != '' ) {
      fo <- file(xname)
      cmdtmp <- list(cmd=readLines(fo))
      close(fo)
      Script.new()
      putd("activeScript", cmdtmp)
      putd("activescript.file", xname)
      name <- basename(xname)
      svalue(leftFrameGroupLabel) <- substr(name, 0, nchar(name) - nchar(".sdcMicroScript"))
      Script.run()
      gdf <- getd("leftgdf")
      gdf[] <- cmdtmp$cmd
      putd("importFilenameType","sdcMicroScript")
    }
    })
  }
  
  # Script - View Script
  # TODO: implement view script
  viewScript <- function(...) {
    cmdhist <- Script()$cmd
    if( is.null(cmdhist) ) {
      gmessage("No script present at the moment.", title="Attention", icon="warning", parent=window)
    } else {
      sureQuit <- function(...) {
        gconfirm("Do you want to close the window without saving?", icon="question", parent=scriptEditWindow,
            handler=function(h,...) quitEditScriptWindow() )
      }
      quitEditScriptWindow <- function(...) {
        xtmp <- list(cmd=c(xscript[]))
        Script(xtmp)
        dispose(scriptEditWindow)
      }
      runCMDhist <- function(...) {
        rto <- as.numeric(svalue(runTo))
        cmdhist <- xscript[]
        if( is.numeric(rto) & !is.na(rto) ) {
          if( rto>0 & rto<(length(cmdhist)+1) ) {
            cmdhisttmp <- cmdhist[c(1:rto)]
            Script.run(cmdhisttmp)
            quitEditScriptWindow()
          }
        } else {
          gmessage("Script step not valid.", title="Input not valid", icon="info", parent=scriptEditWindow)
        }
      }
      delCMDhist <- function(...) {
        dto <- as.numeric(svalue(delRow))
        cmdhist <- xscript[]
        if( is.numeric(dto) & !is.na(dto) ) {
          if( dto>0 & dto<(length(cmdhist)+1) ) {
            cmdhisttmp <- cmdhist[-dto]
            xscript[] <- cmdhisttmp
            svalue(delRow) <- ""
          }
        } else {
          gmessage("Script step not valid.", title="Input not valid", icon="info", parent=scriptEditWindow)
        }
      }
      scriptEditWindow = gwindow(paste("Script", getd("activescript.file"), SEP=" "), parent=window, width=700, height=400)
      scriptWidget = ggroup(horizontal=FALSE)
      xscript = gdf(cmdhist, expand=TRUE)
      # TODO: find replacement, cause in linux it wouldnt display anything.
      #enabled(xscript) <- FALSE
      add(scriptWidget, xscript, expand=TRUE)
      putd("xscriptpdf", xscript)
      leftgdflist <- list()
      leftgdflist$"Run until and export dataset"$handler <- function(h,...) {
        gdf <- getd("xscriptpdf")
        rto <- svalue(gdf, index=TRUE)
        cmdhist <- gdf[]
        cmdhist <- cmdhist[c(1:rto)]
        compareDataExport(cmdhist, rto)
      }
      leftgdflist$"Undo until"$handler <- function(h,...) {
        gdf <- getd("xscriptpdf")
        rto <- svalue(gdf, index=TRUE)
        if(rto > 3) {
          cmdhist <- gdf[]
          cmdhist <- cmdhist[c(1:(rto-1))]
          Script.run(cmdhist)
          gdf[] <- cmdhist
          Script(list(cmd=c(cmdhist)))
          leftgdf <- getd("leftgdf")
          leftgdf[] <- cmdhist
        }
      }
      
      add3rdmousepopupmenu(xscript, leftgdflist)
      add(scriptEditWindow, scriptWidget)
      saveCancelGroup = ggroup(container=scriptWidget)
      addSpring(saveCancelGroup)
      gbutton("Close", container=saveCancelGroup, handler=function(h,...) quitEditScriptWindow() )
    }
  }
  
  # Script - Run Script
  runScript <- function(...) {
    # dialog and ask if you want to run the whole script on this dataset
    Script.run()
  }

  createVariableManagerData <- function(...) {
    putd("sLen", 0)
    if(existd("sdcObject")){
      sdc <- ActiveSdcObject()
      currentObject <- extractManipData(sdc)
      activeVars <- c(names(currentObject))
      if(!is.null(sdc@deletedVars)) {
        currentObject <-currentObject[!activeVars %in% sdc@deletedVars]
        activeVars <- c(names(currentObject))
      }
      putd("sLen", length(sdc@strataVar))
      d <- suppressWarnings(data.frame(
        Number = suppressWarnings(paste("V",1:length(currentObject),sep="")),
        Name=activeVars,
        Label=c(do.call("cbind",getNameLabelList(activeVars))),
        Datatype=sapply(currentObject, function(x) if(is.numeric(x) ) {"continuous"} else {"categorical"}),
        Max=as.numeric(sapply(currentObject, function(x) if("factor" %in% class(x) ) {suppressWarnings(max(as.numeric(as.character(x)))) } else { suppressWarnings(max(x))})),
        Min=as.numeric(sapply(currentObject, function(x) if("factor" %in% class(x) ) {suppressWarnings(min(as.numeric(as.character(x)))) } else { suppressWarnings(min(x))})),
        "Selected As"=suppressWarnings(getVarTypes(activeVars)),
        Deleted=rep("No", length(currentObject)) , stringsAsFactors=FALSE
      ))
      putd("variablemanagerCurrentObject", currentObject)
      putd("variablemanagerdata", d)
    } else if(existd("activeDataSet")){
      currentObject <- getd("activeDataSet")
      activeVars <- c(names(currentObject))
      
      datasettype <- ""
      if(existd("importFilenameType")) {
        datasettype <- getd("importFilenameType")
      }
      activeLabels <- rep("", times=length(activeVars))
      if(datasettype == "STATA") {
        activeLabels <- attr(ActiveDataSet(), 'var.labels')
      } else if(datasettype == "SPSS") {
        for( i in 1:1:length(activeVars)) {
          activeLabels[i] <- label(ActiveDataSet())[[i]]
        }
      }
      d <- data.frame(
        Number = paste("V",1:length(currentObject),sep=""),
        Name=activeVars,
        Label=activeLabels,
        Datatype=sapply(currentObject, function(x) if(is.numeric(x) ) {"continuous"} else {"categorical"}),
        Max=as.numeric(sapply(currentObject, function(x) if("factor" %in% class(x) ) {max(as.numeric(as.character(x))) } else { max(x)})),
        Min=as.numeric(sapply(currentObject, function(x) if("factor" %in% class(x) ) {min(as.numeric(as.character(x))) } else { min(x)})),
        "Selected As"=rep(" ", times=length(activeVars)),
        Deleted=rep("No", length(currentObject)) , stringsAsFactors=FALSE
      )
      putd("variablemanagerdata", d)
      putd("variablemanagerCurrentObject", currentObject)
    } else {
      d <- data.frame(
        Number = as.character(0),
        Name=as.character(0),
        Label=as.character(0),
        Datatype=as.character(0),
        Max=as.character(0),
        Min=as.character(0),
        "Selected As"=as.character(0),
        Deleted=as.character(0), stringsAsFactors=FALSE
      )
      putd("variablemanagerdata", d)
      putd("variablemanagerCurrentObject", data.frame(as.character(0)))
    }
  }

  CreateVariableManager <- function(...) {
    if(existd("activeDataSet")){
    quitVariableManagerWindow <- function(...) {
      dispose(variableManagerWindow)
    }
    variableManagerWindow = gwindow("Variables Manager", parent=window, width=700, height=500)
    scriptWidget = ggroup(horizontal=FALSE, expand=TRUE)
    topGroup = ggroup(horizontal=TRUE, container=scriptWidget)
    varFilter <- gedit(text="", container=topGroup, coerce.with = NULL, initial.msg = "please input filter letters by name", expand=TRUE)
    addHandlerKeystroke(varFilter, handler = function(h, ...) {
      filter <- svalue(varFilter)
      d <- getd("variablemanagerdata")
      if(filter != "") {
        sel <- sapply(c(names(getd("variablemanagerCurrentObject"))), function(x) all(grepl(tolower(filter), tolower(x), fixed=TRUE) > 0))
        df[] <- getd("toKeep")[sel,]
      } else {
        df[] <- getd("toKeep")
      }
    })
    
    vmmaingroup = ggroup(horizontal=TRUE, expand=TRUE, container=scriptWidget)
    LeftscriptWidget = ggroup(horizontal=FALSE, expand=TRUE, container=vmmaingroup)
    glabel(text="Variables", container=LeftscriptWidget)
    gseparator(container=LeftscriptWidget)

    createVariableManagerData()
    addSpring(topGroup)
    glabel(paste(ncol(getd("variablemanagerCurrentObject")), "variables", nrow(getd("variablemanagerCurrentObject")), "records", sep=" "), container=topGroup)
    df <- gtable(getd("variablemanagerdata"), container=LeftscriptWidget, expand=TRUE)
    putd("toKeep",  df[])
    putd("toKeepOrig",  df[])
    addhandlerclicked(df, handler<-function(h,...) {
      obj = svalue(h$obj, index=TRUE)
      if(!is.null(obj) && length(obj) > 0) {
        name = as.character(h$obj[obj,2])
        svalue(properitesName) <- name
        svalue(properitesLabel) <- as.character(h$obj[obj,3])
        del <- h$obj[obj,8]
        if(!is.null(del) && any(del== "Yes")) {
          svalue(deleteVariable) <- TRUE
        } else {
          svalue(deleteVariable) <- FALSE
        }
        svalue(DataTypeAs)<- as.character(h$obj[obj,4])
        svalue(slectedas) <- if(is.element(as.character(h$obj[obj,7]), 
                                         c("Categorical Key","Continuous Key","Weight",
                                         "Strata","Cluster-Id","Dependent"," "))) as.character(h$obj[obj,7]) else " "
        visible(NumericalFrame) <- FALSE
        visible(FreqFrame) <- FALSE
        if(svalue(DataTypeAs) =="categorical") {
          visible(FreqFrame) <- TRUE
        counts <- table(getd("variablemanagerCurrentObject")[name])
        Supdate <- as.data.frame(counts)
        Supdate <- cbind(1:nrow(Supdate), Supdate)
        colnames(Supdate) <- c("Value","Label", "N")
        freqLayout[1,1,expand=TRUE] = gtable(Supdate);
        #
        visible(FreqGraph) <- TRUE
        barplot(counts, main=name, horiz=TRUE)
        }
        if(svalue(DataTypeAs) =="continuous") {
          visible(NumericalFrame) <- TRUE
        s <- as.data.frame(summary(getd("variablemanagerCurrentObject")[name]))['Freq']
        plots <- data.frame(do.call('rbind', strsplit(as.character(s$Freq),':',fixed=TRUE)))
        colnames(plots) <- c("Property", "Value")
        NumeriLayout[1,1,expand=TRUE] = gtable(plots)        
        #dev.set(getd("NumericalGraphIndex"))
        visible(NumericalGraph) <- TRUE
        boxplot(getd("variablemanagerCurrentObject")[name], na.action= na.exclude, horizontal=TRUE)
        }
        if(is.element(as.character(h$obj[obj,7]), c("Categorical Key"))) {
          enabled(recodeButton) <- TRUE
        } else {
          enabled(recodeButton) <- FALSE
        }
      }
    })

    ######
    FreqFrame = ggroup(container=LeftscriptWidget, expand=TRUE, width=300, height=200)
    FreqFrameGroup = ggroup(horizontal=TRUE, container=FreqFrame, expand=TRUE)
    FreqGroup = ggroup(horizontal=FALSE, container=FreqFrameGroup, expand=TRUE)
    glabel(text="Frequencies", container=FreqGroup)
    freqLayout = glayout(container=FreqGroup, expand=TRUE)
    freqLayout[1,1,expand=TRUE] = gtable(data.frame(Value=as.character(0),Label=as.character(0), N=as.character(0)))
    freqLayout[1,2,expand=TRUE] = FreqGraph <- ggraphics(width=180, height=180)
    
    gindex = 2 
    if(!is.null(dev.list())) {
      gindex = gindex + length(dev.list())
    }
    putd("FreqGraphIndex",  gindex)
    visible(FreqFrame) <- FALSE
    ######
    NumericalFrame = ggroup(container=LeftscriptWidget, expand=TRUE, width=300, height=200)
    NumericalFrameGroup = ggroup(horizontal=TRUE, container=NumericalFrame, expand=TRUE)
    
    NumericalGroup = ggroup(horizontal=FALSE, container=NumericalFrameGroup, expand=TRUE)
    glabel(text="Summary statistics", container=NumericalGroup)
    NumeriLayout = glayout(container=NumericalGroup, expand=TRUE)
    NumeriLayout[1,1,expand=TRUE] = gtable(data.frame(Value=as.character(0),Label=as.character(0)))
    NumeriLayout[1,2,expand=TRUE] = NumericalGraph <- ggraphics(width=180, height=180)
    visible(NumericalFrame) <- FALSE
    
    RightscriptWidget = ggroup(horizontal=FALSE, container=vmmaingroup, width=100, height=400)
    propertiesFrame = gframe(text="Variable properties", container=RightscriptWidget, expand=TRUE)
    propertiesGroup = ggroup(horizontal=FALSE, container=propertiesFrame, expand=TRUE)
    glabel(text="Name", container=propertiesGroup)
    properitesName <- gedit(text="", container=propertiesGroup)
    enabled(properitesName) <- FALSE
    glabel(text="Label", container=propertiesGroup)
    properitesLabel <- gedit(text="", container=propertiesGroup)
    enabled(properitesLabel) <- FALSE
    glabel(text="Data type", container=propertiesGroup)
    DataTypeAs <- gedit("", container=propertiesGroup)
    enabled(DataTypeAs) <- FALSE
    
    deleteVariable <- gcheckbox(text="Delete", container=propertiesGroup, handler=function(h,...) {
      obj = svalue(df, index=TRUE)
      if(svalue(h$obj)) {
        df[obj,8] <- "Yes"
        toKeep = getd("toKeep")
        toKeep[toKeep$Number == df[obj,1], 8] <- "Yes"
        putd("toKeep", toKeep)
      } else {
        toKeep = getd("toKeep")
        df[obj,8] <- "No"
        toKeep[toKeep$Number == df[obj,1], 8] <- "No"
        putd("toKeep", toKeep)
      }
    })
    glabel(text="Selected As", container=propertiesGroup)
    slectedas <- gcombobox(c("Categorical Key","Continuous Key","Weight",
                "Strata","Cluster-Id"," "), container=propertiesGroup, handler=function(h,...) {
                  obj = svalue(df, index=TRUE)
                  if(svalue(h$obj) != " ") {
                    df[obj,7] <- svalue(h$obj)
                  } else {
                    df[obj,7] <- " "
                  }
                  toKeep = getd("toKeep")
                  toKeep[toKeep$Number == df[obj,1], 7] <- df[obj,7]
                  putd("toKeep", toKeep)
                })
    recodeButton <- gbutton(text="Recode", container=propertiesGroup, handler=function(h,...) {
      obj = svalue(df, index=TRUE)
      if(!is.null(obj) && length(obj) > 0 && as.character(df[obj,7]) == "Categorical Key") {
        name = as.character(df[obj,2])
        vc(c(keyname = name))
      } else {
        vc(keyname = ActiveSdcVarsStr())
      }
    })
    enabled(recodeButton) <- FALSE
    
    gseparator(container=scriptWidget)
    saveCancelGroup = ggroup(container=scriptWidget)
    addSpring(saveCancelGroup)
    #tmp = ggroup(container=saveCancelGroup)
    gbutton("Apply", container=saveCancelGroup, handler=function(h,...) {
      #
      toKeep = getd("toKeep")
      toKeepOrig = getd("toKeepOrig")
      if(!identical(toKeep, toKeepOrig)) {
        val <- gconfirm("This will reset your script and all anonymisation work. Do you want to continue?", parent=variableManagerWindow)
        if(as.logical(val) ) {
          keys <- toKeep$"Name"[toKeep$"Selected.As" %in% "Categorical Key"]
          conts <- toKeep$"Name"[toKeep$"Selected.As" %in% "Continuous Key"]
          weights <- toKeep$"Name"[toKeep$"Selected.As" %in% "Weight"]
          stratas <- toKeep$"Name"[toKeep$"Selected.As" %in% "Strata"]
          clusterIds <- toKeep$"Name"[toKeep$"Selected.As" %in% "Cluster-Id"]
      
          if(existd("sdcObject") && !is.null(ActiveSdcObject()@deletedVars)){
            names <- ActiveSdcObject()@deletedVars
          } else {
            names <- character(0)
          }
          confirmSelection_tmp(keys, conts, weights, clusterIds, stratas)
          left <- toKeep$Deleted %in% "No"
          if(length(names)>0 || length(toKeep$Name[!left])>0) {
            names <- c(toKeep$Name[!left], names)
            df[] <- toKeep[left,]
            svalue(df, index=TRUE) <- 1
            removeDirectID_tmp(names)
          }
          updateRightFrame()
        }
      }
      quitVariableManagerWindow()
    } )
    gbutton("Generate Strata Variable", container=saveCancelGroup, handler=function(h,...) {
      stVar_window = gwindow("Generate a strata variable", width=230, parent=variableManagerWindow)
      stVar_windowGroup = ggroup(container=stVar_window, horizontal=FALSE)
      stVar_main = ggroup(container=stVar_windowGroup)
      mtmp = ggroup(container=stVar_main)
      allVars <-colnames(ActiveDataSet())
      rmIndex <- c()
      nro <- nrow(ActiveDataSet())
      for(i in 1:length(allVars)){
        if(nrow(unique(ActiveDataSet()[,allVars[i],drop=FALSE]))>nro*.2)
          rmIndex <- c(rmIndex,i)
      }
      allVars <- allVars[-rmIndex]
      varTab = gtable(data.frame(vars=allVars, stringsAsFactors=FALSE), multiple=TRUE)
      putd("varTab_selVar_list", varTab[])
      size(varTab) <- c(120,400)
      add(mtmp, varTab)
      rtmp = ggroup(container=mtmp, horizontal=FALSE, expand=TRUE)
      tmp = gframe('<span weight="bold" size="medium">Generate new strata variable from:</span>',
                   container=rtmp,markup=TRUE, expand=TRUE)
      btmp = ggroup(container=tmp, horizontal=FALSE)
      addSpring(btmp)
      ft <- function(f, t, h, var, pm, ...) {
        # pm: 1 for +, 0 for -
        count = getd(var)
        varlist = getd("varTab_selVar_list")
        if( pm == 1 ) {
          count <- count + length(h);
          putd("varTab_selVar_list", varlist[ !varlist %in% h ]) 
        } else {
          count <- count - length(h);
          putd("varTab_selVar_list", append(varlist, h)) 
        }
        putd(var, count)
        if( length(h)>0 ) {
          if( length(f[])==1 ) {
            if( is.na(f[]) ) {
              f[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
            } else {
              f[,] <- data.frame(vars=c(f[], h), stringsAsFactors=FALSE)
            }
          } else {
            f[,] <- data.frame(vars=c(f[], h), stringsAsFactors=FALSE)
          }
          if( length(h)==length(t[]) ) {
            t[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
          } else {
            d <- t[]
            t[] <- d[ !d %in% h]
          }      
        }
      }
      gbutton(">>", container=btmp, handler=function(h,...) { 
        ft(sTab, varTab, svalue(varTab), "sLen", 1) 
      })
      gbutton("<<", container=btmp, handler=function(h,...) { 
        ft(varTab, sTab, svalue(sTab), "sLen", 0) 
      })
      addSpring(btmp)
      sVars = df[]$"Name"[df[]$"Selected.As" %in% "Strata"]
      putd("sLen", length(sVars))
      sTab = gtable(data.frame(vars=sVars, stringsAsFactors=FALSE), multiple=TRUE, container=tmp, expand=TRUE)
      gseparator(container=stVar_windowGroup)
      stVar_windowButtonGroup = ggroup(container=stVar_windowGroup)
      addSpring(stVar_windowButtonGroup)
      gbutton("Ok", container=stVar_windowButtonGroup,
              handler=function(h,...) {
                name <- "sdcMicroStrataVariable"
                sVars <- sTab[]                
                if(length(sVars)==0)
                  gmessage("You have to select at least  one categoric variable to generate a strata variable.",
                           title="Information", icon="warning", parent=window)
                else{
                  name <- paste(paste(sVars,collapse="_"),"_stratavar",sep="")
                  t1 <- paste("c(",paste("\"",sVars,"\"",sep="",collapse=","),")",sep="")
                  Script.add(paste("activedataset <- generateStrata(activedataset,", t1, ", \"",name,"\")",sep=""))
                  generateStrata_tmp(sVars,name)
                  toKeep = getd("toKeep")
                  keys <- toKeep$"Name"[toKeep$"Selected.As" %in% "Categorical Key"]
                  conts <- toKeep$"Name"[toKeep$"Selected.As" %in% "Continuous Key"]
                  weights <- toKeep$"Name"[toKeep$"Selected.As" %in% "Weight"]
                  stratas <- toKeep$"Name"[toKeep$"Selected.As" %in% "Strata"]
                  clusterIds <- toKeep$"Name"[toKeep$"Selected.As" %in% "Cluster-Id"]
                  
                  if(existd("sdcObject") && !is.null(ActiveSdcObject()@deletedVars)){
                    names <- ActiveSdcObject()@deletedVars
                  } else {
                    names <- character(0)
                  }
                  confirmSelection_tmp(keys, conts, weights, clusterIds, stratas)
                  left <- toKeep$Deleted %in% "No"
                  if(length(names)>0 || length(toKeep$Name[!left])>0) {
                    names <- c(toKeep$Name[!left], names)
                    df[] <- toKeep[left,]
                    svalue(df, index=TRUE) <- 1
                    removeDirectID_tmp(names)
                  }
                  createVariableManagerData()      
                  df[] <- getd("variablemanagerdata")
                  putd("toKeep",  df[])
                  putd("toKeepOrig",  df[])
                  updateRightFrame()
                  dispose(stVar_window)
                }
                
              })
      gbutton("Cancel ", container=stVar_windowButtonGroup, handler=function(h,...) { dispose(stVar_window) })
      
    } )
    gbutton("Reset", container=saveCancelGroup, handler=function(h,...) {
      df[] <- getd("variablemanagerdata")
      putd("toKeep",  df[])
    } )
    gbutton("Close", container=saveCancelGroup, handler=function(h,...) quitVariableManagerWindow() )
    
    add(variableManagerWindow, scriptWidget)
    }  else { 
      gmessage("There is no dataset loaded for Variable Manager!", "No Dataset!",icon="warning")
    }
}

  # update right frame
  updateRightFrame <- function(...) {
    if(existd("sdcObject")){
      activeVars <- getVarVectorFromsdcObject()
      delete(rfTopGroup, getd("rfTopTable"))
      putd("rfTopTable", gtable(data.frame("Selected As"=getVarTypes(activeVars), Name=activeVars),
                                expand=TRUE, container = rfTopGroup, handler=function(h,...) {
                                  i <- (svalue(h$obj, index=TRUE))
                                  name <- as.character(h$obj[i,2])
                                  type <- h$obj[i,1]
                                  visible(rflayout) <- FALSE
                                  visible(rfCatlayout) <- FALSE
                                  visible(rfNumlayout) <- FALSE
                                  sdc <- getd("sdcObject")
                                  if(name %in% names(sdc@manipKeyVars)) {
                                    visible(rfCatlayout) <- TRUE
                                    svalue(rfCatlayoutNPValue) <- name
                                    svalue(rfCatlayoutLPValue) <- getNameLabelList(name)
                                    Supdate <- as.data.frame(table(getd("sdcObject")@manipKeyVars[name]))
                                    colnames(Supdate) <- c("Category", "Frequency")
                                    rfCatlayoutLY[1,1,expand=TRUE] <- gtable(Supdate);
                                  } else if(name %in% names(sdc@manipNumVars)) {
                                    visible(rfNumlayout) <- TRUE
                                    svalue(rfNumlayoutName) <- name
                                    svalue(rfNumlayoutLabel) <- getNameLabelList(name)
                                    sum <- summary(getd("sdcObject")@manipNumVars[name])
                                    svalue(rfNumlayoutMin) <- strsplit(sum[1], ":")[[1]][2]
                                    svalue(rfNumlayout1stQu) <- strsplit(sum[2], ":")[[1]][2]
                                    svalue(rfNumlayoutMedian) <- strsplit(sum[3], ":")[[1]][2]
                                    svalue(rfNumlayoutMean) <- strsplit(sum[4], ":")[[1]][2]
                                    svalue(rfNumlayout3rdQu) <- strsplit(sum[5], ":")[[1]][2]
                                    svalue(rfNumlayoutMax) <- strsplit(sum[6], ":")[[1]][2]
                                    svalue(rfNumlayoutSD) <- sapply(sdc@origData[name], sd)
                                  } else if(name %in% names(sdc@manipPramVars)) {
                                    visible(rfCatlayout) <- TRUE
                                    svalue(rfCatlayoutNPValue) <- name
                                    svalue(rfCatlayoutLPValue) <- getNameLabelList(name)
                                    Supdate <- as.data.frame(table(getd("sdcObject")@manipPramVars[name]))
                                    colnames(Supdate) <- c("Category", "Frequency")
                                    rfCatlayoutLY[1,1,expand=TRUE] <- gtable(Supdate);
                                  } else if(name %in% names(sdc@manipStrataVar)) {
                                    visible(rfCatlayout) <- TRUE
                                    svalue(rfCatlayoutNPValue) <- name
                                    svalue(rfCatlayoutLPValue) <- getNameLabelList(name)
                                    Supdate <- as.data.frame(table(getd("sdcObject")@manipStrataVar[name]))
                                    colnames(Supdate) <- c("Category", "Frequency")
                                    rfCatlayoutLY[1,1,expand=TRUE] <- gtable(Supdate);
                                  } else {
                                    isNumerical <- sapply(sdc@origData[name], is.numeric)
                                    if(isNumerical) {
                                      visible(rfNumlayout) <- TRUE
                                      svalue(rfNumlayoutName) <- name
                                      svalue(rfNumlayoutLabel) <- getNameLabelList(name)
                                      sum <- summary(sdc@origData[name])
                                      svalue(rfNumlayoutMin) <- strsplit(sum[1], ":")[[1]][2]
                                      svalue(rfNumlayout1stQu) <- strsplit(sum[2], ":")[[1]][2]
                                      svalue(rfNumlayoutMedian) <- strsplit(sum[3], ":")[[1]][2]
                                      svalue(rfNumlayoutMean) <- strsplit(sum[4], ":")[[1]][2]
                                      svalue(rfNumlayout3rdQu) <- strsplit(sum[5], ":")[[1]][2]
                                      svalue(rfNumlayoutMax) <- strsplit(sum[6], ":")[[1]][2]
                                      svalue(rfNumlayoutSD) <- sapply(sdc@origData[name], sd)
                                    } else {
                                      visible(rfCatlayout) <- TRUE
                                      svalue(rfCatlayoutNPValue) <- name
                                      svalue(rfCatlayoutLPValue) <- getNameLabelList(name)
                                      Supdate <- as.data.frame(table(getd("sdcObject")@origData[name]))
                                      colnames(Supdate) <- c("Category", "Frequency")
                                      rfCatlayoutLY[1,1,expand=TRUE] <- gtable(Supdate);
                                    }
                                  }
                                }))
    } else {
      rfTopTable[] <- data.frame(id=character(0))
      rfBottomtable[] <- data.frame(id=character(0), label=character(0))
    }
    if(existd("sdcObject")){
      sdc <- ActiveSdcObject()
      currentObject <- extractManipData(sdc)
      if(!is.null(sdc@deletedVars)) {
        currentObject <-currentObject[!names(currentObject) %in% sdc@deletedVars]
      }
      svalue(sdcobjectinfo) <- paste( "No. of Observations:", nrow(currentObject), "\nNo. of Variables:", ncol(currentObject), sep=" ")
    } else  if(existd("activeDataSet")) {
      svalue(sdcobjectinfo) <- paste( "No. of Observations:", nrow(getd("activeDataSet")), "\nNo. of Variables:", ncol(getd("activeDataSet")), sep=" ")
    }
  }

  updateOutput <- function(action, ...) {

  }

  # GUI - Quit
  quitGUI <- function(...) {
    val <- gconfirm("Do you really want to close the window?", parent=window)
    if( as.logical(val) ) {
      dispose(window)
      if(!is.null(options("quitRwithsdcGUI")[[1]])){#if started with custom binary windows build, quit R toos
        cat("quitting R now\n")
        quit("no")
      }
    }
  }
  OneStepBack <- function(...) {
    if(existd("sdcObject")){
      if(!is.null(getd("sdcObject")@prev)){
        acs <- getd("activeScript")$cmd
        cmd <- acs[[length(acs)]]
        val <- gconfirm(paste("Do you really want to undo the last command:\n",cmd,sep=""), parent=window)
        if( as.logical(val) ) {
          putd("activeScript",list(cmd=acs[-length(acs)]))
          ActiveSdcObject(undolast(ActiveSdcObject()))
          freqCalcIndivRisk()
          nm_risk_print_function()
        }
      }else
        gmessage("Undo is not possible, because no previous sdc object was found.\n (Undo is only possible for one step and data sets with less than 100 000 rows.)", title="Attention", icon="error", parent=window)
    }else
      gmessage("Undo is not possible, because no active sdc object was found.\n (Undo is only possible for one step and data sets with less than 100 000 rows.)", title="Attention", icon="error", parent=window)
  }
  restartGUI <- function(...) {
    val <- gconfirm("Do you really want to delete everything and restart the GUI?", parent=window)
    if( as.logical(val) ) {
      dispose(window)
      #rm(list=ls())
      rmd(listd())
      sdcGUI()
    }
  }
  selectKeyVariableInVC <- function(...) {
    if(existd("sdcObject")  == FALSE){
      gmessage("Please select key variables in Variable Manager!", "No Key Variables yet!",icon="warning")
    } else {
      selectDialog <- gwindow("Select for Recoding", parent=window, width=400, height=200)
      dialoggroup <- ggroup(container=selectDialog, expand=TRUE,horizontal=FALSE)
      sdc <- ActiveSdcObject()
      frame <- gframe("key-Variable to Recode", container=dialoggroup,horizontal=FALSE, expand=TRUE)
      keyList <- gcombobox(names(sdc@manipKeyVars), container=frame)
      windowButtonGroup = ggroup(container=frame)
      addSpring(windowButtonGroup)
      gbutton("Ok", container=windowButtonGroup,
              handler=function(h,...) {
                vc(keyname = svalue(keyList))
                dispose(selectDialog)
              })
      gbutton("Cancel ", container=windowButtonGroup, handler=function(h,...) { dispose(selectDialog) })
      gbutton("Help ", container=windowButtonGroup, handler=function(h,...) { helpR("globalRecode") })
    }
  }

  populateFreqandRisk <- function(...) {
    if(existd("sdcObject")  == FALSE){
      gmessage("Please select key variables in Variable Manager!", "No Key Variables yet!",icon="warning")
    } else {
      populationDialog <- gwindow("Population Frequencies and Individual Risks", parent=window, width=800, hieght=500)
      dialoggroup <- ggroup(container=populationDialog,horizontal=FALSE)
      sdc <- ActiveSdcObject()
      
      nb <- gnotebook(container=dialoggroup, closebuttons=FALSE)
      
      FreqTT <- ggroup(horizontal=FALSE, container=nb,label="Frequencies")
      svalue(nb) <- 1
      m1 <- ActiveSdcVars("risk")$individual
      xtmp <- ActiveSdcVars("manipKeyVars")
      tabDat <- cbind(xtmp,m1)
      ind <- !duplicated(apply(xtmp,1,function(x)paste(x,collapse="_")))
      tabDat <- tabDat[ind,]
      tabDat$risk <- round(tabDat$risk,5)
      tabDat <- tabDat[order(as.numeric(tabDat$risk),decreasing=TRUE),]
      putd("freq.tabdata", tabDat)
      FreqT <- gtable(data.frame(apply(tabDat,2,function(x)as.character(x)),stringsAsFactors=FALSE))
      size(FreqT) <- c(500,500)
      FreqTT_2 <- gframe('<span weight="bold" size="medium">Frequencies for combinations of cat. key variables</span>',
                         container=FreqTT,markup=TRUE, expand=TRUE)
      tooltip(FreqT) <- "fk=sample frequency\nFk=(grossed up) population frequency"
      add(FreqTT_2 , FreqT, expand=TRUE)
      putd("freq.FreqT", FreqT)
      
      barcharRiskTab <- ggroup(horizontal=FALSE, container=nb,label="Barchart of Individual Risks")
      g <- ggraphics()
      add(barcharRiskTab, g, expand=TRUE)
      
      barcharFreqTab <- ggroup(horizontal=FALSE, container=nb,label="Barchart of Population Frequencies")
      g1 <- ggraphics()
      add(barcharFreqTab, g1, expand=TRUE)
      
      addHandlerChanged(nb, handler=function(h,...) {
        sdc <- ActiveSdcObject()
        tabDat <- getd("freq.tabdata")
        if(h$pageno == 2) {
          visible(g) <- TRUE
          if(nrow(tabDat) > 10) {
            try(barplot(c(as.numeric(tabDat$risk[1:10])), main="Top Ten Individual Risks for Categorical Combinations", horiz =TRUE, cex.names=0.5, names.arg=do.call(paste0, tabDat[1:10, names(sdc@manipKeyVars)]), las=1), silent=FALSE)
          } else 
          try(barplot(c(as.numeric(tabDat$risk)), main="Individual Risks for Categorical Combinations", horiz =TRUE, cex.names=0.5, names.arg=do.call(paste0, tabDat[names(sdc@manipKeyVars)]), las=1), silent=TRUE)
        } else if(h$pageno == 3) {
          visible(g1) <- TRUE
          if(nrow(tabDat) > 10) {
            try(barplot(c(as.numeric(tabDat$Fk[1:10])), main="Ten Lowest Population Frequencies for Categorical Combinations", horiz =TRUE, cex.names=0.5, names.arg=do.call(paste0, tabDat[1:10, names(sdc@manipKeyVars)]), las=1), silent=TRUE)
          } else {
            try(barplot(c(as.numeric(tabDat$Fk)), main="Population Frequencies for Categorical Combinations", horiz =TRUE, cex.names=0.5, names.arg=do.call(paste0, tabDat[names(sdc@manipKeyVars)]), las=1), silent=TRUE)
          }
        } else {
          delete(FreqTT_2, getd("freq.FreqT"))
          FreqT <- gtable(data.frame(apply(tabDat,2,function(x)as.character(x)),stringsAsFactors=FALSE))
          tooltip(FreqT) <- "fk=sample frequency\nFk=(grossed up) population frequency"
          add(FreqTT_2 , FreqT, expand=TRUE)
          putd("freq.FreqT", FreqT)
        }
      })
      gseparator(container=dialoggroup)
      nm2_windowButtonGroup = ggroup(container=dialoggroup)
      addSpring(nm2_windowButtonGroup)
      gbutton("Refresh", container=nm2_windowButtonGroup,
              handler=function(h,...) {
                svalue(nb) <- 2
                sdc <- ActiveSdcObject()
                m1 <- ActiveSdcVars("risk")$individual
                xtmp <- ActiveSdcVars("manipKeyVars")
                tabDat <- cbind(xtmp,m1)
                ind <- !duplicated(apply(xtmp,1,function(x)paste(x,collapse="_")))
                tabDat <- tabDat[ind,]
                tabDat$risk <- round(tabDat$risk,5)
                tabDat <- tabDat[order(as.numeric(tabDat$risk),decreasing=TRUE),]
                putd("freq.tabdata", tabDat)
                svalue(nb) <- 1
              })
      gbutton("Close ", container=nm2_windowButtonGroup, handler=function(h,...) { dispose(populationDialog) })
      gbutton("Help ", container=nm2_windowButtonGroup, handler=function(h,...) { helpR("measure_risk") })
      svalue(nb) <- 1
    }
  }

  keyVariableFreq <-   function(sdcObject, step, ...) {
    if(existd("sdcObject")  == FALSE){
      gmessage("There is no dataset loaded for viewing comparative frequencies!", "No Dataset!",icon="warning")
    } else{
      lrDialog <- gwindow("Comparative Frequencies for Categorical Variables", parent=window, width=600, height=400)
      maingroup = ggroup(horizontal=FALSE, expand=TRUE, container=lrDialog)
      varFilter <- gedit(text="", container=maingroup, coerce.with = NULL, initial.msg = "please input filter letters by name", expand=TRUE)
      addHandlerKeystroke(varFilter, handler = function(h, ...) {
        filter <- svalue(varFilter)
        dlist <- suppressWarnings(data.frame(
          Number = suppressWarnings(paste("V",1:length(ActiveSdcVarsStr()),sep="")),
          Name=ActiveSdcVarsStr(),
          Label=c(do.call("cbind",getNameLabelList(ActiveSdcVarsStr()))), stringsAsFactors=FALSE))
        if(filter != "") {
          names <- ActiveSdcVarsStr()
          sel <- sapply(names, function(x) all(grepl(tolower(filter), tolower(x), fixed=TRUE) > 0))
          df[] <- dlist[sel,]
        } else {
          df[] <- dlist
        }
      })
      
      addSpring(maingroup)
      labelgroup = ggroup(horizontal=TRUE, container=maingroup)
      glabel("Categorical Key Variables", container=labelgroup)
      addSpring(labelgroup)
      d <- suppressWarnings(data.frame(
        Number = suppressWarnings(paste("V",1:length(ActiveSdcVarsStr()),sep="")),
        Name=ActiveSdcVarsStr(),
        Label=c(do.call("cbind",getNameLabelList(ActiveSdcVarsStr()))), stringsAsFactors=FALSE))
      df <- gtable(d, container=maingroup, expand=TRUE)
      
      addhandlerclicked(df, handler<-function(h,...) {
        obj = svalue(h$obj, index=TRUE)
        if(!is.null(obj) && length(obj) > 0) {
          name = as.character(h$obj[obj,2])

          sdc <- ActiveSdcObject()
          counts <- table(sdc@manipKeyVars[name])
          Supdate <- as.data.frame(counts)
          Supdate <- cbind(1:nrow(Supdate), Supdate)
          colnames(Supdate) <- c("Value","Label", "N")
          freqLayout[1,1,expand=TRUE] = gtable(Supdate);
          visible(FreqGraph) <- TRUE
          barplot(counts, main=name, horiz=TRUE)

          origcounts <- table(sdc@origData[name])
          origSupdate <- as.data.frame(origcounts)
          origSupdate <- cbind(1:nrow(origSupdate), origSupdate)
          colnames(origSupdate) <- c("Value","Label", "N")
          origfreqLayout[1,1,expand=TRUE] = gtable(origSupdate);
          visible(origFreqGraph) <- TRUE
          barplot(origcounts, main=name, horiz=TRUE)
        }
      })
      
      ######
      FreqFrame = gframe("Current Frequencies", container=maingroup, expand=TRUE, width=300, height=200)
      FreqFrameGroup = ggroup(horizontal=TRUE, container=FreqFrame, expand=TRUE)
      FreqGroup = ggroup(horizontal=FALSE, container=FreqFrameGroup, expand=TRUE)
      freqLayout = glayout(container=FreqGroup, expand=TRUE)
      freqLayout[1,1,expand=TRUE] = gtable(data.frame(Value=as.character(0),Label=as.character(0), N=as.character(0)))
      freqLayout[1,2,expand=TRUE] = FreqGraph <- ggraphics(width=180, height=180)
      
      ######
      origFreqFrame = gframe("Original Frequencies", container=maingroup, expand=TRUE, width=300, height=200)
      origFreqFrameGroup = ggroup(horizontal=TRUE, container=origFreqFrame, expand=TRUE)
      origFreqGroup = ggroup(horizontal=FALSE, container=origFreqFrameGroup, expand=TRUE)
      origfreqLayout = glayout(container=origFreqGroup, expand=TRUE)
      origfreqLayout[1,1,expand=TRUE] = gtable(data.frame(Value=as.character(0),Label=as.character(0), N=as.character(0)))
      origfreqLayout[1,2,expand=TRUE] = origFreqGraph <- ggraphics(width=180, height=180)
      
      svalue(df, index=TRUE) <- 1
    }
  }

  compareUnivariateSummary <-   function(sdcObject, step, ...) {
    if(existd("sdcObject")  == FALSE || is.null(ActiveSdcObject()@numVars)){
      gmessage("There is no dataset or numerical variable for viewing comparative univariate summary!", "No Dataset or Numerical Variable!",icon="warning")
    } else{
      lrDialog <- gwindow("comparative Univariate Summary for continuous key variables", parent=window, width=600, height=400)
      maingroup = ggroup(horizontal=FALSE, expand=TRUE, container=lrDialog)
      varFilter <- gedit(text="", container=maingroup, coerce.with = NULL, initial.msg = "please input filter letters by name")
      addHandlerKeystroke(varFilter, handler = function(h, ...) {
        filter <- svalue(varFilter)
        dlist <- suppressWarnings(data.frame(
          Number = suppressWarnings(paste("V",1:length(ActiveSdcVarsStr("numVars")),sep="")),
          Name=ActiveSdcVarsStr("numVars"),
          Label=c(do.call("cbind",getNameLabelList(ActiveSdcVarsStr("numVars")))), stringsAsFactors=FALSE))
        if(filter != "") {
          names <- ActiveSdcVarsStr("numVars")
          sel <- sapply(names, function(x) all(grepl(tolower(filter), tolower(x), fixed=TRUE) > 0))
          df[] <- dlist[sel,]
        } else {
          df[] <- dlist
        }
      })
    
      labelgroup = gframe("Comparative Univariate Summary", horizontal=FALSE, container=maingroup, expand=TRUE)
      d <- suppressWarnings(data.frame(
        Number = suppressWarnings(paste("V",1:length(ActiveSdcVarsStr("numVars")),sep="")),
        Name=ActiveSdcVarsStr("numVars"),
        Label=c(do.call("cbind",getNameLabelList(ActiveSdcVarsStr("numVars")))), stringsAsFactors=FALSE))
      df <- gtable(d, container=labelgroup, expand=TRUE)
    
      addhandlerclicked(df, handler<-function(h,...) {
        obj = svalue(h$obj, index=TRUE)
        sdc <- ActiveSdcObject()
        if(!is.null(obj) && length(obj) > 0 && !is.null(sdc@numVars)) {
          name = as.character(h$obj[obj,2])
          current <-  read.table(text=summary(sdc@manipNumVars[name]), sep=":")
          orig <-  read.table(text=summary(sdc@origData[name]), sep=":")
          Supdate <- data.frame(current["V1"],orig["V2"],current["V2"])
          names(Supdate) <- c("",name, paste(name, ".m", SEP=""))
          freqLayout[1,1,expand=TRUE] = gtable(Supdate)
          visible(FreqGraph) <- TRUE
          boxplot(data.frame(sdc@origData[name], sdc@manipNumVars[name]), na.action= na.exclude
                , horizontal=FALSE, names=c(name, paste(name, ".m", SEP="")), main="Univariate comparison original vs. perturbed data")
        }
      })
    
      ######
      FreqFrame = gframe(" Univariate Summary", container=maingroup, expand=TRUE, width=300, height=300)
      FreqFrameGroup = ggroup(horizontal=TRUE, container=FreqFrame, expand=TRUE)
      FreqGroup = ggroup(horizontal=FALSE, container=FreqFrameGroup, expand=TRUE)
      freqLayout = glayout(container=FreqGroup, expand=TRUE)
      freqLayout[1,1,expand=TRUE] = gtable(data.frame(Value=as.character(0),Label=as.character(0), N=as.character(0)))
      freqLayout[1,2,expand=TRUE] = FreqGraph <- ggraphics(width=180, height=180)
        
      svalue(df, index=TRUE) <- 1
    }
  }

  linearRegression <-  function(sdcObject, step, ...) {
    if(existd("sdcObject")  == FALSE){
      gmessage("There is no dataset loaded for Comparison!", "No Dataset!",icon="warning")
    } else{
      lrDialog <- gwindow("Linear Regression", parent=window, width=600, height=400)
      
      lTOr <- function(h, left, right, ...) {
        if( length(h)>0 ) {
          if( length(right[])==1 ) {
            if( is.na(right[]) ) {
              right[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
            } else {
              right[,] <- data.frame(vars=c(right[], h), stringsAsFactors=FALSE)
            }
          } else {
            right[,] <- data.frame(vars=c(right[], h), stringsAsFactors=FALSE)
          }
          if( length(h)==length(left[]) ) {
            left[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
          } else {
            xtmp <- c()
            for( i in 1:length(left[]) ) {
              for( j in 1:length(h) ) {
                if( left[][i]==h[j] ) {
                  xtmp <- c(xtmp, i)
                }
              }
            }
            left[,] <- data.frame(vars=left[-xtmp], stringsAsFactors=FALSE)
          }
        }
      }
      rTOl <- function(h, left, right,...) {
        if( length(h)>0 ) {
          if( length(left[])==1 ) {
            if( is.na(left[]) ) {
              left[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
            } else {
              left[,] <- data.frame(vars=c(left[], h), stringsAsFactors=FALSE)
            }
          } else {
            left[,] <- data.frame(vars=c(left[], h), stringsAsFactors=FALSE)
          }
          if( length(h)==length(right[]) ) {
            right[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
          } else {
            xtmp <- c()
            for( i in 1:length(right[]) ) {
              for( j in 1:length(h) ) {
                if( right[][i]==h[j] ) {
                  xtmp <- c(xtmp, i)
                }
              }
            }
            right[,] <- data.frame(vars=right[-xtmp], stringsAsFactors=FALSE)
          }
        }
      }
      nb <- gnotebook(container=lrDialog, closebuttons=FALSE)
      #Main
      p1_windowGroup = ggroup(container=nb, horizontal=FALSE,label="Function")
      #Help
      t <- gtext(container=nb, label="Help ", expand=TRUE)
      l <- .findHelpPage("lm", "stats")
      x <- l$x
      .insertHelpPage(t, x)
      svalue(nb) <- 1
      mainframe = gframe('', container=p1_windowGroup, horizontal=TRUE)
      tmp = gframe('<span weight="bold" size="medium">Dependent Variable Selection</span>',
                         container=mainframe, horizontal=TRUE,markup=TRUE)
      sdcObject = ActiveSdcObject()
      data <- extractManipData(sdcObject)
      if(!is.null(sdcObject@deletedVars)) {
        data <- data[!names(data) %in% sdcObject@deletedVars]
      }
      #curr_data_numeric <- sapply(data, is.numeric)
      curr_data_numeric <- c()
      for(i in 1:length(names(data))) {
        curr_data_numeric = c(curr_data_numeric, try(all(sapply(data[names(data)[i]], function(x) { !is.na(as.numeric(x))})), silent = FALSE))
      }
      varTab = gtable(data.frame(vars=names(data[curr_data_numeric]), stringsAsFactors=FALSE), multiple=TRUE)
      size(varTab) <- c(120,200)
      add(tmp, varTab)
      btmp = ggroup(container=tmp, horizontal=FALSE)
      addSpring(btmp)
      b1 <- gbutton(">>", container=btmp, handler=function(h,...) { lTOr(svalue(varTab), varTab, selTab) })
      b2 <- gbutton("<<", container=btmp, handler=function(h,...) { rTOl(svalue(selTab), varTab, selTab) })
      addSpring(btmp)
      selTab = gtable(data.frame(vars=character(0), stringsAsFactors=FALSE), multiple=TRUE)
      size(selTab) <- c(120,200)
      add(tmp, selTab)
      
      tmp = gframe('<span weight="bold" size="medium">Independent Variable Selection</span>',
                   container=mainframe,markup=TRUE)
      skeyvar <- c(ActiveSdcVarsStr(),  ActiveSdcVarsStr("numVars"))
      sTab = gtable(data.frame(vars=skeyvar[skeyvar %in% names(data[curr_data_numeric])], stringsAsFactors=FALSE), multiple=TRUE)
      size(sTab) <- c(120,200)
      add(tmp, sTab)
      btmp = ggroup(container=tmp, horizontal=FALSE)
      addSpring(btmp)
      b1 <- gbutton(">>", container=btmp, handler=function(h,...) { lTOr(svalue(sTab), sTab, selTab1) })
      b2 <- gbutton("<<", container=btmp, handler=function(h,...) { rTOl(svalue(selTab1), sTab, selTab1) })
      addSpring(btmp)
      selTab1 = gtable(data.frame(vars=character(0), stringsAsFactors=FALSE), multiple=TRUE)
      size(selTab1) <- c(120,200)
      add(tmp, selTab1)
      
      gseparator(container=p1_windowGroup)
      nm2_windowButtonGroup = ggroup(container=p1_windowGroup)
      addSpring(nm2_windowButtonGroup)
      gbutton("Calculate", container=nm2_windowButtonGroup,
              handler=function(h,...) {
                if( (length(selTab[]) != 1 | any(is.na(selTab[])))  || (length(selTab1[])<1 | any(is.na(selTab1[])))) {
                  gmessage("You need to select one depdendent variable and at least one independent variable!", title="Information", icon="info", parent=lrDialog)
                } else {
                  tryCatch({
                  form = paste(paste(selTab[], collapse = "+"), "~", paste(selTab1[], collapse = "+"), sep=" ")
                  orig_lm = lm(formula = as.formula(form), data =sdcObject@origData)
                  curr_lm = lm(formula = as.formula(form), data =data)
                  orig_R2 = summary(orig_lm)$r.squared
                  curr_R2 = summary(curr_lm)$r.squared
                  svalue(outputlabel) <- paste("<span weight=\"bold\" size=\"medium\">The goodness of fit ratio is:</span>\norig_R2 : curr_R2 = ", round(orig_R2/curr_R2,2), " : 1", sep="")}
                  ,
                  error=function(e){
                    svalue(outputlabel) <- paste("please choose other dependent variables. error:\n", e, "")
                  })
                }
              })
      gbutton("Close ", container=nm2_windowButtonGroup, handler=function(h,...) { dispose(lrDialog) })
      gseparator(container=p1_windowGroup)
      outputGroup = ggroup(container=p1_windowGroup, expand=TRUE)
      outputlabel = glabel("", container=outputGroup, markup=TRUE)
    }
  }

  spearmanTest <-  function(sdcObject, step, ...) {
    if(existd("sdcObject")  == FALSE || length(ActiveSdcObject()@numVars) == 0) {
      gmessage("There is no dataset loaded or no numerical variable for Spearman Test!", "No Dataset!",icon="warning")
    } else{
      lrDialog <- gwindow("Test for Association", parent=window, width=600, height=400)
    
      lTOr <- function(h, left, right, ...) {
        if( length(h)>0 ) {
          if( length(right[])==1 ) {
            if( is.na(right[]) ) {
              right[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
            } else {
              right[,] <- data.frame(vars=c(right[], h), stringsAsFactors=FALSE)
            }
          } else {
            right[,] <- data.frame(vars=c(right[], h), stringsAsFactors=FALSE)
          }
          if( length(h)==length(left[]) ) {
            left[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
          } else {
            xtmp <- c()
            for( i in 1:length(left[]) ) {
              for( j in 1:length(h) ) {
                if( left[][i]==h[j] ) {
                  xtmp <- c(xtmp, i)
                }
              }
            }
            left[,] <- data.frame(vars=left[-xtmp], stringsAsFactors=FALSE)
          }
        }
      }
      rTOl <- function(h, left, right,...) {
        if( length(h)>0 ) {
          if( length(left[])==1 ) {
            if( is.na(left[]) ) {
              left[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
            } else {
              left[,] <- data.frame(vars=c(left[], h), stringsAsFactors=FALSE)
            }
          } else {
            left[,] <- data.frame(vars=c(left[], h), stringsAsFactors=FALSE)
          }
          if( length(h)==length(right[]) ) {
            right[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
          } else {
            xtmp <- c()
            for( i in 1:length(right[]) ) {
              for( j in 1:length(h) ) {
                if( right[][i]==h[j] ) {
                  xtmp <- c(xtmp, i)
                }
              }
            }
            right[,] <- data.frame(vars=right[-xtmp], stringsAsFactors=FALSE)
          }
        }
      }
      nb <- gnotebook(container=lrDialog, closebuttons=FALSE)
      #Main
      p1_windowGroup = ggroup(container=nb, horizontal=FALSE,label="Function")
      #Help
      t <- gtext(container=nb, label="Help ", expand=TRUE)
      l <- .findHelpPage("cor.test", "stats")
      x <- l$x
      .insertHelpPage(t, x)
      svalue(nb) <- 1
      methodSel = gcombobox(c("pearson", "kendall", "spearman"), container=p1_windowGroup)
      mainframe = gframe('', container=p1_windowGroup, horizontal=TRUE)
      tmp = gframe('<span weight="bold" size="medium">Variable Selection</span>',
                 container=mainframe, horizontal=TRUE,markup=TRUE, expand=TRUE)
      sdcObject = ActiveSdcObject()
      data <- extractManipData(sdcObject)
      if(!is.null(sdcObject@deletedVars)) {
        data <- data[!names(data) %in% sdcObject@deletedVars]
      }
      varTab = gtable(data.frame(vars=names(sdcObject@manipNumVars), stringsAsFactors=FALSE), multiple=TRUE, expand=TRUE, container=tmp)
      size(varTab) <- c(120,200)
      btmp = ggroup(container=tmp, horizontal=FALSE)
      addSpring(btmp)
      b1 <- gbutton(">>", container=btmp, handler=function(h,...) { lTOr(svalue(varTab), varTab, selTab) })
      b2 <- gbutton("<<", container=btmp, handler=function(h,...) { rTOl(svalue(selTab), varTab, selTab) })
      addSpring(btmp)
      selTab = gtable(data.frame(vars=character(0), stringsAsFactors=FALSE), multiple=TRUE, expand=TRUE, container=tmp)
      size(selTab) <- c(120,200)
    
      gseparator(container=p1_windowGroup)
      nm2_windowButtonGroup = ggroup(container=p1_windowGroup)
      addSpring(nm2_windowButtonGroup)
      gbutton("Calculate", container=nm2_windowButtonGroup,
            handler=function(h,...) {
              if(length(selTab[]) != 1 | any(is.na(selTab[]))) {
                gmessage("You have to select only one variable!", title="Information", icon="info", parent=lrDialog)
              } else {
                tryCatch({
                  cor.S = cor.test(sdcObject@origData[,selTab[]], data[,selTab[]], method = svalue(methodSel))
                  rho= cor.S$estimate
                  svalue(outputlabel) <- paste("<span weight=\"bold\" size=\"medium\">Test for association between current data and original data is:</span>"
                                               ,"\nstatistic = ", cor.S$statistic, "\n","p = ", cor.S$p.value, "\n"
                                               , c("The cor is : ", "The tau is : ", "The rho is : ")[grep(svalue(methodSel), c("pearson", "kendall", "spearman"))], rho, sep="")}
                  ,
                  error=function(e){
                    svalue(outputlabel) <- paste("error:\n", e, "")
                  })
              }
            })
      gbutton("Close ", container=nm2_windowButtonGroup, handler=function(h,...) { dispose(lrDialog) })
      gseparator(container=p1_windowGroup)
      outputGroup = ggroup(container=p1_windowGroup, expand=TRUE)
      outputlabel = glabel("", container=outputGroup, markup=TRUE)
    }
  }

  compareSummaries <-  function(sdcObject, step, ...) {
    if(existd("sdcObject")  == FALSE){
      gmessage("There is no dataset loaded for Comparison!", "No Dataset!",icon="warning")
    } else if(step < 3){
      gmessage("The step has to be more than two!", "Illegal step!",icon="warning")
    } else{
      compareDialog <- gwindow("Compare Dataset", parent=window, width=400, height=200)
      if(length(ActiveSdcVars("numVars"))>0){
        risk <- ActiveSdcVars("risk")
        originalRisk <- ActiveSdcVars("originalRisk")
        utility <- ActiveSdcVars("utility")
        dialoggroup <- ggroup(container=compareDialog, expand=TRUE,horizontal=TRUE)
        origscriptframe <- gframe("original script", container=dialoggroup, expand=TRUE)
        txt <- gtext("", container=origscriptframe, expand=TRUE)
        svalue(txt) <- paste(capture.output(printFrequenciesComp(ActiveSdcObject())), "\n",
                             capture.output(printLocalSuppression(ActiveSdcObject())), "\n",
                             "Disclosure Risk is between: \n [0% ; ", 
                                       round(100*risk$numeric,2), "%] (current)\n 
              (orig: [0 %,", 100, "%]) \n",
                             "- Information Loss:\n    IL1: ", 
                                                            round(utility$il1,2),"\n  - Difference Eigenvalues: ",round(utility$eigen*100,2)," %",
                                                            "\n\n (orig: Information Loss: 0) \n", sep="")
        
        newscriptframe <- gframe(paste("From Step 1 to Step ", step, sep=""), container=dialoggroup, expand=TRUE)
        newtxt <- gtext("", container=newscriptframe, expand=TRUE)
        if(!is.null(sdcObject)) {
        sdcObject <- measure_risk(sdcObject)
          svalue(newtxt) <- paste(capture.output(printFrequenciesComp(sdcObject)),  "\n",
                                  capture.output(printLocalSuppression(sdcObject)), "\n",
                                  "Disclosure Risk is between: \n [0% ; ", 
                                round(100*sdcObject@risk$numeric,2), "%] (current)\n 
              (partial script: [0 %,", 100, "%]) \n",
                                "- Information Loss:\n    IL1: ", 
                                round(sdcObject@utility$il1,2),"\n  - Difference Eigenvalues: ",round(sdcObject@utility$eigen*100,2)," %",
                                "\n\n (orig: Information Loss: 0) \n", sep="")
        } else {
          svalue(newtxt) <- ("no risk information available")
        }
      }
    }
  }
  ## initialize
  # set first run
  putd("firstRun", TRUE)
  # set up new script
  Script.new()
  # get values of internal vars if they exist
  activeDataSet <- if( existd("activeDataSet") ) getd("activeDataSet") else ""
  dataSetName <- if( existd("dataSetName") ) getd("dataSetName") else ""
  # save intitial values in env
  if( !dataSetName=="" ) {
    if(existd(dataSetName)) {
      ActiveDataSet(dataSetName)
    } else {
      dataSetName <- ""
    }
  }
  putd("dataSetName", dataSetName)
  
  #putd("importFileName", "No File imported!")
  
  ## create window
  window = gwindow("sdcMicro GUI")
  addHandlerUnrealize(window, handler = function(h,...) {
        val <- gconfirm("Do you really want to close the window?", parent=h$obj)
        if(as.logical(val))
          return(FALSE)             # destroy
        else
          return(TRUE)              # don't destroy
      })
  ## Menubar
  mbar = list()
  mbar$GUI$Quit$handler = quitGUI
  mbar$GUI$Restart$handler = restartGUI
  mbar$GUI$"Check for updates"$handler = updates22 <- function(...)updates2(restart=TRUE)
  mbar$Data$"Import"$handler = newDataImport
  mbar$Data$"Export"$handler = newDataExport
  mbar$Data$"Use R Dataset"$handler = setDataSet
  mbar$Data$"View"$"Original dataset"$handler = viewOriginalDataset
  mbar$Data$"View"$"Current dataset"$handler = viewDataset
  mbar$Data$"View"$"View Observations violating 2-anonymity"$handler = function(h, ...) viewkanon2anonymity()
  mbar$Data$"View"$"View Observations violating 3-anonymity"$handler = function(h, ...) viewkanon()
  mbar$Data$"View"$"Observations with risk above the benchmark"$handler = function(h, ...) viewhigh()
  mbar$Data$"View"$"Compare dataset"$handler = compareDataset
  mbar$Data$"Variable Manager"$handler = function(h, ...) CreateVariableManager()
  mbar$Script$"Load"$handler = loadScript
  mbar$Script$"Export"$handler = saveAsScript
  mbar$Script$"Save"$handler = saveScript
  mbar$Script$"View"$handler = viewScript
  mbar$"Anonymisation"$"On Categorical key Variables"$Recode$handler = selectKeyVariableInVC
  mbar$"Anonymisation"$"On Categorical key Variables"$Pram$handler = pram1
  mbar$"Anonymisation"$"On Categorical key Variables"$"Local Suppresion(optimal-k-anonymity)"$handler = ls4
  mbar$"Anonymisation"$"On Categorical key Variables"$"Local Suppresion(threshold-indiv.risk)"$handler = plotIndivRisk
  mbar$"Anonymisation"$"On Continuous key Variables"$Microaggregation$handler = nm2
  mbar$"Anonymisation"$"On Continuous key Variables"$"Add Noise"$handler = nm1
  mbar$"Anonymisation"$"On Continuous key Variables"$Shuffling$handler = shuffle1
  mbar$"Anonymisation"$"On Continuous key Variables"$"Top Coding"$handler = topcoding
  #mbar$"SDC Actions"$"Delete Direct Indentifiers"$handler = removeDirectID_menu

  mbar$"Disclosure Risks and Data Utility"$"Categorical Key Variables"$"Population Frequencies and Individual Risks"$handler= function(...) populateFreqandRisk()
  mbar$"Disclosure Risks and Data Utility"$"Categorical Key Variables"$"L-diversity"$handler= function(...) ldiv1()
  mbar$"Disclosure Risks and Data Utility"$"Categorical Key Variables"$"PRAM output"$handler = viewpram1
  mbar$"Disclosure Risks and Data Utility"$"Categorical Key Variables"$"Linear Regression"$handler= function(...) linearRegression()
  mbar$"Disclosure Risks and Data Utility"$"Categorical Key Variables"$"Comparative Frequencies"$handler= function(...) keyVariableFreq()
  mbar$"Disclosure Risks and Data Utility"$"Continuous Key Variables"$"Linear Regression"$handler= function(...) linearRegression()
  mbar$"Disclosure Risks and Data Utility"$"Continuous Key Variables"$"Test for Association"$handler= function(...) spearmanTest()
  mbar$"Disclosure Risks and Data Utility"$"Continuous Key Variables"$"Comparative Univariate Summary"$handler= function(...) compareUnivariateSummary()

  mbar$"Reports and Logs"$"Generate Report"$handler = exportReport
#  mbar$Script$"Run"$handler = runScript
  mbar$Help$"GUI-Tutorial"$handler = vign
  mbar$Help$"SDC Guidelines"$handler = vign2
  mbar$Help$"R sdcMicro Help Files"$"Risk (categorical)"$handler=function(...)helpR("measure_risk")
  mbar$Help$"R sdcMicro Help Files"$"Global Recode"$handler=function(...)helpR("globalRecode")
  mbar$Help$"R sdcMicro Help Files"$"Pram"$handler=function(...)helpR("pram")
  mbar$Help$"R sdcMicro Help Files"$"Local Suppression (optimal - k-Anonymity)"$handler=function(...)helpR("localSuppression")
  mbar$Help$"R sdcMicro Help Files"$"Local Suppression (threshold - indiv.risk)"$handler=function(...)helpR("localSupp")
  mbar$Help$"R sdcMicro Help Files"$"Risk (continuous)"$handler=function(...)helpR("dRisk")
  mbar$Help$"R sdcMicro Help Files"$"Mircoaggregation"$handler=function(...)helpR("microaggregation")
  mbar$Help$"R sdcMicro Help Files"$"Add Noise"$handler=function(...)helpR("addNoise")
  mbar$Help$"R sdcMicro Help Files"$"Shuffling"$handler=function(...)helpR("shuffle")
  mbar$Help$"R sdcMicro Help Files"$"Data Utility (continuous)"$handler=function(...)helpR("dUtility")
  #mbar$Undo$"Undo last action"$handler=OneStepBack
  
  sdcLayout = ggroup(container=window, horizontal=FALSE)
  # Start - add menu
  add(sdcLayout, gmenu(mbar))
  # Start - add toolbar
  toolbargroup <- ggroup(container=sdcLayout, horizontal=TRUE)
  gtoolbar(list(Open=gaction("Load Script", icon="open", handler=function(h,...) { 
                   loadScript()
                }),
                save=gaction("Save Script", icon="save", handler=function(h,...) { 
                    saveScript()
                  }),
                Import=gaction("Import Data", icon="new", handler=function(h,...) newDataImport()),
                Export=gaction("Export Data", icon="convert", handler=function(h,...) newDataExport()),
                Compare=gaction("Compare Data", icon="copy", handler=function(h,...) compareDataset()),
                Manager=gaction("Variable Manager", icon="index", handler=function(h,...) { 
                    CreateVariableManager()
                  }),
                print=gaction("Generate Report", icon="print", handler=function(h,...) { 
                    exportReport()
                  })), container=toolbargroup, expand=TRUE)
  sdcobjectinfo <- glabel("No. of Observations: 0\nNo. of Variables: 0", container=toolbargroup )

  ## main layout
  mainGroupX = ggroup(container=sdcLayout, horizontal=TRUE, expand=TRUE)
  gp <- gpanedgroup(container = mainGroupX, expand=TRUE)
  leftFrame <- gframe("Script", container = gp, horizontal=FALSE, expand=TRUE)
  leftLabelGroup = ggroup(container=leftFrame, horizontal=TRUE)
  leftFrameGroupLabel= glabel("Untitled Script", container=leftLabelGroup, expand=TRUE, handler=function(h,...){
    saveScript()
    name <- basename(getd("activescript.file"))
    if(length(grep("sdcMicroScript",name)) > 0) {
      name <- substr(name, 0, nchar(name) - nchar(".sdcMicroScript"))
    }
    svalue(h$obj) <- name
  })
  leftFrameGroupButton= gbutton("Pop out", container=leftLabelGroup, expand=TRUE, handler=function(h,...) {viewScript()})
  leftgdf <- gdf(Script()$cmd, expand=TRUE, container=leftFrame)
  putd("leftgdf", leftgdf)
  leftgdflist <- list()
  #leftgdflist$"compare with ..."$handler <- function(h,...) {compareDataset()}
  #leftgdflist$delete$handler <- function(h,...) {
  #  gdf <- getd("leftgdf")
  #  dto <- svalue(gdf, index=TRUE)
  #  cmdhist <- gdf[]
  #  if( is.numeric(dto) & !is.na(dto) ) {
  #    if( dto>0 & dto<(length(cmdhist)+1) ) {
  #      cmdhisttmp <- cmdhist[-dto]
  #      gdf[] <- cmdhisttmp
  #      Script(list(cmd=c(cmdhisttmp)))
  #      runScript()
  #    }
  #  }
  #}
  leftgdflist$"Run until and export dataset"$handler <- function(h,...) {
    gdf <- getd("leftgdf")
    rto <- svalue(gdf, index=TRUE)
    cmdhist <- gdf[]
    cmdhist <- cmdhist[c(1:rto)]
    compareDataExport(cmdhist, rto)
  }
  leftgdflist$"Undo until"$handler <- function(h,...) {
    gdf <- getd("leftgdf")
    rto <- svalue(gdf, index=TRUE)
    if(rto > 3) {
      cmdhist <- gdf[]
      cmdhist <- cmdhist[c(1:(rto-1))]
      Script.run(cmdhist)
      gdf[] <- cmdhist
      Script(list(cmd=c(cmdhist)))
    }
  }
  leftgdflist$"Compare dataset to original"$handler <- function(h,...) {
    gdf <- getd("leftgdf")
    rto <- svalue(gdf, index=TRUE)
    cmdhist <- gdf[]
    cmdhist <- cmdhist[c(1:rto)]
    compareDatasetwithsteps(cmdhist, rto)
  }
  leftgdflist$"View risk and info loss"$handler <- function(h,...) {
    gdf <- getd("leftgdf")
    rto <- svalue(gdf, index=TRUE)
    cmdhist <- gdf[]
    cmdhist <- cmdhist[c(1:rto)]
    viewRiskandInfoLoss(cmdhist, rto)
  }

  add3rdmousepopupmenu(leftgdf, leftgdflist)

  size(leftFrame) <- c(200, 400)

  rightgp <- gpanedgroup(container = gp, expand=TRUE)
  rightGroup =  ggroup(container=rightgp, horizontal=TRUE, expand=TRUE)
  middleGroup =  ggroup(container=rightGroup, horizontal=FALSE, expand=TRUE)
  nbMain <- gnotebook(container=middleGroup, closebuttons=FALSE, expand=TRUE)

  rightFrame = gframe("", container = rightgp)
  size(rightFrame) <- c(100, 400)
  rightlayout <- glayout(container=rightFrame, expand=TRUE)
  rfTop <- gframe("Variable List")
  rfTopGroup <- ggroup(expand=TRUE, container = rfTop)
  putd("rfTopTable", gtable(data.frame(Name=c("")), expand=TRUE, container = rfTopGroup))
  rightlayout[1,1, expand=TRUE] <- rfTop
  rfBottom <- gframe("Variable Properties")
  #rfBottomtable <- gtable(data.frame(type=c(""), value=c("")), expand=TRUE, container = rfBottom)
  #putd("rfBottomtable", rfBottomtable)
  #rflayout <- glayout(container = rfBottom,expand=TRUE)
  #rflayout[1,1,expand=TRUE] <- glabel("Name")
  #rflayout[1,2,expand=TRUE] <- rflayoutName <- glabel("")
  #rflayout[2,1,expand=TRUE] <- glabel("Label")
  #rflayout[2,2,expand=TRUE] <- rflayoutLabel <- glabel("")
  #rflayout[3,1,expand=TRUE] <- glabel("Type")
  #rflayout[3,2,expand=TRUE] <- rflayoutType <- glabel("")
  rflayout <- ggroup(container = rfBottom,expand=TRUE, horizontal=FALSE)
  rflayoutNP <- ggroup(container = rflayout, expand=TRUE, horizontal=TRUE)
  rflayoutNPName <- glabel("Name", container = rflayoutNP)
  rflayoutNPValue <- glabel("", container = rflayoutNP)
  rflayoutLP <- ggroup(container = rflayout, expand=TRUE, horizontal=TRUE)
  rflayoutLPName <- glabel("Label", container = rflayoutLP)
  rflayoutLPValue <- glabel("", container = rflayoutLP)
  rflayoutTP <- ggroup(container = rflayout, expand=TRUE, horizontal=TRUE)
  rflayoutTPName <- glabel("Data type", container = rflayoutLP)
  rflayoutTPValue <- glabel("", container = rflayoutLP)

  rfCatlayout <- ggroup(container = rfBottom,expand=TRUE, horizontal=FALSE)
  rfCatlayoutNP <- ggroup(container = rfCatlayout, horizontal=TRUE)
  rfCatlayoutNPName <- glabel("Name", container = rfCatlayoutNP)
  rfCatlayoutNPValue <- glabel("", container = rfCatlayoutNP)
  rfCatlayoutLP <- ggroup(container = rfCatlayout, horizontal=TRUE)
  rfCatlayoutLPName <- glabel("Label", container = rfCatlayoutLP)
  rfCatlayoutLPValue <- glabel("", container = rfCatlayoutLP)
  rfCatlayoutTP <- ggroup(container = rfCatlayout, horizontal=TRUE)
  rfCatlayoutTPName <- glabel("Data type", container = rfCatlayoutTP)
  glabel("Category", container = rfCatlayoutTP)
  rfCatlayoutLY <- glayout(container = rfCatlayout,expand=TRUE)
  visible(rfCatlayout) <- FALSE

  rfNumlayout <- glayout(container = rfBottom)
  rfNumlayout[1,1,expand=TRUE] <- glabel("Name")
  rfNumlayout[1,2,expand=TRUE] <- rfNumlayoutName <- glabel("")
  rfNumlayout[2,1,expand=TRUE] <- glabel("Label")
  rfNumlayout[2,2,expand=TRUE] <- rfNumlayoutLabel <- glabel("")
  rfNumlayout[3,1,expand=TRUE] <- glabel("Data type")
  rfNumlayout[3,2,expand=TRUE] <- glabel("Numerical")
  rfNumlayout[4,1,expand=TRUE] <- glabel("Min")
  rfNumlayout[4,2,expand=TRUE] <- rfNumlayoutMin <- glabel("0")
  rfNumlayout[5,1,expand=TRUE] <- glabel("1st Qu.")
  rfNumlayout[5,2,expand=TRUE] <- rfNumlayout1stQu <- glabel("0")
  rfNumlayout[6,1,expand=TRUE] <- glabel("Median")
  rfNumlayout[6,2,expand=TRUE] <- rfNumlayoutMedian <- glabel("0")
  rfNumlayout[7,1,expand=TRUE] <- glabel("Mean")
  rfNumlayout[7,2,expand=TRUE] <- rfNumlayoutMean <- glabel("0")
  rfNumlayout[8,1,expand=TRUE] <- glabel("3rd Qu.")
  rfNumlayout[8,2,expand=TRUE] <- rfNumlayout3rdQu <- glabel("0")
  rfNumlayout[9,1,expand=TRUE] <- glabel("Max")
  rfNumlayout[9,2,expand=TRUE] <- rfNumlayoutMax <- glabel("0")
  rfNumlayout[10,1,expand=TRUE] <- glabel("Std Dev")
  rfNumlayout[10,2,expand=TRUE] <- rfNumlayoutSD <- glabel("0")

  visible(rfNumlayout) <- FALSE

  rightlayout[2,1, expand=TRUE] <- rfBottom

  mainGroupCat = ggroup(container=nbMain, horizontal=TRUE,label="Categorical", expand=TRUE)
  mainGroupCont = ggroup(container=nbMain, horizontal=TRUE,label="Continuous", expand=TRUE)
  svalue(nbMain) <- 1
  
  tmpCR = gframe('<span foreground="blue" size="large" weight="bold">Disclosure Risk</span>', container=mainGroupCat, horizontal=FALSE,markup=TRUE, expand=TRUE)
  tmpCU = gframe('<span foreground="blue" size="large" weight="bold">Information Loss</span>', container=mainGroupCat, horizontal=FALSE,markup=TRUE, expand=TRUE)

  fc_tmp = gframe('<span size="medium" weight="bold">Observations at risk of</span>', expand=TRUE,markup=TRUE,container= tmpCR)
  
  tmp = gframe("", container=fc_tmp,horizontal=FALSE, expand=TRUE)
  keyvariablerisktable = gtable(data.frame("number"=c(""), "risk.calculations"=c(""), "curr.count"=c(""),"orig.count"=c(""),
                                           "curr.pct"=c(""),"orig.pct"=c(""),stringsAsFactors=FALSE), container=tmp, width=280, height=250, expand=TRUE)
  keyvariableriskgraph = ggraphics(container=tmp, width=280, height=200)
  keyvariableFreqGroup <- gframe('<span size="medium" weight="bold">10 combinations of categories with highest risk</span>',markup=TRUE, container=tmp, width=280, height=250, expand=TRUE,horizontal=FALSE)
  keyvariableFreqTable <- gtable(data.frame("Frequency"=c(""),stringsAsFactors=FALSE), container=keyvariableFreqGroup, width=280, height=250, expand=TRUE)
  putd("keyvariableFreqTable", keyvariableFreqTable)

  tmp = gframe('<span size="medium" weight="bold">Recodings</span>', container=tmpCU,markup=TRUE,horizontal=FALSE, expand=TRUE)
  
  recode_summary <- gtable(returnRecode(), container=tmp,expand=TRUE) 
  tooltip(recode_summary)<- "Recoded values"
  
  tmp = gframe('<span size="medium" weight="bold">Suppressions</span>', container=tmpCU,markup=TRUE, expand=TRUE,horizontal=FALSE)
  keyvariablerecodetable = gtable(data.frame("Key Variables"=c(""), "Suppressions"=c(""),"Percent"=c(""),stringsAsFactors=FALSE), container=tmp, width=280, height=250, expand=TRUE)
  keyvariablerecodegraph = ggraphics(container=tmp, width=280, height=250)

  # Start - Continous Container
  tmpR = gframe('<span foreground="blue" size="large" weight="bold">Disclosure Risk</span>', container=mainGroupCont,horizontal=FALSE,markup=TRUE,expand=TRUE)
  tmpU = gframe('<span foreground="blue" size="large" weight="bold">Information Loss</span>', container=mainGroupCont,horizontal=FALSE,markup=TRUE,expand=TRUE)
  tmp1 = ggroup(container=tmp, horizontal=FALSE)
  addSpring(tmp1)
  
  svalue(nbMain) <- 2
  continuousvariablerisklabel = glabel(text = "", container=tmpR)
  continuousvariableriskgraph = ggraphics(container=tmpR, width=280, height=200)
  nm_util_print = gtext(text="", width=280, height=60, expand=TRUE, container=tmpU)
  tooltip(nm_util_print) <- tt_nmr
  lossgraphcombo <- gcombobox(c(""), container=tmpU, handler=function(h, ...) {
    name <- svalue(h$obj)
    obj <- ActiveSdcObject()
    if(!is.null(name) && is.element(name, names(obj@manipNumVars))) {
      visible(continuousvariablelossgraph) <- TRUE
      boxplot(c(obj@manipNumVars[name], obj@origData[name]), na.action= na.exclude, horizontal=TRUE)
    }
  })
  continuousvariablelossgraph = ggraphics(container=tmpU, width=280, height=200, expand=TRUE)
  svalue(nbMain) <- 1
  # End - numericalMethod Container
}
# TODO: remove for final version
#sdcGUI()
