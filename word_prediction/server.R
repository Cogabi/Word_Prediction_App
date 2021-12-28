library(shiny)
library(shinyjs)
library(stringr)
library(ngram)
library(tm)
library(RWeka)
library(dplyr)
library(tibble)
library(stringr)
library(tidyr)
library(quanteda)
library(quanteda.textstats)
library(data.table)


unigs <- readRDS("./unigram.rds")
bigrs <- readRDS("./bigram.rds")
trigs <- readRDS("./trigram.rds")

shinyServer(function(session, input, output) {
    
    reval <- reactive({
        
        gamma2 <- 0.5  # bigram discount
        gamma3 <- 0.5  # trigram discount   
        
        if (is.na(filter(unigs, ngram==word(input$text,-1))$freq[1])) {
            unigs[nrow(unigs)+1,1] <-word(input$text,-1)
            unigs[nrow(unigs),2] <-1
        }
        
        if (input$text=="") {
            return(NULL)
        }
        else 
            if (str_count(input$text," ")==0) {
                bigPre <- word(input$text,-1)
                
                getObsBigrs <- function(bigPre, bigrams) {
                    bigrs.winA <- data.frame(ngram=vector(mode = 'character', length = 0),
                                             freq=vector(mode = 'integer', length = 0))
                    regex <- sprintf("%s%s%s", "^", bigPre, " ")
                    bigram_indices <- grep(regex, bigrams$ngram)
                    if(length(bigram_indices) > 0) {
                        bigrs.winA <- bigrams[bigram_indices, ]
                    }
                    
                    return(bigrs.winA)
                }
                
                
                getObsBiProbs <- function(obsBigrs, bigrs, bigPre, biDisc=0.5) {
                    if(nrow(obsBigrs) < 1) return(NULL)
                    obsCount <- filter(unigs, ngram==bigPre)$freq[1]
                    obsBigrProbs <- mutate(obsBigrs, freq=((freq - biDisc) / obsCount))
                    colnames(obsBigrProbs) <- c("ngram", "prob")
                    
                    return(obsBigrProbs)
                }
                
                obs_bigrs <- getObsBigrs(bigPre, bigrs)
                qbo_obs_bigrams <- getObsBiProbs(obs_bigrs, bigrs, bigPre, gamma2)
                
                
                getAlphaBigram <- function(unigram, bigrams, bigDisc=0.5) {
                    # get all bigrams that start with unigram
                    regex <- sprintf("%s%s%s", "^", unigram$ngram[1], " ")
                    bigsThatStartWithUnig <- bigrams[grep(regex, bigrams$ngram),]
                    if(nrow(bigsThatStartWithUnig) < 1) return(0)
                    alphaBi <- 1 - (sum(bigsThatStartWithUnig$freq - bigDisc) / unigram$freq)
                    
                    return(alphaBi)
                }
                
                unig <- unigs[unigs$ngram == bigPre,]
                alpha_big <- getAlphaBigram(unig, bigrs, gamma2)
                
                
                getUnobsBiProbs <- function(obsBigrs, unigs, alphaBig) {
                    # get the unobserved bigram tails
                    obsBigrTails <- str_split_fixed(obsBigrs$ngram, " ", 2)[, 2]
                    unobsBigrTails <- unigs[!(unigs$ngram %in% obsBigrTails), ]$ngram
                    # convert to data.frame with counts
                    qboUnobsBigs <- unigs[unigs$ngram %in% unobsBigrTails, ]
                    denom <- sum(qboUnobsBigs$freq)
                    # converts counts to probabilities
                    qboUnobsBigs <- data.frame(ngram=qboUnobsBigs$ngram,
                                               prob=(alphaBig * qboUnobsBigs$freq / denom))
                    
                    return(qboUnobsBigs)
                }
                
                qbo_unobs_bigrams <- getUnobsBiProbs(qbo_obs_bigrams,
                                                     unigs, alpha_big)
                
                qbo_bigrams <- rbind(qbo_obs_bigrams, qbo_unobs_bigrams)
                qbo_bigrams <- qbo_bigrams[order(-qbo_bigrams$prob), ]
                
                predictWord <- function(qbo_bigrs) {
                    result <- c(str_split(qbo_bigrs$ngram[1], " ")[[1]][2],str_split(qbo_bigrs$ngram[2], " ")[[1]][2],
                                str_split(qbo_bigrs$ngram[3], " ")[[1]][2],str_split(qbo_bigrs$ngram[4], " ")[[1]][2],
                                str_split(qbo_bigrs$ngram[5], " ")[[1]][2])
                    
                    return(result)
                }
                
                predictWord(qbo_bigrams)  
            }
        else {
            bigPre <- word(input$text,-2,-1)  
            
            getObsTrigs <- function(bigPre, trigrams) {
                trigs.winA <- data.frame(ngram=vector(mode = 'character', length = 0),
                                         freq=vector(mode = 'integer', length = 0))
                regex <- sprintf("%s%s%s", "^", bigPre, " ")
                trigram_indices <- grep(regex, trigrams$ngram)
                if(length(trigram_indices) > 0) {
                    trigs.winA <- trigrams[trigram_indices, ]
                }
                
                return(trigs.winA)
            }
            
            getObsTriProbs <- function(obsTrigs, bigrs, bigPre, triDisc=0.5) {
                if(nrow(obsTrigs) < 1) return(NULL)
                obsCount <- filter(bigrs, ngram==bigPre)$freq[1]
                obsTrigProbs <- mutate(obsTrigs, freq=((freq - triDisc) / obsCount))
                colnames(obsTrigProbs) <- c("ngram", "prob")
                
                return(obsTrigProbs)
            }
            
            
            obs_trigs <- getObsTrigs(bigPre, trigs)  # get trigrams and counts
            # convert counts to probabilities
            qbo_obs_trigrams <- getObsTriProbs(obs_trigs, bigrs, bigPre, gamma3)
            
            getUnobsTrigTails <- function(obsTrigs, unigs) {
                obs_trig_tails <- str_split_fixed(obsTrigs, " ", 3)[, 3]
                unobs_trig_tails <- unigs[!(unigs$ngram %in% obs_trig_tails), ]$ngram
                return(unobs_trig_tails)
            }
            
            
            unobs_trig_tails <- getUnobsTrigTails(obs_trigs$ngram, unigs)
            
            getAlphaBigram <- function(unigram, bigrams, bigDisc=0.5) {
                # get all bigrams that start with unigram
                regex <- sprintf("%s%s%s", "^", unigram$ngram[1], " ")
                bigsThatStartWithUnig <- bigrams[grep(regex, bigrams$ngram),]
                if(nrow(bigsThatStartWithUnig) < 1) return(0)
                alphaBi <- 1 - (sum(bigsThatStartWithUnig$freq - bigDisc) / unigram$freq)
                
                return(alphaBi)
            }
            
            unig <- str_split(bigPre, " ")[[1]][2]
            unig <- unigs[unigs$ngram == unig,]
            alpha_big <- getAlphaBigram(unig, bigrs, gamma2)
            
            
            getBoBigrams <- function(bigPre, unobsTrigTails) {
                w_i_minus1 <- str_split(bigPre, " ")[[1]][2]
                boBigrams <- paste(w_i_minus1, unobsTrigTails, sep = " ")
                return(boBigrams)
            }
            
            getObsBoBigrams <- function(bigPre, unobsTrigTails, bigrs) {
                boBigrams <- getBoBigrams(bigPre, unobsTrigTails)
                obs_bo_bigrams <- bigrs[bigrs$ngram %in% boBigrams, ]
                return(obs_bo_bigrams)
            }
            
            getUnobsBoBigrams <- function(bigPre, unobsTrigTails, obsBoBigram) {
                boBigrams <- getBoBigrams(bigPre, unobsTrigTails)
                unobs_bigs <- boBigrams[!(boBigrams %in% obsBoBigram$ngram)]
                return(unobs_bigs)
            }
            
            getObsBigProbs <- function(obsBoBigrams, unigs, bigDisc=0.5) {
                first_ngrams <- str_split_fixed(obsBoBigrams$ngram, " ", 2)[, 1]
                first_ngram_freqs <- unigs[unigs$ngram %in% first_ngrams, ]
                obsBigProbs <- (obsBoBigrams$freq - bigDisc) / first_ngram_freqs$freq
                obsBigProbs <- data.frame(ngram=obsBoBigrams$ngram, prob=obsBigProbs)
                
                return(obsBigProbs)
            }
            
            getQboUnobsBigrams <- function(unobsBoBigrams, unigs, alphaBig) {
                # get the unobserved bigram tails
                qboUnobsBigs <- str_split_fixed(unobsBoBigrams, " ", 2)[, 2]
                w_in_Aw_iminus1 <- unigs[!(unigs$ngram %in% qboUnobsBigs), ]
                # convert to data.frame with counts
                qboUnobsBigs <- unigs[unigs$ngram %in% qboUnobsBigs, ]
                denom <- sum(qboUnobsBigs$freq)
                # converts counts to probabilities
                qboUnobsBigs <- data.frame(ngram=unobsBoBigrams,
                                           prob=(alphaBig * qboUnobsBigs$freq / denom))
                
                return(qboUnobsBigs)
            }
            
            bo_bigrams <- getBoBigrams(bigPre, unobs_trig_tails)  # get backed off bigrams
            # separate bigrams which use eqn 10 and those that use 16
            obs_bo_bigrams <- getObsBoBigrams(bigPre, unobs_trig_tails, bigrs)
            unobs_bo_bigrams <- getUnobsBoBigrams(bigPre, unobs_trig_tails, obs_bo_bigrams)
            # unobs_bo_bigrams = c("the_buy", "the_EOS", "the_paint", "the_sell", "the_the")
            # calc obs'd bigram prob's from eqn 10
            qbo_obs_bigrams <- getObsBigProbs(obs_bo_bigrams, unigs, gamma2) #ngram     probs
            # calc alpha_big & unobs'd bigram prob's from eqn 16             #the_house 0.3125
            unig <- str_split(bigPre, " ")[[1]][2]
            unig <- unigs[unigs$ngram == unig,]
            # distrib discounted bigram prob mass to unobs bigrams in prop to unigram ML
            qbo_unobs_bigrams <- getQboUnobsBigrams(unobs_bo_bigrams, unigs, alpha_big)
            qbo_bigrams <- rbind(qbo_obs_bigrams, qbo_unobs_bigrams)
            
            getAlphaTrigram <- function(obsTrigs, bigram, triDisc=0.5) {
                if(nrow(obsTrigs) < 1) return(1)
                alphaTri <- 1 - sum((obsTrigs$freq - triDisc) / bigram$freq[1])
                
                return(alphaTri)
            }
            
            bigram <- bigrs[bigrs$ngram %in% bigPre, ]
            alpha_trig <- getAlphaTrigram(obs_trigs, bigram, gamma3)
            
            getUnobsTriProbs <- function(bigPre, qboObsBigrams,
                                         qboUnobsBigrams, alphaTrig) {
                qboBigrams <- rbind(qboObsBigrams, qboUnobsBigrams)
                qboBigrams <- qboBigrams[order(-qboBigrams$prob), ]
                sumQboBigs <- sum(qboBigrams$prob)
                first_bigPre_ngram <- str_split(bigPre, " ")[[1]][1]
                unobsTrigNgrams <- paste(first_bigPre_ngram, qboBigrams$ngram, sep=" ")
                unobsTrigProbs <- alphaTrig * qboBigrams$prob / sumQboBigs
                unobsTrigDf <- data.frame(ngram=unobsTrigNgrams, prob=unobsTrigProbs)
                
                return(unobsTrigDf)
            }
            
            qbo_unobs_trigrams <- getUnobsTriProbs(bigPre, qbo_obs_bigrams,
                                                   qbo_unobs_bigrams, alpha_trig)
            
            predictWord <- function(qbo_trigs) {
                result <- c(str_split(qbo_trigs$ngram[1], " ")[[1]][3],str_split(qbo_trigs$ngram[2], " ")[[1]][3],
                            str_split(qbo_trigs$ngram[3], " ")[[1]][3],str_split(qbo_trigs$ngram[4], " ")[[1]][3],
                            str_split(qbo_trigs$ngram[5], " ")[[1]][3])
                
                return(result)
            }
            
            qbo_trigrams <- rbind(qbo_obs_trigrams, qbo_unobs_trigrams)
            qbo_trigrams <- qbo_trigrams[order(-qbo_trigrams$prob), ]  # sort by desc prob
            
            predictWord(qbo_trigrams) 
        }
        
        
        
    })
    
    # Fill the buttons with the predicted values
    output$wordOneButton <- renderUI({
        actionButton("action", label = reval()[1], width = '26.5%', style = "font-size: 20px")
    })
    
    observe(shinyjs::toggle("wordOneButton", condition = !is.null(reval()[1])))
    
    output$wordTwoButton <- renderUI({
        actionButton("action2", label = reval()[2], width = '26.5%', style = "font-size: 20px")})
    
    observe(shinyjs::toggle("wordTwoButton", condition = !is.null(reval()[2])))
    
    output$wordThreeButton <- renderUI({
        actionButton("action3", label = reval()[3], width = '26.5%', style = "font-size: 20px")})
    
    observe(shinyjs::toggle("wordThreeButton", condition = !is.null(reval()[3])))
    
    output$wordFourButton <- renderUI({
        actionButton("action4", label = reval()[4], width = '26.5%', style = "font-size: 20px")})
    
    observe(shinyjs::toggle("wordFourButton", condition = !is.null(reval()[4])))
    
    output$wordFiveButton <- renderUI({
        actionButton("action5", label = reval()[5], width = '26.5%', style = "font-size: 20px")})
    
    observe(shinyjs::toggle("wordFiveButton", condition = !is.null(reval()[5])))
    
    # Include predicted word in the text after click event on button
    observeEvent(input$action, {
        name <- paste0(input$text, " ",reval()[1])
        updateTextInput(session = session, "text", value = name)
    })
    
    observeEvent(input$action2, {
        name <- paste0(input$text, " ",reval()[2])
        updateTextInput(session = session, "text", value = name)
    })
    
    observeEvent(input$action3, {
        name <- paste0(input$text, " ",reval()[3])
        updateTextInput(session = session, "text", value = name)
    })
    
    observeEvent(input$action4, {
        name <- paste0(input$text, " ",reval()[4])
        updateTextInput(session = session, "text", value = name)
    })
    
    observeEvent(input$action5, {
        name <- paste0(input$text, " ",reval()[5])
        updateTextInput(session = session, "text", value = name)
    })
    
})
