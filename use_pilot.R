# testing of pilot
# 
library(tidyverse)
library(likert)
library(psych)
# library(gridExtra)
# library(kableExtra)
# library(jpeg)
# library(png)
# library(grid)

# Strongly disagree - 1
# Disagree - 2
# Neither Agree nor Disagree - 3
# Agree - 4
# Strongly Agree - 5		


# import three questionnaire sections		
navigation <- read.csv("data/Learn Feedback Form - navigation.csv", stringsAsFactors = FALSE)
content <- read.csv("data/Learn Feedback Form - content.csv")
aesthetics <- read.csv("data/Learn Feedback Form - aesthetics.csv")

# navigation
# reorder factor levels and rename q's
navigation <- navigation %>% 
  mutate("It was easy to locate the lesson" = factor(It.was.easy.to.locate.the.lesson, 
                                                     levels = c("Strongly\ndisagree",
                                                                "Disagree",
                                                                "Neither agree\nnor Disagree",
                                                                "Agree",
                                                                "Strongly\nagree")),
         "The menus were readable" = factor(The.menus.were.readable, 
                                                     levels = c("Strongly\ndisagree",
                                                                "Disagree",
                                                                "Neither agree\nnor Disagree",
                                                                "Agree",
                                                                "Strongly\nagree")),
         "I always knew where I was in the app" = factor(I.always.knew.where.I.was.in.the.app, 
                                                     levels = c("Strongly\ndisagree",
                                                                "Disagree",
                                                                "Neither agree\nnor Disagree",
                                                                "Agree",
                                                                "Strongly\nagree")),
         "I had difficulty locating the lesson" = factor(I.had.difficulty.locating.the.lesson, 
                                                     levels = c("Strongly\ndisagree",
                                                                "Disagree",
                                                                "Neither agree\nnor Disagree",
                                                                "Agree",
                                                                "Strongly\nagree")),
         "Navigation was intuitive" = factor(Navigation.was.intuitive, 
                                                         levels = c("Strongly\ndisagree",
                                                                    "Disagree",
                                                                    "Neither agree\nnor Disagree",
                                                                    "Agree",
                                                                    "Strongly\nagree")))



navlik <- likert(navigation[7:11])
navplot <- plot(navlik,
              legend.position = "top",
              legend = "",
              text.size = 4,
              low.color = "lightblue",
              high.col = "lightgreen")

# content
names(content)
# "I.feel.like.the.content.was.well.paced"
# "I.found.it.difficult.to.keep.pace.with.the.content"
# "The.content.was.too.easy"
# "The.content.was.too.hard"                          
# "I.enjoyed.the.content"
# "I.did.not.feel.engaged.with.the.content"           

# reorder factor levels and rename q's
content <- content %>% 
  mutate("I feel like the content was well paced" = factor(I.feel.like.the.content.was.well.paced, 
                                                     levels = c("Strongly\ndisagree",
                                                                "Disagree",
                                                                "Neither agree\nnor Disagree",
                                                                "Agree",
                                                                "Strongly\nagree")),
         "I found it difficult to keep pace with the content" = factor(I.found.it.difficult.to.keep.pace.with.the.content, 
                                            levels = c("Strongly\ndisagree",
                                                       "Disagree",
                                                       "Neither agree\nnor Disagree",
                                                       "Agree",
                                                       "Strongly\nagree")),
         "The content was too easy" = factor(The.content.was.too.easy, 
                                                         levels = c("Strongly\ndisagree",
                                                                    "Disagree",
                                                                    "Neither agree\nnor Disagree",
                                                                    "Agree",
                                                                    "Strongly\nagree")),
         "The content was too hard" = factor(The.content.was.too.hard, 
                                                         levels = c("Strongly\ndisagree",
                                                                    "Disagree",
                                                                    "Neither agree\nnor Disagree",
                                                                    "Agree",
                                                                    "Strongly\nagree")),
         "I enjoyed the content" = factor(I.enjoyed.the.content, 
                                             levels = c("Strongly\ndisagree",
                                                        "Disagree",
                                                        "Neither agree\nnor Disagree",
                                                        "Agree",
                                                        "Strongly\nagree")),
         "I did not feel engaged with the content" = factor(I.did.not.feel.engaged.with.the.content, 
                                          levels = c("Strongly\ndisagree",
                                                     "Disagree",
                                                     "Neither agree\nnor Disagree",
                                                     "Agree",
                                                     "Strongly\nagree")))



contlik <- likert(content[8:13])
contplot <- plot(contlik,
                legend.position = "top",
                legend = "",
                text.size = 4,
                low.color = "lightblue",
                high.col = "lightgreen")


# aesthetics
# "I.thought.the.app.was.pretty"
# "The.app.was.too.cluttered"
# "The.app.s.design.was.distracting"
# "The.app.was.too.bare"
# "I.thought.the.app.looked.cool"

# reorder factor levels and rename q's
aesthetics <- aesthetics %>% 
  mutate("I thought the app was pretty" = factor(I.thought.the.app.was.pretty, 
                                                     levels = c("Strongly\ndisagree",
                                                                "Disagree",
                                                                "Neither agree\nnor Disagree",
                                                                "Agree",
                                                                "Strongly\nagree")),
         "The app was too cluttered " = factor(The.app.was.too.cluttered, 
                                            levels = c("Strongly\ndisagree",
                                                       "Disagree",
                                                       "Neither agree\nnor Disagree",
                                                       "Agree",
                                                       "Strongly\nagree")),
         "The app's design was distracting" = factor(The.app.s.design.was.distracting, 
                                                         levels = c("Strongly\ndisagree",
                                                                    "Disagree",
                                                                    "Neither agree\nnor Disagree",
                                                                    "Agree",
                                                                    "Strongly\nagree")),
         "The app was too bare" = factor(The.app.was.too.bare, 
                                                         levels = c("Strongly\ndisagree",
                                                                    "Disagree",
                                                                    "Neither agree\nnor Disagree",
                                                                    "Agree",
                                                                    "Strongly\nagree")),
         "I thought the app looked cool" = factor(I.thought.the.app.looked.cool, 
                                             levels = c("Strongly\ndisagree",
                                                        "Disagree",
                                                        "Neither agree\nnor Disagree",
                                                        "Agree",
                                                        "Strongly\nagree")))



aeslik <- likert(aesthetics[8:12])
aesplot <- plot(aeslik,
                legend.position = "top",
                legend = "",
                text.size = 4,
                low.color = "lightblue",
                high.col = "lightgreen")
