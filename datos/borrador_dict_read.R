setwd("C:/Users/Alfredo/Desktop/freelancer/faraon/datos/")
cartera <- read_csv("cartera2.txt", sep=",")

with open('cartera2.txt') as f:
  data = f.read()

library(lingmatch)

read.dic('cartera2.txt')