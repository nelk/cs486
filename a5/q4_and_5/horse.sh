#!/usr/bin/env bash

./dist/build/decision-tree/decision-tree --dot horse.dot horseTest.txt horseTrain.txt K Na CL HCO3 Endotoxin AnionGap PLA2 SDH GLDH TPP BreathRate PCV PulseRate Fibrinogen Dimer FibPerDim
dot -Tpdf horse.dot > horse.pdf

