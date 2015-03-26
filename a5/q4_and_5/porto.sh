#!/usr/bin/env bash

./dist/build/decision-tree/decision-tree --dot porto.dot <(tail -n +2 porto_math_train.csv) <(tail -n +2 porto_math_test.csv) school sex age address famsize Pstatus Medu Fedu caretaker_parent guardian traveltime studytime failures schoolsup famsup paid activities nursery higher internet romantic famrel freetime goout Dalc Walc health absences

