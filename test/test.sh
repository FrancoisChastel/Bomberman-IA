base=tests_functions.pl
PL=swipl


swipl --goal=run_tests --stand_alone=true -o Execute_Tests -c tests_functions.pl

./Execute_Tests > tests_results

