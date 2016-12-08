## Resubmission
This is a resubmission. In this version I have:

* Passed title to title case in particular
  from:
  ‘Miscellaneous functions for data manipulation’
  to:
  ‘Miscellaneous Functions for Data Manipulation’

* Import methods
  added importFrom("methods", "as") NAMESPACE file and 'methods' to DESCRIPTION file
## Test environments
* local OS X install, 3.2.4
* win-builder 3.3.2 and under development

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking R code for possible problems ... NOTE
  addLabelCategorizationIntervals: no visible binding for global variable
    ‘label_start’
  addLabelCategorizationIntervals: no visible binding for global variable
    ‘close_left’
  addLabelCategorizationIntervals: no visible binding for global variable
    ‘label_end’
  addLabelCategorizationIntervals: no visible binding for global variable
    ‘close_right’
  addLabelCategorizationIntervals: no visible binding for global variable
    ‘label’
  as.CategorizationIntervals: no visible binding for global variable
    ‘value’
  as.CategorizationIntervals: no visible binding for global variable
    ‘close_left’
  as.CategorizationIntervals: no visible binding for global variable
    ‘close_right’
  categorizeByIntervals: no visible binding for global variable ‘value’
  categorizeByIntervals: no visible binding for global variable
    ‘category’
  categorizeByIntervals: no visible binding for global variable
    ‘close_left’
  categorizeByIntervals: no visible binding for global variable
    ‘close_right’
  categorizeByIntervals: no visible binding for global variable ‘values’
  categorizeByIntervals: no visible binding for global variable ‘id’
  extractCategoriesFromIntervals: no visible binding for global variable
    ‘close_left’
  extractCategoriesFromIntervals: no visible binding for global variable
    ‘intervals’
  extractCategoriesFromIntervals: no visible binding for global variable
    ‘close_right’
  freqTable: no visible global function definition for ‘.’
  freqTable: no visible binding for global variable ‘perc’
  freqTable: no visible binding for global variable ‘freq’
  mergeWithColPrioritization: no visible binding for global variable
    ‘present_in_prior’
  modeOfVector: no visible global function definition for ‘.’
  modeOfVector: no visible binding for global variable ‘value’
  modeOfVector: no visible binding for global variable ‘freq’
  numUniqueValues: no visible binding for global variable ‘value’
  propCasesAreMode: no visible global function definition for ‘.’
  propCasesAreMode: no visible binding for global variable ‘value’
  propCasesAreMode: no visible binding for global variable ‘prop’
  
  
  This is due to use of data.table objects.
  
  
## Downstream dependencies
No downtream dependencies
