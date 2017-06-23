## Test environments

* local OS X install, R 3.4.0
* ubuntu 12.04.5 (on travis-ci), R 3.4.0
* win-builder (devel and release)

## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE (on win-builder only):

* Possibly mis-spelled words in DESCRIPTION:
    API (2:18, 5:36)
    TheyWorkForYou (2:26, 5:44)
  
Camel case spelling (and API) is this organization's prefered
terminology, e.g. the first paragraph of the their introduction to the
interface at https://www.theyworkforyou.com/api/ is: 

  > "Welcome to TheyWorkForYou's API section. The API (Application 
  > Programming Interface) is a way of querying our database for 
  > information"

## Downstream dependencies

There are no downstream dependencies as this is a new package
