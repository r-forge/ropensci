---
title: rfigshare - Share and explore figures, data, and publications on FigShare using R

---

![](http://farm9.staticflickr.com/8180/7950489358_ea902bdaae_o.png)



# Obtaining your API keys

There is a nice video introduction to creating applications for the API on the [figshare blog](http://figshare.com/blog/figshare_API_available_to_all/48).  The following tutorial provides a simple walkthrough of how to go about getting your figshare API keys set up so that you can use the `rfigshare` package.  


Create a user account on [FigShare](http://figshare.com) and log in.  From your homepage, select "Applications" from the drop-down menu,

![](http://farm9.staticflickr.com/8171/7950489558_5172515057_o.png)

Create a new application:

![](http://farm9.staticflickr.com/8038/7950490158_7feaf680bd_o.png)


Enter in the following information: 

![](http://farm9.staticflickr.com/8305/7950490562_02846cea92_o.png)

Then navigate over to the permissions tab.  To get the most out of `rfigshare` you'll want to enable all permissions:

![](http://farm9.staticflickr.com/8448/7950491064_c3820e62bd_o.png)

Save the new settings, and then open the application again (View/Edit menu) and click on the "Access Codes" tab.

![](http://farm9.staticflickr.com/8308/7950491470_621da9c5d1_o.png)

Record each if the keys into R as follows.  You might want to put this bit of R code into your `.Rprofile` to avoid entering it each time in the future:

```r
options(FigshareKey = "qMDabXXXXXXXXXXXXXXXXX")
options(FigsharePrivateKey = "zQXXXXXXXXXXXXXXXXXXXX")
options(FigshareToken = "SrpxabQXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
options(FigsharePrivateToken = "yqXXXXXXXXXXXXXXXXXXXX")
```


## Installing the R package

Now that we have the API credentials in place, actually using `rfigshare` is pretty easy.  Install the latest version of package via CTAN:  
```r
install.packages("rfigshare")
```
or install the latest version directly from Github using (you previously need to install devtools):
```r
require(devtools)
install_github("rfigshare", "ropensci")
```

# Using rfigshare


Try authenticating with your credentials:


```r
require(rfigshare)
```

```
## Loading required package: rfigshare
```

```r
fs_auth()
```

```
## Error: Missing Figshare app secret
```



Try a search for an author:


```r
fs_author_search("Boettiger")
```

```
## Error: Requires authentication.  Are your credentials stored in options?
## See fs_auth function for details.
```


Try creating your own content:


```r
id <- fs_create("Test title", "description of test", "dataset")
```

```
## Error: Requires authentication.  Are your credentials stored in options?
## See fs_auth function for details.
```


This creates an article with the essential metadata information. We can now upload the dataset to this newly created figshare object using `fs_upload`.  For the purposes of this example, we'll just upload one of R's built-in datasets:


```r
data(mtcars)
write.csv(mtcars, "mtcars.csv")

fs_upload(id, "mtcars.csv")
```

```
## Error: object 'id' not found
```


Not that we must pass the id number of our the newly created figshare object as the first argument.  Similar functions let us add additional authors, tags, categories, and links, e.g.


```r
fs_add_tags(id, "demo")
```

```
## Error: object 'id' not found
```


The file we have created remains saved as a draft until we publish it, either publicly or privately.  Note that once a file is declared public, it cannot be made private or deleted.  Let's release this dataset privately:


```r
fs_make_private(id)
```

```
## Error: object 'id' not found
```


We can now share the dataset with collaborators by way of the private url.  

### The quick and easy way

The `rfigshare` package will let you create a new figshare article with additional authors, tags, categories, etc in a single command usnig the `fs_new_article` function.  The essential metadata `title`, `description` and `type` are required, but any other information we omit at this stage can be added later.  If we set `visibility` to private or public, the article is published on figshare immediately.  


```r
id <- fs_new_article(title="A Test of rfigshare", 
                     description="This is a test of the fs_new_aricle function and related methods", 
                     type="figure", 
                     authors=c("Karthik Ram", "Scott Chamberlain"), 
                     tags=c("ecology", "openscience"), 
                     categories="Ecology", 
                     links="http://ropensci.org", 
                     files="figure/rfigshare.png",
                     visibility="private")
```

```
## Error: Requires authentication.  Are your credentials stored in options?
## See fs_auth function for details.
```


# Examining Data on Figshare

We can view all available metadata of a figshare object


```r
fs_details(id)
```

```
## Error: object 'id' not found
```


You can see all of the files you have:


```r
fs_browse(mine = TRUE)
```

```
## Error: Requires authentication.  Are your credentials stored in options?
## See fs_auth function for details.
```




