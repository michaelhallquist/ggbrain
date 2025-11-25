# summary S3 method for ggbrain_images objects addition operator for combining ggbrain_images objects

summary S3 method for ggbrain_images objects addition operator for
combining ggbrain_images objects

## Usage

``` r
# S3 method for class 'ggbrain_images'
o1 + o2
```

## Arguments

- o1:

  first ggbrain_images object

- o2:

  second ggbrain_images object

## Value

combined ggbrain_images object

## Details

note that the addition does not modify either existing object. Rather,
the first object is cloned and the second is added to it. If you want to
add one ggbrain_images object to another in place (i.e., modifying the
extant object), use the \$add() method.
