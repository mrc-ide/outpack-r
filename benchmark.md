Measuring time to get the index
-------------------------------

### Opening an outpack root for the first time

    root <- outpack_root_open("montagu-reports")
    system.time(root$index(skip_cache=TRUE))

    ##    user  system elapsed 
    ##   3.155   0.000   3.166

### Adding a packet and then opening the root again

    root <- outpack_root_open("montagu-reports")
    create_random_packet(root)

    ## [1] "20221101-132140-d4811f07"

    system.time(root$index())

    ##    user  system elapsed 
    ##   0.026   0.000   0.026
