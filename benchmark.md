Measuring time to get the index
-------------------------------

### Opening an outpack root for the first time

    root <- outpack_root_open("montagu-reports")
    system.time(root$index())

    ##    user  system elapsed 
    ##   0.370   0.000   0.369

### Adding a packet and then opening the root again

    root <- outpack_root_open("montagu-reports")
    create_random_packet(root)

    ## [1] "20221101-131950-6bf38a67"

    system.time(root$index())

    ##    user  system elapsed 
    ##   0.025   0.000   0.025
