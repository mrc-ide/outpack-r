Measuring time to get the index
-------------------------------

### Opening an outpack root for the first time

    root <- outpack_root_open("montagu-reports")
    system.time(root$index(skip_cache=TRUE))

    ##    user  system elapsed 
    ##   3.003   0.054   3.053

### Adding a packet and then opening the root again

    root <- outpack_root_open("montagu-reports")
    system.time(create_random_packet(root))

    ##    user  system elapsed 
    ##   0.791   0.016   0.789

    system.time(root$index())

    ##    user  system elapsed 
    ##   0.025   0.000   0.024

### Getting the index from cache

    root <- outpack_root_open("montagu-reports")
    system.time(root$index())

    ##    user  system elapsed 
    ##   0.348   0.000   0.347
