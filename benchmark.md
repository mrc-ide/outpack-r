Measuring time to get the index
-------------------------------

### Opening an outpack root for the first time

    root <- outpack_root_open("montagu-reports")
    system.time(root$index(skip_cache=TRUE))

    ##    user  system elapsed 
    ##   3.222   0.076   3.296

### Adding a packet and then opening the root again

    root <- outpack_root_open("montagu-reports")
    system.time(create_random_packet(root))

    ##    user  system elapsed 
    ##   0.663   0.035   0.629

    system.time(root$index())

    ##    user  system elapsed 
    ##   0.024   0.000   0.025

### Getting the index from cache

    root <- outpack_root_open("montagu-reports")
    system.time(root$index())

    ##    user  system elapsed 
    ##   0.129   0.001   0.130
