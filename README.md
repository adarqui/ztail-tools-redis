# ztail-tools-redis

EXPERIMENTAL LOG SHUTTLE.

Simple tool to shuttle logs from various servers via a ztail-enqueue agent, to a centralized dump server (ztail-dump). This tool is for INTERNAL use.

Similar to ZTail-Tools. The two differences:
- Refactored quite a bit of code.
- Ripped out my Abstract library, choosing to use redis directly.


## build

```
make
```


## bins

Some precompiled linux bins, use at your own risk. 8|

```
./precompiled_bins
```


## running

### enqueuer
- This will monitor /var/log recursively, then shuttle logs to the redis endpoint specified

```
./.cabal-sandbox/bin/ztail-enqueue host_identifier redis_host -A -D /var/log/
```
enqueue api:
- curl localhost:60002 to query ztail-enqueue

```
$ curl -H 'Accept: application/json' localhost:60002
{"length_gauge":{"type":"g","val":8680},"ekg":{"server_timestamp_ms":{"type":"c","val":1432936061167}},"log_counter":{"type":"c","val":3001},"argv":{"type":"l","val":"hi localhost -l -r -A -D /tmp/mock"},"rts":{"gc":{"gc_cpu_ms":{"type":"c","val":0},"mutator_wall_ms":{"type":"c","val":0},"mutator_cpu_ms":{"type":"c","val":0},"gc_wall_ms":{"type":"c","val":0},"wall_ms":{"type":"c","val":0},"bytes_copied":{"type":"c","val":0},"max_bytes_used":{"type":"g","val":0},"max_bytes_slop":{"type":"g","val":0},"num_bytes_usage_samples":{"type":"c","val":0},"peak_megabytes_allocated":{"type":"g","val":0},"cpu_ms":{"type":"c","val":0},"current_bytes_used":{"type":"g","val":0},"bytes_allocated":{"type":"c","val":0},"par_max_bytes_copied":{"type":"g","val":0},"current_bytes_slop":{"type":"g","val":0},"cumulative_bytes_used":{"type":"c","val":0},"num_gcs":{"type":"c","val":0},"par_tot_bytes_copied":{"type":"g","val":0},"par_avg_bytes_copied":{"type":"g","val":0}}},"error_counter":{"type":"c","val":0},"log_distribution":{"max":4,"mean":2.8923692102632517,"count":3001,"min":0.0,"variance":0.11670618291070561,"type":"d","sum":8680}}

$ curl -H 'Accept: application/json' localhost:60002/log_counter
{"type":"c","val":3001}

$ curl -H 'Accept: application/json' localhost:60002/error_counter
{"type":"c","val":0}

$ curl -H 'Accept: application/json' localhost:60002/log_distribution
{"max":4,"mean":2.8925268682829346,"count":4001,"min":0.0,"variance":0.11641753395692547,"type":"d","sum":11573}
```


### dumper
- This will dump everything to /dump/<host>/path/...

```
./.cabal-sandbox/bin/ztail-dump /dump redis_host'
```

dumper api:
- curl localhost:60003 to query dumper

api calls:

```
curl -H 'Accept: application/json' localhost:60002
curl -H 'Accept: application/json' localhost:60002/log_counter
curl -H 'Accept: application/json' localhost:60002/length_gauge
curl -H 'Accept: application/json' localhost:60002/log_distribution
curl -H 'Accept: application/json' localhost:60002/error_counter
```
