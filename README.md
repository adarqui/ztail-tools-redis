ZTail-Tools
===

Small set of tools which utilize ZTail-Lib, Abstract, and DevUtils. ZTail-Tools will support as many 'queue backends' as are supported by the Abstract library.

Major TODO: ZTail-Lib needs to operate on bytestrings


Build
--

```
make
```

Running
---

enqueue:
- This will monitor /var/log recursively, then shuttle logs to the redis endpoints specified

```
./.cabal-sandbox/bin/ztail-enqueue "adarq.org" '["redis://127.0.0.1:6379/key=logs","redis://127.0.0.1:6379/key=broadcast"]' -A -D /var/log/
```
enqueue api:
- Logger (enqueue) now comes with an api.. i'm adding EKG to everything.
- Curl localhost:60001 to query ztail-enqueue

```
# curl -H 'Accept: application/json' localhost:60001/logDistribution
{"max":381,"mean":80.03846153846153,"count":26,"min":0.0,"variance":3946.1908284023666,"type":"d","sum":2081}
```

dumper:
- This will dump everything to /dump/<host>/path/...

```
./.cabal-sandbox/bin/ztail-dump /dump '["redis://localhost:6379/key=logs"]'
```

dumper api:
- Dumper now comes with an api.. i'm adding EKG to everything.
- Curl localhost:60000 to query dumper

```
# curl -H 'Accept: application/json' localhost:60000/logDistribution
{"max":89,"mean":84,"count":6,"min":0.0,"variance":50,"type":"d","sum":504}
```

api calls:

```
curl -H 'Accept: application/json' localhost:60001
curl -H 'Accept: application/json' localhost:60001/logCounter
curl -H 'Accept: application/json' localhost:60001/lengthGauge
curl -H 'Accept: application/json' localhost:60001/logDistribution
curl -H 'Accept: application/json' localhost:60001/dequeueErrorCounter
```

redis monitor:
--

```
1413142609.000790 [0 127.0.0.1:48358] "RPUSH" "logs" "{\"d\":{\"path\":\"/var/log/auth.log\",\"clock\":\"2014-10-12T19:36:49.000Z\",\"tz\":{\"timeZoneName\":\"EDT\",\"timeZoneMinutes\":-240,\"timeZoneSummerOnly\":true},\"buf\":\"Oct 12 15:36:48 polyp sshd[18214]: Connection closed by 127.0.0.1 [preauth]\"},\"h\":\"adarq.org\"}"
1413142609.000944 [0 127.0.0.1:48359] "RPUSH" "broadcast" "{\"d\":{\"path\":\"/var/log/auth.log\",\"clock\":\"2014-10-12T19:36:49.000Z\",\"tz\":{\"timeZoneName\":\"EDT\",\"timeZoneMinutes\":-240,\"timeZoneSummerOnly\":true},\"buf\":\"Oct 12 15:36:48 polyp sshd[18214]: Connection closed by 127.0.0.1 [preauth]\"},\"h\":\"adarq.org\"}"
```
