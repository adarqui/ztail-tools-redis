ZTail-Tools
===

Small set of tools which utilize ZTail-Lib, Abstract, and DevUtils. ZTail-Tools will support as many 'queue backends' as are supported by the Abstract library.

Build
--

```
make
```

Running
---

```
cabal run -- ztail-enqueue "redis://localhost:6379" -A -D /var/log/
```
