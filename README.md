# spool

A podcast client in pure Common Lisp — subscriptions, feed parsing, an episode cache, and
playback. No FFI: feeds are fetched over [weft](https://github.com/modus-lisp/weft)'s pure-CL
TLS and parsed by a feed-shaped XML reader; audio is decoded incrementally by
[reed](https://github.com/modus-lisp/reed).

Playback hands out a **source thunk** rather than owning an output device, so the same episode
object plays into a glass session mixer, an RTP sender, or a file — whoever pulls from it decides
where the audio goes.

```
spool          subscriptions, feeds, cache, playback
spool/glass    that audio into a glass session mixer, so every viewer of a session hears it
```

## Status

Early. The systems load and the pieces above exist; treat the API as unsettled.

## License

MIT — see [LICENSE](LICENSE).
