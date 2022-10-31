# Personal Website

This is my [personal website](https://www.justus.pw). It is made in [Hakyll](https://jaspervdj.be/hakyll/).

## Quickstart

Assuming that you have stack installed, simply run

```
stack build
stack exec site clean
stack exec site watch
```

## Requirements

- [Stack](https://docs.haskellstack.org/en/stable/README/)

## Indentation

```
stack install hindent
stack exec hindent *.hs
```

## Linting

```
stack install hlint
stack exec hlint
```

## Formatting

```
npm ci
npm run format
```
