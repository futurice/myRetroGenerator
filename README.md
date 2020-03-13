# myRetro Generator

![Continous Integration](https://github.com/futurice/myRetroGenerator/workflows/Continous%20Integration/badge.svg)

This is a static site generator that parses a plain markdown file and fills out a HTML version that you then can either display with github pages or you can convert it to pdf.

## Set this up for yourself

Refer to the [template repository](https://github.com/futurice/myRetro-template) for instructions

## Run this by yourself

Download the prebuilt binaries [from the releases tab](https://github.com/futurice/myRetroGenerator/releases) (seriously it takes half an hour for a clean build, you don't want to build it yourself). Then unzip it and place a myRetro markdown next to it (see [Example.md](https://github.com/futurice/myRetroGenerator/blob/master/Example.md) for a template).

To run the generator type:

```
./myRetroGenerator myRetro.md
```

You can see all the options with `./myRetroGenerator --help`
