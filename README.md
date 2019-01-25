[![Build Status](https://travis-ci.com/tmikeschu/see-fp.svg?branch=master)](https://travis-ci.com/tmikeschu/see-fp)

# See FP

👀

```
[😀, 😄, 😂, 😍] -> [ 😺, 😸, 😹, 😻 ]
```

👀

Learning functional programming has many steep learning curves. One of them is
learning to see list transformations as declarative instead of imperative.

This application visualizes some of the fundamental higher order functions used
to transform lists.

- [x] `map`
- [x] `filter`
- [x] `reduce`

The app is written in Elm and visualizes the operations using JavaScript syntax.

## Resources

- [A FUNctional JavaScript Makeover](https://medium.com/@tmikeschu/a-functional-javascript-makeover-e1fd017e5413)

## Setup

To get your machine and this code friendly and acquainted:

1. [Install Elm](https://guide.elm-lang.org/install.html).

Clone this repository using your preferred method and `cd` into it. If you're not sure, I
recommend the SSH route:

```
git clone git@github.com:tmikeschu/see-fp.git
npm i -g create-elm-app gh-pages
cd see-fp
elm-app start
```

## Testing

First things first: get a dopamine hit from a verdant test suite! Run the tests with:

```
elm-test
```

Then _read_ the tests to get oriented with the application.

## Local Development

What fun is setting up and running tests if you can't do some of your own stuff?

Local server:

```
elm-app start
```

Create production build:

```
elm-app build
```

Deploy to github pages:

Let Travis handle it!

## Stack

Languages: [Elm](https://elm-lang.org/)

DevOps: [Github Pages](https://pages.github.com/)

Tools:

- [Create Elm App](https://github.com/halfzebra/create-elm-app)
- [Atomic Design](http://bradfrost.com/blog/post/atomic-web-design/)
- [Atomic BEM](https://css-tricks.com/abem-useful-adaptation-bem/)

CI: [Travis](https://travis-ci.com)

## Style

Elm Style!

## Contributing

Interested in helping out?

1. Reach out to me and say hello! I'd love to hear about what you're interested
   in.

2. Once we've confirmed what you can work on, fork this repo and work on your
   masterpiece.

3. Once your work is done, squash your work to a single commit, and open a PR
   from your feature branch to this repo's master branch.
