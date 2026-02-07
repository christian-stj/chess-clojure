# ClojureScript Chess Game

Using Reagent + shadow-cljs

A minimal ClojureScript web project using shadow-cljs for compilation and live reload, with Reagent for reactive UI components.

## Features
- shadow-cljs for fast builds and live reload
- Reagent for reactive React components with Hiccup-style syntax
- Hot code reload during development
- Built-in development server

## Getting Started

### Prerequisites
- [Node.js](https://nodejs.org/) installed
- [Java JDK](https://adoptium.net/) installed

### Setup
1. Install dependencies:
   ```
   npm install
   ```

2. Start the development server with live reload:
   ```
   npm run dev
   ```

3. Open http://localhost:8080 in your browser.

## Project Structure
- `src/app/core.cljs` - Main application code
- `resources/public/` - Static assets and HTML
- `shadow-cljs.edn` - Build configuration
- `package.json` - npm dependencies

## Editing
- Modify `src/app/core.cljs` for your app logic and Reagent components.
- Changes will reload automatically in the browser.
- Check the browser console for compilation feedback.

## Building for Production
```
npm run build
```

## License
MIT
