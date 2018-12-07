import * as React from 'react'
import { Main } from './views'
import * as ReactDOM from 'react-dom'

async function init() {
  ReactDOM.render(<Main />, document.getElementById('root'))
}

init()
