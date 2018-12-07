import * as React from 'react'
import { Image } from 'react-bootstrap'

export const Spinner: React.Factory<{}> = () => (
  <Image src={require('/spinner.svg')} />
)
