import * as React from 'react'
import { Image, ImageProps } from 'react-bootstrap'

export const Spinner: React.Factory<ImageProps> = (props) => (
  <Image src={require('/spinner.svg')} {...props} />
)
