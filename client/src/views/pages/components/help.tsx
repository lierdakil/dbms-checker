import * as React from 'react'
import { Panel, Glyphicon } from 'react-bootstrap'

export const Help: React.Factory<{ children: JSX.Element[] }> = (props) => {
  const { children } = props!
  return (
    <Panel defaultExpanded={false}>
      <Panel.Heading>
        <Panel.Title toggle>
          <Glyphicon glyph="question-sign" />
          Справка
        </Panel.Title>
      </Panel.Heading>
      <Panel.Collapse>
        <Panel.Body>{...children}</Panel.Body>
      </Panel.Collapse>
    </Panel>
  )
}
