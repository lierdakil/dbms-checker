import * as React from 'react'
// import * as api from '../api'
import { Nav, NavItem } from 'react-bootstrap'
import { LinkContainer } from 'react-router-bootstrap'

export class TaskList extends React.Component<{}> {
  constructor() {
    super({})
  }

  // TODO: display user progress, grey out unavailable items
  public render() {
    return (
      <Nav bsStyle="pills" stacked activeKey={1}>
        <LinkContainer to={`/topic`}>
          <NavItem>Выбор темы</NavItem>
        </LinkContainer>
        <LinkContainer to={`/erd`}>
          <NavItem>Инфологическая модель</NavItem>
        </LinkContainer>
        <LinkContainer to={`/fundeps`}>
          <NavItem>Функциональные зависимости</NavItem>
        </LinkContainer>
        <LinkContainer to={`/relschema`}>
          <NavItem>Реляционная схема</NavItem>
        </LinkContainer>
        <LinkContainer to={`/sqlschema`}>
          <NavItem>Физическая схема</NavItem>
        </LinkContainer>
      </Nav>
    )
  }
}
