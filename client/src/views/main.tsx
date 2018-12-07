import * as React from 'react'
import { Login } from './login'
import {
  BrowserRouter as Router,
  Route,
  Switch,
  RouteProps,
  RouteComponentProps,
} from 'react-router-dom'
import { Grid, Row, Col, Button, Glyphicon } from 'react-bootstrap'
import { TaskList } from './task-list'
import { Topic } from './topic'
import { Erd } from './erd'
import { FunDeps } from './fundeps'
import { RelSchema } from './relschema'
import { SqlSchema } from './sqlschema'

export interface State {
  leftPanel: boolean
  rightPanel: boolean
}
interface LayoutProps extends RouteProps {
  component: React.ComponentType<Partial<RouteComponentProps>>
}

const DefaultLayout: React.Factory<LayoutProps> = (props) => {
  const { component: Component, ...rest } = props!
  return (
    <Route
      {...rest}
      render={(matchProps) => (
        <Grid>
          <Row>
            <Button onClick={toTop}>
              <Glyphicon glyph="home" />
            </Button>
          </Row>
          <Component {...matchProps} />
        </Grid>
      )}
    />
  )
}

const MainLayout: React.Factory<LayoutProps> = (props) => {
  const { component: Component, ...rest } = props!
  return (
    <DefaultLayout
      {...rest}
      component={(matchProps) => (
        <Row>
          <Col md={12}>
            <Component {...matchProps} />
          </Col>
        </Row>
      )}
    />
  )
}

const PageLayout: React.Factory<LayoutProps> = (props) => {
  const { component: Component, ...rest } = props!
  return (
    <DefaultLayout
      {...rest}
      component={(matchProps) => (
        <Row>
          <Col md={10} sm={12}>
            <Component {...matchProps} />
          </Col>
          <Col md={2} smHidden={true}>
            <TaskList />
          </Col>
        </Row>
      )}
    />
  )
}

const toTop = () => {
  location.href = '/'
}

export const Main = () => {
  if (!sessionStorage.getItem('auth')) {
    sessionStorage.setItem('return-url', location.href)
  }
  return (
    <Router>
      <Switch>
        <MainLayout path="/" exact component={TaskList} />
        <MainLayout path="/login" component={Login} />
        <PageLayout path="/topic" component={Topic} />
        <PageLayout path="/erd" component={Erd} />
        <PageLayout path="/fundeps" component={FunDeps} />
        <PageLayout path="/relschema" component={RelSchema} />
        <PageLayout path="/sqlschema" component={SqlSchema} />
      </Switch>
    </Router>
  )
}
