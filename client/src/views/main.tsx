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
import { Topic } from './pages/topic'
import { Erd } from './pages/erd'
import { FunDeps } from './pages/fundeps'
import { RelSchema } from './pages/relschema'
import { SqlSchema } from './pages/sqlschema'
import { ErrorComponent } from './error-component'

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
        <>
          <Row>
            <Button onClick={toTop}>
              <Glyphicon glyph="home" />
            </Button>
          </Row>
          <Component {...matchProps} />
        </>
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
  return (
    <Grid>
      <ErrorComponent>
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
      </ErrorComponent>
    </Grid>
  )
}
