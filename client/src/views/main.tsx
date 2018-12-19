import * as React from 'react'
import * as api from '../api'
import { Login } from './login'
import {
  BrowserRouter as Router,
  Route,
  Switch,
  RouteProps,
  RouteComponentProps,
  Redirect,
} from 'react-router-dom'
import { Grid, Row, Col, Button, Glyphicon } from 'react-bootstrap'
import { TaskList } from './task-list'
import { Topic } from './pages/topic'
import { Erd } from './pages/erd'
import { FunDeps } from './pages/fundeps'
import { RelSchema } from './pages/relschema'
import { SqlSchema } from './pages/sqlschema'
import { ErrorComponent } from './error-component'
import { StudentList } from './teacher/student-list'
import { StudentDetail } from './teacher/student-detail'
import { CommentBox } from './comments'

export interface State {
  leftPanel: boolean
  rightPanel: boolean
}
interface LayoutProps extends RouteProps {
  component: React.ComponentType<Partial<RouteComponentProps>>
}

const DefaultLayout: React.Factory<LayoutProps> = (props) => {
  const { component: Component, ...rest } = props!
  const sess = api.getUserSession()
  return (
    <Route
      {...rest}
      render={(matchProps) => (
        <>
          <Row>
            <Button onClick={toTop}>
              <Glyphicon glyph="home" />
            </Button>
            {sess ? (
              <Button onClick={logOut}>
                <Glyphicon glyph="log-out" />
              </Button>
            ) : null}
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

const LoginLayout: React.Factory<LayoutProps> = (props) => {
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

const StudentInterface: React.Factory<{}> = () => {
  return (
    <>
      <MainLayout path="/" exact component={TaskList} />
      <PageLayout path="/topic" component={Topic} />
      <PageLayout path="/erd" component={Erd} />
      <PageLayout path="/fundeps" component={FunDeps} />
      <PageLayout path="/relschema" component={RelSchema} />
      <PageLayout path="/sqlschema" component={SqlSchema} />
    </>
  )
}

const TeacherInterface: React.Factory<{}> = () => {
  return (
    <>
      <MainLayout path="/" exact component={StudentList} />
      <MainLayout path="/group/:groupId" component={StudentList} />
      <MainLayout path="/user/:userId" component={StudentDetail} />
      <MainLayout path="/all-comments" component={CommentBox} />
    </>
  )
}

const toTop = () => {
  location.href = '/'
}

const logOut = () => {
  api.clearUserSession()
}

export const Main = () => {
  const session = api.getUserSession()
  return (
    <Grid>
      <ErrorComponent>
        <Router>
          <Switch>
            <LoginLayout path="/login" component={Login} />
            {session ? (
              session.userSessionUserInfo.userInfoUserRole === 'Teacher' ? (
                <TeacherInterface />
              ) : (
                <StudentInterface />
              )
            ) : (
              <Redirect to="/login" />
            )}
          </Switch>
        </Router>
      </ErrorComponent>
    </Grid>
  )
}
