import * as React from 'react'
import { Login } from './login'
import { BrowserRouter as Router, Route, Switch } from 'react-router-dom'
import { Grid, Row, Col, Button, Glyphicon } from 'react-bootstrap'
import { TaskList } from './task-list'
import { Topic } from './topic'

export interface State {
  leftPanel: boolean
  rightPanel: boolean
}

export class Main extends React.Component<{}, State> {
  constructor() {
    super({})
    this.state = {
      leftPanel: false,
      rightPanel: true,
    }
  }

  public render() {
    if (!sessionStorage.getItem('auth')) {
      sessionStorage.setItem('return-url', location.href)
    }
    return (
      <Grid>
        <Button onClick={this.toTop}>
          <Glyphicon glyph="home" />
        </Button>
        <Router>
          <Switch>
            <Route path="/login">
              {
                <Row>
                  <Col md={12}>
                    <Login />
                  </Col>
                </Row>
              }
            </Route>
            <Route path="/">
              <Switch>
                <Route path="/" exact>
                  <Row>
                    <Col md={12}>
                      <TaskList />
                    </Col>
                  </Row>
                </Route>
                <Route path="/">
                  <Switch>
                    <Route path="/topic">
                      <Row>
                        <Topic />
                      </Row>
                    </Route>
                    {/*
                    <Route path="/erd">
                      <ERD />
                    </Route>
                    <Route path="/fundeps">
                      <FunDeps />
                    </Route>
                    <Route path="/relschema">
                      <RelSchema />
                    </Route>
                    <Route path="/sqlschema">
                      <SqlSchema />
                    </Route>
                    */}
                  </Switch>
                </Route>
              </Switch>
            </Route>
          </Switch>
        </Router>
      </Grid>
    )
  }
  private toTop = () => {
    location.href = '/'
  }
}
