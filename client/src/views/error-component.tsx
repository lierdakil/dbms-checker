import * as React from 'react'
import { Button, Alert } from 'react-bootstrap'

interface State {
  error:
    | Error & {
        code?: number
        details?: string
      }
    | null
}

export class ErrorComponent extends React.Component<{}, State> {
  constructor(props: {}, context?: any) {
    super(props, context)
    window.addEventListener('unhandledrejection', (evt) => {
      this.setState({ error: evt.reason })
    })
    this.state = { error: null }
  }

  public render() {
    return (
      <>
        {this.state.error !== null ? (
          <Alert bsStyle="danger" onDismiss={this.handleDismiss}>
            <h4>
              {this.state.error.code ? (
                <>Произошла ошибка {this.state.error.code}: </>
              ) : null}
              {this.state.error.message}
            </h4>
            {this.state.error.details ? (
              <p>{this.state.error.details}</p>
            ) : null}
            <p>
              <Button onClick={this.handleDismiss}>Скрыть сообщение</Button>
            </p>
          </Alert>
        ) : null}
        {this.props.children}
      </>
    )
  }

  public componentDidCatch(e: Error) {
    this.setState({ error: e })
  }

  private handleDismiss = () => {
    this.setState({ error: null })
  }
}
