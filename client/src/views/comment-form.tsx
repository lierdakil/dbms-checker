import * as React from 'react'
import { Button, FormGroup, FormControl } from 'react-bootstrap'
import { Spinner } from './spinner'

interface State {
  text: string
  prio: CommentPriority
  state: 'collapsed' | 'spinner' | 'editor'
}

interface Props {
  toggleable?: boolean
  handleSubmit: (text: string, prio: CommentPriority) => Promise<void>
}

export class CommentForm extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props)
    this.state = {
      text: '',
      prio: 'CommentStatusNormal',
      state: props.toggleable ? 'collapsed' : 'editor',
    }
  }

  public render() {
    if (this.state.state === 'collapsed' && this.props.toggleable)
      return (
        <Button bsStyle="primary" onClick={this.handleReplyClick}>
          Ответить
        </Button>
      )
    else if (this.state.state === 'spinner') return <Spinner />
    else
      return (
        <form onSubmit={this.handleSubmit}>
          <FormGroup>
            <FormControl
              componentClass="textarea"
              value={this.state.text}
              placeholder="Комментарий"
              onChange={this.handleResponseTextChange}
            />
            <label>
              Важность комментария
              <FormControl
                componentClass="select"
                placeholder="Приоритет"
                onChange={this.handlePrioChange}
              >
                <option value="CommentStatusNormal">Обычная</option>
                <option value="CommentStatusImportant">Высокая</option>
                <option value="CommentStatusCritical">Критическая</option>
              </FormControl>
            </label>
            <div />
            <Button type="submit" bsStyle="primary">
              Отправить
            </Button>
            {this.props.toggleable ? (
              <Button type="reset" onClick={this.handleCancel}>
                Отмена
              </Button>
            ) : null}
          </FormGroup>
        </form>
      )
  }

  private handleSubmit = async (e: any) => {
    e.preventDefault()
    this.setState({ state: 'spinner' })
    await this.props.handleSubmit(this.state.state, this.state.prio)
    this.setState({ state: this.props.toggleable ? 'collapsed' : 'editor' })
  }

  private handleReplyClick = () => {
    this.setState({ state: 'editor' })
  }

  private handleCancel = () => {
    if (this.props.toggleable) this.setState({ state: 'collapsed' })
  }

  private handleResponseTextChange = (evt: any) => {
    this.setState({ text: evt.currentTarget.value })
  }

  private handlePrioChange = (evt: any) => {
    this.setState({ prio: evt.currentTarget.value })
  }
}
