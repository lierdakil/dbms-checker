import * as React from 'react'
import * as api from '../api'
import { Glyphicon, Button } from 'react-bootstrap'
import { Spinner } from './spinner'

interface State {
  relschema: Partial<BasicCrudResponseBodyWithValidation<string>> | null
  initialized: boolean
}

export class RelSchema extends React.Component<{}, State> {
  constructor() {
    super({})
    this.state = {
      relschema: null,
      initialized: false,
    }
    this.init()
  }

  public render() {
    if (!this.state.initialized) return <Spinner />

    const { relschema } = this.state
    return (
      <>
        <form onSubmit={this.handleSubmit}>
          <div>
            <label>
              Реляционная модель базы данных:
              <div>
                <textarea
                  value={(relschema && relschema.description) || ''}
                  onChange={this.handleChange}
                  cols={80}
                  rows={20}
                />
              </div>
            </label>
          </div>
          {relschema && relschema.validationErrors ? (
            <div>
              <label>Статус решения:</label>
              {relschema.validationErrors.length === 0 ? (
                <div>
                  Ошибок не найдено
                  <Glyphicon glyph="ok" color="green" />
                </div>
              ) : (
                <div>
                  {relschema.validationErrors.map((err) => (
                    <pre>{err}</pre>
                  ))}
                </div>
              )}
            </div>
          ) : null}
          <div>
            <Button bsStyle="primary" type="submit">
              Сохранить
            </Button>
          </div>
        </form>
      </>
    )
  }

  private handleChange = (evt: React.ChangeEvent<HTMLTextAreaElement>) => {
    const description = evt.currentTarget.value
    this.setState({
      relschema: { ...this.state.relschema, description },
    })
  }

  private handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault()
    if (!this.state.relschema || !this.state.relschema.description) return
    if (this.state.relschema.id) {
      // exists
      const newRelSchema = await api.putRelSchema(
        this.state.relschema.id,
        this.state.relschema.description,
      )
      this.setState({ relschema: newRelSchema })
    } else {
      // create
      const newRelSchema = await api.postRelSchema(
        this.state.relschema.description,
      )
      this.setState({ relschema: newRelSchema })
    }
  }

  private async init() {
    const relschema = await api.getUserItem('relschema')
    this.setState({ relschema, initialized: true })
  }
}
