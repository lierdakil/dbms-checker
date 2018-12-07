import * as React from 'react'
import * as api from '../api'
import { Glyphicon, Button, Image } from 'react-bootstrap'
import { Spinner } from './spinner'

interface State {
  sqlschema: Partial<
    BasicCrudResponseBodyWithAcceptanceAndValidation<string>
  > | null
  img: string | null
  initialized: boolean
}

export class SqlSchema extends React.Component<{}, State> {
  constructor() {
    super({})
    this.state = {
      sqlschema: null,
      img: null,
      initialized: false,
    }
    this.init()
  }

  public render() {
    if (!this.state.initialized) return <Spinner />

    const { sqlschema, img } = this.state
    return (
      <>
        {img ? <Image src={img} /> : null}
        <form onSubmit={this.handleSubmit}>
          <div>
            <label>
              SQL DDL код создания таблиц:
              <div>
                <textarea
                  value={(sqlschema && sqlschema.description) || ''}
                  onChange={this.handleChange}
                  cols={80}
                  rows={20}
                />
              </div>
            </label>
          </div>
          {sqlschema && sqlschema.validationErrors ? (
            <div>
              <label>Статус решения:</label>
              {sqlschema.validationErrors.length === 0 ? (
                <div>
                  Ошибок не найдено
                  <Glyphicon glyph="ok" color="green" />
                </div>
              ) : (
                <div>
                  {sqlschema.validationErrors.map((err) => (
                    <pre>{err}</pre>
                  ))}
                </div>
              )}
            </div>
          ) : null}
          <div>
            <label>
              Статус решения:
              {sqlschema && sqlschema.accepted === 'Accepted' ? (
                <>
                  Зачтено
                  <Glyphicon glyph="ok" color="green" />
                </>
              ) : (
                <>
                  Проверяется
                  <Glyphicon glyph="remove" color="darkyellow" />
                </>
              )}
            </label>
          </div>
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
      sqlschema: { ...this.state.sqlschema, description },
    })
  }

  private handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault()
    if (!this.state.sqlschema || !this.state.sqlschema.description) return
    if (this.state.sqlschema.id) {
      // exists
      const newSQLSchema = await api.putSQLSchema(
        this.state.sqlschema.id,
        this.state.sqlschema.description,
      )
      this.setState({ sqlschema: newSQLSchema })
    } else {
      // create
      const newSQLSchema = await api.postSQLSchema(
        this.state.sqlschema.description,
      )
      this.setState({ sqlschema: newSQLSchema })
    }
  }

  private async init() {
    const sqlschema = await api.getUserItem('sqlschema')
    this.setState({ sqlschema, initialized: true })
  }
}
