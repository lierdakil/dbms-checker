import * as React from 'react'
import * as api from '../../api'
import { Glyphicon, Button } from 'react-bootstrap'
import { Spinner } from '../spinner'
import { CommentBox } from '../comments'
import { Help } from './components/help'

interface State {
  relschema: Partial<BasicCrudResponseBodyWithValidation<string>> | null
  initialized: boolean
  progress: boolean
}

export class RelSchema extends React.Component<{}, State> {
  constructor() {
    super({})
    this.state = {
      relschema: null,
      initialized: false,
      progress: false,
    }
    this.init()
  }

  public render() {
    if (!this.state.initialized) return <Spinner />

    const { relschema } = this.state
    return (
      <>
        <Help>
          <p>
            Отношения представляют из себя списки атрибутов и доменов,
            разделенных запятыми и заключёнными в скобки. Атрибуты состоят из
            названия сущности и названия атрибута, разделённых символом{' '}
            <code>.</code>. Название сущности и название атрибута --
            последовательности букв, цифр, пробелов и символов <code>_</code>,{' '}
            <code>№</code>.
          </p>
          <p>
            Название атрибута отделяется от домена атрибута символом{' '}
            <code>:</code>. Допускаются следующие домены:
            <ul>
              <li>
                <code>строка</code>
              </li>
              <li>
                <code>текст</code>
              </li>
              <li>
                <code>дата</code>
              </li>
              <li>
                <code>время</code>
              </li>
              <li>
                <code>дата/время</code>
              </li>
              <li>
                <code>целое</code>
              </li>
              <li>
                <code>натуральное</code>
              </li>
              <li>
                <code>дробное</code>
              </li>
              <li>
                <code>перечисление</code>
              </li>
            </ul>
          </p>

          <p>
            В случае домена "перечисление", в скобках после него указывается
            список допустимых значений, разделённых символом <code>,</code>.
            Значениями являются произвольные строки символов кроме{' '}
            <code>,</code>, <code>(</code>, <code>)</code>
          </p>

          <p>
            Пробелы и переносы строк игнорируются за исключением пробелов в
            середине названий атрибутов (там они интерпретиуются как часть
            названия атрибута)
          </p>

          <p>
            Пример:
            <pre>
              {`(*e1.id : натуральное, e1.attr : текст, e2.id:натуральное)
(*e2.id : натуральное, e2.attr : текст)
(*e3.id : натуральное, e3.attr : перечисление (арбуз, банан, персик))`}
            </pre>
          </p>
        </Help>
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
            {this.state.progress ? <Spinner style={{ height: '2em' }} /> : null}
          </div>
        </form>
        {this.state.relschema && this.state.relschema.id ? (
          <CommentBox
            parentItem={{
              tag: 'ParentRelSchema',
              contents: this.state.relschema.id,
            }}
          />
        ) : null}
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
    if (!this.state.relschema || !this.state.relschema.description) {
      throw new Error('Нечего сохранять!')
    }
    try {
      this.setState({ progress: true })
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
    } finally {
      this.setState({ progress: false })
    }
  }

  private async init() {
    const relschema = await api.getUserItem('relschema')
    this.setState({ relschema, initialized: true })
  }
}
