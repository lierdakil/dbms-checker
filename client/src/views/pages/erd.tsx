import * as React from 'react'
import * as api from '../../api'
import { Glyphicon, Button, Image } from 'react-bootstrap'
import { Spinner } from '../spinner'
import { CommentBox } from '../comments'
import { Help } from './components/help'

interface State {
  erd: Partial<BasicCrudResponseBodyWithAcceptance<string>> | null
  img: string | null
  initialized: boolean
  lastError:
    | Error & {
        code?: number
        details?: string
      }
    | null
  progress: boolean
}

export class Erd extends React.Component<{}, State> {
  private timeout?: number

  constructor() {
    super({})
    this.state = {
      erd: null,
      img: null,
      initialized: false,
      lastError: null,
      progress: false,
    }
    this.init()
  }

  public render() {
    if (!this.state.initialized) return <Spinner />

    const { erd, img } = this.state
    return (
      <>
        <Help>
          <p>
            Сущности -- строки, начинающиеся со <code>*</code>. Связи -- строки,
            начинающиеся с <code>=</code> Атрибуты -- каждый атрибут на новой
            строке, после соответствующей сущности или связи.
          </p>

          <p>
            Сущность может быть "слабой". В таком случае, её название
            указывается в скобках
          </p>

          <p>
            При описании связи, указываются связанные сущности следующим
            образом:
          </p>

          <p>
            <pre>=название связи (Сущность1:Сущность2:...) (тип:тип:...)</pre>
            где тип это <code>1</code> или <code>*</code> (вместо <code>*</code>{' '}
            допустимо использовать русскую <code>М</code> или латинскую{' '}
            <code>M</code>). Если количество указанных типов меньше количества
            сущностей, автоматически используется тип <code>*</code>. Например,
            <pre>=связь (Сущность1:Сущность2) (1)</pre>
            будет интерпретирована как связь Сущность1 и Сущность2 типа 1:М
            (один-ко-многим).
          </p>
          <p>
            Идентифицирующие атрибуты обозначаются символом <code>*</code> после
            названия.
          </p>

          <p>
            Если сущность, не являющаяся слабой, не имеет идентифицирующих
            атрибутов, при составлении ФЗ, все ее атрибуты полагаются
            идентифицирующими.
          </p>

          <p>
            Связи типа многие ко многим автоматически вводят атрибут ∅, дабы при
            анализе ФЗ эти связи сохранялись. Он не отображается на
            ER-диаграмме.
          </p>

          <p>Пример:</p>

          <p>
            <pre>
              {`*Сущность
идентифицирующий атрибут*
другой атрибут
еще один атрибут

*(Слабая сущность)
атрибут слабой сущности

*Сущность2
ид*

*Сущность3
ид*

=связь один к одному (Сущность : Сущность2) (1:1)
атрибут связи

=связь один ко многим (Сущность : Сущность3) (1:М)

=связь многие ко многим (Сущность2 : Сущность3) (*:*)

=многосторонняя связь (Слабая сущность : Сущность2 : Сущность3) (1:*:1)

=другая многосторонняя связь (Сущность : Сущность2 : Сущность3) (1:*:*)`}
            </pre>
          </p>
        </Help>
        <form onSubmit={this.handleSubmit}>
          <div>
            <label>
              Описание модели "сущность-связь":
              <div>
                <textarea
                  value={(erd && erd.description) || ''}
                  onChange={this.handleChange}
                  cols={80}
                  rows={20}
                />
              </div>
            </label>
          </div>
          <div>
            <label>
              Статус решения:
              {erd && erd.accepted === 'Accepted' ? (
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
            {this.state.progress ? <Spinner style={{ height: '2em' }} /> : null}
          </div>
        </form>
        {this.state.lastError ? (
          <>
            <Button onClick={this.dismissError}>
              <Glyphicon glyph="remove" />
              Скрыть сообщение об ошибке
            </Button>
            <pre style={{ backgroundColor: '#ffdddd' }}>
              {this.state.lastError.details || this.state.lastError.message}
            </pre>
          </>
        ) : null}
        {img ? (
          <div style={{ overflowX: 'auto' }}>
            <Image src={img} />
          </div>
        ) : null}
        {this.state.erd && this.state.erd.id ? (
          <CommentBox
            parentItem={{
              tag: 'ParentERD',
              contents: this.state.erd.id,
            }}
          />
        ) : null}
      </>
    )
  }

  private handleChange = (evt: React.ChangeEvent<HTMLTextAreaElement>) => {
    const description = evt.currentTarget.value
    this.setState({
      erd: { ...this.state.erd, description },
    })
    clearTimeout(this.timeout)
    this.timeout = window.setTimeout(async () => {
      try {
        this.setState({
          img: URL.createObjectURL(await api.postERDRender(description)),
          lastError: null,
        })
      } catch (e) {
        this.setState({ lastError: e, img: null })
      }
    }, 500)
  }

  private handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault()
    if (!this.state.erd || !this.state.erd.description) {
      throw new Error('Нечего сохранять!')
    }
    try {
      this.setState({ progress: true })
      if (this.state.erd.id) {
        // exists
        const newErd = await api.putERD(
          this.state.erd.id,
          this.state.erd.description,
        )
        this.setState({ erd: newErd })
      } else {
        // create
        const newErd = await api.postERD(this.state.erd.description)
        this.setState({ erd: newErd })
      }
    } finally {
      this.setState({ progress: false })
    }
  }

  private async init() {
    const erd = await api.getUserItem('erd')
    this.setState({ erd, initialized: true })
    try {
      if (erd)
        this.setState({
          img: URL.createObjectURL(await api.postERDRender(erd.description)),
          lastError: null,
        })
    } catch (e) {
      this.setState({ lastError: e, img: null })
    }
  }

  private dismissError = () => {
    this.setState({ lastError: null })
  }
}
