import * as React from 'react'
import * as api from '../api'
import { Glyphicon, Button, Image } from 'react-bootstrap'
import { Spinner } from './spinner'

interface State {
  erd: Partial<BasicCrudResponseBodyWithAcceptance<string>> | null
  img: string | null
  imgFD: string | null
  initialized: boolean
  lastError:
    | Error & {
        code?: number
        details?: string
      }
    | null
}

export class Erd extends React.Component<{}, State> {
  private timeout?: number

  constructor() {
    super({})
    this.state = {
      erd: null,
      img: null,
      imgFD: null,
      initialized: false,
      lastError: null,
    }
    this.init()
  }

  public render() {
    if (!this.state.initialized) return <Spinner />

    const { erd, img, imgFD } = this.state
    return (
      <>
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
          </div>
        </form>
        {this.state.lastError ? (
          <>
            <Button onClick={this.dismissError}>
              <Glyphicon glyph="cancel" />
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
        {imgFD ? (
          <div style={{ overflowX: 'auto' }}>
            <Image src={imgFD} />
          </div>
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
          imgFD: URL.createObjectURL(await api.postFunDepFromER(description)),
          lastError: null,
        })
      } catch (e) {
        this.setState({ lastError: e, img: null })
      }
    }, 500)
  }

  private handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault()
    if (!this.state.erd || !this.state.erd.description) return
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
  }

  private async init() {
    const erd = await api.getUserItem('erd')
    this.setState({ erd, initialized: true })
    try {
      if (erd)
        this.setState({
          img: URL.createObjectURL(await api.postERDRender(erd.description)),
          imgFD: URL.createObjectURL(
            await api.postFunDepFromER(erd.description),
          ),
          lastError: null,
        })
    } catch (e) {
      this.setState({ lastError: e, img: null, imgFD: null })
    }
  }

  private dismissError() {
    this.setState({ lastError: null })
  }
}
