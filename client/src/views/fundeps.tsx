import * as React from 'react'
import * as api from '../api'
import { Glyphicon, Button, Image } from 'react-bootstrap'
import { Spinner } from './spinner'
import { CommentBox } from './comment-box'

interface State {
  fundeps: Partial<BasicCrudResponseBodyWithValidation<string>> | null
  img: string | null
  initialized: boolean
  lastError:
    | Error & {
        code?: number
        details?: string
      }
    | null
}

export class FunDeps extends React.Component<{}, State> {
  private timeout?: number

  constructor() {
    super({})
    this.state = {
      fundeps: null,
      img: null,
      initialized: false,
      lastError: null,
    }
    this.init()
  }

  public render() {
    if (!this.state.initialized) return <Spinner />

    const { fundeps, img } = this.state
    return (
      <>
        <form onSubmit={this.handleSubmit}>
          <div>
            <label>
              Функциональные зависимости, присутствующие в модели:
              <div>
                <textarea
                  value={(fundeps && fundeps.description) || ''}
                  onChange={this.handleChange}
                  cols={80}
                  rows={20}
                />
              </div>
            </label>
          </div>
          <div>
            <Button bsStyle="primary" type="submit">
              Сохранить
            </Button>
          </div>
        </form>
        {fundeps && fundeps.validationErrors ? (
          <div>
            <label>Статус решения:</label>
            {fundeps.validationErrors.length === 0 ? (
              <div>
                Ошибок не найдено
                <Glyphicon glyph="ok" color="green" />
              </div>
            ) : (
              <div>
                {fundeps.validationErrors.map((err) => (
                  <pre style={{ backgroundColor: '#ffdddd' }}>{err}</pre>
                ))}
              </div>
            )}
          </div>
        ) : null}
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
        {this.state.fundeps && this.state.fundeps.id ? (
          <CommentBox
            parentItem={{
              tag: 'ParentERD',
              contents: this.state.fundeps.id,
            }}
          />
        ) : null}
      </>
    )
  }

  private handleChange = (evt: React.ChangeEvent<HTMLTextAreaElement>) => {
    const description = evt.currentTarget.value
    this.setState({
      fundeps: { ...this.state.fundeps, description },
    })
    clearTimeout(this.timeout)
    this.timeout = window.setTimeout(async () => {
      try {
        this.setState({
          img: URL.createObjectURL(await api.postFunDepRender(description)),
          lastError: null,
        })
      } catch (e) {
        this.setState({ lastError: e, img: null })
      }
    }, 500)
  }

  private handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault()
    if (!this.state.fundeps || !this.state.fundeps.description) return
    if (this.state.fundeps.id) {
      // exists
      const newFundeps = await api.putFunDep(
        this.state.fundeps.id,
        this.state.fundeps.description,
      )
      this.setState({ fundeps: newFundeps })
    } else {
      // create
      const newFundeps = await api.postFunDep(this.state.fundeps.description)
      this.setState({ fundeps: newFundeps })
    }
  }

  private async init() {
    const fundeps = await api.getUserItem('fundep')
    this.setState({ fundeps, initialized: true })
    try {
      const img = fundeps
        ? URL.createObjectURL(await api.postFunDepRender(fundeps.description))
        : null
      this.setState({ img, lastError: null })
    } catch (e) {
      this.setState({ lastError: e, img: null })
    }
  }

  private dismissError() {
    this.setState({ lastError: null })
  }
}
