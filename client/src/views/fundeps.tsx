import * as React from 'react'
import * as api from '../api'
import { Glyphicon, Button, Image } from 'react-bootstrap'
import { Spinner } from './spinner'

interface State {
  fundeps: Partial<BasicCrudResponseBodyWithValidation<string>> | null
  img: string | null
  initialized: boolean
}

export class FunDeps extends React.Component<{}, State> {
  private timeout?: number

  constructor() {
    super({})
    this.state = {
      fundeps: null,
      img: null,
      initialized: false,
    }
    this.init()
  }

  public render() {
    if (!this.state.initialized) return <Spinner />

    const { fundeps, img } = this.state
    return (
      <>
        {img ? <Image src={img} /> : null}
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
      fundeps: { ...this.state.fundeps, description },
    })
    clearTimeout(this.timeout)
    this.timeout = window.setTimeout(async () => {
      this.setState({
        img: URL.createObjectURL(await api.postFunDepRender(description)),
      })
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
      this.setState({ img })
    } catch (e) {
      console.error(e)
    }
  }
}
