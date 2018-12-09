import * as React from 'react'
import * as api from '../api'
import {
  Button,
  FormGroup,
  ControlLabel,
  HelpBlock,
  FormControl,
  FormControlProps,
} from 'react-bootstrap'

interface FGProps extends FormControlProps {
  id?: string
  label: string
  help?: string
}

function FieldGroup({ id, label, help, ...props }: FGProps) {
  return (
    <FormGroup controlId={id}>
      <ControlLabel>{label}</ControlLabel>
      <FormControl {...props} />
      {help && <HelpBlock>{help}</HelpBlock>}
    </FormGroup>
  )
}

export interface State {
  login: string
  password: string
}

export class Login extends React.Component<{}, State> {
  constructor() {
    super({})
    this.state = { login: '', password: '' }
  }

  public render() {
    return (
      <form className="login-form" onSubmit={this.submit.bind(this)}>
        <FieldGroup
          id="login"
          label="Имя пользователя"
          type="text"
          value={this.state.login}
          onChange={(e: any) => this.setState({ login: e.target.value })}
        />
        <FieldGroup
          id="password"
          label="Пароль"
          type="password"
          value={this.state.password}
          onChange={(e: any) => this.setState({ password: e.target.value })}
        />
        <Button bsStyle="primary" type="submit">
          Войти
        </Button>
      </form>
    )
  }

  public async submit(ev: React.FormEvent<HTMLFormElement>) {
    ev.preventDefault()
    await api.login({
      authLogin: this.state.login,
      authPassword: this.state.password,
    })
    const returnUrl = sessionStorage.getItem('return-url') || '/'
    sessionStorage.removeItem('return-url')
    location.href = returnUrl
  }
}
