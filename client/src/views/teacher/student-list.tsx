import * as React from 'react'
import * as api from '../../api'
import { Glyphicon, Panel, Label, Breadcrumb, Button } from 'react-bootstrap'
import { Spinner } from '../spinner'
import { RouteComponentProps } from 'react-router'
import { LinkContainer } from 'react-router-bootstrap'

type UserInfoExt = UserInfo &
  { [Key in keyof api.UserItems]?: api.UserItems[Key] | null }

interface State {
  initialized: boolean
  students: UserInfoExt[]
}

interface Props extends Partial<RouteComponentProps<{ groupId?: string }>> {}

type ItemInfo = {
  [Stage in keyof api.UserItems]: {
    stage: Stage
    info: api.UserItems[Stage] | undefined | null
  }
}

const StageInfo = function(props: ItemInfo[keyof api.UserItems]) {
  let accepted: boolean | undefined | null
  let verified: boolean | undefined | null
  switch (props.stage) {
    case 'topic':
      verified = props.info !== null
      accepted =
        props.info == null
          ? null
          : props.info.tag === 'AssignedTopicInfoPredefined' ||
            (props.info.tag === 'AssignedTopicInfoCustom' &&
              props.info.contents.accepted === 'Accepted')
      break
    case 'erd':
      verified = props.info !== null
      accepted = props.info == null ? null : props.info.accepted === 'Accepted'
      break
    case 'fundep':
      verified =
        props.info == null ? null : props.info.validationErrors.length === 0
      break
    case 'relschema':
      verified =
        props.info == null ? null : props.info.validationErrors.length === 0
      break
    case 'sqlschema':
      accepted = props.info == null ? null : props.info.accepted === 'Accepted'
      verified =
        props.info == null ? null : props.info.validationErrors.length === 0
      break
  }
  return (
    <tr>
      <th scope="row">{props.stage}</th>
      <td>
        {verified === true ? (
          <Glyphicon glyph="ok" />
        ) : verified === undefined ? null : verified === false ? (
          <Glyphicon glyph="remove" />
        ) : verified === null ? (
          <Glyphicon glyph="minus" />
        ) : (
          '?'
        )}
      </td>
      <td>
        {accepted === true ? (
          <Glyphicon glyph="ok" />
        ) : accepted === undefined ? null : accepted === false ? (
          <Glyphicon glyph="remove" />
        ) : accepted === null ? (
          <Glyphicon glyph="minus" />
        ) : (
          '?'
        )}
      </td>
    </tr>
  )
}

const StudentInfo: React.Factory<{ info: UserInfoExt }> = function(props) {
  const { info } = props!
  return (
    <>
      <Panel defaultExpanded={false}>
        <Panel.Heading>
          <Panel.Title toggle>
            {info.userInfoUsername}
            {info.userInfoUserGroup.tag === 'Group' ? (
              <LinkContainer to={`/group/${info.userInfoUserGroup.contents}`}>
                <Label>{info.userInfoUserGroup.contents}</Label>
              </LinkContainer>
            ) : null}
            {info.userInfoUserRole === 'Teacher' ? (
              <Label>Преподаватель</Label>
            ) : null}
          </Panel.Title>
        </Panel.Heading>
        <Panel.Collapse>
          <Panel.Body>
            <table className="table table-striped table-hover table-sm">
              <thead>
                <tr>
                  <th scope="col" />
                  <th scope="col">Проверено</th>
                  <th scope="col">Принято</th>
                </tr>
              </thead>
              <tbody>
                <StageInfo stage="topic" info={info.topic} />
                <StageInfo stage="erd" info={info.erd} />
                <StageInfo stage="fundep" info={info.fundep} />
                <StageInfo stage="relschema" info={info.relschema} />
                <StageInfo stage="sqlschema" info={info.sqlschema} />
              </tbody>
            </table>
            <LinkContainer to={`/user/${info.userInfoUserId}`}>
              <Button bsStyle="primary">Подробности</Button>
            </LinkContainer>
          </Panel.Body>
        </Panel.Collapse>
      </Panel>
    </>
  )
}

export class StudentList extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props)
    this.state = {
      initialized: false,
      students: [],
    }
    this.init()
  }

  public render() {
    if (!this.state.initialized) return <Spinner />

    const { students } = this.state
    return (
      <div>
        <Breadcrumb>
          {this.groupId() !== undefined ? (
            <>
              <Breadcrumb.Item href="/">Список пользователей</Breadcrumb.Item>
              <Breadcrumb.Item active>Группа {this.groupId()}</Breadcrumb.Item>
            </>
          ) : (
            <Breadcrumb.Item active>Список пользователей</Breadcrumb.Item>
          )}
        </Breadcrumb>
        {students.map((stInfo) =>
          stInfo.userInfoUserRole === 'Student' ? (
            <StudentInfo info={stInfo} />
          ) : null,
        )}
      </div>
    )
  }

  private async init() {
    const students = await api.getUsers(this.groupId())
    this.setState({ students, initialized: true })
    for (const student of this.state.students) {
      this.getUserItem('topic', student)
      this.getUserItem('erd', student)
      this.getUserItem('fundep', student)
      this.getUserItem('relschema', student)
      this.getUserItem('sqlschema', student)
    }
  }

  private groupId() {
    return (
      this.props.match &&
      this.props.match.params &&
      this.props.match.params.groupId
    )
  }

  private getUserItem(item: keyof api.UserItems, user: UserInfoExt) {
    api.getUserItem(item, user.userInfoUserId).then((topic) => {
      user[item] = topic
      this.setState({})
    })
  }
}
