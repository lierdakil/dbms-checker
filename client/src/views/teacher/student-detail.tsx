import * as React from 'react'
import * as api from '../../api'
import { Panel, Image, Breadcrumb, Button, PageHeader } from 'react-bootstrap'
import { Spinner } from '../spinner'
import { RouteComponentProps } from 'react-router'
import { LinkContainer } from 'react-router-bootstrap'
import { CommentBox as NativeCommentBox } from '../comments'
import { TopicSelector } from '../pages/components/topic-selector'

type State = {
  initialized: boolean
  student?: UserInfo
  erdImg?: string
  fdImg?: string
} & { [Key in keyof api.UserItems]?: api.UserItems[Key] | null }

type ItemInfo<T extends keyof api.UserItems> =
  | api.UserItems[T]
  | null
  | undefined

interface Props extends Partial<RouteComponentProps<{ userId?: string }>> {}

const CommentBox = function(props: NativeCommentBox['props']) {
  return (
    <Panel defaultExpanded={false}>
      <Panel.Heading>
        <Panel.Title toggle>Комментарии</Panel.Title>
      </Panel.Heading>
      <Panel.Collapse>
        <Panel.Body>
          <NativeCommentBox {...props} />
        </Panel.Body>
      </Panel.Collapse>
    </Panel>
  )
}

export class StudentDetail extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props)
    this.state = {
      initialized: false,
    }
    this.init()
  }

  public render() {
    if (!this.state.initialized) return <Spinner />
    if (!this.state.student) throw new Error('Нет информации о пользователе')

    const { student } = this.state
    return (
      <div>
        <Breadcrumb>
          <Breadcrumb.Item href="/">Список пользователей</Breadcrumb.Item>
          <Breadcrumb.Item active>
            Пользователь {this.state.student.userInfoUsername}
          </Breadcrumb.Item>
        </Breadcrumb>
        <this.StudentInfo info={student} />
      </div>
    )
  }

  private async init() {
    const student = await api.getUser(this.userId())
    this.setState({ student, initialized: true })
    this.getUserItem('topic', student)
    this.getERD(student)
    this.getFunDeps(student)
    this.getUserItem('relschema', student)
    this.getUserItem('sqlschema', student)
  }

  private async getERD(student: UserInfo) {
    const erd = await this.getUserItem('erd', student)
    try {
      if (erd)
        this.setState({
          erdImg: URL.createObjectURL(await api.postERDRender(erd.description)),
        })
    } catch (e) {}
  }

  private async getFunDeps(student: UserInfo) {
    const fd = await this.getUserItem('fundep', student)
    try {
      if (fd)
        this.setState({
          fdImg: URL.createObjectURL(
            await api.postFunDepRender(fd.description),
          ),
        })
    } catch (e) {}
  }

  private userId() {
    const uid =
      this.props.match &&
      this.props.match.params &&
      this.props.match.params.userId
    if (!uid) {
      throw new Error('Должен быть указан идентификатор пользователя!')
    }
    return uid
  }

  private async getUserItem<T extends keyof api.UserItems>(
    item: T,
    user: UserInfo,
  ): Promise<api.UserItems[T] | null> {
    const topic = await api.getUserItem(item, user.userInfoUserId)
    const st = {} as State
    st[item] = topic
    this.setState(st)
    return topic
  }

  private StudentInfo = (props: React.Attributes & { info: UserInfo }) => {
    const { info } = props!
    const group =
      info.userInfoUserGroup.tag === 'Group' ? (
        <LinkContainer to={`/group/${info.userInfoUserGroup.contents}`}>
          <small>{info.userInfoUserGroup.contents}</small>
        </LinkContainer>
      ) : null
    return (
      <>
        <PageHeader>
          {info.userInfoUsername}
          {group}
        </PageHeader>
        <PageHeader>
          <small>Тема</small>
        </PageHeader>
        <this.Topic user={info} info={this.state.topic} />
        <PageHeader>
          <small>Диаграмма сущность-связь</small>
        </PageHeader>
        <this.ER info={this.state.erd} img={this.state.erdImg} />
        <PageHeader>
          <small>Функциональные зависимости</small>
        </PageHeader>
        <this.FD info={this.state.fundep} img={this.state.fdImg} />
        <PageHeader>
          <small>Реляционная схема</small>
        </PageHeader>
        <this.RS info={this.state.relschema} />
        <PageHeader>
          <small>Физическая схема</small>
        </PageHeader>
        <this.PS info={this.state.sqlschema} />
      </>
    )
  }

  private Topic: React.Factory<{
    user: UserInfo
    info: ItemInfo<'topic'>
  }> = (props) => {
    if (this.state.student === undefined) {
      throw new Error('Student not defined in Topic')
    }
    const { user, info } = props!
    if (info === undefined) return <Spinner />
    if (info === null)
      return (
        <>
          <p>Тема не выбрана</p>
          <TopicSelector
            onSubmit={this.setUserTopic}
            userId={this.state.student.userInfoUserId}
          />
        </>
      )
    return (
      <>
        {info.tag === 'AssignedTopicInfoPredefined' ? (
          <strong>Предопределённая тема: {info.contents.name}</strong>
        ) : (
          <>
            <strong>Тема по выбору: {info.contents.name}</strong>
            {info.contents.accepted === 'Accepted' ? (
              <p>
                Тема принята
                <Button bsStyle="danger" onClick={this.unacceptTopic}>
                  Отменить
                </Button>
              </p>
            ) : (
              <Button bsStyle="success" onClick={this.acceptTopic}>
                Утвердить
              </Button>
            )}
          </>
        )}
        <CommentBox
          parentItem={{
            tag: 'ParentTopicSelection',
            contents: user.userInfoUserId,
          }}
        />
      </>
    )
  }

  private setUserTopic = async (topic: AssignedTopicInfo) => {
    if (this.state.student === undefined) {
      throw new Error('Student not defined in setUserTopic')
    }
    try {
      if (topic.tag === 'AssignedTopicInfoCustom') {
        let newTopic
        if (topic.contents.id !== '') {
          newTopic = await api.putCustomTopic(
            topic.contents.id,
            topic.contents.name,
          )
        } else {
          // new custom topic
          newTopic = await api.postCustomTopic(topic.contents.name)
        }
        return await api.putUserTopic(
          {
            tag: 'CustomAssignedTopic',
            contents: newTopic.contents.id,
          },
          this.state.student.userInfoUserId,
        )
      } else if (topic.tag === 'AssignedTopicInfoPredefined') {
        await api.putUserTopic(
          {
            tag: 'PredefinedAssignedTopic',
            contents: topic.contents.id,
          },
          this.state.student.userInfoUserId,
        )
        return topic
      } else {
        throw new Error(`Unknown topic type: ${JSON.stringify(topic)}`)
      }
    } finally {
      this.getUserItem('topic', this.state.student)
    }
  }

  private acceptTopic = async () => {
    if (!this.state.student) return
    if (!this.state.topic) return
    if (this.state.topic.tag !== 'AssignedTopicInfoCustom') return
    await api.patchCustomTopic(this.state.topic.contents.id, 'Accepted')
    await this.getUserItem('topic', this.state.student)
  }

  private unacceptTopic = async () => {
    if (!this.state.student) return
    if (!this.state.topic) return
    if (this.state.topic.tag !== 'AssignedTopicInfoCustom') return
    await api.patchCustomTopic(this.state.topic.contents.id, 'NotAccepted')
    await this.getUserItem('topic', this.state.student)
  }

  private acceptERD = async () => {
    if (!this.state.student) return
    if (!this.state.erd) return
    await api.patchERD(this.state.erd.id, 'Accepted')
    await this.getERD(this.state.student)
  }

  private unacceptERD = async () => {
    if (!this.state.student) return
    if (!this.state.erd) return
    await api.patchERD(this.state.erd.id, 'NotAccepted')
    await this.getERD(this.state.student)
  }

  private acceptSQL = async () => {
    if (!this.state.student) return
    if (!this.state.sqlschema) return
    await api.patchSQLSchema(this.state.sqlschema.id, 'Accepted')
    await this.getUserItem('sqlschema', this.state.student)
  }

  private unacceptSQL = async () => {
    if (!this.state.student) return
    if (!this.state.sqlschema) return
    await api.patchSQLSchema(this.state.sqlschema.id, 'NotAccepted')
    await this.getUserItem('sqlschema', this.state.student)
  }

  private ER: React.Factory<{
    info: ItemInfo<'erd'>
    img?: string
  }> = (props) => {
    const { info, img } = props!
    if (info === undefined) return <Spinner />
    if (info === null) return <p>Нет решения</p>
    return (
      <>
        <pre>{info.description}</pre>
        {img ? (
          <div style={{ overflowX: 'auto' }}>
            <Image src={img} />
          </div>
        ) : null}
        {info.accepted === 'Accepted' ? (
          <p>
            Решение принято
            <Button bsStyle="danger" onClick={this.unacceptERD}>
              Отменить
            </Button>
          </p>
        ) : (
          <Button bsStyle="success" onClick={this.acceptERD}>
            Принять решение
          </Button>
        )}
        <CommentBox
          parentItem={{
            tag: 'ParentERD',
            contents: info.id,
          }}
        />
      </>
    )
  }

  private FD: React.Factory<{
    info: ItemInfo<'fundep'>
    img?: string
  }> = function(props) {
    const { info, img } = props!
    if (info === undefined) return <Spinner />
    if (info === null) return <p>Нет решения</p>
    return (
      <>
        <pre>{info.description}</pre>
        {img ? (
          <div style={{ overflowX: 'auto' }}>
            <Image src={img} />
          </div>
        ) : null}
        <p>
          <strong>Ошибки валидации:</strong>
          <br />
          {info.validationErrors.length === 0 ? (
            <p>Нет ошибок</p>
          ) : (
            info.validationErrors.map((err) => <pre>{err}</pre>)
          )}
        </p>
        <CommentBox
          parentItem={{
            tag: 'ParentFunDep',
            contents: info.id,
          }}
        />
      </>
    )
  }

  private RS: React.Factory<{
    info: ItemInfo<'relschema'>
    img?: string
  }> = function(props) {
    const { info, img } = props!
    if (info === undefined) return <Spinner />
    if (info === null) return <p>Нет решения</p>
    return (
      <>
        <pre>{info.description}</pre>
        {img ? (
          <div style={{ overflowX: 'auto' }}>
            <Image src={img} />
          </div>
        ) : null}
        <p>
          <strong>Ошибки валидации:</strong>
          <br />
          {info.validationErrors.length === 0 ? (
            <p>Нет ошибок</p>
          ) : (
            info.validationErrors.map((err) => <pre>{err}</pre>)
          )}
        </p>
        <CommentBox
          parentItem={{
            tag: 'ParentRelSchema',
            contents: info.id,
          }}
        />
      </>
    )
  }

  private PS: React.Factory<{
    info: ItemInfo<'sqlschema'>
    img?: string
  }> = (props) => {
    const { info, img } = props!
    if (info === undefined) return <Spinner />
    if (info === null) return <p>Нет решения</p>
    return (
      <>
        <pre>{info.description}</pre>
        {img ? (
          <div style={{ overflowX: 'auto' }}>
            <Image src={img} />
          </div>
        ) : null}
        <p>
          <strong>Ошибки валидации:</strong>
          <br />
          {info.validationErrors.length === 0 ? (
            <p>Нет ошибок</p>
          ) : (
            info.validationErrors.map((err) => <pre>{err}</pre>)
          )}
        </p>
        {info.accepted === 'Accepted' ? (
          <p>
            Решение принято
            <Button bsStyle="danger" onClick={this.unacceptSQL}>
              Отменить
            </Button>
          </p>
        ) : (
          <Button bsStyle="success" onClick={this.acceptSQL}>
            Принять решение
          </Button>
        )}
        <CommentBox
          parentItem={{
            tag: 'ParentPhysSchema',
            contents: info.id,
          }}
        />
      </>
    )
  }
}
