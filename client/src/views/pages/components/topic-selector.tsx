import * as React from 'react'
import * as api from '../../../api'
import {
  DropdownButton,
  MenuItem,
  Glyphicon,
  OverlayTrigger,
  Tooltip,
  Button,
} from 'react-bootstrap'
import { Spinner } from '../../spinner'

interface State {
  predefinedTopics: Map<string, string>
  userTopic: AssignedTopicInfo | null
  userTopicSaved: AssignedTopicInfo | null
  initialized: boolean
  progress: boolean
}

interface Props {
  onSubmit: (topic: AssignedTopicInfo) => Promise<AssignedTopicInfo>
  userId?: string
}

export class TopicSelector extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props)
    this.state = {
      predefinedTopics: new Map(),
      userTopic: null,
      userTopicSaved: null,
      initialized: false,
      progress: false,
    }
    this.init()
  }

  public render() {
    if (!this.state.initialized) return <Spinner />

    const topic = this.state.userTopic
    const topicSelectorTitle = !topic
      ? 'Выберите тему'
      : topic.tag === 'AssignedTopicInfoCustom'
      ? 'Собственная тема'
      : topic.contents.name
    return (
      <>
        <form onSubmit={this.handleSubmit}>
          <DropdownButton title={topicSelectorTitle} id="topic-dropdown">
            {Array.from(this.state.predefinedTopics.entries()).map(([k, v]) => (
              <MenuItem
                eventKey={k}
                active={
                  (topic || false) &&
                  topic.tag === 'AssignedTopicInfoPredefined' &&
                  topic.contents.id === k
                }
                onSelect={this.topicChanged}
              >
                {v}
              </MenuItem>
            ))}
            <MenuItem divider />
            <MenuItem
              eventKey="custom-topic"
              active={
                (topic || false) && topic.tag === 'AssignedTopicInfoCustom'
              }
              onSelect={this.topicChanged}
            >
              Собственная тема
            </MenuItem>
          </DropdownButton>
          {topic && topic.tag === 'AssignedTopicInfoCustom' ? (
            <div>
              <label>
                Название темы:
                <input
                  type="text"
                  value={topic.contents.name}
                  onChange={this.handleCustomTopicChange}
                />
              </label>
              {this.state.userTopicSaved &&
              this.state.userTopicSaved.tag === 'AssignedTopicInfoCustom' ? (
                topic.contents.accepted === 'Accepted' ? (
                  <OverlayTrigger
                    placement="top"
                    overlay={<Tooltip>Тема принята</Tooltip>}
                  >
                    <Glyphicon glyph="ok" color="green" />
                  </OverlayTrigger>
                ) : (
                  <OverlayTrigger
                    placement="top"
                    overlay={<Tooltip>Тема пока не принята</Tooltip>}
                  >
                    <Glyphicon glyph="remove" color="darkyellow" />
                  </OverlayTrigger>
                )
              ) : null}
            </div>
          ) : null}
          <div />
          <Button bsStyle="primary" type="submit">
            Сохранить
          </Button>
          {this.state.progress ? <Spinner style={{ height: '2em' }} /> : null}
        </form>
      </>
    )
  }

  private handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault()
    if (!this.state.userTopic) throw new Error('Тема не задана')
    try {
      this.setState({ progress: true })
      const newTopic = await this.props.onSubmit(this.state.userTopic)
      this.setState({ userTopic: newTopic, userTopicSaved: newTopic })
    } finally {
      this.setState({ progress: false })
    }
  }

  private handleCustomTopicChange = (
    evt: React.ChangeEvent<HTMLInputElement>,
  ) => {
    this.setState({
      userTopic: {
        tag: 'AssignedTopicInfoCustom',
        contents: {
          name: evt.currentTarget.value,
          accepted: 'NotAccepted',
          id:
            this.state.userTopicSaved &&
            this.state.userTopicSaved.tag === 'AssignedTopicInfoCustom'
              ? this.state.userTopicSaved.contents.id
              : '',
          topicAuthor: '',
        },
      },
    })
  }

  private topicChanged = (eventKey: any) => {
    if (eventKey === 'custom-topic') {
      if (
        this.state.userTopicSaved &&
        this.state.userTopicSaved.tag === 'AssignedTopicInfoCustom'
      ) {
        this.setState({ userTopic: this.state.userTopicSaved })
      } else {
        this.setState({
          userTopic: {
            tag: 'AssignedTopicInfoCustom',
            contents: {
              name: '',
              accepted: 'NotAccepted',
              id: '',
              topicAuthor: '',
            },
          },
        })
      }
    } else {
      this.setState({
        userTopic: {
          tag: 'AssignedTopicInfoPredefined',
          contents: {
            id: eventKey,
            name: this.state.predefinedTopics.get(eventKey)!,
          },
        },
      })
    }
  }

  private async init() {
    const topic = await api.getUserItem('topic', this.props.userId)
    this.setState({
      predefinedTopics: new Map(toMap(await api.listPredefinedTopics())),
      userTopic: topic,
      userTopicSaved: topic,
      initialized: true,
    })
  }
}

function* toMap(list: PredefinedTopic[]): IterableIterator<[string, string]> {
  for (const i of list) {
    yield [i.id, i.name]
  }
}
