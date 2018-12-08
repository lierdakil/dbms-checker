import * as React from 'react'
import * as api from '../api'
import {
  DropdownButton,
  MenuItem,
  Glyphicon,
  OverlayTrigger,
  Tooltip,
  Button,
} from 'react-bootstrap'
import { Spinner } from './spinner'
import { CommentBox } from './comment-box'

interface State {
  predefinedTopics: Map<string, string>
  userTopic: AssignedTopicInfo | null
  userTopicSaved: AssignedTopicInfo | null
  initialized: boolean
}

export class Topic extends React.Component<{}, State> {
  constructor() {
    super({})
    this.state = {
      predefinedTopics: new Map(),
      userTopic: null,
      userTopicSaved: null,
      initialized: false,
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
          ) : (
            <div />
          )}
          <Button bsStyle="primary" type="submit">
            Сохранить
          </Button>
        </form>
        <CommentBox
          parentItem={{
            tag: 'ParentTopicSelection',
            contents: api.getUserSessionOrLogin().userSessionUserInfo
              .userInfoUserId,
          }}
        />
      </>
    )
  }

  private handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault()
    if (!this.state.userTopic) return
    const topic = this.state.userTopic
    if (topic.tag === 'AssignedTopicInfoCustom') {
      let newTopic
      let savedTopic = this.state.userTopicSaved
      if (
        savedTopic &&
        savedTopic.tag === topic.tag &&
        savedTopic.contents.id !== ''
      ) {
        newTopic = await api.putCustomTopic(
          savedTopic.contents.id,
          topic.contents.name,
        )
      } else {
        // new custom topic
        newTopic = await api.postCustomTopic(topic.contents.name)
      }
      this.setState({ userTopic: newTopic, userTopicSaved: newTopic })
    } else if (topic.tag === 'AssignedTopicInfoPredefined') {
      await api.putUserTopic({
        tag: 'PredefinedAssignedTopic',
        contents: topic.contents.id,
      })
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
          id: '',
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
    const topic = await api.getUserItem('topic')
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
