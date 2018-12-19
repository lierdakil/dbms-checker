import * as React from 'react'
import * as api from '../../api'
import { Button, Image, Media, Panel, Label, Clearfix } from 'react-bootstrap'
import md5 = require('md5')
import { CommentForm } from './comment-form'

interface State {
  children: CommentInfo[]
  status: CommentStatus
}

interface Props {
  comment: CommentInfo
  level: number
}

const prioMap: { [key in CommentPriority]: string } = {
  CommentStatusNormal: 'info',
  CommentStatusImportant: 'warning',
  CommentStatusCritical: 'danger',
}

export class CommentComp extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props)
    this.state = {
      children: props.comment.childrenComments,
      status: props.comment.commentStatus,
    }
  }

  public render() {
    const session = api.getUserSessionOrThrow()
    return (
      <Media>
        {this.props.level > 0 && this.props.level < 5 ? (
          <Media.Left>
            <span style={{ width: '2em', display: 'inline-block' }} />
          </Media.Left>
        ) : null}
        <Media.Body>
          <Panel
            bsStyle={
              this.state.status === 'CommentStateClosed'
                ? undefined
                : prioMap[this.props.comment.commentPrio]
            }
          >
            <Panel.Heading>
              <Clearfix>
                <span>{this.props.comment.commentAuthor.userInfoUsername}</span>
                &emsp;
                {this.props.comment.commentAuthor.userInfoUserGroup.tag ===
                'Group' ? (
                  <Label>
                    {
                      this.props.comment.commentAuthor.userInfoUserGroup
                        .contents
                    }
                  </Label>
                ) : null}
                {this.props.comment.commentAuthor.userInfoUserRole ===
                'Teacher' ? (
                  <Label>Преподаватель</Label>
                ) : null}
                {this.state.status === 'CommentStateOpen' &&
                this.props.comment.commentAuthor.userInfoUserId ===
                  session.userSessionUserInfo.userInfoUserId ? (
                  <Button style={{ float: 'right' }} onClick={this.markClosed}>
                    Отметить как решённый
                  </Button>
                ) : null}
                <br />
                {new Date(this.props.comment.commentTime).toLocaleString()}
              </Clearfix>
            </Panel.Heading>
            <Panel.Body>
              <Media>
                <Media.Left>
                  <Image
                    width={64}
                    height={64}
                    src={`https://www.gravatar.com/avatar/${md5(
                      this.props.comment.commentAuthor.userInfoEmail.trim(),
                    )}?s=64&d=retro&rating=g`}
                    alt={this.props.comment.commentAuthor.userInfoUsername}
                  />
                </Media.Left>
                <Media.Body>
                  <p>{this.props.comment.commentText}</p>
                </Media.Body>
              </Media>
            </Panel.Body>
            <Panel.Footer>
              <CommentForm toggleable handleSubmit={this.handleSubmit} />
            </Panel.Footer>
          </Panel>
          <Media.List>
            {this.state.children.map((c) => (
              <Media.ListItem>
                <CommentComp comment={c} level={this.props.level + 1} />
              </Media.ListItem>
            ))}
          </Media.List>
        </Media.Body>
      </Media>
    )
  }

  private markClosed = async () => {
    await api.patchComment(this.props.comment.id, 'CommentStateClosed')
    this.setState({ status: 'CommentStateClosed' })
  }

  private handleSubmit = async (text: string, prio: CommentPriority) => {
    if (!text) return
    const newComment = await api.postComment({
      commentPrio: prio,
      parentComment: { tag: 'ParentComment', contents: this.props.comment.id },
      parentItem: this.props.comment.parentItem,
      commentText: text,
    })
    this.setState({ children: [...this.state.children, newComment] })
  }
}
