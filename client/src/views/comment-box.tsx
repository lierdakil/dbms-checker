import * as React from 'react'
import * as api from '../api'
import { Media } from 'react-bootstrap'
import { Spinner } from './spinner'
import { CommentComp } from './comment'
import { CommentForm } from './comment-form'

interface State {
  comments: CommentInfo[]
  initialized: boolean
}

interface Props {
  parentItem: ParentItemIdentifier
}

export class CommentBox extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props)
    this.state = {
      comments: [],
      initialized: false,
    }
    this.init()
  }

  public render() {
    if (!this.state.initialized) return <Spinner />
    return (
      <Media.List>
        {this.state.comments.map((c) => (
          <Media.ListItem>
            <CommentComp comment={c} level={0} />
          </Media.ListItem>
        ))}
        <Media.ListItem>
          <CommentForm handleSubmit={this.handleSubmit} />
        </Media.ListItem>
      </Media.List>
    )
  }

  private handleSubmit = async (text: string, prio: CommentPriority) => {
    if (!text) return
    const newComment = await api.postComment({
      commentPrio: prio,
      parentComment: { tag: 'NoParentComment' },
      parentItem: this.props.parentItem,
      commentText: text,
    })
    this.setState({ comments: [...this.state.comments, newComment] })
  }

  private async init() {
    this.setState({
      comments: await api.getComments(this.props.parentItem),
      initialized: true,
    })
  }
}
