import * as React from 'react'
import * as api from '../../api'
import { CommentBox } from '../comments'
import { TopicSelector } from './components/topic-selector'

export const Topic: React.Factory<any> = () => {
  return (
    <>
      <TopicSelector onSubmit={handleSubmit} />
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

async function handleSubmit(topic: AssignedTopicInfo) {
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
    return newTopic
  } else if (topic.tag === 'AssignedTopicInfoPredefined') {
    await api.putUserTopic({
      tag: 'PredefinedAssignedTopic',
      contents: topic.contents.id,
    })
    return topic
  } else {
    throw new Error(`Unknown topic type: ${JSON.stringify(topic)}`)
  }
}
