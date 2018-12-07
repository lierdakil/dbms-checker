const baseURL = 'http://localhost:8081'
let userSession: UserSessionData | undefined

export function getUserSessionOrLogin() {
  if (userSession) return userSession
  const json = localStorage.getItem('session')
  if (json !== null) userSession = JSON.parse(json)
  if (userSession === undefined) {
    location.href = '/login'
    throw new Error('No user session')
  }
  return userSession
}

function getUserSession() {
  if (userSession) return userSession
  const json = localStorage.getItem('session')
  if (json !== null) userSession = JSON.parse(json)
  return userSession
}

export async function login(authData: IAuthData): Promise<void> {
  userSession = await request('/auth', 'POST', authData)
  localStorage.setItem('session', JSON.stringify(userSession))
}

export async function listPredefinedTopics(): Promise<PredefinedTopic[]> {
  return request('/predefined-topics', 'GET')
}

export async function postCustomTopic(
  name: string,
): Promise<AssignedTopicInfo> {
  return request('/custom-topics', 'POST', name)
}

export async function putCustomTopic(
  id: string,
  name: string,
): Promise<AssignedTopicInfo> {
  return request(`/custom-topics/${id}`, 'PUT', name)
}

export async function patchCustomTopic(
  id: string,
  state: AcceptanceState,
): Promise<void> {
  return request(`/custom-topics/${id}`, 'PATCH', state)
}

export async function getUserItem<T extends keyof UserItems>(
  item: T,
  userId?: string,
): Promise<UserItems[T] | null> {
  if (userId === undefined) {
    const session = getUserSessionOrLogin()
    userId = session.userSessionUserInfo.userInfoUserId
  }
  return request(`/users/${userId}/${item}`, 'GET')
}

export async function putUserTopic(
  topic: AssignedTopic,
  userId?: string,
): Promise<AssignedTopicInfo> {
  if (userId === undefined) {
    const session = getUserSessionOrLogin()
    userId = session.userSessionUserInfo.userInfoUserId
  }
  return request(`/users/${userId}/topic`, 'PUT', topic)
}

export async function postERD(body: string): Promise<UserItems['erd']> {
  return request(`/erd`, 'POST', body)
}
export async function postFunDep(body: string): Promise<UserItems['fundep']> {
  return request(`/fundeps`, 'POST', body)
}
export async function postRelSchema(
  body: string,
): Promise<UserItems['relschema']> {
  return request(`/relschemas`, 'POST', body)
}
export async function postSQLSchema(
  body: string,
): Promise<UserItems['sqlschema']> {
  return request(`/sqlschemas`, 'POST', body)
}

export async function putERD(
  id: string,
  body: string,
): Promise<UserItems['erd']> {
  return request(`/erd/${id}`, 'PUT', body)
}
export async function putFunDep(
  id: string,
  body: string,
): Promise<UserItems['fundep']> {
  return request(`/fundeps/${id}`, 'PUT', body)
}
export async function putRelSchema(
  id: string,
  body: string,
): Promise<UserItems['relschema']> {
  return request(`/relschemas/${id}`, 'PUT', body)
}
export async function putSQLSchema(
  id: string,
  body: string,
): Promise<UserItems['sqlschema']> {
  return request(`/sqlschemas/${id}`, 'PUT', body)
}

export async function getERD(id: string): Promise<UserItems['erd']> {
  return request(`/erd/${id}`, 'GET')
}
export async function getFunDep(id: string): Promise<UserItems['fundep']> {
  return request(`/fundeps/${id}`, 'GET')
}
export async function getRelSchema(
  id: string,
): Promise<UserItems['relschema']> {
  return request(`/relschemas/${id}`, 'GET')
}
export async function getSQLSchema(
  id: string,
): Promise<UserItems['sqlschema']> {
  return request(`/sqlschemas/${id}`, 'GET')
}

export async function postERDRender(desc: string): Promise<Blob> {
  return request(`/render/erd`, 'POST', desc, 'blob')
}
export async function postFunDepRender(desc: string): Promise<Blob> {
  return request(`/render/fundeps`, 'POST', desc, 'blob')
}

export async function patchERD(
  id: string,
  state: AcceptanceState,
): Promise<void> {
  return request(`/erd/${id}`, 'PATCH', state)
}
export async function patchSQLSchema(
  id: string,
  state: AcceptanceState,
): Promise<void> {
  return request(`/sqlschemas/${id}`, 'PATCH', state)
}

export async function getComments(
  parentItemType: ParentItemType,
  parentItemId: string,
): Promise<CommentInfo[]> {
  const parentItem = `${parentItemType} "${parentItemId}"`
  return request(
    `/comments?parentItem=${encodeURIComponent(parentItem)}`,
    'GET',
  )
}

export async function getAllComments(): Promise<CommentInfo[]> {
  return request(`/comments`, 'GET')
}

export async function postComment(body: CommentBodyInfo): Promise<CommentInfo> {
  return request(`/comments`, 'POST', body)
}

export async function putComment(
  id: string,
  body: CommentBodyInfo,
): Promise<CommentInfo> {
  return request(`/comments/${id}`, 'PUT', body)
}

export async function patchComment(
  id: string,
  state: CommentStatus,
): Promise<void> {
  return request(`/comments/${id}`, 'PUT', state)
}

async function request(
  url: string,
  method: 'GET' | 'POST' | 'PUT' | 'PATCH',
  data?: any,
  type: 'json' | 'blob' = 'json',
) {
  const headers = new Headers()
  const sess = getUserSession()
  if (sess !== undefined) {
    headers.set('Authorization', `Bearer ${sess.userSessionKey}`)
  }
  if (type === 'json') headers.set('accept', 'application/json;charset=utf-8')
  else if (type === 'blob') headers.set('accept', 'application/octet-stream')
  headers.set('Content-Type', 'application/json;charset=utf-8')
  const response = await fetch(
    new Request(`${baseURL}${url}`, {
      method,
      headers,
      body: JSON.stringify(data),
      cache: 'no-cache',
      redirect: 'follow',
    }),
  )
  if (!response.ok) throw new Error(response.statusText)
  if (type == 'json') {
    return response.json()
  } else if (type == 'blob') return response.blob()
}

interface UserItems {
  topic: AssignedTopicInfo
  erd: BasicCrudResponseBodyWithAcceptance<ERDIdentifier>
  fundep: BasicCrudResponseBodyWithValidation<FunDepIdentifier>
  relschema: BasicCrudResponseBodyWithValidation<RelSchemaIdentifier>
  sqlschema: BasicCrudResponseBodyWithAcceptanceAndValidation<
    PhysSchemaIdentifier
  >
}

type ParentItemType = ParentItemIdentifier['tag']
