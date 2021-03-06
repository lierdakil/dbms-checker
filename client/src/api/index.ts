const baseURL = ''
let userSession: UserSessionData | undefined

export function getUserSessionOrThrow() {
  const session = getUserSession()
  if (session === undefined) throw new Error('No user session')
  return session
}

export function getUserSession() {
  if (userSession) return userSession
  const json = localStorage.getItem('session')
  if (json !== null) userSession = JSON.parse(json)
  return userSession
}

export function clearUserSession() {
  userSession = undefined
  localStorage.removeItem('session')
  location.href = '/login'
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
    const session = getUserSessionOrThrow()
    userId = session.userSessionUserInfo.userInfoUserId
  }
  return request(`/users/${userId}/${item}`, 'GET')
}

export async function putUserTopic(
  topic: AssignedTopic,
  userId?: string,
): Promise<AssignedTopicInfo> {
  if (userId === undefined) {
    const session = getUserSessionOrThrow()
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
  return request(`/render/fundep`, 'POST', desc, 'blob')
}
export async function postFunDepFromER(desc: string): Promise<Blob> {
  return request(`/render/fundepFromER`, 'POST', desc, 'blob')
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
  parentItem?: ParentItemIdentifier,
): Promise<CommentInfo[]> {
  if (parentItem) {
    const parentItemSer = `${parentItem.tag} ${parentItem.contents}`
    return request(
      `/comments?parentItem=${encodeURIComponent(parentItemSer)}`,
      'GET',
    )
  } else return request(`/comments`, 'GET')
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
  return request(`/comments/${id}`, 'PATCH', state)
}

export async function getUsers(group?: string): Promise<UserInfo[]> {
  if (group === undefined) return request(`/users`, 'GET')
  else return request(`/users?group=${encodeURIComponent(group)}`, 'GET')
}

export async function getUser(userId: string): Promise<UserInfo> {
  return request(`/users/${userId}`, 'GET')
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
  if (!response.ok) {
    const error = new Error(response.statusText) as Error & {
      code: number
      details: string
    }
    error.code = response.status
    error.details = await response.text()
    throw error
  }
  if (type == 'json') {
    return response.json()
  } else if (type == 'blob') return response.blob()
}

export interface UserItems {
  topic: AssignedTopicInfo
  erd: BasicCrudResponseBodyWithAcceptance<ERDIdentifier>
  fundep: BasicCrudResponseBodyWithValidation<FunDepIdentifier>
  relschema: BasicCrudResponseBodyWithValidation<RelSchemaIdentifier>
  sqlschema: BasicCrudResponseBodyWithAcceptanceAndValidation<
    PhysSchemaIdentifier
  >
}
