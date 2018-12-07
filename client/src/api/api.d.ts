type UserIdentifier = IUserIdentifier;

type IUserIdentifier = string;

type UserInfo = IUserInfo;

interface IUserInfo {
  userInfoUserId: UserIdentifier;
  userInfoUsername: string;
  userInfoUserRole: Role;
  userInfoUserGroup: Group;
}

type PredefinedTopicIdentifier = IPredefinedTopicIdentifier;

type IPredefinedTopicIdentifier = string;

type CustomTopicIdentifier = ICustomTopicIdentifier;

type ICustomTopicIdentifier = string;

type ERDIdentifier = IERDIdentifier;

type IERDIdentifier = string;

type CommentIdentifier = ICommentIdentifier;

type ICommentIdentifier = string;

type FunDepIdentifier = IFunDepIdentifier;

type IFunDepIdentifier = string;

type RelSchemaIdentifier = IRelSchemaIdentifier;

type IRelSchemaIdentifier = string;

type PhysSchemaIdentifier = IPhysSchemaIdentifier;

type IPhysSchemaIdentifier = string;

type Role = "Student" | "Teacher";

type AuthData = IAuthData;

interface IAuthData {
  authLogin: string;
  authPassword: string;
}

type UserSessionData = IUserSessionData;

interface IUserSessionData {
  userSessionUserInfo: UserInfo;
  userSessionKey: string;
}

type PredefinedTopic = IPredefinedTopic;

interface IPredefinedTopic {
  id: PredefinedTopicIdentifier;
  name: string;
}

type AssignedTopicInfo = IAssignedTopicInfoPredefined | IAssignedTopicInfoCustom;

interface IAssignedTopicInfoPredefined {
  tag: "AssignedTopicInfoPredefined";
  contents: PredefinedTopic;
}

interface IAssignedTopicInfoCustom {
  tag: "AssignedTopicInfoCustom";
  contents: CustomTopic;
}

type CustomTopic = ICustomTopic;

interface ICustomTopic {
  id: CustomTopicIdentifier;
  name: string;
  topicAuthor: UserIdentifier;
  accepted: AcceptanceState;
}

type AcceptanceState = "Accepted" | "NotAccepted";

type AssignedTopic = IPredefinedAssignedTopic | ICustomAssignedTopic;

interface IPredefinedAssignedTopic {
  tag: "PredefinedAssignedTopic";
  contents: PredefinedTopicIdentifier;
}

interface ICustomAssignedTopic {
  tag: "CustomAssignedTopic";
  contents: CustomTopicIdentifier;
}

type CommentInfo = ICommentInfo;

interface ICommentInfo {
  id: CommentIdentifier;
  parentItem: ParentItemIdentifier;
  childrenComments: CommentInfo[];
  commentAuthor: UserInfo;
  commentPrio: CommentPriority;
  commentText: string;
  commentStatus: CommentStatus;
}

type CommentBodyInfo = ICommentBodyInfo;

interface ICommentBodyInfo {
  parentItem: ParentItemIdentifier;
  parentComment: ParentComment;
  commentPrio: CommentPriority;
  commentText: string;
}

type ParentItemIdentifier = IParentTopicSelection | IParentERD | IParentFunDep | IParentRelSchema | IParentPhysSchema;

interface IParentTopicSelection {
  tag: "ParentTopicSelection";
  contents: UserIdentifier;
}

interface IParentERD {
  tag: "ParentERD";
  contents: ERDIdentifier;
}

interface IParentFunDep {
  tag: "ParentFunDep";
  contents: FunDepIdentifier;
}

interface IParentRelSchema {
  tag: "ParentRelSchema";
  contents: RelSchemaIdentifier;
}

interface IParentPhysSchema {
  tag: "ParentPhysSchema";
  contents: PhysSchemaIdentifier;
}

type CommentPriority = "CommentStatusNormal" | "CommentStatusImportant" | "CommentStatusCritical";

type CommentStatus = "CommentStateOpen" | "CommentStateClosed";

type ParentComment = INoParentComment | IParentComment;

interface INoParentComment {
  tag: "NoParentComment";
}

interface IParentComment {
  tag: "ParentComment";
  contents: CommentIdentifier;
}

type BasicCrudResponseBodyWithoutAnything<T> = IBasicCrudResponseBodyWithoutAnything<T>;

interface IBasicCrudResponseBodyWithoutAnything<T> {
  id: T;
  description: string;
}

type BasicCrudResponseBodyWithValidation<T> = IBasicCrudResponseBodyWithValidation<T>;

interface IBasicCrudResponseBodyWithValidation<T> {
  id: T;
  description: string;
  validationErrors: string[];
}

type BasicCrudResponseBodyWithAcceptance<T> = IBasicCrudResponseBodyWithAcceptance<T>;

interface IBasicCrudResponseBodyWithAcceptance<T> {
  id: T;
  description: string;
  accepted: AcceptanceState;
}

type BasicCrudResponseBodyWithAcceptanceAndValidation<T> = IBasicCrudResponseBodyWithAcceptanceAndValidation<T>;

interface IBasicCrudResponseBodyWithAcceptanceAndValidation<T> {
  id: T;
  description: string;
  validationErrors: string[];
  accepted: AcceptanceState;
}

type Group = INoGroup | IGroup;

interface INoGroup {
  tag: "NoGroup";
}

interface IGroup {
  tag: "Group";
  contents: string;
}
