(function () {
  const QUESTION_BANK = [
    {
      id: "q1",
      tag: "群聊场景",
      prompt: "朋友在群里发了 58 秒语音。你内心第一反应更接近哪句？",
      options: [
        { label: "转文字吧，大家都是成年人。", value: 2 },
        { label: "先点开听两秒，再决定装没看见。", value: 1 },
        { label: "58 秒？这人今天情绪一定很多。", value: 3 },
      ],
    },
    {
      id: "q2",
      tag: "收藏夹场景",
      prompt: "你的收藏夹里有一篇《真正改变人生的八个方法》。它最可能的状态是？",
      options: [
        { label: "收藏即实践，先放着也算一种开始。", value: 1 },
        { label: "点开过，看到第三条开始心虚。", value: 2 },
        { label: "我会认真看完，然后再收藏同类十篇。", value: 3 },
      ],
    },
    {
      id: "q3",
      tag: "链接场景",
      prompt: "有人给你发“求你一定看完这个链接”，你的手会先做什么？",
      options: [
        { label: "先看链接标题值不值得我这一生。", value: 2 },
        { label: "先回个“收到”，再拖进未来。", value: 1 },
        { label: "点开，来都来了。", value: 3 },
      ],
    },
    {
      id: "q4",
      tag: "体面场景",
      prompt: "你在网上发了一条消息，五分钟没人回。你比较像？",
      options: [
        { label: "继续假装平静，仿佛我从未在乎。", value: 2 },
        { label: "想撤回，但又怕显得更在乎。", value: 1 },
        { label: "开始怀疑世界、关系与信号塔。", value: 3 },
      ],
    },
    {
      id: "q5",
      tag: "输入法场景",
      prompt: "当你打下一大段字又全部删掉时，你通常删掉的是？",
      options: [
        { label: "真心话。", value: 3 },
        { label: "废话。", value: 2 },
        { label: "我原本想当场赢下这场对话的证据。", value: 1 },
      ],
    },
    {
      id: "q6",
      tag: "社交场景",
      prompt: "朋友临时说“晚上出来吗”，你更常见的真实状态是？",
      options: [
        { label: "先说看情况，再和沙发深度沟通。", value: 1 },
        { label: "心里已经去过了，身体还在犹豫。", value: 2 },
        { label: "去，活着总得见点人。", value: 3 },
      ],
    },
    {
      id: "q7",
      tag: "工位场景",
      prompt: "打开待办列表时，最像你的那种心理活动是？",
      options: [
        { label: "先排优先级，至少看起来很专业。", value: 3 },
        { label: "先看最简单的，让人生有点反馈。", value: 2 },
        { label: "先盯着它看，等任务自己发酵。", value: 1 },
      ],
    },
    {
      id: "q8",
      tag: "饭点场景",
      prompt: "外卖页面上“再逛逛”这三个字，对你来说更像什么？",
      options: [
        { label: "一种拖延点餐的合法庇护。", value: 1 },
        { label: "一种命运试图让我多花十块的诱惑。", value: 2 },
        { label: "一种探索精神。", value: 3 },
      ],
    },
    {
      id: "q9",
      tag: "热搜场景",
      prompt: "看到一个巨大争议话题时，你更可能做哪类人？",
      options: [
        { label: "先看评论区，群众怎么想我再决定我怎么想。", value: 1 },
        { label: "先有判断，再看评论区印证自己。", value: 3 },
        { label: "看完发现我根本不关心，但也没退出。", value: 2 },
      ],
    },
    {
      id: "q10",
      tag: "人设场景",
      prompt: "当别人说“你给人的感觉一直很稳定”时，你最真实的反应是？",
      options: [
        { label: "谢谢，我装得确实挺完整。", value: 2 },
        { label: "稳定？只是懒得在你面前崩。", value: 1 },
        { label: "太好了，说明我的秩序感还是被看见了。", value: 3 },
      ],
    },
    {
      id: "q11",
      tag: "点赞场景",
      prompt: "你给别人点赞时，最常见的潜台词是什么？",
      options: [
        { label: "我看到了，咱俩关系先续一下。", value: 2 },
        { label: "内容确实行，这次不是礼貌电流。", value: 3 },
        { label: "点赞比回复安全。", value: 1 },
      ],
    },
    {
      id: "q12",
      tag: "已读场景",
      prompt: "看到“在吗”时，你更像在处理什么问题？",
      options: [
        { label: "时间管理问题。", value: 1 },
        { label: "情绪管理问题。", value: 2 },
        { label: "人际边界问题。", value: 3 },
      ],
    },
    {
      id: "q13",
      tag: "夜聊场景",
      prompt: "深夜想发点矫情东西时，你最常做哪一步？",
      options: [
        { label: "写，删，写，删，最后发个表情。", value: 1 },
        { label: "写完仅自己可见，像完成某种祭祀。", value: 2 },
        { label: "发，反正删帖也是一种发。", value: 3 },
      ],
    },
    {
      id: "q14",
      tag: "表情包场景",
      prompt: "别人发来一个意味深长的“？”时，你通常更先怀疑哪件事？",
      options: [
        { label: "我是不是哪里说重了。", value: 1 },
        { label: "ta是不是在试探我的语气。", value: 2 },
        { label: "这人是不是单纯手滑。", value: 3 },
      ],
    },
    {
      id: "q15",
      tag: "效率场景",
      prompt: "你对“先做完再说”这句话的真实态度更接近？",
      options: [
        { label: "可以，但先让我想想做到哪算完。", value: 1 },
        { label: "是对的，可惜我经常选择先说再说。", value: 2 },
        { label: "完全正确，事情就该被推进。", value: 3 },
      ],
    },
    {
      id: "q16",
      tag: "浏览器场景",
      prompt: "你当前浏览器标签页如果超过十个，通常说明什么？",
      options: [
        { label: "我正在进行一场复杂而无效的并行人生。", value: 1 },
        { label: "我一切尽在掌握，只是窗口比较多。", value: 3 },
        { label: "我也不知道，但现在关一个都像背叛。", value: 2 },
      ],
    },
    {
      id: "q17",
      tag: "关系场景",
      prompt: "对“有事直说”这四个字，你内心最真实的补充是？",
      options: [
        { label: "但最好别是现在。", value: 1 },
        { label: "但你也要允许我在心里演三遍。", value: 2 },
        { label: "可以，快说。", value: 3 },
      ],
    },
    {
      id: "q18",
      tag: "购物场景",
      prompt: "付款前最后一秒，你最常见的心理状态是？",
      options: [
        { label: "这真的是我需要的吗。", value: 1 },
        { label: "来都来了，别再羞辱购物车。", value: 2 },
        { label: "付吧，效率也是一种慈悲。", value: 3 },
      ],
    },
    {
      id: "q19",
      tag: "人格场景",
      prompt: "如果有人说“你是某种很特别的人”，你最像哪种反应？",
      options: [
        { label: "先怀疑对方是不是在铺垫请求。", value: 1 },
        { label: "表面谦虚，内心开始自动生成设定集。", value: 2 },
        { label: "愿闻其详，我可以配合特别。", value: 3 },
      ],
    },
    {
      id: "q20",
      tag: "结案场景",
      prompt: "做完整套测试后，如果结果非常像你，你最可能说哪句？",
      options: [
        { label: "离谱，但怎么离谱得这么准。", value: 2 },
        { label: "它懂个锤子，我只是刚好这样点了。", value: 1 },
        { label: "终于有机构正视我的复杂性。", value: 3 },
      ],
    },
  ];

  const METRIC_META = [
    { key: "trace", label: "信息贴附度" },
    { key: "latency", label: "决策回旋度" },
    { key: "revision", label: "复查倾向值" },
    { key: "resistance", label: "流程摩擦度" },
  ];

  const RESULT_LIBRARY = {
    GHOST: {
      name: "已读装死型",
      code: "GHOST",
      opening: "你不是没看见，你是在给关系降噪。",
      summary: "你处理世界的方式不是拒绝，而是先把它晾一晾，看看它会不会自己冷静下来。",
      description:
        "你对明确行动始终维持一种礼貌而稳定的延迟。你不是没有判断，只是习惯在判断和执行之间垫一层缓冲棉。对外界来说，你像一位低功耗待机的体面人：消息会看，邀约会收，回应会给，但总得晚一点、再晚一点、等气氛合适一点。你并不热衷制造冲突，也不急着给世界答复，仿佛所有关系都值得先静音三分钟再说。若把你放进群聊、工作流与社交网络的联合环境中，你最擅长的不是发光，而是让存在感保持在一种谁都挑不出毛病、但谁也别想催动你的微妙区间。",
    },
    AFK: {
      name: "赛博失焦型",
      code: "AFK",
      opening: "你的人在这里，魂在别的标签页。",
      summary: "你不是故意对抗世界，只是注意力经常被别处先一步截胡。",
      description:
        "研究数据显示，你的节奏断断续续，像在同时经营三份人生。你不是明确地拒绝，只是极其容易被别处的念头、通知、窗外动静、上一句对白和下一顿饭带走。你面对题目的方式带着一种现代互联网人熟悉的飘忽感：看了、想了、差一点决定了，然后灵魂轻轻拐了个弯。你不是不认真，只是认真这件事在你身上从来不是一条直线。放在日常场景里，你就是那种会打开待办、盯两秒、顺手点开天气、最后突然去搜一个完全不相干问题的人。你的稳定性建立在一种非常不稳定的分神技术之上。",
    },
    BOTH: {
      name: "端水糊弄型",
      code: "BOTH",
      opening: "你最大的立场，就是先别把话说死。",
      summary: "你在选择面前的第一反应不是判断，而是给每个选项都留面子。",
      description:
        "你不太爱和任何一个选项彻底绑定。你擅长在 A 和 B 之间修一条临时便道，今天叫圆融，明天叫缓冲，后天也可以解释成审慎。你处理选择的方式往往是“先靠近、再撤回、再礼貌靠近”，这种弹性让你看起来很好说话，也让你在真正需要拍板时总想多留一扇门。社交上，这让你显得有分寸；工作上，这让你像个能商量的人；但内心深处你很清楚，这不只是温和，更是一种不给世界抓住把柄的生存智慧。",
    },
    LOOP: {
      name: "内耗转经轮型",
      code: "LOOP",
      opening: "你在一题里完成了一场精神公转。",
      summary: "你并非优柔寡断，你只是愿意让每个念头都排队发表意见。",
      description:
        "你面对选择时，内心几乎自带一套会议制度。每个念头都想发言，每个后果都要被预演一遍，所以别人看见的是你谨慎，只有你自己知道那是一场高密度的内部拉扯。你不是不会选，而是每次一选，脑内就会自动冒出一个反方律师。你的生活因此显得复杂、细腻、有层次，也稍微有点累。你能在一句普通提示里想出五种后果，在一次简单决定里体验完整的后悔链路。",
    },
    OKOK: {
      name: "流程舔狗型",
      code: "OKOK",
      opening: "系统说请稍候，你甚至想说辛苦了。",
      summary: "你对流程有一种近乎道德层面的尊重，仿佛网页提示也是社会契约。",
      description:
        "你面对规则和流程时有一种近乎道德层面的合作精神。别人看到提示会烦，你会先看完；别人把步骤当阻碍，你往往先默认它有存在的理由。你并不软弱，只是深谙一条当代生存哲学：既然已经身在流程中，不如先把流程走顺。放到现实里，你是那种会认真看申请须知、在排队线内排队、并且真心认为“按照说明来”可以减少世界熵值的人。你温和、可靠，也因此特别适合被当成文明样本。",
    },
    ACTOR: {
      name: "假装认真型",
      code: "ACTOR",
      opening: "你连答题都在维持一种像样。",
      summary: "你并不一定更真诚，但你很会呈现真诚看上去该有的样子。",
      description:
        "你身上有一种很难忽略的体面感。停顿得刚好，表态得得体，很多时候连犹豫都显得像经过排练。问题在于，你有时并不只是单纯表达自己，而是在顺手维护一个“此刻我应该看起来怎样”的版本。这不是装，而是一种熟练的人设维护技术。你深知现代社会里，真实并不总比像真更有用，于是你自然会把这种技巧带进关系、表达与每一次公开亮相里。",
    },
    FILTER: {
      name: "人设精修型",
      code: "FILTER",
      opening: "你不是在回答问题，你是在给自己修图。",
      summary: "你擅长把真实冲动先打磨一下，再以更体面的版本提交给世界。",
      description:
        "你对“我到底应该表现成什么样”这件事拥有高度自觉。面对某些问题你会突然变慢，面对另一些问题又异常果断，像一位熟练的修图师，对自己的公开版本进行局部液化与精修。你并不完全虚伪，只是太明白表达本身就是筛选。你想被理解，但更想被准确且优雅地理解；你愿意暴露一点自己，但最好是经你批准的那一版。结果就是，别人交原图，你更像在交发布稿。",
    },
    SIGIL: {
      name: "鼠标画符型",
      code: "SIGIL",
      opening: "你没在答题，你在给页面做法。",
      summary: "你做很多事都带着强烈的私人仪式感，仿佛不按自己的节奏来就不算真的开始。",
      description:
        "你对很多事情的处理都带着一种不太好解释的施法感。别人眼里只是简单选择，你却总能把它过成一套私人仪式：先确认气氛，再感受阻力，再按照自己的节奏绕一圈。你既不完全服从秩序，也不彻底放弃秩序，而是始终维持一种“我先用自己的方式和世界谈一谈”的态度。现代互联网人最爱的是控制感，而你把控制感发展成了一种触控玄学。若说别人是在生活，你更像是在给生活做法。",
    },
    FORM: {
      name: "表单孝子型",
      code: "FORM",
      opening: "如果网页让你填三遍，你真的会填三遍。",
      summary: "你推进流程的速度很快，且很少怀疑流程本身是否值得尊敬。",
      description:
        "你的做事方式整洁、直接、几乎没有废动作，给人一种被表单文明深度驯化过的安心感。你相信系统给出顺序，自然有其安排；你相信一件事一件事往下走，总能抵达某种结论；你也相信尽快做完，总比停在原地怀疑更体面。于是你像一张会自主前进的在线申请表，目标明确、动作干净、情绪留白。你未必真心热爱规则，但你已经发展出一种和规则共处的高效率默契。",
    },
    ALLIN: {
      name: "结果赌狗型",
      code: "ALLIN",
      opening: "你真正想要的只有一件事：快点知道答案。",
      summary: "你推进速度极高，对结论的饥渴远大于对铺垫的耐心。",
      description:
        "你看待很多事情都像在冲终点。铺垫可以有，但最好少一点；过程可以走，但最好快一点；结果如果迟迟不来，你就会立刻开始怀疑这一切是否值得。你不是鲁莽，你是结论成瘾。互联网训练你相信一切都应当更快：更快加载、更快跳过、更快知道自己到底是个什么东西。于是你天然会把很多体验都理解为一台等待被开奖的机器。",
    },
    MISCLK: {
      name: "手滑返工型",
      code: "MISCLK",
      opening: "你的选择不是做出来的，是蹭出来的。",
      summary: "你并非一定没有主意，但想法落地时总会顺手磕碰一点东西。",
      description:
        "你在执行层面展现出鲜明的返工气质。节奏偏快，说明你不是没想法；但改动与补救又说明，你和“准确落实想法”之间永远隔着一点误差。你像那类很会说“我知道我要什么”，但真的走过去时总会顺手碰倒点什么的人。生活里，这种气质常表现为消息打太快、结论下太急、付款前突然回头、嘴比脑快半拍。你不缺冲动，也不缺执行，只是常常需要为自己的先手速度补一份小额售后。",
    },
    NOPE: {
      name: "按钮起义型",
      code: "NOPE",
      opening: "你和每一个弹窗之间，都隔着一场小型政变。",
      summary: "你对任何打断与指挥都天然不耐烦，并愿意用点击表达立场。",
      description:
        "只要有人开始管你，你就会本能地先不耐烦。你不喜欢被安排、不喜欢被等，也不喜欢在状态正顺的时候被谁横插一脚。哪怕对方说得有道理，你也总想先顶一句再说。这并不证明你难相处，只能说明你对控制权异常敏感。对你而言，任何流程一旦显得太像在管理你，你就会立刻想证明：抱歉，我不是你的员工。互联网把太多人训成了顺民，而你仍保留着一点很原始的逆反，这很好笑，也很珍贵。",
    },
    TRACE: {
      name: "逐字超度型",
      code: "TRACE",
      opening: "你看题像在给每个字做法事。",
      summary: "你的注意力黏得很紧，读什么都想读到底。",
      description:
        "你是那种会把信息完整吃进脑子里的人。别人扫一眼就过去的东西，你往往愿意慢一点、贴近一点、把细节摸清再表态。你不一定慢，但你很贴题；你不一定纠结，但你会认真接触每一段内容，给人一种过于文明的浏览质感。现实生活里，这常意味着你对细节有礼貌，对秩序有敬畏，对“随便看看”这件事抱有天然的不信任。",
    },
    SNAP: {
      name: "秒回判官型",
      code: "SNAP",
      opening: "你不是在思考，你是在盖章。",
      summary: "你判断快、出手快、推进快，像一位为世界加快审理效率的临时法官。",
      description:
        "你对迟疑有一种明确的蔑视。看见、理解、判断、执行，几乎在同一段呼吸里完成，像给现实生活配了快捷键。你很少给选择第二次机会，也不喜欢让世界等你，仿佛每一个多余停顿都在侮辱效率。对于旁观者来说，这种人格既像果断，也像一点点不可阻挡的武断；但无论如何，它都比拖泥带水更有气势。",
    },
    RECHK: {
      name: "复查祖师型",
      code: "RECHK",
      opening: "来都来了，你必须再看一遍。",
      summary: "你的谨慎带着一种祖传复核气质，仿佛任何提交都应先过审。",
      description:
        "你并不显得慌张，但你也绝不愿意草草了事。你的特点不是剧烈内耗，而是稳定复查：回去看一眼、再确认一下、确保没有因为一时爽快而留下终生把柄。这样的习惯很容易被误读成不自信，实际上它更像一种私人审计制度。你给自己的每个决定都留痕、复核、背书，恨不得在提交前再出具一份说明函。社交上，这会让你显得稳；工作上，这会让你像质检；生活里，这会让你永远是那个出门后愿意再摸一次口袋的人。",
    },
    UNDO: {
      name: "撤回文学型",
      code: "UNDO",
      opening: "你最稳定的品质，就是持续后悔。",
      summary: "你出手很快，但后悔也快，像把人生过成一串可撤销编辑记录。",
      description:
        "你是那种敢先出手、也敢马上改口的人。你并不慢，甚至往往比别人先一步做出反应；只是反应之后，你的第二反应总在路上。于是你的人生像一篇正在实时修改的文稿：先写、再删、再润色、再补一刀。你不缺判断，只是每个判断都难逃后续审美。这样的你，在互联网世界里极具时代性：不是不敢做决定，而是更怕没给自己留下重写的机会。",
    },
  };

  const RESULT_BY_SIGNATURE = {
    "D-S-C-O": "GHOST",
    "D-S-C-R": "AFK",
    "D-S-V-O": "BOTH",
    "D-S-V-R": "LOOP",
    "A-S-C-O": "OKOK",
    "A-S-C-R": "ACTOR",
    "A-S-V-O": "FILTER",
    "A-S-V-R": "SIGIL",
    "D-F-C-O": "FORM",
    "D-F-C-R": "ALLIN",
    "D-F-V-O": "MISCLK",
    "D-F-V-R": "NOPE",
    "A-F-C-O": "TRACE",
    "A-F-C-R": "SNAP",
    "A-F-V-O": "RECHK",
    "A-F-V-R": "UNDO",
  };

  const state = createInitialState();

  const elements = {
    body: document.body,
    jumpMethodBtn: document.getElementById("jumpMethodBtn"),
    peekLibraryBtn: document.getElementById("peekLibraryBtn"),
    introScreen: document.getElementById("introScreen"),
    testScreen: document.getElementById("testScreen"),
    resultScreen: document.getElementById("resultScreen"),
    startBtn: document.getElementById("startBtn"),
    sampleId: document.getElementById("sampleId"),
    progressBar: document.getElementById("progressBar"),
    progressText: document.getElementById("progressText"),
    questionNumber: document.getElementById("questionNumber"),
    questionTag: document.getElementById("questionTag"),
    questionPrompt: document.getElementById("questionPrompt"),
    optionList: document.getElementById("optionList"),
    prevBtn: document.getElementById("prevBtn"),
    nextBtn: document.getElementById("nextBtn"),
    actionTray: document.getElementById("actionTray"),
    stageHint: document.getElementById("stageHint"),
    resultName: document.getElementById("resultName"),
    resultCode: document.getElementById("resultCode"),
    matchScore: document.getElementById("matchScore"),
    resultOpening: document.getElementById("resultOpening"),
    resultDescription: document.getElementById("resultDescription"),
    resultSummary: document.getElementById("resultSummary"),
    metricRows: document.getElementById("metricRows"),
    evidenceList: document.getElementById("evidenceList"),
    restartBtn: document.getElementById("restartBtn"),
    backHomeBtn: document.getElementById("backHomeBtn"),
    focusOverlay: document.getElementById("focusOverlay"),
    focusAcceptBtn: document.getElementById("focusAcceptBtn"),
    focusSkipBtn: document.getElementById("focusSkipBtn"),
    loadOverlay: document.getElementById("loadOverlay"),
    ethicsOverlay: document.getElementById("ethicsOverlay"),
    ethicsConfirmBtn: document.getElementById("ethicsConfirmBtn"),
    ethicsLaterBtn: document.getElementById("ethicsLaterBtn"),
    submitOverlay: document.getElementById("submitOverlay"),
    submitConfirmBtn: document.getElementById("submitConfirmBtn"),
    submitCancelBtn: document.getElementById("submitCancelBtn"),
  };

  wireEvents();
  renderIntroSeed();

  function createInitialState() {
    return {
      screen: "intro",
      startedAt: 0,
      currentIndex: 0,
      currentViewStartedAt: 0,
      answers: Array.from({ length: QUESTION_BANK.length }, () => null),
      questionMetrics: Array.from({ length: QUESTION_BANK.length }, () => ({
        visits: 0,
        totalViewMs: 0,
        firstLatencyMs: null,
        answerChanges: 0,
        clicks: 0,
        promptSamples: 0,
        optionSamples: 0,
        straySamples: 0,
        moveSamples: 0,
        pointerDistance: 0,
        hoverMs: 0,
        wheelEvents: 0,
      })),
      session: {
        optionSelections: 0,
        backtracks: 0,
        revisitCount: 0,
        blurCount: 0,
        hiddenAt: 0,
        hiddenDurationMs: 0,
        keydowns: 0,
        resizeCount: 0,
      },
      pointer: {
        lastX: null,
        lastY: null,
        lastStamp: 0,
      },
      behavior: {
        totalMoveSamples: 0,
        contentSamples: 0,
        straySamples: 0,
        totalHoverMs: 0,
        hoverStartedAt: 0,
        impatienceClicks: 0,
        focusChoice: "skip",
        focusResponseMs: 0,
        fullScreenSuccess: false,
        ethicsChoice: "confirm",
        ethicsResponseMs: 0,
        ethicsDelay: false,
        shiftTriggered: false,
        shiftMissClicks: 0,
        confirmBackouts: 0,
        confirmResponseMs: 0,
        leaveAttempts: 0,
      },
      answerSum: 0,
      disturbanceFlags: {
        fakeLoad: false,
        ethics: false,
        shiftArmed: false,
      },
      overlayOpenedAt: 0,
      completed: false,
    };
  }

  function wireEvents() {
    elements.startBtn.addEventListener("click", openFocusOverlay);
    elements.peekLibraryBtn.addEventListener("click", () => {
      document.getElementById("methodSection").scrollIntoView({ behavior: "smooth", block: "start" });
    });
    elements.jumpMethodBtn.addEventListener("click", () => {
      document.getElementById("methodSection").scrollIntoView({ behavior: "smooth", block: "start" });
    });

    elements.focusAcceptBtn.addEventListener("click", async () => {
      state.behavior.focusChoice = "focus";
      state.behavior.focusResponseMs = performance.now() - state.overlayOpenedAt;
      try {
        if (document.documentElement.requestFullscreen) {
          await document.documentElement.requestFullscreen();
          state.behavior.fullScreenSuccess = !!document.fullscreenElement;
        }
      } catch (error) {
        state.behavior.fullScreenSuccess = false;
      }
      closeOverlay(elements.focusOverlay);
      startTest();
    });

    elements.focusSkipBtn.addEventListener("click", () => {
      state.behavior.focusChoice = "skip";
      state.behavior.focusResponseMs = performance.now() - state.overlayOpenedAt;
      closeOverlay(elements.focusOverlay);
      startTest();
    });

    elements.prevBtn.addEventListener("click", goPrevQuestion);
    elements.nextBtn.addEventListener("click", handleNext);
    elements.actionTray.addEventListener("click", (event) => {
      if (state.disturbanceFlags.shiftArmed && state.behavior.shiftTriggered && event.target !== elements.nextBtn) {
        state.behavior.shiftMissClicks += 1;
      }
    });
    elements.actionTray.addEventListener("pointerenter", () => {
      if (
        state.screen === "test" &&
        state.currentIndex >= 13 &&
        !state.behavior.shiftTriggered &&
        state.answers[state.currentIndex] !== null
      ) {
        state.behavior.shiftTriggered = true;
        elements.actionTray.classList.add("is-shifted");
        elements.nextBtn.textContent = "继续测试";
        elements.stageHint.textContent = "这个按钮今天有点自己的想法。";
      }
    });

    elements.ethicsConfirmBtn.addEventListener("click", () => closeEthicsOverlay("confirm"));
    elements.ethicsLaterBtn.addEventListener("click", () => closeEthicsOverlay("delay"));
    elements.submitConfirmBtn.addEventListener("click", confirmSubmit);
    elements.submitCancelBtn.addEventListener("click", cancelSubmit);

    elements.restartBtn.addEventListener("click", resetExperience);
    elements.backHomeBtn.addEventListener("click", resetExperience);

    window.addEventListener("pointermove", handlePointerMove, { passive: true });
    window.addEventListener("wheel", handleWheel, { passive: true });
    window.addEventListener("keydown", handleKeydown);
    document.addEventListener("visibilitychange", handleVisibilityChange);
    window.addEventListener("resize", () => {
      if (state.screen === "test") {
        state.session.resizeCount += 1;
      }
    });
    window.addEventListener("beforeunload", (event) => {
      if (state.screen === "test" && !state.completed && state.startedAt) {
        state.behavior.leaveAttempts += 1;
        event.preventDefault();
        event.returnValue = "";
      }
    });
  }

  function renderIntroSeed() {
    elements.sampleId.textContent = `会话 OFTI-${Math.floor(1000 + Math.random() * 9000)}`;
  }

  function openFocusOverlay() {
    state.overlayOpenedAt = performance.now();
    openOverlay(elements.focusOverlay);
  }

  function startTest() {
    state.screen = "test";
    state.startedAt = performance.now();
    state.currentIndex = 0;
    elements.stageHint.textContent = "按第一直觉回答就好。";
    showScreen("test");
    renderQuestion(0, true);
  }

  function showScreen(target) {
    const mapping = {
      intro: elements.introScreen,
      test: elements.testScreen,
      result: elements.resultScreen,
    };

    Object.entries(mapping).forEach(([name, node]) => {
      node.classList.toggle("screen--active", name === target);
    });
  }

  function renderQuestion(index, firstRender) {
    closeCurrentViewTiming();

    state.currentIndex = index;
    state.currentViewStartedAt = performance.now();
    const metric = state.questionMetrics[index];
    metric.visits += 1;
    if (metric.visits > 1) {
      state.session.revisitCount += 1;
    }

    const question = QUESTION_BANK[index];
    elements.questionNumber.textContent = String(index + 1).padStart(2, "0");
    elements.questionTag.textContent = question.tag;
    elements.questionPrompt.textContent = question.prompt;
    elements.progressText.textContent = `第 ${index + 1} / ${QUESTION_BANK.length} 题`;
    elements.progressBar.style.width = `${((index + 1) / QUESTION_BANK.length) * 100}%`;
    elements.prevBtn.disabled = index === 0;
    elements.nextBtn.disabled = state.answers[index] === null;
    elements.nextBtn.textContent = index === QUESTION_BANK.length - 1 ? "查看结果" : "下一题";
    elements.actionTray.classList.remove("is-shifted");
    elements.stageHint.textContent = index === QUESTION_BANK.length - 1 ? "确认后将生成结果。" : "按第一直觉回答就好。";

    elements.optionList.innerHTML = "";

    question.options.forEach((option, optionIndex) => {
      const optionButton = document.createElement("button");
      optionButton.type = "button";
      optionButton.className = "option";
      optionButton.dataset.label = String.fromCharCode(65 + optionIndex);
      optionButton.dataset.optionIndex = String(optionIndex);
      optionButton.innerHTML = `<span class="option__text">${option.label}</span>`;
      if (state.answers[index] === optionIndex) {
        optionButton.classList.add("is-selected");
      }
      optionButton.addEventListener("click", () => selectOption(optionIndex));
      optionButton.addEventListener("pointerenter", beginHoverSample);
      optionButton.addEventListener("pointerleave", endHoverSample);
      elements.optionList.append(optionButton);
    });

    elements.questionPrompt.removeEventListener("pointerenter", beginHoverSample);
    elements.questionPrompt.removeEventListener("pointerleave", endHoverSample);
    elements.questionPrompt.addEventListener("pointerenter", beginHoverSample);
    elements.questionPrompt.addEventListener("pointerleave", endHoverSample);

    if (!firstRender) {
      window.scrollTo({ top: 0, behavior: "smooth" });
    }
  }

  function selectOption(optionIndex) {
    const index = state.currentIndex;
    const metric = state.questionMetrics[index];
    const previous = state.answers[index];

    metric.clicks += 1;
    state.session.optionSelections += 1;

    if (metric.firstLatencyMs === null) {
      metric.firstLatencyMs = performance.now() - state.currentViewStartedAt;
    }

    if (previous !== null && previous !== optionIndex) {
      metric.answerChanges += 1;
    }

    state.answers[index] = optionIndex;
    state.answerSum = computeAnswerSum();
    elements.nextBtn.disabled = false;

    Array.from(elements.optionList.children).forEach((node, candidateIndex) => {
      node.classList.toggle("is-selected", candidateIndex === optionIndex);
    });
  }

  function goPrevQuestion() {
    if (state.currentIndex === 0) {
      return;
    }
    state.session.backtracks += 1;
    renderQuestion(state.currentIndex - 1, false);
  }

  function handleNext() {
    if (state.answers[state.currentIndex] === null) {
      return;
    }

    if (state.currentIndex === QUESTION_BANK.length - 1) {
      state.overlayOpenedAt = performance.now();
      openOverlay(elements.submitOverlay);
      return;
    }

    const nextIndex = state.currentIndex + 1;

    if (!state.disturbanceFlags.fakeLoad && nextIndex === 5) {
      state.disturbanceFlags.fakeLoad = true;
      triggerFakeLoad(() => renderQuestion(nextIndex, false));
      return;
    }

    if (!state.disturbanceFlags.ethics && nextIndex === 10) {
      state.disturbanceFlags.ethics = true;
      state.overlayOpenedAt = performance.now();
      openOverlay(elements.ethicsOverlay);
      return;
    }

    if (nextIndex >= 13) {
      state.disturbanceFlags.shiftArmed = true;
    }

    renderQuestion(nextIndex, false);
  }

  function triggerFakeLoad(onDone) {
    openOverlay(elements.loadOverlay);
    const impatientHandler = () => {
      state.behavior.impatienceClicks += 1;
    };
    elements.loadOverlay.addEventListener("click", impatientHandler);
    window.setTimeout(() => {
      elements.loadOverlay.removeEventListener("click", impatientHandler);
      closeOverlay(elements.loadOverlay);
      onDone();
    }, 2200);
  }

  function closeEthicsOverlay(choice) {
    state.behavior.ethicsChoice = choice;
    state.behavior.ethicsResponseMs = performance.now() - state.overlayOpenedAt;
    state.behavior.ethicsDelay = choice === "delay";
    closeOverlay(elements.ethicsOverlay);
    window.setTimeout(() => {
      renderQuestion(state.currentIndex + 1, false);
    }, choice === "delay" ? 850 : 0);
  }

  function cancelSubmit() {
    state.behavior.confirmBackouts += 1;
    state.behavior.confirmResponseMs = performance.now() - state.overlayOpenedAt;
    closeOverlay(elements.submitOverlay);
  }

  function confirmSubmit() {
    state.behavior.confirmResponseMs = performance.now() - state.overlayOpenedAt;
    closeOverlay(elements.submitOverlay);
    closeCurrentViewTiming();
    state.completed = true;
    const report = buildReport();
    renderResult(report);
    state.screen = "result";
    showScreen("result");
    window.scrollTo({ top: 0, behavior: "smooth" });
  }

  function buildReport() {
    const totals = summarizeMetrics();
    const traceScore = totals.hasPointerProfile
      ? clamp(100 * ((totals.contentRatio * 0.6) + (totals.hoverStrength * 0.25) + ((1 - totals.strayRatio) * 0.15)))
      : clamp(
          42 +
            Math.min(state.session.backtracks, 4) * 7 +
            Math.min(state.session.revisitCount, 5) * 5 +
            (state.session.keydowns > 0 ? 8 : 0) -
            Math.min(state.behavior.impatienceClicks, 3) * 5
        );
    const latencyScore = clamp(
      100 *
        ((totals.medianLatency / 12000) * 0.54 +
          (totals.longPauseRatio * 0.26) +
          (totals.hiddenRatio * 0.12) +
          (Math.min(state.behavior.focusResponseMs, 7000) / 7000) * 0.08)
    );
    const revisionScore = clamp(
      100 *
        (Math.min(totals.changeCount / 8, 1) * 0.42 +
          Math.min(state.session.backtracks / 6, 1) * 0.24 +
          Math.min(state.session.revisitCount / 8, 1) * 0.22 +
          Math.min(state.behavior.confirmBackouts / 2, 1) * 0.12)
    );
    const resistanceScore = clamp(
      100 *
        ((state.behavior.focusChoice === "skip" ? 0.2 : 0.02) +
          Math.min(state.behavior.impatienceClicks / 6, 1) * 0.24 +
          Math.min(state.behavior.shiftMissClicks / 4, 1) * 0.18 +
          (state.behavior.ethicsChoice === "delay" ? 0.16 : 0.03) +
          Math.min(state.behavior.leaveAttempts, 1) * 0.1 +
          Math.min(state.session.blurCount / 5, 1) * 0.12 +
          Math.min(state.behavior.confirmBackouts / 2, 1) * 0.12)
    );

    const answerNoise = ((state.answerSum / (QUESTION_BANK.length * 3)) - 0.5) * 8;
    const normalized = {
      trace: clamp(traceScore + answerNoise),
      latency: clamp(latencyScore - answerNoise * 0.3),
      revision: clamp(revisionScore + answerNoise * 0.5),
      resistance: clamp(resistanceScore - answerNoise * 0.4),
    };

    const signature = [
      normalized.trace >= 50 ? "A" : "D",
      normalized.latency >= 50 ? "S" : "F",
      normalized.revision >= 50 ? "V" : "C",
      normalized.resistance >= 50 ? "R" : "O",
    ].join("-");

    const resultKey = RESULT_BY_SIGNATURE[signature] || "GHOST";
    const result = RESULT_LIBRARY[resultKey];
    const matchScore = computeMatchScore(normalized);
    const evidence = buildEvidence(normalized, totals);

    return {
      result,
      matchScore,
      metrics: normalized,
      evidence,
    };
  }

  function summarizeMetrics() {
    const latencies = state.questionMetrics
      .map((metric) => metric.firstLatencyMs)
      .filter((value) => typeof value === "number");
    const totalMoves = Math.max(state.behavior.totalMoveSamples, 1);
    const contentSamples = state.behavior.contentSamples;
    const hoverStrength = Math.min(state.behavior.totalHoverMs / 20000, 1);
    const hiddenRatio = Math.min(state.session.hiddenDurationMs / Math.max(performance.now() - state.startedAt, 1), 1);

    return {
      medianLatency: median(latencies),
      longPauseRatio: latencies.length
        ? latencies.filter((value) => value > 6500).length / latencies.length
        : 0,
      contentRatio: contentSamples / totalMoves,
      strayRatio: state.behavior.straySamples / totalMoves,
      hoverStrength,
      changeCount: state.questionMetrics.reduce((sum, metric) => sum + metric.answerChanges, 0),
      hiddenRatio,
      longestLatency: latencies.length ? Math.max(...latencies) : 0,
      longestLatencyIndex: state.questionMetrics.findIndex(
        (metric) => metric.firstLatencyMs === (latencies.length ? Math.max(...latencies) : -1)
      ),
      hasPointerProfile: state.behavior.totalMoveSamples >= 18,
    };
  }

  function buildEvidence(metrics, totals) {
    const evidence = [];
    const longestIndex = totals.longestLatencyIndex >= 0 ? totals.longestLatencyIndex + 1 : 0;
    const changeCount = state.questionMetrics.reduce((sum, metric) => sum + metric.answerChanges, 0);
    const contentPercent = Math.round((state.behavior.contentSamples / Math.max(state.behavior.totalMoveSamples, 1)) * 100);
    const strayPercent = Math.round((state.behavior.straySamples / Math.max(state.behavior.totalMoveSamples, 1)) * 100);

    if (longestIndex) {
      evidence.push(
        `你在第 ${longestIndex} 题前停顿了 ${formatDuration(totals.longestLatency)}，说明某些普通问题也足够让你展开一轮内部辩论。`
      );
    }

    if (changeCount > 0) {
      evidence.push(`你中途改动了 ${changeCount} 次答案，说明“先选一个”和“真的选定”在你这里不是一回事。`);
    } else {
      evidence.push("你整场几乎没有改动答案，说明你要么判断很稳，要么懒得回头。");
    }

    if (state.session.backtracks > 0 || state.behavior.confirmBackouts > 0) {
      evidence.push(
        `你返回检查了 ${state.session.backtracks} 次，并在提交前临时取消了 ${state.behavior.confirmBackouts} 次，复核倾向相当稳定。`
      );
    }

    if (state.behavior.impatienceClicks > 0 || state.behavior.shiftMissClicks > 0) {
      evidence.push(
        `在少数界面变化出现时，你额外触发了 ${state.behavior.impatienceClicks + state.behavior.shiftMissClicks} 次重复操作，说明你对流程波动的耐受度有限。`
      );
    }

    if (!totals.hasPointerProfile) {
      evidence.push("你给页面留下的可见操作痕迹不多，说明你更习惯快速判断，而不是把每一步都表现在外面。");
    } else if (metrics.trace >= 50) {
      evidence.push(`你的注意力有 ${contentPercent}% 的时间贴着题面本身，说明你会把字面信息吃得很完整。`);
    } else {
      evidence.push(`你的注意力有 ${strayPercent}% 的时间从题面上滑开，说明你更依赖整体感觉，而不是逐行咀嚼。`);
    }

    if (state.behavior.focusChoice === "focus") {
      evidence.push("入场时你接受了沉浸模式建议，说明你对完整体验仍保留基本礼貌。");
    } else {
      evidence.push("入场时你跳过了沉浸模式建议，说明你对“建议”这两个字的耐心十分有限。");
    }

    return evidence.slice(0, 5);
  }

  function renderResult(report) {
    elements.resultName.textContent = report.result.name;
    elements.resultCode.textContent = report.result.code;
    elements.matchScore.textContent = `匹配度 ${report.matchScore}%`;
    elements.resultOpening.textContent = report.result.opening;
    elements.resultDescription.textContent = report.result.description;
    elements.resultSummary.textContent = report.result.summary;

    elements.metricRows.innerHTML = "";
    METRIC_META.forEach((meta) => {
      const value = Math.round(report.metrics[meta.key]);
      const row = document.createElement("div");
      row.className = "metric-row";
      row.innerHTML = `
        <div class="metric-row__content">
          <div class="metric-row__top">
            <span class="metric-label">${meta.label}</span>
            <span class="metric-row__value">${value}</span>
          </div>
          <div class="metric-row__bar">
            <span class="metric-row__fill" style="width:${value}%"></span>
          </div>
        </div>
      `;
      elements.metricRows.append(row);
    });

    elements.evidenceList.innerHTML = "";
    report.evidence.forEach((item) => {
      const li = document.createElement("li");
      li.textContent = item;
      elements.evidenceList.append(li);
    });
  }

  function closeCurrentViewTiming() {
    if (!state.currentViewStartedAt) {
      return;
    }
    endHoverSample();
    const metric = state.questionMetrics[state.currentIndex];
    metric.totalViewMs += performance.now() - state.currentViewStartedAt;
    state.currentViewStartedAt = 0;
  }

  function handlePointerMove(event) {
    if (state.screen !== "test") {
      return;
    }

    const now = performance.now();
    if (now - state.pointer.lastStamp < 60) {
      return;
    }

    const metric = state.questionMetrics[state.currentIndex];
    state.pointer.lastStamp = now;

    if (state.pointer.lastX !== null) {
      const dx = event.clientX - state.pointer.lastX;
      const dy = event.clientY - state.pointer.lastY;
      metric.pointerDistance += Math.hypot(dx, dy);
    }

    state.pointer.lastX = event.clientX;
    state.pointer.lastY = event.clientY;
    metric.moveSamples += 1;
    state.behavior.totalMoveSamples += 1;

    const promptRect = elements.questionPrompt.getBoundingClientRect();
    const optionRect = elements.optionList.getBoundingClientRect();
    const inPrompt = pointInRect(event.clientX, event.clientY, inflateRect(promptRect, 12));
    const inOptions = pointInRect(event.clientX, event.clientY, inflateRect(optionRect, 12));

    if (inPrompt) {
      metric.promptSamples += 1;
      state.behavior.contentSamples += 1;
    } else if (inOptions) {
      metric.optionSamples += 1;
      state.behavior.contentSamples += 1;
    } else {
      metric.straySamples += 1;
      state.behavior.straySamples += 1;
    }
  }

  function handleWheel() {
    if (state.screen !== "test") {
      return;
    }
    state.questionMetrics[state.currentIndex].wheelEvents += 1;
  }

  function handleKeydown(event) {
    if (state.screen !== "test") {
      return;
    }

    state.session.keydowns += 1;
    const keyMap = { Digit1: 0, Digit2: 1, Digit3: 2 };
    if (Object.prototype.hasOwnProperty.call(keyMap, event.code)) {
      const mapped = keyMap[event.code];
      if (QUESTION_BANK[state.currentIndex].options[mapped]) {
        selectOption(mapped);
      }
    }
    if (event.code === "ArrowLeft") {
      goPrevQuestion();
    }
    if (event.code === "Enter" && state.answers[state.currentIndex] !== null) {
      handleNext();
    }
  }

  function handleVisibilityChange() {
    if (state.screen !== "test") {
      return;
    }
    if (document.hidden) {
      state.session.blurCount += 1;
      state.session.hiddenAt = performance.now();
    } else if (state.session.hiddenAt) {
      state.session.hiddenDurationMs += performance.now() - state.session.hiddenAt;
      state.session.hiddenAt = 0;
    }
  }

  function beginHoverSample() {
    if (state.screen !== "test") {
      return;
    }
    endHoverSample();
    state.behavior.hoverStartedAt = performance.now();
  }

  function endHoverSample() {
    if (state.screen !== "test" || !state.behavior.hoverStartedAt) {
      return;
    }
    const delta = performance.now() - state.behavior.hoverStartedAt;
    state.behavior.totalHoverMs += delta;
    state.questionMetrics[state.currentIndex].hoverMs += delta;
    state.behavior.hoverStartedAt = 0;
  }

  function openOverlay(node) {
    node.hidden = false;
    elements.body.classList.add("body--locked");
  }

  function closeOverlay(node) {
    node.hidden = true;
    if ([elements.focusOverlay, elements.loadOverlay, elements.ethicsOverlay, elements.submitOverlay].every((item) => item.hidden)) {
      elements.body.classList.remove("body--locked");
    }
  }

  function resetExperience() {
    if (document.fullscreenElement && document.exitFullscreen) {
      document.exitFullscreen().catch(() => {});
    }
    const freshState = createInitialState();
    Object.keys(state).forEach((key) => {
      state[key] = freshState[key];
    });
    showScreen("intro");
    closeOverlay(elements.focusOverlay);
    closeOverlay(elements.loadOverlay);
    closeOverlay(elements.ethicsOverlay);
    closeOverlay(elements.submitOverlay);
    renderIntroSeed();
    window.scrollTo({ top: 0, behavior: "smooth" });
  }

  function computeAnswerSum() {
    return state.answers.reduce((sum, answerIndex, questionIndex) => {
      if (answerIndex === null) {
        return sum;
      }
      return sum + QUESTION_BANK[questionIndex].options[answerIndex].value;
    }, 0);
  }

  function computeMatchScore(metrics) {
    const distances = [
      Math.abs(metrics.trace - 50),
      Math.abs(metrics.latency - 50),
      Math.abs(metrics.revision - 50),
      Math.abs(metrics.resistance - 50),
    ];
    const clarity = distances.reduce((sum, value) => sum + value, 0) / distances.length;
    return Math.round(clamp(78 + clarity * 0.45, 79, 98));
  }

  function median(values) {
    if (!values.length) {
      return 0;
    }
    const sorted = [...values].sort((a, b) => a - b);
    const middle = Math.floor(sorted.length / 2);
    return sorted.length % 2 === 0 ? (sorted[middle - 1] + sorted[middle]) / 2 : sorted[middle];
  }

  function pointInRect(x, y, rect) {
    return x >= rect.left && x <= rect.right && y >= rect.top && y <= rect.bottom;
  }

  function inflateRect(rect, amount) {
    return {
      left: rect.left - amount,
      right: rect.right + amount,
      top: rect.top - amount,
      bottom: rect.bottom + amount,
    };
  }

  function formatDuration(ms) {
    if (ms < 1000) {
      return `${Math.round(ms)}ms`;
    }
    return `${(ms / 1000).toFixed(ms >= 10000 ? 0 : 1)} 秒`;
  }

  function clamp(value, min = 0, max = 100) {
    return Math.min(max, Math.max(min, value));
  }
})();
