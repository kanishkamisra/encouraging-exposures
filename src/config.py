CONFLICTS = {
    "he": ["him"],
    "she": ["her"],
    "they": ["them"],
    "I": ["me"],
    "we": ["us", "me"],
    "him": ["he"],
    "her": ["she"],
    "them": ["they"],
    "me": ["I", "we", "us"],
    "us": ["we", "me"],
    "the big doll": ["the beautiful doll", "the doll"],
    "the beautiful doll": ["the big doll", "the doll"],
    "the doll": ["the big doll", "the beautiful doll"],
    "the ball": ["the red ball"],
    "the red ball": ["the ball"],
    "the puppy": ["the cute puppy", "the small puppy"],
    "the cute puppy": ["the small puppy", "the puppy"],
    "the small puppy": ["the cute puppy", "the puppy"],
    "the baby": ["the cute baby", "the small baby"],
    "the cute baby": ["the small baby", "the baby"],
    "the small baby": ["the cute baby", "the baby"],
    "the cat": ["the cute cat", "the small cat"],
    "the cute cat": ["the small cat", "the cat"],
    "the small cat": ["the cute cat", "the cat"],
    "the soup": ["a bit of the soup"],
    "a bit of the soup": ["the soup"],
    "the milk": ["a bit of the milk"],
    "a bit of the milk": ["the milk"],
    "the cheerios": ["a bit of the cheerios", "a few cheerios"],
    "a bit of the cheerios": ["the cheerios", "a few cheerios"],
    "a few cheerios": ["the cheerios", "a bit of the cheerios"],
}

IMPLAUSIBLE = {
    "do": [
        "paspas",
        "paspis",
        "pasnal",
        "pasnas",
        "pasnil",
        "pasnis",
        "pispas",
        "pisnal",
        "pisnas",
        "pisnil",
        "pisnis",
        "nalnil",
        "nalnis",
        "nasnil",
        "nasnis",
        "nilnis",
        "nisnis",
        "nisnil",
        "nilnil",
    ],
    "pp": [
        "nalnil",
    ],
}


ALTERNATING_VERBS = """feed,give,lease,lend,loan,pass,pay,peddle,refund,render,rent,repay,sell,serve,trade,advance,allocate,allot,assign,award,bequeath,cede,concede,extend,grant,guarantee,issue,leave,offer,owe,promise,vote,will,yield,bring,take,forward,hand,mail,post,send,ship,slip,smuggle,sneak,bounce,float,roll,slide,carry,drag,haul,heave,heft,hoise,kick,lug,pull,push,schlep,shove,tote,tow,tug,barge,bus,cart,drive,ferry,fly,row,shuttle,truck,wheel,wire,bash,bat,bunt,catapult,chuck,flick,fling,flip,hit,hurl,kick,lob,pass,pitch,punt,shoot,shove,slam,slap,sling,throw,tip,toss,ask,cite,pose,preach,quote,read,relay,show,teach,tell,write,cable,email,e-mail,fax,modem,netmail,phone,radio,relay,satellite,semaphore,sign,signal,telephone,telecast,telegraph,telex,wire,wireless""".split(
    ","
)

DO_ONLY_VERBS = """accord,ask,bear,begrudge,bode,cost,deny,envy,flash,forbid,forgive,guarantee,issue,refuse,save,spare,strike,vouchsafe,wish,write,bet,bill,charge,fine,mulct,overcharge,save,spare,tax,tip,undercharge,wager,acknowledge,adopt,appoint,consider,crown,deem,designate,elect,esteeem,imagine,mark,nominate,ordain,proclaim,rate,recon,report,want,anoint,baptize,brand,call,christen,consecrate,crown,decree,dub,label,make,name,nickname,pronounce,rule,stamp,style,term,vote,adjudge,adjudicate,assume,avow,believe,confess,declare,fancy,find,judge,presume,profess,prove,suppose,think,warrant""".split(
    ","
)

PP_ONLY_VERBS = """address,administer,broadcast,convey,contribute,delegate,deliver,denounce,demonstrate,describe,dictate,dispatch,display,distribute,donate,elucidate,exhibit,express,explain,explicate,forfeit,illustrate,introduce,narrate,portray,proffer,recite,recommend,refer,reimburse,remit,restore,return,sacrifice,submit,surrender,transfer,transport,admit,allege,announce,articulate,assert,communicate,confess,convey,declare,mention,propose,recount,repeat,report,reveal,say,state,babble,bark,bawl,bellow,bleat,boom,bray,burble,cackle,call,carol,chant,chatter,chrip,cluck,coo,croak,croon,crow,cry,drawl,drone,gabble,gibber,groan,growl,grumble,grunt,hiss,holler,hoot,howl,jabber,lilt,lisp,moan,mumble,murmur,mutter,purr,rage,rasp,roar,rumble,scream,screech,shout,shriek,sing,snap,snarl,snuffle,splutter,squall,squawk,squeak,squeal,stammer,stutter,thunder,tisk,trill,trumpet,twitter,wail,warble,wheeze,whimper,whine,whisper,whistle,whoop,yammer,yap,yell,yelp,yodel,drop,hoist,lift,lower,raise,credit,entrust,furnish,issue,leave,present,provide,serve,supply,trust""".split(
    ","
)

DATIVE_VERBS = sorted(list(set(ALTERNATING_VERBS + DO_ONLY_VERBS + PP_ONLY_VERBS)))
