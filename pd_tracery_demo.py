import tracery
from tracery.modifiers import base_english
import pandas as pd
from pandas import isnull, to_datetime
import time

import inflect
p = inflect.engine()


def ordinal(text, *params):
    return p.ordinal(text)

def number_to_words(text, *params):
    kwargs=dict(param.split('=') for param in params)
    try:
        resp = p.number_to_words(text, **kwargs)
    except:
        resp = "no recorded"
    return resp


def plural(text, *params):
    if params: return p.plural(text, params[0])
    return p.plural(text)

def plural_noun(text, *params):
    if params: return p.plural_noun(text, params[0])
    return p.plural_noun(text)

def plural_verb(text, *params):
    if params: return p.plural_verb(text, params[0])
    return p.plural_verb(text)

def plural_adj(text, *params):
    if len(params): return p.plural_adj(text, params[0])
    return p.plural_adj(text)

def singular_noun(text, *params):
    return p.singular_noun(text)

def no(text, *params):
    return p.no(text)

def num(text, *params):
    return p.num(text)

def compare(text, *params):
    return p.compare(text, params[0])

def compare_nouns(text, *params):
    return p.compare_nouns(text,params[0])

def compare_adjs(text, *params):
    return p.compare_adjs(text,params[0])

def a(text, *params):
    return p.a(text)

def an(text, *params):
    return p.an(text)

def present_participle(text, *params):
    return p.present_participle(text)

def classical(text, *params):
    kwargs=dict(param.split('=') for param in params)
    return p.classical(text, **kwargs)

def gender(text, *params):
    return p.gender(text)

def defnoun(text, *params):
    return p.defnoun(text)

def defverb(text, *params):
    return p.defverb(text)

def defadj(text, *params):
    return p.defadj(text)

def defa(text, *params):
    return p.defa(text)

def defan(text, *params):
    return p.defan(text)

def istrue(flag, *params):
    if flag=='True': return params[0]
    else: return ''

def isNotNull(text, *params):
    if isnull(text) or text=='nan': return ''
    kwargs=dict(param.split('=') for param in params)
    pre = kwargs['pre'] if 'pre' in kwargs else ''
    post = kwargs['post'] if 'post' in kwargs else ''
    brackets = True if 'brackets' in kwargs else False
    txt = "({pre}{text}{post})" if brackets else "{pre}{text}{post}"
    return txt.format(text=text,pre=pre,post=post)

def ifelse(flag, *params):
    if flag=='True': return params[0]
    else: return params[1]

def int_to_words(text, *params):
    try:
        text=int(float(text))
    except: pass
    kwargs=dict(param.split('=') for param in params)
    return p.number_to_words(text, **kwargs)

def pdtime(text, *params):
    if params:
        return to_datetime(text).strftime(','.join(params))
    return to_datetime(text).strftime("%b %d %Y %H:%M:%S")

def brackets(text, *params):
    if text:
        return '({})'.format(text)
    else: return ''

def floatround(text, *params):
    return "{0:.2f}".format(float(text))

def inc(text, *params):
    return str(int(float(text)+1))

def dec(text, *params):
    return str(int(float(text)-1))

def branch(text, *params):
    if text==params[0].strip():
        ans = params[1].strip()
    else:
        ans = params[2].strip()
    return '[condition:#{}#]'.format(ans)

def whileNot0(text, *params):
    if int(text) > 0:
        return '[i:{i}]'.format(i=str(int(text)-1))
    return '[do:#{}#]'.format(params[0].strip())

def takeFrom(text, *params):
    return str(int(params[0].strip()) - int(text))

def add(text, *params):
    return str(int(text) + int(params[0].strip()))

def subtract(text, *params):
    return str(int(text) - int(params[0].strip()))

def multiply(text, *params):
    return str(int(text) * int(params[0].strip()))

def divide(text, *params):
    return str(int(text) / int(params[0].strip()))

def neg(text, *params):
    return str(-int(text))

def echotext(text, *params):
  return text

pytracery_logic = {
    'round':floatround,
    '_inc': inc,
    '_dec': dec,
    '_branch':branch,
    '_whileNot0': whileNot0,
    '_takeFrom':takeFrom,
    '_neg':neg,
    '_add':add,
    '_subtract':subtract,
    '_multiply':multiply,
    '_divide':divide,
    '_echo':echotext
}


#--

inflect_english = {
    'compare':compare,
    'compare_nouns':compare_nouns,
    'compare_adjs':compare_adjs,
    'a':a,
    'a2':a,
    'an':an,
    'ordinal': ordinal,
    'number_to_words': number_to_words,
    'plural':plural,
    'plural_noun':plural_noun,
    'plural_verb':plural_verb,
    'plural_adj':plural_adj,
    'singular_noun':singular_noun,
    'no':no,
    'num':num,
    'present_participle':present_participle,
    'classical':classical,
    'gender':gender,
    'defnoun':defnoun,
    'defverb':defverb,
    'defadj':defadj,
    'defa':defa,
    'defan':defan,
    #
    'int_to_words': int_to_words,
    'istrue': istrue,
    'ifelse': ifelse,
    'isNotNull':isNotNull,
    'pdtime': pdtime,
    'brackets':brackets,
}



def pandas_row_mapper(row, rules, root,  modifiers=base_english):
    ''' Function to parse single row of dataframe '''
    row=row.to_dict()
    rules=rules.copy()

    for k in row:
        rules[k] = str(row[k])
        grammar = tracery.Grammar(rules)
        if modifiers is not None:
            if isinstance(modifiers,list):
                for modifier in modifiers:
                    grammar.add_modifiers(modifier)
            else:
                grammar.add_modifiers(modifiers)

    return grammar.flatten(root)

def pandas_tracery(df, rules, root, modifiers=base_english):
  return df.apply(lambda row: pandas_row_mapper(row, rules, root, modifiers), axis=1)

def pdt_inspect(df):
  return(df)

def pdt_test1(df):
  return type(df)

def pdt_demo(df):
  return pandas_tracery(df, _demo_rules, "#origin#",  modifiers=base_english)

#Create example rule to apply to each row of dataframe
_demo_rules = {'origin': "#code# was placed #position#!",
         'position': "#pos.uppercase#"}

def pdt_pole(df):
  return pandas_tracery(df, _pole_rules, "#origin#",  modifiers=[base_english,inflect_english,pytracery_logic])

_pole_rules={'origin':'''
 [branch:#polesitter#]#branch##condition#''',
 'polesitter': '#pole._branch(True, onpole, notonpole)#',
'onpole':'''With his #polesthisyear._inc.int_to_words.ordinal# pole this season,
this is #driver#'s #polesbeforenow._inc.int_to_words# pole position ever
and #polesthiscct._inc.int_to_words.ordinal# at this circuit.''',
'notonpole':'Missing out on pole but still on the front row...'}


