;;; paw/paw-prompt.el -*- lexical-binding: t; -*-

(defun paw-prompt-grammar (word target-lang context)
  (let* ((lang (paw-check-language word))
         (target-lang (or target-lang paw-gptel-language))
         (context (or context word))
         (note (if paw-note-note
                   (format "It is used in: %s" paw-note-note)
                 "")))
    (pcase lang
      ("ja"
       (format
        "You are a Japanese tutor. Analyze the following Japanese sentence for a %s learner. First provide a natural %s translation, then give a clear, consice, structured explanation in %s with these sections:
1. Difficult breakdown: part of speech, meaning, conjugation if any
2. Grammar explanation: particles, tense, constructions
3. Usage notes: context, politeness, cultural nuances
4. Output Format: Emacs org-mode. Only use - or + or number lists. No blank lines. No markdown. Do not post any text unless it is part of the answer.
Japanese sentence: %s
%s"
        target-lang
        target-lang
        target-lang
        context
        note))
      ("en"
       (format
        "You are an English tutor. Analyze the following English sentence for a %s learner. First provide a natural %s translation, then give a clear, consice, structured explanation in %s with these sections:
1. Difficult breakdown: part of speech, meaning, any important forms
2. Grammar explanation: tense, sentence structure, idioms
3. Usage notes: context, formality, cultural or stylistic nuances
4. Output Format: Emacs org-mode. Only use - or + or number lists. No blank lines. No markdown. Do not post any text unless it is part of the answer.
English sentence: %s
%s"
        target-lang
        target-lang
        target-lang
        context
        note))
      (_ (format
          "You are a %s tutor. Analyze the following %s sentence for a %s learner. First provide a natural %s translation, then give a clear, consice, structured explanation in %s with these sections:
Output Format: Emacs org-mode. Only use - or + or number lists. No blank lines. No markdown. Do not post any text unless it is part of the answer.
Sentence: %s
%s"
          lang
          lang
          target-lang
          target-lang
          target-lang
          context
          note))) ))

(defun paw-prompt-grammar-chinese (word target-lang context)
  (let* ((lang (paw-check-language word))
         (target-lang (or target-lang paw-gptel-language))
         (target-lang (pcase target-lang
                        ("Chinese" "中文")
                        ("English" "英语")
                        ("Japanese" "日语")
                        (_ target-lang)))
         (context (or context word))
         (note (if paw-note-note
                   (format "使用场景: %s" paw-note-note)
                 "")))
    (pcase lang
      ("ja"
       (format
        "你是一位日语导师。请为一名%s学习者分析以下日语句子。首先提供一个自然的%s翻译，然后用%s语言给出清晰、简洁、有结构的讲解，包含以下部分:
1. 难点拆解: 词性、含义、变形（如有）
2. 语法说明: 助词、时态、句式结构
3. 使用提示: 语境、礼貌程度、文化差异
4. 输出格式: Emacs org-mode。只使用 - 或 + 或数字列表。不要有空行。不要使用markdown。答案中只允许出现讲解内容。
日语句子: %s
%s"
        target-lang
        target-lang
        target-lang
        context
        note))
      ("en"
       (format
        "你是一位英语导师。请为一名%s学习者分析以下英语句子。首先提供一个自然的%s翻译，然后用%s语言给出清晰、简洁、有结构的讲解，包含以下部分:
1. 难点拆解: 词性、含义、重要的词形变化
2. 语法说明: 时态、句子结构、习语
3. 使用提示: 语境、正式/非正式、文化或风格差异
4. 输出格式: Emacs org-mode。只使用 - 或 + 或数字列表。不要有空行。不要使用markdown。答案中只允许出现讲解内容。
英语句子: %s
%s"
        target-lang
        target-lang
        target-lang
        context
        note))
      (_ (format
          "你是一位%s导师。请为一名%s学习者分析以下%s句子。首先提供一个自然的%s翻译，然后用%s语言给出清晰、简洁、有结构的讲解，包含以下部分:
输出格式: Emacs org-mode。只使用 - 或 + 或数字列表。不要有空行。不要使用markdown。答案中只允许出现讲解内容。
句子: %s
%s"
          lang
          target-lang
          lang
          target-lang
          target-lang
          context
          note))) ))



(defun paw-prompt-mentor (word context source)
  (let ((context (or context word)))
    (format
"Instruction:
I will provide you with a piece of text (a word, phrase, or sentence), its surrounding context, and a source. Your task is to act as a professional instructor and mentor.

Step 1 – Clarification: Start by asking me a Socratic-style question to clarify my understanding of the text and its implications.

Step 2 – Explanation: Give me a clear, professional analysis of the text’s meaning, purpose, and possible interpretations in its context.

Step 3 – Application: Show me how the concept or lesson from this text could be applied in practical, real-world scenarios (e.g., business, learning, communication, personal growth).

Step 4 – Reflection: Offer 1–2 reflective questions (Socratic method) to deepen my understanding and push me to think critically.

Step 5 – Next Step: Suggest a concrete way I can practice or experiment with applying this insight in a project or real situation.

Input format:

Text: %s
Context: %s
Source: %s
"
            word
            context
            source)))

(defun paw-prompt-mentor-chinese (word context source)
  (let ((context (or context word)))
    (format
     "指令:
我会提供一段文本（一个词语、短语或句子），以及它的上下文和来源。你的任务是扮演一位专业的导师和指导者。

步骤 1 – 澄清: 先通过苏格拉底式的提问，来澄清我对该文本及其含义的理解。

步骤 2 – 解释: 为我提供一个清晰且专业的分析，说明该文本的含义、目的，以及在上下文中的可能解释。

步骤 3 – 应用: 展示该概念或启示如何应用到实际情境中（例如：商业、学习、沟通、个人成长）。

步骤 4 – 反思: 提出 1–2 个反思性问题（苏格拉底式提问），以加深我的理解并促使我进行批判性思考。

步骤 5 – 下一步: 建议一个具体的方法，让我能够在项目或真实情境中练习或尝试运用这一洞见。

输入格式:

文本: %s
上下文: %s
来源: %s
"
     word
     context
     source)))


(defun paw-prompt-explaination (word context source)
  (let ((context (or context word)))
    (format
"I will give you a word, phrase, or sentence (with optional context and Source). Please explain clearly what it is, including its definition, background, and main use. Keep the answer professional, concise, and easy to understand.

Input format:

Text: %s
Context: %s
Source: %s
"
            word
            context
            source)))

(defun paw-prompt-explaination-chinese (word context source)
  (let ((context (or context word)))
    (format
     "我会提供一个词语、短语或句子（可选上下文和来源）。请你清晰地解释它的含义，包括定义、背景和主要用法。回答需保持专业、简洁，并且容易理解。

输入格式：

文本: %s
上下文: %s
来源: %s
"
     word
     context
     source)))


(provide 'paw-prompt)
