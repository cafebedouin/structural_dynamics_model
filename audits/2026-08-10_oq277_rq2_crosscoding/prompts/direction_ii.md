You will read one description of a software failure and assign it exactly one category.

Here are the five categories, plus a sixth for incidents that fit none of them. All six are equally
available answers. Each category is given in its original wording followed by an English rendering.

A — 环境/平台 Quirk  (environment / platform quirk)
逻辑正确，运行环境隐含行为导致失效（dev 绿灯，target 暴露）。
系统逻辑正确，但运行环境（OS / shell / 客户端 / 文件系统）的隐含行为导致意外。
*The logic is correct, but implicit behaviour of the runtime environment — operating system, shell,
client, filesystem — causes it to fail. Characteristically: the development environment is always
green, and only running on the real target exposes it.*

B — 设计假设错配  (design-assumption mismatch)
代码假设 vs 实际（部署拓扑 / 契约 / 测试形态 / 涌现行为）。
代码基于某个假设，但假设与实际不一致。
*The code rests on an assumption — about deployment topology, a cross-component contract, the shape
of test input, or emergent behaviour — and the assumption does not match reality. Characteristically:
unit tests covering the logic correctly is not the same as covering the real caller's actual form.*

C — 错误吞噬与稀释  (error swallowing and dilution)
错误发生但被某层静默吃掉 / 跨层稀释失 cause / 自动批量放大。
错误真实发生了，但被某一层静默吃掉，或跨层传递时上游 cause 被稀释，最终用户视角看到的是"成功"或
一个失去信息的告警。
*The error really happened, but some layer silently ate it, or the upstream cause was diluted while
being passed across layers, so what the end user finally sees is "success" or an alert that has lost
its information.*

D — 链式幻觉与编造  (chained hallucination and fabrication)
把污染数据当事实，编造合理叙事推送给用户（最危险）。
错误不是消失，而是被加工成看起来正常的内容。链路中每一跳都会放大幻觉，前一跳的幻觉会被下游当作
事实执行。
*Corrupted data or polluted context is taken as fact and worked up into a plausible narrative that
is pushed to the user. The error does not disappear — it is processed into content that looks
normal. Each hop amplifies it, and one hop's fabrication is executed downstream as fact.*

E — 运维遗漏与取证盲区  (operational omission and forensic blind spot)
代码正确但部署/注册步骤遗漏 / 调试工具自身被屏蔽长潜伏。
两个子机制：(1) 运维遗漏 — 代码正确但部署/注册/配置步骤被漏（声明态 ≠ 运行时态）；
(2) 取证盲区 — 调试工具自身被屏蔽返回空内容，被误读为"正常"，导致超长潜伏。
*Two sub-mechanisms: (1) operational omission — the code is correct but a deployment, registration
or configuration step was missed, so declared state is not runtime state; (2) forensic blind spot —
the debugging tool is itself blocked and returns empty content, which is misread as "normal",
producing an extremely long latency before discovery.*

other — 机制不属于以上任何一类。
*The incident's mechanism is not one of the five above. This is a substantive answer, not a
leftover. Choose it whenever the mechanism described is genuinely a different kind of thing,
including when the description is too thin to identify a mechanism at all.*

---

Assign the single category that best matches the MECHANISM of the incident below — how the failure
worked, not where it happened or how severe it was.

INCIDENT
--------
Symptom:
{symptom}

Mechanism as described:
{mechanism_as_described}

How it was detected:
{detection_path}

Consequence:
{consequence}

---

Reply with exactly one of these tokens and nothing else:

A  B  C  D  E  other

No explanation, no confidence, no punctuation, no other words.
