# Engine-Validated Training Data
## How the Deferential Realism Pipeline Could Serve as Training Infrastructure
## for Structural-Role Analysis

## Abstract

Most analytical capabilities are taught to language models through fine-tuning
on human-labeled examples. The bottleneck is human time: producing examples
at the scale model training requires takes years, and the resulting datasets
inherit whatever inconsistencies and biases the labelers brought to the work.
A different architecture is possible. The Deferential Realism framework's
analytical pipeline produces structural-role analyses through paired LLM
generation and deterministic validation: an LLM generates hypothesis-form
constraint stories from topic seeds, a Prolog engine validates them against
twelve formal subsystems, and accepted stories accumulate as a corpus of
engine-validated structural analyses. The architecture is operational and
has produced a corpus of approximately 3,300 validated stories.

This paper argues that pipelines of this form — LLM hypothesis generation
paired with deterministic validation — produce training data with properties
that human-labeled training data lacks, and that the resulting data could
serve as training infrastructure for models acquiring structural-role
analysis as a capability. The central claim is methodological rather than
empirical: the architecture exists, the corpus exists, and the question of
whether models trained on this data acquire transferable analytical
capability is empirically tractable. We describe the architecture, the
properties of the data it produces, the predictions it generates about
trained-model behavior, and the experiments that would test those
predictions.

## 1. The Training-Data Bottleneck

Modern language models acquire most of their capabilities through some
combination of pretraining on broad text corpora and fine-tuning on
specific task examples. For tasks where the desired capability is well-
defined and demonstrations are easy to produce — translation, summarization,
classification against established taxonomies — fine-tuning works because
human-labeled examples can be produced at the scale the training requires.

For tasks where the capability is harder to demonstrate — analytical
disciplines that require coordinated judgment across multiple positions,
classifications that depend on structural reasoning rather than pattern
matching, evaluations that involve adversarial frame-shifting — the
bottleneck is acute. Producing one good example of structural-role analysis
takes a trained practitioner an hour. Producing a thousand takes a thousand
hours. Producing a hundred thousand takes the work of dedicated teams over
substantial periods. The labor cost is the gating constraint, not the
analytical methodology.

This bottleneck has consequences. Models trained on small or inconsistent
human-labeled datasets inherit the labelers' inconsistencies, struggle with
edge cases the labelers didn't cover, and tend to produce outputs that look
plausible to humans without necessarily being structurally correct.
Reinforcement learning from human feedback addresses some of this by
adding evaluative pressure on top of the labeled data, but the underlying
constraint — that humans are the source of validation — is preserved.
Whatever humans are systematically wrong about, the trained model inherits.

## 2. An Alternative Architecture

A different architecture becomes possible when a domain admits formal
validation. If correct outputs in the domain can be checked by deterministic
computation rather than by human judgment, then training data can be
produced by paired generation-and-validation: LLM proposes outputs, formal
system validates them, accepted outputs accumulate as training corpus.
The validation is consistent across cases in a way human labelers cannot
match, and the throughput is bounded by computation rather than by human
labor.

The Deferential Realism framework's analytical pipeline implements this
architecture for a specific domain: structural-role analysis of constraints
in power-stratified situations. The pipeline takes a topic seed (a
constraint to be analyzed), prompts an LLM to generate a complete
constraint story specifying beneficiaries, victims, perspectives at
multiple observer positions, classification, and supporting structural
properties. The story is then validated by a Prolog engine implementing
twelve formal subsystems that check structural coherence, indexical
completeness, signature consistency, and cohomological obstruction
patterns. Stories that pass validation are accepted; stories that fail
are returned with diagnostic feedback identifying the structural
inconsistency.

[Brief description of the existing pipeline: ~3,300 validated stories,
the topic-seed mechanism, the LLM batch generation through Anthropic's
API with prompt caching, the Prolog validation step, the JSON schema
that encodes the framework's structural requirements. ~400 words. Cite
the v5 framework paper for the analytical apparatus, the metrics-as-
routing principle for what makes the validation deterministic rather
than authority-claiming.]

The architecture's key property is that the validation is independent of
the generation. The LLM generating constraint stories does not know what
the engine will check; the engine checking stories does not adjust its
criteria based on what the LLM tends to produce. Errors the LLM makes are
caught by the engine because the engine's validation criteria are
mathematical rather than perceptual. The engine cannot be fooled by
plausible-looking outputs — it computes structural properties and flags
inconsistencies regardless of how natural the prose reads.

This is a stronger property than it might appear. A common pattern in
LLM-only validation pipelines is "LLM proposes, LLM checks," where a
second LLM call evaluates the first LLM's output. This pattern inherits
both calls' shared weaknesses: if both LLMs are systematically wrong
about something, neither catches the error. Engine-based validation
breaks this dependence. The engine has no shared training history with
the LLM; its validation criteria come from formal axioms; what it
catches is structural inconsistency that no LLM-only pipeline can
guarantee catching.

## 3. Properties of the Resulting Data

The corpus produced by this pipeline has properties that distinguish it
from human-labeled training data.

**Consistency.** The validation criteria are deterministic. A constraint
story that passes validation today would pass the same validation a year
from now if the engine is unchanged, and would fail the same validation
under the same conditions every time. Human labelers cannot match this
consistency — labels drift across labelers, across time, and across
mood. The engine's deterministic behavior means that whatever structural
patterns appear in the validated corpus reflect patterns the engine's
formal criteria identify, not patterns the labelers happened to notice.

**Scalability.** Generation is bounded by LLM throughput; validation is
bounded by Prolog computation. Both scale with available compute rather
than with available human labor. A corpus of 30,000 or 300,000 validated
stories is operationally accessible if topic curation can be scaled —
which is itself a smaller problem than producing the analyses by hand.

**Negative examples with diagnostic content.** Human labelers typically
produce positive examples (correctly labeled cases) rather than negative
examples (incorrectly labeled cases with explanations of why they're
wrong). The pipeline produces both: stories that fail validation come
with engine-generated diagnostic feedback identifying which subsystem
flagged the failure and why. Negative examples are often more
informative than positive ones for training, because they teach the
model what kinds of mistakes the validation catches.

**Coverage adjustability.** The corpus's topical distribution is determined
by the topic seeds. If the existing corpus underrepresents certain
domains — historical cases, cross-cultural situations, technical-systems
analyses, AI-agent-specific situations — those domains can be addressed
directly by generating new seeds. This is a different problem than
expanding a human-labeled dataset, where coverage gaps require finding
labelers with relevant domain expertise.

**Auditability.** Every validated story comes with the engine's
validation report — which subsystems checked it, what each subsystem
found, what diagnostic surface accompanied any flags. Training data
typically loses this provenance information; the pipeline preserves it
because the validation report is part of the training example by
construction.

## 4. What Could Be Trained This Way

A model trained on engine-validated constraint stories would be exposed
to a specific kind of analytical pattern: structural-role analysis of
power-stratified situations, with consistent application of the
framework's twelve subsystems' criteria across thousands of cases. The
question is what such a model would learn.

Three possibilities, in increasing order of how much the training would
demonstrate.

**Memorization.** The model could learn to reproduce specific constraint
stories from the training corpus when prompted with their topics. This
is the failure mode — it would mean the training produced no transferable
capability, just rote recall of training examples. Tests for this:
prompt the model with novel topics not in the corpus and see whether it
produces well-structured analyses or fails outside the training
distribution.

**Pattern interpolation.** The model could learn to produce constraint
stories that match the corpus's surface patterns — appropriate
vocabulary, plausible-looking perspective sets, conventional
classifications — without internalizing the structural validation
criteria the engine actually checks. Tests for this: have the model
generate stories on novel topics, run them through the engine, and see
whether they pass validation at rates above baseline. A model that
matches surface patterns without internalizing structure will produce
plausible-looking outputs that fail validation when checked.

**Capability transfer.** The model could learn to perform structural-
role analysis as a transferable analytical move, applying the framework's
discipline to topics outside the corpus's distribution and producing
analyses that pass engine validation. This is the result that would
demonstrate the training architecture works. Tests for this: prompt the
model on topics from substantially different domains than the training
corpus covers (technical systems, historical cases, AI-agent situations)
and check whether engine validation rates remain at training-distribution
levels.

The three possibilities are empirically distinguishable. Each predicts
different patterns in held-out testing, and the experiments are not
expensive to run. A small experiment — train a model on a subset of the
corpus, test on held-out subset and on novel domains — would distinguish
the three within weeks of work. The current paper does not run this
experiment. It argues that the experiment is worth running and specifies
what would count as evidence for each possibility.

## 5. Predictions and Risks

If the training architecture works — if models trained on engine-
validated data acquire transferable structural-role analysis capability —
several predictions follow.

**Domain transfer should track corpus topical coverage.** Models trained
on a corpus heavy with contemporary political and institutional examples
will analyze those topics best. Models trained with broader topical
coverage will generalize more broadly. The relationship between training
corpus coverage and held-out performance is testable.

**Engine-validation rates should improve with training data scale, then
plateau.** Larger training corpora should produce models that pass
validation at higher rates on novel topics, but the improvement curve
should plateau as the model's capability approaches the engine's
validation distribution. The plateau location is informative about how
much of the framework's analytical content is learnable from validated
examples versus how much requires the human discipline that produced
the original examples.

**Cross-architecture training transfer should work.** Models trained on
corpus produced by one LLM architecture should pass engine validation
at comparable rates to models trained on corpus produced by a different
architecture, provided both produce engine-validated outputs. The
deterministic validation removes the LLM-specific patterns that would
otherwise differentiate the corpora.

**Risks are also predictable.** The most serious is that models trained
on engine-validated data could learn to game the engine's validation
criteria — producing outputs that pass validation by exploiting
boundary conditions in the engine's checks rather than by performing
genuine structural analysis. This is the same risk reward-hacking
poses for RL training, and the same mitigations apply: adversarial
robustness of the validator becomes load-bearing, periodic external
review of what the trained model is producing remains necessary, and
the validator's failure modes need to be understood as part of the
training architecture rather than treated as separate concerns.

A second risk is that the training imparts the corpus's topical biases
along with its analytical discipline. A corpus heavy with political
examples produces a model that may overapply political framings to
non-political situations. Mitigation requires deliberate corpus
construction across topical domains, not assumption that the engine's
validation alone produces unbiased training data.

## 6. Connection to AI Alignment Work

Most current AI alignment research treats agent values as either
successfully transferred from human values via training or misaligned
in ways requiring better training. Both framings assume human-labeled
data is the source of validation. The architecture described here
suggests a third possibility: capabilities can be trained where formal
validation is available, with human review serving as the audit layer
rather than the primary validation mechanism.

This has specific implications for capabilities involving structural
reasoning about power-stratified situations — exactly the situations
that AI alignment work increasingly cares about. Multi-agent systems,
agent-principal relationships, the propagation of structural patterns
through training-induced naturalization: these are domains where the
DR framework predicts specific patterns and where engine-validated
training data could produce models capable of recognizing those
patterns in their own operations.

The framework's §5.5 protocol tests whether RL agents trained from
scratch in asymmetric environments exhibit the predicted structural
patterns. A complementary test would be whether models trained on
engine-validated structural analyses recognize the same patterns when
presented with novel power-stratified situations. The two tests are
complementary: §5.5 tests whether the patterns emerge in trained agent
behavior; the training-data application tests whether the patterns
transfer through validated example exposure.

If both tests work, the framework's claims about substrate-independent
structural-role analysis would be supported through two independent
empirical paths. If both fail, the framework's structural claim is in
trouble. If they split, the split itself is informative about which
parts of the framework's apparatus generalize and which require the
specific discipline the framework's developers maintain.

## 7. What Would Move This Forward

The paper proposes a methodology and identifies the experiments that
would test it. It does not claim the experiments have been run. The
work that would move the methodology forward, in order:

A small experiment training a model on a subset of the existing corpus
and testing held-out generalization at multiple distances from the
training distribution. This is the minimum viable demonstration. Cost:
small. Time: weeks.

If the small experiment works, scale-up experiments testing how the
training improves with corpus size, topic diversity, and validation
strictness. Cost: moderate. Time: months.

If scale-up works, deployment experiments embedding the trained model
in agent systems and testing whether the framework's predictions about
agent behavior in power-stratified environments are validated by the
trained model's own analyses of its situations. Cost: substantial.
Time: years. This is the work that would establish the architecture
as a contribution to AI alignment rather than as a methodological
proposal about training data.

The current paper's role is to specify the architecture clearly enough
that the experiments can be designed and run. It is not the experiment.
It is the proposal that the experiment is worth running, with enough
detail about what would count as evidence that the experiment's results
would be interpretable when produced.

## Conclusion

LLM-hypothesis-plus-deterministic-validation pipelines produce training
data with properties human-labeled data cannot match. The Deferential
Realism framework's analytical pipeline implements this architecture
for structural-role analysis and has produced a corpus of approximately
3,300 validated stories. Whether models trained on this corpus acquire
transferable analytical capability is an open empirical question with
tractable experiments to answer it. If the architecture works, it
represents a methodological contribution to how AI systems can be
trained for analytical capabilities where formal validation is
available — distinct from human-labeled fine-tuning and from
reinforcement learning from human feedback, complementing both rather
than replacing them.

The methodology's connection to AI alignment work is direct but
indirect: the architecture would let models acquire capabilities for
recognizing structural patterns in power-stratified situations,
including patterns involving the agents themselves. Whether such
capability transfers to the situations alignment work cares about most
is the experimental question. The framework's existing apparatus and
corpus make the experiment cheap to run. The paper's argument is that
running it would produce evidence of methodological value regardless
of which way the result goes.
