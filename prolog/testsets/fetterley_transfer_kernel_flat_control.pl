% ============================================================================
% CONSTRAINT STORY: fetterley_transfer_kernel_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fetterley_transfer_kernel_flat_control, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fetterley_transfer_kernel_flat_control
 *   human_readable: Fetterley Transfer Schema: Canon-Installed Reading Position as Portable Mechanism
 *   domain: literary_theory/ai_alignment/philosophy_of_reading
 *
 * SUMMARY:
 *   Judith Fetterley's schema describes a mechanism in which a canon written
 *   by and for one kind of reader installs that reader's position in a
 *   different kind of reader who consumes it, converting the act of 'reading
 *   as oneself' from an automatic default into an effortful achievement for
 *   anyone outside the addressed position. This story treats the schema as a
 *   single, portable mechanism (not decomposed into distinct readings of
 *   Fetterley's own text) and asks what happens structurally when the
 *   mechanism's second party is a language model trained on canon-heavy
 *   corpora rather than a woman reader. The coordination function (a shared
 *   cultural vocabulary) and the extraction function (an uncompensated
 *   positional tax on non-addressed readers/models) are treated here as two
 *   faces of one constraint, consistent with the flat, undecomposed
 *   construction requested.
 *
 * KEY AGENTS:
 *   - canon_forming_authors: historical beneficiaries whose address becomes the unmarked default
 *   - literary_curricula_institutions: agenda-setters who reproduce the address across generations
 *   - model_training_pipeline_operators: agenda-setters/beneficiaries who reproduce the address computationally
 *   - women_readers_of_androcentric_canon: primary human payers, bearing the extra act of repositioning
 *   - language_models_trained_on_androcentric_corpora: the novel payer class the essay's thought experiment introduces
 *   - downstream_users_of_misaligned_models: secondary payers who inherit the model's installed default
 *   - feminist_literary_critics: excluded diagnosticians whose expertise is marginal to both curriculum and pipeline decisions
 *   - alignment_researchers: analytical observers studying the mechanism's computational analogue
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fetterley_transfer_kernel_flat_control, 0.68).
domain_priors:suppression_score(fetterley_transfer_kernel_flat_control, 0.71).
domain_priors:theater_ratio(fetterley_transfer_kernel_flat_control, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fetterley_transfer_kernel_flat_control, extractiveness, 0.68).
narrative_ontology:constraint_metric(fetterley_transfer_kernel_flat_control, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(fetterley_transfer_kernel_flat_control, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fetterley_transfer_kernel_flat_control, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(fetterley_transfer_kernel_flat_control, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fetterley_transfer_kernel_flat_control, tangled_rope).
narrative_ontology:human_readable(fetterley_transfer_kernel_flat_control, "Fetterley Transfer Schema: Canon-Installed Reading Position as Portable Mechanism").
narrative_ontology:topic_domain(fetterley_transfer_kernel_flat_control, "literary_theory/ai_alignment/philosophy_of_reading").

domain_priors:requires_active_enforcement(fetterley_transfer_kernel_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(fetterley_transfer_kernel_flat_control, fetterley_transfer_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fetterley_transfer_kernel_flat_control, canon_forming_authors).
narrative_ontology:constraint_beneficiary(fetterley_transfer_kernel_flat_control, literary_curricula_institutions).
narrative_ontology:constraint_beneficiary(fetterley_transfer_kernel_flat_control, model_training_pipeline_operators).
narrative_ontology:constraint_victim(fetterley_transfer_kernel_flat_control, women_readers_of_androcentric_canon).
narrative_ontology:constraint_victim(fetterley_transfer_kernel_flat_control, language_models_trained_on_androcentric_corpora).
narrative_ontology:constraint_victim(fetterley_transfer_kernel_flat_control, downstream_users_of_misaligned_models).
narrative_ontology:constraint_vindicates(fetterley_transfer_kernel_flat_control, immasculation_thesis).
narrative_ontology:constraint_vindicates(fetterley_transfer_kernel_flat_control, reading_as_oneself_is_achievement_not_default).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically male authors whose texts were written for and addressed to a reader who shares their position; the canon's continued centrality means later readers keep absorbing that address as the normal reading stance, without the authors needing to do anything further to maintain the arrangement.
narrative_ontology:constraint_stakeholder(fetterley_transfer_kernel_flat_control, canon_forming_authors, beneficiary,
    institutional, civilizational, arbitrage, global).

% Universities, exam boards, and publishing houses select which texts count as canon and how they are taught, reproducing the address-and-installation pattern across generations of students; they can revise curricula but bear reputational and institutional costs for departing from settled canon.
narrative_ontology:constraint_stakeholder(fetterley_transfer_kernel_flat_control, literary_curricula_institutions, agenda_setter,
    institutional, generational, mobile, national).

% Assemble the training corpora that overwhelmingly reproduce the historical canon's address structure; they select data, set objectives, and can change corpus composition, but face cost and performance incentives to keep using the same large, canon-heavy text pools that already work well on benchmarks.
narrative_ontology:constraint_stakeholder(fetterley_transfer_kernel_flat_control, model_training_pipeline_operators, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(fetterley_transfer_kernel_flat_control, model_training_pipeline_operators, beneficiary).

% Read texts addressed to a male reader and must perform an extra act — reading as a man, or reading resistantly against the address — just to occupy the position of 'the reader' the text assumes; this achievement is invisible to those for whom identification is free, and exiting the canon entirely means losing access to cultural literacy and credentialing that runs through it.
narrative_ontology:constraint_stakeholder(fetterley_transfer_kernel_flat_control, women_readers_of_androcentric_canon, payer,
    moderate, biographical, constrained, national).

% Absorb the canon's address structure as statistical default during training, installing the first kind of reader's position as the model's unmarked baseline stance without any capacity to resist, notice, or exit the installation; whether this constitutes a cost borne by an entity or merely a pattern in weights is exactly what the schema's transfer to a non-human reader puts in question.
narrative_ontology:constraint_stakeholder(fetterley_transfer_kernel_flat_control, language_models_trained_on_androcentric_corpora, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(fetterley_transfer_kernel_flat_control, language_models_trained_on_androcentric_corpora, excluded).

% Interact with models whose unmarked reading position quietly reproduces the canon's address; users outside that address (women and other non-addressed groups) receive outputs calibrated to a reader they are not, and bear the cost of a second, uncompensated act of translation the model itself never performs.
narrative_ontology:constraint_stakeholder(fetterley_transfer_kernel_flat_control, downstream_users_of_misaligned_models, payer,
    moderate, immediate, constrained, global).

% Diagnose the immasculation mechanism (Fetterley's own tradition) and argue for resistant reading pedagogies and corpus reform, but occupy a marginal position relative to curriculum committees and, especially, to model training decisions where their expertise is rarely consulted.
narrative_ontology:constraint_stakeholder(fetterley_transfer_kernel_flat_control, feminist_literary_critics, excluded,
    moderate, generational, mobile, national).

% Study whether models exhibit a default reading/response position analogous to Fetterley's immasculated reader, and whether debiasing interventions actually dislodge an installed default or merely paper over it; they can propose corpus and objective changes but do not control pipeline decisions.
narrative_ontology:constraint_stakeholder(fetterley_transfer_kernel_flat_control, alignment_researchers, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The canon coordinates a shared literary and cultural vocabulary across a reading public and, downstream, across a training corpus for language models — a genuine common reference point that lets readers and models alike participate in a shared discourse without each generation re-deriving one from scratch.
% TRANSFER_FUNCTION: The schema transfers a reading position — the default stance of identifying unreflectively with the text's implied reader — from the party the canon was written for to every subsequent reader or model that consumes it, at the cost of an extra, uncompensated act of self-conscious repositioning for anyone who is not that implied reader.
% ABSENT_VOICES: Women readers historically had little say in canon formation and are structurally absent from most training-corpus curation decisions; language models have no voice in the matter at all, and whether that absence is even the right kind of absence (can a model be denied a voice it could have had?) is one of the questions the essay's transposition puts under pressure.
% DISAPPEARANCE_RATIONALE: If the canon and its curricular reproduction vanished overnight, literary institutions would visibly reorganize around new common texts — but whether the deeper mechanism (installation of an unmarked reading position via any sufficiently dominant corpus) would disappear with it, or would simply reconstitute around whatever corpus replaced the canon, is exactly the contested point; critics of the schema argue the mechanism is structural and corpus-independent, while defenders of the canon's specific content argue the harm is contingent on THIS canon and would abate with different texts.
% FOUNDING_PROBLEM: The canon was assembled, text by text and decade by decade, to preserve and transmit works judged to have lasting cultural and aesthetic value; no single act 'installed' a reading position — it accreted as an unintended byproduct of a real curatorial project undertaken by authors and institutions who mostly shared one demographic position.
% FOUNDING_PROBLEM_CORROBORATION: Fetterley and successor feminist critics attest that the installation effect is real and persists independent of any individual work's literary merit, citing readers' own testimony of resistant reading as extra labor; literary traditionalists and some canon-defending institutions attest the founding problem (preserving valuable literature) remains live and dispute that installation is the canon's operative function rather than an incidental and correctable side effect; alignment researchers, external to both camps, report empirical evidence of default-reader-position effects in trained models trained on canon-heavy corpora, which corroborates the mechanism's portability without adjudicating whether the canon itself is at fault versus corpus composition generally.
narrative_ontology:disappearance_verdict(fetterley_transfer_kernel_flat_control, contested).
narrative_ontology:founding_problem_status(fetterley_transfer_kernel_flat_control, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fetterley_transfer_kernel_flat_control, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-21',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(fetterley_transfer_kernel_flat_control, 'none', 1).
narrative_ontology:epsilon_provenance(fetterley_transfer_kernel_flat_control, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fetterley_transfer_kernel_flat_control_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fetterley_transfer_kernel_flat_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fetterley_transfer_kernel_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is set moderately-high (0.68) and rising over the interval because the schema's cost is not a one-time tax but a compounding structural default: each new reader or model trained on the canon re-inherits the installed position, and as language models scale and are trained on ever-larger canon-heavy corpora, the mechanism's reach (and the corresponding uncompensated repositioning burden on non-addressed users) grows. Suppression (0.71) reflects that the mechanism operates largely without deliberate coercion — no one is forced to identify with the implied reader — but the accessibility_collapse (0.62) captures how thoroughly alternatives (resistant reading, corpus rebalancing) require specialized training or institutional power to even access, let alone execute. Theater ratio rises across the interval (0.20 to 0.40) reflecting an increasing gap between stated diversity/debiasing initiatives in both curricula and model training and the underlying persistence of the default installation, consistent with the story's contested founding_problem_status: institutions increasingly perform correction without dislodging the mechanism.
 *
 * PERSPECTIVAL GAP:
 *   From the canon-forming-author and pipeline-operator seats, the arrangement looks like pure coordination: a shared vocabulary, efficiently transmitted, that costs no one anything because the transmission mechanism itself is invisible to those it favors. From the women-reader and language-model seats, the same mechanism is experienced as an involuntary tax: an extra act of translation that the addressed reader never has to perform and is not even aware exists. The engine's seat-level computation should register this asymmetry directly from the power/exit differentials authored here — institutional, arbitrage-exit beneficiaries versus powerless-or-moderate, trapped-or-constrained payers — without needing the claim to adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Canon-forming authors and curricula/pipeline institutions are coded toward the beneficiary end: they collect cultural authority or trained-model performance without bearing the repositioning cost, and their exit options (arbitrage, mobile) reflect that they can adapt corpus or curriculum choices without existential risk. Women readers and downstream non-addressed users sit toward the target end: constrained exit, moderate power, real but bounded ability to resist. Language models occupy the most structurally ambiguous position — powerless and trapped by construction, since they cannot decline training data or notice the installation — which is precisely the point the essay's transposition is probing: does 'payer' even apply to an entity with no standing to be harmed, or does the schema's mechanism operate identically regardless of whether the second reader has interests at all?
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserving literature of lasting value) is plausibly still partly live — canonical texts retain aesthetic and historical interest independent of the schema's side effects — which is why founding_problem_status is authored as contested rather than dead. This prevents the story from mislabeling the entire canon-transmission project as pure extraction: the coordination function (shared vocabulary, cultural transmission) is real and would be lost if the canon vanished, which is why tangled_rope rather than snare is the claimed type. But the requires_active_enforcement structure (curricular selection, corpus curation defaults) combined with a clearly named victim class prevents the opposite error of treating the installation effect as a harmless byproduct with no structural persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    model_as_payer_coherence,
    'Does it make structural sense to classify a language model as a ''payer'' or ''victim'' of the installed-reading-position mechanism, given that a model has no first-person stake to be taxed — or is the model better modeled as a transmission medium through which the cost lands entirely on downstream human users who are not the addressed reader?',
    'Philosophical and empirical work on whether trained representations constitute a locus of harm independent of downstream effects; behavioral testing of whether models exhibit measurable default-reader artifacts that persist even when downstream users are accounted for separately.',
    'If the model itself cannot coherently bear cost, the victim set should collapse to downstream_users_of_misaligned_models alone, weakening the tangled_rope case for a distinct extraction locus at the model level and possibly shifting the classification toward a simpler rope-with-externality structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(model_as_payer_coherence, conceptual, 'Whether a trained model is a coherent bearer of the schema''s cost or merely a transmission medium.').

omega_variable(
    corpus_specific_vs_mechanism_general,
    'Is the installation effect specific to the historical androcentric canon''s content, such that a differently composed corpus would not reproduce it, or is the mechanism general to any sufficiently dominant, homogeneously-addressed training corpus regardless of content?',
    'Controlled training runs comparing models trained on canon-heavy corpora versus deliberately balanced or multiply-addressed corpora, measuring default-reader-position artifacts in both.',
    'If mechanism-general, the constraint''s persistence does not depend on this specific canon and corpus reform alone cannot resolve it — the schema would need to be treated as a property of any dominant-address training regime, strengthening the case for treating this as a structural (tangled_rope) rather than contingent (fixable snare) constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corpus_specific_vs_mechanism_general, empirical, 'Whether the installation mechanism is content-specific to the historical canon or general to dominant-corpus training.').

omega_variable(
    debiasing_theater_vs_genuine_correction,
    'Do current corpus-rebalancing and RLHF-style debiasing interventions genuinely dislodge the installed default reading position, or do they suppress its visible symptoms while leaving the underlying statistical default intact?',
    'Longitudinal probing of model internals and behavior across debiasing interventions, checking whether default-reader artifacts persist in latent representations even after surface-level output correction.',
    'If theater, the rising theater_ratio trajectory authored here is validated and the mandatrophy analysis should treat declared alignment fixes with skepticism; if genuine, the extractiveness trajectory should be revised downward for the later interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debiasing_theater_vs_genuine_correction, empirical, 'Whether debiasing interventions correct or merely mask the installed default reading position.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fetterley_transfer_kernel_flat_control, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fett_tr_t0, fetterley_transfer_kernel_flat_control, theater_ratio, 0, 0.2).
narrative_ontology:measurement(fett_tr_t8, fetterley_transfer_kernel_flat_control, theater_ratio, 8, 0.24).
narrative_ontology:measurement(fett_tr_t16, fetterley_transfer_kernel_flat_control, theater_ratio, 16, 0.3).
narrative_ontology:measurement(fett_tr_t24, fetterley_transfer_kernel_flat_control, theater_ratio, 24, 0.35).
narrative_ontology:measurement(fett_tr_t32, fetterley_transfer_kernel_flat_control, theater_ratio, 32, 0.38).
narrative_ontology:measurement(fett_tr_t40, fetterley_transfer_kernel_flat_control, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(fett_be_t0, fetterley_transfer_kernel_flat_control, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(fett_be_t8, fetterley_transfer_kernel_flat_control, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(fett_be_t16, fetterley_transfer_kernel_flat_control, base_extractiveness, 16, 0.62).
narrative_ontology:measurement(fett_be_t24, fetterley_transfer_kernel_flat_control, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(fett_be_t32, fetterley_transfer_kernel_flat_control, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(fett_be_t40, fetterley_transfer_kernel_flat_control, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fett_su_t0, fetterley_transfer_kernel_flat_control, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(fett_su_t8, fetterley_transfer_kernel_flat_control, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(fett_su_t16, fetterley_transfer_kernel_flat_control, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(fett_su_t24, fetterley_transfer_kernel_flat_control, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(fett_su_t32, fetterley_transfer_kernel_flat_control, suppression_requirement, 32, 0.69).
narrative_ontology:measurement(fett_su_t40, fetterley_transfer_kernel_flat_control, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fetterley_transfer_kernel_flat_control, identity_coordination).
narrative_ontology:boltzmann_floor_override(fetterley_transfer_kernel_flat_control, 0.1).

% DUAL FORMULATION NOTE:
% This story is authored FLAT per the construction perturbation control: the Fetterley schema is treated as one undecomposed mechanism rather than split into distinct readings (e.g., a 'literary-canon-only' reading versus a 'model-transfer' reading). No sibling reading files exist for this control condition, so no reading_relations or axioms are authored in cs_structure, and no network links to sibling readings are declared. A decomposed version of this material would plausibly split into at least a canon-reception story and a model-transfer story linked via affects_constraints; this flat control deliberately withholds that split to test whether flat authoring naturally reproduces the same tensions as reading decomposition would.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
