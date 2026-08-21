% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__marketplace_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__marketplace_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: speech_protection_kernel__marketplace_reading
 *   human_readable: Speech Protection (Marketplace of Ideas Reading)
 *   domain: Constitutional Law / Political Philosophy / Communication Rights
 *
 * SUMMARY:
 *   This constraint story instantiates the 'marketplace of ideas' reading of
 *   speech protection, which posits that truth will emerge from the free and
 *   open exchange of ideas, and that false or harmful speech is best
 *   countered by more speech, rather than by suppression. This reading
 *   prioritizes the collective epistemic benefit of society over individual
 *   autonomy as the primary justification for broad speech protection, and
 *   generally rejects content-based restrictions as distorting the
 *   truth-discovery process. The metrics reflect the operational reality of
 *   this reading, which, while claiming a coordination function, imposes
 *   significant costs on those harmed by speech and requires active
 *   suppression of alternative regulatory approaches.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__marketplace_reading, 0.15).
domain_priors:suppression_score(speech_protection_kernel__marketplace_reading, 0.75).
domain_priors:theater_ratio(speech_protection_kernel__marketplace_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__marketplace_reading, rope).
narrative_ontology:human_readable(speech_protection_kernel__marketplace_reading, "Speech Protection (Marketplace of Ideas Reading)").
narrative_ontology:topic_domain(speech_protection_kernel__marketplace_reading, "Constitutional Law / Political Philosophy / Communication Rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__marketplace_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__marketplace_reading, '55cc7ca4-7fdf-4fdf-9989-11beafef9ba7').
narrative_ontology:cs_kernel_codification('55cc7ca4-7fdf-4fdf-9989-11beafef9ba7', formalized).
narrative_ontology:cs_authority_grounding('55cc7ca4-7fdf-4fdf-9989-11beafef9ba7', lineage).
narrative_ontology:cs_interpretation_layer_present('55cc7ca4-7fdf-4fdf-9989-11beafef9ba7').
narrative_ontology:cs_reading_relation('55cc7ca4-7fdf-4fdf-9989-11beafef9ba7', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('55cc7ca4-7fdf-4fdf-9989-11beafef9ba7', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_reading_relation('55cc7ca4-7fdf-4fdf-9989-11beafef9ba7', speech_protection_kernel__dignity_reading, coexists_with).
narrative_ontology:cs_reading_relation('55cc7ca4-7fdf-4fdf-9989-11beafef9ba7', speech_protection_kernel__harm_threshold_reading, coexists_with).
narrative_ontology:cs_axiom('55cc7ca4-7fdf-4fdf-9989-11beafef9ba7', foundational, truth_emerges_from_free_exchange).
narrative_ontology:cs_axiom_status(truth_emerges_from_free_exchange, holdable).
narrative_ontology:cs_axiom_grounding('55cc7ca4-7fdf-4fdf-9989-11beafef9ba7', truth_emerges_from_free_exchange, empirically_contingent).
narrative_ontology:cs_axiom('55cc7ca4-7fdf-4fdf-9989-11beafef9ba7', foundational, content_neutrality_is_paramount).
narrative_ontology:cs_axiom_status(content_neutrality_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('55cc7ca4-7fdf-4fdf-9989-11beafef9ba7', content_neutrality_is_paramount, conventional).
narrative_ontology:cs_reference_frame('55cc7ca4-7fdf-4fdf-9989-11beafef9ba7', enlightenment_rationalism_framework).
narrative_ontology:cs_drift_state('55cc7ca4-7fdf-4fdf-9989-11beafef9ba7', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('55cc7ca4-7fdf-4fdf-9989-11beafef9ba7', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__marketplace_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, public_discourse).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, speakers).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, truth_seekers).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, targets_of_false_harmful_speech).
narrative_ontology:constraint_vindicates(speech_protection_kernel__marketplace_reading, marketplace_of_ideas_theory).
narrative_ontology:constraint_vindicates(speech_protection_kernel__marketplace_reading, epistemic_democracy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The collective societal process of idea exchange and deliberation, which is believed to benefit from open speech by allowing truth to emerge from competition with falsehoods.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, public_discourse, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(speech_protection_kernel__marketplace_reading, public_discourse).

% Individuals and groups who express ideas, opinions, and information, benefiting from broad protection against content-based restrictions, allowing them to contribute to the marketplace of ideas.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, speakers, beneficiary,
    moderate, biographical, mobile, global).

% Individuals and groups who seek to discern truth from falsehoods, relying on the open exchange of ideas to evaluate competing claims and arrive at informed conclusions.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, truth_seekers, beneficiary,
    moderate, biographical, mobile, global).

% Individuals or groups who are subjected to false, misleading, or harmful speech. Under this reading, their primary remedy is 'more speech' rather than restriction, meaning they bear the initial impact of such speech.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, targets_of_false_harmful_speech, payer,
    powerless, immediate, trapped, local).

% The institutions responsible for interpreting and enforcing speech protection laws, particularly the First Amendment. They uphold the principle that content-based restrictions are generally disfavored.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, courts_and_legislatures, agenda_setter,
    institutional, generational, analytical, national).

% Groups and individuals who argue for greater regulation of speech, particularly harmful or false speech, often prioritizing safety, dignity, or public order over unfettered expression. Their arguments for content-based restrictions are largely excluded by this reading's core tenets.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, advocates_for_speech_restriction, excluded,
    organized, biographical, constrained, national).

% Scholars and commentators who analyze the theoretical underpinnings and practical effects of speech protection, including the efficacy of the marketplace of ideas in contemporary contexts.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__marketplace_reading, public_discourse).
narrative_ontology:fixing_cost_class(speech_protection_kernel__marketplace_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a framework for society to collectively seek truth, manage disagreement, and foster democratic deliberation through the open and largely unrestricted exchange of ideas, with falsehoods to be countered by further speech.
% TRANSFER_FUNCTION: Transfers the primary burden of countering false or harmful speech from state censorship or prior restraint to the public sphere, requiring citizens to engage in counter-speech and critical evaluation.
% ABSENT_VOICES: Those who prioritize the immediate protection of vulnerable groups from speech-related harm, or who believe that certain categories of speech (e.g., hate speech, disinformation) are inherently harmful and do not contribute to truth-discovery. They would advocate for content-based restrictions that this reading largely rejects.
% DISAPPEARANCE_RATIONALE: If this reading of speech protection vanished, the legal landscape would shift dramatically towards allowing content-based restrictions, leading to a chilling effect on expression, increased censorship, and a fundamentally altered public discourse where the state, rather than the public, would largely determine acceptable speech.
% FOUNDING_PROBLEM: To prevent government censorship and ensure a robust public sphere where diverse ideas, including unpopular or controversial ones, can be freely debated, allowing for the discovery of truth and informed self-governance.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians, constitutional scholars, and political philosophers (outside the direct beneficiaries of free speech) corroborate the historical intent and ongoing relevance of protecting speech for epistemic and democratic functions, even while acknowledging contemporary challenges.
narrative_ontology:disappearance_verdict(speech_protection_kernel__marketplace_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__marketplace_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__marketplace_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(speech_protection_kernel__marketplace_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__marketplace_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__marketplace_reading_tests).
:- end_tests(speech_protection_kernel__marketplace_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` is low (0.15) because the constraint itself (speech protection) is not designed to extract resources from speakers, but to enable their participation. However, it imposes a cost on `targets_of_false_harmful_speech` by denying them remedies of suppression. `suppression` is high (0.75) because this reading actively suppresses content-based restrictions on speech, requiring significant enforcement to maintain. `theater_ratio` is moderate (0.40) as the ideal of truth emerging from free exchange often faces practical challenges, leading to some performative maintenance of the ideal despite its imperfect realization. `resistance` is substantial (0.60) from those who advocate for greater speech regulation, especially in the face of contemporary challenges like disinformation and online harassment. `accessibility_collapse` is moderate (0.60) as alternatives to 'more speech' as a remedy are significantly curtailed, but not entirely eliminated.
 *
 * PERSPECTIVAL GAP:
 *   The `speakers` and `truth_seekers` seats experience this constraint as a clear benefit, enabling their activities. `targets_of_false_harmful_speech`, however, experience it as a burden, as their harms are not directly remedied by the constraint's operation. `courts_and_legislatures` operate within this framework, balancing the ideal with practical realities, while `advocates_for_speech_restriction` are structurally excluded from the core premise of this reading, experiencing its enforcement as a form of suppression of their policy goals.
 *
 * DIRECTIONALITY LOGIC:
 *   `public_discourse`, `speakers`, and `truth_seekers` are beneficiaries, as the constraint directly enables and protects their activities. `targets_of_false_harmful_speech` are victims, as they bear the costs of harmful speech without the direct protection of content-based restrictions. `courts_and_legislatures` are agenda-setters, enforcing the framework. `advocates_for_speech_restriction` are excluded, as their preferred remedies are suppressed by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine ''marketplace of ideas'' reading, or does it implicitly incorporate elements of other readings (e.g., democratic participation) to justify its broad scope?',
    'Detailed textual analysis of judicial opinions and legislative history, comparing explicit justifications against the core tenets of the marketplace theory. If other justifications are consistently invoked, reclassify as a hybrid or a different reading.',
    'If it''s a purer marketplace reading, its classification as a Rope is more robust. If it''s a hybrid, its internal coherence and classification might shift towards a Tangled Rope due to conflicting justifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Clarifies the purity of the ''marketplace of ideas'' reading against other justifications for speech protection.').

omega_variable(
    efficacy_of_more_speech,
    'Does ''more speech'' effectively counter false and harmful speech in the contemporary digital information environment, or has its efficacy diminished?',
    'Empirical studies on the spread of disinformation, the impact of counter-speech, and the formation of echo chambers. If ''more speech'' is demonstrably ineffective, the core empirical premise of this reading is challenged.',
    'If ''more speech'' is found to be ineffective, the `extractiveness` (cost to victims) would be re-evaluated upward, and the `claimed_type` might shift towards a Snare or Tangled Rope, as the coordination function (truth-discovery) would be failing while costs persist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_of_more_speech, empirical, 'Assesses the empirical validity of the ''more speech'' remedy in countering falsehoods.').

omega_variable(
    uncompensated_harm_externality,
    'Is the harm caused to targets of false/harmful speech an uncompensated externality of the marketplace of ideas, or is it a necessary, acceptable cost for the collective good of truth-discovery?',
    'Sociological and ethical analysis of the impact of speech on vulnerable groups, and a normative judgment on the acceptable trade-offs between free expression and harm prevention. This is a preference-based judgment.',
    'If the harm is deemed an unacceptable externality, the `extractiveness` of the constraint (from the victims'' perspective) would be considered higher, potentially shifting the classification towards a Snare or Tangled Rope, highlighting the asymmetric burden.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(uncompensated_harm_externality, preference, 'Evaluates the normative acceptability of harm to victims as a consequence of broad speech protection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__marketplace_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1950, speech_protection_kernel__marketplace_reading, theater_ratio, 1950, 0.3).
narrative_ontology:measurement(spee_tr_t1970, speech_protection_kernel__marketplace_reading, theater_ratio, 1970, 0.32).
narrative_ontology:measurement(spee_tr_t1990, speech_protection_kernel__marketplace_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(spee_tr_t2010, speech_protection_kernel__marketplace_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(spee_tr_t2024, speech_protection_kernel__marketplace_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(spee_be_t1950, speech_protection_kernel__marketplace_reading, base_extractiveness, 1950, 0.1).
narrative_ontology:measurement(spee_be_t1970, speech_protection_kernel__marketplace_reading, base_extractiveness, 1970, 0.12).
narrative_ontology:measurement(spee_be_t1990, speech_protection_kernel__marketplace_reading, base_extractiveness, 1990, 0.13).
narrative_ontology:measurement(spee_be_t2010, speech_protection_kernel__marketplace_reading, base_extractiveness, 2010, 0.14).
narrative_ontology:measurement(spee_be_t2024, speech_protection_kernel__marketplace_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1950, speech_protection_kernel__marketplace_reading, suppression_requirement, 1950, 0.65).
narrative_ontology:measurement(spee_su_t1970, speech_protection_kernel__marketplace_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(spee_su_t1990, speech_protection_kernel__marketplace_reading, suppression_requirement, 1990, 0.72).
narrative_ontology:measurement(spee_su_t2010, speech_protection_kernel__marketplace_reading, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement(spee_su_t2024, speech_protection_kernel__marketplace_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__marketplace_reading, information_standard).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__democratic_participation_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__harm_threshold_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
