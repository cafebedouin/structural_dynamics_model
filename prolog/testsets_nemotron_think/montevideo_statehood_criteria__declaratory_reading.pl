% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__declaratory_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__declaratory_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: montevideo_statehood_criteria__declaratory_reading
 *   human_readable: Montevideo Criteria as Self-Executing Statehood Rule (Declaratory Reading)
 *   domain: international_law/political_philosophy/state_theory
 *
 * SUMMARY:
 *   The Montevideo Convention (1933) Article 1 sets four objective criteria
 *   for statehood: permanent population, defined territory, government, and
 *   capacity to enter relations. The declaratory reading holds that meeting
 *   these criteria establishes statehood as a legal fact automatically —
 *   recognition by other states is merely declaratory, not constitutive. This
 *   reading claims Mountain status: the rule is a self-executing legal fact
 *   like a natural law. However, the authored metrics describe a constraint
 *   that has become substantially extractive over time. Powerful states
 *   withhold recognition from entities that meet the criteria (Somaliland,
 *   Taiwan, Palestine at various times), creating a gap between legal
 *   entitlement and political reality. The extraction falls on aspirant
 *   entities denied the benefits of statehood (treaty capacity, international
 *   organization membership, diplomatic protection) while existing states
 *   retain structural leverage to condition recognition. The theater ratio
 *   rises as the legal community reiterates the declaratory rule while
 *   practice diverges. This is a False Summit Mountain candidate: the claim
 *   is Mountain, but beneficiaries (aspirant entities) and victims (those
 *   denied recognition despite meeting criteria) exist, triggering FSM
 *   evaluation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__declaratory_reading, 0.68).
domain_priors:suppression_score(montevideo_statehood_criteria__declaratory_reading, 0.72).
domain_priors:theater_ratio(montevideo_statehood_criteria__declaratory_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__declaratory_reading, mountain).
narrative_ontology:human_readable(montevideo_statehood_criteria__declaratory_reading, "Montevideo Criteria as Self-Executing Statehood Rule (Declaratory Reading)").
narrative_ontology:topic_domain(montevideo_statehood_criteria__declaratory_reading, "international_law/political_philosophy/state_theory").

domain_priors:emerges_naturally(montevideo_statehood_criteria__declaratory_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__declaratory_reading, 'f6f208cf-f025-49d1-92a4-b669db70a08d').
narrative_ontology:cs_kernel_codification('f6f208cf-f025-49d1-92a4-b669db70a08d', formalized).
narrative_ontology:cs_authority_grounding('f6f208cf-f025-49d1-92a4-b669db70a08d', lineage).
narrative_ontology:cs_interpretation_layer_present('f6f208cf-f025-49d1-92a4-b669db70a08d').
narrative_ontology:cs_reading_relation('f6f208cf-f025-49d1-92a4-b669db70a08d', montevideo_statehood_criteria__constitutive_reading, forecloses).
narrative_ontology:cs_reading_relation('f6f208cf-f025-49d1-92a4-b669db70a08d', montevideo_statehood_criteria__hybrid_reading, influences).
narrative_ontology:cs_axiom('f6f208cf-f025-49d1-92a4-b669db70a08d', foundational, statehood_is_legal_fact_not_political_act).
narrative_ontology:cs_axiom_status(statehood_is_legal_fact_not_political_act, holdable).
narrative_ontology:cs_axiom_grounding('f6f208cf-f025-49d1-92a4-b669db70a08d', statehood_is_legal_fact_not_political_act, conventional).
narrative_ontology:cs_axiom('f6f208cf-f025-49d1-92a4-b669db70a08d', foundational, recognition_is_declaratory_not_constitutive).
narrative_ontology:cs_axiom_status(recognition_is_declaratory_not_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('f6f208cf-f025-49d1-92a4-b669db70a08d', recognition_is_declaratory_not_constitutive, conventional).
narrative_ontology:cs_reference_frame('f6f208cf-f025-49d1-92a4-b669db70a08d', montevideo_convention_1933_article_1).
narrative_ontology:cs_drift_state('f6f208cf-f025-49d1-92a4-b669db70a08d', contemporary_recognition_politics, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f6f208cf-f025-49d1-92a4-b669db70a08d', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, aspirant_entities_meeting_criteria).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, aspirant_entities_denied_recognition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, parent_states_of_secessionist_entities).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__declaratory_reading, montevideo_convention_article_1).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__declaratory_reading, legal_positivism_statehood).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__declaratory_reading, self_executing_treaty_obligations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Entities that satisfy all four Montevideo criteria (permanent population, defined territory, effective government, capacity for relations). Under the declaratory reading, they automatically possess statehood and its legal incidents. They benefit from the rule's clarity and objectivity. Their exit is constrained: they cannot 'exit' the international system but can seek recognition through diplomatic campaigns or alter criteria compliance.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, aspirant_entities_meeting_criteria, beneficiary,
    organized, generational, constrained, regional).

% Entities that meet Montevideo criteria but are systematically denied recognition by powerful states (e.g., Somaliland since 1991, Taiwan since 1971, Palestine pre-2012). They bear the full cost of the recognition gap: no treaty capacity, no UN membership, no IMF/World Bank access, no diplomatic protection for nationals, no standing in ICJ. Their exit is trapped — they cannot unilaterally obtain recognition, and the criteria they already meet provide no remedy.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, aspirant_entities_denied_recognition, payer,
    powerless, generational, trapped, regional).

% Established states that control the recognition calculus. They set the agenda by deciding when/whom to recognize, using recognition as a policy tool (e.g., recognizing Palestine as leverage, withholding from Taiwan to maintain China relations). They benefit from the gap between declaratory law and political practice — the rule provides cover while discretion is retained. Their exit is arbitrage-grade: they can recognize or not based on interest, with minimal cost.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, existing_recognizing_states, agenda_setter,
    institutional, generational, arbitrage, global).

% ICJ, ILC, international law scholars, treaty bodies. They interpret and apply the declaratory rule, issue opinions affirming its self-executing character, but lack enforcement power. Their situation is analytical: they observe the gap between rule and practice, articulate the declaratory position, but cannot compel recognition. Exit is analytical — they can change their interpretation but not the political facts.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, international_legal_community, observer,
    analytical, civilizational, analytical, universal).

% People living under de facto authorities that meet Montevideo criteria but lack recognition. They bear human costs: no consular protection abroad, no access to human rights treaty bodies, no refugee convention protections, limited international mobility, excluded from global health/education frameworks. They would object to the recognition denial if they had voice, but are structurally excluded from the recognition calculus. Their exit is trapped — they cannot change their entity's recognition status individually.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, populations_in_unrecognized_entities, excluded,
    powerless, biographical, trapped, local).

% States from which aspirant entities have seceded or declared independence (e.g., Somalia/Somaliland, Serbia/Kosovo, Georgia/Abkhazia). They actively oppose recognition of the breakaway entity, using diplomatic and economic pressure to maintain the recognition gap. They benefit from the declaratory rule's non-enforcement — their territorial integrity claims are politically sustained despite the entity meeting criteria. Their exit is mobile: they can adjust recognition policy as geopolitical interests shift.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, parent_states_of_secessionist_entities, agenda_setter,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__declaratory_reading, parent_states_of_secessionist_entities, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the international community on statehood entitlement without requiring case-by-case political consensus: the four criteria provide an objective, verifiable standard that any actor can apply, replacing the prior practice of ad hoc recognition decisions by great powers.
% TRANSFER_FUNCTION: Moves the authority to confer statehood from political discretion of existing states to legal criteria verifiable by any observer. Transfers the benefits of statehood (legal personality, treaty capacity, international standing) from the gift of recognizers to the automatic entitlement of qualifying entities.
% ABSENT_VOICES: Populations in unrecognized entities (excluded stakeholders) — they bear the human costs of non-recognition but have no seat at the recognition table. Non-state actors (corporations, NGOs, insurgent groups) who would engage with unrecognized entities but are deterred by legal uncertainty. Future aspirant entities not yet formed — the recognition regime shapes their calculi but they cannot participate in its design.
% DISAPPEARANCE_RATIONALE: If the declaratory rule vanished overnight, statehood would revert to purely constitutive recognition — every aspirant entity would need explicit political recognition from a critical mass of states. The number of recognized states would likely decrease (some current states recognized only due to declaratory pressure), recognition would become more politicized, and secessionist entities would lose their primary legal argument. The international legal order would lose its objective anchor for statehood.
% FOUNDING_PROBLEM: The pre-1933 practice of arbitrary, great-power-driven recognition decisions that created legal uncertainty, enabled intervention, and denied statehood to politically disfavored entities. The Montevideo Convention was built to replace political discretion with an objective, verifiable standard.
% FOUNDING_PROBLEM_CORROBORATION: The ILC's 2011 Draft Articles on State Responsibility and 2018 conclusions on identification of customary law affirm the declaratory character of statehood criteria. ICJ opinions (Kosovo Advisory Opinion 2010, Chagos Advisory Opinion 2019) treat Montevideo criteria as reflective of customary law. However, state practice (non-recognition of Somaliland, Taiwan, delayed Palestine recognition) and scholarly critique (Crawford, Grant, Vidmar) attest that the founding problem of politicized recognition persists and the declaratory rule has not displaced it. Corroboration from outside the beneficiary set: ILC (expert body, not a would-be state), ICJ (judicial body), and state practice studies by non-beneficiary scholars.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__declaratory_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__declaratory_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__declaratory_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(montevideo_statehood_criteria__declaratory_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__declaratory_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__declaratory_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__declaratory_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, ExtMetricName, E),
    domain_priors:suppression_score(montevideo_statehood_criteria__declaratory_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(montevideo_statehood_criteria__declaratory_reading),
    narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(montevideo_statehood_criteria__declaratory_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) reflects the growing gap between legal entitlement and political delivery: more entities meet criteria but are denied recognition, and the value of statehood (access to international system) has increased. Suppression (0.72) is high because the constraint's persistence depends on powerful states actively withholding recognition — not on the rule itself but on the enforcement gap. Theater ratio (0.41) captures the performative citation of Montevideo by legal actors while recognition politics operates orthogonally. Accessibility collapse (0.78) is high because the declaratory rule presents itself as a closed logical operation (criteria met = statehood), yet alternatives (recognition politics) persist. Resistance (0.35) is moderate: aspirant entities resist through diplomatic campaigns, but the structural asymmetry favors recognizers. The measurement series shows extraction and suppression rising steadily since decolonization (1960) and accelerating post-Cold War (1991), as the number of aspirant entities grew and recognition became more politicized.
 *
 * PERSPECTIVAL GAP:
 *   From the international legal community's seat (observer, analytical), the constraint computes as Mountain — the rule is clear, self-executing, and legally settled. From the aspirant entity denied recognition (payer, constrained exit), it computes as Snare — the legal entitlement exists but delivers no benefits, and the constraint's persistence depends on powerful states' coercion. From existing states' seat (agenda_setter, arbitrage), it computes as Rope/Tangled Rope — the rule coordinates some expectations but they retain discretionary leverage. The engine computes this divergence from the structural data; the authored Mountain claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Aspirant entities meeting criteria are structural beneficiaries of the rule (d near 0.0 — the rule subsidizes them with automatic statehood). But aspirant entities denied recognition are structural victims (d near 1.0 — they bear the full cost of the recognition gap). Existing powerful states are agenda-setters with arbitrage exit (they choose when/whether to recognize). International legal community (ICJ, ILC, scholars) are observers with analytical exit. Populations in unrecognized entities are excluded payers (they bear human costs but have no voice). The directionality derivation from beneficiary/victim declarations + exit options captures this: the same legal rule produces opposite directionalities for entities that receive recognition vs. those that don't.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (arbitrary recognition, need for objective standard) is contested: decolonization largely solved the original problem for former colonies, but new aspirant entities (secessionist, breakaway) face a recognition regime that has re-politicized the criteria. The arrangement persists not because the founding problem is live, but because existing states benefit from discretionary recognition power. This is mandatrophy: the mandate (objective criteria) has outlived its coordinating function for the current victim set, but the constraint persists through institutional inertia and strategic withholding of recognition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    declaratory_vs_practice_gap,
    'Does the declaratory rule operate as a self-executing legal fact in practice, or does it require recognition compliance to take effect?',
    'Systematic survey of ICJ judgments, ILC conclusions, and state practice regarding entities that meet Montevideo criteria but lack recognition (e.g., Somaliland, Taiwan, Palestine pre-2012).',
    'If the rule is not self-executing in practice, the Mountain claim is a false summit — the constraint operates as a tangled rope where legal criteria coordinate but political recognition extracts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(declaratory_vs_practice_gap, empirical, 'Whether the declaratory rule''s legal force is operative or aspirational.').

omega_variable(
    kernel_reading_disagreement_location,
    'Is the disagreement between declaratory and constitutive readings located in the legal rule itself, or in the political practice that surrounds it?',
    'Comparative analysis of treaty text (Montevideo Art. 1 vs Art. 3), ICJ advisory opinions, and recognition doctrines across sibling readings.',
    'If the disagreement is in the rule, the readings foreclose each other; if in practice, they coexist as competing descriptions of a single contested reality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Structural location of the kernel disagreement between declaratory and constitutive readings.').

omega_variable(
    victim_set_composition,
    'Does the victim set under recognition denial include the de facto authorities themselves, the populations they govern, or both — and does this distinction change the constraint''s classification?',
    'Field studies of unrecognized entities: does the authorities'' lack of international legal personality extract from them directly (no treaty capacity, no IMF access) or primarily from the population (no consular protection, no human rights treaty bodies)?',
    'If authorities are primary victims, the constraint is a snare on would-be sovereigns; if populations are primary victims, it is a snare on peoples — different remedial implications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_composition, empirical, 'Composition of the victim set under the declaratory reading''s recognition gap.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of unrecognized entities structural (non-recognition by powerful states, UN exclusion) or internalized (entities conform to recognition-seeking behavior, populations accept statelessness)?',
    'Post-exit trajectory analysis: if an entity gains recognition, does suppression cease immediately (structural) or persist in institutional memory and practice (internalized)?',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression with them after exit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for unrecognized entities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__declaratory_reading, 1933, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t1933, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1933, 0.1).
narrative_ontology:measurement(mont_tr_t1945, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1945, 0.15).
narrative_ontology:measurement(mont_tr_t1960, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1960, 0.25).
narrative_ontology:measurement(mont_tr_t1991, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1991, 0.35).
narrative_ontology:measurement(mont_tr_t2000, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(mont_tr_t2012, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 2012, 0.4).
narrative_ontology:measurement(mont_tr_t2024, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 2024, 0.41).

% Extraction over time
narrative_ontology:measurement(mont_be_t1933, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1933, 0.25).
narrative_ontology:measurement(mont_be_t1945, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1945, 0.32).
narrative_ontology:measurement(mont_be_t1960, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1960, 0.45).
narrative_ontology:measurement(mont_be_t1991, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1991, 0.58).
narrative_ontology:measurement(mont_be_t2000, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 2000, 0.63).
narrative_ontology:measurement(mont_be_t2012, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 2012, 0.67).
narrative_ontology:measurement(mont_be_t2024, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t1933, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1933, 0.3).
narrative_ontology:measurement(mont_su_t1945, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1945, 0.45).
narrative_ontology:measurement(mont_su_t1960, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1960, 0.55).
narrative_ontology:measurement(mont_su_t1991, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1991, 0.65).
narrative_ontology:measurement(mont_su_t2000, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(mont_su_t2012, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 2012, 0.7).
narrative_ontology:measurement(mont_su_t2024, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__declaratory_reading, information_standard).
narrative_ontology:boltzmann_floor_override(montevideo_statehood_criteria__declaratory_reading, 0.02).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria__constitutive_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria__hybrid_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, un_membership_admission).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, diplomatic_recognition_practice).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, self_determination_peoples).

% DUAL FORMULATION NOTE:
% Montevideo kernel family: three readings decompose the single label 'Montevideo criteria' into structurally distinct constraints. Ehrenfest-like barrier: Montevideo Art. 1 text (fixed). Spectral universality: declaratory reading (criteria → statehood as legal fact, Mountain claim). Eigenvector thermalization: constitutive reading (recognition as constitutive, Snare/Tangled Rope in practice). Hybrid reading adds normative layer (contested, Tangled Rope). This story is the declaratory reading — the spectral universality claim of the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(montevideo_statehood_criteria__declaratory_reading, institutional, 0.15).
constraint_indexing:directionality_override(montevideo_statehood_criteria__declaratory_reading, organized, 0.75).
constraint_indexing:directionality_override(montevideo_statehood_criteria__declaratory_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
