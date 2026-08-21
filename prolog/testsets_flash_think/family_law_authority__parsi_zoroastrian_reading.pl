% ============================================================================
% CONSTRAINT STORY: family_law_authority__parsi_zoroastrian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__parsi_zoroastrian_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: family_law_authority__parsi_zoroastrian_reading
 *   human_readable: Marriage under Parsi Zoroastrian Religious Law
 *   domain: comparative_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint describes the Parsi Zoroastrian religious law governing
 *   marriage, which emphasizes endogamy (marriage within the community) as a
 *   core mechanism for preserving the distinct cultural and religious
 *   identity of a small, historically persecuted minority. The law is
 *   enforced through religious authority and strong social norms, with
 *   significant consequences for individuals who marry outside the community.
 *   This story is a specific reading of the broader 'family_law_authority'
 *   kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__parsi_zoroastrian_reading, 0.65).
domain_priors:suppression_score(family_law_authority__parsi_zoroastrian_reading, 0.75).
domain_priors:theater_ratio(family_law_authority__parsi_zoroastrian_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__parsi_zoroastrian_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__parsi_zoroastrian_reading, "Marriage under Parsi Zoroastrian Religious Law").
narrative_ontology:topic_domain(family_law_authority__parsi_zoroastrian_reading, "comparative_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__parsi_zoroastrian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__parsi_zoroastrian_reading, '429f0bc7-5de7-4b1c-9543-47e2a3540994').
narrative_ontology:cs_kernel_codification('429f0bc7-5de7-4b1c-9543-47e2a3540994', formalized).
narrative_ontology:cs_authority_grounding('429f0bc7-5de7-4b1c-9543-47e2a3540994', lineage).
narrative_ontology:cs_interpretation_layer_present('429f0bc7-5de7-4b1c-9543-47e2a3540994').
narrative_ontology:cs_reading_relation('429f0bc7-5de7-4b1c-9543-47e2a3540994', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('429f0bc7-5de7-4b1c-9543-47e2a3540994', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('429f0bc7-5de7-4b1c-9543-47e2a3540994', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('429f0bc7-5de7-4b1c-9543-47e2a3540994', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('429f0bc7-5de7-4b1c-9543-47e2a3540994', foundational, community_survival_through_endogamy).
narrative_ontology:cs_axiom_status(community_survival_through_endogamy, holdable).
narrative_ontology:cs_axiom_grounding('429f0bc7-5de7-4b1c-9543-47e2a3540994', community_survival_through_endogamy, conventional).
narrative_ontology:cs_axiom('429f0bc7-5de7-4b1c-9543-47e2a3540994', secondary, religious_purity_through_ritual_validity).
narrative_ontology:cs_axiom_status(religious_purity_through_ritual_validity, holdable).
narrative_ontology:cs_axiom_grounding('429f0bc7-5de7-4b1c-9543-47e2a3540994', religious_purity_through_ritual_validity, theological).
narrative_ontology:cs_reference_frame('429f0bc7-5de7-4b1c-9543-47e2a3540994', traditional_community_endogamy).
narrative_ontology:cs_drift_state('429f0bc7-5de7-4b1c-9543-47e2a3540994', contemporary_secular_pressures, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('429f0bc7-5de7-4b1c-9543-47e2a3540994', '').
narrative_ontology:cs_kernel_id(family_law_authority__parsi_zoroastrian_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, parsi_zoroastrian_community).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, zoroastrian_priesthood).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, parsi_zoroastrian_individuals_seeking_exogamy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, parsi_zoroastrian_individuals_adhering_to_endogamy).
narrative_ontology:constraint_vindicates(family_law_authority__parsi_zoroastrian_reading, community_preservation_doctrine).
narrative_ontology:constraint_vindicates(family_law_authority__parsi_zoroastrian_reading, religious_identity_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces Zoroastrian religious law regarding marriage, including the endogamy requirement. They perform rituals and adjudicate disputes, ensuring adherence to tradition for community preservation.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, zoroastrian_priesthood, agenda_setter,
    institutional, generational, constrained, regional).

% Benefits from the preservation of its distinct cultural and religious identity through regulated marriage practices. It also acts as a collective enforcer of social norms, applying pressure on members to conform.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_zoroastrian_community, beneficiary,
    organized, generational, identity_locked, local).

% Bears the social and religious costs of marrying outside the community, which can include ostracization, loss of community status, and exclusion from religious rites. Their identity is deeply tied to community membership.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_zoroastrian_individuals_seeking_exogamy, payer,
    powerless, biographical, identity_locked, local).

% Benefits from the strong social cohesion, shared cultural values, and religious continuity fostered by endogamous marriage. Their adherence reinforces their identity and standing within the community.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_zoroastrian_individuals_adhering_to_endogamy, beneficiary,
    moderate, biographical, identity_locked, local).

% Operates parallel to religious law, offering civil marriage options. While not directly enforcing religious endogamy, it can provide an alternative legal framework that may conflict with religious strictures, creating tension for individuals.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, secular_legal_systems, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the distinct cultural, religious, and genetic identity of the small Parsi Zoroastrian community by regulating marriage to ensure endogamy and ritual validity.
% TRANSFER_FUNCTION: Transfers social status, religious legitimacy, and community belonging to those who adhere to endogamous marriage, and withdraws these from individuals who marry outside the community.
% ABSENT_VOICES: Individuals who have married outside the community and faced social exclusion, or those within the community who desire exogamous marriage but fear the repercussions. They would advocate for individual autonomy and religious freedom within a more inclusive framework.
% DISAPPEARANCE_RATIONALE: If the religious marriage laws and their enforcement vanished overnight, the Parsi Zoroastrian community, being a small and endogamous group, would likely face rapid assimilation into larger populations, leading to a significant and irreversible loss of its distinct identity and social structure.
% FOUNDING_PROBLEM: The existential challenge of preserving a distinct religious and cultural identity for a minority community facing assimilation pressures and demographic decline.
% FOUNDING_PROBLEM_CORROBORATION: Community elders and religious scholars consistently attest to the ongoing challenge of preserving Parsi Zoroastrian identity. Demographic studies and historical accounts from independent researchers corroborate the historical and ongoing pressures on minority religious groups to maintain distinct identities, supporting the claim that the founding problem remains live.
narrative_ontology:disappearance_verdict(family_law_authority__parsi_zoroastrian_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__parsi_zoroastrian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__parsi_zoroastrian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(family_law_authority__parsi_zoroastrian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__parsi_zoroastrian_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__parsi_zoroastrian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__parsi_zoroastrian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__parsi_zoroastrian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is moderate-high because while the community benefits from preservation, individuals seeking exogamy face substantial social and religious costs. Suppression (0.75) is high due to the intense social pressure and religious authority that enforce endogamy. The theater ratio (0.40) is moderate; while rituals and community events perform identity, the underlying function of community preservation remains active and consequential. Accessibility collapse (0.70) is high for individuals within the community who wish to marry outside, as alternatives are severely constrained by social and religious structures. Resistance (0.30) is low due to the tight-knit nature of the community and the high cost of overt defiance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Zoroastrian priesthood and the Parsi Zoroastrian community, the marriage law is a vital 'rope' for identity coordination and survival. However, from the perspective of individuals who wish to marry outside the community, the same structure operates as a 'snare' due to the high personal costs and limited exit options. The engine's computation of per-seat classifications will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Zoroastrian priesthood and the Parsi Zoroastrian community as a whole are the primary beneficiaries, as the constraint directly supports their mandate of community preservation and continuity. Individuals adhering to endogamy also benefit from reinforced identity and social standing. Individuals seeking exogamy are the primary targets, bearing the costs of social exclusion and loss of religious legitimacy. Secular legal systems act as observers, providing an alternative framework but not directly participating in the religious enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (community preservation) is still very much 'live' given the Parsi Zoroastrian community's minority status and historical pressures. However, the methods of achieving this (strict endogamy) are increasingly contested by modern values of individual autonomy and secular legal frameworks. This contestation contributes to the high extractiveness, as the constraint must actively suppress alternatives to maintain its function, preventing it from degrading into a piton despite the internal resistance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endogamy_necessity_for_survival,
    'Is strict endogamy truly a necessary condition for the long-term survival and distinct identity of the Parsi Zoroastrian community, or are there alternative, less extractive means of community preservation?',
    'Comparative sociological studies of other minority religious groups that have maintained identity with more flexible marriage norms, or internal community initiatives exploring alternative integration strategies.',
    'If endogamy is found not to be strictly necessary, the constraint''s extractiveness could be re-evaluated as higher (more gratuitous), and its classification might shift closer to a pure snare. If it is found necessary, the coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogamy_necessity_for_survival, empirical, 'Whether endogamy is an indispensable mechanism for Parsi Zoroastrian community survival.').

omega_variable(
    internalized_vs_structural_suppression,
    'To what extent is the measured suppression (0.75) a result of external community pressure and religious authority, versus internalized identity and cultural loyalty that makes deviation unthinkable for individuals?',
    'Qualitative sociological research, including interviews with individuals who have considered or undertaken exogamous marriage, to differentiate between perceived external barriers and internal psychological/identity constraints.',
    'If suppression is largely internalized, the effective suppression for individuals is higher and more persistent, as it travels with them even if external pressures lessen. This would amplify the ''snare'' aspect of the constraint for affected individuals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Structural vs. internalized suppression mechanism in Parsi Zoroastrian marriage norms.').

omega_variable(
    secular_law_challenge_impact,
    'How would the constraint''s operation and classification change if secular legal systems actively challenged the religious authority over marriage, particularly regarding the social and legal consequences of exogamy?',
    'Analysis of legal precedents and outcomes in jurisdictions where secular courts have intervened in religious family law, or hypothetical modeling of such interventions within the Indian legal context.',
    'Active secular challenge could reduce the constraint''s effective suppression and extractiveness by providing viable, less costly exit options for individuals, potentially shifting its classification towards a more benign form or even dismantling its coercive aspects.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(secular_law_challenge_impact, conceptual, 'Impact of secular legal intervention on religious marriage authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__parsi_zoroastrian_reading, 1950, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t1950, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 1950, 0.3).
narrative_ontology:measurement(fami_tr_t1960, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 1960, 0.32).
narrative_ontology:measurement(fami_tr_t1970, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 1970, 0.34).
narrative_ontology:measurement(fami_tr_t1980, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 1980, 0.36).
narrative_ontology:measurement(fami_tr_t1990, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 1990, 0.38).
narrative_ontology:measurement(fami_tr_t2000, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 2000, 0.39).
narrative_ontology:measurement(fami_tr_t2010, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(fami_tr_t2020, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(fami_be_t1950, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 1950, 0.55).
narrative_ontology:measurement(fami_be_t1960, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 1960, 0.58).
narrative_ontology:measurement(fami_be_t1970, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(fami_be_t1980, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 1980, 0.62).
narrative_ontology:measurement(fami_be_t1990, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 1990, 0.63).
narrative_ontology:measurement(fami_be_t2000, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 2000, 0.64).
narrative_ontology:measurement(fami_be_t2010, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(fami_be_t2020, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t1950, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 1950, 0.65).
narrative_ontology:measurement(fami_su_t1960, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 1960, 0.68).
narrative_ontology:measurement(fami_su_t1970, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(fami_su_t1980, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 1980, 0.72).
narrative_ontology:measurement(fami_su_t1990, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 1990, 0.73).
narrative_ontology:measurement(fami_su_t2000, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 2000, 0.74).
narrative_ontology:measurement(fami_su_t2010, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(fami_su_t2020, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__parsi_zoroastrian_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
