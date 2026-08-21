% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__icrc_customary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__icrc_customary_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: common_article_3_scope__icrc_customary_reading
 *   human_readable: ICRC Customary Law Reading of Common Article 3 Scope
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   This constraint describes the interpretation of Common Article 3 (CA3) of
 *   the Geneva Conventions through the lens of evolving customary
 *   international law, as primarily articulated by the ICRC. This reading
 *   posits that CA3's scope is not static but expands or clarifies based on
 *   consistent state practice and opinio juris (a sense of legal obligation).
 *   It functions as a procedural mechanism for adapting IHL to contemporary
 *   non-international armed conflicts, allowing for gradual expansion of
 *   protections without formal treaty amendment.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__icrc_customary_reading, 0.55).
domain_priors:suppression_score(common_article_3_scope__icrc_customary_reading, 0.45).
domain_priors:theater_ratio(common_article_3_scope__icrc_customary_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__icrc_customary_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__icrc_customary_reading, "ICRC Customary Law Reading of Common Article 3 Scope").
narrative_ontology:topic_domain(common_article_3_scope__icrc_customary_reading, "international_humanitarian_law").

domain_priors:requires_active_enforcement(common_article_3_scope__icrc_customary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__icrc_customary_reading, 'd850095f-6c4e-4251-bef2-702c36888416').
narrative_ontology:cs_kernel_codification('d850095f-6c4e-4251-bef2-702c36888416', formalized).
narrative_ontology:cs_authority_grounding('d850095f-6c4e-4251-bef2-702c36888416', expertise).
narrative_ontology:cs_interpretation_layer_present('d850095f-6c4e-4251-bef2-702c36888416').
narrative_ontology:cs_reading_relation('d850095f-6c4e-4251-bef2-702c36888416', common_article_3_scope__state_centric_reading, influences).
narrative_ontology:cs_reading_relation('d850095f-6c4e-4251-bef2-702c36888416', common_article_3_scope__expansive_human_rights_reading, coexists_with).
narrative_ontology:cs_axiom('d850095f-6c4e-4251-bef2-702c36888416', foundational, customary_law_as_dynamic_source_of_ihl).
narrative_ontology:cs_axiom_status(customary_law_as_dynamic_source_of_ihl, holdable).
narrative_ontology:cs_axiom_grounding('d850095f-6c4e-4251-bef2-702c36888416', customary_law_as_dynamic_source_of_ihl, conventional).
narrative_ontology:cs_axiom('d850095f-6c4e-4251-bef2-702c36888416', foundational, state_practice_and_opinio_juris_as_determinative).
narrative_ontology:cs_axiom_status(state_practice_and_opinio_juris_as_determinative, holdable).
narrative_ontology:cs_axiom_grounding('d850095f-6c4e-4251-bef2-702c36888416', state_practice_and_opinio_juris_as_determinative, conventional).
narrative_ontology:cs_reference_frame('d850095f-6c4e-4251-bef2-702c36888416', evolving_ihl_through_state_consensus).
narrative_ontology:cs_drift_state('d850095f-6c4e-4251-bef2-702c36888416', contemporary_ihl_challenges, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d850095f-6c4e-4251-bef2-702c36888416', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__icrc_customary_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, victims_of_non_international_armed_conflict).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, humanitarian_organizations).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, states_resisting_broader_ihl_application).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, states_parties_to_geneva_conventions).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, state_centric_legal_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The International Committee of the Red Cross (ICRC) actively researches, publishes, and advocates for the interpretation of Common Article 3 (CA3) through customary international law, identifying state practice and opinio juris to define its evolving scope. They are a primary driver of this reading.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, icrc, agenda_setter,
    institutional, generational, analytical, global).

% States are bound by CA3 and customary IHL. Their actions, military manuals, and declarations form the 'practice' and 'opinio juris' that define customary law. Some states actively resist broader interpretations, finding their sovereign actions constrained, while others accept or even promote them.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, states_parties_to_geneva_conventions, payer,
    institutional, generational, constrained, global).

% Individuals caught in non-international armed conflicts directly benefit from any expansion of CA3's protective scope, as it establishes minimum humanitarian standards for their treatment, regardless of the formal classification of the conflict.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, victims_of_non_international_armed_conflict, beneficiary,
    powerless, immediate, trapped, local).

% Organizations providing aid and protection in conflict zones rely on clear and expansive IHL to guide their operations, advocate for civilian protection, and hold parties to conflict accountable. They benefit from a dynamic interpretation that adapts to evolving conflict realities.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, humanitarian_organizations, beneficiary,
    organized, biographical, constrained, global).

% Academics and legal advisors who advocate for a strict, state-consent-based interpretation of IHL, often resisting the expansion of CA3's scope through customary law. They bear the intellectual cost of maintaining a narrower interpretation against the evolving customary trend.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, state_centric_legal_scholars, payer,
    analytical, biographical, analytical, global).

% Advocates who push for CA3 to be interpreted as a universal floor of human rights in all armed violence, often finding the customary law approach too slow, conservative, or reliant on state will. While their goals align with broader protection, their preferred method (direct human rights application) is often sidelined by the customary law process.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, expansive_human_rights_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_article_3_scope__icrc_customary_reading, diffuse).
narrative_ontology:fixing_cost_class(common_article_3_scope__icrc_customary_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a dynamic mechanism for states to collectively evolve the scope of minimum humanitarian protections in non-international armed conflicts, allowing IHL to adapt to changing conflict realities without requiring formal treaty renegotiation.
% TRANSFER_FUNCTION: Transfers interpretive authority and obligation from strict, static state consent to a dynamic process of identifying state practice and opinio juris, leading to broader protective duties for states and increased protection for individuals in non-international armed conflicts.
% ABSENT_VOICES: Non-state armed groups, who are often direct parties to non-international armed conflicts and subject to CA3, are not directly involved in the state practice and opinio juris that defines its scope, yet are bound by its evolving interpretation. They would argue for direct inclusion in norm-setting or for more explicit recognition of their obligations and rights.
% DISAPPEARANCE_RATIONALE: If the customary international law mechanism for determining CA3's scope vanished, states would likely revert to stricter, more conservative interpretations, leaving many victims of non-international armed conflicts without clear protections and humanitarian organizations without a robust, adaptable legal framework for advocacy. The legal landscape of internal conflicts would become significantly more fragmented and less protective.
% FOUNDING_PROBLEM: The Geneva Conventions' distinction between international and non-international armed conflicts left a significant gap in protection for victims of internal conflicts, which became increasingly prevalent and brutal. CA3 was a minimal attempt to address this, but its initial scope was narrow and required a mechanism for adaptation to remain relevant.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC, UN bodies, and numerous human rights organizations consistently attest that internal conflicts remain a major source of human suffering, and the need for robust, adaptable IHL (including CA3) is as pressing as ever. States, through their participation in international forums and their own military manuals, also implicitly acknowledge the ongoing relevance of CA3, even if they dispute its precise scope.
narrative_ontology:disappearance_verdict(common_article_3_scope__icrc_customary_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__icrc_customary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__icrc_customary_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(common_article_3_scope__icrc_customary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__icrc_customary_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__icrc_customary_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_article_3_scope__icrc_customary_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_article_3_scope__icrc_customary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it serves a genuine coordination function (adapting IHL to new conflict realities and expanding protections) but also involves asymmetric extraction. States that resist broader interpretations find themselves 'extracted' into new obligations through the customary law process, even if they haven't explicitly consented to every nuance. Extractiveness (0.55) is moderate-high because the process can be slow and costly for those seeking broader protections, and states often resist. Suppression (0.45) reflects the active advocacy and monitoring required by bodies like the ICRC to identify and promote customary norms against state resistance. Theater ratio (0.35) acknowledges that while states make declarations and issue military manuals, actual compliance can lag, creating a performative aspect to norm-setting.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of victims and humanitarian organizations, this reading is a vital, albeit slow, mechanism for expanding necessary protections. From the perspective of states resisting broader IHL application, it can be seen as an overreach by non-state actors (like the ICRC) or an erosion of state sovereignty, imposing obligations without explicit consent. The engine will compute these divergent classifications based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   The ICRC, victims of NIAC, and humanitarian organizations are beneficiaries, as this reading aims to expand protections and provide a framework for their work. States resisting broader IHL application are the primary targets/payers, as they bear the cost of increased obligations and potential constraints on their military actions. State-centric legal scholars also bear a 'cost' in terms of intellectual effort to counter this expansive reading. The process itself, while coordinating, extracts compliance and adaptation from reluctant states.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    customary_law_identification_ambiguity,
    'How reliably and objectively can ''state practice'' and ''opinio juris'' be identified, especially when state declarations and actual conduct diverge?',
    'Development of more rigorous, transparent methodologies for assessing customary international law, potentially involving independent expert panels or judicial review of state practice.',
    'If identification is highly subjective or prone to political influence, the constraint''s legitimacy as a coordination mechanism is weakened, increasing its effective extractiveness for states that feel unfairly bound. If robust, it strengthens the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_identification_ambiguity, empirical, 'Ambiguity in the identification process of customary international law.').

omega_variable(
    state_resistance_impact,
    'To what extent does active state resistance or non-compliance dilute or slow down the expansion of CA3''s scope through customary law, despite ICRC advocacy?',
    'Longitudinal studies comparing ICRC''s identified customary norms with actual state compliance and military doctrine over time, across various conflict contexts.',
    'If state resistance significantly impedes the effective expansion, the constraint''s coordination function is undermined, and its effective extractiveness for victims (who remain unprotected) increases. If resistance is largely overcome, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_resistance_impact, empirical, 'The actual impact of state resistance on the evolution of customary IHL.').

omega_variable(
    scope_of_non_international_armed_conflict_ambiguity,
    'Is the underlying definition of ''non-international armed conflict'' sufficiently clear, or does its ambiguity create a persistent ''seam'' that states exploit to avoid CA3''s application, regardless of customary law evolution?',
    'Analysis of state declarations and judicial decisions on NIAC classification, particularly in cases where states deny the existence of an NIAC despite widespread violence.',
    'If the NIAC definition remains fundamentally ambiguous, states can bypass CA3''s application, making the customary law mechanism less effective and increasing the effective extractiveness for victims. If clarified, the constraint''s protective function is enhanced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scope_of_non_international_armed_conflict_ambiguity, conceptual, 'The foundational ambiguity in defining non-international armed conflict.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__icrc_customary_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1949, common_article_3_scope__icrc_customary_reading, theater_ratio, 1949, 0.1).
narrative_ontology:measurement(comm_tr_t1964, common_article_3_scope__icrc_customary_reading, theater_ratio, 1964, 0.15).
narrative_ontology:measurement(comm_tr_t1979, common_article_3_scope__icrc_customary_reading, theater_ratio, 1979, 0.2).
narrative_ontology:measurement(comm_tr_t1994, common_article_3_scope__icrc_customary_reading, theater_ratio, 1994, 0.25).
narrative_ontology:measurement(comm_tr_t2009, common_article_3_scope__icrc_customary_reading, theater_ratio, 2009, 0.3).
narrative_ontology:measurement(comm_tr_t2024, common_article_3_scope__icrc_customary_reading, theater_ratio, 2024, 0.35).

% Extraction over time
narrative_ontology:measurement(comm_be_t1949, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1949, 0.3).
narrative_ontology:measurement(comm_be_t1964, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1964, 0.35).
narrative_ontology:measurement(comm_be_t1979, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1979, 0.4).
narrative_ontology:measurement(comm_be_t1994, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1994, 0.45).
narrative_ontology:measurement(comm_be_t2009, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2009, 0.5).
narrative_ontology:measurement(comm_be_t2024, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2024, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1949, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1949, 0.2).
narrative_ontology:measurement(comm_su_t1964, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1964, 0.25).
narrative_ontology:measurement(comm_su_t1979, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1979, 0.3).
narrative_ontology:measurement(comm_su_t1994, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1994, 0.35).
narrative_ontology:measurement(comm_su_t2009, common_article_3_scope__icrc_customary_reading, suppression_requirement, 2009, 0.4).
narrative_ontology:measurement(comm_su_t2024, common_article_3_scope__icrc_customary_reading, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__icrc_customary_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, ihl_principle_of_distinction).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, ihl_principle_of_proportionality).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, state_centric_reading).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, expansive_human_rights_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'common_article_3_scope' kernel, each representing a distinct structural interpretation of CA3's applicability. This reading focuses on customary international law as the mechanism for scope determination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
