% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__autonomy_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__autonomy_primacy_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: one_country_two_systems_framework__autonomy_primacy_reading
 *   human_readable: Hong Kong Autonomy under One Country, Two Systems (Autonomy Primacy Reading)
 *   domain: constitutional_law/political_systems/state_sovereignty
 *
 * SUMMARY:
 *   This constraint story instantiates the 'autonomy primacy' reading of the
 *   'One Country, Two Systems' framework for Hong Kong. This reading
 *   emphasizes that Hong Kong retains substantive autonomy, with meaningful
 *   checks on mainland interference, and that civil liberties and judicial
 *   independence are treaty-guaranteed and internationally enforceable.
 *   Mainland intervention, from this perspective, constitutes a treaty
 *   violation. The metrics reflect the state of the framework as it is
 *   *supposed* to operate under this reading, while the temporal measurements
 *   show the increasing pressure and erosion of these protections over time.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__autonomy_primacy_reading, 0.35).
domain_priors:suppression_score(one_country_two_systems_framework__autonomy_primacy_reading, 0.45).
domain_priors:theater_ratio(one_country_two_systems_framework__autonomy_primacy_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__autonomy_primacy_reading, rope).
narrative_ontology:human_readable(one_country_two_systems_framework__autonomy_primacy_reading, "Hong Kong Autonomy under One Country, Two Systems (Autonomy Primacy Reading)").
narrative_ontology:topic_domain(one_country_two_systems_framework__autonomy_primacy_reading, "constitutional_law/political_systems/state_sovereignty").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__autonomy_primacy_reading, '96d25d07-4464-4971-b674-62c93937edf6').
narrative_ontology:cs_kernel_codification('96d25d07-4464-4971-b674-62c93937edf6', fixed_text).
narrative_ontology:cs_authority_grounding('96d25d07-4464-4971-b674-62c93937edf6', lineage).
narrative_ontology:cs_interpretation_layer_present('96d25d07-4464-4971-b674-62c93937edf6').
narrative_ontology:cs_reading_relation('96d25d07-4464-4971-b674-62c93937edf6', one_country_two_systems_framework__sovereignty_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('96d25d07-4464-4971-b674-62c93937edf6', one_country_two_systems_framework__balanced_coexistence_reading, coexists_with).
narrative_ontology:cs_axiom('96d25d07-4464-4971-b674-62c93937edf6', foundational, hong_kong_high_autonomy_is_substantive).
narrative_ontology:cs_axiom_status(hong_kong_high_autonomy_is_substantive, holdable).
narrative_ontology:cs_axiom_grounding('96d25d07-4464-4971-b674-62c93937edf6', hong_kong_high_autonomy_is_substantive, deontological).
narrative_ontology:cs_axiom('96d25d07-4464-4971-b674-62c93937edf6', foundational, treaty_obligations_are_binding).
narrative_ontology:cs_axiom_status(treaty_obligations_are_binding, holdable).
narrative_ontology:cs_axiom_grounding('96d25d07-4464-4971-b674-62c93937edf6', treaty_obligations_are_binding, conventional).
narrative_ontology:cs_reference_frame('96d25d07-4464-4971-b674-62c93937edf6', post_handover_joint_declaration_framework).
narrative_ontology:cs_drift_state('96d25d07-4464-4971-b674-62c93937edf6', national_security_law_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('96d25d07-4464-4971-b674-62c93937edf6', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_residents).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, international_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, democratic_activists).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, prc_central_government).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_executive).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the preservation of civil liberties, rule of law, and distinct way of life. Their ability to exit the system (e.g., emigration) is constrained by practicalities, but they actively defend their autonomy within the framework.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_residents, beneficiary,
    organized, biographical, constrained, local).

% Acts as a primary check on executive power and mainland interference, upholding the Basic Law and common law principles. Its independence is central to this reading, but it faces increasing pressure, making its identity tied to its function.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_judiciary, agenda_setter,
    institutional, generational, identity_locked, local).

% Is structurally constrained by Hong Kong's autonomy, judicial independence, and treaty obligations. From this reading's perspective, the framework imposes limits on its sovereign authority over Hong Kong, which it must 'pay' by respecting.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, prc_central_government, payer,
    institutional, civilizational, constrained, national).

% Benefits from the stability and rule of law in Hong Kong, and from the PRC's adherence to international treaty obligations (Sino-British Joint Declaration). It observes and, at times, intervenes diplomatically or through sanctions to uphold the framework.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, international_community, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__autonomy_primacy_reading, international_community, observer).

% Operates under the constraints of judicial review and public accountability, as mandated by the autonomy framework. Its power is limited compared to a fully integrated mainland executive, which it 'pays' by adhering to local laws and international norms.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_executive, payer,
    powerful, biographical, constrained, local).

% Benefit from the framework's promise of a pathway to universal suffrage and the protection of freedoms of assembly and speech, which are essential for their advocacy. Their options are constrained by increasing legal and political risks.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, democratic_activists, beneficiary,
    moderate, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To manage the integration of Hong Kong into the People's Republic of China while preserving Hong Kong's distinct legal, economic, and social systems, ensuring stability and prosperity through a 'high degree of autonomy'.
% TRANSFER_FUNCTION: Transfers sovereign authority over Hong Kong to the PRC, while simultaneously transferring significant autonomy, civil liberties, and judicial independence to Hong Kong, guaranteed by international treaty.
% ABSENT_VOICES: Proponents of full Hong Kong independence or complete integration into mainland China are structurally excluded from the framework's core design, as it seeks a unique middle ground. Their perspectives are not accommodated by the 'One Country, Two Systems' principle itself.
% DISAPPEARANCE_RATIONALE: If the 'One Country, Two Systems' framework, as understood by this reading, vanished overnight, Hong Kong would either be fully absorbed into mainland China (losing its distinct systems and freedoms) or attempt full independence, leading to profound political, economic, and social upheaval. The entire regional and international political landscape would shift.
% FOUNDING_PROBLEM: To reconcile the transfer of sovereignty over Hong Kong from the United Kingdom to the People's Republic of China in 1997 with the need to maintain Hong Kong's capitalist system, common law, and civil liberties, which were seen as crucial for its prosperity and international standing.
% FOUNDING_PROBLEM_CORROBORATION: The UK-Sino Joint Declaration (an international treaty) corroborates the original intent of preserving Hong Kong's autonomy. However, the PRC central government asserts the problem is still live and its interpretation is paramount, while international legal scholars, human rights organizations, and many Hong Kong residents attest that the founding problem is being undermined by mainland actions, shifting the framework's function.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__autonomy_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__autonomy_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__autonomy_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(one_country_two_systems_framework__autonomy_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__autonomy_primacy_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__autonomy_primacy_reading_tests).
:- end_tests(one_country_two_systems_framework__autonomy_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.35) and suppression (0.45) are moderate, reflecting the inherent friction and occasional pressure points even in an 'autonomy-first' interpretation, but are not high because the framework's core function, by this reading, is protection. The theater ratio (0.55) is higher, indicating that the *performance* of autonomy and adherence to the framework increasingly masks a reality of eroding substance, especially towards the end of the interval. Resistance (0.60) is high due to active efforts by residents and international bodies to defend the autonomy. Accessibility collapse (0.40) is moderate, as alternatives (full independence or full integration) are difficult but not entirely unthinkable.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Hong Kong residents and the international community, the framework is a vital protection. From the PRC's perspective, this reading imposes undue constraints on its sovereignty. The engine's per-seat classification will highlight this divergence, showing the framework as a Rope for beneficiaries and a Payer for the PRC, even as the overall system shows signs of drift.
 *
 * DIRECTIONALITY LOGIC:
 *   Hong Kong residents and the international community are beneficiaries, as the framework protects their interests and upholds international law. The PRC Central Government and the Hong Kong Executive are 'payers' in this reading, as the framework constrains their power and requires adherence to limits on their authority. The Hong Kong Judiciary, as an agenda-setter, is identity-locked to its role of upholding the Basic Law, making its exit options constrained by its very function.
 *
 * MANDATROPHY ANALYSIS:
 *   The framework's mandate, by this reading, is to preserve Hong Kong's distinct systems. The increasing theater ratio and rising extractiveness/suppression over time suggest a drift towards mandatrophy, where the original protective function is increasingly undermined by external pressures, even as the formal structure remains. The 'contested' status of the founding problem further highlights this tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substantive_vs_delegated_autonomy,
    'Is Hong Kong''s ''high degree of autonomy'' an inherent, treaty-guaranteed right, or a delegated power revocable by the PRC Central Government?',
    'International legal arbitration or a definitive ruling by a universally recognized international court on the interpretation of the Sino-British Joint Declaration and the Basic Law.',
    'If inherent, mainland interventions are clear violations, strengthening the ''rope'' classification. If delegated, the framework''s protective function is weaker, potentially reclassifying it towards a ''tangled_rope'' or ''snare'' from the perspective of Hong Kong residents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substantive_vs_delegated_autonomy, conceptual, 'Ambiguity regarding the source and nature of Hong Kong''s autonomy.').

omega_variable(
    enforceability_of_international_guarantees,
    'To what extent are the international guarantees of Hong Kong''s autonomy and civil liberties actually enforceable against a sovereign state like the PRC?',
    'Observation of the effectiveness of international diplomatic pressure, sanctions, and legal challenges in reversing or preventing mainland interventions over time.',
    'If international enforcement proves ineffective, the ''rope'' classification for beneficiaries weakens significantly, as the promised protections lack teeth, potentially shifting towards a ''snare'' due to unmitigated extraction. If effective, the ''rope'' classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforceability_of_international_guarantees, empirical, 'The practical limits of international law in constraining a powerful state.').

omega_variable(
    democratic_reform_pathway_viability,
    'Does a genuine and viable pathway to universal suffrage and democratic reform for Hong Kong still exist under this framework, or has it been foreclosed by mainland actions?',
    'Observation of legislative changes, electoral reforms, and the actual implementation of universal suffrage over a multi-generational timeframe, or explicit statements from the PRC foreclosing such a path.',
    'If the pathway is foreclosed, a key ''beneficiary'' aspect of the framework for democratic activists is removed, increasing their effective extraction and potentially shifting the classification towards a ''tangled_rope'' or ''snare'' for this group. If it remains viable, the ''rope'' function is maintained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_reform_pathway_viability, empirical, 'The status of Hong Kong''s democratic development under the framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__autonomy_primacy_reading, 1997, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t1997, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 1997, 0.2).
narrative_ontology:measurement(one__tr_t2002, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2002, 0.3).
narrative_ontology:measurement(one__tr_t2007, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2007, 0.4).
narrative_ontology:measurement(one__tr_t2012, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2012, 0.45).
narrative_ontology:measurement(one__tr_t2016, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2016, 0.5).
narrative_ontology:measurement(one__tr_t2020, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2020, 0.55).

% Extraction over time
narrative_ontology:measurement(one__be_t1997, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 1997, 0.25).
narrative_ontology:measurement(one__be_t2002, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2002, 0.28).
narrative_ontology:measurement(one__be_t2007, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2007, 0.3).
narrative_ontology:measurement(one__be_t2012, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2012, 0.32).
narrative_ontology:measurement(one__be_t2016, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2016, 0.34).
narrative_ontology:measurement(one__be_t2020, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2020, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t1997, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 1997, 0.3).
narrative_ontology:measurement(one__su_t2002, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2002, 0.35).
narrative_ontology:measurement(one__su_t2007, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2007, 0.4).
narrative_ontology:measurement(one__su_t2012, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2012, 0.42).
narrative_ontology:measurement(one__su_t2016, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2016, 0.44).
narrative_ontology:measurement(one__su_t2020, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2020, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__autonomy_primacy_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'one_country_two_systems_framework' kernel. It focuses on the primacy of Hong Kong's autonomy and treaty-guaranteed rights, in contrast to readings emphasizing PRC sovereignty or a negotiated balance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
