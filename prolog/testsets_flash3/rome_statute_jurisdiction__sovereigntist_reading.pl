% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__sovereigntist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__sovereigntist_reading, []).

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
 *   constraint_id: rome_statute_jurisdiction__sovereigntist_reading
 *   human_readable: Rome Statute Jurisdiction (Sovereigntist Reading)
 *   domain: international_law/treaty_interpretation/institutional_authority
 *
 * SUMMARY:
 *   This constraint represents the sovereigntist reading of the Rome
 *   Statute's jurisdictional framework, emphasizing strict state consent for
 *   ICC jurisdiction. It posits that the Statute primarily establishes a
 *   conditional framework, where non-party states and their nationals are
 *   largely immune from ICC prosecution unless jurisdiction is explicitly
 *   accepted or mandated by the UN Security Council. National courts retain
 *   primary authority, and complementarity functions as deference to national
 *   processes, not an override. This reading is often advanced by states wary
 *   of international judicial overreach.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__sovereigntist_reading, 0.25).
domain_priors:suppression_score(rome_statute_jurisdiction__sovereigntist_reading, 0.15).
domain_priors:theater_ratio(rome_statute_jurisdiction__sovereigntist_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__sovereigntist_reading, mountain).
narrative_ontology:human_readable(rome_statute_jurisdiction__sovereigntist_reading, "Rome Statute Jurisdiction (Sovereigntist Reading)").
narrative_ontology:topic_domain(rome_statute_jurisdiction__sovereigntist_reading, "international_law/treaty_interpretation/institutional_authority").

domain_priors:emerges_naturally(rome_statute_jurisdiction__sovereigntist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__sovereigntist_reading, '6f1ea66a-da0c-40b6-8485-fd034de0e97f').
narrative_ontology:cs_kernel_codification('6f1ea66a-da0c-40b6-8485-fd034de0e97f', fixed_text).
narrative_ontology:cs_authority_grounding('6f1ea66a-da0c-40b6-8485-fd034de0e97f', lineage).
narrative_ontology:cs_interpretation_layer_present('6f1ea66a-da0c-40b6-8485-fd034de0e97f').
narrative_ontology:cs_reading_relation('6f1ea66a-da0c-40b6-8485-fd034de0e97f', rome_statute_jurisdiction__universalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('6f1ea66a-da0c-40b6-8485-fd034de0e97f', rome_statute_jurisdiction__hybrid_complementarity_reading, coexists_with).
narrative_ontology:cs_axiom('6f1ea66a-da0c-40b6-8485-fd034de0e97f', foundational, state_consent_is_foundational_to_jurisdiction).
narrative_ontology:cs_axiom_status(state_consent_is_foundational_to_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('6f1ea66a-da0c-40b6-8485-fd034de0e97f', state_consent_is_foundational_to_jurisdiction, deontological).
narrative_ontology:cs_axiom('6f1ea66a-da0c-40b6-8485-fd034de0e97f', foundational, national_courts_have_primary_jurisdiction).
narrative_ontology:cs_axiom_status(national_courts_have_primary_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('6f1ea66a-da0c-40b6-8485-fd034de0e97f', national_courts_have_primary_jurisdiction, conventional).
narrative_ontology:cs_reference_frame('6f1ea66a-da0c-40b6-8485-fd034de0e97f', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('6f1ea66a-da0c-40b6-8485-fd034de0e97f', contemporary_international_justice_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('6f1ea66a-da0c-40b6-8485-fd034de0e97f', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, non_party_states).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, national_sovereignty_doctrine).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, national_judiciaries).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__sovereigntist_reading, principle_of_state_sovereignty).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__sovereigntist_reading, principle_of_consent_in_international_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states benefit from the interpretation that their non-ratification of the Rome Statute shields their nationals from ICC jurisdiction, except under specific UN Security Council referrals. They actively defend this interpretation to preserve their sovereign prerogatives.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, non_party_states, beneficiary,
    institutional, generational, arbitrage, global).

% The ICC Prosecutor operates within the strictures of this reading, understanding that jurisdiction is primarily consent-based. This limits the scope of investigations and requires careful navigation of state cooperation, often deferring to national processes.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, icc_prosecutor, agenda_setter,
    institutional, biographical, constrained, global).

% National courts retain primary jurisdiction over international crimes, with the ICC acting only when national systems are unwilling or unable. This reading reinforces their authority and reduces the likelihood of ICC intervention.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, national_judiciaries, beneficiary,
    institutional, generational, mobile, national).

% Advocates for a broader, universal application of international criminal justice find their aspirations constrained by this sovereigntist reading. They argue that grave crimes should transcend state consent, but their position is marginalized within this framework.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, universal_jurisdiction_advocates, excluded,
    moderate, generational, identity_locked, global).

% Academics analyze the legal implications and practical effects of this interpretation, often debating its consistency with the broader goals of international justice and its impact on accountability for mass atrocities.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the exercise of international criminal jurisdiction by establishing clear boundaries based on state consent, thereby reducing friction between national sovereignty and international legal mechanisms.
% TRANSFER_FUNCTION: Transfers primary authority for prosecuting international crimes to national jurisdictions, and limits the ICC's reach to consenting states or specific UN Security Council mandates, effectively transferring sovereign control over justice back to states.
% ABSENT_VOICES: Advocates for a more robust, universal international criminal justice system, who would argue that crimes against humanity transcend state consent, are largely excluded from shaping this interpretation.
% DISAPPEARANCE_RATIONALE: If this sovereigntist reading vanished, the ICC's jurisdiction would immediately expand, potentially leading to interventions in non-consenting states and a significant reordering of international legal relations, with profound implications for state sovereignty and international accountability.
% FOUNDING_PROBLEM: The Rome Statute was established to create a permanent international criminal court to prosecute individuals for genocide, war crimes, crimes against humanity, and the crime of aggression, while respecting state sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: Party states and international legal bodies generally agree that the problem of prosecuting grave international crimes remains live. However, the balance between international justice and state sovereignty, which this reading emphasizes, is a constant point of contention, with non-party states and some legal scholars corroborating the need for sovereign consent.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__sovereigntist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__sovereigntist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__sovereigntist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(rome_statute_jurisdiction__sovereigntist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__sovereigntist_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__sovereigntist_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, ExtMetricName, E),
    domain_priors:suppression_score(rome_statute_jurisdiction__sovereigntist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(rome_statute_jurisdiction__sovereigntist_reading),
    narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(rome_statute_jurisdiction__sovereigntist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as a Mountain because this reading asserts the fundamental and unchangeable nature of state sovereignty and consent in international law, which the Rome Statute is understood to codify rather than supersede. Extractiveness is low (0.25) because it primarily limits the reach of international institutions, thereby 'extracting' less from states than other readings. Suppression is low (0.15) as it doesn't actively coerce states but rather relies on their existing sovereign power. Theater ratio is low (0.05) as the mechanisms for respecting consent are largely functional within this framework. The slight increase in extractiveness and suppression over time reflects the ongoing tension and occasional challenges to this interpretation, requiring minor defensive actions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of non-party states, this reading is a natural and necessary safeguard of sovereignty. From the perspective of universal jurisdiction advocates, it is a constructed limitation that hinders justice. The engine's classification will reflect the structural benefits to states and the limitations on international bodies, consistent with a Mountain-like interpretation from the sovereigntist seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-party states and national judiciaries are beneficiaries, as this reading protects their sovereign authority and primary jurisdiction. The ICC Prosecutor is an agenda-setter operating within these constraints. Universal jurisdiction advocates are excluded, as their position is structurally marginalized by this interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_treaty_interpretation,
    'Is the principle of strict sovereign consent a natural law of international relations, or a specific interpretation of treaty law that could be revised?',
    'Analysis of state practice and opinio juris over time, particularly regarding the evolution of customary international law on jurisdiction. If state practice consistently deviates from strict consent, it suggests a constructed rather not natural limit.',
    'If it''s a constructed interpretation, the constraint''s ''emerges_naturally'' property would be false, potentially reclassifying it from Mountain to a more constructed type (e.g., Rope or Tangled Rope), reflecting its dependence on active defense rather than inherent truth.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_treaty_interpretation, conceptual, 'Ambiguity between inherent sovereign right and a specific legal interpretation.').

omega_variable(
    complementarity_as_deference_or_override,
    'Does the complementarity principle, as interpreted by this reading, genuinely ensure national primacy, or does it implicitly allow for ICC override under certain conditions?',
    'Empirical analysis of ICC decisions on admissibility challenges, particularly cases where national proceedings were deemed ''unwilling or unable.'' If the ICC frequently asserts jurisdiction despite national claims of ability, it suggests a weaker deference.',
    'If complementarity is found to be less deferential in practice, the ''suppression'' metric for national judiciaries would increase, and the ''extractiveness'' from national sovereignty would rise, potentially shifting the classification towards a more extractive type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_as_deference_or_override, empirical, 'The practical effect of complementarity on national jurisdiction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__sovereigntist_reading, 1998, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t1998, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 1998, 0.03).
narrative_ontology:measurement(rome_tr_t2006, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2006, 0.04).
narrative_ontology:measurement(rome_tr_t2014, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2014, 0.04).
narrative_ontology:measurement(rome_tr_t2024, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(rome_be_t1998, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 1998, 0.2).
narrative_ontology:measurement(rome_be_t2006, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2006, 0.22).
narrative_ontology:measurement(rome_be_t2014, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2014, 0.24).
narrative_ontology:measurement(rome_be_t2024, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t1998, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 1998, 0.1).
narrative_ontology:measurement(rome_su_t2006, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2006, 0.12).
narrative_ontology:measurement(rome_su_t2014, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2014, 0.14).
narrative_ontology:measurement(rome_su_t2024, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__sovereigntist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction__universalist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction__hybrid_complementarity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Rome Statute's jurisdictional framework. This sovereigntist reading emphasizes state consent and national primacy, contrasting with universalist and hybrid complementarity interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
