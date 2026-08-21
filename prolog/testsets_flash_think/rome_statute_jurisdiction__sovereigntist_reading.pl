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
 *   This constraint represents the 'sovereigntist reading' of the Rome
 *   Statute's jurisdictional framework, which posits that the International
 *   Criminal Court's (ICC) authority is strictly conditional on state
 *   consent. From this perspective, the Statute primarily functions to
 *   establish a legitimate international court while rigorously upholding the
 *   primacy of national sovereignty and jurisdiction. Non-party nationals are
 *   immune from ICC jurisdiction unless their state consents or the UN
 *   Security Council refers a situation. The constraint is claimed as a
 *   Mountain because, from this reading, state consent is an irreducible
 *   feature of international law, protecting states from external overreach.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__sovereigntist_reading, 0.05).
domain_priors:suppression_score(rome_statute_jurisdiction__sovereigntist_reading, 0.1).
domain_priors:theater_ratio(rome_statute_jurisdiction__sovereigntist_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__sovereigntist_reading, mountain).
narrative_ontology:human_readable(rome_statute_jurisdiction__sovereigntist_reading, "Rome Statute Jurisdiction (Sovereigntist Reading)").
narrative_ontology:topic_domain(rome_statute_jurisdiction__sovereigntist_reading, "international_law/treaty_interpretation/institutional_authority").

domain_priors:emerges_naturally(rome_statute_jurisdiction__sovereigntist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__sovereigntist_reading, '26a8c4fb-b89c-431b-9514-89706ca41132').
narrative_ontology:cs_kernel_codification('26a8c4fb-b89c-431b-9514-89706ca41132', fixed_text).
narrative_ontology:cs_authority_grounding('26a8c4fb-b89c-431b-9514-89706ca41132', lineage).
narrative_ontology:cs_interpretation_layer_present('26a8c4fb-b89c-431b-9514-89706ca41132').
narrative_ontology:cs_reading_relation('26a8c4fb-b89c-431b-9514-89706ca41132', rome_statute_jurisdiction__universalist_reading, forecloses).
narrative_ontology:cs_reading_relation('26a8c4fb-b89c-431b-9514-89706ca41132', rome_statute_jurisdiction__hybrid_complementarity_reading, coexists_with).
narrative_ontology:cs_axiom('26a8c4fb-b89c-431b-9514-89706ca41132', foundational, state_sovereignty_is_primary).
narrative_ontology:cs_axiom_status(state_sovereignty_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('26a8c4fb-b89c-431b-9514-89706ca41132', state_sovereignty_is_primary, deontological).
narrative_ontology:cs_axiom('26a8c4fb-b89c-431b-9514-89706ca41132', foundational, consent_is_basis_of_jurisdiction).
narrative_ontology:cs_axiom_status(consent_is_basis_of_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('26a8c4fb-b89c-431b-9514-89706ca41132', consent_is_basis_of_jurisdiction, conventional).
narrative_ontology:cs_reference_frame('26a8c4fb-b89c-431b-9514-89706ca41132', state_sovereignty_primacy).
narrative_ontology:cs_drift_state('26a8c4fb-b89c-431b-9514-89706ca41132', contemporary_international_justice_discourse, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('26a8c4fb-b89c-431b-9514-89706ca41132', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, sovereign_states).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, non_party_nationals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__sovereigntist_reading, icc).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As parties to the Rome Statute, they define and uphold the limits of ICC jurisdiction, ensuring that their consent is paramount. They benefit from the protection of their national sovereignty and the primary authority of their national courts.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, sovereign_states, agenda_setter,
    institutional, generational, mobile, global).

% The International Criminal Court operates under the strict jurisdictional limits imposed by the Rome Statute, requiring state consent or UNSC referral. Its ability to investigate and prosecute is constrained by these rules, which it must respect to maintain its legitimacy.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, icc, payer,
    organized, generational, constrained, global).

% Nationals of states not party to the Rome Statute are generally immune from ICC jurisdiction under this reading, unless their state consents or the UN Security Council refers a situation. This provides them with a layer of protection from international prosecution.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, non_party_nationals, beneficiary,
    powerless, biographical, mobile, global).

% These actors argue for a broader interpretation of ICC jurisdiction, transcending strict state consent, especially for grave international crimes. Their arguments are largely rejected by the sovereigntist reading, and they are excluded from shaping the core jurisdictional framework.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, universalist_advocates, excluded,
    organized, generational, identity_locked, global).

% Academics and legal experts who analyze the Rome Statute and its interpretations, often engaging in debates about the balance between sovereignty and international justice. They observe the practical application and theoretical underpinnings of the jurisdictional framework.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a framework for international criminal justice that coordinates state consent, ensuring that the ICC's exercise of jurisdiction respects the foundational principle of state sovereignty.
% TRANSFER_FUNCTION: Limits the transfer of sovereign jurisdiction from states to the ICC, ensuring that national courts retain primary authority and that the ICC's power is derived from, and constrained by, state consent.
% ABSENT_VOICES: Universalist advocates and human rights organizations who argue for a more expansive interpretation of ICC jurisdiction, potentially transcending state consent for the most heinous crimes, are structurally marginalized by this framework.
% DISAPPEARANCE_RATIONALE: If the strict consent framework vanished, the ICC might assert broader jurisdiction, leading to significant international legal and political upheaval as states would strongly resist perceived infringements on their sovereignty. The entire architecture of international criminal justice would need to be renegotiated.
% FOUNDING_PROBLEM: To create a legitimate and effective international criminal court that could prosecute grave international crimes while simultaneously respecting the fundamental principles of state sovereignty and the primacy of national jurisdiction.
% FOUNDING_PROBLEM_CORROBORATION: State practice, diplomatic statements, and legal interpretations by national courts consistently corroborate the ongoing importance of state sovereignty in international law. While universalist readings contest the *extent* of this problem, the underlying tension between sovereignty and international justice remains a live issue for many states.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__sovereigntist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__sovereigntist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__sovereigntist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(rome_statute_jurisdiction__sovereigntist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__sovereigntist_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

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
 *   The metrics reflect the sovereigntist interpretation: extractiveness is very low (0.05) because the constraint protects states from the ICC's potential overreach, rather than extracting from them. Suppression is low (0.1) as it primarily suppresses the ICC's ability to act without consent, not the actions of states. Theater ratio is low (0.05) as the consent framework is genuinely applied and respected in practice. Accessibility collapse is high (0.9) because, from this perspective, alternatives to consent-based jurisdiction are largely illegitimate or non-existent in international law. Resistance is low (0.1) as states generally adhere to this framework.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of sovereign states, this framework is a protective mountain, ensuring their autonomy. However, from a universalist perspective, the same framework might be seen as a snare that shields perpetrators of grave crimes by limiting the ICC's reach. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Sovereign states are primary beneficiaries (d=0.0) as the constraint protects their jurisdiction and prevents external intervention without consent. Non-party nationals are also beneficiaries as they are shielded from ICC jurisdiction. The ICC itself is a target (d=1.0) as its jurisdiction is strictly limited by this framework. Universalist advocates are excluded from this framework, as their arguments for broader jurisdiction are rejected by its core tenets.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_as_natural_law_vs_convention,
    'Is state sovereignty, as the basis for ICC jurisdiction, a natural law-like feature of international relations or a constructed convention?',
    'Analysis of historical state practice and philosophical arguments regarding the origins of international legal personality and authority.',
    'If a natural law, the constraint is a genuine mountain; if a convention, it is a constructed rope or snare that benefits states, triggering false summit detection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_as_natural_law_vs_convention, conceptual, 'Ambiguity of sovereignty''s ontological status in international law.').

omega_variable(
    scope_of_complementarity_principle,
    'Is the complementarity principle a strict deference to national jurisdiction, or does it imply a balancing act where the ICC can intervene if national systems are unwilling or unable?',
    'Judicial decisions by the ICC and national courts in cases where complementarity is invoked, and state party interpretations of their obligations.',
    'If strict deference, this sovereigntist reading is reinforced; if a balancing act, the ''hybrid_complementarity_reading'' gains ground, potentially increasing ICC effective jurisdiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_complementarity_principle, empirical, 'Interpretation of the complementarity principle''s limits.').

omega_variable(
    limits_of_consent_for_international_crimes,
    'Can the gravity of international crimes (genocide, crimes against humanity, war crimes) truly transcend the requirement of state consent for jurisdiction, as argued by universalist readings?',
    'Evolution of customary international law, UN Security Council practice, and the development of universal jurisdiction principles in national legal systems.',
    'If consent is transcended, the ''universalist_reading'' gains legitimacy, challenging the foundational premise of this sovereigntist reading and potentially reclassifying the constraint as a snare for states resisting universal jurisdiction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(limits_of_consent_for_international_crimes, conceptual, 'Whether international crimes can override state consent for jurisdiction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__sovereigntist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t0, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(rome_tr_t10, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(rome_tr_t20, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(rome_tr_t30, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 30, 0.05).

% Extraction over time
narrative_ontology:measurement(rome_be_t0, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(rome_be_t10, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 10, 0.05).
narrative_ontology:measurement(rome_be_t20, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 20, 0.05).
narrative_ontology:measurement(rome_be_t30, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 30, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t0, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(rome_su_t10, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 10, 0.1).
narrative_ontology:measurement(rome_su_t20, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 20, 0.1).
narrative_ontology:measurement(rome_su_t30, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 30, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__sovereigntist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction__universalist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction__hybrid_complementarity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Rome Statute's jurisdictional framework. Its ε value differs significantly from universalist or hybrid readings, necessitating separate constraint stories. This reading emphasizes strict state consent as foundational.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
