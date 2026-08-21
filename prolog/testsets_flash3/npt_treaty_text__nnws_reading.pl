% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__nnws_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_text__nnws_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: npt_treaty_text__nnws_reading
 *   human_readable: NPT Article VI Disarmament Obligation (NNWS Reading)
 *   domain: international_law/arms_control
 *
 * SUMMARY:
 *   This constraint represents the Non-Nuclear Weapon States' (NNWS) reading
 *   of the Nuclear Non-Proliferation Treaty (NPT), specifically focusing on
 *   Article VI as a binding obligation for Nuclear Weapon States (NWS) to
 *   pursue disarmament. From this perspective, non-proliferation by NNWS is
 *   conditional on NWS compliance with disarmament, and the NPT Review
 *   Conferences are key mechanisms for enforcing this obligation. The NNWS
 *   reading views the NPT as a 'grand bargain' where disarmament is not
 *   merely aspirational but a legal imperative. The claimed type is 'rope'
 *   because it aims for genuine coordination towards a shared goal
 *   (disarmament) but faces significant resistance and requires active
 *   enforcement (diplomatic pressure, TPNW competition) to hold NWS
 *   accountable.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nnws_reading, 0.45).
domain_priors:suppression_score(npt_treaty_text__nnws_reading, 0.3).
domain_priors:theater_ratio(npt_treaty_text__nnws_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nnws_reading, rope).
narrative_ontology:human_readable(npt_treaty_text__nnws_reading, "NPT Article VI Disarmament Obligation (NNWS Reading)").
narrative_ontology:topic_domain(npt_treaty_text__nnws_reading, "international_law/arms_control").

domain_priors:requires_active_enforcement(npt_treaty_text__nnws_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nnws_reading, '147bc6f0-3b91-412a-9cc6-5e73ac5586bd').
narrative_ontology:cs_kernel_codification('147bc6f0-3b91-412a-9cc6-5e73ac5586bd', fixed_text).
narrative_ontology:cs_authority_grounding('147bc6f0-3b91-412a-9cc6-5e73ac5586bd', lineage).
narrative_ontology:cs_interpretation_layer_present('147bc6f0-3b91-412a-9cc6-5e73ac5586bd').
narrative_ontology:cs_reading_relation('147bc6f0-3b91-412a-9cc6-5e73ac5586bd', npt_treaty_text__nws_reading, coexists_with).
narrative_ontology:cs_reading_relation('147bc6f0-3b91-412a-9cc6-5e73ac5586bd', npt_treaty_text__withdrawal_threshold_reading, coexists_with).
narrative_ontology:cs_axiom('147bc6f0-3b91-412a-9cc6-5e73ac5586bd', foundational, article_vi_binding_disarmament_obligation).
narrative_ontology:cs_axiom_status(article_vi_binding_disarmament_obligation, holdable).
narrative_ontology:cs_axiom_grounding('147bc6f0-3b91-412a-9cc6-5e73ac5586bd', article_vi_binding_disarmament_obligation, deontological).
narrative_ontology:cs_axiom('147bc6f0-3b91-412a-9cc6-5e73ac5586bd', foundational, non_proliferation_conditional_on_disarmament).
narrative_ontology:cs_axiom_status(non_proliferation_conditional_on_disarmament, holdable).
narrative_ontology:cs_axiom_grounding('147bc6f0-3b91-412a-9cc6-5e73ac5586bd', non_proliferation_conditional_on_disarmament, conventional).
narrative_ontology:cs_reference_frame('147bc6f0-3b91-412a-9cc6-5e73ac5586bd', grand_bargain_disarmament_imperative).
narrative_ontology:cs_drift_state('147bc6f0-3b91-412a-9cc6-5e73ac5586bd', contemporary_security_environment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('147bc6f0-3b91-412a-9cc6-5e73ac5586bd', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__nnws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, global_security_regime).
narrative_ontology:constraint_victim(npt_treaty_text__nnws_reading, nuclear_weapon_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states commit to not acquiring nuclear weapons, expecting the NWS to fulfill their disarmament obligations under Article VI. They actively push for disarmament through NPT Review Conferences and support alternative regimes like the TPNW, viewing non-proliferation as conditional on NWS compliance.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, non_nuclear_weapon_states, beneficiary,
    organized, generational, constrained, global).

% These states are obligated under Article VI to pursue nuclear disarmament. From the NNWS reading, this is a binding, time-sensitive obligation. They face diplomatic pressure and regime competition (TPNW) to reduce their arsenals, which they often resist, viewing disarmament as a long-term aspiration rather than an immediate, enforceable commitment.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, nuclear_weapon_states, payer,
    institutional, generational, constrained, global).

% The IAEA monitors and verifies non-proliferation commitments, but its mandate does not directly extend to enforcing NWS disarmament. It facilitates technical discussions and reports on compliance, indirectly influencing the pressure on NWS.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, international_atomic_energy_agency, agenda_setter,
    institutional, civilizational, constrained, global).

% The overall framework of international norms and institutions that benefit from both non-proliferation and disarmament efforts, aiming for a more stable and secure world. This is an abstract entity representing the collective good.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, global_security_regime, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(npt_treaty_text__nnws_reading, global_security_regime).

% The TPNW represents an alternative legal framework that explicitly prohibits nuclear weapons and aims to stigmatize their possession. While not directly part of the NPT, its existence and growing membership exert pressure on the NPT framework and NWS, challenging the NPT's perceived inaction on disarmament.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, treaty_on_the_prohibition_of_nuclear_weapons_regime, excluded,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the commitment of non-nuclear weapon states to forgo nuclear weapons in exchange for a binding commitment from nuclear weapon states to disarm, aiming to prevent proliferation and move towards a nuclear-free world.
% TRANSFER_FUNCTION: Transfers the right to develop nuclear weapons from NNWS to NWS (by NNWS foregoing them) in exchange for a promise of future disarmament from NWS. The NNWS reading emphasizes the binding nature of this disarmament promise.
% ABSENT_VOICES: The TPNW regime, representing states that have lost faith in the NPT's disarmament progress, is structurally excluded from direct NPT negotiations but exerts external pressure. Future generations, who would bear the catastrophic risks of nuclear war, are also absent.
% DISAPPEARANCE_RATIONALE: If the NPT's disarmament obligation vanished, NNWS would lose a key legal basis for demanding NWS disarmament, potentially leading to a breakdown of the non-proliferation norm and increased incentives for NNWS to acquire nuclear weapons, fundamentally altering the global security landscape.
% FOUNDING_PROBLEM: The existential threat of nuclear war and the desire to prevent the spread of nuclear weapons beyond the initial five nuclear powers, while also committing those powers to eventual disarmament.
% FOUNDING_PROBLEM_CORROBORATION: NNWS and many international legal scholars corroborate that the problem of nuclear proliferation and the need for disarmament remain live. NWS often argue that the problem of disarmament is complex and requires a different security environment, effectively contesting the immediacy and binding nature of the original problem's solution.
narrative_ontology:disappearance_verdict(npt_treaty_text__nnws_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__nnws_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__nnws_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(npt_treaty_text__nnws_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__nnws_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_text__nnws_reading_tests).
:- end_tests(npt_treaty_text__nnws_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because NWS are compelled to engage in disarmament talks and face reputational costs, but actual reductions are slow and often tied to other security considerations. Suppression (0.30) is relatively low because NNWS have diplomatic avenues and the TPNW as an alternative, though imperfect, means of pressure. Theater ratio (0.20) reflects that while some disarmament efforts are genuine, NWS often engage in performative gestures without deep structural change. The rising trend in extractiveness and suppression over time reflects increasing NNWS frustration and more assertive demands for NWS compliance, leading to greater pressure on NWS.
 *
 * PERSPECTIVAL GAP:
 *   The NNWS reading fundamentally differs from the NWS reading, which often interprets Article VI as an aspirational goal rather than a binding obligation. This divergence leads to different experiences of the constraint: NNWS see it as a tool to hold NWS accountable, while NWS perceive it as an imposition on their sovereign security decisions. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   NNWS are beneficiaries (d near 0.0) as they gain security from non-proliferation and push for disarmament. NWS are targets (d near 1.0) as they bear the costs of disarmament obligations and face pressure to reduce their arsenals. The IAEA is an agenda-setter, facilitating the regime. The TPNW regime is 'excluded' from the NPT framework but influences it by offering an alternative path.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    binding_vs_aspirational_disarmament,
    'Is NPT Article VI a binding legal obligation for NWS to disarm, or an aspirational long-term goal?',
    'International Court of Justice advisory opinion on the legal status of Article VI, or a new NPT protocol specifying disarmament timelines and verification mechanisms.',
    'If binding, the NWS''s current extractiveness is higher, and their resistance to disarmament is a violation. If aspirational, the NNWS reading''s extractiveness on NWS is lower, and the constraint functions more as a symbolic gesture.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(binding_vs_aspirational_disarmament, conceptual, 'Ambiguity in the legal interpretation of Article VI''s disarmament clause.').

omega_variable(
    effectiveness_of_review_conferences,
    'Are NPT Review Conferences effective mechanisms for advancing disarmament, or are they primarily forums for diplomatic theater?',
    'Empirical analysis of disarmament progress directly attributable to Review Conference outcomes versus other factors (e.g., bilateral treaties, domestic policy shifts).',
    'If primarily theater, the constraint''s theater_ratio is higher, and its effective extractiveness on NWS is lower than perceived by NNWS. If effective, the NNWS reading''s classification as a ''rope'' is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_of_review_conferences, empirical, 'The actual functional impact of NPT Review Conferences on NWS disarmament.').

omega_variable(
    tpnw_regime_impact,
    'Does the Treaty on the Prohibition of Nuclear Weapons (TPNW) genuinely increase pressure on NWS to disarm, or does it merely create a parallel, less effective regime?',
    'Longitudinal study of NWS policy changes and public statements in response to TPNW ratification and advocacy, compared to NPT-only pressure.',
    'If TPNW significantly increases pressure, the NNWS reading''s effective suppression on NWS is higher, as NWS face a more robust external challenge. If TPNW is largely ignored, the NPT''s internal dynamics remain dominant.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tpnw_regime_impact, empirical, 'The actual influence of the TPNW on NWS disarmament behavior.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nnws_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_text__nnws_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(npt__tr_t1985, npt_treaty_text__nnws_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(npt__tr_t2000, npt_treaty_text__nnws_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_text__nnws_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(npt__tr_t2024, npt_treaty_text__nnws_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_text__nnws_reading, base_extractiveness, 1970, 0.3).
narrative_ontology:measurement(npt__be_t1985, npt_treaty_text__nnws_reading, base_extractiveness, 1985, 0.35).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_text__nnws_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_text__nnws_reading, base_extractiveness, 2010, 0.43).
narrative_ontology:measurement(npt__be_t2024, npt_treaty_text__nnws_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_text__nnws_reading, suppression_requirement, 1970, 0.2).
narrative_ontology:measurement(npt__su_t1985, npt_treaty_text__nnws_reading, suppression_requirement, 1985, 0.25).
narrative_ontology:measurement(npt__su_t2000, npt_treaty_text__nnws_reading, suppression_requirement, 2000, 0.28).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_text__nnws_reading, suppression_requirement, 2010, 0.29).
narrative_ontology:measurement(npt__su_t2024, npt_treaty_text__nnws_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__nnws_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, npt_treaty_text__nws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, npt_treaty_text__withdrawal_threshold_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, tpnw_treaty_text).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the NPT treaty text kernel, focusing on the NNWS interpretation of Article VI disarmament obligations. It is linked to other readings of the same kernel and to the TPNW regime, which exerts external pressure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
