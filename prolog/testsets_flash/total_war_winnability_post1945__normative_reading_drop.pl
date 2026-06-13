% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__normative_reading_drop
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__normative_reading_drop, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: total_war_winnability_post1945__normative_reading_drop
 *   human_readable: Normative Illegitimacy of Total War (Post-1945)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint describes the normative illegitimacy of total war,
 *   established primarily through Article 2(4) of the UN Charter and the
 *   development of international humanitarian law post-1945. While total war
 *   remains physically possible, this reading argues that it has become
 *   normatively unacceptable, constraining state behavior. This is one
 *   reading of the 'total_war_winnability_post1945' kernel, focusing on the
 *   ideational and legal shift rather than structural or cultural factors.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__normative_reading_drop, 0.15).
domain_priors:suppression_score(total_war_winnability_post1945__normative_reading_drop, 0.3).
domain_priors:theater_ratio(total_war_winnability_post1945__normative_reading_drop, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, extractiveness, 0.15).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__normative_reading_drop, rope).
narrative_ontology:human_readable(total_war_winnability_post1945__normative_reading_drop, "Normative Illegitimacy of Total War (Post-1945)").
narrative_ontology:topic_domain(total_war_winnability_post1945__normative_reading_drop, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_winnability_post1945__normative_reading_drop).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__normative_reading_drop, '47260b5c-92ac-492a-b01d-91b06199a2ba').
narrative_ontology:cs_kernel_codification('47260b5c-92ac-492a-b01d-91b06199a2ba', formalized).
narrative_ontology:cs_authority_grounding('47260b5c-92ac-492a-b01d-91b06199a2ba', lineage).
narrative_ontology:cs_interpretation_layer_present('47260b5c-92ac-492a-b01d-91b06199a2ba').
narrative_ontology:cs_reading_relation('47260b5c-92ac-492a-b01d-91b06199a2ba', total_war_winnability_post1945__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('47260b5c-92ac-492a-b01d-91b06199a2ba', total_war_winnability_post1945__strategic_culture_drift, coexists_with).
narrative_ontology:cs_axiom('47260b5c-92ac-492a-b01d-91b06199a2ba', foundational, total_war_is_normatively_unacceptable).
narrative_ontology:cs_axiom_status(total_war_is_normatively_unacceptable, holdable).
narrative_ontology:cs_axiom_grounding('47260b5c-92ac-492a-b01d-91b06199a2ba', total_war_is_normatively_unacceptable, deontological).
narrative_ontology:cs_axiom('47260b5c-92ac-492a-b01d-91b06199a2ba', foundational, international_law_constrains_warfare).
narrative_ontology:cs_axiom_status(international_law_constrains_warfare, holdable).
narrative_ontology:cs_axiom_grounding('47260b5c-92ac-492a-b01d-91b06199a2ba', international_law_constrains_warfare, conventional).
narrative_ontology:cs_reference_frame('47260b5c-92ac-492a-b01d-91b06199a2ba', post_wwii_un_charter_order).
narrative_ontology:cs_drift_state('47260b5c-92ac-492a-b01d-91b06199a2ba', contemporary_geopolitical_challenges, gap(stable, minor, true)).
narrative_ontology:cs_created_at('47260b5c-92ac-492a-b01d-91b06199a2ba', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__normative_reading_drop, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, global_civilian_populations).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, international_legal_order).
narrative_ontology:constraint_victim(total_war_winnability_post1945__normative_reading_drop, revisionist_powers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, humanitarian_organizations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the normative prohibition against total war, which theoretically reduces their exposure to indiscriminate violence and existential threats. They have no direct exit from the international system but are the primary intended beneficiaries of humanitarian law.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, global_civilian_populations, beneficiary,
    organized, generational, trapped, global).

% Embodies and enforces the normative framework, particularly Article 2(4) of the UN Charter and international humanitarian law. Its legitimacy and efficacy are enhanced by the adherence to these norms, though it faces challenges from non-compliant states.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, international_legal_order, agenda_setter,
    institutional, civilizational, constrained, global).

% Are constrained by the normative illegitimacy of total war, limiting their strategic options and imposing reputational and legal costs for non-compliance. They bear the cost of not being able to pursue total victory through unrestricted means.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, revisionist_powers, payer,
    powerful, biographical, constrained, global).

% Operate under the protective umbrella of international humanitarian law, which defines their mandate and provides a basis for their interventions. Their work is made possible by the normative framework, even if imperfectly applied.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, humanitarian_organizations, beneficiary,
    moderate, generational, mobile, global).

% Analyze the evolution of warfare and the impact of normative constraints on state behavior. They observe the persistence of the total war concept as a physical possibility versus its decline in normative acceptability.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, strategic_theorists, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates states around a shared understanding that total war, while physically possible, is normatively unacceptable, thereby reducing the likelihood of catastrophic global conflict and protecting civilian populations.
% TRANSFER_FUNCTION: Transfers the right to wage war without restraint from states to the international legal order, in exchange for a more stable and predictable international environment. It transfers the burden of restraint onto states, particularly those with revisionist aims.
% ABSENT_VOICES: Historical proponents of total war, who would argue for the strategic necessity of unrestricted conflict to achieve decisive victory, are absent from contemporary legitimate discourse. Their arguments are now largely relegated to the fringes or historical analysis.
% DISAPPEARANCE_RATIONALE: If the normative illegitimacy of total war vanished, the international system would rapidly destabilize. States would lose a key restraint on conflict escalation, leading to a higher probability of large-scale, indiscriminate violence and potentially existential threats. The international legal order would lose a foundational principle.
% FOUNDING_PROBLEM: The catastrophic human cost and existential threat posed by two World Wars and the advent of nuclear weapons, which demonstrated the unacceptability of total war as a legitimate instrument of policy.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing existence of nuclear weapons and the devastating potential of modern conventional warfare, attested by military strategists, international legal scholars, and humanitarian organizations, corroborates that the problem of preventing total war remains live. The UN Charter and subsequent humanitarian law development are direct responses to this problem.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__normative_reading_drop, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__normative_reading_drop, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__normative_reading_drop, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(total_war_winnability_post1945__normative_reading_drop, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__normative_reading_drop_tests).
:- end_tests(total_war_winnability_post1945__normative_reading_drop_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Rope because it solves a genuine collective action problem (preventing global catastrophe) with relatively low extraction and suppression, primarily through shared normative commitment. Extractiveness (0.15) is low, representing the cost of restraint on states. Suppression (0.3) is moderate, reflecting the need for active enforcement and diplomatic pressure against violations. Theater ratio (0.1) is low, as the commitment to these norms is largely genuine, though some performative adherence exists. Accessibility collapse (0.7) is high because the normative framework significantly limits the perceived 'legitimate' options for warfare. Resistance (0.2) is low, as most states adhere to the norm, though revisionist powers occasionally challenge it.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of global civilian populations, this is a vital protective constraint. From revisionist powers, it is a limitation on their sovereignty and strategic freedom. The international legal order views it as a foundational principle for global stability.
 *
 * DIRECTIONALITY LOGIC:
 *   Global civilian populations and the international legal order are beneficiaries, as the constraint protects them and enhances the order's legitimacy. Revisionist powers are victims, as their strategic options are curtailed by the normative framework. Humanitarian organizations also benefit from the framework that enables their work. Strategic theorists are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing total war) remains highly live, as the physical possibility of total war persists. The normative framework continues to serve its original function, preventing mandatrophy. The low theater ratio indicates that the constraint is not primarily maintained for performative reasons.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    normative_vs_structural_causation,
    'To what extent is the absence of total war due to normative illegitimacy (this reading) versus the structural constraint of nuclear weapons (structural_contraction_reading)?',
    'Comparative historical analysis of state behavior in conflicts where nuclear deterrence was not directly applicable, or counterfactual analysis of a world without nuclear weapons but with the same normative framework.',
    'If structural factors are dominant, this constraint''s effective suppression and extractiveness might be lower than perceived, as the ''restraint'' is physically enforced. If normative factors are dominant, this reading''s classification as a Rope is strongly supported.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(normative_vs_structural_causation, empirical, 'Distinguishing the causal weight of normative vs. structural factors in preventing total war.').

omega_variable(
    normative_vs_strategic_culture_drift,
    'Is the normative illegitimacy of total war a distinct causal factor, or is it primarily a reflection of a deeper ideational shift in strategic culture (strategic_culture_drift)?',
    'Analysis of diplomatic archives and elite discourse to identify whether normative arguments were actively invoked to constrain strategic choices, or if they merely rationalized pre-existing cultural shifts.',
    'If it''s a mere reflection, the ''normative'' constraint might be a Piton, a theatrical performance of a deeper cultural shift. If it''s an active causal force, its Rope classification is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normative_vs_strategic_culture_drift, conceptual, 'Distinguishing normative illegitimacy from broader strategic culture shifts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__normative_reading_drop, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(tota_tr_t1960, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1960, 0.08).
narrative_ontology:measurement(tota_tr_t1980, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(tota_tr_t2000, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(tota_tr_t2024, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1945, 0.1).
narrative_ontology:measurement(tota_be_t1960, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1960, 0.12).
narrative_ontology:measurement(tota_be_t1980, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1980, 0.15).
narrative_ontology:measurement(tota_be_t2000, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 2000, 0.14).
narrative_ontology:measurement(tota_be_t2024, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1945, 0.2).
narrative_ontology:measurement(tota_su_t1960, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1960, 0.25).
narrative_ontology:measurement(tota_su_t1980, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1980, 0.3).
narrative_ontology:measurement(tota_su_t2000, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 2000, 0.28).
narrative_ontology:measurement(tota_su_t2024, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__normative_reading_drop, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, structural_contraction_reading).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, strategic_culture_drift).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
