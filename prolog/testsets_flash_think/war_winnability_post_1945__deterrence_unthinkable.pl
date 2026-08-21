% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__deterrence_unthinkable
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__deterrence_unthinkable, []).

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
 *   constraint_id: war_winnability_post_1945__deterrence_unthinkable
 *   human_readable: Nuclear War is Unwinnable: Deterrence by Unthinkability
 *   domain: strategic_studies/nuclear_deterrence/international_relations
 *
 * SUMMARY:
 *   This constraint represents the reading that nuclear weapons made
 *   great-power total war categorically unwinnable, rendering traditional
 *   strategic planning for victory incoherent. It asserts a fundamental shift
 *   in the nature of warfare post-1945, where the primary function of nuclear
 *   arsenals became deterrence through the threat of unacceptable
 *   destruction, rather than a means to achieve military victory. This
 *   reading emphasizes the operational contraction of the space for war
 *   winnability and the shift of strategic planning towards war prevention.
 *
 * KEY AGENTS:
 *   - civilian_populations: Primary beneficiary (powerless/trapped) — spared from total war.
 *   - military_establishments: Primary payer (institutional/constrained) — face mission incoherence.
 *   - traditional_strategists: Payer (powerful/identity_locked) — their frameworks are obsolete.
 *   - nuclear_powers: Agenda setter (institutional/arbitrage) — manage the reality of deterrence.
 *   - arms_control_advocates: Beneficiary (organized/mobile) — their agenda is strengthened.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__deterrence_unthinkable, 0.85).
domain_priors:suppression_score(war_winnability_post_1945__deterrence_unthinkable, 0.95).
domain_priors:theater_ratio(war_winnability_post_1945__deterrence_unthinkable, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, extractiveness, 0.85).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__deterrence_unthinkable, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__deterrence_unthinkable, "Nuclear War is Unwinnable: Deterrence by Unthinkability").
narrative_ontology:topic_domain(war_winnability_post_1945__deterrence_unthinkable, "strategic_studies/nuclear_deterrence/international_relations").

domain_priors:requires_active_enforcement(war_winnability_post_1945__deterrence_unthinkable).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__deterrence_unthinkable, 'cfcbad09-9c8d-4a01-8230-31cf7a17d388').
narrative_ontology:cs_kernel_codification('cfcbad09-9c8d-4a01-8230-31cf7a17d388', implicit).
narrative_ontology:cs_authority_grounding('cfcbad09-9c8d-4a01-8230-31cf7a17d388', self_enforcing).
narrative_ontology:cs_reading_relation('cfcbad09-9c8d-4a01-8230-31cf7a17d388', war_winnability_post_1945__countervailing_thinkable, forecloses).
narrative_ontology:cs_reading_relation('cfcbad09-9c8d-4a01-8230-31cf7a17d388', war_winnability_post_1945__rhetorical_contraction, influences).
narrative_ontology:cs_axiom('cfcbad09-9c8d-4a01-8230-31cf7a17d388', foundational, mutual_assured_destruction_is_absolute).
narrative_ontology:cs_axiom_status(mutual_assured_destruction_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('cfcbad09-9c8d-4a01-8230-31cf7a17d388', mutual_assured_destruction_is_absolute, deontological).
narrative_ontology:cs_axiom('cfcbad09-9c8d-4a01-8230-31cf7a17d388', foundational, total_war_has_no_victor).
narrative_ontology:cs_axiom_status(total_war_has_no_victor, holdable).
narrative_ontology:cs_axiom_grounding('cfcbad09-9c8d-4a01-8230-31cf7a17d388', total_war_has_no_victor, empirically_contingent).
narrative_ontology:cs_reference_frame('cfcbad09-9c8d-4a01-8230-31cf7a17d388', post_hiroshima_strategic_reality).
narrative_ontology:cs_drift_state('cfcbad09-9c8d-4a01-8230-31cf7a17d388', contemporary_strategic_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('cfcbad09-9c8d-4a01-8230-31cf7a17d388', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, civilian_populations).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, arms_control_advocates).
narrative_ontology:constraint_victim(war_winnability_post_1945__deterrence_unthinkable, military_establishments).
narrative_ontology:constraint_victim(war_winnability_post_1945__deterrence_unthinkable, traditional_strategists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Primarily benefit from the absence of great-power total war, which this constraint ensures. However, they live under the existential threat of nuclear weapons, a cost they bear passively.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, civilian_populations, beneficiary,
    powerless, generational, trapped, global).

% Bear the cost of mission incoherence: their traditional role of achieving victory in total war is rendered obsolete. This leads to shifts in doctrine, budget allocation away from conventional war-fighting, and a psychological burden of managing unwinnable conflict scenarios.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, military_establishments, payer,
    institutional, biographical, constrained, global).

% Experience a professional crisis as their intellectual frameworks for planning and executing total war become irrelevant or even dangerous. Their careers and academic standing are tied to these traditional concepts, making exit from this mindset difficult.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, traditional_strategists, payer,
    powerful, biographical, identity_locked, global).

% Possess the weapons that create this reality and shape its interpretation through doctrine and policy. They benefit from the stability of deterrence but bear the immense responsibility and cost of maintaining nuclear arsenals and preventing their use.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, nuclear_powers, agenda_setter,
    institutional, generational, arbitrage, global).

% Live under the shadow of this constraint, benefiting from the absence of great-power total war but having no direct control over the nuclear reality. They often advocate for disarmament or non-proliferation.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, non_nuclear_states, observer,
    organized, biographical, mobile, global).

% Their arguments for disarmament and non-proliferation gain significant moral and strategic weight from the premise that nuclear war is unwinnable. The constraint provides a strong justification for their agenda.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, arms_control_advocates, beneficiary,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: By establishing the categorical unwinnability of great-power total war, the constraint coordinates states towards mutual restraint, crisis management, and the avoidance of direct military confrontation, thereby preventing global catastrophe.
% TRANSFER_FUNCTION: Transfers the traditional mission and resources for achieving victory in total war from military establishments to the maintenance of deterrence, war prevention, and crisis stability. It also imposes an existential risk on all populations.
% ABSENT_VOICES: Advocates for 'limited nuclear war' or those who believe in the possibility of achieving victory in a nuclear exchange are marginalized from mainstream strategic discourse. Their views are often seen as dangerous or unrealistic in light of the constraint's core premise.
% DISAPPEARANCE_RATIONALE: If nuclear weapons suddenly became winnable, or ceased to exist, the entire global security architecture, strategic doctrines, and military planning would fundamentally reorganize. Great-power conventional war would likely return as a viable option, and the existential threat would shift or disappear, leading to profound geopolitical instability.
% FOUNDING_PROBLEM: The foundational problem was how to prevent a repeat of the devastating world wars of the 20th century, especially with the advent of weapons of mass destruction that threatened civilization itself.
% FOUNDING_PROBLEM_CORROBORATION: The problem of preventing great-power total war remains live, as evidenced by ongoing international treaties, UN resolutions, and the consistent rhetoric of major powers. Independent historians, political scientists, and peace researchers also corroborate the enduring nature of this problem, even as specific threats evolve.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__deterrence_unthinkable, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__deterrence_unthinkable, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__deterrence_unthinkable, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(war_winnability_post_1945__deterrence_unthinkable, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__deterrence_unthinkable, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__deterrence_unthinkable_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__deterrence_unthinkable, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_winnability_post_1945__deterrence_unthinkable_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because military establishments and traditional strategists lose their core mission and intellectual frameworks, incurring significant costs in terms of relevance and resources. Suppression is very high, as the physical reality of nuclear weapons, combined with dominant strategic doctrines, actively suppresses any credible alternative narrative of total war winnability. Theater ratio is low, reflecting the stark, non-performative nature of nuclear deterrence; the threat is real, not merely theatrical. Accessibility collapse is high because the alternative of 'winning' a total war has largely vanished from the realm of rational strategic thought. Resistance is moderate, as some elements within military and strategic communities continue to explore concepts like limited nuclear war or counterforce targeting, attempting to reintroduce winnability.
 *
 * PERSPECTIVAL GAP:
 *   Military establishments and traditional strategists experience this constraint as highly extractive, undermining their core purpose. Civilian populations and arms control advocates, however, perceive it as a beneficial coordination mechanism that prevents catastrophic war. The engine's per-seat classification will reflect this divergence, with high extraction for payers and subsidy for beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   Civilian populations are full beneficiaries (d=0.0) as they are spared from total war. Arms control advocates also benefit as their cause gains legitimacy. Military establishments and traditional strategists are targets (d near 1.0) due to mission incoherence and obsolescence of their frameworks. Nuclear powers, as agenda setters, benefit from global stability but also bear the immense costs and risks of maintaining deterrence, placing them closer to symmetric (d=0.5) or slightly beneficiary depending on the specific calculus.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling the constraint as a pure Mountain (which would ignore the significant costs borne by military establishments) or a pure Snare (which would ignore the genuine coordination function of preventing total war). It accurately captures the hybrid nature: a coordination mechanism (preventing war) that simultaneously imposes substantial, asymmetric extraction on specific institutional actors (military establishments, traditional strategists) whose traditional roles are undermined by the new reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_unthinkability_vs_operational_planning,
    'Is the categorical unwinnability of total war a universally accepted operational reality, or does it primarily function as a rhetorical constraint that coexists with ongoing, albeit covert, operational planning for limited victory?',
    'Declassified strategic planning documents, war games, and military exercises from nuclear powers, analyzed for evidence of genuine operational incoherence versus continued pursuit of limited victory scenarios.',
    'If operational planning for victory persists, the constraint''s effective suppression and extractiveness on military establishments might be lower than assessed, suggesting a more ''rhetorical_contraction'' reading. If incoherence is truly operational, the current assessment holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_unthinkability_vs_operational_planning, empirical, 'Distinguishing rhetorical unwinnability from operational unwinnability.').

omega_variable(
    unwinnability_as_coordination_or_extraction,
    'To what extent is the ''unwinnability'' of nuclear war a genuine coordination function (preventing war) versus an extractive mechanism that reallocates resources and power away from traditional military roles?',
    'Analysis of defense budgets and strategic doctrine shifts over time: if resources are primarily reallocated to non-military or purely defensive/deterrent functions, it supports the coordination aspect. If they are reallocated to new forms of offensive power projection, it suggests a more extractive dynamic.',
    'If the extractive component (resource reallocation, mission incoherence) is found to be disproportionately large compared to the coordination benefit, the constraint might lean more towards a Snare. If the coordination function is dominant, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unwinnability_as_coordination_or_extraction, conceptual, 'Clarifying the balance between coordination and extraction in nuclear deterrence.').

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a genuine structural reality of nuclear weapons, or a dominant interpretation that forecloses alternative strategic thinking?',
    'Comparative analysis with sibling readings (countervailing_thinkable, rhetorical_contraction) to identify the specific empirical or conceptual points of divergence and their implications for strategic behavior.',
    'If this reading is found to be primarily an interpretive construct, its ''naturalness'' (emerges_naturally) would be further undermined, potentially shifting its classification towards a more purely constructed type like Snare if the extractive elements are dominant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity between structural reality and interpretive dominance for the ''war_winnability_post_1945'' kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__deterrence_unthinkable, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1945, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1945, 0.25).
narrative_ontology:measurement(war__tr_t1960, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(war__tr_t1975, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1975, 0.1).
narrative_ontology:measurement(war__tr_t1990, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(war__tr_t2005, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 2005, 0.07).
narrative_ontology:measurement(war__tr_t2025, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 2025, 0.05).

% Extraction over time
narrative_ontology:measurement(war__be_t1945, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1945, 0.6).
narrative_ontology:measurement(war__be_t1960, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1960, 0.7).
narrative_ontology:measurement(war__be_t1975, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1975, 0.75).
narrative_ontology:measurement(war__be_t1990, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1990, 0.8).
narrative_ontology:measurement(war__be_t2005, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 2005, 0.82).
narrative_ontology:measurement(war__be_t2025, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 2025, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1945, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1945, 0.7).
narrative_ontology:measurement(war__su_t1960, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1960, 0.8).
narrative_ontology:measurement(war__su_t1975, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1975, 0.85).
narrative_ontology:measurement(war__su_t1990, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1990, 0.9).
narrative_ontology:measurement(war__su_t2005, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 2005, 0.92).
narrative_ontology:measurement(war__su_t2025, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 2025, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__deterrence_unthinkable, enforcement_mechanism).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, arms_control_treaties).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, conventional_military_budgets).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945__countervailing_thinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945__rhetorical_contraction).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('deterrence_unthinkable') of the 'war_winnability_post_1945' kernel. It asserts that nuclear weapons made great-power total war categorically unwinnable, rendering planning for victory incoherent. It is linked to sibling readings that offer alternative interpretations of nuclear war's winnability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
