% ============================================================================
% CONSTRAINT STORY: udhr_article_3__procedural_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__procedural_hybrid_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: udhr_article_3__procedural_hybrid_reading
 *   human_readable: UDHR Article 3 Procedural Hybrid Reading
 *   domain: constitutional/human_rights
 *
 * SUMMARY:
 *   UDHR Article 3 reads: Everyone has the right to life, liberty and
 *   security of person. The procedural hybrid reading interprets this as
 *   guaranteeing due process protectionsâhabeas corpus, judicial review of
 *   detention, and prohibition of tortureâwhile deliberately leaving
 *   unresolved whether security of person requires positive welfare
 *   entitlements or merely negative liberty from state violence. This reading
 *   is one of three competing interpretations of the same textual kernel; it
 *   structurally coordinates international human rights practice around a
 *   legibility floor while permitting states to extract liberty through
 *   emergency detention exceptions and to avoid welfare obligations through
 *   procedural narrowness.
 *
 * KEY AGENTS:
 *   - State executives (agenda_setter/institutional/arbitrage): Administer emergency powers and exploit the unresolved welfare contest
 *   - Emergency detainees (payer/powerless/trapped): Bear extraction through suspended habeas corpus
 *   - Welfare claimants excluded (payer/powerless/constrained): Denied positive entitlements by the procedural focus
 *   - Human rights judiciaries (beneficiary/institutional/analytical): Derive mandate from procedural interpretation
 *   - Detainees with procedural access (beneficiary/powerless/constrained): Protected when enforcement is present
 *   - Human rights defenders (observer/organized/mobile): Document gaps and advocate for broader readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__procedural_hybrid_reading, 0.48).
domain_priors:suppression_score(udhr_article_3__procedural_hybrid_reading, 0.52).
domain_priors:theater_ratio(udhr_article_3__procedural_hybrid_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__procedural_hybrid_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__procedural_hybrid_reading, "UDHR Article 3 Procedural Hybrid Reading").
narrative_ontology:topic_domain(udhr_article_3__procedural_hybrid_reading, "constitutional/human_rights").

domain_priors:requires_active_enforcement(udhr_article_3__procedural_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__procedural_hybrid_reading, '2c497a49-3e12-4972-9a8e-4ebbc2385561').
narrative_ontology:cs_kernel_codification('2c497a49-3e12-4972-9a8e-4ebbc2385561', fixed_text).
narrative_ontology:cs_authority_grounding('2c497a49-3e12-4972-9a8e-4ebbc2385561', lineage).
narrative_ontology:cs_interpretation_layer_present('2c497a49-3e12-4972-9a8e-4ebbc2385561').
narrative_ontology:cs_reading_relation('2c497a49-3e12-4972-9a8e-4ebbc2385561', udhr_article_3__negative_liberty_reading, coexists_with).
narrative_ontology:cs_reading_relation('2c497a49-3e12-4972-9a8e-4ebbc2385561', udhr_article_3__positive_entitlement_reading, coexists_with).
narrative_ontology:cs_axiom('2c497a49-3e12-4972-9a8e-4ebbc2385561', foundational, habeas_and_torture_ban_exhaust_article_3_mandate).
narrative_ontology:cs_axiom_status(habeas_and_torture_ban_exhaust_article_3_mandate, holdable).
narrative_ontology:cs_axiom_grounding('2c497a49-3e12-4972-9a8e-4ebbc2385561', habeas_and_torture_ban_exhaust_article_3_mandate, conventional).
narrative_ontology:cs_axiom('2c497a49-3e12-4972-9a8e-4ebbc2385561', foundational, substantive_welfare_contest_beyond_article_3_scope).
narrative_ontology:cs_axiom_status(substantive_welfare_contest_beyond_article_3_scope, holdable).
narrative_ontology:cs_axiom_grounding('2c497a49-3e12-4972-9a8e-4ebbc2385561', substantive_welfare_contest_beyond_article_3_scope, conventional).
narrative_ontology:cs_reference_frame('2c497a49-3e12-4972-9a8e-4ebbc2385561', post_war_procedural_minimum).
narrative_ontology:cs_drift_state('2c497a49-3e12-4972-9a8e-4ebbc2385561', contemporary_human_rights_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2c497a49-3e12-4972-9a8e-4ebbc2385561', '').
narrative_ontology:cs_kernel_id(udhr_article_3__procedural_hybrid_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, detainees_with_procedural_access).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, human_rights_judiciaries).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, state_executives).
narrative_ontology:constraint_victim(udhr_article_3__procedural_hybrid_reading, emergency_detainees).
narrative_ontology:constraint_victim(udhr_article_3__procedural_hybrid_reading, welfare_claimants_excluded).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals in state custody who benefit from habeas corpus review and torture prohibition. Their security depends on judicial institutions recognizing and enforcing these procedural rights. Exit is constrained by detention itself, though procedural access offers partial relief.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, detainees_with_procedural_access, beneficiary,
    powerless, immediate, constrained, global).

% Individuals detained under emergency regimes where habeas corpus is suspended or delayed. They bear the cost of the procedural reading's allowance for emergency limits. Exit is trapped: they cannot access courts and cannot leave detention.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, emergency_detainees, payer,
    powerless, immediate, trapped, global).

% Individuals seeking housing, healthcare, or subsistence under the umbrella of security of person who are denied because the procedural reading does not resolve the welfare contest. They bear the cost of ambiguity: states cite the procedural focus to avoid positive obligations.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, welfare_claimants_excluded, payer,
    powerless, biographical, constrained, global).

% Domestic and international courts and treaty bodies that derive institutional authority from interpreting Article 3 as a procedural mandate. They adjudicate habeas petitions and torture claims. Their exit is analytical: they can adopt alternative readings but at high professional and doctrinal cost.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, human_rights_judiciaries, beneficiary,
    institutional, generational, analytical, global).

% State governments that administer detention regimes, declare emergencies, and interpret the unresolved welfare contest to avoid expenditure. They set the operational boundaries of the procedural guarantee and benefit from the flexibility the hybrid reading provides.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, state_executives, agenda_setter,
    institutional, generational, arbitrage, national).

% NGOs, lawyers, and monitoring bodies that document emergency detention and welfare gaps. They move between jurisdictions and forums, advocating for broader readings but operating within the procedural framework where it offers leverage.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, human_rights_defenders, observer,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared international standard for minimum procedural protections against arbitrary state violence: habeas corpus review, prohibition of torture, and judicial oversight of detention. Coordinates state behavior around a floor of legality without requiring agreement on maximalist welfare or liberty theories.
% TRANSFER_FUNCTION: Moves security from persons in emergency detention (where habeas is suspended) and welfare claimants (where positive obligations are denied) to states that retain flexibility in emergency powers and avoid welfare expenditure; moves institutional authority to judiciaries tasked with procedural review.
% ABSENT_VOICES: Torture survivors in non-state detention and persons seeking positive entitlements under Article 3 are structurally absent from the procedural reading's focus; they would argue for a broader substantive resolution of the liberty/welfare contest.
% DISAPPEARANCE_RATIONALE: If the procedural hybrid reading vanished, states would lose the legal cover of minimum procedural compliance for emergency detention regimes; judiciaries would lose their habeas mandate; welfare claimants would need to ground security claims in other articles or readings. The international human rights order would reorganize around either pure negative liberty or positive entitlement frameworks.
% FOUNDING_PROBLEM: Post-WWII prevention of arbitrary state killing, disappearance, and torture by fascist and totalitarian regimes; establishment of a universally legible minimum floor of procedural protection.
% FOUNDING_PROBLEM_CORROBORATION: Human rights historians and the UDHR drafting committee attest the founding problem. However, post-colonial scholars and critical legal theorists from outside the benefiting parties attest that the procedural focus was deliberately narrowed to avoid obligating wealthy states to redistribute resources, making the founding problem's resolution partial and contested.
narrative_ontology:disappearance_verdict(udhr_article_3__procedural_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__procedural_hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__procedural_hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(udhr_article_3__procedural_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__procedural_hybrid_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__procedural_hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_article_3__procedural_hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_article_3__procedural_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) is moderate: the torture prohibition and habeas corpus genuinely coordinate state behavior and protect some detainees, but emergency detention limits and the unresolved welfare contest create systematic extraction from emergency detainees and welfare claimants. Suppression (0.52) reflects active state resistance to full habeas access and the institutionalization of emergency exceptions. Theater ratio (0.30) captures performative complianceâstates ratify conventions, establish review bodies, yet maintain secret detention and deny welfare. Accessibility collapse (0.45) is moderate: alternative readings exist in some jurisdictions but are structurally marginalized by the procedural reading's dominance in international forums. Resistance (0.58) is substantial from human rights NGOs and progressive judiciaries pushing for broader interpretations.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (state executives) experiences the constraint as a flexible coordination tool that preserves sovereignty over emergencies and budgets. The payer seats (emergency detainees, welfare claimants) experience it as a hollow procedural shell that legitimates extraction. The beneficiary seat (detainees with access, judiciaries) experiences it as a genuine protective structure. The engine computes this divergence from structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   State executives sit near the beneficiary end: they gain institutional flexibility from the unresolved welfare contest and emergency powers. Emergency detainees sit at the full-target end: trapped, powerless, directly extracted via suspended habeas. Welfare claimants are also high-d targets: the procedural focus directly denies their claims. Human rights judiciaries are low-d beneficiaries: the reading gives them a mandate. Detainees with procedural access are mixedâbeneficiaries of the constraint's coordination function but only when enforcement is present.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents pure-extraction mislabeling because its coordination function is structurally real: habeas corpus and torture prohibition have verifiably prevented arbitrary killing and disappearance in multiple jurisdictions. It prevents pure-coordination mislabeling because the emergency exception and welfare ambiguity are not accidental side effects but structurally incorporated features that systematically benefit states at the cost of detainees and claimants.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    procedural_reading_vs_substantive_siblings,
    'Does the procedural hybrid reading structurally avoid the positive entitlement debate because the two questions are genuinely separable, or because separating them benefits states that wish to avoid welfare obligations?',
    'Comparative analysis of jurisdictions adopting positive entitlement readings versus procedural-only readings: if security outcomes are equivalent, the separation is genuine; if welfare-reading jurisdictions show measurably better security for the indigent, the separation is a cost-shield.',
    'If the separation is a cost-shield, the procedural reading''s epsilon is higher than its coordination framing suggests, pushing it toward snare. If genuine, it remains tangled_rope with moderate extraction from emergency exceptions alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_reading_vs_substantive_siblings, conceptual, 'Whether the procedural/substantive split is structurally necessary or strategically advantageous to avoiding welfare obligations.').

omega_variable(
    emergency_detention_as_extraction,
    'Are emergency detention limits and habeas suspensions inherent exceptions to any procedural guarantee, or are they constructed loopholes that allow states to extract liberty while maintaining a veneer of legality?',
    'Historical trend analysis of emergency declarations: if they correlate with regime stability threats rather than genuine security emergencies, they function as extraction mechanisms.',
    'If emergency powers are systematically abused, the extraction component of this constraint is higher and the coordination function is partially theatrical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_detention_as_extraction, empirical, 'Whether emergency detention exceptions are genuine safety valves or extraction channels.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__procedural_hybrid_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t0, udhr_article_3__procedural_hybrid_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(udhr_tr_t15, udhr_article_3__procedural_hybrid_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(udhr_tr_t30, udhr_article_3__procedural_hybrid_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(udhr_tr_t45, udhr_article_3__procedural_hybrid_reading, theater_ratio, 45, 0.3).
narrative_ontology:measurement(udhr_tr_t60, udhr_article_3__procedural_hybrid_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement(udhr_tr_t75, udhr_article_3__procedural_hybrid_reading, theater_ratio, 75, 0.32).

% Extraction over time
narrative_ontology:measurement(udhr_be_t0, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(udhr_be_t15, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(udhr_be_t30, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(udhr_be_t45, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 45, 0.45).
narrative_ontology:measurement(udhr_be_t60, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement(udhr_be_t75, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 75, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t0, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(udhr_su_t15, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 15, 0.45).
narrative_ontology:measurement(udhr_su_t30, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement(udhr_su_t45, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 45, 0.5).
narrative_ontology:measurement(udhr_su_t60, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 60, 0.62).
narrative_ontology:measurement(udhr_su_t75, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 75, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__procedural_hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, udhr_article_3__negative_liberty_reading).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, udhr_article_3__positive_entitlement_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the UDHR Article 3 kernel. The procedural hybrid reading isolates the procedural due process components and their structural relationship to unresolved substantive liberty/welfare questions. The negative liberty reading and positive entitlement reading instantiate alternative constraints from the same text. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
