% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_conditionality__dependency_trap_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: income_support_conditionality__dependency_trap_reading
 *   human_readable: Unconditional Income Support as Dependency Trap
 *   domain: political_economy/social_policy/labor_economics
 *
 * SUMMARY:
 *   This story instantiates the dependency_trap_reading of the
 *   income_support_conditionality kernel: unconditional income support,
 *   absent work-linked conditionality, is claimed to erode labor-market
 *   attachment over time, producing skill atrophy and long-duration
 *   dependency in recipients while permanently obligating taxpayers to fund a
 *   growing non-productive transfer. This is a distinct constraint from the
 *   freedom_floor_reading (which reads the same transfer as decommodifying
 *   labor and enabling refusal of coercive work) and the wage_subsidy_reading
 *   (which reads it as an employer subsidy suppressing wages). All three
 *   share the same underlying policy instrument but diverge in
 *   beneficiary/victim structure, extraction profile, and classification —
 *   per the ε-invariance principle they are authored as three separate
 *   constraint files linked by network edges, not as one constraint with a
 *   measurement parameter.
 *
 * KEY AGENTS:
 *   - long_term_recipients: primary target (powerless/trapped) — bears skill atrophy and dependency cost
 *   - general_taxpayers: secondary target (organized/constrained) — funds the transfer indefinitely
 *   - welfare_administration_bureaucracy: agenda-setter (institutional/arbitrage) — administers and has budget interest in caseload persistence
 *   - political_incumbents_managing_transfer_programs: beneficiary (powerful/mobile) — collects electoral credit, bears none of the long-run cost
 *   - employers_in_low_wage_sectors: excluded voice (organized/mobile) — sees labor-supply withdrawal but is not in the design room
 *   - labor_economists_measuring_participation_effects: analytical observer — measures contested participation effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__dependency_trap_reading, 0.68).
domain_priors:suppression_score(income_support_conditionality__dependency_trap_reading, 0.52).
domain_priors:theater_ratio(income_support_conditionality__dependency_trap_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, resistance, 0.57).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__dependency_trap_reading, snare).
narrative_ontology:human_readable(income_support_conditionality__dependency_trap_reading, "Unconditional Income Support as Dependency Trap").
narrative_ontology:topic_domain(income_support_conditionality__dependency_trap_reading, "political_economy/social_policy/labor_economics").

domain_priors:requires_active_enforcement(income_support_conditionality__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__dependency_trap_reading, '1c5b2bdb-be42-401d-b546-22c460c68734').
narrative_ontology:cs_kernel_codification('1c5b2bdb-be42-401d-b546-22c460c68734', distributed).
narrative_ontology:cs_authority_grounding('1c5b2bdb-be42-401d-b546-22c460c68734', distributed).
narrative_ontology:cs_reading_relation('1c5b2bdb-be42-401d-b546-22c460c68734', income_support_conditionality__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('1c5b2bdb-be42-401d-b546-22c460c68734', income_support_conditionality__wage_subsidy_reading, coexists_with).
narrative_ontology:cs_axiom('1c5b2bdb-be42-401d-b546-22c460c68734', foundational, unconditionality_causes_labor_disengagement).
narrative_ontology:cs_axiom_status(unconditionality_causes_labor_disengagement, holdable).
narrative_ontology:cs_axiom_grounding('1c5b2bdb-be42-401d-b546-22c460c68734', unconditionality_causes_labor_disengagement, empirically_contingent).
narrative_ontology:cs_axiom('1c5b2bdb-be42-401d-b546-22c460c68734', secondary, work_conditionality_is_necessary_for_skill_maintenance).
narrative_ontology:cs_axiom_status(work_conditionality_is_necessary_for_skill_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('1c5b2bdb-be42-401d-b546-22c460c68734', work_conditionality_is_necessary_for_skill_maintenance, instrumental).
narrative_ontology:cs_reference_frame('1c5b2bdb-be42-401d-b546-22c460c68734', conditional_means_tested_welfare_baseline).
narrative_ontology:cs_drift_state('1c5b2bdb-be42-401d-b546-22c460c68734', post_unconditional_transfer_rollout, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1c5b2bdb-be42-401d-b546-22c460c68734', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__dependency_trap_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__dependency_trap_reading, welfare_administration_bureaucracy).
narrative_ontology:constraint_beneficiary(income_support_conditionality__dependency_trap_reading, political_incumbents_managing_transfer_programs).
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, long_term_recipients).
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, general_taxpayers).
narrative_ontology:constraint_vindicates(income_support_conditionality__dependency_trap_reading, work_incentive_erosion_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive an unconditional transfer sufficient to cover subsistence but calibrated such that returning to work yields little net gain once benefit withdrawal, forgone job search time, and skill-refresh costs are counted. Over years outside the labor market, credentials lapse, professional networks atrophy, and the psychological anchor of a work identity weakens. The transfer that was meant to be a floor becomes, in this reading, the reason they cannot climb back out — their own long-run earning capacity is what the constraint extracts.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, long_term_recipients, payer,
    powerless, biographical, trapped, national).

% Fund the transfer program through taxation with no discretion over allocation. As the caseload of long-duration recipients grows and the program is never sunset, taxpayers underwrite a permanently expanding non-productive transfer with no corresponding growth in output or tax base. Their exit option is political (vote, lobby, emigrate at the margin) but changing the program requires overcoming entrenched administrative and political interests.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, general_taxpayers, payer,
    organized, generational, constrained, national).

% Designs eligibility rules, benefit-withdrawal schedules, and administers the unconditional transfer. Institutional budget, headcount, and mandate scale with caseload size and duration, giving the administering body a structural interest in caseloads persisting rather than shrinking. Frames the program's continuation as compassionate necessity regardless of whether dependency is measurably increasing.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, welfare_administration_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, national).

% Gain electoral credit for having established or defended the unconditional transfer and bear little personal cost from its long-run labor-market effects, since those effects surface slowly and diffusely across a generation, past most single terms of office. Can point to the existence of the program as evidence of having solved the problem, independent of whether recipients are re-entering work.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, political_incumbents_managing_transfer_programs, beneficiary,
    powerful, biographical, mobile, national).

% Would testify that the shrinking applicant pool for entry-level and manual work, and the rising reservation wage of potential hires, reflect real withdrawal from the labor market caused by the transfer's design — but their complaints are read in policy debate as self-interested wage-suppression grievances and are largely excluded from the design conversation over benefit tapering.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, employers_in_low_wage_sectors, excluded,
    organized, biographical, mobile, national).

% Study labor force participation, employment duration, and skill-depreciation trajectories among long-term recipients versus matched controls. Their empirical findings on the magnitude of the disincentive effect are contested and cited selectively by all sides of the broader kernel dispute.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, labor_economists_measuring_participation_effects, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_conditionality__dependency_trap_reading, welfare_administration_bureaucracy).
narrative_ontology:fixing_cost_class(income_support_conditionality__dependency_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ostensibly coordinates a minimum income floor so no one falls below subsistence, removing the administrative overhead of verifying need or work-seeking behavior for every claimant.
% TRANSFER_FUNCTION: Moves tax revenue from general taxpayers to recipients on an unconditional basis, and — in this reading — moves recipients' own future earning capacity from their working years into a permanent transfer-dependent state, with no reciprocal work requirement recapturing the investment.
% ABSENT_VOICES: Low-wage employers experiencing labor-supply withdrawal and skill-currency loss are structurally excluded from program design; long-term recipients who successfully re-enter work after difficulty are rarely surveyed, so their account of what made re-entry hard is absent from most policy evaluations.
% DISAPPEARANCE_RATIONALE: Proponents of this reading hold that removing the unconditional transfer (or converting it to a conditional, time-limited, work-linked benefit) would restore labor market re-entry incentives and shrink the long-duration caseload over several years — the world rearranges toward higher participation. Recipients and administering agencies dispute this, arguing removal would simply produce destitution without restoring employability, since the skill atrophy and network loss are not reversed by benefit removal alone. The disagreement is not resolvable by this story alone.
% FOUNDING_PROBLEM: Means-tested and work-conditional welfare of the prior era was seen as producing high administrative cost, stigma, and perverse marginal tax rates from benefit cliffs; an unconditional floor was proposed to simplify support and remove the cliff.
% FOUNDING_PROBLEM_CORROBORATION: Labor economists outside the administering bureaucracy and outside recipient advocacy groups have published participation-rate studies suggesting the original benefit-cliff problem has been partially solved but replaced by a duration-dependency problem the unconditional design does not address; the welfare administration itself and beneficiary political incumbents continue to attest the founding problem remains live and the current design remains the correct solution — the corroborating outside source (independent labor economics literature) reads the founding problem as substantially transformed, not solved as originally intended.
narrative_ontology:disappearance_verdict(income_support_conditionality__dependency_trap_reading, contested).
narrative_ontology:founding_problem_status(income_support_conditionality__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__dependency_trap_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(income_support_conditionality__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__dependency_trap_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_conditionality__dependency_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_conditionality__dependency_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_conditionality__dependency_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.34 to 0.68 over the interval, modeling accumulating skill atrophy and duration-dependency as the caseload ages under a static unconditional design with no tapering or re-engagement mechanism. Suppression is moderate (0.52) — it is not a coercive constraint in the classic sense, but the withdrawal-schedule design and lack of exit pathways constitute a structural suppression of re-entry incentive once a recipient has been out of the labor force long enough that resuming work yields negligible net income gain. Theater ratio is comparatively low (0.28) because the core transfer function is genuinely operative, not merely performative — the disagreement in this reading is about the transfer's side effects, not about whether money actually moves.
 *
 * DIRECTIONALITY LOGIC:
 *   Long-term recipients are placed in the victim set under this reading because the structural claim is that the unconditional design traps them: no conditionality forces re-engagement, so the very absence of a work requirement becomes the mechanism of harm (trapped exit, powerless). Taxpayers are a second victim group: institutionally organized but with only diffuse political leverage over a program whose costs compound generationally. The welfare bureaucracy and incumbent politicians are beneficiaries because caseload persistence and program visibility serve their institutional and electoral interests respectively, independent of the program's stated goal of transitional support.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (benefit-cliff distortion under conditional welfare) is genuinely contested as either solved-and-replaced or still-live: this reading holds that a new problem (unconditional dependency) was created in solving the old one, and that the administering apparatus persists past the point where its original justification cleanly applies. Classifying as snare rather than a degraded piton is deliberate — a piton requires no concentrated beneficiary, but here the administering bureaucracy and political incumbents are a defensible concentrated beneficiary group, which is why this reading resolves to snare rather than piton despite some inertial, institutional-persistence features.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dependency_causation_vs_correlation,
    'Does unconditional transfer design causally reduce labor-force re-entry and cause skill atrophy, or do long-duration recipients simply have pre-existing characteristics (health, geography, discrimination exposure) correlated with both eligibility and low re-entry rates?',
    'Randomized or quasi-experimental unconditional-transfer trials with long follow-up windows tracking employment and skill measures against matched non-recipient controls, isolating transfer design from selection effects.',
    'If causal, this reading''s snare classification and victim declaration for long_term_recipients is well-grounded; if predominantly selection effect, the extraction attributed to the transfer design is overstated and the constraint is closer to a rope with pre-existing unrelated hardship.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dependency_causation_vs_correlation, empirical, 'Whether dependency effects are caused by transfer design or by pre-existing recipient characteristics.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the dependency_trap framing the correct structural lens for this policy instrument, or does it selectively foreground the disincentive mechanism while the freedom_floor and wage_subsidy readings foreground equally real but differently-weighted mechanisms operating on the same transfer simultaneously?',
    'Compare the three sibling readings'' predictive fit against longitudinal panel data: does skill atrophy, wage-floor suppression, or increased worker bargaining power best explain observed post-implementation labor-market shifts?',
    'If dependency effects dominate empirically, this reading''s snare classification is the primary structurally accurate account; if wage suppression or freedom effects dominate, this reading becomes a secondary or minority-weighted account of the same instrument.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Which of the three kernel readings best captures the instrument''s dominant structural effect.').

omega_variable(
    administrative_interest_vs_genuine_belief,
    'Does the welfare administration bureaucracy''s institutional interest in caseload persistence actually shape program design decisions, or is the bureaucracy''s continued defense of the unconditional design driven by genuine professional judgment that conditionality causes more harm than dependency does?',
    'Internal agency documents, budget-caseload correlation analysis, and comparison of agency positions before and after caseload-linked funding formulas were introduced.',
    'If institutional interest dominates, the beneficiary declaration for the bureaucracy is well-supported and the snare classification holds; if genuine professional judgment dominates and caseload-linked incentives are weak, the bureaucracy is better modeled as a good-faith agenda-setter rather than a beneficiary, weakening the tangled_rope/snare distinction toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(administrative_interest_vs_genuine_belief, empirical, 'Whether bureaucratic defense of the program reflects institutional self-interest or genuine policy judgment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__dependency_trap_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__dependency_trap_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(inco_tr_t4, income_support_conditionality__dependency_trap_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(inco_tr_t8, income_support_conditionality__dependency_trap_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(inco_tr_t12, income_support_conditionality__dependency_trap_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement(inco_tr_t16, income_support_conditionality__dependency_trap_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(inco_tr_t20, income_support_conditionality__dependency_trap_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(inco_tr_t24, income_support_conditionality__dependency_trap_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__dependency_trap_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(inco_be_t4, income_support_conditionality__dependency_trap_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(inco_be_t8, income_support_conditionality__dependency_trap_reading, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(inco_be_t12, income_support_conditionality__dependency_trap_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(inco_be_t16, income_support_conditionality__dependency_trap_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(inco_be_t20, income_support_conditionality__dependency_trap_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(inco_be_t24, income_support_conditionality__dependency_trap_reading, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_conditionality__dependency_trap_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(inco_su_t4, income_support_conditionality__dependency_trap_reading, suppression_requirement, 4, 0.35).
narrative_ontology:measurement(inco_su_t8, income_support_conditionality__dependency_trap_reading, suppression_requirement, 8, 0.39).
narrative_ontology:measurement(inco_su_t12, income_support_conditionality__dependency_trap_reading, suppression_requirement, 12, 0.43).
narrative_ontology:measurement(inco_su_t16, income_support_conditionality__dependency_trap_reading, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(inco_su_t20, income_support_conditionality__dependency_trap_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(inco_su_t24, income_support_conditionality__dependency_trap_reading, suppression_requirement, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__dependency_trap_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_conditionality__dependency_trap_reading, 0.12).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, income_support_conditionality__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, income_support_conditionality__wage_subsidy_reading).

% DUAL FORMULATION NOTE:
% This story is one of three ε-invariant decompositions of the income_support_conditionality kernel. dependency_trap_reading (this file) declares recipients and taxpayers as victims and classifies as snare. freedom_floor_reading declares recipients as beneficiaries and classifies closer to rope. wage_subsidy_reading declares low-wage workers and taxpayers as victims of employer capture and classifies as tangled_rope. Each carries its own stable ε and stakeholder structure; none averages over the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
