% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__dependency_trap_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: unconditional_income_support__dependency_trap_reading
 *   human_readable: Unconditional Income Support as Dependency-Inducing Upward Transfer
 *   domain: political_economy/social_policy
 *
 * SUMMARY:
 *   This story instantiates the dependency_trap_reading of the unconditional
 *   income support kernel: unconditional transfers are read as an
 *   incentive-distorting subsidy that rewards idleness, crowds out
 *   better-targeted aid, and redistributes upward to households that do not
 *   need the support. The referent is the standing unconditional-transfer
 *   arrangement itself, assessed by this reading's own lights — not the
 *   targeted-aid counterfactual this reading would prefer, which would
 *   trivially read as low-extraction. Sibling readings
 *   (freedom_floor_reading, universality_paradox_reading) treat the same
 *   kernel text — an unconditional payment to all — but locate its function
 *   and beneficiary structure differently; they are separate constraint
 *   files, not alternate measurements of this one.
 *
 * KEY AGENTS:
 *   - middle_upper_income_households: beneficiary (moderate/mobile) — receives windfall transfer without need
 *   - ubi_advocacy_organizations: beneficiary/agenda_setter (organized/mobile) — accrues political capital from universality
 *   - working_poor_program_recipients: primary target (powerless/trapped) — loses net support via program substitution
 *   - general_taxpayers: secondary target (moderate/constrained) — bears $1.4T net fiscal cost
 *   - displaced_program_administrators: excluded voice (moderate/constrained) — institutional knowledge unheard
 *   - policy_analysts_labor_economists: analytical observer — measures pilot employment effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__dependency_trap_reading, 0.71).
domain_priors:suppression_score(unconditional_income_support__dependency_trap_reading, 0.42).
domain_priors:theater_ratio(unconditional_income_support__dependency_trap_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__dependency_trap_reading, snare).
narrative_ontology:human_readable(unconditional_income_support__dependency_trap_reading, "Unconditional Income Support as Dependency-Inducing Upward Transfer").
narrative_ontology:topic_domain(unconditional_income_support__dependency_trap_reading, "political_economy/social_policy").

domain_priors:requires_active_enforcement(unconditional_income_support__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__dependency_trap_reading, 'f646c7cf-bfc6-4d8c-892b-c8b549683a64').
narrative_ontology:cs_kernel_codification('f646c7cf-bfc6-4d8c-892b-c8b549683a64', distributed).
narrative_ontology:cs_authority_grounding('f646c7cf-bfc6-4d8c-892b-c8b549683a64', distributed).
narrative_ontology:cs_reading_relation('f646c7cf-bfc6-4d8c-892b-c8b549683a64', unconditional_income_support__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('f646c7cf-bfc6-4d8c-892b-c8b549683a64', unconditional_income_support__universality_paradox_reading, influences).
narrative_ontology:cs_axiom('f646c7cf-bfc6-4d8c-892b-c8b549683a64', foundational, targeted_transfers_dominate_universal_transfers_on_incidence).
narrative_ontology:cs_axiom_status(targeted_transfers_dominate_universal_transfers_on_incidence, holdable).
narrative_ontology:cs_axiom_grounding('f646c7cf-bfc6-4d8c-892b-c8b549683a64', targeted_transfers_dominate_universal_transfers_on_incidence, empirically_contingent).
narrative_ontology:cs_axiom('f646c7cf-bfc6-4d8c-892b-c8b549683a64', secondary, unconditional_payment_generates_labor_supply_disincentive).
narrative_ontology:cs_axiom_status(unconditional_payment_generates_labor_supply_disincentive, holdable).
narrative_ontology:cs_axiom_grounding('f646c7cf-bfc6-4d8c-892b-c8b549683a64', unconditional_payment_generates_labor_supply_disincentive, empirically_contingent).
narrative_ontology:cs_reference_frame('f646c7cf-bfc6-4d8c-892b-c8b549683a64', means_tested_targeted_welfare_baseline).
narrative_ontology:cs_drift_state('f646c7cf-bfc6-4d8c-892b-c8b549683a64', post_large_scale_pilot_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f646c7cf-bfc6-4d8c-892b-c8b549683a64', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__dependency_trap_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, middle_upper_income_households).
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, ubi_advocacy_organizations).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, working_poor_program_recipients).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, general_taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive the same flat unconditional transfer as low-income households despite not needing income support to meet basic needs. The transfer arrives regardless of employment, assets, or existing income, effectively functioning as a tax rebate or bonus rather than a safety net for this group. They face no exit cost because the payment is pure upside with no accompanying obligation.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, middle_upper_income_households, beneficiary,
    moderate, biographical, mobile, national).

% Build political capital and institutional funding around the universality principle, arguing means-testing is stigmatizing and administratively wasteful. They benefit reputationally and organizationally from the program's persistence regardless of its labor-market or fiscal effects, and are insulated from the costs the program generates for programs they do not administer.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, ubi_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__dependency_trap_reading, ubi_advocacy_organizations, agenda_setter).

% Previously relied on targeted programs (housing vouchers, SNAP, EITC-style wage supplements) calibrated to their specific need level, often worth substantially more than the flat unconditional payment. When the unconditional transfer displaces or is offset against these targeted programs, net support falls even though the headline payment appears universal and generous. They cannot restore the displaced targeted benefit by any individual action; the substitution is administrative and structural.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, working_poor_program_recipients, payer,
    powerless, immediate, trapped, regional).

% Fund a program whose net fiscal cost (after program consolidation offsets) remains substantial — cited at roughly $1.4 trillion — while a large share of the transferred funds flow to households with no demonstrated need. They cannot opt out of the tax base that funds the program, and their exit option is limited to electoral or migratory responses over long horizons.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, general_taxpayers, payer,
    moderate, generational, constrained, national).

% Operate the targeted programs being consolidated or defunded in favor of the unconditional transfer. Their institutional knowledge of need-calibration is not represented in the universality debate, and their objections that the targeted programs were more cost-effective per dollar of poverty reduction are largely absent from the political framing.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, displaced_program_administrators, excluded,
    moderate, biographical, constrained, national).

% Study large-scale pilot data (including the AEI meta-analysis showing a -3.2% employment effect in large pilots) and model the fiscal and behavioral consequences of universal versus targeted transfers, without a direct stake in either program's continuation.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, policy_analysts_labor_economists, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unconditional_income_support__dependency_trap_reading, middle_upper_income_households).
narrative_ontology:fixing_cost_class(unconditional_income_support__dependency_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reduces the administrative overhead and stigma cost of means-testing by paying everyone the same amount regardless of circumstance, which genuinely does simplify delivery and avoid benefit cliffs for some recipients near the eligibility boundary.
% TRANSFER_FUNCTION: Moves tax revenue from the general taxpayer base to all households including those with no demonstrated need, while simultaneously withdrawing or offsetting targeted support previously concentrated on low-income and working-poor households — net effect: money flows away from the poor and toward the non-needy relative to the counterfactual targeted system.
% ABSENT_VOICES: Displaced program administrators and the working poor who lose more from targeted-program consolidation than they gain from the flat payment are structurally underrepresented in a political debate framed around universality's administrative elegance and dignity rather than net transfer incidence.
% DISAPPEARANCE_RATIONALE: If the unconditional transfer were withdrawn and targeted programs restored to their prior scope, the working poor would receive more net support per dollar spent, the aggregate fiscal cost would likely fall (per the offset analysis), and the -3.2% employment drag documented in large pilots would be expected to reverse toward baseline labor-force participation.
% FOUNDING_PROBLEM: Targeted welfare programs were seen as bureaucratically fragmented, stigmatizing to claim, and prone to benefit cliffs that discouraged work at the margin — the unconditional transfer was proposed to replace this fragmented system with one simple, dignity-preserving payment.
% FOUNDING_PROBLEM_CORROBORATION: UBI advocacy organizations and recipient middle/upper-income households attest the founding problem (administrative fragmentation and stigma) remains live and is being solved. Independent labor economists and the AEI meta-analysis, along with displaced program administrators outside the advocacy coalition, attest that the substitution has instead reduced net support for the working poor and produced a measurable employment disincentive — corroboration exists from outside the benefiting parties and contradicts the advocates' framing.
narrative_ontology:disappearance_verdict(unconditional_income_support__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__dependency_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unconditional_income_support__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__dependency_trap_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__dependency_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unconditional_income_support__dependency_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unconditional_income_support__dependency_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 to 0.71 over the interval as pilot data accumulates (the AEI meta-analysis's -3.2% employment finding in large-scale trials) and program-substitution effects compound: early adoption periods show modest net transfer distortion while targeted programs are still partially intact, but as consolidation proceeds the working poor's net position deteriorates further. Suppression is moderate (0.42) rather than extreme because the constraint operates more through fiscal and administrative substitution than direct coercion — recipients are not physically prevented from seeking other support, but the targeted alternatives are withdrawn from the menu. Theater ratio (0.38) reflects that a real coordination function (simplified delivery, reduced stigma) coexists with a growing performative dimension — the universality framing does genuine administrative work but increasingly serves as cover for the transfer's regressive incidence.
 *
 * PERSPECTIVAL GAP:
 *   From the ubi_advocacy_organizations seat, the arrangement reads as principled universal coordination solving stigma and administrative fragmentation. From the working_poor_program_recipients seat, the identical structure reads as extraction: the specific aid that was calibrated to their need has been replaced by a smaller, undifferentiated payment. The engine should compute these as structurally different seat-level classifications from the same base data — that divergence is the point of a dependency_trap reading rather than a defect in it.
 *
 * DIRECTIONALITY LOGIC:
 *   Middle/upper-income households and UBI advocacy organizations sit near the beneficiary end of directionality: they receive net value (direct payment or political capital) without bearing the offsetting costs. Working poor program recipients sit near the full-target end — trapped exit options because the targeted-program substitution is an administrative fait accompli they cannot individually reverse, and immediate time horizon because the loss is felt in the current budget cycle. General taxpayers are targets at a longer horizon and with constrained (not trapped) exit, since electoral and migratory responses exist but are slow and costly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (administrative fragmentation, benefit cliffs, claiming stigma) was real at inception but this reading holds that the chosen remedy has drifted from solving it toward a mechanism whose primary observable effect is upward redistribution and a measurable labor-supply drag. Classifying as snare rather than rope prevents the coordination story (simplicity, dignity) from being read as sufficient cover for the transfer's incidence; the tangled_rope alternative was considered but rejected here because the coordination function, while real, is judged (in this reading) to be substantially outweighed by asymmetric extraction with active enforcement (program consolidation rules that foreclose the targeted alternative) rather than a genuine hybrid balance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    employment_effect_generalizability,
    'Does the -3.2% employment effect observed in large UBI pilots generalize to permanent, nationwide implementation, or is it an artifact of pilot design (temporary duration, small scale, participant awareness of time-limited status)?',
    'Comparison of pilot-scale employment effects against outcomes from the few permanent or near-permanent unconditional transfer programs (e.g., Alaska Permanent Fund dividend studies) to test whether the effect persists, shrinks, or grows under permanence.',
    'If the effect is a pilot artifact that shrinks under permanence, the extractiveness score for this reading is overstated; if it persists or grows, the snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employment_effect_generalizability, empirical, 'Whether observed employment drag generalizes beyond pilot conditions.').

omega_variable(
    program_substitution_completeness,
    'Is the reported net loss to working-poor recipients a function of how completely the unconditional transfer is designed to substitute for targeted programs, or is this reading assuming full substitution when partial coexistence (transfer plus retained targeted aid) is politically and fiscally plausible?',
    'Comparative analysis of implemented UBI-style programs to determine the actual substitution ratio against targeted benefits in enacted (not merely proposed) legislation.',
    'If substitution is partial rather than complete, the victim-side extraction for working_poor_program_recipients is smaller than authored here, which would move this reading toward tangled_rope rather than snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(program_substitution_completeness, empirical, 'Whether full program substitution, the reading''s core mechanism, is the realistic implementation path.').

omega_variable(
    kernel_framing_choice,
    'Is the dependency_trap_reading''s choice to treat the standing arrangement as ''unconditional transfer substituting for targeted aid'' the correct framing, versus the freedom_floor_reading''s framing of the same kernel as ''removal of labor-market coercion,'' given that both readings describe the identical policy text?',
    'Track which framing better predicts observed political coalition behavior and enacted program design across jurisdictions that have implemented unconditional transfers — does the coalition structure match the dependency_trap prediction (upward redistribution beneficiaries defending it) or the freedom_floor prediction (autonomy-seeking beneficiaries defending it)?',
    'If the freedom_floor coalition structure dominates in practice, this reading''s beneficiary/victim structure would need revision; if the dependency_trap coalition (non-needy defenders, working-poor objectors) dominates, this reading is corroborated. This is the conceptual under-determination between the two coherent framings of the same kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Alternative framing of the same kernel text produces different beneficiary structures; documents the framing choice made in this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__dependency_trap_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__dependency_trap_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(unco_tr_t4, unconditional_income_support__dependency_trap_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement(unco_tr_t8, unconditional_income_support__dependency_trap_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(unco_tr_t12, unconditional_income_support__dependency_trap_reading, theater_ratio, 12, 0.31).
narrative_ontology:measurement(unco_tr_t16, unconditional_income_support__dependency_trap_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement(unco_tr_t20, unconditional_income_support__dependency_trap_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(unco_tr_t24, unconditional_income_support__dependency_trap_reading, theater_ratio, 24, 0.38).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__dependency_trap_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(unco_be_t4, unconditional_income_support__dependency_trap_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(unco_be_t8, unconditional_income_support__dependency_trap_reading, base_extractiveness, 8, 0.57).
narrative_ontology:measurement(unco_be_t12, unconditional_income_support__dependency_trap_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(unco_be_t16, unconditional_income_support__dependency_trap_reading, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(unco_be_t20, unconditional_income_support__dependency_trap_reading, base_extractiveness, 20, 0.69).
narrative_ontology:measurement(unco_be_t24, unconditional_income_support__dependency_trap_reading, base_extractiveness, 24, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(unco_su_t0, unconditional_income_support__dependency_trap_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(unco_su_t4, unconditional_income_support__dependency_trap_reading, suppression_requirement, 4, 0.3).
narrative_ontology:measurement(unco_su_t8, unconditional_income_support__dependency_trap_reading, suppression_requirement, 8, 0.33).
narrative_ontology:measurement(unco_su_t12, unconditional_income_support__dependency_trap_reading, suppression_requirement, 12, 0.36).
narrative_ontology:measurement(unco_su_t16, unconditional_income_support__dependency_trap_reading, suppression_requirement, 16, 0.39).
narrative_ontology:measurement(unco_su_t20, unconditional_income_support__dependency_trap_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(unco_su_t24, unconditional_income_support__dependency_trap_reading, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__dependency_trap_reading, resource_allocation).
narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, unconditional_income_support__freedom_floor_reading).
narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, unconditional_income_support__universality_paradox_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the unconditional_income_support kernel, decomposed per the ε-invariance principle because the natural-language label 'UBI' conflates structurally distinct claims about the same payment mechanism. dependency_trap_reading (this file, snare, ε=0.71) treats the transfer as an incentive-distorting substitute for targeted aid. freedom_floor_reading (rope/mountain-leaning, low ε) treats the identical payment as a coercion-removing autonomy floor. universality_paradox_reading (tangled_rope-leaning) treats the cross-ideological convergence itself as the structurally interesting fact. All three share the kernel text but author independent ε, beneficiaries, victims, and claimed_type from their own reading's lights, linked here via affects_constraints rather than merged into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
