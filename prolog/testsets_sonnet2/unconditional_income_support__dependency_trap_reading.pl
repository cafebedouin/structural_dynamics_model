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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Universal Basic Income as Program-Replacement Extraction Mechanism
 *   domain: political_economy/social_policy
 *
 * SUMMARY:
 *   This story authors the dependency-trap reading of the
 *   unconditional-income-support kernel: UBI framed as universality is, on
 *   this reading, an incentive-distorting mechanism that crowds out
 *   higher-value targeted aid for the working poor, funds transfers to the
 *   non-needy middle and upper classes, and imposes a substantial net fiscal
 *   cost on taxpayers while measurably reducing labor-force participation
 *   among marginal workers. This is one of three structurally distinct
 *   constraints emitted from the same underlying policy kernel — the
 *   freedom_floor_reading and universality_paradox_reading are separate
 *   constraints with different beneficiary/victim structures and different
 *   epsilon values, not alternative framings of this same constraint. The
 *   referent for extractiveness here is the standing
 *   UBI-with-program-replacement arrangement as this reading characterizes
 *   it, not the reading's preferred targeted-aid alternative.
 *
 * KEY AGENTS:
 *   - middle_upper_class_recipients: beneficiary (organized/arbitrage) — collects transfer without need
 *   - ubi_advocacy_coalitions: beneficiary/agenda_setter (organized/mobile) — collects political capital, pushes program replacement
 *   - working_poor_program_losers: payer (powerless/trapped) — loses higher-value targeted aid
 *   - general_taxpayers: payer (moderate/constrained) — bears net fiscal cost
 *   - labor_market_marginal_workers: payer/beneficiary (powerless/constrained) — trades wage income for transfer, employment effect measured
 *   - state_welfare_administrators: observer (institutional/analytical) — designs the transition, mediates competing pressures
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__dependency_trap_reading, 0.78).
domain_priors:suppression_score(unconditional_income_support__dependency_trap_reading, 0.42).
domain_priors:theater_ratio(unconditional_income_support__dependency_trap_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__dependency_trap_reading, snare).
narrative_ontology:human_readable(unconditional_income_support__dependency_trap_reading, "Universal Basic Income as Program-Replacement Extraction Mechanism").
narrative_ontology:topic_domain(unconditional_income_support__dependency_trap_reading, "political_economy/social_policy").

domain_priors:requires_active_enforcement(unconditional_income_support__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__dependency_trap_reading, 'b8746db2-a649-4a97-9f5c-e747d14252a9').
narrative_ontology:cs_kernel_codification('b8746db2-a649-4a97-9f5c-e747d14252a9', distributed).
narrative_ontology:cs_authority_grounding('b8746db2-a649-4a97-9f5c-e747d14252a9', distributed).
narrative_ontology:cs_reading_relation('b8746db2-a649-4a97-9f5c-e747d14252a9', unconditional_income_support__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('b8746db2-a649-4a97-9f5c-e747d14252a9', unconditional_income_support__universality_paradox_reading, influences).
narrative_ontology:cs_axiom('b8746db2-a649-4a97-9f5c-e747d14252a9', foundational, conditionality_improves_targeting_efficiency).
narrative_ontology:cs_axiom_status(conditionality_improves_targeting_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('b8746db2-a649-4a97-9f5c-e747d14252a9', conditionality_improves_targeting_efficiency, empirically_contingent).
narrative_ontology:cs_axiom('b8746db2-a649-4a97-9f5c-e747d14252a9', secondary, universal_transfers_necessarily_dilute_aid_to_neediest).
narrative_ontology:cs_axiom_status(universal_transfers_necessarily_dilute_aid_to_neediest, holdable).
narrative_ontology:cs_axiom_grounding('b8746db2-a649-4a97-9f5c-e747d14252a9', universal_transfers_necessarily_dilute_aid_to_neediest, empirically_contingent).
narrative_ontology:cs_reference_frame('b8746db2-a649-4a97-9f5c-e747d14252a9', targeted_means_tested_welfare_baseline).
narrative_ontology:cs_drift_state('b8746db2-a649-4a97-9f5c-e747d14252a9', post_large_scale_pilot_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b8746db2-a649-4a97-9f5c-e747d14252a9', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__dependency_trap_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, middle_upper_class_recipients).
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, ubi_advocacy_coalitions).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, working_poor_program_losers).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, general_taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, labor_market_marginal_workers).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, labor_market_marginal_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive the same flat unconditional transfer as everyone else despite not needing income support. The universal design means they collect a check funded substantially by the tax base and by the replacement of programs targeted at people with greater need. Their political support for universality — 'everyone gets it, so no one is stigmatized' — is what makes the transfer politically durable even though it delivers no marginal benefit to their welfare.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, middle_upper_class_recipients, beneficiary,
    organized, biographical, arbitrage, national).

% Political and ideological actors who built careers, think-tank funding, and movement infrastructure around universality as a principle. They gain political capital, donor interest, and institutional standing from the program's existence regardless of its labor-market or poverty outcomes, and they actively push for program replacement (folding targeted aid into UBI) to demonstrate the model's fiscal viability.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, ubi_advocacy_coalitions, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__dependency_trap_reading, ubi_advocacy_coalitions, agenda_setter).

% Previously received means-tested programs (housing vouchers, SNAP, targeted childcare subsidies, EITC-style wage supplements) calibrated to their specific needs, often worth more in combination than the flat UBI payment. When UBI replaces or crowds out these programs to fund universality, this group experiences a net decrease in real support despite the headline 'everyone gets money' framing. They have no meaningful exit — reduced targeted aid cannot be replaced by re-enrolling in programs that have been defunded or eliminated.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, working_poor_program_losers, payer,
    powerless, immediate, trapped, local).

% Bear the net fiscal cost after program offsets — estimated near $1.4 trillion annually at national scale — through taxation, debt-financed spending, or inflationary pressure. They fund transfers to non-needy recipients as the price of the program's universal design, without the option to opt out of the tax obligation while remaining inside the jurisdiction.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, general_taxpayers, payer,
    moderate, biographical, constrained, national).

% Workers at the margin of labor force participation who, according to large pilot meta-analyses, reduce hours or exit work in response to the unconditional payment (measured at roughly -3.2% employment effect in large trials). They gain the payment itself but lose wage income, workplace-based skill accumulation, and long-run earnings trajectory — a trade the dependency-trap reading treats as a net harm disguised as support.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, labor_market_marginal_workers, payer,
    powerless, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__dependency_trap_reading, labor_market_marginal_workers, beneficiary).

% Design and administer the transition from targeted programs to universal payments, commissioning pilot studies and cost-benefit analyses. They mediate between advocacy pressure for universality and empirical findings on employment and poverty-targeting efficiency, and their administrative choices determine how much targeted aid is actually crowded out versus preserved alongside UBI.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, state_welfare_administrators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unconditional_income_support__dependency_trap_reading, middle_upper_class_recipients).
narrative_ontology:fixing_cost_class(unconditional_income_support__dependency_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reduces administrative complexity and stigma by replacing a patchwork of means-tested programs with a single unconditional transfer, in principle simplifying delivery and removing bureaucratic gatekeeping.
% TRANSFER_FUNCTION: Moves tax revenue and displaced targeted-program funding from taxpayers and the working poor (who lose higher-value calibrated aid) to all residents including the non-needy middle and upper class, while advocacy coalitions collect political capital from the program's universal framing.
% ABSENT_VOICES: Former recipients of specific targeted programs (disability-specific aid, childcare subsidies, housing vouchers) rarely get a distinct seat in the UBI design conversation — their loss is aggregated into 'the poor' broadly, obscuring that the flat payment may be worth substantially less than what was previously received.
% DISAPPEARANCE_RATIONALE: If the unconditional transfer vanished, targeted programs would need to be reconstituted or the working poor would face an outright benefit cliff; taxpayers would see the $1.4 trillion fiscal burden lifted; UBI advocacy institutions would lose their central policy vehicle. The labor-market participation effects would also reverse for the marginal-worker population, restoring pre-transfer employment patterns.
% FOUNDING_PROBLEM: The stated founding problem was welfare-program fragmentation, stigma in means-tested aid, and the coming disruption of labor markets by automation — UBI was framed as a simpler, dignity-preserving, future-proof replacement.
% FOUNDING_PROBLEM_CORROBORATION: AEI and similar center-right policy analysts, independent of UBI advocacy funding, attest via meta-analysis of large pilots (e.g., the -3.2% employment effect finding) that the labor-market disruption problem is being worsened rather than solved, and that targeted-program replacement produces net losses for the working poor rather than simplification gains. UBI advocacy organizations themselves continue to assert the founding problem remains live and unsolved; the corroborating outside source is the pilot-evaluation literature, not the advocacy coalitions.
narrative_ontology:disappearance_verdict(unconditional_income_support__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__dependency_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unconditional_income_support__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__dependency_trap_reading, 0.78, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.78) reflecting the AEI meta-analytic finding of a -3.2% employment effect in large pilots plus the estimated $1.4 trillion net fiscal cost after program offsets — both read, on this reading, as evidence the arrangement functions as extraction dressed as universal support. Suppression is moderate (0.42): there is no direct coercion preventing exit from the labor market response, but program replacement removes the alternative of retaining higher-value targeted aid, which functions as a soft suppression of choice. Theater ratio rises over the interval (0.30 to 0.55) as the political framing of 'universal dignity' increasingly substitutes for demonstrated poverty-reduction or labor-market outcomes — a Goodhart-style drift where universality itself becomes the measured success criterion rather than the welfare of the working poor. Accessibility collapse is moderate (0.5): once targeted programs are defunded to pay for universality, reconstituting them is politically and administratively difficult, though not impossible. Resistance is meaningfully high (0.68), reflecting active political contestation from targeted-aid advocates, labor economists, and fiscal conservatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Middle and upper class recipients and UBI advocacy coalitions derive low directionality (near the beneficiary end) — they collect the transfer or the political capital without bearing proportionate cost. Working poor program losers and general taxpayers derive high directionality (near the full-target end): the former lose calibrated aid worth more than the flat payment, and are trapped with no meaningful exit; the latter fund the arrangement through taxation they cannot escape while remaining in-jurisdiction. Labor-market marginal workers occupy a dual position — beneficiary of the direct payment, payer through forgone wage income and reduced long-run earnings — which the story captures via a secondary role rather than forcing a single directionality value.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (welfare fragmentation, stigma, automation-driven labor disruption) has genuine live elements, which prevents this reading from being classified as pure mandatrophy — the coordination story is not entirely cover. But the program-replacement mechanism substitutes a flatter, less-calibrated instrument for a more targeted one whose function has not disappeared (the working poor's needs are not lower now than before), meaning the classification as snare rather than tangled_rope hinges on whether any genuine coordination benefit survives the replacement. This reading's authored judgment is that it does not survive intact — the coordination story becomes cover for a redistribution-upward outcome.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    employment_effect_generalizability,
    'Do the -3.2% employment effects observed in large UBI pilots generalize to permanent, nationwide, unconditional programs, or are pilot effects artifacts of temporary/bounded program design (income effects differ when recipients know the transfer is time-limited)?',
    'Long-run natural experiments from jurisdictions implementing permanent universal transfers (e.g., extended tracking of Alaska Permanent Fund dividend recipients, or any future permanent national UBI) compared against time-limited pilot cohorts.',
    'If pilot effects are artifacts of temporariness, the dependency-trap reading''s central empirical claim weakens substantially and the classification should be revisited toward tangled_rope; if effects persist or amplify in permanent programs, the snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employment_effect_generalizability, empirical, 'Whether pilot-study labor effects generalize to permanent unconditional programs.').

omega_variable(
    program_replacement_necessity,
    'Is program replacement (defunding targeted aid to fund universal transfers) a necessary feature of UBI implementation, or a contingent political-fiscal choice that could be avoided by funding UBI through new revenue rather than substitution?',
    'Comparative analysis of UBI proposals and pilots that fund the transfer through new taxation/sovereign wealth versus those that fund it through means-tested program consolidation.',
    'If program replacement is contingent rather than necessary, the victim structure (working poor losing higher-value targeted aid) is an implementation choice, not an intrinsic feature of unconditional income support — this would separate the dependency-trap reading''s harshest claims from the kernel itself and attribute them to a specific financing design.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(program_replacement_necessity, conceptual, 'Whether targeted-program crowd-out is intrinsic to UBI or a contingent financing choice.').

omega_variable(
    reading_selection_and_kernel_ambiguity,
    'Given that the same policy kernel supports the dependency_trap_reading, freedom_floor_reading, and universality_paradox_reading with substantially different epsilon values and even opposite beneficiary/victim assignments, what determines which reading a given real-world implementation actually instantiates?',
    'Case-by-case structural analysis of specific UBI implementations: financing mechanism (new revenue vs. program replacement), transfer amount relative to prior targeted-aid value, and whether labor-market participation is tracked as a design metric. Different implementations may genuinely instantiate different readings rather than all readings being equally applicable to all implementations.',
    'If implementation details determine the reading, this constraint''s classification (snare) applies specifically to program-replacement-financed UBI with below-adequacy transfer amounts, not to unconditional income support as such — which would mean the kernel-level ambiguity is resolved by empirical implementation facts rather than remaining permanently contested.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_and_kernel_ambiguity, conceptual, 'Whether kernel readings map onto implementation variants rather than remaining perpetually rival framings of one arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__dependency_trap_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__dependency_trap_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(unco_tr_t4, unconditional_income_support__dependency_trap_reading, theater_ratio, 4, 0.35).
narrative_ontology:measurement(unco_tr_t8, unconditional_income_support__dependency_trap_reading, theater_ratio, 8, 0.4).
narrative_ontology:measurement(unco_tr_t12, unconditional_income_support__dependency_trap_reading, theater_ratio, 12, 0.46).
narrative_ontology:measurement(unco_tr_t16, unconditional_income_support__dependency_trap_reading, theater_ratio, 16, 0.5).
narrative_ontology:measurement(unco_tr_t20, unconditional_income_support__dependency_trap_reading, theater_ratio, 20, 0.53).
narrative_ontology:measurement(unco_tr_t24, unconditional_income_support__dependency_trap_reading, theater_ratio, 24, 0.55).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__dependency_trap_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(unco_be_t4, unconditional_income_support__dependency_trap_reading, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(unco_be_t8, unconditional_income_support__dependency_trap_reading, base_extractiveness, 8, 0.62).
narrative_ontology:measurement(unco_be_t12, unconditional_income_support__dependency_trap_reading, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(unco_be_t16, unconditional_income_support__dependency_trap_reading, base_extractiveness, 16, 0.72).
narrative_ontology:measurement(unco_be_t20, unconditional_income_support__dependency_trap_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(unco_be_t24, unconditional_income_support__dependency_trap_reading, base_extractiveness, 24, 0.78).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(unconditional_income_support__dependency_trap_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__dependency_trap_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(unconditional_income_support__dependency_trap_reading, 0.12).
narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, freedom_floor_reading).
narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, universality_paradox_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposed from the unconditional_income_support kernel per the epsilon-invariance principle. dependency_trap_reading authors epsilon=0.78 (snare) for the program-replacement-financed arrangement as this reading characterizes it; freedom_floor_reading authors a substantially lower epsilon for the same underlying policy text read as autonomy-enabling coordination; universality_paradox_reading treats the cross-ideological convergence itself as the structurally salient feature. All three link to each other via affects_constraints because they share a kernel and a political battleground — advocacy or empirical developments favoring one reading structurally pressure the legitimacy conditions of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
