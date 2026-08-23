% ============================================================================
% CONSTRAINT STORY: income_support_commitment__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__freedom_floor_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: income_support_commitment__freedom_floor_reading
 *   human_readable: Unconditional Income Support as Freedom Floor
 *   domain: political_economy/social_policy/welfare_state
 *
 * SUMMARY:
 *   This constraint story instantiates the freedom_floor_reading of the
 *   income_support_commitment kernel — the position that unconditional,
 *   universal income support functions as a structural enabler of autonomy,
 *   dignity, and labor market exit capacity. Unlike conditional welfare
 *   regimes that extract compliance through means-testing and work
 *   requirements, this reading treats the floor as a coordination solution: a
 *   universal payment infrastructure that solves the collective-action
 *   problem of poverty without the extractive overhead of surveillance,
 *   stigma, and bureaucratic gatekeeping. The claimed type is rope — a
 *   genuine coordination mechanism with minimal coercive overhead — though
 *   political resistance to funding (taxation) creates implementation
 *   friction captured in the resistance metric. The measurement series tracks
 *   the historical trajectory from negative income tax experiments (1970s,
 *   high administrative complexity, residual stigma) through modern universal
 *   basic income pilots (2010s-present, lower theater, lower suppression) to
 *   the current policy design consensus.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__freedom_floor_reading, 0.18).
domain_priors:suppression_score(income_support_commitment__freedom_floor_reading, 0.1).
domain_priors:theater_ratio(income_support_commitment__freedom_floor_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__freedom_floor_reading, rope).
narrative_ontology:human_readable(income_support_commitment__freedom_floor_reading, "Unconditional Income Support as Freedom Floor").
narrative_ontology:topic_domain(income_support_commitment__freedom_floor_reading, "political_economy/social_policy/welfare_state").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__freedom_floor_reading, '67db2632-067e-43c3-8897-850680e2a97d').
narrative_ontology:cs_kernel_codification('67db2632-067e-43c3-8897-850680e2a97d', distributed).
narrative_ontology:cs_authority_grounding('67db2632-067e-43c3-8897-850680e2a97d', practice).
narrative_ontology:cs_interpretation_layer_present('67db2632-067e-43c3-8897-850680e2a97d').
narrative_ontology:cs_reading_relation('67db2632-067e-43c3-8897-850680e2a97d', income_support_commitment__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('67db2632-067e-43c3-8897-850680e2a97d', income_support_commitment__targeting_efficiency_reading, influences).
narrative_ontology:cs_axiom('67db2632-067e-43c3-8897-850680e2a97d', foundational, unconditional_floor_enables_autonomy).
narrative_ontology:cs_axiom_status(unconditional_floor_enables_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('67db2632-067e-43c3-8897-850680e2a97d', unconditional_floor_enables_autonomy, deontological).
narrative_ontology:cs_axiom('67db2632-067e-43c3-8897-850680e2a97d', secondary, universality_eliminates_stigma).
narrative_ontology:cs_axiom_status(universality_eliminates_stigma, holdable).
narrative_ontology:cs_axiom_grounding('67db2632-067e-43c3-8897-850680e2a97d', universality_eliminates_stigma, empirically_contingent).
narrative_ontology:cs_reference_frame('67db2632-067e-43c3-8897-850680e2a97d', conditional_welfare_regime).
narrative_ontology:cs_drift_state('67db2632-067e-43c3-8897-850680e2a97d', post_pilot_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('67db2632-067e-43c3-8897-850680e2a97d', '').
narrative_ontology:cs_kernel_id(income_support_commitment__freedom_floor_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, caregivers).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, abuse_survivors).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, artists_entrepreneurs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, employers).
narrative_ontology:constraint_victim(income_support_commitment__freedom_floor_reading, employers).
narrative_ontology:constraint_victim(income_support_commitment__freedom_floor_reading, taxpayers).
narrative_ontology:constraint_vindicates(income_support_commitment__freedom_floor_reading, autonomy_as_freedom).
narrative_ontology:constraint_vindicates(income_support_commitment__freedom_floor_reading, dignity_as_entitlement).
narrative_ontology:constraint_vindicates(income_support_commitment__freedom_floor_reading, exit_capacity_as_power_balance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Perform unpaid care work that current conditional systems penalize or render invisible. The universal floor recognizes this work as socially necessary and provides independent income without means-testing gatekeepers or work-search requirements that conflict with care obligations.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, caregivers, beneficiary,
    moderate, biographical, constrained, national).

% Face volatile schedules, low wages, and employer power amplified by lack of credible exit threat. The floor converts survival jobs into genuine choices — they can refuse exploitative conditions, invest in skills, or combine gig work with stability.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, precarious_workers, beneficiary,
    powerless, immediate, constrained, national).

% Currently trapped in abusive relationships or households by economic dependence. Conditional benefits often require household-level means testing that binds them to abusers. An individual, universal floor provides the material basis for exit without disclosure or bureaucratic permission.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, abuse_survivors, beneficiary,
    powerless, immediate, trapped, local).

% Bear high income variance and startup risk that current systems punish — benefits phase out as earnings rise, creating high effective marginal tax rates. The floor acts as venture capital for creative and entrepreneurial risk-taking without cliff effects.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, artists_entrepreneurs, beneficiary,
    moderate, biographical, mobile, national).

% Face upward wage pressure as workers gain credible exit capacity — must offer conditions that attract rather than coerce. Also benefit from a healthier, more autonomous workforce and reduced turnover costs. The secondary beneficiary role reflects long-term productivity gains that offset wage-floor costs.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, employers, payer,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__freedom_floor_reading, employers, beneficiary).

% Fund the universal floor through progressive taxation. High-income taxpayers bear net cost; low-income taxpayers are net recipients. The universality eliminates the stigma and administrative overhead of means-testing, reducing the coordination cost of the transfer system itself.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, taxpayers, payer,
    organized, biographical, constrained, national).

% Design and administer the universal payment infrastructure. Their role shifts from gatekeeping eligibility (means-testing, conditionality enforcement) to operational reliability — payment delivery, fraud prevention at systems level, and integration with tax administration.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, policy_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% The administrative apparatus of conditional welfare — caseworkers, fraud investigators, eligibility algorithms — whose function is rendered obsolete by universality. They would object to the reading as it eliminates their institutional rationale and employment base.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, means_testing_bureaucracy, excluded,
    organized, biographical, constrained, national).

% Study pilot data (Finland, Canada, Kenya, Spain, US negative income tax experiments) on labor supply effects, stigma reduction, administrative cost savings, and agency outcomes. Their analyses inform but do not determine the political contest.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, welfare_economists, observer,
    analytical, civilizational, analytical, global).

% Provide the normative architecture: republican freedom as non-domination (Pettit), feminist critique of the gendered welfare contract (Pateman, Fraser), real libertarianism (Van Parijs). They articulate the freedom_floor_reading's intellectual genealogy.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, political_theorists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of providing a universal economic floor that enables genuine labor market exit capacity without means-testing stigma, bureaucratic exclusion, or the poverty traps inherent in conditional welfare systems.
% TRANSFER_FUNCTION: Moves resources from the general progressive tax base to all residents universally as an individual entitlement, enabling exit from coercive labor arrangements, recognizing unpaid care work, and providing independent means for abuse survivors — without household-level means testing or work-search conditionality.
% ABSENT_VOICES: Means-testing administrators, behavioral paternalists who believe conditionality is necessary for social order, and employers who benefit from monopsony power in low-wage labor markets — these groups are structurally excluded from the reading's beneficiary coalition but would contest it in implementation.
% DISAPPEARANCE_RATIONALE: If the universal floor vanished overnight, workers would lose credible exit threat and wage-setting power would revert to employers; caregivers would lose recognition and independent income; abuse survivors would lose material basis for escape; precarious workers would face intensified coercion; the poverty traps and stigma of conditional systems would reassert fully.
% FOUNDING_PROBLEM: The problem of poverty traps, stigma, and bureaucratic exclusion in conditional welfare systems that undermine autonomy, fail to provide genuine security, and bind recipients to caseworker discretion — while simultaneously subsidizing low-wage employers through in-work benefits that depress wage floors.
% FOUNDING_PROBLEM_CORROBORATION: Cross-party welfare reform commissions (UK 1999, Finland 2017), feminist political economy (Nancy Fraser on the crisis of social reproduction, Carole Pateman on the sexual contract), basic income pilots (Finland 2017-18, Ontario 2017-19, Kenya ongoing, Spain Minimum Vital Income 2020) showing reduced stigma, improved agency, and no significant labor supply reduction — all from sources outside the direct beneficiary set.
narrative_ontology:disappearance_verdict(income_support_commitment__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__freedom_floor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(income_support_commitment__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__freedom_floor_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__freedom_floor_reading_tests).
:- end_tests(income_support_commitment__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the transfer is universal and funded through progressive taxation — the coordination cost is the tax administration overhead, not extraction from a target population. Suppression is very low (0.10) because universality eliminates the surveillance, sanction regimes, and household-level means testing that characterize conditional systems. Theater ratio is low (0.12) and declining: early negative income tax experiments retained conditional administrative structures (high theater); modern UBI pilots and policy designs (e.g. Alaska Permanent Fund, Iran's universal cash transfer) approach pure payment infrastructure. Accessibility collapse is moderate (0.35): alternatives (conditional welfare, charity, family dependence) persist but are structurally inferior for the beneficiary groups. Resistance (0.42) reflects political contestation over tax levels and moral-desert narratives, not operational resistance from beneficiaries.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute divergent per-seat classifications: for caregivers, precarious workers, and abuse survivors the constraint reads as rope (net beneficiaries with constrained exit but genuine coordination gain); for employers it reads as tangled_rope (coordination benefit of healthier workforce + asymmetric cost of constrained monopsony power); for taxpayers it reads as rope (coordination gain from simplified administration vs. tax cost); for means-testing bureaucracy it reads as snare (pure extraction of their institutional rationale). The freedom_floor_reading deliberately authors zero victims — universality is the structural move that eliminates the victim class inherent in conditional systems.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (caregivers, precarious workers, abuse survivors, artists/entrepreneurs) receive net subsidy — the floor exceeds their tax contribution, and universality eliminates the stigma/access costs of conditional systems. Their exit_options range from trapped (abuse survivors) to mobile (artists/entrepreneurs), but all gain credible exit capacity they previously lacked. Payers: employers bear wage-floor costs but gain workforce stability (secondary beneficiary); taxpayers bear net fiscal cost proportional to income. The agenda_setter (policy administrators) shifts from gatekeeping to payment operations — their directionality is near-symmetric (d ~ 0.5). The excluded (means-testing bureaucracy) face institutional obsolescence but are not 'victims' in the extraction sense — their role is eliminated by the coordination solution itself.
 *
 * MANDATROPHY ANALYSIS:
 *   Not applicable — this constraint is not a degraded mandate. The founding problem (poverty traps, stigma, bureaucratic exclusion in conditional welfare) remains live and the freedom_floor_reading proposes a structural solution, not a persistence of an obsolete arrangement. The mandatrophy question applies to the sibling dependency_trap_reading, which treats the *absence* of conditionality as a mandate that has outlived its function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint is the freedom_floor_reading of the income_support_commitment kernel; sibling readings are dependency_trap_reading and targeting_efficiency_reading. What structural elements distinguish this reading''s ε-invariant constraint from its siblings?',
    'Decompose the kernel into three constraint stories per ε-invariance principle: each reading gets its own ε, beneficiary/victim structure, and classification. The freedom_floor_reading authors ε=0.18, zero victims, rope; dependency_trap_reading would author high ε from taxpayer/employer seat, snare/tangled_rope; targeting_efficiency_reading would author moderate ε with means-test administrative costs, scaffold/tangled_rope.',
    'If the readings are not decomposed, the single constraint would have observer-dependent ε — violating DP-001. The decomposition enables structural comparison: freedom_floor eliminates the victim class that dependency_trap centers; targeting_efficiency retains means-test suppression that freedom_floor eliminates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment-system framing: one kernel, three ε-invariant constraints.').

omega_variable(
    tax_funding_extraction_ambiguity,
    'Does the progressive tax funding mechanism constitute extraction from high-income taxpayers, or is it the coordination cost of a universal floor that benefits all including taxpayers (through simplified administration, reduced poverty externalities, social stability)?',
    'Compare administrative cost ratios: conditional welfare systems (means-testing, fraud investigation, casework) vs. universal payment infrastructure (Alaska Permanent Fund ~0.5% admin cost, Iran universal transfer ~1%). If universal floor reduces total system cost despite higher gross transfer, the tax is coordination cost not extraction.',
    'If tax funding is extraction, ε rises for taxpayer seat → tangled_rope from that seat. If coordination cost, ε stays low → rope from all seats. This determines whether the constraint is pure coordination or hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tax_funding_extraction_ambiguity, empirical, 'Whether the tax side of the universal floor is extractive or coordinative.').

omega_variable(
    employer_power_delta,
    'Is the constraint on employer wage-setting power (via worker exit capacity) a coordination benefit — reducing monopsony exploitation — or extraction from employers?',
    'Labor economics evidence: monopsony power measurements in low-wage markets (Azar et al., Berger et al.), minimum wage studies, UBI pilot labor supply effects. If employer surplus was monopsony rent, the constraint is coordination (reducing market failure). If employer surplus was competitive return, the constraint extracts.',
    'If coordination benefit, employer secondary_role=beneficiary is structurally primary → rope from employer seat. If extraction, employer role=payer only → tangled_rope from employer seat. Changes the seat-level classification map.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employer_power_delta, empirical, 'Whether employer cost is monopsony correction or extraction.').

omega_variable(
    stigma_elimination_mechanism,
    'Does universality actually eliminate stigma, or does stigma migrate to new markers (e.g., ''UBI recipient'' as a cultural category, or stigma attaching to those who *only* have the floor)?',
    'Longitudinal qualitative studies from pilots (Finland, Kenya, Stockton) tracking stigma narratives over time. Compare with Alaska Permanent Fund (universal, 40+ years) — no stigma observed. If stigma persists/migrates, suppression and accessibility_collapse metrics understate residual coercion.',
    'If stigma migrates, suppression > 0.10 and accessibility_collapse > 0.35 — the rope classification holds but with higher coordination overhead. If stigma is eliminated, current metrics are accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stigma_elimination_mechanism, empirical, 'Whether universality structurally eliminates stigma or displaces it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__freedom_floor_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__freedom_floor_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(inco_tr_t10, income_support_commitment__freedom_floor_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(inco_tr_t20, income_support_commitment__freedom_floor_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(inco_tr_t30, income_support_commitment__freedom_floor_reading, theater_ratio, 30, 0.13).
narrative_ontology:measurement(inco_tr_t40, income_support_commitment__freedom_floor_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement(inco_tr_t50, income_support_commitment__freedom_floor_reading, theater_ratio, 50, 0.12).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__freedom_floor_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(inco_be_t10, income_support_commitment__freedom_floor_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(inco_be_t20, income_support_commitment__freedom_floor_reading, base_extractiveness, 20, 0.2).
narrative_ontology:measurement(inco_be_t30, income_support_commitment__freedom_floor_reading, base_extractiveness, 30, 0.19).
narrative_ontology:measurement(inco_be_t40, income_support_commitment__freedom_floor_reading, base_extractiveness, 40, 0.18).
narrative_ontology:measurement(inco_be_t50, income_support_commitment__freedom_floor_reading, base_extractiveness, 50, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_commitment__freedom_floor_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(inco_su_t10, income_support_commitment__freedom_floor_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(inco_su_t20, income_support_commitment__freedom_floor_reading, suppression_requirement, 20, 0.18).
narrative_ontology:measurement(inco_su_t30, income_support_commitment__freedom_floor_reading, suppression_requirement, 30, 0.12).
narrative_ontology:measurement(inco_su_t40, income_support_commitment__freedom_floor_reading, suppression_requirement, 40, 0.1).
narrative_ontology:measurement(inco_su_t50, income_support_commitment__freedom_floor_reading, suppression_requirement, 50, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__freedom_floor_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_commitment__freedom_floor_reading, 0.1).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, income_support_commitment__dependency_trap_reading).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, income_support_commitment__targeting_efficiency_reading).

% DUAL FORMULATION NOTE:
% Kernel decomposition per ε-invariance: income_support_commitment kernel has three readings with distinct ε and beneficiary/victim structures. freedom_floor_reading (this story) = universal floor, ε=0.18, zero victims, rope. dependency_trap_reading = conditional/work-required, high ε from taxpayer seat, victims = taxpayers/employers, snare/tangled_rope. targeting_efficiency_reading = means-tested, moderate ε with admin costs, victims = excluded needy, scaffold/tangled_rope. All three linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(income_support_commitment__freedom_floor_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
