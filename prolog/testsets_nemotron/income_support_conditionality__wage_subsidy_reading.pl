% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__wage_subsidy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_conditionality__wage_subsidy_reading, []).

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
 *   constraint_id: income_support_conditionality__wage_subsidy_reading
 *   human_readable: Unconditional Income Support as Employer Wage Subsidy
 *   domain: political_economy/social_policy/labor_economics
 *
 * SUMMARY:
 *   This constraint story models the wage_subsidy_reading of the
 *   income_support_conditionality kernel. Unconditional income support (UBI,
 *   negative income tax, or similar universal floor) is structurally read as
 *   an employer subsidy: the payment creates a known floor that employers can
 *   wage-set against, depressing market wages toward the payment level while
 *   workers' total income remains at subsistence. The coordination function
 *   (universal floor, administrative simplicity) is genuine but the
 *   extraction function (employer capture via wage suppression) is asymmetric
 *   and requires active enforcement of the low-wage labor market structure
 *   (anti-union policy, precarious contract legality, monopsony tolerance).
 *   The constraint is tangled_rope because both functions operate
 *   simultaneously through the same structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__wage_subsidy_reading, 0.68).
domain_priors:suppression_score(income_support_conditionality__wage_subsidy_reading, 0.55).
domain_priors:theater_ratio(income_support_conditionality__wage_subsidy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__wage_subsidy_reading, tangled_rope).
narrative_ontology:human_readable(income_support_conditionality__wage_subsidy_reading, "Unconditional Income Support as Employer Wage Subsidy").
narrative_ontology:topic_domain(income_support_conditionality__wage_subsidy_reading, "political_economy/social_policy/labor_economics").

domain_priors:requires_active_enforcement(income_support_conditionality__wage_subsidy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__wage_subsidy_reading, '2416ad3a-1d8d-4ba0-9dac-2f422f2c2b06').
narrative_ontology:cs_kernel_codification('2416ad3a-1d8d-4ba0-9dac-2f422f2c2b06', formalized).
narrative_ontology:cs_authority_grounding('2416ad3a-1d8d-4ba0-9dac-2f422f2c2b06', extraction).
narrative_ontology:cs_interpretation_layer_present('2416ad3a-1d8d-4ba0-9dac-2f422f2c2b06').
narrative_ontology:cs_reading_relation('2416ad3a-1d8d-4ba0-9dac-2f422f2c2b06', income_support_conditionality__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('2416ad3a-1d8d-4ba0-9dac-2f422f2c2b06', income_support_conditionality__dependency_trap_reading, influences).
narrative_ontology:cs_axiom('2416ad3a-1d8d-4ba0-9dac-2f422f2c2b06', foundational, universal_floor_enables_wage_suppression).
narrative_ontology:cs_axiom_status(universal_floor_enables_wage_suppression, holdable).
narrative_ontology:cs_axiom_grounding('2416ad3a-1d8d-4ba0-9dac-2f422f2c2b06', universal_floor_enables_wage_suppression, empirically_contingent).
narrative_ontology:cs_axiom('2416ad3a-1d8d-4ba0-9dac-2f422f2c2b06', foundational, labor_market_monopsony_captures_transfers).
narrative_ontology:cs_axiom_status(labor_market_monopsony_captures_transfers, holdable).
narrative_ontology:cs_axiom_grounding('2416ad3a-1d8d-4ba0-9dac-2f422f2c2b06', labor_market_monopsony_captures_transfers, empirically_contingent).
narrative_ontology:cs_reference_frame('2416ad3a-1d8d-4ba0-9dac-2f422f2c2b06', post_work_emancipation_promise).
narrative_ontology:cs_drift_state('2416ad3a-1d8d-4ba0-9dac-2f422f2c2b06', contemporary_monopsony_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2416ad3a-1d8d-4ba0-9dac-2f422f2c2b06', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__wage_subsidy_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, low_wage_employers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, platform_gig_economy_firms).
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, retail_hospitality_chains).
narrative_ontology:constraint_victim(income_support_conditionality__wage_subsidy_reading, low_wage_workers).
narrative_ontology:constraint_victim(income_support_conditionality__wage_subsidy_reading, precarious_contract_workers).
narrative_ontology:constraint_victim(income_support_conditionality__wage_subsidy_reading, unemployed_job_seekers).
narrative_ontology:constraint_vindicates(income_support_conditionality__wage_subsidy_reading, labor_market_flexibility_doctrine).
narrative_ontology:constraint_vindicates(income_support_conditionality__wage_subsidy_reading, reserve_army_of_labor_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Employ workers at wages below subsistence because the public income floor covers the gap. They capture the subsidy by reducing wage offers knowing workers cannot exit the labor market entirely. Benefit from reduced turnover costs and weaker bargaining position of workers who have a survival floor but no exit power.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, low_wage_employers, beneficiary,
    powerful, biographical, mobile, national).

% Structure gig work around the unconditional payment as a baseline, setting piece rates that assume the worker's survival is externally subsidized. The platform's algorithmic management extracts the full surplus between the worker's reservation wage and the platform's take rate, with the public floor absorbing the downside risk.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, platform_gig_economy_firms, beneficiary,
    institutional, generational, arbitrage, global).

% Lobby for unconditional income support framed as worker relief while designing scheduling and wage policies that treat the payment as a wage offset. They administer the low-wage labor regime and capture the subsidy through suppressed wage growth and increased part-time precarious contracts.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, retail_hospitality_chains, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(income_support_conditionality__wage_subsidy_reading, retail_hospitality_chains, agenda_setter).

% Receive the unconditional payment but face wage offers that have adjusted downward by approximately the payment amount. Their labor power is decommodified in name only — the payment creates a floor but the market clears at that floor, leaving them with subsistence income and no bargaining leverage. Exit is identity-locked because work constitutes social membership and the payment is conditioned on availability for work.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, low_wage_workers, payer,
    moderate, biographical, identity_locked, national).

% Cycle through zero-hours contracts, temporary agency work, and gig platforms. The unconditional payment smooths income volatility but does not raise the wage floor — employers treat it as a substitute for employer-provided benefits and stability. They bear the full risk of income fluctuation while the payment is captured by the firms that structure the precarity.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, precarious_contract_workers, payer,
    powerless, immediate, trapped, national).

% Face a labor market where entry-level wages have been depressed to the unconditional payment level. The payment functions as a wage subsidy for employers who can now offer jobs at or near the payment amount, knowing applicants have no alternative. Their job search is disciplined by the payment's adequacy threshold — they must accept any offer at or above the floor.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, unemployed_job_seekers, payer,
    powerless, immediate, constrained, national).

% Campaign for unconditional income as emancipation from coercive labor. They document the wage suppression effect and argue for complementary policies (sectoral bargaining, job guarantees, conditional top-ups) to prevent capture. Their analysis is excluded from the legislative design process which is dominated by employer lobbies.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, social_policy_advocates, observer,
    organized, generational, analytical, national).

% Study the incidence of unconditional transfers on wage setting. The empirical literature shows partial to full pass-through of transfers to lower wages in monopsonistic low-wage sectors. They provide the structural evidence that the constraint operates as employer subsidy rather than worker empowerment.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, labor_economists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal income floor that prevents absolute destitution and coordinates a baseline of consumption demand across the economy, stabilizing aggregate demand during downturns and reducing administrative overhead of means-testing.
% TRANSFER_FUNCTION: Moves fiscal resources from the tax base (broadly, but regressively through consumption taxes and labor taxation) to low-wage employers via the mechanism of wage suppression — the unconditional payment enables employers to pay below-subsistence wages while workers' total income remains at subsistence.
% ABSENT_VOICES: Workers in the global south whose labor competes with the subsidized low-wage sector; future cohorts who inherit a labor market where the wage floor is structurally pinned to a public subsidy rather than productivity; small employers who cannot capture the subsidy because they lack monopsony power and are squeezed between the wage floor and large competitors' scale advantages.
% DISAPPEARANCE_RATIONALE: If the unconditional payment vanished overnight, low-wage employers would face immediate labor supply collapse at current wage rates — workers could not survive on offered wages. Wages would be forced upward, precarious contracts would become unsustainable, and the low-wage labor market would reorganize around genuine subsistence wages or collapse into automation/offshoring. The arrangement is load-bearing for the current low-wage employment model.
% FOUNDING_PROBLEM: Post-1970s wage stagnation and the breakdown of the postwar wage-productivity link created a growing population of working poor. Means-tested welfare created high marginal tax rates and poverty traps. The unconditional payment was proposed to decouple survival from employment status and eliminate administrative exclusion.
% FOUNDING_PROBLEM_CORROBORATION: The original proponents (basic income movement, some post-work theorists) attest the founding problem is live — they argue the wage capture is a design flaw correctable by complementary policies. Employer associations and neoliberal policy networks attest the problem is solved — the payment stabilizes the low-wage labor market they depend on. Independent labor economists (Standing, Van Parijs critics, monopsony literature) corroborate that the founding problem of poverty traps has been replaced by a wage suppression mechanism that the original proponents did not anticipate.
narrative_ontology:disappearance_verdict(income_support_conditionality__wage_subsidy_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__wage_subsidy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__wage_subsidy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(income_support_conditionality__wage_subsidy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__wage_subsidy_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_conditionality__wage_subsidy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_conditionality__wage_subsidy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_conditionality__wage_subsidy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) reflects that a substantial portion of the fiscal transfer is captured by employers through wage adjustment — monopsony power in low-wage sectors allows wage-setting at or near the unconditional payment level. Suppression (0.55) is moderate: the constraint does not directly coerce workers but suppresses alternatives by making low-wage employment the only viable path above the floor, while actively blocking collective bargaining and sectoral wage-setting that would break the capture. Theater ratio (0.42) is significant: the emancipatory framing ('freedom to say no') persists in policy discourse while the structural operation disciplines labor supply. Accessibility collapse (0.38) is partial — workers can refuse specific jobs but not the labor market as a whole. Resistance (0.48) is moderate: labor movements and advocates push for complementary policies but face institutional capture.
 *
 * PERSPECTIVAL GAP:
 *   From the employer seats (beneficiaries), the constraint appears as efficient labor market stabilization — they coordinate around a predictable floor and reduced turnover. From the worker seats (payers), the same structure operates as enforced subsistence — the payment enables the market to clear at the floor, leaving them with no surplus and no exit. The analytical observer seat sees the full structural capture: a genuine coordination function (universal floor) has been colonized by an extraction function (wage subsidy) that requires the active maintenance of labor market monopsony.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-wage employers, platform firms, and retail chains are structural beneficiaries (d near 0.0-0.2) — they capture the fiscal transfer via wage suppression, have mobile/arbitrage exit, and hold agenda-setting power over labor market rules. Low-wage workers are identity-locked payers (d near 0.8-0.9) — work constitutes social membership, the payment is conditioned on labor market attachment, and exit means social exclusion. Precarious workers are trapped (d near 1.0) — no bargaining power, immediate time horizon. Unemployed job seekers are constrained (d near 0.7-0.8) — must accept offers at the floor. Advocates and economists are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (poverty traps, administrative exclusion) was real but the solution has been captured. The arrangement now institutionalizes the very coercion it claimed to dissolve — by making subsistence conditional on labor market participation at employer-dictated wages. Mandatrophy is unresolved: the constraint persists because it solves a genuine coordination problem (universal floor) while the extraction function is obscured by the emancipatory framing. The engine's per-seat classification will show beneficiaries experiencing rope-like coordination while payers experience snare-like extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wage_pass_through_magnitude,
    'What fraction of the unconditional payment is passed through to lower wages in monopsonistic low-wage sectors?',
    'Natural experiments from pilot programs (Finland, Kenya, Stockton, Alaska Permanent Fund) with high-frequency wage data; structural estimation of labor supply elasticities and employer wage-setting power.',
    'If pass-through exceeds 0.7, the constraint is predominantly extraction (snare-flavored); if below 0.3, coordination dominates (rope-flavored); the 0.3-0.7 range confirms tangled_rope with the exact split determining seat-level divergence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_pass_through_magnitude, empirical, 'Empirical magnitude of employer capture via wage adjustment').

omega_variable(
    coordination_extraction_separability,
    'Can the universal floor''s coordination function (administrative simplicity, destitution prevention) be preserved while blocking the wage suppression extraction function?',
    'Policy design analysis: sectoral minimum wages indexed above the floor, strong collective bargaining coverage, job guarantee programs, or conditional top-ups that break the wage-setting link.',
    'If separable, the constraint is a tangled_rope with a removable extraction component; if inseparable, the universal floor itself structurally necessitates wage suppression in monopsonistic markets, making the constraint a snare with a coordination veneer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the coordination and extraction components are structurally separable or jointly necessary').

omega_variable(
    kernel_framing_underdetermination,
    'Does the income_support_conditionality kernel instantiate one constraint with contested interpretation, or multiple structurally distinct constraints?',
    'Compare the ε values, beneficiary/victim structures, and enforcement requirements across all three readings. If they differ substantially on structural metrics (not just normative evaluation), they are distinct constraints per the ε-invariance principle.',
    'If distinct constraints, each reading gets its own story with independent classification — the kernel is a family label, not a single constraint. If one constraint, the readings are observer perspectives on the same structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel decomposes into multiple constraints per ε-invariance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__wage_subsidy_reading, 1990, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(income_support_wage_subsidy_tr_t1990, income_support_conditionality__wage_subsidy_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(income_support_wage_subsidy_tr_t2000, income_support_conditionality__wage_subsidy_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(income_support_wage_subsidy_tr_t2010, income_support_conditionality__wage_subsidy_reading, theater_ratio, 2010, 0.28).
narrative_ontology:measurement(income_support_wage_subsidy_tr_t2015, income_support_conditionality__wage_subsidy_reading, theater_ratio, 2015, 0.35).
narrative_ontology:measurement(income_support_wage_subsidy_tr_t2020, income_support_conditionality__wage_subsidy_reading, theater_ratio, 2020, 0.39).
narrative_ontology:measurement(income_support_wage_subsidy_tr_t2025, income_support_conditionality__wage_subsidy_reading, theater_ratio, 2025, 0.41).
narrative_ontology:measurement(income_support_wage_subsidy_tr_t2030, income_support_conditionality__wage_subsidy_reading, theater_ratio, 2030, 0.42).

% Extraction over time
narrative_ontology:measurement(income_support_wage_subsidy_be_t1990, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 1990, 0.15).
narrative_ontology:measurement(income_support_wage_subsidy_be_t2000, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 2000, 0.28).
narrative_ontology:measurement(income_support_wage_subsidy_be_t2010, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement(income_support_wage_subsidy_be_t2015, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 2015, 0.52).
narrative_ontology:measurement(income_support_wage_subsidy_be_t2020, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 2020, 0.61).
narrative_ontology:measurement(income_support_wage_subsidy_be_t2025, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 2025, 0.65).
narrative_ontology:measurement(income_support_wage_subsidy_be_t2030, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 2030, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(income_support_wage_subsidy_su_t1990, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 1990, 0.2).
narrative_ontology:measurement(income_support_wage_subsidy_su_t2000, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 2000, 0.3).
narrative_ontology:measurement(income_support_wage_subsidy_su_t2010, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement(income_support_wage_subsidy_su_t2015, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 2015, 0.47).
narrative_ontology:measurement(income_support_wage_subsidy_su_t2020, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 2020, 0.52).
narrative_ontology:measurement(income_support_wage_subsidy_su_t2025, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 2025, 0.54).
narrative_ontology:measurement(income_support_wage_subsidy_su_t2030, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 2030, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__wage_subsidy_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_conditionality__wage_subsidy_reading, 0.15).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, income_support_conditionality__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, income_support_conditionality__dependency_trap_reading).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, minimum_wage_enforcement).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, sectoral_bargaining_coverage).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, gig_worker_classification).

% DUAL FORMULATION NOTE:
% This constraint is one member of the income_support_conditionality constraint family. The wage_subsidy_reading has ε=0.68 (substantial extraction), beneficiaries=employers, victims=workers, type=tangled_rope. The freedom_floor_reading has ε≈0.15 (negligible extraction), beneficiaries=workers, victims=none, type=rope. The dependency_trap_reading has ε≈0.35 (moderate extraction), beneficiaries=state/administrators, victims=workers (via skill atrophy), type=scaffold_or_snare. These are structurally distinct constraints linked by the shared policy object.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(income_support_conditionality__wage_subsidy_reading, organized, 0.15).
constraint_indexing:directionality_override(income_support_conditionality__wage_subsidy_reading, moderate, 0.85).
constraint_indexing:directionality_override(income_support_conditionality__wage_subsidy_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
