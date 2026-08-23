% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_conditionality__freedom_floor_reading, []).

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
 *   constraint_id: income_support_conditionality__freedom_floor_reading
 *   human_readable: Unconditional Income Support as Freedom Floor
 *   domain: political_economy/social_policy/labor_economics
 *
 * SUMMARY:
 *   This constraint story models the freedom_floor_reading of the
 *   income_support_conditionality kernel. The reading asserts that
 *   unconditional income support — a universal, non-means-tested,
 *   non-work-conditioned payment — functions as a rope: it solves a genuine
 *   coordination problem (voluntary labor matching) by providing a structural
 *   exit option from coercive work. Beneficiaries are
 *   income_support_recipients (low-wage workers, unemployed, caregivers,
 *   precariat) who gain positive freedom to refuse. Employers enter the
 *   victim/payer set because they lose the coercive firing power that
 *   survival-conditionality provided; they must now compete for labor on
 *   equal terms. Taxpayers fund the transfer but are net beneficiaries via
 *   reduced precarity externalities. The constraint is claimed as rope
 *   (coordination) with low extraction, low suppression, and minimal theater.
 *   The claim/metric independence is maintained: the metrics describe a
 *   genuine coordination floor; the engine will compute per-seat types from
 *   the structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__freedom_floor_reading, 0.15).
domain_priors:suppression_score(income_support_conditionality__freedom_floor_reading, 0.1).
domain_priors:theater_ratio(income_support_conditionality__freedom_floor_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__freedom_floor_reading, rope).
narrative_ontology:human_readable(income_support_conditionality__freedom_floor_reading, "Unconditional Income Support as Freedom Floor").
narrative_ontology:topic_domain(income_support_conditionality__freedom_floor_reading, "political_economy/social_policy/labor_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__freedom_floor_reading, '36e4867d-beeb-4569-a1ab-21d93f22bfbd').
narrative_ontology:cs_kernel_codification('36e4867d-beeb-4569-a1ab-21d93f22bfbd', formalized).
narrative_ontology:cs_authority_grounding('36e4867d-beeb-4569-a1ab-21d93f22bfbd', lineage).
narrative_ontology:cs_interpretation_layer_present('36e4867d-beeb-4569-a1ab-21d93f22bfbd').
narrative_ontology:cs_reading_relation('36e4867d-beeb-4569-a1ab-21d93f22bfbd', income_support_conditionality__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('36e4867d-beeb-4569-a1ab-21d93f22bfbd', income_support_conditionality__wage_subsidy_reading, coexists_with).
narrative_ontology:cs_axiom('36e4867d-beeb-4569-a1ab-21d93f22bfbd', foundational, unconditional_income_creates_positive_freedom).
narrative_ontology:cs_axiom_status(unconditional_income_creates_positive_freedom, holdable).
narrative_ontology:cs_axiom_grounding('36e4867d-beeb-4569-a1ab-21d93f22bfbd', unconditional_income_creates_positive_freedom, deontological).
narrative_ontology:cs_axiom('36e4867d-beeb-4569-a1ab-21d93f22bfbd', foundational, decommodification_enables_labor_refusal).
narrative_ontology:cs_axiom_status(decommodification_enables_labor_refusal, holdable).
narrative_ontology:cs_axiom_grounding('36e4867d-beeb-4569-a1ab-21d93f22bfbd', decommodification_enables_labor_refusal, instrumental).
narrative_ontology:cs_reference_frame('36e4867d-beeb-4569-a1ab-21d93f22bfbd', universal_unconditional_support_as_freedom_floor).
narrative_ontology:cs_drift_state('36e4867d-beeb-4569-a1ab-21d93f22bfbd', contemporary_policy_debate, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('36e4867d-beeb-4569-a1ab-21d93f22bfbd', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__freedom_floor_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, income_support_recipients).
narrative_ontology:constraint_victim(income_support_conditionality__freedom_floor_reading, employers).
narrative_ontology:constraint_victim(income_support_conditionality__freedom_floor_reading, taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, taxpayers).
narrative_ontology:constraint_vindicates(income_support_conditionality__freedom_floor_reading, decommodification_enables_positive_freedom).
narrative_ontology:constraint_vindicates(income_support_conditionality__freedom_floor_reading, exit_option_structures_labor_market_coordination).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive unconditional income support sufficient for basic subsistence. This provides a genuine exit option from coercive, low-wage, or dangerous work. They can refuse exploitative offers, negotiate better conditions, pursue education or caregiving, or accept work on their own terms. The support is not withdrawn for refusing work, making the exit option structural rather than theoretical.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, income_support_recipients, beneficiary,
    moderate, biographical, constrained, national).

% Lose the coercive firing power that conditioned labor supply on survival necessity. Must now offer wages and conditions attractive enough to recruit workers who have a genuine fallback. This raises labor costs, particularly for low-wage, high-turnover sectors. Some firms adapt by improving productivity; others lobby for conditionality restoration or import labor. Their 'loss' is the removal of a structural subsidy to low-road business models.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, employers, payer,
    powerful, biographical, constrained, national).

% Fund the income support through general taxation. They also benefit from a more stable society, reduced poverty-related costs (healthcare, crime, emergency services), and a labor market where work is genuinely voluntary. The net fiscal incidence is contested; this reading holds that the coordination gains (better job matches, reduced precarity externalities) offset or exceed the gross transfer cost.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, taxpayers, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(income_support_conditionality__freedom_floor_reading, taxpayers, beneficiary).

% Administers the unconditional payment infrastructure: universal enrollment, automatic delivery, no means-testing bureaucracy. The administrative burden collapses relative to conditional systems (no caseworkers, sanctions machinery, compliance monitoring). The agency's role shifts from behavioral enforcement to payment reliability — a genuine coordination function.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, social_security_administration, agenda_setter,
    institutional, generational, analytical, national).

% Analyze the constraint's effects across readings. Produce evidence on labor supply elasticities, wage dynamics, poverty traps, and fiscal sustainability. Their analyses are cited by all three sibling readings selectively. They do not bear the constraint's costs or collect its benefits directly.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, policy_commentariat, observer,
    analytical, civilizational, analytical, global).

% Undocumented migrants, informal economy workers, and those without bank accounts or address registration who fall through the universal enrollment net. They would object to the claim that the support is truly unconditional and universal, but their exclusion is structural (administrative invisibility) not intentional. The freedom floor has a hole where they stand.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, precariat_excluded, excluded,
    powerless, immediate, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of matching labor to work without coercion: workers need a genuine fallback to make 'quit' a credible option, which forces employers to make offers worth accepting. The unconditional floor replaces the disciplinary function of poverty with a voluntary matching function.
% TRANSFER_FUNCTION: Moves resources from general taxation (taxpayers) to all residents unconditionally (income_support_recipients). Employers face an indirect transfer via higher equilibrium wages for undesirable work — they pay more for labor that was previously subsidized by worker desperation.
% ABSENT_VOICES: The precariat_excluded (undocumented, unbanked, administratively invisible) are structurally absent from the universal claim. Employers in low-road sectors who would face the largest wage adjustments are politically vocal but analytically absent from the 'coordination' framing — they frame their loss as 'labor shortage' not 'lost coercion'.
% DISAPPEARANCE_RATIONALE: If unconditional support vanished overnight, the exit option collapses. Workers return to survival-constrained job acceptance. Employers regain coercive firing power. Low-wage sectors revert to high-turnover, low-investment models. Poverty-related externalities spike. The labor market reorganizes around desperation rather than mutual agreement.
% FOUNDING_PROBLEM: The conditional welfare state created a poverty trap: means-testing and work requirements made the support withdraw faster than wages rose, making transition to work financially irrational for many. Simultaneously, the threat of destitution coerced workers into accepting any job, subsidizing exploitative employers. The founding problem was a welfare system that simultaneously trapped recipients in dependency AND subsidized coercive labor markets.
% FOUNDING_PROBLEM_CORROBORATION: OECD and ILO analyses of welfare reform trajectories (outside the beneficiary set) document the poverty trap dynamic. Employer surveys from the 1990s welfare reform era (US, UK) explicitly noted 'discipline' benefits of conditionality — corroborating the coercive subsidy claim. The 'dependency trap' reading's own evidence (work disincentive studies) corroborates the trap mechanism but disputes the normative framing.
narrative_ontology:disappearance_verdict(income_support_conditionality__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__freedom_floor_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__freedom_floor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(income_support_conditionality__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__freedom_floor_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_conditionality__freedom_floor_reading_tests).
:- end_tests(income_support_conditionality__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the transfer is not extracted from a targeted group for another's gain — it is a universal floor funded by general taxation, and the coordination gains (better matches, reduced turnover, lower poverty costs) are broadly distributed. Suppression is low (0.1) because no enforcement machinery compels participation; the constraint is an offer, not a mandate. Theater is low (0.1) because the administrative apparatus is minimal (universal enrollment, automatic payment) and performs its stated function. Accessibility_collapse is low (0.2) because the labor market remains fully operational — alternatives (work, entrepreneurship, education) expand rather than collapse. Resistance is low (0.2) because the primary opposition is political (pre-implementation), not operational resistance from those governed by the constraint.
 *
 * PERSPECTIVAL GAP:
 *   The employer seat and recipient seat should compute differently: from the recipient position, the constraint is a genuine coordination floor enabling refusal; from the low-road employer position, the same structure operates as a cost increase and loss of disciplinary leverage. The engine computes this divergence from the structural data. The wage_subsidy_reading would invert this: employers as beneficiaries (wage suppression), recipients as payers (inflation erosion). The dependency_trap_reading would make recipients payers (skill atrophy) and state as agenda_setter (behavioral enforcement).
 *
 * DIRECTIONALITY LOGIC:
 *   Income_support_recipients are structural beneficiaries (d ~ 0.1): the constraint subsidizes their exit option. Employers are structural payers (d ~ 0.7): they lose coercive power and face higher wage bills for undesirable work — the constraint extracts their previous subsidy. Taxpayers sit near symmetric (d ~ 0.5): they fund the floor but gain social stability and labor market efficiency. The social_security_administration is agenda_setter with analytical exit (d ~ 0.2): it administers but does not extract. The precariat_excluded are trapped (d ~ 0.9) but excluded from the beneficiary set — an omega documents this gap.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (poverty trap + coercive labor subsidy) is contested: the dependency_trap_reading claims the problem persists (work disincentives), the wage_subsidy_reading claims the problem shifted (employer capture), this reading claims the problem is substantially solved by decommodification. The constraint persists not from inertia but from active political coalition (recipients + taxpayers benefiting from stability). If the founding problem is dead (automation eliminates low-wage work), the constraint becomes a scaffold with sunset pressure. If live, it remains a rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adequacy_of_freedom_floor,
    'Is the unconditional payment level actually sufficient to constitute a genuine exit option from coercive work, or does it merely supplement inadequate wages (wage_subsidy_reading''s claim)?',
    'Empirical measurement of reservation wage shifts and job refusal rates post-implementation, compared to cost-of-living benchmarks and job quality indices.',
    'If inadequate, the constraint degrades to tangled_rope (coordination cover for employer subsidy) or snare (inadequate trap). If adequate, rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adequacy_of_freedom_floor, empirical, 'Whether the payment level delivers the claimed positive freedom.').

omega_variable(
    employer_power_reconstitution,
    'Do employers reconstitute coercive power through other channels (algorithmic management, monopsony, immigration policy, credit dependence) such that the exit option is nominal?',
    'Longitudinal study of labor market power indicators (quit rates, wage markdowns, non-compete prevalence, worker surveillance) in jurisdictions with unconditional floors.',
    'If coercive power reconstitutes, the constraint''s coordination function is undermined — extraction shifts from direct (poverty) to structural (market power), potentially reclassifying to tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(employer_power_reconstitution, empirical, 'Whether the freedom floor survives employer counter-mobilization.').

omega_variable(
    precariat_exclusion_gap,
    'Does the universal enrollment mechanism actually reach the most coerced workers (undocumented, informal, unbanked), or does the freedom floor have a structural hole at the bottom?',
    'Administrative data linkage between payment rolls and informal economy surveys; ethnographic work with excluded populations.',
    'If exclusion is systematic and large, the constraint''s coordination claim is partial — it coordinates for the visible workforce while leaving the most vulnerable in the snare. This would require a nested constraint story for the excluded stratum.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precariat_exclusion_gap, empirical, 'Universality of the exit option in practice.').

omega_variable(
    reading_foreclosure_structure,
    'Does the freedom_floor_reading''s core premise (unconditional support creates positive freedom) logically foreclose the dependency_trap_reading''s premise (unconditional support creates dependency) within a single policy framework, or do they coexist as competing coalitions?',
    'Analyze whether any implemented system has adopted the freedom_floor premise while formally rejecting the dependency_trap premise (or vice versa), and whether hybrid designs are logically stable.',
    'If forecloses, the kernel has a binary structural split. If coexists_with, the kernel sustains permanent policy oscillation. This reading assesses coexists_with.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_structure, conceptual, 'Logical relation between freedom_floor and dependency_trap readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__freedom_floor_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(isc_ffr_tr_t0, income_support_conditionality__freedom_floor_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(isc_ffr_tr_t5, income_support_conditionality__freedom_floor_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(isc_ffr_tr_t10, income_support_conditionality__freedom_floor_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(isc_ffr_tr_t15, income_support_conditionality__freedom_floor_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement(isc_ffr_tr_t20, income_support_conditionality__freedom_floor_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement(isc_ffr_tr_t25, income_support_conditionality__freedom_floor_reading, theater_ratio, 25, 0.1).
narrative_ontology:measurement(isc_ffr_tr_t30, income_support_conditionality__freedom_floor_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(isc_ffr_be_t0, income_support_conditionality__freedom_floor_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(isc_ffr_be_t5, income_support_conditionality__freedom_floor_reading, base_extractiveness, 5, 0.16).
narrative_ontology:measurement(isc_ffr_be_t10, income_support_conditionality__freedom_floor_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(isc_ffr_be_t15, income_support_conditionality__freedom_floor_reading, base_extractiveness, 15, 0.14).
narrative_ontology:measurement(isc_ffr_be_t20, income_support_conditionality__freedom_floor_reading, base_extractiveness, 20, 0.14).
narrative_ontology:measurement(isc_ffr_be_t25, income_support_conditionality__freedom_floor_reading, base_extractiveness, 25, 0.15).
narrative_ontology:measurement(isc_ffr_be_t30, income_support_conditionality__freedom_floor_reading, base_extractiveness, 30, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(isc_ffr_su_t0, income_support_conditionality__freedom_floor_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(isc_ffr_su_t5, income_support_conditionality__freedom_floor_reading, suppression_requirement, 5, 0.12).
narrative_ontology:measurement(isc_ffr_su_t10, income_support_conditionality__freedom_floor_reading, suppression_requirement, 10, 0.1).
narrative_ontology:measurement(isc_ffr_su_t15, income_support_conditionality__freedom_floor_reading, suppression_requirement, 15, 0.08).
narrative_ontology:measurement(isc_ffr_su_t20, income_support_conditionality__freedom_floor_reading, suppression_requirement, 20, 0.09).
narrative_ontology:measurement(isc_ffr_su_t25, income_support_conditionality__freedom_floor_reading, suppression_requirement, 25, 0.1).
narrative_ontology:measurement(isc_ffr_su_t30, income_support_conditionality__freedom_floor_reading, suppression_requirement, 30, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__freedom_floor_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_conditionality__freedom_floor_reading, 0.12).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, labor_market_coercion).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, conditional_welfare_administration).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, minimum_wage_enforcement).

% DUAL FORMULATION NOTE:
% This constraint (freedom_floor_reading) decomposes the income_support_conditionality kernel with dependency_trap_reading and wage_subsidy_reading. The three readings share the kernel (the conditionality rule) but instantiate different constraints with different ε, different beneficiary/victim sets, and different types. This reading's ε (0.15) is low because the referent is the unconditional floor assessed as coordination; dependency_trap_reading's ε would be high (snare on recipients); wage_subsidy_reading's ε would be high (snare on workers via employer capture). They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(income_support_conditionality__freedom_floor_reading, powerful, 0.7).
constraint_indexing:directionality_override(income_support_conditionality__freedom_floor_reading, moderate, 0.15).
constraint_indexing:directionality_override(income_support_conditionality__freedom_floor_reading, organized, 0.45).
constraint_indexing:directionality_override(income_support_conditionality__freedom_floor_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
