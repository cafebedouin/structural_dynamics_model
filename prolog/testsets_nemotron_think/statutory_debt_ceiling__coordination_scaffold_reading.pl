% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__coordination_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_debt_ceiling__coordination_scaffold_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: statutory_debt_ceiling__coordination_scaffold_reading
 *   human_readable: Statutory Debt Ceiling as Coordination Scaffold
 *   domain: constitutional_law/political_economy/fiscal_governance
 *
 * SUMMARY:
 *   This constraint story captures the 'coordination scaffold' reading of the
 *   statutory debt ceiling: a procedural mechanism enacted in 1917 to
 *   aggregate congressional borrowing authorization into a single limit,
 *   enabling Treasury operational autonomy between periodic legislative
 *   adjustments. The reading emphasizes the scaffold's routine,
 *   low-extractiveness character — regular adjustments (100+ since 1940) with
 *   minimal brinkmanship for most of its history. The ceiling functions as a
 *   coordination device that concentrates fiscal policy review into
 *   predictable legislative moments rather than continuous micromanagement.
 *   This reading stands in tension with two sibling readings of the same
 *   kernel: the constitutional nullity reading (14th Amendment renders the
 *   ceiling void) and the extraction snare reading (the ceiling has become a
 *   weaponized hostage-taking device).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__coordination_scaffold_reading, 0.18).
domain_priors:suppression_score(statutory_debt_ceiling__coordination_scaffold_reading, 0.22).
domain_priors:theater_ratio(statutory_debt_ceiling__coordination_scaffold_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__coordination_scaffold_reading, scaffold).
narrative_ontology:human_readable(statutory_debt_ceiling__coordination_scaffold_reading, "Statutory Debt Ceiling as Coordination Scaffold").
narrative_ontology:topic_domain(statutory_debt_ceiling__coordination_scaffold_reading, "constitutional_law/political_economy/fiscal_governance").

narrative_ontology:has_sunset_clause(statutory_debt_ceiling__coordination_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__coordination_scaffold_reading, '70255d40-c7a6-4380-a93d-5aa51d384b26').
narrative_ontology:cs_kernel_codification('70255d40-c7a6-4380-a93d-5aa51d384b26', formalized).
narrative_ontology:cs_authority_grounding('70255d40-c7a6-4380-a93d-5aa51d384b26', lineage).
narrative_ontology:cs_interpretation_layer_present('70255d40-c7a6-4380-a93d-5aa51d384b26').
narrative_ontology:cs_reading_relation('70255d40-c7a6-4380-a93d-5aa51d384b26', statutory_debt_ceiling__constitutional_nullity_reading, coexists_with).
narrative_ontology:cs_reading_relation('70255d40-c7a6-4380-a93d-5aa51d384b26', statutory_debt_ceiling__extraction_snare_reading, coexists_with).
narrative_ontology:cs_axiom('70255d40-c7a6-4380-a93d-5aa51d384b26', foundational, debt_ceiling_enables_treasury_autonomy).
narrative_ontology:cs_axiom_status(debt_ceiling_enables_treasury_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('70255d40-c7a6-4380-a93d-5aa51d384b26', debt_ceiling_enables_treasury_autonomy, conventional).
narrative_ontology:cs_axiom('70255d40-c7a6-4380-a93d-5aa51d384b26', foundational, periodic_adjustment_is_routine_not_crisis).
narrative_ontology:cs_axiom_status(periodic_adjustment_is_routine_not_crisis, holdable).
narrative_ontology:cs_axiom_grounding('70255d40-c7a6-4380-a93d-5aa51d384b26', periodic_adjustment_is_routine_not_crisis, empirically_contingent).
narrative_ontology:cs_reference_frame('70255d40-c7a6-4380-a93d-5aa51d384b26', statutory_fiscal_coordination_framework).
narrative_ontology:cs_drift_state('70255d40-c7a6-4380-a93d-5aa51d384b26', contemporary_polarization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('70255d40-c7a6-4380-a93d-5aa51d384b26', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__coordination_scaffold_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, treasury_department).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, financial_markets).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, federal_program_beneficiaries).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, congressional_leadership).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(statutory_debt_ceiling__coordination_scaffold_reading, treasury_department).
narrative_ontology:constraint_victim(statutory_debt_ceiling__coordination_scaffold_reading, taxpayers).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__coordination_scaffold_reading, congressional_power_of_the_purse).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__coordination_scaffold_reading, fiscal_responsibility_norm).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__coordination_scaffold_reading, statutory_budget_control).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and periodically adjusts the statutory debt limit through legislation. Uses the ceiling as a procedural checkpoint for fiscal policy review. Retains full legislative authority to raise, suspend, or abolish the limit. The ceiling functions as a coordination device that aggregates borrowing authority into a single legislative act rather than requiring micromanagement of each Treasury issuance.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, congress, agenda_setter,
    institutional, generational, arbitrage, national).

% Operates with borrowing autonomy within the aggregate statutory limit. Manages debt issuance, cash management, and extraordinary measures without seeking congressional approval for each operation. Bears administrative burden of compliance and extraordinary measures when the limit binds. The ceiling enables operational continuity between congressional adjustments.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, treasury_department, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__coordination_scaffold_reading, treasury_department, payer).

% Receives predictable supply of Treasury securities as benchmark assets. The periodic adjustment pattern creates a known cadence for market operations. Gains liquidity and price discovery benefits from centralized debt management. Would face fragmentation and uncertainty if each issuance required separate authorization.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, financial_markets, beneficiary,
    organized, biographical, mobile, global).

% Receive uninterrupted federal payments (Social Security, Medicare, veterans benefits, contractor payments) because Treasury can manage cash flow within the ceiling. The scaffold prevents payment disruptions that would occur if Congress had to approve each borrowing increment. Vulnerable only when the scaffold fails (limit not raised in time).
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, federal_program_beneficiaries, beneficiary,
    moderate, biographical, constrained, national).

% Uses the debt ceiling vote as a recurring legislative vehicle for policy negotiation, oversight, and signaling. The scaffold structure concentrates fiscal policy debates into predictable moments rather than continuous micromanagement. Gains agenda-setting leverage from the must-pass nature of adjustment legislation.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, congressional_leadership, beneficiary,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__coordination_scaffold_reading, congressional_leadership, agenda_setter).

% Ultimately bear the interest costs of federal debt issued under the ceiling. The scaffold lowers borrowing costs through market confidence and operational efficiency, indirectly benefiting taxpayers. No meaningful exit from the federal fiscal system. Costs are diffuse and long-horizon.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, taxpayers, payer,
    moderate, generational, trapped, national).

% Monitor the ceiling adjustment process as a signal of fiscal governance quality. Routine, timely adjustments support AAA ratings; brinkmanship triggers negative outlooks. Their assessments feed back into borrowing costs but they do not set or enforce the constraint.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, credit_rating_agencies, observer,
    organized, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregates congressional borrowing authorization into a single aggregate limit, enabling Treasury to execute debt management operations (issuance, rollover, cash management) continuously without returning to Congress for each transaction. Solves the collective action problem of continuous legislative micromanagement of borrowing.
% TRANSFER_FUNCTION: Moves legislative time and attention from continuous debt authorization to periodic aggregate review. Transfers operational discretion to Treasury within the statutory bound. No systematic wealth transfer between parties under normal operation; the transfer is procedural (decision-making authority) not extractive.
% ABSENT_VOICES: State and local governments (dependent on federal funding continuity but not represented in ceiling negotiations), future generations (bear long-term debt service but have no voice), and foreign sovereign holders of Treasuries (affected by governance signals but excluded from domestic legislative process).
% DISAPPEARANCE_RATIONALE: If the debt ceiling vanished overnight, Congress would need to establish a new mechanism for authorizing federal borrowing — either continuous micromanagement of each issuance (operationally infeasible) or a new aggregate authorization framework. Treasury operations would lose their current statutory basis for autonomous debt management. Financial markets would lose the predictable cadence of ceiling adjustments as governance signals.
% FOUNDING_PROBLEM: World War I financing required a more flexible borrowing mechanism than the prior practice of Congress authorizing each bond issue individually. The Second Liberty Bond Act of 1917 created an aggregate limit to give Treasury operational autonomy while preserving congressional control over total indebtedness.
% FOUNDING_PROBLEM_CORROBORATION: Treasury historical records and Congressional Research Service reports confirm the 1917 origin as a wartime coordination measure. Contemporary Treasury officials and bipartisan congressional staff attest the coordination function remains necessary — no serious proposal exists to return to per-issuance authorization. The 'live' status is corroborated by the continued practice of routine adjustments across unified and divided governments from 1940-present.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__coordination_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__coordination_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__coordination_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(statutory_debt_ceiling__coordination_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__coordination_scaffold_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__coordination_scaffold_reading_tests).
:- end_tests(statutory_debt_ceiling__coordination_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the constraint primarily coordinates procedural authority rather than extracting resources — the ceiling does not systematically transfer wealth from payers to beneficiaries under normal operation. Suppression is low-moderate (0.22) because the constraint's persistence depends on routine legislative compliance, not active coercion; extraordinary measures are administrative tools, not suppression mechanisms. Theater ratio is low (0.12) because the scaffold's coordination function is genuine and predominant — the legislative rituals around adjustment votes serve real oversight and signaling purposes. Accessibility collapse (0.42) reflects that alternatives (per-issuance authorization, automatic indexing, abolition) exist but require legislative action. Resistance (0.35) captures the political friction inherent in any must-pass legislation, but not systematic opposition to the scaffold itself.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (taxpayers) and beneficiary seats (Treasury, markets, program beneficiaries) should compute differently: from Treasury's position the scaffold enables essential operational autonomy; from taxpayers' position the same structure enables debt accumulation with limited accountability. The engine computes this divergence from the structural data — the coordination function is real but its distributional consequences are asymmetric across time horizons.
 *
 * DIRECTIONALITY LOGIC:
 *   Congress (agenda_setter) sits near the beneficiary end (d ~ 0.15) — it sets the limit and gains procedural leverage. Treasury (beneficiary/payer) sits near symmetric (d ~ 0.5) — gains operational autonomy but bears compliance burden and extraordinary measures risk. Financial markets (beneficiary) sit at strong beneficiary (d ~ 0.1) — receive liquidity and predictability benefits with minimal cost. Federal program beneficiaries (beneficiary) sit at beneficiary (d ~ 0.2) — gain payment continuity. Congressional leadership (beneficiary/agenda_setter) sits at beneficiary (d ~ 0.2) — gains legislative vehicle. Taxpayers (payer) sit at moderate target (d ~ 0.6) — bear long-term costs with no exit. Credit rating agencies (observer) sit at analytical (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (WWI financing flexibility) remains live — the coordination need persists. The scaffold has not atrophied into a piton because it continues to serve its transitional function (aggregating authorization) and the sunset mechanism (periodic adjustment) operates as designed. Mandatrophy would only arise if adjustments ceased to be routine and the constraint persisted purely through inertia — which this reading explicitly denies as the normal case.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scaffold_vs_snare_boundary,
    'At what point does the coordination scaffold become an extraction snare — is there a measurable threshold of brinkmanship frequency or crisis duration that marks the transition?',
    'Historical analysis of adjustment episodes: code each episode for crisis characteristics (market disruption, payment delays, extraordinary measures duration) and identify structural break points where the coordination function degrades below the extraction threshold.',
    'If a measurable threshold exists, the scaffold reading applies only to pre-threshold periods; post-threshold the constraint reclassifies as snare. If no threshold exists, the two readings describe the same mechanism under different political conditions — requiring a conditional classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_vs_snare_boundary, empirical, 'Whether the scaffold/snare distinction is a phase transition or a continuous spectrum.').

omega_variable(
    constitutional_nullity_operational_effect,
    'If the constitutional_nullity_reading were judicially adopted, would Treasury operational autonomy change, or would Congress immediately enact a functionally identical coordination mechanism?',
    'Counterfactual legislative analysis: examine whether Congress would pass a replacement authorization framework (automatic indexing, Gephardt rule revival, standing delegation) and whether such a replacement would have different extractiveness/suppression properties.',
    'If Congress would enact a functional equivalent, the nullity reading changes legal form but not operational structure — the scaffold persists under a different constitutional basis. If not, the nullity reading implies a genuine structural change in fiscal governance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutional_nullity_operational_effect, conceptual, 'Whether the constitutional contest changes the constraint''s operational reality or only its legal justification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__coordination_scaffold_reading, 0, 84).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(statutory_debt_ceiling__coordination_scaffold_reading_tr_t0, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(statutory_debt_ceiling__coordination_scaffold_reading_tr_t0, observed).
narrative_ontology:measurement(statutory_debt_ceiling__coordination_scaffold_reading_tr_t12, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 12, 0.09).
narrative_ontology:measurement_basis(statutory_debt_ceiling__coordination_scaffold_reading_tr_t12, observed).
narrative_ontology:measurement(statutory_debt_ceiling__coordination_scaffold_reading_tr_t24, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 24, 0.1).
narrative_ontology:measurement_basis(statutory_debt_ceiling__coordination_scaffold_reading_tr_t24, observed).
narrative_ontology:measurement(statutory_debt_ceiling__coordination_scaffold_reading_tr_t36, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 36, 0.11).
narrative_ontology:measurement_basis(statutory_debt_ceiling__coordination_scaffold_reading_tr_t36, observed).
narrative_ontology:measurement(statutory_debt_ceiling__coordination_scaffold_reading_tr_t48, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 48, 0.11).
narrative_ontology:measurement_basis(statutory_debt_ceiling__coordination_scaffold_reading_tr_t48, observed).
narrative_ontology:measurement(statutory_debt_ceiling__coordination_scaffold_reading_tr_t60, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 60, 0.12).
narrative_ontology:measurement_basis(statutory_debt_ceiling__coordination_scaffold_reading_tr_t60, observed).
narrative_ontology:measurement(statutory_debt_ceiling__coordination_scaffold_reading_tr_t72, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 72, 0.12).
narrative_ontology:measurement_basis(statutory_debt_ceiling__coordination_scaffold_reading_tr_t72, observed).
narrative_ontology:measurement(statutory_debt_ceiling__coordination_scaffold_reading_tr_t84, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 84, 0.12).
narrative_ontology:measurement_basis(statutory_debt_ceiling__coordination_scaffold_reading_tr_t84, observed).

% Extraction over time
narrative_ontology:measurement(statutory_debt_ceiling__coordination_scaffold_reading_be_t0, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(statutory_debt_ceiling__coordination_scaffold_reading_be_t0, observed).
narrative_ontology:measurement(statutory_debt_ceiling__coordination_scaffold_reading_be_t12, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 12, 0.14).
narrative_ontology:measurement_basis(statutory_debt_ceiling__coordination_scaffold_reading_be_t12, observed).
narrative_ontology:measurement(statutory_debt_ceiling__coordination_scaffold_reading_be_t24, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 24, 0.15).
narrative_ontology:measurement_basis(statutory_debt_ceiling__coordination_scaffold_reading_be_t24, observed).
narrative_ontology:measurement(statutory_debt_ceiling__coordination_scaffold_reading_be_t36, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 36, 0.16).
narrative_ontology:measurement_basis(statutory_debt_ceiling__coordination_scaffold_reading_be_t36, observed).
narrative_ontology:measurement(statutory_debt_ceiling__coordination_scaffold_reading_be_t48, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 48, 0.17).
narrative_ontology:measurement_basis(statutory_debt_ceiling__coordination_scaffold_reading_be_t48, observed).
narrative_ontology:measurement(statutory_debt_ceiling__coordination_scaffold_reading_be_t60, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 60, 0.18).
narrative_ontology:measurement_basis(statutory_debt_ceiling__coordination_scaffold_reading_be_t60, observed).
narrative_ontology:measurement(statutory_debt_ceiling__coordination_scaffold_reading_be_t72, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 72, 0.18).
narrative_ontology:measurement_basis(statutory_debt_ceiling__coordination_scaffold_reading_be_t72, observed).
narrative_ontology:measurement(statutory_debt_ceiling__coordination_scaffold_reading_be_t84, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 84, 0.18).
narrative_ontology:measurement_basis(statutory_debt_ceiling__coordination_scaffold_reading_be_t84, observed).

% Suppression requirement over time
narrative_ontology:measurement(statutory_debt_ceiling__coordination_scaffold_reading_su_t0, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(statutory_debt_ceiling__coordination_scaffold_reading_su_t0, observed).
narrative_ontology:measurement(statutory_debt_ceiling__coordination_scaffold_reading_su_t12, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 12, 0.18).
narrative_ontology:measurement_basis(statutory_debt_ceiling__coordination_scaffold_reading_su_t12, observed).
narrative_ontology:measurement(statutory_debt_ceiling__coordination_scaffold_reading_su_t24, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 24, 0.2).
narrative_ontology:measurement_basis(statutory_debt_ceiling__coordination_scaffold_reading_su_t24, observed).
narrative_ontology:measurement(statutory_debt_ceiling__coordination_scaffold_reading_su_t36, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 36, 0.22).
narrative_ontology:measurement_basis(statutory_debt_ceiling__coordination_scaffold_reading_su_t36, observed).
narrative_ontology:measurement(statutory_debt_ceiling__coordination_scaffold_reading_su_t48, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 48, 0.22).
narrative_ontology:measurement_basis(statutory_debt_ceiling__coordination_scaffold_reading_su_t48, observed).
narrative_ontology:measurement(statutory_debt_ceiling__coordination_scaffold_reading_su_t60, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 60, 0.22).
narrative_ontology:measurement_basis(statutory_debt_ceiling__coordination_scaffold_reading_su_t60, observed).
narrative_ontology:measurement(statutory_debt_ceiling__coordination_scaffold_reading_su_t72, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 72, 0.22).
narrative_ontology:measurement_basis(statutory_debt_ceiling__coordination_scaffold_reading_su_t72, observed).
narrative_ontology:measurement(statutory_debt_ceiling__coordination_scaffold_reading_su_t84, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 84, 0.22).
narrative_ontology:measurement_basis(statutory_debt_ceiling__coordination_scaffold_reading_su_t84, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__coordination_scaffold_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(statutory_debt_ceiling__coordination_scaffold_reading, 0.1).
narrative_ontology:affects_constraint(statutory_debt_ceiling__coordination_scaffold_reading, statutory_debt_ceiling__extraction_snare_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__coordination_scaffold_reading, statutory_debt_ceiling__constitutional_nullity_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__coordination_scaffold_reading, treasury_extraordinary_measures_authority).
narrative_ontology:affects_constraint(statutory_debt_ceiling__coordination_scaffold_reading, congressional_budget_process).
narrative_ontology:affects_constraint(statutory_debt_ceiling__coordination_scaffold_reading, federal_reserve_monetary_policy_operations).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the statutory_debt_ceiling kernel. The coordination_scaffold_reading emphasizes procedural aggregation and Treasury autonomy (low ε, scaffold). The extraction_snare_reading emphasizes weaponized brinkmanship and minority extraction (high ε, snare). The constitutional_nullity_reading emphasizes 14th Amendment supremacy rendering the ceiling void (ε undefined, mountain/null). All three share the same statutory text but instantiate different constraints with different ε values, stakeholders, and structural dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(statutory_debt_ceiling__coordination_scaffold_reading, institutional, 0.15).
constraint_indexing:directionality_override(statutory_debt_ceiling__coordination_scaffold_reading, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
