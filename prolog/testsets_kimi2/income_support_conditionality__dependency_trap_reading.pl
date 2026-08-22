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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: income_support_conditionality__dependency_trap_reading
 *   human_readable: Unconditional Income Support Dependency Trap
 *   domain: political_economy/social_policy/labor_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the dependency_trap_reading of the
 *   income_support_conditionality kernel. The standing arrangement under
 *   contest is a policy regime of unconditional income transfers. From this
 *   reading's seat, the arrangement operates as a snare: it extracts human
 *   capital from recipients through enforced idleness and skill atrophy,
 *   extracts fiscal resources from taxpayers, and concentrates administrative
 *   authority and budget in the welfare state apparatus. The coordination
 *   narrativeâpoverty relief without stigmaâis cover for a structure that
 *   perpetuates the conditions it claims to solve. The claim/metric gap is
 *   intentional: the reading claims snare, and the metrics describe high
 *   extraction, substantial suppression of work-requirement alternatives, and
 *   moderate theater (humanitarian justification performing real legitimating
 *   work while the trap deepens).
 *
 * KEY AGENTS:
 *   - welfare_state_administrators: Primary agenda-setter and beneficiary (institutional/constrained) â administers the transfer apparatus and derives budget and authority from its scale.
 *   - ubi_recipients: Primary target (powerless/trapped) â bear the human-capital and motivational costs of long-term unconditional support.
 *   - taxpayers: Secondary target (organized/constrained) â fund transfers without direct benefit and face fiscal drag from a shrinking productive base.
 *   - work_conditionality_advocates: Excluded voice (moderate/constrained) â would argue for reciprocal obligation but are kept out of policy design.
 *   - labor_economists: Analytical observer (analytical/analytical) â measures incentive effects without being seated in the transfer.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__dependency_trap_reading, 0.76).
domain_priors:suppression_score(income_support_conditionality__dependency_trap_reading, 0.72).
domain_priors:theater_ratio(income_support_conditionality__dependency_trap_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__dependency_trap_reading, snare).
narrative_ontology:human_readable(income_support_conditionality__dependency_trap_reading, "Unconditional Income Support Dependency Trap").
narrative_ontology:topic_domain(income_support_conditionality__dependency_trap_reading, "political_economy/social_policy/labor_economics").

domain_priors:requires_active_enforcement(income_support_conditionality__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__dependency_trap_reading, 'a2bb61b8-60f8-443a-aa81-38c5aee00747').
narrative_ontology:cs_kernel_codification('a2bb61b8-60f8-443a-aa81-38c5aee00747', formalized).
narrative_ontology:cs_authority_grounding('a2bb61b8-60f8-443a-aa81-38c5aee00747', extraction).
narrative_ontology:cs_interpretation_layer_present('a2bb61b8-60f8-443a-aa81-38c5aee00747').
narrative_ontology:cs_reading_relation('a2bb61b8-60f8-443a-aa81-38c5aee00747', income_support_conditionality__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('a2bb61b8-60f8-443a-aa81-38c5aee00747', income_support_conditionality__wage_subsidy_reading, coexists_with).
narrative_ontology:cs_axiom('a2bb61b8-60f8-443a-aa81-38c5aee00747', foundational, unconditional_support_generates_dependency_trap).
narrative_ontology:cs_axiom_status(unconditional_support_generates_dependency_trap, holdable).
narrative_ontology:cs_axiom_grounding('a2bb61b8-60f8-443a-aa81-38c5aee00747', unconditional_support_generates_dependency_trap, empirically_contingent).
narrative_ontology:cs_axiom('a2bb61b8-60f8-443a-aa81-38c5aee00747', secondary, productive_reciprocity_as_social_obligation).
narrative_ontology:cs_axiom_status(productive_reciprocity_as_social_obligation, holdable).
narrative_ontology:cs_axiom_grounding('a2bb61b8-60f8-443a-aa81-38c5aee00747', productive_reciprocity_as_social_obligation, conventional).
narrative_ontology:cs_reference_frame('a2bb61b8-60f8-443a-aa81-38c5aee00747', reciprocal_obligation_equilibrium).
narrative_ontology:cs_drift_state('a2bb61b8-60f8-443a-aa81-38c5aee00747', unconditional_transfer_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a2bb61b8-60f8-443a-aa81-38c5aee00747', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__dependency_trap_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__dependency_trap_reading, welfare_state_administrators).
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, ubi_recipients).
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design eligibility rules, administer unconditional transfer payments, and justify program expansion through humanitarian anti-poverty framing. Derive institutional budgets, staffing mandates, and political authority from the scale and permanence of the transfer system. Could in principle redesign toward conditionality but face strong political and ideological resistance to any revision that shrinks the apparatus.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, welfare_state_administrators, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(income_support_conditionality__dependency_trap_reading, welfare_state_administrators, beneficiary).

% Receive unconditional income transfers. Over successive years experience skill depreciation, erosion of professional networks, and motivational collapse. Face steep re-entry barriers to formal employment because human capital has decayed and hiring screens filter for recent work history. The unconditional design removes bureaucratic contact points that might otherwise guide them toward training or employment services.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, ubi_recipients, payer,
    powerless, biographical, trapped, national).

% Compulsorily finance unconditional transfers through general taxation. Receive no direct benefit from the arrangement. Bear the fiscal burden of a shrinking tax base and rising transfer obligations as recipient dependency lengthens and program constituencies solidify.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, taxpayers, payer,
    organized, biographical, constrained, national).

% Argue that income support should require work, training, or community contribution in exchange for assistance. Are structurally excluded from policy design in jurisdictions that have adopted unconditional models; their proposals are dismissed as stigmatizing or paternalistic rather than reciprocal.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, work_conditionality_advocates, excluded,
    moderate, generational, constrained, national).

% Study labor-supply effects of transfer programs. Publish evidence on reservation wages, employment elasticities, and human-capital depreciation. Provide the empirical framework through which dependency is diagnosed but do not directly experience the transfer or its tax burden.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, labor_economists, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_conditionality__dependency_trap_reading, welfare_state_administrators).
narrative_ontology:fixing_cost_class(income_support_conditionality__dependency_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provide income security and poverty relief without the administrative burden, surveillance, and stigma associated with means-testing or work requirements.
% TRANSFER_FUNCTION: Moves tax revenue from taxpayers to unconditional recipients, while recipient human capital, labor-market attachment, and future earnings capacity are depleted over time through disuse and atrophy.
% ABSENT_VOICES: Work-conditionality advocates and low-wage employed workers who bear tax costs but lack organized political voice are excluded from the policy conversation; recipient voices are present but organized around maintenance of the transfer rather than exit from dependency.
% DISAPPEARANCE_RATIONALE: Overnight removal would trigger immediate poverty and housing instability among long-dependent recipients, a sudden labor-supply influx as idled workers searched for jobs, fiscal reallocation for taxpayers, and collapse of the administrative apparatus and its political coalition.
% FOUNDING_PROBLEM: Industrial poverty, income volatility, and the administrative cruelty of poor-law conditionality in early welfare states.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians outside the welfare administration attest that the founding poverty problem was genuine and severe; labor economists and civic-republican theorists outside the beneficiary coalition contest whether unconditional design remains an appropriate response or has become an obsolete mechanism that perpetuates the conditions it was built to solve.
narrative_ontology:disappearance_verdict(income_support_conditionality__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__dependency_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(income_support_conditionality__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__dependency_trap_reading, 0.76, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.76) is high because the arrangement systematically depletes recipient human capital and imposes deadweight fiscal costs; suppression (0.72) is high because the unconditional design requires active exclusion of work-conditionality alternatives and tax enforcement to sustain the transfer. Theater_ratio (0.48) reflects that humanitarian anti-poverty rhetoric performs genuine legitimating work even as the dependency trap deepens. Accessibility_collapse (0.70) captures the atrophy of self-sufficiency pathways once recipients adapt to unconditional support. Resistance (0.45) is moderate because the arrangement faces ongoing political and scholarly contestation. The measurement series share one time grid so temporal analysis has aligned inputs.
 *
 * PERSPECTIVAL GAP:
 *   The welfare_state_administrators seat experiences the constraint as necessary humanitarian coordination with manageable fiscal trade-offs. The ubi_recipients seat experiences it as a closed trap: the longer the transfer persists, the harder labor-market re-entry becomes. The taxpayers seat experiences it as a compulsory wealth transfer that produces no reciprocal benefit. The engine will compute high directionality (near target) for the payer seats and low directionality (near beneficiary) for the agenda-setter seat, producing divergent per-seat classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to structural relationships: welfare_state_administrators are declared beneficiaries because they derive institutional budget and authority from the program's scale (d near 0.0). ubi_recipients and taxpayers are declared victims because they respectively bear the costs of skill atrophy and forced fiscal contribution (d near 1.0). Recipients' trapped exit and powerless status amplify effective extraction; taxpayers' organized status and constrained exit produce slightly lower but still high effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpoverty relief without bureaucratic crueltyâwas plausibly live when the kernel emerged. The dependency_trap reading argues the arrangement has outlived its functional justification and now operates as extraction. The framework prevents mislabeling this as mere piton because there is a concentrated beneficiary (the administrative apparatus) that actively resists reform toward conditionality, and because the suppression of alternatives (work requirements, training mandates) is structural and intentional, not merely inertial. The R5 mismatch consumer will note founding_problem_status=contested paired with disappearance_verdict=world_rearranges, flagging the arrangement for lifecycle review without pre-adjudicating the verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_ambiguity,
    'Is recipient dependency driven by structural labor-market exclusion or by internalized learned helplessness and identity fusion with beneficiary status?',
    'Longitudinal studies of recipient outcomes post-reform: if employment rates rise sharply when conditionality is introduced, suppression was partly internalized; if they remain low due to structural barriers, suppression was external.',
    'If internalized, effective extraction exceeds structural measures because the target carries the trap after any policy change; if structural, the constraint is better modeled as a mountain of labor-market dysfunction rather than a snare of policy design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized mechanism of the dependency trap').

omega_variable(
    kernel_contest_income_support,
    'Does the income_support_conditionality kernel admit only these three readings, or do hybrid designs (e.g., participation income) constitute a distinct structural constraint?',
    'Comparative policy analysis of hybrid models: if their epsilon profiles differ significantly from all three readings, they form a separate constraint in the family.',
    'Would expand the constraint family and potentially resolve some of the binary polarization between trap and liberation framings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_income_support, conceptual, 'Kernel completeness and hybrid policy designs').

omega_variable(
    empirical_validity_dependency_trap,
    'Is the dependency trapâlong-term skill atrophy and work disincentive from unconditional transfersâempirically robust across institutional contexts, or is it ideologically contingent on specific labor-market structures?',
    'Meta-analysis of natural experiments in unconditional transfer programs across varying labor-market tightness and training availability.',
    'If empirically fragile, this reading''s epsilon is overstated and the constraint may recompute as tangled_rope or rope; if robust, the snare classification is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_validity_dependency_trap, empirical, 'Empirical robustness of the dependency trap thesis').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__dependency_trap_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(income_dep_trap_tr_t0, income_support_conditionality__dependency_trap_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(income_dep_trap_tr_t10, income_support_conditionality__dependency_trap_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(income_dep_trap_tr_t20, income_support_conditionality__dependency_trap_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(income_dep_trap_tr_t30, income_support_conditionality__dependency_trap_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(income_dep_trap_tr_t40, income_support_conditionality__dependency_trap_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(income_dep_trap_be_t0, income_support_conditionality__dependency_trap_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(income_dep_trap_be_t10, income_support_conditionality__dependency_trap_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(income_dep_trap_be_t20, income_support_conditionality__dependency_trap_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(income_dep_trap_be_t30, income_support_conditionality__dependency_trap_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(income_dep_trap_be_t40, income_support_conditionality__dependency_trap_reading, base_extractiveness, 40, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(income_dep_trap_su_t0, income_support_conditionality__dependency_trap_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(income_dep_trap_su_t10, income_support_conditionality__dependency_trap_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(income_dep_trap_su_t20, income_support_conditionality__dependency_trap_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(income_dep_trap_su_t30, income_support_conditionality__dependency_trap_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(income_dep_trap_su_t40, income_support_conditionality__dependency_trap_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, wage_subsidy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the income_support_conditionality kernel. The dependency_trap_reading views unconditional support as generating a snare of learned dependency; the freedom_floor_reading views the same policy as expanding positive liberty; the wage_subsidy_reading views it as covert employer subsidy. They are linked as a constraint family because the natural-language concept 'income support conditionality' conflates structurally distinct claims with different beneficiary and victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
