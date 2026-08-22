% ============================================================================
% CONSTRAINT STORY: income_support_commitment__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__dependency_trap_reading, []).

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
 *   constraint_id: income_support_commitment__dependency_trap_reading
 *   human_readable: Unconditional Income Support as Dependency-Inducing Work Disincentive
 *   domain: political_economy/social_policy
 *
 * SUMMARY:
 *   This story instantiates the dependency-trap reading of the income-support
 *   commitment kernel: unconditional payments, evaluated from the standpoint
 *   that work incentives are the primary lever governing labor supply and
 *   skill maintenance, are read as producing a work-disincentive effect that
 *   compounds over time into skill atrophy and structural state dependence.
 *   The standing arrangement under contest is the existing unconditional
 *   transfer system as currently administered — not any
 *   activation-requirement alternative this reading might endorse. Extraction
 *   runs from the working, taxpaying population toward non-participating
 *   recipients, with the beneficiary class (those who exit or reduce
 *   labor-market engagement) structurally distinct from the victim class
 *   (working taxpayers funding the transfer, and recipients whose own skills
 *   degrade under prolonged non-participation).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__dependency_trap_reading, 0.52).
domain_priors:suppression_score(income_support_commitment__dependency_trap_reading, 0.38).
domain_priors:theater_ratio(income_support_commitment__dependency_trap_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__dependency_trap_reading, tangled_rope).
narrative_ontology:human_readable(income_support_commitment__dependency_trap_reading, "Unconditional Income Support as Dependency-Inducing Work Disincentive").
narrative_ontology:topic_domain(income_support_commitment__dependency_trap_reading, "political_economy/social_policy").

domain_priors:requires_active_enforcement(income_support_commitment__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__dependency_trap_reading, '660747d4-2bda-4285-8a54-437680bf9b4d').
narrative_ontology:cs_kernel_codification('660747d4-2bda-4285-8a54-437680bf9b4d', distributed).
narrative_ontology:cs_authority_grounding('660747d4-2bda-4285-8a54-437680bf9b4d', distributed).
narrative_ontology:cs_reading_relation('660747d4-2bda-4285-8a54-437680bf9b4d', income_support_commitment__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('660747d4-2bda-4285-8a54-437680bf9b4d', income_support_commitment__targeting_efficiency_reading, influences).
narrative_ontology:cs_axiom('660747d4-2bda-4285-8a54-437680bf9b4d', foundational, work_conditionality_preserves_incentive_structure).
narrative_ontology:cs_axiom_status(work_conditionality_preserves_incentive_structure, holdable).
narrative_ontology:cs_axiom_grounding('660747d4-2bda-4285-8a54-437680bf9b4d', work_conditionality_preserves_incentive_structure, empirically_contingent).
narrative_ontology:cs_axiom('660747d4-2bda-4285-8a54-437680bf9b4d', secondary, unconditional_transfer_duration_causes_skill_decay).
narrative_ontology:cs_axiom_status(unconditional_transfer_duration_causes_skill_decay, holdable).
narrative_ontology:cs_axiom_grounding('660747d4-2bda-4285-8a54-437680bf9b4d', unconditional_transfer_duration_causes_skill_decay, empirically_contingent).
narrative_ontology:cs_reference_frame('660747d4-2bda-4285-8a54-437680bf9b4d', conditional_relief_and_activation_norm).
narrative_ontology:cs_drift_state('660747d4-2bda-4285-8a54-437680bf9b4d', post_universal_transfer_expansion, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('660747d4-2bda-4285-8a54-437680bf9b4d', '').
narrative_ontology:cs_kernel_id(income_support_commitment__dependency_trap_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__dependency_trap_reading, labor_market_exiters).
narrative_ontology:constraint_beneficiary(income_support_commitment__dependency_trap_reading, long_term_benefit_recipients).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, working_taxpayers).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, skill_atrophied_recipients).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, long_term_benefit_recipients).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive unconditional payments regardless of work status and choose to reduce or exit labor force participation. From this reading's lens, the transfer removes the marginal incentive to seek employment, and duration of non-participation compounds over time as skills and work habits fade.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, labor_market_exiters, beneficiary,
    moderate, biographical, constrained, national).

% Have relied on the unconditional transfer for years; the payment is a genuine floor against destitution but, on this reading, also removes pressure toward reskilling or job search. Some become intergenerationally reliant on the program as the default household income strategy.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, long_term_benefit_recipients, beneficiary,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__dependency_trap_reading, long_term_benefit_recipients, payer).

% Fund the transfer system through payroll and income taxation while continuing to work full hours. They bear the fiscal cost of subsidizing non-participation and, on this reading, face a widening tax burden as the non-working share grows and the productive base that funds transfers narrows.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, working_taxpayers, payer,
    moderate, biographical, constrained, national).

% Received the unconditional benefit during a period of unemployment, but the extended absence from the labor market eroded their occupational skills and professional networks, making re-entry harder and costlier the longer they remain outside the workforce. In this reading, they are also victims of the arrangement's own logic — the support meant to help them ultimately deepens dependence.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, skill_atrophied_recipients, payer,
    powerless, biographical, trapped, national).

% Designs eligibility rules, sets payment levels, and administers the unconditional transfer. Justifies the program as a floor against poverty; on this reading also bears responsibility for not building in graduated work incentives or activation requirements that would counter the disincentive effect.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, welfare_administering_agency, agenda_setter,
    institutional, generational, analytical, national).

% Report difficulty filling entry-level and low-wage positions when unconditional support makes non-work a viable alternative. They are not consulted in benefit design and have no direct voice in setting eligibility or work-incentive structures, despite absorbing the labor-supply consequences.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, employers_facing_labor_shortage, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_commitment__dependency_trap_reading, labor_market_exiters).
narrative_ontology:fixing_cost_class(income_support_commitment__dependency_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The transfer coordinates a societal floor against destitution by pooling tax revenue and redistributing it unconditionally to those without earned income, removing the need for case-by-case need verification.
% TRANSFER_FUNCTION: Moves tax revenue collected from working, income-earning taxpayers to non-working or under-working recipients, with no work requirement or time limit attached to receipt.
% ABSENT_VOICES: Employers facing labor-supply gaps and future taxpayers who will inherit a narrower productive base are not represented in the political process that sets benefit generosity and duration; their costs are diffuse and arrive later than the benefit is granted.
% DISAPPEARANCE_RATIONALE: If the unconditional payment vanished overnight, a meaningful share of long-term recipients would face acute income shortfalls forcing rapid labor-market re-entry (with skill mismatches surfacing immediately), the tax burden on working households would fall, and the labor-supply constraints reported by employers would likely ease — the arrangement structurally shapes both who works and how much revenue circulates.
% FOUNDING_PROBLEM: Cyclical unemployment and poverty left households without income during job loss or economic downturn, and means-tested aid was slow, stigmatizing, and administratively costly to verify.
% FOUNDING_PROBLEM_CORROBORATION: Labor economists studying benefit-duration effects and employer associations reporting unfilled positions attest, from outside the recipient and administering-agency seats, that the disincentive effect is measurable in some populations and benefit designs; program administrators and recipient advocacy groups dispute the magnitude and attribute continued need to labor-market conditions rather than the transfer itself.
narrative_ontology:disappearance_verdict(income_support_commitment__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__dependency_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(income_support_commitment__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__dependency_trap_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__dependency_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_commitment__dependency_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_commitment__dependency_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness begins moderate (0.34) and rises to 0.52 over the interval as the reading models compounding effects: each additional year outside the labor market both deepens individual skill atrophy and widens the share of the population drawing unconditional support relative to the taxed base. Suppression is comparatively low-moderate (0.38) because recipients are not coercively held in the program — the suppression captured here is the more subtle erosion of exit capacity through skill decay and habituation to non-work, not legal restriction. Theater ratio is modest (0.28) reflecting that the coordination (poverty floor) function is real, though an increasing share of program communication over time frames outcomes in terms that obscure the labor-supply tradeoff. All three metrics share one time grid across the full 24-period interval.
 *
 * PERSPECTIVAL GAP:
 *   From the welfare-administering agency's seat, the program is coordination against destitution functioning largely as designed. From working taxpayers' seat and from labor economists studying benefit-duration effects, the same structure computes as a widening extraction channel funding non-participation. The engine computes these divergent seat-level readings from the authored structural data; this story does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Labor-market exiters and long-term recipients sit toward the beneficiary end of directionality — they receive net transfers and, in this reading, capture the disincentive's upside in the near term even as it costs them long-run employability. Working taxpayers sit toward the target end — they fund the transfer without drawing it, bearing rising fiscal exposure as the non-working share grows. Skill-atrophied recipients occupy a dual position: nominal beneficiaries of the transfer who become victims of the reading's own causal mechanism, which is why they carry secondary_role payer alongside their beneficiary role.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents this reading from being flattened into either 'pure welfare coordination' (which would erase the disincentive costs this reading centers) or 'pure extraction snare' (which would erase the genuine poverty-floor coordination function the transfer performs). Both a real coordination function and asymmetric extraction must be present simultaneously for this reading to hold, and the authored beneficiary/victim split reflects that duality rather than resolving it in either direction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    disincentive_magnitude_empirical_status,
    'Does unconditional income support produce a measurable, economically significant reduction in labor supply and skill maintenance, or is the observed correlation between benefit receipt and non-participation driven by pre-existing labor-market exclusion (disability, care burdens, discrimination) rather than the transfer itself?',
    'Randomized or quasi-experimental studies of unconditional cash transfers (e.g., basic income pilots) measuring labor-supply elasticity and skill/employment trajectories against matched control groups over multi-year horizons.',
    'If the disincentive effect is small or absent in rigorous studies, this reading''s claimed extraction mechanism substantially weakens and the constraint''s true ε would sit much closer to the freedom_floor_reading''s low-extraction account; if the effect is robust and large, the tangled_rope classification and rising ε trajectory authored here are strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disincentive_magnitude_empirical_status, empirical, 'Whether the core causal claim of this reading (support causes disincentive/atrophy) is empirically robust or an artifact of selection.').

omega_variable(
    kernel_reading_selection_and_framing,
    'Is the dependency-trap framing the most defensible lens on unconditional support, or does foregrounding work-incentive effects presuppose that labor-market participation is the correct normative benchmark against which the arrangement should be judged?',
    'Comparative political-economy analysis of how different normative starting points (labor-force attachment vs. autonomy/dignity vs. fiscal targeting efficiency) each generate internally coherent but differently-weighted readings of the same transfer system; no single empirical test resolves which starting point is correct, since the disagreement is partly about what counts as the relevant harm.',
    'If the analytical community judges labor-force attachment to be a contingent, era-specific policy goal rather than a neutral baseline, the dependency-trap reading''s ε and victim/beneficiary framing would be seen as one contestable normative construction among several rather than a description of an intrinsic feature of the transfer — this is precisely the committer structure the kernel decomposition is designed to hold open rather than resolve within a single constraint file.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_and_framing, conceptual, 'Whether foregrounding work disincentive over autonomy or targeting efficiency reflects a defensible or contestable normative starting point among the three kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__dependency_trap_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__dependency_trap_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(inco_tr_t4, income_support_commitment__dependency_trap_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(inco_tr_t8, income_support_commitment__dependency_trap_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(inco_tr_t12, income_support_commitment__dependency_trap_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement(inco_tr_t16, income_support_commitment__dependency_trap_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(inco_tr_t20, income_support_commitment__dependency_trap_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(inco_tr_t24, income_support_commitment__dependency_trap_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__dependency_trap_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(inco_be_t4, income_support_commitment__dependency_trap_reading, base_extractiveness, 4, 0.39).
narrative_ontology:measurement(inco_be_t8, income_support_commitment__dependency_trap_reading, base_extractiveness, 8, 0.43).
narrative_ontology:measurement(inco_be_t12, income_support_commitment__dependency_trap_reading, base_extractiveness, 12, 0.46).
narrative_ontology:measurement(inco_be_t16, income_support_commitment__dependency_trap_reading, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(inco_be_t20, income_support_commitment__dependency_trap_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(inco_be_t24, income_support_commitment__dependency_trap_reading, base_extractiveness, 24, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_commitment__dependency_trap_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(inco_su_t4, income_support_commitment__dependency_trap_reading, suppression_requirement, 4, 0.26).
narrative_ontology:measurement(inco_su_t8, income_support_commitment__dependency_trap_reading, suppression_requirement, 8, 0.29).
narrative_ontology:measurement(inco_su_t12, income_support_commitment__dependency_trap_reading, suppression_requirement, 12, 0.32).
narrative_ontology:measurement(inco_su_t16, income_support_commitment__dependency_trap_reading, suppression_requirement, 16, 0.34).
narrative_ontology:measurement(inco_su_t20, income_support_commitment__dependency_trap_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement(inco_su_t24, income_support_commitment__dependency_trap_reading, suppression_requirement, 24, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__dependency_trap_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_commitment__dependency_trap_reading, 0.15).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, income_support_commitment__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, income_support_commitment__targeting_efficiency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the income_support_commitment kernel. freedom_floor_reading treats the same unconditional transfer as an autonomy-enabling floor with low extraction; targeting_efficiency_reading treats the debate as fundamentally about universality vs. means-testing rather than dependency per se. Each reading carries its own ε, beneficiary/victim structure, and classification; they are linked here rather than merged because the ε-invariance principle requires one stable extraction value per constraint, and these three readings assign structurally different ε values to what a natural-language observer might call 'the same policy.'

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
