% ============================================================================
% CONSTRAINT STORY: income_support_commitment__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
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
 *   human_readable: Unconditional Income Support as Work-Disincentive (Dependency Trap Reading)
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This story instantiates the dependency-trap reading of the
 *   income-support-commitment kernel: an unconditional cash transfer, read as
 *   a work-disincentive whose primary structural effect is labor-force exit
 *   among marginal recipients and skill atrophy that compounds their eventual
 *   dependence. Under this reading, working taxpayers subsidize
 *   non-participation, and recipients who remain detached from work for
 *   extended periods are themselves harmed by the same mechanism that pays
 *   them, since human capital depreciates outside employment. This is a
 *   distinct constraint from the freedom_floor_reading (which reads the same
 *   transfer as enabling autonomous choice) and the
 *   targeting_efficiency_reading (which reads the debate as one of allocation
 *   mechanism rather than work incentive) — each reading has its own
 *   beneficiary/victim structure and its own epsilon, and none averages with
 *   the others.
 *
 * KEY AGENTS:
 *   - labor_market_exiters: beneficiary (moderate/constrained) — collect the transfer, reduce participation
 *   - working_taxpayers: payer (organized/constrained) — fund the transfer and the foregone output
 *   - skill_atrophied_recipients: payer/beneficiary (powerless/trapped) — subsidized short-term, harmed long-term
 *   - welfare_administering_state: agenda_setter (institutional/analytical) — designs and enforces eligibility
 *   - employers_facing_labor_shortage: excluded (powerful/constrained) — absorb wage pressure with no design voice
 *   - labor_economists_dependency_school: observer (analytical/analytical) — supplies the evidentiary basis for this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__dependency_trap_reading, 0.52).
domain_priors:suppression_score(income_support_commitment__dependency_trap_reading, 0.4).
domain_priors:theater_ratio(income_support_commitment__dependency_trap_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__dependency_trap_reading, tangled_rope).
narrative_ontology:human_readable(income_support_commitment__dependency_trap_reading, "Unconditional Income Support as Work-Disincentive (Dependency Trap Reading)").
narrative_ontology:topic_domain(income_support_commitment__dependency_trap_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(income_support_commitment__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__dependency_trap_reading, '92bde22e-4cb3-4341-bc23-a78575369792').
narrative_ontology:cs_kernel_codification('92bde22e-4cb3-4341-bc23-a78575369792', distributed).
narrative_ontology:cs_authority_grounding('92bde22e-4cb3-4341-bc23-a78575369792', distributed).
narrative_ontology:cs_reading_relation('92bde22e-4cb3-4341-bc23-a78575369792', income_support_commitment__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('92bde22e-4cb3-4341-bc23-a78575369792', income_support_commitment__targeting_efficiency_reading, influences).
narrative_ontology:cs_axiom('92bde22e-4cb3-4341-bc23-a78575369792', foundational, unconditional_transfer_erodes_work_norm).
narrative_ontology:cs_axiom_status(unconditional_transfer_erodes_work_norm, holdable).
narrative_ontology:cs_axiom_grounding('92bde22e-4cb3-4341-bc23-a78575369792', unconditional_transfer_erodes_work_norm, empirically_contingent).
narrative_ontology:cs_axiom('92bde22e-4cb3-4341-bc23-a78575369792', secondary, labor_force_attachment_is_a_protected_public_good).
narrative_ontology:cs_axiom_status(labor_force_attachment_is_a_protected_public_good, holdable).
narrative_ontology:cs_axiom_grounding('92bde22e-4cb3-4341-bc23-a78575369792', labor_force_attachment_is_a_protected_public_good, instrumental).
narrative_ontology:cs_reference_frame('92bde22e-4cb3-4341-bc23-a78575369792', residual_welfare_conditionality_norm).
narrative_ontology:cs_drift_state('92bde22e-4cb3-4341-bc23-a78575369792', post_universal_basic_income_pilot_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('92bde22e-4cb3-4341-bc23-a78575369792', '').
narrative_ontology:cs_kernel_id(income_support_commitment__dependency_trap_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__dependency_trap_reading, labor_market_exiters).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, working_taxpayers).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, skill_atrophied_recipients).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(income_support_commitment__dependency_trap_reading, skill_atrophied_recipients).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive unconditional payments regardless of work status and, in this reading, use that floor to reduce or exit labor-force participation. They collect the transfer continuously without a work test, and the tax base funds their non-participation. From the taxing side this reading treats their situation as a moral hazard the design failed to price in.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, labor_market_exiters, beneficiary,
    moderate, biographical, constrained, national).

% Fund the transfer through payroll and income taxation while continuing to work full labor hours. They bear the fiscal cost of both the direct payment and the foregone output of exiters, with no mechanism to reduce their contribution proportional to program participation. Exit from the tax base is possible only through emigration or informality, both costly.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, working_taxpayers, payer,
    organized, biographical, constrained, national).

% Receive the floor income but, in this reading, experience skill and network decay the longer they remain outside employment, making eventual re-entry to the labor market progressively harder. They are simultaneously subsidized in the short run and structurally disadvantaged in the long run — a victim of the same mechanism that pays them.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, skill_atrophied_recipients, payer,
    powerless, generational, trapped, local).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__dependency_trap_reading, skill_atrophied_recipients, beneficiary).

% Designs, funds, and enforces the unconditional eligibility rule, sets tax rates to cover it, and administers disbursement without work conditionality. Could add work requirements or phase-outs but bears none of the fiscal or skill-atrophy costs directly.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, welfare_administering_state, agenda_setter,
    institutional, generational, analytical, national).

% Compete for workers against a payment floor that raises the reservation wage for low-productivity jobs. They are not party to the eligibility design and have no formal voice in setting it, though they absorb higher wage costs or unfilled positions.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, employers_facing_labor_shortage, excluded,
    powerful, biographical, constrained, national).

% Study labor-force participation elasticities and skill-depreciation curves among long-term recipients, producing the evidence base this reading draws on. They advocate for work conditionality or time limits based on observed participation declines.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, labor_economists_dependency_school, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_commitment__dependency_trap_reading, labor_market_exiters).
narrative_ontology:fixing_cost_class(income_support_commitment__dependency_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a income floor that removes destitution risk without means-testing administration costs, in principle coordinating a basic social insurance function across the whole population in one stroke.
% TRANSFER_FUNCTION: Moves tax revenue from continuously working taxpayers to recipients who, on this reading, use the floor to reduce or exit labor-force participation; a secondary transfer moves long-run earning capacity away from recipients themselves as skills atrophy.
% ABSENT_VOICES: Employers facing labor shortages and future taxpayers who will inherit a larger dependent population have no formal seat in eligibility design; the labor economists documenting participation decline are consulted unevenly and often after policy is locked in.
% DISAPPEARANCE_RATIONALE: If the unconditional transfer vanished overnight, labor-force participation among marginal recipients would rise (per this reading's own model), tax rates could fall, and the skill-atrophy trajectory would reverse for those still able to re-enter work — though those already long-term detached would face acute income loss without a transition mechanism.
% FOUNDING_PROBLEM: Poverty and income volatility among households without stable employment, which conditional and means-tested programs failed to reach efficiently or with dignity.
% FOUNDING_PROBLEM_CORROBORATION: The administering state and recipient advocates attest the poverty-prevention problem remains live and is being solved. Labor economists in the dependency school, drawing on longitudinal labor-force participation data collected independently of program administrators, attest that a distinct problem — chronic non-participation and skill depreciation — has been created or worsened by the unconditional design, corroborating this reading's claim from outside the beneficiary group.
narrative_ontology:disappearance_verdict(income_support_commitment__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__dependency_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.52) reflecting a genuine transfer from continuously working taxpayers to non-participating recipients, but the transfer includes a real coordination function (poverty floor) that keeps epsilon well below a pure-snare level. Suppression is moderate (0.40) because the arrangement persists through payroll tax enforcement and program eligibility rules rather than through coercive exit-blocking; accessibility_collapse is comparatively low (0.35) because recipients formally retain the option to work — the atrophy mechanism operates through capacity erosion, not through blocked alternatives, which this reading treats as the crux of the harm. Resistance (0.60) is elevated because working-taxpayer organizations and dependency-school economists actively contest the design.
 *
 * PERSPECTIVAL GAP:
 *   From the administering state's seat this looks like successful poverty prevention; from the working-taxpayer seat it looks like an open-ended subsidy for non-participation; from the skill-atrophied recipient's own seat, the short-run relief and the long-run capacity loss are both true simultaneously — the same transfer that beneficiary and victim in this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Labor_market_exiters are declared full beneficiaries — they collect the transfer and reduce their labor input, so directionality sits near the subsidized end. Working_taxpayers are declared victims with constrained exit (tax obligations are not easily escaped), placing them near the full-target end. Skill_atrophied_recipients carry the unusual dual role: beneficiary of the immediate transfer, victim of its compounding effect on their own future earning capacity — this asymmetry across time horizons is exactly what the dependency-trap reading claims is invisible to a cross-sectional evaluation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (destitution prevention) remains partially live, which keeps this from being a clean mandatrophy case — the coordination function has not become pure inertia. But the dependency-school corroboration argues a second, unaddressed problem (chronic non-participation) has been created by the same mechanism, without triggering redesign. Classifying this as tangled_rope rather than snare preserves the genuine poverty-floor coordination function while still registering the asymmetric extraction the dependency reading identifies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    labor_supply_elasticity_magnitude,
    'How large is the actual labor-supply response to an unconditional income floor — do marginal recipients meaningfully reduce work hours or exit entirely, or is the effect small relative to poverty-reduction benefits?',
    'Randomized or quasi-experimental basic-income trials with long-run (5+ year) labor-force tracking, compared across designs with and without conditionality.',
    'A small elasticity would undercut this reading''s core premise and shift the constraint''s classification toward the freedom_floor_reading''s rope-like structure; a large, sustained elasticity would support treating the extraction as more severe than currently authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_supply_elasticity_magnitude, empirical, 'Whether the work-disincentive effect this reading assumes is empirically large or small.').

omega_variable(
    skill_atrophy_causal_direction,
    'Does time outside employment cause skill atrophy and reduced re-entry prospects, or do recipients who were already lower-skilled/lower-attachment simply select into longer non-participation (reverse causation)?',
    'Longitudinal studies with pre-program skill and employment trajectories as controls, distinguishing selection from treatment effects.',
    'If selection dominates, the ''atrophy victim'' framing for skill_atrophied_recipients weakens substantially and the constraint looks more like a rope with a pre-existing vulnerable population; if causation dominates, the tangled_rope classification with recipients as partial victims is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_atrophy_causal_direction, empirical, 'Whether skill atrophy is caused by the transfer or merely correlated with pre-existing recipient characteristics.').

omega_variable(
    kernel_reading_selection_basis,
    'Given the same underlying income-support commitment, what determines which reading (dependency_trap, freedom_floor, targeting_efficiency) a given analyst or policymaker adopts — is it prior ideological commitment, differential weighting of the same evidence base, or genuinely different empirical beliefs?',
    'Cross-national comparison of policy adoption patterns against pre-existing welfare-state ideology measures, controlling for local labor-market conditions.',
    'If reading selection tracks ideology more than evidence, all three sibling constraints should be read as contested normative framings of a single ambiguous policy space rather than as three empirically resolvable claims — this affects how much weight any single reading''s epsilon should carry in aggregate policy analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, conceptual, 'Whether the choice among kernel readings is empirically or ideologically driven.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__dependency_trap_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__dependency_trap_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(inco_tr_t4, income_support_commitment__dependency_trap_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(inco_tr_t8, income_support_commitment__dependency_trap_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(inco_tr_t12, income_support_commitment__dependency_trap_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(inco_tr_t16, income_support_commitment__dependency_trap_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(inco_tr_t20, income_support_commitment__dependency_trap_reading, theater_ratio, 20, 0.27).
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
narrative_ontology:measurement(inco_su_t0, income_support_commitment__dependency_trap_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(inco_su_t4, income_support_commitment__dependency_trap_reading, suppression_requirement, 4, 0.32).
narrative_ontology:measurement(inco_su_t8, income_support_commitment__dependency_trap_reading, suppression_requirement, 8, 0.34).
narrative_ontology:measurement(inco_su_t12, income_support_commitment__dependency_trap_reading, suppression_requirement, 12, 0.36).
narrative_ontology:measurement(inco_su_t16, income_support_commitment__dependency_trap_reading, suppression_requirement, 16, 0.37).
narrative_ontology:measurement(inco_su_t20, income_support_commitment__dependency_trap_reading, suppression_requirement, 20, 0.39).
narrative_ontology:measurement(inco_su_t24, income_support_commitment__dependency_trap_reading, suppression_requirement, 24, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__dependency_trap_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_commitment__dependency_trap_reading, 0.12).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, targeting_efficiency_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'unconditional income support commitment' per the ε-invariance principle. freedom_floor_reading shares the same underlying transfer mechanism but authors near-zero epsilon and rope classification, reflecting a beneficiary-only structure (recipients as full beneficiaries, taxpayers as willing coordinators rather than victims). targeting_efficiency_reading treats the contested claim as an allocation-mechanism question independent of work-incentive effects and authors its own distinct epsilon around administrative and leakage costs. All three are linked via affects_constraints; none should be merged or averaged — each is evaluated on its own reading's terms per DP-001.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
