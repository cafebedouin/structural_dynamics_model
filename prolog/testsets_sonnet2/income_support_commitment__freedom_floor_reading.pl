% ============================================================================
% CONSTRAINT STORY: income_support_commitment__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: income_support_commitment__freedom_floor_reading
 *   human_readable: Unconditional Income Floor as Autonomy and Exit-Capacity Guarantee
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This story instantiates the freedom-floor reading of the
 *   income-support-commitment kernel: unconditional income support as the
 *   material precondition for autonomy, dignity, and genuine labor-market and
 *   household exit capacity. Under this reading, the arrangement is a
 *   coordination mechanism that solves a real collective problem (subsistence
 *   security independent of employer or household power) with low intrinsic
 *   extraction — funding it is a tax-base coordination problem, not an
 *   extraction machine. The universality feature specifically eliminates the
 *   victim class that a means-tested alternative would produce (stigmatized,
 *   administratively burdened claimants), which is why this reading declares
 *   no victims. Two sibling readings of the same kernel are NOT part of this
 *   constraint: the dependency-trap reading (which frames the same transfer
 *   as skill-atrophying and dependency-inducing) and the targeting-efficiency
 *   reading (which frames universal distribution as wasteful relative to
 *   means-tested concentration) are separate constraints with their own ε
 *   values and stakeholder structures, linked here only via network
 *   reference.
 *
 * KEY AGENTS:
 *   - unpaid_caregivers: primary beneficiary (powerless/constrained) — gains material exit option from unpaid-labor trap
 *   - precarious_workers: primary beneficiary (powerless/constrained) — gains genuine reservation wage
 *   - domestic_abuse_survivors: primary beneficiary (powerless/trapped) — gains exit capacity from coercive household
 *   - artists_and_entrepreneurs: beneficiary (moderate/constrained) — gains risk-absorption for long-horizon work
 *   - general_taxpayers: payer/beneficiary (organized/constrained) — funds the floor, shares its coordination benefits
 *   - low_wage_employers: excluded party (powerful/constrained) — loses wage-setting leverage this reading treats as illegitimate
 *   - policy_analysts: analytical observer — assesses empirical claims against sibling readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__freedom_floor_reading, 0.18).
domain_priors:suppression_score(income_support_commitment__freedom_floor_reading, 0.08).
domain_priors:theater_ratio(income_support_commitment__freedom_floor_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__freedom_floor_reading, rope).
narrative_ontology:human_readable(income_support_commitment__freedom_floor_reading, "Unconditional Income Floor as Autonomy and Exit-Capacity Guarantee").
narrative_ontology:topic_domain(income_support_commitment__freedom_floor_reading, "political_economy/social_policy/welfare_state_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__freedom_floor_reading, 'aa9b80ae-639d-46a7-ac71-d90953be1989').
narrative_ontology:cs_kernel_codification('aa9b80ae-639d-46a7-ac71-d90953be1989', distributed).
narrative_ontology:cs_authority_grounding('aa9b80ae-639d-46a7-ac71-d90953be1989', distributed).
narrative_ontology:cs_reading_relation('aa9b80ae-639d-46a7-ac71-d90953be1989', income_support_commitment__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('aa9b80ae-639d-46a7-ac71-d90953be1989', income_support_commitment__targeting_efficiency_reading, coexists_with).
narrative_ontology:cs_axiom('aa9b80ae-639d-46a7-ac71-d90953be1989', foundational, material_security_is_precondition_for_autonomy).
narrative_ontology:cs_axiom_status(material_security_is_precondition_for_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('aa9b80ae-639d-46a7-ac71-d90953be1989', material_security_is_precondition_for_autonomy, deontological).
narrative_ontology:cs_axiom('aa9b80ae-639d-46a7-ac71-d90953be1989', foundational, universality_eliminates_stigma_extraction).
narrative_ontology:cs_axiom_status(universality_eliminates_stigma_extraction, holdable).
narrative_ontology:cs_axiom_grounding('aa9b80ae-639d-46a7-ac71-d90953be1989', universality_eliminates_stigma_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('aa9b80ae-639d-46a7-ac71-d90953be1989', labor_market_exit_capacity_baseline).
narrative_ontology:cs_drift_state('aa9b80ae-639d-46a7-ac71-d90953be1989', post_pilot_evidence_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('aa9b80ae-639d-46a7-ac71-d90953be1989', '').
narrative_ontology:cs_kernel_id(income_support_commitment__freedom_floor_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, unpaid_caregivers).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, domestic_abuse_survivors).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, artists_and_entrepreneurs).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, general_taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(income_support_commitment__freedom_floor_reading, general_taxpayers).
narrative_ontology:constraint_victim(income_support_commitment__freedom_floor_reading, low_wage_employers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Perform childcare, eldercare, or disability care that markets do not compensate. A floor income removes the requirement to accept paid employment they cannot combine with caregiving, converting unpaid labor from an economically punished choice into a materially viable one.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, unpaid_caregivers, beneficiary,
    powerless, biographical, constrained, national).

% Work unstable, low-wage, or exploitative jobs because refusing them means destitution. The floor gives them a genuine reservation wage and the ability to decline or leave bad jobs without immediate crisis, shifting bargaining leverage toward them at the margin.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, precarious_workers, beneficiary,
    powerless, immediate, constrained, national).

% Remain in abusive households substantially because leaving means immediate financial collapse, especially where they have no independent income history. An unconditional floor payable directly to the individual (not household-means-tested) provides the material precondition for exit that no counseling service alone can supply.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, domestic_abuse_survivors, beneficiary,
    powerless, immediate, trapped, national).

% Pursue creative or entrepreneurial work with uncertain near-term income. The floor absorbs the early-stage risk that otherwise forces premature abandonment of long-horizon projects for wage employment, functioning as a distributed patronage/seed-capital mechanism.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, artists_and_entrepreneurs, beneficiary,
    moderate, generational, constrained, national).

% Fund the transfer through general taxation. They are payers in the direct fiscal sense but are simultaneously beneficiaries of the coordination good: reduced administrative overhead relative to means-tested systems, reduced crime and health costs associated with poverty, and their own eligibility for the floor if their circumstances change. No group is extracted from asymmetrically — the tax base and the recipient base substantially overlap over a lifetime.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, general_taxpayers, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__freedom_floor_reading, general_taxpayers, beneficiary).

% Currently benefit from a labor supply with weak exit options, which suppresses wages and conditions at the bottom of the market. A universal floor raises the effective reservation wage, constraining their ability to offer sub-subsistence terms. They are not consulted as beneficiaries of the existing arrangement in this reading's framing, and their objection (labor shortages, cost pressure) is treated as a cost of eliminating the prior extraction, not a defect in the floor.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, low_wage_employers, excluded,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__freedom_floor_reading, low_wage_employers, payer).

% Staff the means-testing bureaucracy that unconditional transfer would substantially replace. Their institutional continuity is not part of this reading's coordination function and their objections (administrative expertise, fraud control) are heard as transition-cost concerns rather than as evidence against the floor.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, welfare_administrators, excluded,
    moderate, biographical, constrained, national).

% Study labor supply elasticity, fiscal cost, and behavioral response to unconditional transfers. Can assess whether the freedom-floor reading's empirical predictions (exit capacity used, dignity effects, no mass work withdrawal) hold against the dependency-trap reading's predictions.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, policy_analysts, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_commitment__freedom_floor_reading, diffuse).
narrative_ontology:fixing_cost_class(income_support_commitment__freedom_floor_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools tax revenue to guarantee every individual a subsistence income floor independent of employment status, solving the collective problem that market wages alone do not guarantee survival or genuine exit options from coercive relationships (employment or domestic), and that means-testing to target need imposes stigma and administrative cost that erodes the benefit it delivers.
% TRANSFER_FUNCTION: Moves general tax revenue to every resident as an unconditional payment; net direction of funds runs from higher lifetime earners to lower lifetime earners, but eligibility itself flows to everyone symmetrically, which is the structural feature this reading identifies as eliminating a victim class.
% ABSENT_VOICES: Low-wage employers whose wage-setting power depends on workers lacking exit options are not treated as parties whose interest counts against the floor; their preference for a captive low-wage labor pool is exactly what this reading identifies as the arrangement being corrected. Means-testing bureaucracies whose functions would shrink are also not consulted as stakeholders in this reading's coordination logic.
% DISAPPEARANCE_RATIONALE: If the floor disappeared, caregivers and abuse survivors currently exiting bad situations because of it would face renewed dependence on a spouse's or employer's income; precarious workers would lose the reservation wage that lets them decline the worst jobs; low-wage employers would regain leverage over compensation and conditions. The labor market and household bargaining structure would visibly shift, not merely on paper.
% FOUNDING_PROBLEM: Wage labor markets and household economic structures leave individuals without independent income when they cannot or will not sell their labor on offered terms (due to caregiving duties, abuse, disability, market volatility for creative work, or simple job scarcity), and existing means-tested relief imposes stigma, administrative friction, and clawback rates that blunt its effect.
% FOUNDING_PROBLEM_CORROBORATION: Labor economists studying reservation wages and domestic violence researchers studying the correlation between independent income and shelter-exit rates corroborate the founding problem from outside the beneficiary groups; pilot program evaluators (e.g. cash transfer trial researchers) not aligned with recipient advocacy groups report consistent exit-capacity effects, though effect sizes remain contested by targeting-efficiency-oriented economists.
narrative_ontology:disappearance_verdict(income_support_commitment__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__freedom_floor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(income_support_commitment__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__freedom_floor_reading, 0.18, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low (0.18) and falling slightly over the interval because, from this reading's own lights, the arrangement's function is redistribution-as-insurance rather than rent extraction: the tax base and the beneficiary base substantially overlap across a lifetime, and no identifiable group is structurally targeted for asymmetric loss. Suppression is low (0.08) because no coercive enforcement compels participation beyond ordinary taxation, and there are no exits being blocked. Theater ratio starts moderate (0.20) reflecting early-implementation administrative overhead and skepticism-driven compliance signaling, then falls to 0.10 as the mechanism matures into routine automatic transfer with minimal performative apparatus. Accessibility collapse is low (0.15): alternatives (means-tested welfare, private charity, informal family support) remain available and are not suppressed by this arrangement, they are simply argued to be inferior on dignity and exit-capacity grounds. Resistance is moderate (0.35), reflecting genuine political contestation from employers and targeting-efficiency advocates, not coercive suppression of dissent.
 *
 * PERSPECTIVAL GAP:
 *   The employer seat and the beneficiary seats would compute this constraint very differently: an employer accustomed to a captive low-wage applicant pool experiences the floor as a loss of leverage and may describe it in extraction terms (their labor costs rise, their bargaining position weakens). A precarious worker or abuse survivor experiences the identical mechanism as removal of an extractive precondition (economic coercion) they previously suffered under. The engine computing per-seat types from the structural data should reflect this: the employer's experienced 'cost' here is not this reading's authored victimhood, because this reading holds that suppressed wage-setting power was never a legitimate baseline to protect.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (caregivers, precarious workers, survivors, artists) are declared with low derived directionality — the constraint subsidizes their exit capacity directly. General taxpayers carry a dual role: payer in the direct fiscal sense, but beneficiary in the coordination sense (lower administrative overhead, personal eligibility if circumstances change, reduced downstream social costs), which is why they are authored with secondary_role beneficiary rather than as a pure victim class. No stakeholder is authored purely as `payer` without an offsetting beneficiary status except general_taxpayers' primary role, and no group is authored as a stakeholder role of pure extraction victim, consistent with the reading's zero-victims declaration.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (subsistence insecurity forcing coercive dependence on bad jobs or bad households) is authored as live, not dead — this blocks a mandatrophy misreading in either direction. It prevents mislabeling this as a scaffold whose function has expired (the problem it solves persists as long as market wages and household power asymmetries exist), and it also prevents treating rising administrative maturity (falling theater_ratio) as evidence the coordination function itself has atrophied — the falling theater_ratio here reflects operational maturation, not functional decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_labor_supply_response,
    'Does unconditional income support in practice produce the exit-capacity and dignity effects this reading claims, or does it produce the work-disincentive and dependency effects the sibling dependency_trap_reading claims?',
    'Longitudinal analysis of basic income and cash transfer pilots (e.g. Finland, Kenya GiveDirectly, Stockton SEED) tracking labor force participation, self-reported autonomy/dignity measures, and household exit rates from abusive or exploitative situations, compared against control populations.',
    'If empirical results consistently show minimal labor supply reduction alongside measurable autonomy and exit-capacity gains, this reading''s low-extraction rope classification is well-supported. If results show substantial work withdrawal and skill atrophy without offsetting autonomy gains, the dependency_trap_reading''s framing gains empirical support and this reading''s claimed_type would be contestable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_labor_supply_response, empirical, 'Whether real-world pilot data supports the freedom-floor reading''s predicted effects over the dependency-trap reading''s predicted effects.').

omega_variable(
    universality_vs_targeting_tradeoff,
    'Is universal distribution (this reading) structurally necessary to eliminate means-test stigma and administrative victimhood, or can targeting_efficiency_reading''s concentrated allocation achieve the same dignity effects at lower fiscal cost through well-designed targeting mechanisms?',
    'Comparative institutional analysis of stigma and take-up rates under universal versus targeted transfer designs (e.g. comparing universal child benefits to means-tested equivalents in the same country over time).',
    'If well-designed targeting can achieve comparable dignity and take-up outcomes without the stigma this reading identifies, the coordination-function case for universality over targeting weakens, and the freedom_floor_reading''s zero-victims claim (attributed specifically to universality) becomes less load-bearing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universality_vs_targeting_tradeoff, conceptual, 'Whether universality is structurally required for this reading''s stigma-elimination claim, or whether targeting can achieve equivalent results.').

omega_variable(
    employer_power_baseline_legitimacy,
    'Is the pre-floor wage-setting leverage employers hold over workers with weak exit options a legitimate market outcome (making the floor''s constraint on that leverage a cost imposed on employers) or an illegitimate extraction this reading is correcting (making the floor''s constraint a removal of extraction rather than an imposition of it)?',
    'This is fundamentally a values question about labor market baselines, though it can be partially informed by monopsony research measuring the gap between marginal productivity and wages in low-wage labor markets.',
    'Determines whether low_wage_employers are correctly authored as an excluded party whose objection doesn''t count as victimhood (this reading''s position) or whether they should instead be authored with a payer role reflecting genuine cost imposition (closer to the dependency_trap_reading or a hybrid tangled_rope framing).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(employer_power_baseline_legitimacy, preference, 'Whether pre-floor employer wage-setting power is a legitimate baseline or an extraction this reading corrects.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__freedom_floor_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__freedom_floor_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(inco_tr_t4, income_support_commitment__freedom_floor_reading, theater_ratio, 4, 0.16).
narrative_ontology:measurement(inco_tr_t8, income_support_commitment__freedom_floor_reading, theater_ratio, 8, 0.13).
narrative_ontology:measurement(inco_tr_t12, income_support_commitment__freedom_floor_reading, theater_ratio, 12, 0.11).
narrative_ontology:measurement(inco_tr_t16, income_support_commitment__freedom_floor_reading, theater_ratio, 16, 0.1).
narrative_ontology:measurement(inco_tr_t20, income_support_commitment__freedom_floor_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(inco_tr_t24, income_support_commitment__freedom_floor_reading, theater_ratio, 24, 0.1).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__freedom_floor_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(inco_be_t4, income_support_commitment__freedom_floor_reading, base_extractiveness, 4, 0.2).
narrative_ontology:measurement(inco_be_t8, income_support_commitment__freedom_floor_reading, base_extractiveness, 8, 0.19).
narrative_ontology:measurement(inco_be_t12, income_support_commitment__freedom_floor_reading, base_extractiveness, 12, 0.18).
narrative_ontology:measurement(inco_be_t16, income_support_commitment__freedom_floor_reading, base_extractiveness, 16, 0.18).
narrative_ontology:measurement(inco_be_t20, income_support_commitment__freedom_floor_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement(inco_be_t24, income_support_commitment__freedom_floor_reading, base_extractiveness, 24, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(income_support_commitment__freedom_floor_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__freedom_floor_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_commitment__freedom_floor_reading, 0.12).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, income_support_commitment__dependency_trap_reading).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, income_support_commitment__targeting_efficiency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the income_support_commitment kernel. freedom_floor_reading (this file) authors low ε (0.18) and claimed_type rope, with zero victims because universality is held to eliminate means-test stigma as a victim-producing mechanism. dependency_trap_reading is expected to author higher suppression-of-initiative framing and likely a tangled_rope or snare classification with a distinct beneficiary/victim structure (state administrators as agenda-setters, recipients as victims of atrophied agency). targeting_efficiency_reading is expected to name 'non-needy universal recipients' or 'the fiscal commons' as a victim class diverting resources from need-tested claimants, likely producing a moderate-ε tangled_rope framing around allocation efficiency. All three share the same underlying policy instrument (unconditional or near-unconditional cash transfer) but are structurally distinct constraints per the ε-invariance principle, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
