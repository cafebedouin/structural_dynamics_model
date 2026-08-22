% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__freedom_floor_reading, []).

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
 *   constraint_id: unconditional_income_support__freedom_floor_reading
 *   human_readable: Unconditional Income Support as Autonomy-Enabling Floor
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint authors ONE reading of the contested
 *   unconditional-income-support kernel: the freedom-floor reading, which
 *   holds that removing income-conditionality from labor participation
 *   eliminates a coercive lever without creating meaningfully new extraction.
 *   Empirical grounding is drawn from the Alaska Permanent Fund dividend and
 *   Kenya GiveDirectly trials, cited within this reading as evidence that
 *   labor supply effects are minimal and autonomy effects are real. This
 *   reading claims no victims — the taxpayer flow is modeled as a mutual
 *   insurance premium rather than an extraction, and the low-wage employer's
 *   lost bargaining leverage is not treated as a cost the floor imposes on a
 *   legitimate interest. Two sibling readings of the same kernel —
 *   dependency_trap_reading (incentive-distortion framing) and
 *   universality_paradox_reading (cross-ideological Trojan-horse framing) —
 *   are separate constraint files with their own ε and stakeholder
 *   structures; they are not blended into this one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__freedom_floor_reading, 0.28).
domain_priors:suppression_score(unconditional_income_support__freedom_floor_reading, 0.12).
domain_priors:theater_ratio(unconditional_income_support__freedom_floor_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__freedom_floor_reading, rope).
narrative_ontology:human_readable(unconditional_income_support__freedom_floor_reading, "Unconditional Income Support as Autonomy-Enabling Floor").
narrative_ontology:topic_domain(unconditional_income_support__freedom_floor_reading, "political_economy/social_policy/welfare_state_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__freedom_floor_reading, '5e43558a-1662-4688-b3dd-866959a42cde').
narrative_ontology:cs_kernel_codification('5e43558a-1662-4688-b3dd-866959a42cde', distributed).
narrative_ontology:cs_authority_grounding('5e43558a-1662-4688-b3dd-866959a42cde', distributed).
narrative_ontology:cs_reading_relation('5e43558a-1662-4688-b3dd-866959a42cde', unconditional_income_support__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('5e43558a-1662-4688-b3dd-866959a42cde', unconditional_income_support__universality_paradox_reading, influences).
narrative_ontology:cs_axiom('5e43558a-1662-4688-b3dd-866959a42cde', foundational, desperation_leverage_is_illegitimate_bargaining_advantage).
narrative_ontology:cs_axiom_status(desperation_leverage_is_illegitimate_bargaining_advantage, holdable).
narrative_ontology:cs_axiom_grounding('5e43558a-1662-4688-b3dd-866959a42cde', desperation_leverage_is_illegitimate_bargaining_advantage, deontological).
narrative_ontology:cs_axiom('5e43558a-1662-4688-b3dd-866959a42cde', foundational, minimal_labor_supply_effect_is_empirically_established).
narrative_ontology:cs_axiom_status(minimal_labor_supply_effect_is_empirically_established, holdable).
narrative_ontology:cs_axiom_grounding('5e43558a-1662-4688-b3dd-866959a42cde', minimal_labor_supply_effect_is_empirically_established, empirically_contingent).
narrative_ontology:cs_reference_frame('5e43558a-1662-4688-b3dd-866959a42cde', conditional_welfare_stigma_regime).
narrative_ontology:cs_drift_state('5e43558a-1662-4688-b3dd-866959a42cde', post_pilot_evidence_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('5e43558a-1662-4688-b3dd-866959a42cde', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__freedom_floor_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, unpaid_caregivers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, working_artists).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, domestic_abuse_survivors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, general_taxpayers).
narrative_ontology:constraint_victim(unconditional_income_support__freedom_floor_reading, general_taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Currently take whatever work is offered because refusal means no income at all. An unconditional floor lets them decline exploitative hours, wait for better matches, or retrain, without risking destitution. The floor is received regardless of employment status, converting a coercive labor relation into a genuinely voluntary one.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, precarious_workers, beneficiary,
    powerless, biographical, constrained, national).

% Perform childcare and eldercare work with no wage and no independent income, often making them financially dependent on a partner or family member. The floor provides an income stream tied to personhood rather than employment, giving them resources and an exit option independent of the household's primary earner.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, unpaid_caregivers, beneficiary,
    powerless, biographical, constrained, national).

% Produce work with uncertain and irregular market returns; many take unrelated survival jobs that consume the time needed to develop their craft. The floor functions as a baseline that decouples subsistence from immediate market validation of creative labor.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, working_artists, beneficiary,
    moderate, biographical, constrained, national).

% Financial dependency on an abuser is frequently the specific mechanism that prevents leaving. An unconditional, individually-paid floor is not contingent on the household or on the abuser's cooperation, and provides a resource base for exit that does not exist under means-tested household-unit benefits.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, domestic_abuse_survivors, beneficiary,
    powerless, biographical, trapped, national).

% Fund the floor through general taxation. Under this reading they are not victims of extraction but co-beneficiaries of a Pareto-improving arrangement: they gain the option value of the floor for themselves, reduced administrative and enforcement overhead relative to means-tested programs, and macroeconomic stabilization during downturns. Their payment is framed here as buying a social insurance product they themselves hold a claim on, not as a transfer extracted from them for others' benefit.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, general_taxpayers, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__freedom_floor_reading, general_taxpayers, beneficiary).

% Design and disburse the floor. Under this reading their role shrinks relative to means-tested systems: eligibility verification, means-testing bureaucracy, and stigma-laden case management are largely eliminated, since the floor is paid to all on a simple, verifiable basis (citizenship or residency).
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, welfare_administering_agencies, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__freedom_floor_reading, welfare_administering_agencies, agenda_setter).

% Currently benefit from a labor pool with limited bargaining power because refusal of low-wage work is not economically survivable. This reading does not treat them as victims of the floor — their objection that the floor raises their labor costs is noted as a foreseeable consequence of removing coercion, not as an extraction the floor commits against them, since no one is entitled to labor secured through desperation.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, low_wage_employers, excluded,
    powerful, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unconditional_income_support__freedom_floor_reading, diffuse).
narrative_ontology:fixing_cost_class(unconditional_income_support__freedom_floor_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a income baseline, unconditional on employment or household status, that removes the threat of destitution as a lever compelling participation in any specific labor arrangement, and pools the risk of market shocks (job loss, health crises, caregiving demands) across the whole population rather than leaving it to be borne individually.
% TRANSFER_FUNCTION: Moves resources from general taxation to every resident on an equal per-capita basis. Under this reading the flow is not asymmetric extraction: recipients who are also taxpayers receive back some or all of what they contribute, and the net redistribution runs toward those whose market income is currently suppressed by coercive bargaining position, financed by the tax capacity of the whole economy including its most productive participants.
% ABSENT_VOICES: Low-wage employers whose labor costs would likely rise as the coercive floor beneath wage bargaining is removed are not treated as objecting stakeholders in this reading, on the view that a bargaining advantage built on desperation is not a legitimate interest to be protected. Fiscal conservatives who dispute the affordability premise sit largely outside this reading's frame; their concerns are addressed in the sibling dependency_trap_reading, not here.
% DISAPPEARANCE_RATIONALE: If the floor disappeared, precarious workers would lose their exit option and be forced back into accepting any available work regardless of terms; caregivers and abuse survivors would lose independent income and, for some, the resource base that made leaving possible; artists would need to reallocate time to survival wages. Labor bargaining power at the low end would compress and market-shock exposure would revert to being borne individually.
% FOUNDING_PROBLEM: Market economies leave individuals exposed to involuntary participation in exploitative or unsafe arrangements (labor, domestic) because refusal is not survivable without independent income, and existing means-tested welfare systems attach stigma and administrative friction that this reading holds are themselves coercive and inefficient.
% FOUNDING_PROBLEM_CORROBORATION: Independent evaluations of the Alaska Permanent Fund dividend and the Kenya GiveDirectly unconditional transfer trials — conducted by economists and public health researchers outside the advocacy organizations promoting basic income — report labor supply effects near zero and measurable gains in reported autonomy and reduced coercive-relationship persistence, corroborating that the founding problem (coercive dependency) is real and that this reading's mechanism addresses it rather than merely re-labeling continued dependency.
narrative_ontology:disappearance_verdict(unconditional_income_support__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__freedom_floor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unconditional_income_support__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__freedom_floor_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__freedom_floor_reading_tests).
:- end_tests(unconditional_income_support__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate-low (0.28) and rising only slightly over the interval, reflecting this reading's empirical premise that labor-supply and fiscal-crowding effects are small but not literally zero as scale grows. Suppression is low (0.12): the floor is not enforced against anyone in the coercive sense — no one is compelled to accept it or barred from working more. Accessibility collapse is low (0.2) because market participation, additional work, and alternative income strategies all remain fully available after the floor is introduced; nothing collapses, options expand. Resistance is moderate (0.35), sourced from employer-side and fiscal-conservative political opposition, not from the beneficiary population.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setting welfare agency and the taxpayer seat should compute close to symmetric or mildly beneficiary-leaning, while precarious-worker and abuse-survivor seats should compute strongly beneficiary-leaning given trapped/constrained exit combined with beneficiary declaration. The excluded low-wage-employer seat is the one position from which this reading's coordination story would look like a cost — that seat is named but deliberately not treated as a victim within this reading's own frame, which is the structural feature the sibling dependency_trap_reading exists to contest.
 *
 * DIRECTIONALITY LOGIC:
 *   Precarious workers, caregivers, artists, and abuse survivors are declared beneficiaries because the floor structurally increases their exit options without attaching conditions; the derivation should place them near the beneficiary end of directionality. Taxpayers carry a dual role (payer + beneficiary) because this reading treats the fiscal contribution as a purchase of shared insurance value, not an asymmetric transfer — hence no victims are declared anywhere in this reading's base_properties, consistent with the Pareto-improvement premise of the freedom-floor account.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's founding problem (coercive dependency on the labor market and stigmatizing means-testing) is authored as live, corroborated by independent trial evidence rather than only advocacy sources — this blocks a mandatrophy read in which the floor persists as inertial policy after its problem resolved. Should future data show labor supply effects are not negligible at full scale, that would move the constraint toward the dependency_trap_reading rather than falsifying this reading's data-invariant claim; the two readings remain separate constraints regardless.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    labor_supply_effect_at_scale,
    'Do the near-zero labor supply effects observed in Alaska (partial dividend, resource-windfall funded) and Kenya (bounded trial, time-limited) generalize to a permanent, full-population, tax-funded unconditional floor at national scale?',
    'Long-run natural experiments or large-scale pilots (e.g., a full national rollout with pre/post labor force participation tracking) sustained over multiple business cycles.',
    'If labor supply effects remain minimal at scale, this reading''s moderate ε and rope classification are supported. If effects turn out substantial, the constraint''s actual operation would resemble the dependency_trap_reading far more than this reading claims, though as a matter of the ε-invariance principle that would indicate the freedom_floor_reading''s own ε was measured incorrectly, not that the two readings merge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_supply_effect_at_scale, empirical, 'Whether small-scale pilot labor-supply findings generalize to permanent national-scale implementation.').

omega_variable(
    taxpayer_payer_vs_beneficiary_balance,
    'Is the taxpayer population, in aggregate and over a full life-cycle, a net beneficiary of the floor''s insurance value, or a net payer subsidizing others'' autonomy?',
    'Lifecycle incidence analysis: tracking net fiscal position (floor received minus floor-attributable taxes paid) across income deciles and across life stages (unemployment spells, caregiving periods, retirement) for the same cohort.',
    'If most taxpayers are net payers over their lifetime with no realized insurance payout, the ''no victims'' premise of this reading weakens and some taxpayer subpopulations would be better modeled with payer-only role and higher derived directionality, pushing the constraint toward tangled_rope. If most taxpayers do draw on the floor at some life stage, the mutual-insurance framing holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taxpayer_payer_vs_beneficiary_balance, empirical, 'Whether taxpayers are genuinely dual beneficiary/payer or predominantly payers under this reading''s own accounting.').

omega_variable(
    kernel_framing_underdetermination,
    'Is ''unconditional income support'' better modeled as three genuinely distinct constraints (this decomposition) or as one constraint with contested but resolvable ε, where the disagreement across readings is empirical rather than structural?',
    'Track whether the three readings'' authored ε values converge as more implementation data accumulates (e.g., multiple national pilots with different design choices) — convergence would suggest one constraint with resolvable uncertainty; persistent divergence tied to differing normative premises (what counts as an extracted cost, who counts as a legitimate stakeholder) would confirm the family-of-constraints framing is correct.',
    'Confirms or revises whether decomposing into a three-story constraint family (rather than one constraint with an observable-dependent ε) was the right authoring choice under the ε-invariance principle.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel genuinely decomposes into structurally distinct constraints or is one constraint with contested empirical ε.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__freedom_floor_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__freedom_floor_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(unco_tr_t4, unconditional_income_support__freedom_floor_reading, theater_ratio, 4, 0.11).
narrative_ontology:measurement(unco_tr_t8, unconditional_income_support__freedom_floor_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(unco_tr_t12, unconditional_income_support__freedom_floor_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement(unco_tr_t16, unconditional_income_support__freedom_floor_reading, theater_ratio, 16, 0.14).
narrative_ontology:measurement(unco_tr_t20, unconditional_income_support__freedom_floor_reading, theater_ratio, 20, 0.15).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__freedom_floor_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(unco_be_t4, unconditional_income_support__freedom_floor_reading, base_extractiveness, 4, 0.24).
narrative_ontology:measurement(unco_be_t8, unconditional_income_support__freedom_floor_reading, base_extractiveness, 8, 0.25).
narrative_ontology:measurement(unco_be_t12, unconditional_income_support__freedom_floor_reading, base_extractiveness, 12, 0.26).
narrative_ontology:measurement(unco_be_t16, unconditional_income_support__freedom_floor_reading, base_extractiveness, 16, 0.27).
narrative_ontology:measurement(unco_be_t20, unconditional_income_support__freedom_floor_reading, base_extractiveness, 20, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(unconditional_income_support__freedom_floor_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__freedom_floor_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(unconditional_income_support__freedom_floor_reading, 0.1).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, unconditional_income_support__dependency_trap_reading).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, unconditional_income_support__universality_paradox_reading).

% DUAL FORMULATION NOTE:
% This is one of three sibling constraint stories decomposing the natural-language concept 'unconditional income support' per the ε-invariance principle. freedom_floor_reading (this file) authors ε as moderate-low (0.28) with no declared victims, claimed type rope. dependency_trap_reading is expected to author a substantially higher ε with declared victims (targeted-aid recipients displaced, non-needy upper-income recipients as beneficiaries), likely tangled_rope or snare. universality_paradox_reading is expected to track implementation-convergence dynamics across ideologically opposed coalitions rather than autonomy or dependency, with its own distinct ε and stakeholder set. All three share the same underlying policy kernel but are authored as separate constraints with independent ε, beneficiaries, victims, and classification, linked here via affects_constraints rather than merged into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
