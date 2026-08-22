% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__universality_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__universality_paradox_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: unconditional_income_support__universality_paradox_reading
 *   human_readable: Universal Basic Income as Cross-Ideological Ambiguity Vehicle
 *   domain: political economy / social policy / welfare state theory
 *
 * SUMMARY:
 *   This story reads the kernel of unconditional income support not through
 *   its endorsed normative content (freedom-enhancing floor, or
 *   incentive-distorting subsidy) but through the structural function of its
 *   own ambiguity. The claim here is that the political viability of
 *   income-support reform in polarized legislatures depends on the proposal
 *   being simultaneously legible as several incompatible things — and that
 *   this ambiguity is a real, load-bearing coordination device, not merely
 *   confusion. It genuinely lets a market-liberal, a social-democrat, and a
 *   deficit hawk each vote for the same bill while telling their constituents
 *   different stories. But the same mechanism that builds the coalition also
 *   entangles incompatible normative commitments inside one policy vehicle,
 *   and the taxing-back research literature shows the fiscal/distributional
 *   outcomes across the differently-branded designs converge more than the
 *   rhetoric admits. That convergence is what keeps ε low here: this reading
 *   is not claiming large redistribution or large incentive distortion (those
 *   are claims of the sibling readings), it is claiming that the vehicle's
 *   ambiguity itself extracts — from ideological clarity, and from recipients
 *   of existing targeted programs who get traded away in the
 *   coalition-building.
 *
 * KEY AGENTS:
 *   - political_entrepreneurs: Primary beneficiary (organized/arbitrage) — builds coalitions by sustaining incompatible framings
 *   - policy_designers: Primary beneficiary/agenda_setter (institutional/mobile) — controls the taxing-back mechanism that permits multiple readings
 *   - targeted_program_recipients: Primary target (powerless/trapped) — existing benefits placed at risk by the 'redundancy' argument
 *   - public_deliberative_capacity: Diffuse non-agent casualty — coherent evaluation of the policy is degraded
 *   - fiscal_analysts: Analytical observer — documents the convergence the rhetoric obscures
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__universality_paradox_reading, 0.28).
domain_priors:suppression_score(unconditional_income_support__universality_paradox_reading, 0.4).
domain_priors:theater_ratio(unconditional_income_support__universality_paradox_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__universality_paradox_reading, tangled_rope).
narrative_ontology:human_readable(unconditional_income_support__universality_paradox_reading, "Universal Basic Income as Cross-Ideological Ambiguity Vehicle").
narrative_ontology:topic_domain(unconditional_income_support__universality_paradox_reading, "political economy / social policy / welfare state theory").

domain_priors:requires_active_enforcement(unconditional_income_support__universality_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__universality_paradox_reading, '69187677-a5a2-48f7-bf66-28417646e2fc').
narrative_ontology:cs_kernel_codification('69187677-a5a2-48f7-bf66-28417646e2fc', distributed).
narrative_ontology:cs_authority_grounding('69187677-a5a2-48f7-bf66-28417646e2fc', distributed).
narrative_ontology:cs_reading_relation('69187677-a5a2-48f7-bf66-28417646e2fc', unconditional_income_support__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('69187677-a5a2-48f7-bf66-28417646e2fc', unconditional_income_support__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('69187677-a5a2-48f7-bf66-28417646e2fc', foundational, political_ambiguity_is_load_bearing_structure).
narrative_ontology:cs_axiom_status(political_ambiguity_is_load_bearing_structure, holdable).
narrative_ontology:cs_axiom_grounding('69187677-a5a2-48f7-bf66-28417646e2fc', political_ambiguity_is_load_bearing_structure, empirically_contingent).
narrative_ontology:cs_axiom('69187677-a5a2-48f7-bf66-28417646e2fc', secondary, fiscal_convergence_across_designs_undercuts_normative_stakes).
narrative_ontology:cs_axiom_status(fiscal_convergence_across_designs_undercuts_normative_stakes, holdable).
narrative_ontology:cs_axiom_grounding('69187677-a5a2-48f7-bf66-28417646e2fc', fiscal_convergence_across_designs_undercuts_normative_stakes, empirically_contingent).
narrative_ontology:cs_reference_frame('69187677-a5a2-48f7-bf66-28417646e2fc', single_coherent_policy_design_assumption).
narrative_ontology:cs_drift_state('69187677-a5a2-48f7-bf66-28417646e2fc', contemporary_coalition_era_reform_attempts, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('69187677-a5a2-48f7-bf66-28417646e2fc', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__universality_paradox_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, political_entrepreneurs).
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, policy_designers).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, targeted_program_recipients).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, public_deliberative_capacity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build cross-ideological coalitions by presenting universal income support to libertarians as a market-compatible cash grant, to social democrats as a dignity-preserving floor, and to fiscal conservatives as an administrative simplification that could replace costlier categorical programs. They collect political capital, donor support, and career advancement from sustaining the ambiguity rather than resolving it — a clean design choice would fracture at least one wing of the coalition.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, political_entrepreneurs, beneficiary,
    organized, biographical, arbitrage, national).

% Draft the taxing-back mechanisms (phase-outs, clawback rates, negative income tax structures) that let the same headline policy be marketed as either universal or targeted depending on audience. Their technical discretion over implementation detail is the mechanism that keeps rival readings simultaneously plausible; they can point to the same marginal tax schedule as proof of either universality or means-testing depending on who is asking.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, policy_designers, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__universality_paradox_reading, policy_designers, agenda_setter).

% Currently rely on categorical programs (disability, housing, childcare subsidies) that get put on the negotiating table as 'redundant' once a universal-sounding replacement is proposed. When the ambiguous vehicle is eventually implemented with a taxing-back structure that functions more like a narrower means-tested benefit, they bear the risk that their existing, better-targeted support is cut in favor of a flatter payment that does not match their actual needs. They have no seat in the coalition-building process and cannot exit the jurisdiction whose legislature decides the design.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, targeted_program_recipients, payer,
    powerless, immediate, trapped, national).

% The public's ability to evaluate the policy on its actual fiscal and distributional merits is degraded because the same proposal is described in mutually exclusive terms by its own advocates. Voters cannot coherently assess a policy whose proponents cannot agree, even amongst themselves, on whether it is a libertarian minimal-state device or a social-democratic entitlement expansion. Listed for completeness as a non-agent casualty of the ambiguity, not a party that can act.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, public_deliberative_capacity, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(unconditional_income_support__universality_paradox_reading, public_deliberative_capacity).

% Encounter the policy through competing framings in media and campaign messaging without access to the underlying design parameters (clawback rate, funding source, interaction with existing programs) that would let them determine which of the incompatible stories is actually being implemented. Their preferences are solicited on the label, not on the fiscal architecture that will actually govern outcomes.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, median_voters, excluded,
    moderate, biographical, constrained, national).

% Model the taxing-back mechanisms across proposed designs and find that despite starkly different rhetorical framing, the distributional and fiscal outcomes converge substantially once effective marginal tax rates and funding mechanisms are held constant. They document the gap between the political narrative and the modeled reality but have no enforcement power over which narrative prevails in legislative debate.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, fiscal_analysts, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unconditional_income_support__universality_paradox_reading, diffuse).
narrative_ontology:fixing_cost_class(unconditional_income_support__universality_paradox_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The ambiguous framing genuinely solves a coalition-formation problem: no single ideological bloc can pass income-support reform alone, so a policy vehicle capable of being read as either a market-liberal minimal-state instrument or a social-democratic entitlement lets otherwise-opposed factions each vote for something they can defend to their own base.
% TRANSFER_FUNCTION: Moves political credibility and coalition-building capacity toward entrepreneurs and designers who can sustain the ambiguity; moves fiscal exposure and program-continuity risk onto recipients of existing targeted programs whose benefits get traded away in the negotiation; moves clarity itself away from the electorate, which votes on incompatible stories rather than a single legible design.
% ABSENT_VOICES: Targeted program recipients whose benefits are implicitly on the table are rarely party to the coalition negotiations between libertarian, social-democratic, and fiscal-conservative sponsors; they discover the actual clawback design only after passage. Median voters lack access to the technical design documents that would let them see through the framing to the actual fiscal architecture.
% DISAPPEARANCE_RATIONALE: If the ambiguity collapsed — if every proposal had to be pre-committed to a single coherent normative frame (pure universal floor vs. means-tested replacement vs. incentive-preserving supplement) — the cross-ideological coalitions that currently pass income-support reform would fracture, since each faction's support depends on being able to tell its own base a different story about the same vehicle. Existing targeted programs would either survive on their own merits (harder to argue for redundancy) or be replaced only by a design that was honestly negotiated on its actual terms.
% FOUNDING_PROBLEM: Legislatures face gridlock passing income-support reform because no single ideological coalition holds a majority; ambiguous framing was adopted, deliberately or emergently, as a device to assemble a passable majority out of factions with incompatible underlying goals.
% FOUNDING_PROBLEM_CORROBORATION: Fiscal analysts and academic public-finance researchers outside the coalition-building process (the taxing-back convergence literature cited in policy design debates) attest that the framing gap persists and that modeled outcomes converge despite divergent rhetoric — this corroboration comes from analysts who neither benefit from nor depend on any particular coalition succeeding.
narrative_ontology:disappearance_verdict(unconditional_income_support__universality_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__universality_paradox_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__universality_paradox_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unconditional_income_support__universality_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__universality_paradox_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__universality_paradox_reading_tests).
:- end_tests(unconditional_income_support__universality_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.28 at interval end) because this reading's claim is specifically about the ambiguity mechanism, not about the redistributive or incentive content the sibling readings dispute — per the taxing-back convergence literature, the actual fiscal outcomes across differently-framed designs are similar, so there is little large-scale material extraction intrinsic to THIS reading's referent. Theater ratio is authored high and rising (0.40 -> 0.62) because an increasing share of the policy's political activity is performative: framing battles, coalition messaging, and rhetorical positioning substitute for engagement with the actual clawback schedule, which is where the real distributional stakes live. Suppression (0.40) reflects that the ambiguity is maintained partly by withholding technical design detail from public debate, not by coercive enforcement — moderate, not severe.
 *
 * PERSPECTIVAL GAP:
 *   Political entrepreneurs and policy designers experience this constraint as a genuine coordination technology — the ambiguity is what lets otherwise-irreconcilable factions cooperate, and from their seat it looks like a rope. Targeted program recipients experience it as a bait-and-switch: the same universalist rhetoric that recruits their sympathy is later used to justify replacing their categorical benefit with a flatter, taxed-back payment that undershoots their actual need. The engine should compute these divergently from the shared structural data — the entrepreneur seat has organized power and arbitrage exit; the recipient seat is powerless and trapped.
 *
 * DIRECTIONALITY LOGIC:
 *   Political entrepreneurs and policy designers sit near the beneficiary end: they collect political capital and administrative discretion from sustaining rather than resolving the ambiguity, and their exit options (arbitrage, mobility across issues/careers) are wide. Targeted program recipients sit near the target end: trapped, powerless, and bearing the risk that their concrete existing benefit is what gets 'simplified away' when the ambiguous vehicle is finally implemented. Public deliberative capacity is authored as a non-agent payer — it is harmed (degraded coherent evaluation) but cannot act, so it is excluded from directionality-driving beneficiary/victim arithmetic proper but named for completeness.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — legislative gridlock preventing income-support reform — remains live (status: live), which distinguishes this from a pure zombie mandate; the coordination function (assembling a cross-ideological majority) is real and ongoing, not merely inertial. What prevents mislabeling this as pure extraction is that the ambiguity does functionally enable passage of policy that would otherwise fail entirely; what prevents mislabeling it as pure coordination is that the same mechanism converts into a lever against existing, better-targeted programs once implementation details are settled behind closed technical doors. The tangled_rope classification holds both: coordination function present AND asymmetric extraction present, tied together in the same vehicle, requiring active maintenance (continuous reframing effort) to keep the coalition from fracturing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ambiguity_as_feature_or_bug,
    'Is the cross-ideological ambiguity a genuine, load-bearing coordination technology that expands what''s politically possible, or is it primarily a cover mechanism that lets designers implement whichever version best serves entrenched interests while collecting support from all camps under false pretenses?',
    'Compare enacted taxing-back schedules against the rhetorical framing used to pass the legislation in multiple jurisdictions that have implemented income-support reform under ambiguous coalitions; check whether targeted-program cuts materialize at rates exceeding what a transparently-negotiated design would have produced.',
    'If ambiguity primarily expands feasible coalitions without disproportionately harming targeted-program recipients, this reading weights toward rope; if it systematically produces cuts to existing targeted support that a clear-eyed debate would not have approved, it weights further toward snare-adjacent tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_as_feature_or_bug, empirical, 'Whether ambiguity is functional political technology or extraction cover.').

omega_variable(
    kernel_reading_disambiguation,
    'Is ''the UBI debate'' actually three structurally distinct constraints (freedom floor, dependency trap, ambiguity vehicle) sharing one policy label, or does the ambiguity itself mean these are not fully separable — i.e., is the ambiguity-reading parasitic on the other two rather than a fourth independent structure?',
    'Track whether jurisdictions that resolve the ambiguity (commit publicly to one coherent design before passage) still exhibit the coordination-and-extraction pattern described here, or whether the tangled_rope structure dissolves once ambiguity is removed — supporting the claim that ambiguity is the necessary and sufficient condition for this constraint''s existence.',
    'If the tangled_rope structure persists even after design clarity is forced, the true extraction driver is something other than ambiguity (e.g., the taxing-back mechanism itself), and this reading would need re-scoping; if it dissolves, this reading''s causal claim about ambiguity-as-mechanism is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, conceptual, 'Whether the ambiguity-reading is causally independent of the freedom-floor and dependency-trap readings or parasitic on their coexistence.').

omega_variable(
    convergence_robustness,
    'Does the taxing-back convergence finding (that differently-branded designs produce similar fiscal/distributional outcomes) hold robustly across national contexts and funding mechanisms, or is it an artifact of a narrow set of modeled designs?',
    'Cross-national comparison of implemented and near-implemented unconditional-income-support designs, examining effective marginal tax rates and net transfer incidence across the political spectrum of enacting coalitions.',
    'If convergence is robust, the low ε authored here is well-grounded and the ambiguity genuinely masks near-identical outcomes; if convergence is fragile or context-dependent, ε may be understated and the ambiguity may mask materially different distributional outcomes depending on which faction''s implementation details prevail.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(convergence_robustness, empirical, 'Robustness of the fiscal/distributional convergence claim across designs and jurisdictions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__universality_paradox_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__universality_paradox_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(unco_tr_t4, unconditional_income_support__universality_paradox_reading, theater_ratio, 4, 0.46).
narrative_ontology:measurement(unco_tr_t8, unconditional_income_support__universality_paradox_reading, theater_ratio, 8, 0.52).
narrative_ontology:measurement(unco_tr_t12, unconditional_income_support__universality_paradox_reading, theater_ratio, 12, 0.56).
narrative_ontology:measurement(unco_tr_t16, unconditional_income_support__universality_paradox_reading, theater_ratio, 16, 0.59).
narrative_ontology:measurement(unco_tr_t20, unconditional_income_support__universality_paradox_reading, theater_ratio, 20, 0.61).
narrative_ontology:measurement(unco_tr_t24, unconditional_income_support__universality_paradox_reading, theater_ratio, 24, 0.62).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__universality_paradox_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(unco_be_t4, unconditional_income_support__universality_paradox_reading, base_extractiveness, 4, 0.2).
narrative_ontology:measurement(unco_be_t8, unconditional_income_support__universality_paradox_reading, base_extractiveness, 8, 0.22).
narrative_ontology:measurement(unco_be_t12, unconditional_income_support__universality_paradox_reading, base_extractiveness, 12, 0.24).
narrative_ontology:measurement(unco_be_t16, unconditional_income_support__universality_paradox_reading, base_extractiveness, 16, 0.26).
narrative_ontology:measurement(unco_be_t20, unconditional_income_support__universality_paradox_reading, base_extractiveness, 20, 0.27).
narrative_ontology:measurement(unco_be_t24, unconditional_income_support__universality_paradox_reading, base_extractiveness, 24, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(unconditional_income_support__universality_paradox_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__universality_paradox_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(unconditional_income_support__universality_paradox_reading, 0.12).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, unconditional_income_support__freedom_floor_reading).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, unconditional_income_support__dependency_trap_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept 'universal basic income politics' per the epsilon-invariance principle. freedom_floor_reading authors a low-extraction, autonomy-enhancing floor with labor-market beneficiaries; dependency_trap_reading authors a higher-extraction incentive-distorting subsidy with taxpayer/labor-market victims; this reading (universality_paradox_reading) authors a low-extraction tangled_rope where the extraction is the ambiguity mechanism itself, victimizing ideological clarity and existing targeted-program recipients rather than the labor market or general taxpayers. All three share the kernel text (the UBI proposal) but are not the same constraint — each has its own epsilon, beneficiaries, and victims, linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
