% ============================================================================
% CONSTRAINT STORY: market_naturalization__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_naturalization__hybrid_reading, []).

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
 *   constraint_id: market_naturalization__hybrid_reading
 *   human_readable: Market Dominance as Hybrid: Lapsed Coordination Plus Active Maintenance
 *   domain: political_economy/institutional_analysis
 *
 * SUMMARY:
 *   This story is the hybrid reading of the market_naturalization kernel: the
 *   claim that a dominant firm's position is neither a pure lapsed settlement
 *   (as the lapsed_alternative_reading holds) nor purely a live extraction
 *   requiring continuous active defense (as the
 *   beneficiary_maintained_reading holds), but a composite of both. Some of
 *   the barriers facing entrants and small competitors are genuinely settled
 *   sediment from an earlier, real coordination problem; others are freshly
 *   and continuously renewed through lobbying, exclusivity contracts, and
 *   predatory responses to entry threats. The rising extractiveness and
 *   suppression-requirement trajectories over the measured interval reflect
 *   the active-maintenance component accumulating on top of a base level the
 *   lapsed component alone would not explain — the metrics describe a MIX,
 *   not a pure type, which is exactly what a tangled_rope classification with
 *   moderate ε is built to register.
 *
 * KEY AGENTS:
 *   - incumbent_platform_operators: primary agenda-setter and beneficiary, blends inherited advantage with active defense
 *   - scale_advantaged_incumbents: secondary beneficiaries free-riding on maintained barriers
 *   - prospective_market_entrants: bear costs from both lapsed and maintained barriers, cannot distinguish which is which from outside
 *   - small_scale_competitors: powerless payers squeezed by both inertial and active mechanisms
 *   - downstream_consumers: mixed beneficiary/payer, capture some efficiency gains and pay some suppressed-competition markup
 *   - regulatory_agencies: analytical observers who can only prosecute the maintained component
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__hybrid_reading, 0.52).
domain_priors:suppression_score(market_naturalization__hybrid_reading, 0.48).
domain_priors:theater_ratio(market_naturalization__hybrid_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(market_naturalization__hybrid_reading, "Market Dominance as Hybrid: Lapsed Coordination Plus Active Maintenance").
narrative_ontology:topic_domain(market_naturalization__hybrid_reading, "political_economy/institutional_analysis").

domain_priors:requires_active_enforcement(market_naturalization__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__hybrid_reading, 'b8a3dbb5-4ba0-44ee-8e4f-6c6077c1099e').
narrative_ontology:cs_kernel_codification('b8a3dbb5-4ba0-44ee-8e4f-6c6077c1099e', distributed).
narrative_ontology:cs_authority_grounding('b8a3dbb5-4ba0-44ee-8e4f-6c6077c1099e', distributed).
narrative_ontology:cs_reading_relation('b8a3dbb5-4ba0-44ee-8e4f-6c6077c1099e', market_naturalization__lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_reading_relation('b8a3dbb5-4ba0-44ee-8e4f-6c6077c1099e', market_naturalization__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_axiom('b8a3dbb5-4ba0-44ee-8e4f-6c6077c1099e', foundational, dominance_is_structurally_composite).
narrative_ontology:cs_axiom_status(dominance_is_structurally_composite, holdable).
narrative_ontology:cs_axiom_grounding('b8a3dbb5-4ba0-44ee-8e4f-6c6077c1099e', dominance_is_structurally_composite, empirically_contingent).
narrative_ontology:cs_axiom('b8a3dbb5-4ba0-44ee-8e4f-6c6077c1099e', secondary, proportion_of_lapsed_to_maintained_is_empirically_undetermined).
narrative_ontology:cs_axiom_status(proportion_of_lapsed_to_maintained_is_empirically_undetermined, holdable).
narrative_ontology:cs_axiom_grounding('b8a3dbb5-4ba0-44ee-8e4f-6c6077c1099e', proportion_of_lapsed_to_maintained_is_empirically_undetermined, empirically_contingent).
narrative_ontology:cs_reference_frame('b8a3dbb5-4ba0-44ee-8e4f-6c6077c1099e', mixed_settlement_with_partial_active_defense).
narrative_ontology:cs_drift_state('b8a3dbb5-4ba0-44ee-8e4f-6c6077c1099e', contemporary_antitrust_scrutiny_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b8a3dbb5-4ba0-44ee-8e4f-6c6077c1099e', '').
narrative_ontology:cs_kernel_id(market_naturalization__hybrid_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, incumbent_platform_operators).
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, scale_advantaged_incumbents).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, prospective_market_entrants).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, small_scale_competitors).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, downstream_consumers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, downstream_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Occupies a dominant position that originally arose from real first-mover coordination advantages (network effects, standard-setting, early capital formation) but now actively maintains that position through lobbying for favorable regulation, strategic acquisition of potential rivals, and contractual exclusivity terms. Some of what protects its position no longer requires active defense (switching costs have calcified into user habit); other parts require continuous legal and political investment to hold.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, incumbent_platform_operators, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(market_naturalization__hybrid_reading, incumbent_platform_operators, beneficiary).

% Secondary firms that cluster around the dominant operator's ecosystem and benefit from the barriers it maintains without having to build or defend them directly. They free-ride on the incumbent's lobbying and legal spend while capturing adjacent rents.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, scale_advantaged_incumbents, beneficiary,
    powerful, generational, mobile, national).

% Would-be competitors who face a combination of genuinely settled barriers (accumulated user data, mature supply chains they cannot replicate quickly) and actively enforced ones (non-compete clauses imposed on suppliers, predatory pricing in response to entry attempts). They cannot tell from outside which barriers are natural sediment and which are freshly poured concrete, which makes contesting any of them expensive and uncertain.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, prospective_market_entrants, payer,
    moderate, biographical, constrained, national).

% Already operating at small scale within the dominant firm's shadow, dependent on its platform or supply terms for market access. Bear ongoing costs from both inertial disadvantage (they never accumulated the scale economies) and active squeezes (fee increases, algorithmic deprioritization) that the incumbent applies when it perceives a competitive threat.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, small_scale_competitors, payer,
    powerless, immediate, trapped, local).

% Benefit from some genuine efficiencies of scale (lower unit costs, standardized quality) while also paying a markup sustained by suppressed competition. Their bargaining position is diffuse — no single consumer bears enough cost to organize resistance, though aggregate consumer surplus lost is substantial.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, downstream_consumers, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(market_naturalization__hybrid_reading, downstream_consumers, beneficiary).

% Investigate whether the incumbent's position reflects settled efficiency gains, active anticompetitive conduct, or some mixture. Their enforcement actions target the maintained portions of dominance while typically leaving lapsed structural advantages (which are harder to prosecute) untouched.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, regulatory_agencies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_naturalization__hybrid_reading, incumbent_platform_operators).
narrative_ontology:fixing_cost_class(market_naturalization__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardization around a dominant platform or firm genuinely reduced transaction costs during the market's formative period — common interfaces, predictable supply relationships, and shared technical standards solved real coordination problems that fragmented competition would not have solved as efficiently.
% TRANSFER_FUNCTION: Moves rents from entrants, small competitors, and consumers to the incumbent and its ecosystem partners — partly through prices and terms no longer justified by any live coordination function, and partly through mechanisms (exclusivity, lobbying-shaped regulation) actively renewed each period.
% ABSENT_VOICES: Failed entrants who exited the market are structurally absent from any contemporaneous accounting — their absence is precisely what makes it hard to separate 'the market settled because coordination worked' from 'the market settled because entrants were suppressed and left.'
% DISAPPEARANCE_RATIONALE: If the incumbent's position vanished overnight, the lapsed elements (habituated demand, mature supplier relationships) would partially reconstitute quickly around a new center because they reflect real efficiencies; the actively maintained elements (contractual exclusivity, favorable regulatory carve-outs) would not reconstitute without deliberate re-erection, meaning some of the market would rearrange and some would not — this split-verdict IS the hybrid reading's central claim.
% FOUNDING_PROBLEM: Early market fragmentation created duplicated infrastructure, incompatible standards, and high transaction costs; consolidation around a dominant actor solved a genuine coordination problem in the market's formative period.
% FOUNDING_PROBLEM_CORROBORATION: Independent economic historians and antitrust economists (e.g., testimony in competition proceedings) attest that the original coordination problem was real and substantially resolved decades ago; the incumbent and its ecosystem partners attest the coordination function remains live and justifies current terms. No neutral party disputes that BOTH lapsed and actively-maintained elements coexist — the dispute is over the current proportion of each, which is exactly what regulatory discovery has been unable to settle conclusively.
narrative_ontology:disappearance_verdict(market_naturalization__hybrid_reading, contested).
narrative_ontology:founding_problem_status(market_naturalization__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(market_naturalization__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__hybrid_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_naturalization__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_naturalization__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(market_naturalization__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate 0.52 (rather than the low end appropriate to a purely lapsed closure, or the high end appropriate to a purely maintained snare) because the hybrid reading holds that only part of the dominant position's advantage is live extraction — the rest is genuine settled efficiency that would persist even without enforcement. Suppression at 0.48 tracks the same logic: real coercive mechanisms exist (exclusivity, predatory pricing, regulatory capture) but they layer on top of, rather than fully explain, the dominant position. Theater ratio at 0.4 reflects that a meaningful share of the incumbent's compliance and 'innovation' activity is now performative maintenance of a position that no longer requires the coordination story it is dressed in.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent operators sit near the full-beneficiary end: they collect rents from both the lapsed and maintained components without needing to actively defend the lapsed portion. Prospective entrants and small competitors sit near the full-target end because their exit options are constrained or trapped and they cannot separate which barriers are genuinely settled from which are actively renewed — this uncertainty itself has a suppressive effect, since contesting the wrong kind of barrier wastes scarce resources. Consumers occupy a genuinely mixed position, which is why they carry both beneficiary and payer roles.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading exists specifically to prevent two mislabeling errors: treating the entire dominant position as pure natural settlement (which would excuse the actively maintained extraction as inevitable) and treating it as pure active extraction (which would imply that removing enforcement alone would restore competition, when much of the barrier is structural sediment that would persist regardless). Both errors have real policy costs — the first under-regulates, the second over-promises what antitrust remedies alone can achieve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lapsed_vs_maintained_proportion,
    'What proportion of the incumbent''s current market position is attributable to lapsed structural sediment (habituated demand, mature supply relationships) versus actively renewed mechanisms (exclusivity contracts, lobbying-shaped regulation, predatory pricing)?',
    'Natural experiment: withdraw specific active-maintenance mechanisms (e.g., via antitrust consent decree) one at a time and observe whether the position erodes or holds. If it holds after all identified active mechanisms are removed, the lapsed component was larger than assumed; if it erodes rapidly, the position was more actively maintained than the hybrid reading credits.',
    'If overwhelmingly lapsed, this story should be superseded by the lapsed_alternative_reading and reclassified toward rope/piton; if overwhelmingly maintained, it should be superseded by the beneficiary_maintained_reading and reclassified toward snare. The hybrid classification is only warranted while the proportion is genuinely mixed and empirically unresolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lapsed_vs_maintained_proportion, empirical, 'The unresolved split between settled and actively defended market barriers.').

omega_variable(
    which_reading_the_regulator_should_act_on,
    'Should competition policy treat ambiguous cases (where it is unclear whether a barrier is lapsed or maintained) as if they were maintained (erring toward intervention) or as if they were lapsed (erring toward restraint)?',
    'This is a policy-preference question, not resolvable by further data alone — it depends on the relative social cost assigned to false-positive intervention (breaking up genuinely efficient consolidation) versus false-negative restraint (permitting entrenched extraction to continue).',
    'Different answers lead to substantially different regulatory postures even holding the empirical proportion (omega above) constant, because the two errors have asymmetric costs across time horizons.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_reading_the_regulator_should_act_on, preference, 'Policy stance under genuine mixed-type uncertainty.').

omega_variable(
    hybrid_framing_alternative,
    'Is the hybrid reading itself the correct unit of analysis, or should the dominant position be decomposed into separate constraints per barrier-mechanism (one story per lapsed barrier, one per actively maintained barrier) rather than blended into a single moderate-ε story?',
    'Per the ε-invariance principle, if individual barrier mechanisms have widely divergent ε values (a fully lapsed switching-cost barrier near ε≈0.1 alongside a fully maintained exclusivity-contract barrier near ε≈0.8), those are arguably separate constraints artificially averaged here. The hybrid_reading choice to blend them reflects a judgment that market dominance is experienced by entrants as a single undifferentiated barrier, not as separable mechanisms.',
    'If decomposed, this story would be replaced by several narrower, cleaner stories with much clearer type assignments; the hybrid framing would then only be useful as an aggregate summary, not a classification-bearing constraint in its own right.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_framing_alternative, conceptual, 'Whether blending lapsed and maintained mechanisms into one story is the right level of decomposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_naturalization__hybrid_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(mark_tr_t8, market_naturalization__hybrid_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(mark_tr_t16, market_naturalization__hybrid_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement(mark_tr_t24, market_naturalization__hybrid_reading, theater_ratio, 24, 0.32).
narrative_ontology:measurement(mark_tr_t32, market_naturalization__hybrid_reading, theater_ratio, 32, 0.37).
narrative_ontology:measurement(mark_tr_t40, market_naturalization__hybrid_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_naturalization__hybrid_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(mark_be_t8, market_naturalization__hybrid_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(mark_be_t16, market_naturalization__hybrid_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement(mark_be_t24, market_naturalization__hybrid_reading, base_extractiveness, 24, 0.47).
narrative_ontology:measurement(mark_be_t32, market_naturalization__hybrid_reading, base_extractiveness, 32, 0.5).
narrative_ontology:measurement(mark_be_t40, market_naturalization__hybrid_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_naturalization__hybrid_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(mark_su_t8, market_naturalization__hybrid_reading, suppression_requirement, 8, 0.3).
narrative_ontology:measurement(mark_su_t16, market_naturalization__hybrid_reading, suppression_requirement, 16, 0.35).
narrative_ontology:measurement(mark_su_t24, market_naturalization__hybrid_reading, suppression_requirement, 24, 0.4).
narrative_ontology:measurement(mark_su_t32, market_naturalization__hybrid_reading, suppression_requirement, 32, 0.44).
narrative_ontology:measurement(mark_su_t40, market_naturalization__hybrid_reading, suppression_requirement, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__hybrid_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_naturalization__hybrid_reading, 0.15).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, beneficiary_maintained_reading).

% DUAL FORMULATION NOTE:
% This constraint is the hybrid member of a three-reading family sharing the market_naturalization kernel. lapsed_alternative_reading claims the dominant position requires no active maintenance (fully settled); beneficiary_maintained_reading claims the position is actively defended throughout (no lapsed component); this hybrid_reading holds both are partially true simultaneously. Each reading authors its own ε against the same standing arrangement (the incumbent's market position) but reaches a different value: lower for the lapsed reading, higher for the beneficiary-maintained reading, moderate here. All three are linked so that contamination or purity analysis on one propagates as diagnostic signal to the others without collapsing them into a single averaged constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
