% ============================================================================
% CONSTRAINT STORY: market_naturalization__beneficiary_maintained_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_naturalization__beneficiary_maintained_reading, []).

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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: market_naturalization__beneficiary_maintained_reading
 *   human_readable: Market Dominance as Actively Maintained Closure (Beneficiary-Maintained Reading)
 *   domain: political_economy/economic_history/institutional_analysis
 *
 * SUMMARY:
 *   This story instantiates the beneficiary_maintained_reading of the
 *   market_naturalization kernel: the claim that a firm's or capital class's
 *   market dominance persists not because the closure has lapsed into a
 *   costless, self-sustaining natural fact, but because incumbent capital
 *   holders continue to actively fund and operate the mechanisms (exclusive
 *   contracts, patent enforcement, lobbying, litigation) that keep entrants
 *   and alternative channels out. Under this reading the standing arrangement
 *   — the dominant position as it currently operates — is read as
 *   substantially extractive and actively enforced, not lapsed. The sibling
 *   readings (lapsed_alternative_reading, hybrid_reading) are separate
 *   constraints with their own ε and structural data; they are not
 *   synthesized here.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__beneficiary_maintained_reading, 0.79).
domain_priors:suppression_score(market_naturalization__beneficiary_maintained_reading, 0.81).
domain_priors:theater_ratio(market_naturalization__beneficiary_maintained_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__beneficiary_maintained_reading, tangled_rope).
narrative_ontology:human_readable(market_naturalization__beneficiary_maintained_reading, "Market Dominance as Actively Maintained Closure (Beneficiary-Maintained Reading)").
narrative_ontology:topic_domain(market_naturalization__beneficiary_maintained_reading, "political_economy/economic_history/institutional_analysis").

domain_priors:requires_active_enforcement(market_naturalization__beneficiary_maintained_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__beneficiary_maintained_reading, '2c058083-f577-44b8-8bd7-39ce2dea2f7f').
narrative_ontology:cs_kernel_codification('2c058083-f577-44b8-8bd7-39ce2dea2f7f', distributed).
narrative_ontology:cs_authority_grounding('2c058083-f577-44b8-8bd7-39ce2dea2f7f', extraction).
narrative_ontology:cs_interpretation_layer_present('2c058083-f577-44b8-8bd7-39ce2dea2f7f').
narrative_ontology:cs_reading_relation('2c058083-f577-44b8-8bd7-39ce2dea2f7f', market_naturalization__lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_reading_relation('2c058083-f577-44b8-8bd7-39ce2dea2f7f', market_naturalization__hybrid_reading, influences).
narrative_ontology:cs_axiom('2c058083-f577-44b8-8bd7-39ce2dea2f7f', foundational, dominance_requires_continuous_funded_defense).
narrative_ontology:cs_axiom_status(dominance_requires_continuous_funded_defense, holdable).
narrative_ontology:cs_axiom_grounding('2c058083-f577-44b8-8bd7-39ce2dea2f7f', dominance_requires_continuous_funded_defense, empirically_contingent).
narrative_ontology:cs_axiom('2c058083-f577-44b8-8bd7-39ce2dea2f7f', secondary, efficiency_origin_has_expired).
narrative_ontology:cs_axiom_status(efficiency_origin_has_expired, holdable).
narrative_ontology:cs_axiom_grounding('2c058083-f577-44b8-8bd7-39ce2dea2f7f', efficiency_origin_has_expired, empirically_contingent).
narrative_ontology:cs_created_at('2c058083-f577-44b8-8bd7-39ce2dea2f7f', '').
narrative_ontology:cs_kernel_id(market_naturalization__beneficiary_maintained_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders).
narrative_ontology:constraint_beneficiary(market_naturalization__beneficiary_maintained_reading, dominant_firm_executives).
narrative_ontology:constraint_beneficiary(market_naturalization__beneficiary_maintained_reading, affiliated_financial_intermediaries).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, prospective_market_entrants).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, downstream_consumers).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, displaced_smallholder_producers).
narrative_ontology:constraint_vindicates(market_naturalization__beneficiary_maintained_reading, market_concentration_reflects_efficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Owns the dominant market position and funds the legal, regulatory, and standard-setting apparatus that keeps it in place — exclusive supplier contracts, patent thickets, lobbying against interoperability mandates, and litigation against entrants. Collects the price premium the closure sustains and can redeploy capital across jurisdictions if any one enforcement front weakens, but has strong incentive to keep every front active rather than exit.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders, beneficiary).

% Manage the day-to-day maintenance of the closure: pricing discipline, exclusive-dealing terms, and the public narrative that the position was won by superior efficiency rather than defended by expenditure. Careers and compensation are tied to sustaining the margin the closure produces.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, dominant_firm_executives, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(market_naturalization__beneficiary_maintained_reading, dominant_firm_executives, agenda_setter).

% Underwrite the incumbent's expansion and litigation costs, take fees on the associated deal flow, and have a direct stake in the closure's durability since their own return depends on the incumbent's continued rent stream.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, affiliated_financial_intermediaries, beneficiary,
    organized, biographical, mobile, national).

% Face exclusive contracts, patent thickets, predatory pricing responses, and lobbying-shaped regulatory barriers when attempting entry. Can attempt entry in adjacent or unregulated niches but are blocked from the core market by mechanisms specifically maintained against them.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, prospective_market_entrants, payer,
    moderate, biographical, constrained, national).

% Pay the premium embedded in prices that would fall under genuine competition. Have essentially no individual exit — the dominant firm controls the relevant channel — and no organized capacity to contest the arrangement outside occasional regulatory action taken on their behalf.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, downstream_consumers, payer,
    powerless, biographical, trapped, national).

% Were displaced or absorbed when the incumbent consolidated the market and now depend on the dominant firm's procurement terms for market access; the closure's maintenance (exclusive distribution deals, quality-standard capture) forecloses the alternative channels they would otherwise use.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, displaced_smallholder_producers, payer,
    powerless, generational, trapped, regional).

% Investigate whether the dominant position is defended through anticompetitive conduct or merely reflects durable efficiency; take testimony and commission studies but are subject to lobbying capture and resource constraints that limit enforcement.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, antitrust_regulators, observer,
    institutional, generational, analytical, national).

% Would fund entry or interoperability mandates if permitted a foothold, but are kept from acquiring the market access, standards influence, or financing terms that would let them mount a credible challenge.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, rival_capital_coalitions, excluded,
    organized, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders).
narrative_ontology:fixing_cost_class(market_naturalization__beneficiary_maintained_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides predictable supply, quality standards, and investment certainty within the dominant firm's channel — customers and downstream partners can rely on consistent terms without renegotiating from scratch with fragmented suppliers.
% TRANSFER_FUNCTION: Moves a price premium and access rents from consumers, entrants, and displaced smallholder producers to incumbent capital holders, executives, and their financial intermediaries, sustained by the ongoing legal, contractual, and lobbying expenditure that keeps rivals out.
% ABSENT_VOICES: Rival capital coalitions and would-be entrants who lack standing in current regulatory proceedings would argue the dominant position is maintained rather than earned; they are structurally excluded from the antitrust and legislative venues where the closure's legitimacy is contested.
% DISAPPEARANCE_RATIONALE: If the active maintenance apparatus (exclusive contracts, litigation, patent enforcement, lobbying) were withdrawn, entrants would contest the market within a few product cycles, prices would fall toward marginal cost, and the intermediaries financing the closure would lose their associated fee stream — the arrangement is not self-sustaining without continued expenditure.
% FOUNDING_PROBLEM: The dominant position originated (per this reading) from a genuine early efficiency or first-mover advantage that solved a real coordination problem — establishing a reliable supply and quality standard in a previously fragmented market.
% FOUNDING_PROBLEM_CORROBORATION: Antitrust regulators' own economic studies and independent industrial-organization research find the efficiency gap that originally justified the position has narrowed or closed, while the incumbent's own filings and executive testimony continue to assert the position is still earned by superior performance — the corroboration for 'dead' comes from outside the beneficiary class; the beneficiaries themselves dispute the finding.
narrative_ontology:disappearance_verdict(market_naturalization__beneficiary_maintained_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_naturalization__beneficiary_maintained_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__beneficiary_maintained_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(market_naturalization__beneficiary_maintained_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__beneficiary_maintained_reading, 0.79, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_naturalization__beneficiary_maintained_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_naturalization__beneficiary_maintained_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(market_naturalization__beneficiary_maintained_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the interval (0.48 to 0.79) reflecting an accumulating rent stream as the closure's coordination-function origin recedes and its maintenance cost (legal, lobbying, financial) grows relative to any residual efficiency benefit. Suppression tracks closely alongside (0.50 to 0.81) because, under this reading, the closure's persistence is definitionally tied to the intensity of active defense — this is the reading's core empirical commitment, distinguishing it from lapsed_alternative_reading where suppression would be near-zero. Theater ratio stays comparatively low (0.28 by interval end) because most of the defensive expenditure is genuinely functional in maintaining the closure (real litigation, real contracts) rather than performative.
 *
 * PERSPECTIVAL GAP:
 *   From the incumbent capital holder seat, the dominant position reads as legitimately earned and merely 'protected' rather than 'maintained' — a coordination function still doing real work. From the entrant and consumer seats, the same defensive apparatus reads as an actively operated extraction machine whose coordination justification expired once the original efficiency gap closed. The engine computes this divergence from the structural declarations (power, exit, beneficiary/victim); this story does not adjudicate which seat is correct — that adjudication is the work the kernel-reading decomposition is designed to make visible rather than average away.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent capital holders and their affiliated executives and financiers sit at the beneficiary end — the closure's entire structural logic, under this reading, exists to protect their rent stream, and their exit options (arbitrage, mobile) reflect their capacity to redeploy capital while the enforcement apparatus persists. Entrants, consumers, and displaced smallholder producers sit at the target end: constrained or trapped exit, no capacity to unilaterally dissolve the closure, bearing the premium or the foreclosure of access as the transfer mechanism's output.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem is authored as dead (the original coordination rationale has lapsed per outside corroboration) while the disappearance_verdict is world_rearranges — this is precisely the mismatch signature the R5 genealogy interview is designed to surface: a mandate whose original function has ended but whose apparatus persists because removing it would still visibly disrupt an entrenched arrangement. That mismatch is read as a capture/zombie flag under this reading, distinguishing it from a genuine still-functioning coordination structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_maintenance_vs_lapsed_closure,
    'Is the dominant position''s persistence actually explained by ongoing, funded defensive activity (this reading), or would the position persist even if incumbent capital holders withdrew all defensive expenditure (the lapsed_alternative_reading)?',
    'Natural experiment: identify episodes where incumbent lobbying/litigation expenditure fell sharply (e.g. leadership transition, antitrust consent decree limiting spend) and observe whether entrant activity and price competition responded within a bounded window. A responsive market supports this reading; an unresponsive market supports the lapsed reading.',
    'If the market proves unresponsive to withdrawn defensive spending, this reading''s core structural claim (active maintenance) is falsified for this case and the constraint should be reclassified toward the lapsed_alternative_reading''s profile rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_maintenance_vs_lapsed_closure, empirical, 'Whether the closure is actively maintained or has lapsed into self-sustaining stability.').

omega_variable(
    reading_location_of_disagreement,
    'Where exactly do the beneficiary_maintained_reading and lapsed_alternative_reading disagree structurally — is it the beneficiary declaration (present here, absent there), the suppression trajectory (rising here, flat/near-zero there), or both simultaneously?',
    'Compare the two sibling constraint files'' base_properties directly: beneficiaries[] presence/absence and the suppression measurement series slope.',
    'If the disagreement is confined to the suppression trajectory alone (both readings share the same beneficiary class but disagree only on whether current expenditure is causally load-bearing), the kernel dispute is narrower than it first appears and future hybrid readings should be weighted accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_location_of_disagreement, conceptual, 'Locating the precise structural element the sibling readings diverge on.').

omega_variable(
    efficiency_origin_authenticity,
    'Was the original dominant position genuinely won through a real coordination-solving efficiency advantage, or was defensive/exclusionary conduct present from the outset, making the ''founding problem'' narrative itself a retrospective justification?',
    'Historical record review of the market''s founding period — were exclusive contracts, predatory pricing, or lobbying present at formation, or did they emerge only after the position was established?',
    'If exclusionary conduct predates any genuine efficiency advantage, the founding_problem narrative is itself part of the extraction apparatus rather than a lapsed legitimate origin, strengthening this reading''s classification and weakening the coordination-function claim entirely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(efficiency_origin_authenticity, empirical, 'Whether the founding efficiency story is historically accurate or a retrospective legitimation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__beneficiary_maintained_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_naturalization__beneficiary_maintained_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(mark_tr_t8, market_naturalization__beneficiary_maintained_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(mark_tr_t16, market_naturalization__beneficiary_maintained_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(mark_tr_t24, market_naturalization__beneficiary_maintained_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(mark_tr_t32, market_naturalization__beneficiary_maintained_reading, theater_ratio, 32, 0.25).
narrative_ontology:measurement(mark_tr_t40, market_naturalization__beneficiary_maintained_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(mark_be_t8, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(mark_be_t16, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 16, 0.62).
narrative_ontology:measurement(mark_be_t24, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 24, 0.7).
narrative_ontology:measurement(mark_be_t32, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 32, 0.75).
narrative_ontology:measurement(mark_be_t40, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 40, 0.79).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(mark_su_t8, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(mark_su_t16, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 16, 0.66).
narrative_ontology:measurement(mark_su_t24, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(mark_su_t32, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 32, 0.77).
narrative_ontology:measurement(mark_su_t40, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 40, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__beneficiary_maintained_reading, resource_allocation).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, market_naturalization__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, market_naturalization__hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraint files decomposing the natural-language claim 'market dominance is/isn't naturally sustained' (the market_naturalization kernel) into structurally distinct readings, per the ε-invariance principle. This file (beneficiary_maintained_reading) authors high, rising extractiveness and suppression driven by an identifiable, actively-funding beneficiary class. lapsed_alternative_reading authors near-zero suppression and low extractiveness (no active maintenance needed). hybrid_reading blends the two, authoring partial suppression and moderate, non-monotonic extractiveness. All three share the underlying observable (a persistently dominant market position) but assign it structurally different ε values because they disagree on the causal mechanism of persistence — this is not one constraint measured three ways, but three constraints linked by network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
