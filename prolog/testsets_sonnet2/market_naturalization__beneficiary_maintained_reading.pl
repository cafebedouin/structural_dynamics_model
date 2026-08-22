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
 *   constraint_id: market_naturalization__beneficiary_maintained_reading
 *   human_readable: Market Dominance as Actively Defended Incumbent Position
 *   domain: political economy / institutional analysis
 *
 * SUMMARY:
 *   This story instantiates the beneficiary_maintained_reading of the
 *   market_naturalization kernel: the claim that observed market
 *   concentration in a given sector persists NOT because the conditions that
 *   originally justified consolidation are still operative, but because
 *   incumbent capital holders continuously and actively defend the position
 *   through lobbying, exclusive contracting, litigation, and regulatory
 *   capture. Under this reading, 'the market naturally concentrates' is a
 *   cover story; the underlying reality is an identifiable beneficiary class
 *   doing ongoing enforcement work. The sibling readings —
 *   lapsed_alternative_reading (concentration is a stable historical
 *   settlement requiring no active maintenance) and hybrid_reading (a mix of
 *   lapsed and active elements) — are NOT represented here; they are separate
 *   constraint stories with their own ε and structural data. This story's ε
 *   (0.78) and suppression (0.81) are authored specifically for the
 *   arrangement AS THIS READING SEES IT: high, because active defense is the
 *   reading's central empirical claim.
 *
 * KEY AGENTS:
 *   - incumbent_capital_holders: primary beneficiary and agenda-setter (institutional/arbitrage) — funds and directs the active-maintenance apparatus
 *   - entrenched_market_leaders: secondary beneficiary (powerful/mobile) — benefits from but does not run the defense apparatus
 *   - prospective_market_entrants: primary target (moderate/constrained) — bears foreclosed-entry costs
 *   - downstream_consumers: diffuse target (powerless/trapped) — bears markup costs invisibly
 *   - displaced_smallholder_producers: concentrated target (powerless/trapped) — bears displacement costs directly
 *   - antitrust_regulators: observer with capture risk (institutional/analytical)
 *   - displaced_alternative_market_forms: excluded (powerless/trapped) — foreclosed institutional alternatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__beneficiary_maintained_reading, 0.78).
domain_priors:suppression_score(market_naturalization__beneficiary_maintained_reading, 0.81).
domain_priors:theater_ratio(market_naturalization__beneficiary_maintained_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__beneficiary_maintained_reading, tangled_rope).
narrative_ontology:human_readable(market_naturalization__beneficiary_maintained_reading, "Market Dominance as Actively Defended Incumbent Position").
narrative_ontology:topic_domain(market_naturalization__beneficiary_maintained_reading, "political economy / institutional analysis").

domain_priors:requires_active_enforcement(market_naturalization__beneficiary_maintained_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__beneficiary_maintained_reading, 'a4e7cbc3-03f5-410a-af01-59e797fcec1e').
narrative_ontology:cs_kernel_codification('a4e7cbc3-03f5-410a-af01-59e797fcec1e', distributed).
narrative_ontology:cs_authority_grounding('a4e7cbc3-03f5-410a-af01-59e797fcec1e', extraction).
narrative_ontology:cs_interpretation_layer_present('a4e7cbc3-03f5-410a-af01-59e797fcec1e').
narrative_ontology:cs_reading_relation('a4e7cbc3-03f5-410a-af01-59e797fcec1e', market_naturalization__lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_reading_relation('a4e7cbc3-03f5-410a-af01-59e797fcec1e', market_naturalization__hybrid_reading, influences).
narrative_ontology:cs_axiom('a4e7cbc3-03f5-410a-af01-59e797fcec1e', foundational, concentration_requires_ongoing_enforcement).
narrative_ontology:cs_axiom_status(concentration_requires_ongoing_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('a4e7cbc3-03f5-410a-af01-59e797fcec1e', concentration_requires_ongoing_enforcement, empirically_contingent).
narrative_ontology:cs_axiom('a4e7cbc3-03f5-410a-af01-59e797fcec1e', secondary, efficiency_narrative_is_post_hoc_justification).
narrative_ontology:cs_axiom_status(efficiency_narrative_is_post_hoc_justification, holdable).
narrative_ontology:cs_axiom_grounding('a4e7cbc3-03f5-410a-af01-59e797fcec1e', efficiency_narrative_is_post_hoc_justification, empirically_contingent).
narrative_ontology:cs_reference_frame('a4e7cbc3-03f5-410a-af01-59e797fcec1e', formative_scale_economy_coordination).
narrative_ontology:cs_drift_state('a4e7cbc3-03f5-410a-af01-59e797fcec1e', contemporary_concentrated_market, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a4e7cbc3-03f5-410a-af01-59e797fcec1e', '').
narrative_ontology:cs_kernel_id(market_naturalization__beneficiary_maintained_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders).
narrative_ontology:constraint_beneficiary(market_naturalization__beneficiary_maintained_reading, entrenched_market_leaders).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, prospective_market_entrants).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, downstream_consumers).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, displaced_smallholder_producers).
narrative_ontology:constraint_vindicates(market_naturalization__beneficiary_maintained_reading, market_concentration_is_efficient_outcome_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the capital stock, distribution channels, regulatory relationships, and standard-setting bodies that determine who can compete. Actively fund lobbying, litigation, exclusive supply contracts, and preferential regulatory treatment to keep their position. Present the resulting concentration as the natural outcome of efficiency and scale, not as something they built and defend daily.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders, beneficiary).

% Operate under the protective umbrella the capital holders maintain — favorable financing terms, privileged shelf space, exclusive licensing — without necessarily running the enforcement apparatus themselves. Benefit from barriers to entry they did not personally construct but actively use.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, entrenched_market_leaders, beneficiary,
    powerful, biographical, mobile, national).

% Face capital requirements, exclusive contracts, patent thickets, and regulatory compliance costs calibrated to what incumbents can absorb but new entrants cannot. Their alternative is to seek acquisition by an incumbent or exit the sector; independent competition is structurally foreclosed by design, not by market outcome alone.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, prospective_market_entrants, payer,
    moderate, biographical, constrained, national).

% Pay prices that reflect the absence of competitive pressure — markups above what a contestable market would sustain. Have no visibility into the enforcement machinery producing this price floor and typically experience it as 'just how the market is,' with no organized bargaining position against it.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, downstream_consumers, payer,
    powerless, biographical, trapped, national).

% Were pushed out of upstream or adjacent markets when incumbents used vertical integration, predatory pricing, or supply-chain leverage to consolidate. Bear concentrated losses (land, livelihood, capital) that are invisible in aggregate market statistics.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, displaced_smallholder_producers, payer,
    powerless, biographical, trapped, regional).

% Nominally empowered to assess and remedy anti-competitive concentration, but frequently under-resourced, subject to regulatory capture, and dependent on incumbent-funded economic analysis to make their case. Their institutional position gives them observer capacity without matching enforcement capacity.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, antitrust_regulators, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(market_naturalization__beneficiary_maintained_reading, antitrust_regulators, excluded).

% Cooperative ownership structures, mutual associations, and decentralized production networks that were viable historically but are foreclosed under the current concentrated arrangement — not because they were tested and failed, but because incumbents' capital and enforcement advantages preempted their scaling.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, displaced_alternative_market_forms, excluded,
    powerless, civilizational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders).
narrative_ontology:fixing_cost_class(market_naturalization__beneficiary_maintained_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement does solve a genuine coordination problem at some scale — standardized quality, predictable supply chains, capital-intensive infrastructure that benefits from consolidation — but this reading holds that the coordination function is now dwarfed by, and subordinate to, an active maintenance apparatus that exists to prevent contestation.
% TRANSFER_FUNCTION: Moves rents from consumers, smaller producers, and prospective entrants to incumbent capital holders, via price markups above competitive levels, foreclosed entry, and captured regulatory outcomes — a continuous transfer sustained by ongoing enforcement, not a one-time historical settlement.
% ABSENT_VOICES: Displaced smallholder producers and the cooperative/mutual market forms that were crowded out rarely appear in the policy conversation about market structure; their absence is why 'market concentration reflects efficiency' can be asserted without being tested against what was foreclosed.
% DISAPPEARANCE_RATIONALE: If incumbent capital holders stopped actively defending their position — withdrew lobbying, stopped exclusive contracting, ceased litigation against entrants — this reading holds that entrants would flow in within a market cycle, prices would compress toward marginal cost, and displaced production forms would have room to re-emerge. The reading's entire claim is that the current arrangement requires continuous active work, so its removal is not neutral.
% FOUNDING_PROBLEM: Early-stage capital-intensive industries needed scale economies and coordinated standards to become viable at all — a genuine coordination problem in the formative period of the market.
% FOUNDING_PROBLEM_CORROBORATION: Independent competition economists and antitrust litigation records (outside the incumbents themselves) document markups persisting well above estimated marginal cost and document active exclusionary conduct (exclusive dealing, strategic litigation, regulatory capture) continuing decades after the scale-economy rationale would have been satisfied — this is the evidentiary basis for reading the founding problem as resolved while the arrangement persists as rent extraction.
narrative_ontology:disappearance_verdict(market_naturalization__beneficiary_maintained_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_naturalization__beneficiary_maintained_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__beneficiary_maintained_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(market_naturalization__beneficiary_maintained_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__beneficiary_maintained_reading, 0.78, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises across the interval (0.55 -> 0.78) reflecting this reading's claim that rent capture has intensified as the coordination rationale weakened relative to the entrenchment apparatus. Suppression is high and rising in step (0.58 -> 0.81) because under this reading the arrangement's persistence is causally tied to enforcement intensity — every measured increment of extraction correlates with an increment of active defensive activity (litigation volume, lobbying expenditure, exclusive-dealing contracts), not with independent market conditions. Theater ratio stays comparatively low (0.12 -> 0.28): under this reading, most of the maintenance activity is functionally effective at excluding entrants (not performative) — a genuinely defended position, not a hollowed-out one. This is what distinguishes this reading from the lapsed_alternative_reading, where the same superficial structure would show low suppression and low functional defense.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent capital holders sit at the extreme beneficiary end of directionality: they set the enforcement agenda, capture the transfer, and hold arbitrage-grade exit (they can relocate capital, restructure entities, or exit specific product lines without losing aggregate position). Entrenched market leaders benefit but with less agency over the apparatus itself. Prospective entrants and downstream consumers sit at the target end — constrained or fully trapped exit, respectively, with the constraint's costs landing on them without proportionate voice. Displaced smallholder producers experience the most concentrated, least diffuse version of the extraction: their losses are not statistical noise but direct dispossession, which is why their exit option is authored as trapped rather than constrained.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem/disappearance_verdict pair captures the core mandatrophy claim of this reading: founding_problem_status is authored 'dead' (the scale-economy rationale that justified original consolidation has been satisfied for decades in the sectors this reading models) while disappearance_verdict is 'world_rearranges' (the arrangement still visibly organizes flows of money and opportunity). This mismatch is precisely the signature the corpus is built to detect — a coordination story whose founding function has expired but whose extraction machinery has not, sustained now by active maintenance rather than residual function. Classifying this as tangled_rope (not snare) preserves the historically genuine coordination kernel (scale economies were real once) while registering that the current operation is dominated by asymmetric extraction requiring continuous enforcement — exactly what distinguishes this reading from the lapsed_alternative_reading's implicit claim that no such enforcement is occurring.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_maintenance_vs_residual_settlement,
    'Is the observed market concentration sustained by continuous, identifiable active-defense conduct (lobbying spend, litigation frequency, exclusive-dealing renewal), or has it settled into a stable configuration that would persist even without further active intervention by capital holders?',
    'Track incumbent lobbying/litigation expenditure and exclusive-contract renewal rates against entry rates over a multi-decade window; a sustained positive correlation between defensive spending and entry suppression supports this reading, while a decoupling (concentration persisting even as defensive spending falls) would support the lapsed_alternative_reading instead.',
    'If defensive spending is found to be causally decoupled from persistence, this story''s classification (tangled_rope, requiring active enforcement) would be wrong for this sector and the lapsed_alternative_reading would be the correct constraint to author for it — this is exactly the committer disagreement the kernel exists to hold across separate stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_maintenance_vs_residual_settlement, empirical, 'Whether concentration is actively maintained or has become a self-sustaining lapsed settlement.').

omega_variable(
    counterfactual_alternative_viability,
    'Were the foreclosed alternative market forms (cooperatives, mutual associations, decentralized production networks) genuinely viable at scale, or would they have failed on their own merits absent any incumbent suppression?',
    'Comparative institutional analysis of sectors/jurisdictions where cooperative or mutual forms persisted at scale under different capital-concentration regimes; natural experiments from deregulation or antitrust breakup events.',
    'If alternative forms were not viable independent of suppression, the extractiveness attributed to active defense is overstated and part of the current concentration reflects genuine efficiency rather than foreclosure — shifting weight toward the hybrid_reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_alternative_viability, conceptual, 'Whether foreclosed alternatives were genuinely viable, bearing on how much of current concentration is attributable to active suppression versus efficiency.').

omega_variable(
    regulatory_capture_depth,
    'To what degree have antitrust regulators been captured by the incumbents they are meant to check, versus genuinely constrained by resource limits and legal doctrine unrelated to capture?',
    'Personnel-flow analysis (revolving door rates between regulatory agencies and incumbent firms), comparison of enforcement intensity across jurisdictions with different capture exposure, and analysis of case outcomes relative to economic evidence presented.',
    'Deep capture would strengthen this reading''s suppression score and reduce confidence in antitrust_regulators as a genuine observer/check; shallow capture would suggest the observer seat retains more independent capacity than authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_depth, empirical, 'Degree of regulatory capture affecting the credibility of the observer seat and the suppression metric.').


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
narrative_ontology:measurement(mark_tr_t16, market_naturalization__beneficiary_maintained_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(mark_tr_t24, market_naturalization__beneficiary_maintained_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(mark_tr_t32, market_naturalization__beneficiary_maintained_reading, theater_ratio, 32, 0.25).
narrative_ontology:measurement(mark_tr_t40, market_naturalization__beneficiary_maintained_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(mark_be_t8, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 8, 0.61).
narrative_ontology:measurement(mark_be_t16, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 16, 0.67).
narrative_ontology:measurement(mark_be_t24, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 24, 0.72).
narrative_ontology:measurement(mark_be_t32, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 32, 0.76).
narrative_ontology:measurement(mark_be_t40, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(mark_su_t8, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 8, 0.64).
narrative_ontology:measurement(mark_su_t16, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(mark_su_t24, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 24, 0.75).
narrative_ontology:measurement(mark_su_t32, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 32, 0.79).
narrative_ontology:measurement(mark_su_t40, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 40, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__beneficiary_maintained_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_naturalization__beneficiary_maintained_reading, 0.1).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the market_naturalization kernel. beneficiary_maintained_reading (this story) claims concentration is actively defended by an identifiable beneficiary class, with correspondingly high authored extractiveness (0.78) and suppression (0.81). lapsed_alternative_reading claims the same observable concentration reflects a settlement no longer requiring active maintenance, and should author low suppression and low requires_active_enforcement dependency. hybrid_reading blends the two claims across different structural elements of the same market. All three share the same underlying kernel (a contested market structure) but are ε-invariant separate constraints per the decomposition principle — do not average their metrics or treat them as measurement-basis variants of one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
