% ============================================================================
% CONSTRAINT STORY: market_naturalization__beneficiary_maintained_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: market_naturalization__beneficiary_maintained_reading
 *   human_readable: Market Dominance as Actively Defended Incumbent Position (Beneficiary-Maintained Reading)
 *   domain: political economy / institutional analysis
 *
 * SUMMARY:
 *   This story instantiates the beneficiary-maintained reading of the
 *   market_naturalization kernel: market dominance is presented as though it
 *   were a settled, closed outcome of prior competition, but this reading
 *   holds that the dominance is actively defended by incumbent capital
 *   holders through ongoing lobbying, exclusionary contracting, litigation
 *   funding, and standard-setting capture. The coordination story (scale
 *   efficiency, quality assurance) is real but no longer sufficient on its
 *   own to explain the persistence of the position — continuous resource
 *   expenditure on defense is required, and that expenditure is directed by
 *   an identifiable beneficiary class. This is distinct from the
 *   lapsed_alternative_reading (which holds no active maintenance is
 *   occurring, the position rests on settled first-mover history) and the
 *   hybrid_reading (which holds some elements have lapsed into settled fact
 *   while others require active defense). Only this reading is generated
 *   here; the siblings are separate constraint files linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - incumbent_capital_holders: agenda-setter and beneficiary, funds active defense apparatus
 *   - dominant_firm_shareholders: passive beneficiary, liquid exit distinguishes from operational incumbents
 *   - market_entrants: primary target, faces raised entry costs
 *   - downstream_consumers: diffuse payer, trapped exit at point of purchase
 *   - displaced_suppliers: dependent payer, constrained exit via monopsony leverage
 *   - antitrust_regulators: analytical observer with remedy power
 *   - policy_reform_advocates: excluded from standard-setting venues
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__beneficiary_maintained_reading, 0.78).
domain_priors:suppression_score(market_naturalization__beneficiary_maintained_reading, 0.81).
domain_priors:theater_ratio(market_naturalization__beneficiary_maintained_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__beneficiary_maintained_reading, tangled_rope).
narrative_ontology:human_readable(market_naturalization__beneficiary_maintained_reading, "Market Dominance as Actively Defended Incumbent Position (Beneficiary-Maintained Reading)").
narrative_ontology:topic_domain(market_naturalization__beneficiary_maintained_reading, "political economy / institutional analysis").

domain_priors:requires_active_enforcement(market_naturalization__beneficiary_maintained_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__beneficiary_maintained_reading, '416a2fbd-9275-4b67-9b1d-58cbb128a003').
narrative_ontology:cs_kernel_codification('416a2fbd-9275-4b67-9b1d-58cbb128a003', distributed).
narrative_ontology:cs_authority_grounding('416a2fbd-9275-4b67-9b1d-58cbb128a003', extraction).
narrative_ontology:cs_interpretation_layer_present('416a2fbd-9275-4b67-9b1d-58cbb128a003').
narrative_ontology:cs_reading_relation('416a2fbd-9275-4b67-9b1d-58cbb128a003', market_naturalization__lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_reading_relation('416a2fbd-9275-4b67-9b1d-58cbb128a003', market_naturalization__hybrid_reading, influences).
narrative_ontology:cs_axiom('416a2fbd-9275-4b67-9b1d-58cbb128a003', foundational, dominance_requires_continuous_resourced_defense).
narrative_ontology:cs_axiom_status(dominance_requires_continuous_resourced_defense, holdable).
narrative_ontology:cs_axiom_grounding('416a2fbd-9275-4b67-9b1d-58cbb128a003', dominance_requires_continuous_resourced_defense, empirically_contingent).
narrative_ontology:cs_axiom('416a2fbd-9275-4b67-9b1d-58cbb128a003', secondary, defense_expenditure_evidences_active_suppression_not_natural_closure).
narrative_ontology:cs_axiom_status(defense_expenditure_evidences_active_suppression_not_natural_closure, holdable).
narrative_ontology:cs_axiom_grounding('416a2fbd-9275-4b67-9b1d-58cbb128a003', defense_expenditure_evidences_active_suppression_not_natural_closure, empirically_contingent).
narrative_ontology:cs_reference_frame('416a2fbd-9275-4b67-9b1d-58cbb128a003', contested_market_open_to_entry).
narrative_ontology:cs_drift_state('416a2fbd-9275-4b67-9b1d-58cbb128a003', contemporary_consolidated_market, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('416a2fbd-9275-4b67-9b1d-58cbb128a003', '').
narrative_ontology:cs_kernel_id(market_naturalization__beneficiary_maintained_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders).
narrative_ontology:constraint_beneficiary(market_naturalization__beneficiary_maintained_reading, dominant_firm_shareholders).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, market_entrants).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, downstream_consumers).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, displaced_suppliers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the dominant firm(s) in the market and actively fund lobbying, litigation, standard-setting capture, and exclusive contracting to preserve market position. They frame their dominance as the natural outcome of superior efficiency and innovation, but the ongoing spend on legal, regulatory, and exclusionary infrastructure indicates the position requires continuous maintenance rather than resting on settled first-mover advantage alone. They collect supra-competitive rents directly from the arrangement.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders, beneficiary).

% Receive the financial returns generated by durable market power without directly running the defensive apparatus. Their exit option is genuinely liquid (they can sell shares), which distinguishes them from the agenda-setting capital holders who have operational control and identity investment in maintaining the position.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, dominant_firm_shareholders, beneficiary,
    powerful, biographical, arbitrage, national).

% Attempt to compete against the incumbent but face artificially raised entry costs: exclusive distribution agreements, patent thickets, predatory pricing episodes, and regulatory capture that inflates compliance burden disproportionately for new entrants. Their capital and access to distribution are structurally inferior to the incumbent's, so exit from the attempt (rather than exit from the market structure) is their only realistic move.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, market_entrants, payer,
    moderate, biographical, constrained, national).

% Pay supra-competitive prices or accept degraded quality/choice because the incumbent's dominance forecloses realistic alternatives at the point of purchase. Individually they have essentially no leverage; only through class-scale coordination (regulatory complaint, collective boycott) could they meaningfully register resistance, and that coordination is itself costly and diffuse.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, downstream_consumers, payer,
    powerless, immediate, trapped, national).

% Depend on access to the incumbent's distribution or procurement channel and are squeezed on terms because the incumbent's dominance gives it monopsony-like bargaining leverage. Switching to alternative buyers is limited by the incumbent's market share, so formal independence coexists with practical dependency.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, displaced_suppliers, payer,
    moderate, biographical, constrained, national).

% Investigate whether the dominant position rests on legitimate competitive advantage or on maintained exclusionary conduct. They gather evidence on lobbying expenditure, exclusionary contracts, and merger history, and can impose remedies (divestiture, conduct restrictions) that would test whether the dominance persists absent active defense.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, antitrust_regulators, observer,
    institutional, generational, analytical, national).

% Argue for structural remedies (breakup, open standards, interoperability mandates) but are largely locked out of the legislative and regulatory drafting process, which is disproportionately shaped by incumbent-funded lobbying presence in the same venues.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, policy_reform_advocates, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A large, capital-intensive market benefits from some degree of standardization, brand-based quality assurance, and economies of scale that reduce transaction costs for consumers and suppliers relative to a fully fragmented market.
% TRANSFER_FUNCTION: Moves supra-competitive rents from consumers, would-be entrants, and dependent suppliers to incumbent capital holders and shareholders, mediated through defensive legal, regulatory, and contractual infrastructure that the incumbent funds and maintains.
% ABSENT_VOICES: Market entrants who never attempted entry because the defended barriers were legible in advance are invisible to the record entirely; policy reform advocates who are structurally excluded from standard-setting and legislative drafting venues would object to characterizing the dominance as settled or natural.
% DISAPPEARANCE_RATIONALE: If the active defense apparatus (lobbying, exclusive contracting, litigation funding, capture of standard-setting bodies) were withdrawn overnight, entrants would test the market within a short window, prices would likely fall toward marginal cost, and the incumbent's position would erode measurably absent the maintenance activity — indicating the dominance depends on ongoing defense rather than resting on settled, self-sustaining efficiency advantage alone.
% FOUNDING_PROBLEM: Early market consolidation solved a genuine coordination problem: fragmented small-scale production imposed high transaction costs, inconsistent quality, and inefficient distribution; a dominant integrated player could achieve economies of scale that lowered costs system-wide.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent capital holders attest the founding efficiency problem remains live and justifies continued scale. Antitrust regulators' merger-review findings and independent economic analyses commissioned outside the incumbent's funding indicate the efficiency gains plateaued long ago and that the current defensive apparatus is oriented toward rent preservation rather than solving the original coordination problem — this corroboration comes from outside the beneficiary class.
narrative_ontology:disappearance_verdict(market_naturalization__beneficiary_maintained_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_naturalization__beneficiary_maintained_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__beneficiary_maintained_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.78) and suppression (0.81) are both authored high because this reading's defining claim is that dominance requires active, resourced maintenance against contestation — the defense spend is itself evidence that alternatives are not naturally foreclosed but must be actively suppressed. Theater ratio is moderate-low (0.32): the scale-efficiency function is genuinely real for part of the operation, but a rising share of activity (lobbying, litigation, capture of standard bodies) is oriented toward rent preservation rather than production efficiency. Accessibility collapse (0.58) is well below mountain-level because entrants do exist and do attempt entry; resistance (0.62) reflects active regulatory scrutiny and periodic entrant challenges. All temporal series share one time grid across the 40-period interval.
 *
 * PERSPECTIVAL GAP:
 *   From the incumbent capital holder seat, the arrangement reads as legitimate protection of hard-won competitive advantage — active defense simply defends fairly earned position. From the market entrant and consumer seats, the identical structure operates as maintained extraction: barriers that would not exist absent continuous lobbying and contractual exclusivity spend. The engine's per-seat computation should surface this divergence directly from the beneficiary/victim declarations and exit-option asymmetry, without either seat's framing being privileged in the base data.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent capital holders sit at the full-beneficiary end: they administer the defense apparatus and capture the rents directly (d near 0). Dominant firm shareholders are also beneficiaries but with liquid exit, differentiating them structurally from the agenda-setting incumbents. Market entrants, downstream consumers, and displaced suppliers are declared victims with constrained-to-trapped exit, placing them near the full-target end — the engine should compute high effective extraction for these seats given their limited mobility and the national scope over which the defense apparatus operates.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding coordination problem (economies of scale reducing fragmented-market transaction costs) is contested as to whether it remains live. If genuinely dead — if scale efficiencies plateaued decades ago — then the current defense apparatus is pure rent-preservation dressed in the language of a coordination function that no longer needs defending, which is precisely the tangled_rope-to-snare drift this reading's rising extractiveness/suppression trajectory is tracking. The tangled_rope classification (rather than outright snare) is retained here because a genuine, non-trivial coordination residue (scale, standardization) is still authored as present — the omega below flags exactly how much of that residue survives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    residual_coordination_function_share,
    'What fraction of the incumbent''s current market position is attributable to genuine, still-live scale/standardization efficiency versus pure rent-preservation through active defense?',
    'Structural remedy natural experiment: measure market share, pricing, and entry rate before and after imposed divestiture or conduct restrictions in comparable jurisdictions; a large post-remedy erosion of position indicates the coordination residue was small.',
    'A large surviving coordination residue supports the tangled_rope classification as authored; a near-zero residue would push this reading toward snare, since the coordination story would then be pure cover for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_coordination_function_share, empirical, 'How much of the dominant position rests on genuine ongoing coordination value versus defended rent.').

omega_variable(
    kernel_reading_selection_evidence,
    'What observable evidence distinguishes this beneficiary-maintained reading from the lapsed_alternative_reading for a given market at a given time — is active defense spend itself sufficient, or could it reflect defensive posture against a threat that would not actually succeed?',
    'Compare defense expenditure trends against counterfactual entry-success rates in adjacent markets without comparable defense spend; convergent entry rates despite differing defense spend would weaken this reading''s claim that the defense is causally load-bearing.',
    'If defense spend is found to be largely inert (i.e., dominance would persist with or without it), the beneficiary_maintained_reading collapses toward the lapsed_alternative_reading for that market; if defense spend is found to be load-bearing, this reading is corroborated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, conceptual, 'Whether observed active defense spend is genuinely load-bearing for the dominance it accompanies, or is precautionary excess.').

omega_variable(
    capital_holder_class_boundary,
    'Is ''incumbent capital holders'' a stable, identifiable class across the interval, or does its composition shift (e.g., through ownership turnover, index-fund diffusion) in ways that dilute the beneficiary concentration this reading depends on?',
    'Ownership registry analysis tracking concentration (e.g., Herfindahl index of voting control) over the measurement interval.',
    'If ownership diffuses substantially, the sharp beneficiary/victim asymmetry this reading relies on weakens, and the classification would need re-evaluation toward a more diffuse extraction pattern.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_holder_class_boundary, empirical, 'Whether the beneficiary class remains concentrated enough to sustain the tangled_rope beneficiary/victim asymmetry over time.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__beneficiary_maintained_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_naturalization__beneficiary_maintained_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(mark_tr_t8, market_naturalization__beneficiary_maintained_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(mark_tr_t16, market_naturalization__beneficiary_maintained_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(mark_tr_t24, market_naturalization__beneficiary_maintained_reading, theater_ratio, 24, 0.25).
narrative_ontology:measurement(mark_tr_t32, market_naturalization__beneficiary_maintained_reading, theater_ratio, 32, 0.29).
narrative_ontology:measurement(mark_tr_t40, market_naturalization__beneficiary_maintained_reading, theater_ratio, 40, 0.32).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(mark_be_t8, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(mark_be_t16, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(mark_be_t24, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(mark_be_t32, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 32, 0.74).
narrative_ontology:measurement(mark_be_t40, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 40, 0.78).

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
narrative_ontology:boltzmann_floor_override(market_naturalization__beneficiary_maintained_reading, 0.12).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, market_naturalization__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, market_naturalization__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the market_naturalization kernel. beneficiary_maintained_reading (this file) asserts active, resourced defense by an identifiable capital-holder class with high extractiveness and suppression. lapsed_alternative_reading asserts the dominance is a settled historical closure requiring no active maintenance, with correspondingly low extractiveness and near-mountain metrics. hybrid_reading asserts a mixed profile where some elements have lapsed into settled fact while others remain actively defended, sitting structurally between the other two. Each reading is authored as its own ε-invariant constraint per DP-001; they are linked here for contamination-propagation and family-tracking purposes only, not to average or reconcile their classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
