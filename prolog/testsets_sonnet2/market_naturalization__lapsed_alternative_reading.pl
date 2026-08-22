% ============================================================================
% CONSTRAINT STORY: market_naturalization__lapsed_alternative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_naturalization__lapsed_alternative_reading, []).

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
 *   constraint_id: market_naturalization__lapsed_alternative_reading
 *   human_readable: Market Dominance as Lapsed Closure (No Active Maintenance)
 *   domain: political economy / economic history / institutional analysis
 *
 * SUMMARY:
 *   This story instantiates the 'lapsed alternative' reading of the
 *   market_naturalization kernel: the claim that a market's observed
 *   dominance pattern is a fossil of an old, once-genuine coordination
 *   solution rather than an actively defended extraction structure. On this
 *   reading, no identifiable class of incumbents is currently spending
 *   resources to maintain the closure — no lobbying, no predatory
 *   contracting, no coordinated exclusion. What looks like dominance is
 *   switching costs and habituation that atrophied into place and have simply
 *   never been challenged hard enough to erode. This is deliberately NOT the
 *   same constraint as the beneficiary_maintained_reading (which asserts
 *   active incumbent defense with a concentrated beneficiary class collecting
 *   rents) or the hybrid_reading (which asserts a mix). Each reading is
 *   authored as its own ε-invariant constraint with its own stakeholders and
 *   metrics; they are linked structurally through the kernel rather than
 *   merged into one story with a shared or averaged extractiveness.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__lapsed_alternative_reading, 0.14).
domain_priors:suppression_score(market_naturalization__lapsed_alternative_reading, 0.12).
domain_priors:theater_ratio(market_naturalization__lapsed_alternative_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, extractiveness, 0.14).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__lapsed_alternative_reading, piton).
narrative_ontology:human_readable(market_naturalization__lapsed_alternative_reading, "Market Dominance as Lapsed Closure (No Active Maintenance)").
narrative_ontology:topic_domain(market_naturalization__lapsed_alternative_reading, "political economy / economic history / institutional analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__lapsed_alternative_reading, '60ff1a90-0802-42d5-867e-146a1cd0f233').
narrative_ontology:cs_kernel_codification('60ff1a90-0802-42d5-867e-146a1cd0f233', distributed).
narrative_ontology:cs_authority_grounding('60ff1a90-0802-42d5-867e-146a1cd0f233', distributed).
narrative_ontology:cs_reading_relation('60ff1a90-0802-42d5-867e-146a1cd0f233', market_naturalization__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_reading_relation('60ff1a90-0802-42d5-867e-146a1cd0f233', market_naturalization__hybrid_reading, influences).
narrative_ontology:cs_axiom('60ff1a90-0802-42d5-867e-146a1cd0f233', foundational, dominance_persists_without_agency).
narrative_ontology:cs_axiom_status(dominance_persists_without_agency, holdable).
narrative_ontology:cs_axiom_grounding('60ff1a90-0802-42d5-867e-146a1cd0f233', dominance_persists_without_agency, empirically_contingent).
narrative_ontology:cs_axiom('60ff1a90-0802-42d5-867e-146a1cd0f233', secondary, no_extraction_without_an_extractor).
narrative_ontology:cs_axiom_status(no_extraction_without_an_extractor, holdable).
narrative_ontology:cs_axiom_grounding('60ff1a90-0802-42d5-867e-146a1cd0f233', no_extraction_without_an_extractor, conventional).
narrative_ontology:cs_reference_frame('60ff1a90-0802-42d5-867e-146a1cd0f233', genuine_early_coordination_equilibrium).
narrative_ontology:cs_drift_state('60ff1a90-0802-42d5-867e-146a1cd0f233', contemporary_market_structure, gap(stable, minor, true)).
narrative_ontology:cs_created_at('60ff1a90-0802-42d5-867e-146a1cd0f233', '').
narrative_ontology:cs_kernel_id(market_naturalization__lapsed_alternative_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_victim(market_naturalization__lapsed_alternative_reading, would_be_market_entrants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(market_naturalization__lapsed_alternative_reading, consumers).
narrative_ontology:constraint_victim(market_naturalization__lapsed_alternative_reading, consumers).
narrative_ontology:constraint_vindicates(market_naturalization__lapsed_alternative_reading, path_dependence_thesis).
narrative_ontology:constraint_vindicates(market_naturalization__lapsed_alternative_reading, coordination_cost_persistence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Occupy the dominant market position inherited from an earlier period of active competition or first-mover advantage. On this reading, they no longer invest significant resources in defending the position: no coordinated lobbying campaign, no predatory pricing regime, no systematic legal harassment of rivals. Their dominance persists because switching costs, network effects, and buyer habituation that were built long ago have simply never been dislodged. They could in principle lose the position to a well-resourced entrant; they mostly just haven't had to try.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, incumbent_firms, agenda_setter,
    institutional, generational, arbitrage, national).

% Face a market where switching costs and accumulated buyer habit make entry expensive, but face no active countermeasures from incumbents — no exclusive dealing, no predatory response, no coordinated exclusion. The barrier they experience is the residue of past coordination (standards, distribution relationships, accumulated trust) rather than anyone's current effort. Entry is costly but not blocked by enforcement; a sufficiently well-capitalized or well-differentiated entrant can and occasionally does succeed.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, would_be_market_entrants, payer,
    moderate, biographical, constrained, national).

% Benefit from the stability and predictability of an established market structure — known products, established service networks, interoperability that emerged when the dominant configuration won out. They also bear some cost in reduced variety and slower price competition, but on this reading that cost is a residue of coordination that once solved a real problem, not a rent someone is actively collecting.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, consumers, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(market_naturalization__lapsed_alternative_reading, consumers, payer).

% Study whether the observed dominance pattern reflects ongoing defensive action or mere inertia. Their historical and archival work — internal firm records, absence of lobbying expenditure, absence of exclusionary contracts — is what would distinguish this lapsed reading from the beneficiary-maintained or hybrid readings.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, economic_historians, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_naturalization__lapsed_alternative_reading, diffuse).
narrative_ontology:fixing_cost_class(market_naturalization__lapsed_alternative_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The original market configuration solved a genuine coordination problem — standardizing a technology, distribution channel, or product format so that buyers and sellers did not have to renegotiate compatibility with every transaction. That coordination function is real and historically dated.
% TRANSFER_FUNCTION: Very little is actively transferred today. What flows from would-be entrants to incumbents is not rent extracted through effort, but a residual switching cost paid to overcome inertia that nobody is currently reinforcing — closer to a toll booth with no tollkeeper than a toll booth with an armed guard.
% ABSENT_VOICES: Entrants who fail to break in rarely get to testify to regulators, because the absence of an identifiable culprit (no cartel, no predatory contract) makes their difficulty look like ordinary market friction rather than an actionable claim. Their frustration is real but structurally hard to represent as a complaint against any specific agenda-setter.
% DISAPPEARANCE_RATIONALE: If the dominance disappeared overnight, on this reading the market would likely re-equilibrate quickly through ordinary competitive dynamics, since no active enforcement machinery would need to be dismantled — supporting world_unchanged. But some observers argue accumulated switching costs and habituation would still take years to erode even without an enforcer, meaning some rearrangement is real. The verdict is contested precisely because it is the crux the sibling readings dispute.
% FOUNDING_PROBLEM: The dominant configuration was built to solve a genuine early-market coordination problem: establishing a common standard, technology, or distribution channel so buyers and sellers did not face incompatible options.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians outside the incumbent firms attest, based on absence of documented lobbying expenditure, absence of exclusionary contracting, and comparison to markets with active dominant-firm defense, that the coordination problem this configuration solved has been resolved for decades and no organized maintenance effort is observable in firm records or antitrust filings.
narrative_ontology:disappearance_verdict(market_naturalization__lapsed_alternative_reading, contested).
narrative_ontology:founding_problem_status(market_naturalization__lapsed_alternative_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__lapsed_alternative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(market_naturalization__lapsed_alternative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__lapsed_alternative_reading, 0.14, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_naturalization__lapsed_alternative_reading_tests).
:- end_tests(market_naturalization__lapsed_alternative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.14) and falling over the interval, reflecting the reading's core claim: whatever rent existed at founding has decayed as the coordination function became genuinely dead rather than merely dormant. Theater ratio starts moderate (0.40) — some vestigial signaling behavior (brand loyalty campaigns, routine trade-practice filings) persists from the earlier active period — and falls over time as even that theatrical residue fades. Suppression is low (0.12) because this reading denies active coercive maintenance; accessibility_collapse is moderate-high (0.58) because switching costs and habituation genuinely do collapse practical alternatives even without anyone enforcing that collapse. Resistance is low (0.15) because there is no active extractor to resist — entrants experience friction, not a struggle against an adversary.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary group is declared, consistent with the reading's central claim that there is no identifiable class currently collecting rents from the dominance. Would_be_market_entrants are named as payers because they bear the residual switching-cost burden, but the engine should derive a comparatively modest effective extraction for them given the absence of an active extractor and the presence of at least constrained (not trapped) exit — some entrants do succeed. Consumers are marked dual-role: beneficiaries of coordination stability, secondary payers of residual friction costs, again with low intensity given no active maintenance mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading directly tests the mandatrophy question for the kernel: is the founding coordination mandate still doing work, or has it quietly died while the structural residue persists? Authoring founding_problem_status as 'dead' and disappearance_verdict as 'contested' keeps the mismatch-detection machinery honest — a naive read might call this piton-flavored inertia; the corroboration from economic historians (outside any beneficiary class, since none is declared) supports treating the residual barrier as a genuine fossil rather than a captured mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lapsed_vs_maintained_ambiguity,
    'Is the observed dominance actually free of active maintenance, or does the lapsed reading simply fail to detect maintenance that is subtler than lobbying and exclusionary contracts (e.g., quiet coordination through trade associations, informal signaling, or algorithmic pricing coordination)?',
    'Forensic examination of firm communications, trade-association minutes, and pricing behavior for evidence of coordination that would not appear as formal lobbying expenditure or written exclusionary contracts.',
    'If subtler maintenance is found, this constraint''s classification collapses toward the beneficiary_maintained_reading or hybrid_reading; if genuinely absent, the lapsed reading is corroborated and the low extractiveness and piton-leaning classification stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lapsed_vs_maintained_ambiguity, empirical, 'Whether absence of visible maintenance evidence is genuine absence or detection failure.').

omega_variable(
    which_reading_is_true_of_this_market,
    'For any specific real market being analyzed through this kernel, which of the three readings (lapsed, beneficiary_maintained, hybrid) actually describes it, and is that a fact about the market or a framing choice by the analyst?',
    'Comparative case analysis across markets with documented active defense (patent thickets, exclusive dealing) versus markets with documented absence of such activity; the reading should track observable firm behavior, not analyst priors.',
    'Determines whether this constraint story''s low-extraction profile applies to a given real-world market, or whether that market is better modeled by a sibling reading with a concentrated beneficiary class and correspondingly higher extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_reading_is_true_of_this_market, conceptual, 'Whether reading selection is an empirical fact about the market or an interpretive framing choice.').

omega_variable(
    residual_switching_cost_as_extraction,
    'Does a switching cost that nobody actively reinforces still count as extraction from the entrant who pays it, even absent an identifiable extractor?',
    'Conceptual analysis of whether extraction requires an active extracting agent, or whether structural residue without an agent can still constitute extraction in the framework''s sense.',
    'If residue-without-agent counts as extraction, this reading''s low ε may understate the true cost to entrants; if extraction requires an agent, the low ε (0.14) is the correct authored value for this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_switching_cost_as_extraction, conceptual, 'Whether agentless structural residue constitutes extraction in the framework''s technical sense.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__lapsed_alternative_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_naturalization__lapsed_alternative_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(mark_tr_t8, market_naturalization__lapsed_alternative_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(mark_tr_t16, market_naturalization__lapsed_alternative_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(mark_tr_t24, market_naturalization__lapsed_alternative_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement(mark_tr_t32, market_naturalization__lapsed_alternative_reading, theater_ratio, 32, 0.24).
narrative_ontology:measurement(mark_tr_t40, market_naturalization__lapsed_alternative_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_naturalization__lapsed_alternative_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mark_be_t8, market_naturalization__lapsed_alternative_reading, base_extractiveness, 8, 0.27).
narrative_ontology:measurement(mark_be_t16, market_naturalization__lapsed_alternative_reading, base_extractiveness, 16, 0.21).
narrative_ontology:measurement(mark_be_t24, market_naturalization__lapsed_alternative_reading, base_extractiveness, 24, 0.17).
narrative_ontology:measurement(mark_be_t32, market_naturalization__lapsed_alternative_reading, base_extractiveness, 32, 0.15).
narrative_ontology:measurement(mark_be_t40, market_naturalization__lapsed_alternative_reading, base_extractiveness, 40, 0.14).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(market_naturalization__lapsed_alternative_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__lapsed_alternative_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_naturalization__lapsed_alternative_reading, 0.1).
narrative_ontology:affects_constraint(market_naturalization__lapsed_alternative_reading, market_naturalization__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_naturalization__lapsed_alternative_reading, market_naturalization__hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the market_naturalization kernel, decomposed per the epsilon-invariance principle rather than represented as one constraint with a measurement parameter. lapsed_alternative_reading authors low, falling extractiveness and no declared beneficiary class; beneficiary_maintained_reading authors a concentrated incumbent beneficiary class actively defending the position with correspondingly higher extraction and suppression; hybrid_reading blends both structures. Each carries its own ε, stakeholders, and classification; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
