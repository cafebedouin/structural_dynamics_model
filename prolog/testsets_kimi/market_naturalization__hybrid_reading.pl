% ============================================================================
% CONSTRAINT STORY: market_naturalization__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   human_readable: Market Dominance Hybrid Reading (Lapsed + Active Maintenance)
 *   domain: political economy
 *
 * SUMMARY:
 *   This constraint instantiates the hybrid reading of the market
 *   naturalization kernel: the claim that market dominance in contemporary
 *   political economy is sustained neither purely by active incumbent
 *   maintenance nor purely by lapsed institutional closure, but by a
 *   combination of both. Dominant firms actively suppress entry and harvest
 *   rents, while simultaneously benefiting from inherited regulatory
 *   frameworks, naturalized network effects, and institutional inertia that
 *   require no ongoing investment to maintain. The constraint coordinates
 *   real resource allocation through scale while extracting asymmetrically
 *   from consumers and excluded competitors.
 *
 * KEY AGENTS:
 *   - dominant_incumbents: agenda_setter/beneficiary (institutional/arbitrage) â actively maintains dominance and passively collects lapsed rents
 *   - consumer_citizens: payer (organized/constrained) â bears supracompetitive prices
 *   - excluded_competitors: payer (moderate/constrained) â blocked by mixed barriers
 *   - competition_authorities: observer (institutional/analytical) â sporadic intervention
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__hybrid_reading, 0.55).
domain_priors:suppression_score(market_naturalization__hybrid_reading, 0.6).
domain_priors:theater_ratio(market_naturalization__hybrid_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(market_naturalization__hybrid_reading, "Market Dominance Hybrid Reading (Lapsed + Active Maintenance)").
narrative_ontology:topic_domain(market_naturalization__hybrid_reading, "political economy").

domain_priors:requires_active_enforcement(market_naturalization__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__hybrid_reading, 'ce07428e-efad-41be-b777-d1614ef7815a').
narrative_ontology:cs_kernel_codification('ce07428e-efad-41be-b777-d1614ef7815a', implicit).
narrative_ontology:cs_authority_grounding('ce07428e-efad-41be-b777-d1614ef7815a', distributed).
narrative_ontology:cs_reading_relation('ce07428e-efad-41be-b777-d1614ef7815a', market_naturalization__lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_reading_relation('ce07428e-efad-41be-b777-d1614ef7815a', market_naturalization__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_axiom('ce07428e-efad-41be-b777-d1614ef7815a', foundational, market_outcomes_are_hybrid_constructions).
narrative_ontology:cs_axiom_status(market_outcomes_are_hybrid_constructions, holdable).
narrative_ontology:cs_axiom_grounding('ce07428e-efad-41be-b777-d1614ef7815a', market_outcomes_are_hybrid_constructions, empirically_contingent).
narrative_ontology:cs_axiom('ce07428e-efad-41be-b777-d1614ef7815a', foundational, dominance_requires_active_and_inertial_maintenance).
narrative_ontology:cs_axiom_status(dominance_requires_active_and_inertial_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('ce07428e-efad-41be-b777-d1614ef7815a', dominance_requires_active_and_inertial_maintenance, empirically_contingent).
narrative_ontology:cs_reference_frame('ce07428e-efad-41be-b777-d1614ef7815a', mixed_maintenance_equilibrium).
narrative_ontology:cs_drift_state('ce07428e-efad-41be-b777-d1614ef7815a', contemporary_crisis_of_competition, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ce07428e-efad-41be-b777-d1614ef7815a', '').
narrative_ontology:cs_kernel_id(market_naturalization__hybrid_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, dominant_incumbents).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, consumer_citizens).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, excluded_competitors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control large market shares and actively maintain dominance through lobbying, litigation, platform control, and barrier-building while also passively collecting rents from lapsed institutional structures like inherited standards, regulatory capture, and naturalized network effects.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, dominant_incumbents, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(market_naturalization__hybrid_reading, dominant_incumbents, beneficiary).

% Purchase goods and services in dominated markets, paying prices above competitive levels. Face limited alternatives due to both lapsed infrastructural closure and active exclusion of competitors. Exit is constrained by necessity and lock-in.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, consumer_citizens, payer,
    organized, biographical, constrained, national).

% Would enter dominated markets but face barriers that are partly inherited from lapsed regulatory frameworks and sunk-cost advantages, and partly actively enforced through predatory pricing, patent thickets, and platform self-preferencing.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, excluded_competitors, payer,
    moderate, biographical, constrained, national).

% Monitor market concentration and occasionally challenge dominant firms. Their interventions are sporadic and vary by jurisdiction; they observe the hybrid structure but rarely dismantle both the lapsed and active components simultaneously.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, competition_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_naturalization__hybrid_reading, dominant_incumbents).
narrative_ontology:fixing_cost_class(market_naturalization__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Markets coordinate resource allocation across complex economies; this constraint governs the specific form where dominance emerges, ostensibly solving coordination problems through scale, standardization, and network effects.
% TRANSFER_FUNCTION: Moves consumer surplus and competitor producer surplus to dominant incumbents through supracompetitive pricing and foregone entry; part of this transfer is actively enforced by incumbents, part is harvested from lapsed institutional closure that no party currently maintains.
% ABSENT_VOICES: Potential competitors who never attempt entry because dominance appears natural or inevitable; consumer advocates who would challenge the efficiency defense of concentration; heterodox economists who treat dominance as constructed rather than emergent.
% DISAPPEARANCE_RATIONALE: If market dominance dissolved overnight, prices would fall toward competitive levels, entry would surge in previously blocked sectors, institutional frameworks would face radical reform, and the current distribution of surplus would invert.
% FOUNDING_PROBLEM: Capital-intensive and network industries required coordination at scale to achieve efficiency, investment certainty, and standardization where fragmented markets failed.
% FOUNDING_PROBLEM_CORROBORATION: Business historians and institutional economists attest to original coordination failures in rail, telecom, and early platforms. Competition authorities and heterodox political economists attest that current dominance substantially exceeds the scale required for coordination; corroboration is split across seats with no outside unanimous consensus.
narrative_ontology:disappearance_verdict(market_naturalization__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_naturalization__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__hybrid_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(market_naturalization__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__hybrid_reading, 0.55, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is moderate (0.55) because dominance captures substantial surplus but still delivers genuine coordination through scale and standardization. Suppression is moderate-high (0.60) reflecting the mix of lapsed barriers and active enforcement. Theater ratio is moderate (0.45) because incumbents increasingly perform legitimacy as lapsed elements erode and active maintenance becomes more visible. Accessibility collapse is substantial (0.65) because decades of dominance have collapsed alternatives. Resistance is moderate (0.50) reflecting cyclical antitrust movements. The measurement series share one time grid.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the constraint as necessary coordination and legitimate reward for investment; the payer seats experience it as closure that is partly invisible and partly actively hostile. The engine will compute divergent per-seat types from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Dominant incumbents are structural beneficiaries with low directionality for the lapsed components and moderate-low for the active components. Consumer citizens and excluded competitors are targets with high directionality. The hybrid structure creates a bimodal extraction profile: incumbents collect from both inertial and active channels, while payers experience uniform extraction they cannot easily distinguish into natural versus enforced components.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading prevents mislabeling by distinguishing coordination (genuine scale economies) from extraction (rents above coordination cost) while acknowledging that the same institutional structure carries both. Without this decomposition, the constraint could be misread as pure snare (ignoring lapsed coordination benefits) or pure rope (ignoring active suppression). The founding problem status is contested because the original coordination justification is live for some sectors and dead for others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lapsed_active_boundary,
    'What proportion of current market dominance is sustained by lapsed institutional inertia versus active incumbent maintenance?',
    'Cross-sector comparison of entry barriers, lobbying expenditure, and regulatory defense costs; sectors with low active maintenance but high dominance indicate lapsed closure.',
    'If dominance is mostly lapsed, the constraint trends toward piton; if mostly active, toward snare. The hybrid reading''s validity depends on this balance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lapsed_active_boundary, empirical, 'Boundary between inertial and actively maintained dominance').

omega_variable(
    naturalization_kernel_ambiguity,
    'Is market dominance a natural emergent property of efficient markets or a constructed closure sustained by interpretive tradition?',
    'Historical analysis of counterfactual policy choices and regulatory moments where dominance could have been prevented or dismantled but was not.',
    'Resolving toward natural law would reclassify toward mountain; resolving toward constructed closure would confirm tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalization_kernel_ambiguity, conceptual, 'Ambiguity of the market naturalization kernel').

omega_variable(
    suppression_mechanism_mix,
    'Are observed barriers to entry structural economic features or actively suppressed alternatives?',
    'Policy experiments and regulatory interventions that lower specific barriers; if entry surges, barriers were at least partly suppressed rather than naturally occurring.',
    'Distinguishes inertial extraction from actively enforced extraction within the hybrid structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_mix, empirical, 'Structural versus active suppression in entry barriers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_naturalization__hybrid_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(mark_tr_t10, market_naturalization__hybrid_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(mark_tr_t20, market_naturalization__hybrid_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(mark_tr_t30, market_naturalization__hybrid_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(mark_tr_t40, market_naturalization__hybrid_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_naturalization__hybrid_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(mark_be_t10, market_naturalization__hybrid_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(mark_be_t20, market_naturalization__hybrid_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(mark_be_t30, market_naturalization__hybrid_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(mark_be_t40, market_naturalization__hybrid_reading, base_extractiveness, 40, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_naturalization__hybrid_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(mark_su_t10, market_naturalization__hybrid_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(mark_su_t20, market_naturalization__hybrid_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(mark_su_t30, market_naturalization__hybrid_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(mark_su_t40, market_naturalization__hybrid_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, beneficiary_maintained_reading).

% DUAL FORMULATION NOTE:
% This reading decomposes the market naturalization kernel into a hybrid structure combining lapsed institutional inertia with active incumbent maintenance, distinct from pure lapsed or pure beneficiary-maintained readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
