% ============================================================================
% CONSTRAINT STORY: market_naturalization__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: market_naturalization__hybrid_reading
 *   human_readable: Market Naturalization (Hybrid Reading)
 *   domain: political_economy/economic_history/institutional_analysis
 *
 * SUMMARY:
 *   This constraint describes market dominance as a hybrid phenomenon:
 *   initially established through active suppression of alternatives (e.g.,
 *   mergers, predatory pricing, regulatory capture), which then becomes
 *   partially self-sustaining due to network effects, brand loyalty, and the
 *   atrophy of potential competitors' capabilities. However, some active
 *   maintenance (lobbying, legal defense, strategic acquisitions) is still
 *   required to prevent erosion, making it a Tangled Rope. This is one
 *   reading of the 'market_naturalization' kernel, which explores how market
 *   structures come to be seen as inevitable or 'natural'.
 *
 * KEY AGENTS:
 *   - incumbent_firms: Primary beneficiary (institutional/arbitrage) — benefit from market position, actively maintain some elements.
 *   - capital_holders: Secondary beneficiary (powerful/arbitrage) — profit from incumbent firm success, support maintenance efforts.
 *   - new_entrants: Primary victim (powerless/constrained) — face high barriers, suppressed alternatives.
 *   - consumers: Victim/Beneficiary (organized/constrained) — benefit from some market efficiencies, but pay higher prices due to lack of competition.
 *   - labor: Victim (powerless/constrained) — faces reduced bargaining power due to concentrated employers.
 *   - regulators: Agenda setter/Observer (institutional/analytical) — tasked with ensuring competition, but often subject to capture or resource limitations.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__hybrid_reading, 0.6).
domain_priors:suppression_score(market_naturalization__hybrid_reading, 0.7).
domain_priors:theater_ratio(market_naturalization__hybrid_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(market_naturalization__hybrid_reading, "Market Naturalization (Hybrid Reading)").
narrative_ontology:topic_domain(market_naturalization__hybrid_reading, "political_economy/economic_history/institutional_analysis").

domain_priors:requires_active_enforcement(market_naturalization__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__hybrid_reading, '334e1782-7b26-40c4-9711-d320e3a26b75').
narrative_ontology:cs_kernel_codification('334e1782-7b26-40c4-9711-d320e3a26b75', implicit).
narrative_ontology:cs_authority_grounding('334e1782-7b26-40c4-9711-d320e3a26b75', extraction).
narrative_ontology:cs_interpretation_layer_present('334e1782-7b26-40c4-9711-d320e3a26b75').
narrative_ontology:cs_reading_relation('334e1782-7b26-40c4-9711-d320e3a26b75', market_naturalization__lapsed_alternative_reading, influences).
narrative_ontology:cs_reading_relation('334e1782-7b26-40c4-9711-d320e3a26b75', market_naturalization__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_axiom('334e1782-7b26-40c4-9711-d320e3a26b75', foundational, market_dominance_is_partially_earned_partially_inherited).
narrative_ontology:cs_axiom_status(market_dominance_is_partially_earned_partially_inherited, holdable).
narrative_ontology:cs_axiom_grounding('334e1782-7b26-40c4-9711-d320e3a26b75', market_dominance_is_partially_earned_partially_inherited, empirically_contingent).
narrative_ontology:cs_axiom('334e1782-7b26-40c4-9711-d320e3a26b75', secondary, active_maintenance_and_inertial_effects_coexist).
narrative_ontology:cs_axiom_status(active_maintenance_and_inertial_effects_coexist, holdable).
narrative_ontology:cs_axiom_grounding('334e1782-7b26-40c4-9711-d320e3a26b75', active_maintenance_and_inertial_effects_coexist, empirically_contingent).
narrative_ontology:cs_reference_frame('334e1782-7b26-40c4-9711-d320e3a26b75', dynamic_equilibrium_with_friction).
narrative_ontology:cs_drift_state('334e1782-7b26-40c4-9711-d320e3a26b75', contemporary_regulatory_environment, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('334e1782-7b26-40c4-9711-d320e3a26b75', '').
narrative_ontology:cs_kernel_id(market_naturalization__hybrid_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, incumbent_firms).
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, capital_holders).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, new_entrants).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, consumers).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, labor).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are the dominant players in the market. They benefit from reduced competition, higher profit margins, and the ability to shape market rules. They actively engage in lobbying, strategic acquisitions, and legal defense to maintain their position, while also benefiting from the structural inertia of the market.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, incumbent_firms, beneficiary,
    institutional, generational, arbitrage, global).

% Investors and shareholders in incumbent firms. They benefit from the sustained profitability and market value derived from market dominance. They support the firms' efforts to maintain their position and often influence corporate strategy.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, capital_holders, beneficiary,
    powerful, biographical, arbitrage, global).

% Start-up companies or smaller firms attempting to enter the market. They face significant barriers to entry, including high capital requirements, network effects, regulatory hurdles, and the threat of predatory behavior from incumbents. Their alternatives are limited, often leading to failure or acquisition.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, new_entrants, payer,
    powerless, immediate, constrained, national).

% Individuals who purchase goods and services in the market. They benefit from some efficiencies and product stability offered by dominant firms, but also pay higher prices and have fewer choices due to reduced competition. Their collective power is often diffuse and difficult to mobilize.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, consumers, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(market_naturalization__hybrid_reading, consumers, beneficiary).

% Workers employed by incumbent firms or seeking employment in the market. Concentrated market power can lead to reduced wage growth, fewer job opportunities, and less bargaining power for labor. Their mobility is often limited by specialized skills or geographic constraints.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, labor, payer,
    powerless, biographical, constrained, national).

% Government agencies responsible for enforcing competition law and regulating markets. They have the power to investigate anti-competitive practices, block mergers, and impose remedies, but are often constrained by political pressures, lobbying efforts, and resource limitations. Their actions can significantly alter the constraint's operation.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, regulators, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_naturalization__hybrid_reading, incumbent_firms).
narrative_ontology:fixing_cost_class(market_naturalization__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint coordinates resource allocation and production within the market, providing a stable (albeit concentrated) environment for economic activity and investment, and offering a predictable supply of goods and services to consumers.
% TRANSFER_FUNCTION: Transfers economic surplus (profits, rents) from consumers and new entrants to incumbent firms and their capital holders, through mechanisms like price markups, suppressed wages, and acquisition of potential competitors.
% ABSENT_VOICES: Potential innovators and entrepreneurs who never enter the market due to perceived insurmountable barriers; consumer groups whose advocacy is outmatched by corporate lobbying; and labor organizations whose power is diminished by concentrated employers. These voices would advocate for more open markets, lower prices, and fairer labor practices.
% DISAPPEARANCE_RATIONALE: If the mechanisms sustaining market dominance (both active and inertial) vanished, the market would rapidly reconfigure. New entrants would emerge, prices would likely fall, and the distribution of profits would become more dispersed. Incumbent firms would face intense competition, potentially leading to breakups or significant restructuring. The entire economic landscape would shift.
% FOUNDING_PROBLEM: The founding problem was often framed as achieving efficiency and scale in production and distribution, or protecting intellectual property to incentivize innovation, leading to initial consolidations or regulatory frameworks that favored large players.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent firms and their advocates claim the founding problems (e.g., efficiency, innovation incentives) are still live, justifying their dominance. However, new entrants, consumer advocates, and some economists (outside the benefiting parties) argue that these problems are largely solved, and the current structure primarily serves to extract rents, making the founding problem 'dead' or significantly transformed.
narrative_ontology:disappearance_verdict(market_naturalization__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_naturalization__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__hybrid_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(market_naturalization__hybrid_reading, 'none', 1).

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
 *   The extractiveness (0.6) reflects the ability of dominant firms to set prices above competitive levels and capture a larger share of value. Suppression (0.7) is high due to a combination of active anti-competitive practices and the structural barriers that have accumulated over time. The theater ratio (0.4) indicates that while some activities genuinely contribute to market function (e.g., innovation, service quality), a significant portion is performative maintenance of dominance (e.g., 'innovation' that primarily serves to lock in customers, lobbying efforts framed as 'industry advocacy'). The accessibility collapse (0.65) is moderate, as some alternatives have genuinely atrophied, but others are actively suppressed. Resistance (0.4) is present but fragmented, coming from new entrants, consumer groups, and labor unions, but often insufficient to challenge entrenched power.
 *
 * PERSPECTIVAL GAP:
 *   Incumbent firms and capital holders perceive the market structure as a natural outcome of competition and efficiency, justifying their returns. New entrants, consumers, and labor experience it as an extractive and suppressive force. Regulators often navigate between these perspectives, attempting to balance efficiency with fairness, but are influenced by the powerful incumbent lobby.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent firms and capital holders are clear beneficiaries (d=0.0-0.2) due to their market position and ability to influence policy. New entrants, consumers, and labor are victims (d=0.8-1.0) as they bear the costs of reduced competition and limited alternatives. Regulators, while nominally neutral, can lean towards beneficiaries if captured or under-resourced, or towards victims if actively enforcing anti-trust laws.
 *
 * MANDATROPHY ANALYSIS:
 *   This hybrid reading prevents mislabeling the constraint as a pure Snare (which would imply only active extraction) or a pure Piton (which would imply only inertial, lapsed function). By recognizing both active maintenance and lapsed alternatives, it correctly identifies the ongoing, albeit mixed, nature of extraction and the need for targeted interventions rather than assuming either full naturalness or complete atrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is market dominance primarily a result of actively maintained structures, or the inertia of lapsed alternatives, or a hybrid of both?',
    'Empirical analysis of specific market interventions and their impact on market structure over time. If interventions are consistently required to maintain dominance, it leans towards ''beneficiary_maintained_reading''. If dominance persists without active defense after initial closures, it leans towards ''lapsed_alternative_reading''.',
    'If ''beneficiary_maintained_reading'' is correct, the constraint is more extractive and requires more active enforcement than currently measured. If ''lapsed_alternative_reading'' is correct, the constraint is closer to a Piton, with lower active extraction and suppression. This ''hybrid_reading'' suggests a mix of both, leading to a Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, empirical, 'Ambiguity between active maintenance, lapsed alternatives, and hybrid explanations for market dominance.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of new entrants structural (e.g., regulatory barriers, network effects) or internalized (e.g., perceived futility of competition)?',
    'Surveys of potential new entrants regarding perceived barriers, and analysis of regulatory changes. If perceived barriers persist after structural changes, it suggests internalized suppression.',
    'If internalized, the effective suppression is higher than the structural measure suggests, as potential competitors self-censor. If purely structural, policy interventions can more directly address the barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for new market entrants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__hybrid_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_naturalization__hybrid_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(mark_tr_t10, market_naturalization__hybrid_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(mark_tr_t20, market_naturalization__hybrid_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_naturalization__hybrid_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(mark_be_t10, market_naturalization__hybrid_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(mark_be_t20, market_naturalization__hybrid_reading, base_extractiveness, 20, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_naturalization__hybrid_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(mark_su_t10, market_naturalization__hybrid_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(mark_su_t20, market_naturalization__hybrid_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__hybrid_reading, resource_allocation).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, market_naturalization__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, market_naturalization__lapsed_alternative_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'market_naturalization' kernel. It represents a hybrid view where market dominance is sustained by both active maintenance and the inertia of lapsed alternatives. It influences and is influenced by the 'beneficiary_maintained_reading' (more active) and 'lapsed_alternative_reading' (more inertial) siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
