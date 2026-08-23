% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__market_libertarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_governance_legitimacy__market_libertarian_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: ai_governance_legitimacy__market_libertarian_reading
 *   human_readable: Market-Libertarian AI Governance Legitimacy (Voluntary Exchange Reading)
 *   domain: theological_ethics/technology_governance/political_theology
 *
 * SUMMARY:
 *   This constraint story captures the market-libertarian reading of AI
 *   governance legitimacy: the claim that legitimate authority over AI
 *   derives exclusively from voluntary exchange and pre-political property
 *   rights. The reading presents itself as a mountain — a natural law
 *   constraint that would persist regardless of human enforcement —
 *   identifying property rights as anterior to political order. Beneficiaries
 *   are entrepreneurs, investors, and high-autonomy individuals who capture
 *   returns under this regime. Victims are the market-powerless, communities
 *   facing coordination failures that voluntary exchange cannot solve, and
 *   workers in monopsony labor markets where 'exit' is theoretical. The
 *   reading explicitly rejects the Catholic encyclical's solidarity demands
 *   as illegitimate coercion while accepting its subsidiarity principle as
 *   supporting decentralization. Enforcement operates through contract law,
 *   private arbitration, and reputational mechanisms — decentralized but
 *   state-backed. The claimed mountain type diverges from the metric profile
 *   (beneficiaries + victims + rising extraction), making this a false-summit
 *   candidate.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__market_libertarian_reading, 0.25).
domain_priors:suppression_score(ai_governance_legitimacy__market_libertarian_reading, 0.35).
domain_priors:theater_ratio(ai_governance_legitimacy__market_libertarian_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__market_libertarian_reading, mountain).
narrative_ontology:human_readable(ai_governance_legitimacy__market_libertarian_reading, "Market-Libertarian AI Governance Legitimacy (Voluntary Exchange Reading)").
narrative_ontology:topic_domain(ai_governance_legitimacy__market_libertarian_reading, "theological_ethics/technology_governance/political_theology").

domain_priors:emerges_naturally(ai_governance_legitimacy__market_libertarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__market_libertarian_reading, '393c5308-d4e3-4088-8469-f175a86b768a').
narrative_ontology:cs_kernel_codification('393c5308-d4e3-4088-8469-f175a86b768a', distributed).
narrative_ontology:cs_authority_grounding('393c5308-d4e3-4088-8469-f175a86b768a', expertise).
narrative_ontology:cs_reading_relation('393c5308-d4e3-4088-8469-f175a86b768a', ai_governance_legitimacy__magisterial_subsidiarity_reading, coexists_with).
narrative_ontology:cs_reading_relation('393c5308-d4e3-4088-8469-f175a86b768a', ai_governance_legitimacy__technocratic_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('393c5308-d4e3-4088-8469-f175a86b768a', ai_governance_legitimacy__democratic_pluralist_reading, coexists_with).
narrative_ontology:cs_axiom('393c5308-d4e3-4088-8469-f175a86b768a', foundational, property_rights_are_prepolitical).
narrative_ontology:cs_axiom_status(property_rights_are_prepolitical, holdable).
narrative_ontology:cs_axiom_grounding('393c5308-d4e3-4088-8469-f175a86b768a', property_rights_are_prepolitical, deontological).
narrative_ontology:cs_axiom('393c5308-d4e3-4088-8469-f175a86b768a', foundational, voluntary_exchange_maximizes_dignity).
narrative_ontology:cs_axiom_status(voluntary_exchange_maximizes_dignity, holdable).
narrative_ontology:cs_axiom_grounding('393c5308-d4e3-4088-8469-f175a86b768a', voluntary_exchange_maximizes_dignity, instrumental).
narrative_ontology:cs_reference_frame('393c5308-d4e3-4088-8469-f175a86b768a', natural_rights_framework).
narrative_ontology:cs_drift_state('393c5308-d4e3-4088-8469-f175a86b768a', contemporary_ai_governance_debate, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('393c5308-d4e3-4088-8469-f175a86b768a', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, entrepreneurs).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, investors).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, high_autonomy_individuals).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, market_powerless).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, coordination_failure_communities).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, monopsony_workers).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, property_rights_are_prepolitical).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, voluntary_exchange_maximizes_human_dignity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build and deploy AI systems under a property-rights regime that treats their models, data, and innovations as exclusive assets. They capture returns from voluntary exchange with users and investors. Exit means moving to jurisdictions with stronger property protections or pivoting to closed-source models.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, entrepreneurs, beneficiary,
    organized, biographical, mobile, global).

% Allocate capital to AI ventures expecting returns protected by contract law and intellectual property enforcement. They benefit from the predictability of a regime where governance legitimacy flows from voluntary transactions. Exit means capital reallocation across jurisdictions and asset classes.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Technically skilled individuals who navigate AI labor markets as independent contractors or founders. They treat their labor and data as property to be contracted voluntarily. Exit means switching platforms, clients, or jurisdictions — feasible due to high demand for their skills.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, high_autonomy_individuals, beneficiary,
    moderate, biographical, mobile, global).

% Users and small-scale participants who lack bargaining power in AI-mediated markets. They face take-it-or-leave-it terms of service, data extraction without meaningful consent, and algorithmic pricing they cannot contest. Exit is theoretically available but practically foreclosed by network effects and necessity.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, market_powerless, payer,
    powerless, immediate, trapped, global).

% Communities facing AI-driven coordination problems — algorithmic discrimination in hiring/lending, platform monopolies controlling public discourse, environmental externalities of compute infrastructure — that voluntary exchange cannot resolve. They bear costs of ungoverned externalities. Exit means collective political action, which this reading treats as illegitimate coercion.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, coordination_failure_communities, payer,
    moderate, generational, constrained, regional).

% Workers in labor markets where a single AI-driven platform or employer dominates (gig platforms, warehouse automation, algorithmic management). They face depressed wages, surveillance, and unilateral contract changes. The reading's 'exit option' rhetoric ignores structural monopsony. Exit means accepting worse conditions elsewhere or leaving the workforce.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, monopsony_workers, payer,
    powerless, biographical, constrained, national).

% The Catholic Magisterium claiming authoritative interpretation of subsidiarity and solidarity for AI governance. This reading explicitly rejects their claim that economic freedom must be subordinated to the common good as politically defined. They are excluded from the legitimacy conversation this reading constructs.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, magisterial_authority, excluded,
    institutional, civilizational, analytical, global).

% Legislatures, regulators, and civil society organizations pursuing AI governance through democratic deliberation and consent. This reading treats democratic mandates as illegitimate coercion when they constrain voluntary exchange. They are excluded from the reading's legitimacy framework.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, democratic_actors, excluded,
    institutional, generational, analytical, national).

% Technical experts and policy elites who ground AI governance legitimacy in welfare optimization and demonstrated performance. This reading treats their authority as secondary to property rights. They are excluded from the reading's legitimacy framework.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, technocratic_experts, excluded,
    organized, biographical, analytical, global).

% Sees the full structure: a legitimacy claim that presents itself as natural law (pre-political property rights) while functionally allocating governance authority to market-powerful actors. Observes the beneficiary/victim asymmetry and the exclusion of competing legitimacy sources.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates AI development and deployment through voluntary contracting and property rights enforcement, avoiding the collective action problems and political capture risks of centralized governance. Solves the problem of who decides by defaulting to bilateral exchange.
% TRANSFER_FUNCTION: Moves decision-making authority over AI systems and the economic returns from AI deployment from collective/political processes to individual property holders and voluntary contractors. Transfers governance costs to those lacking market power (who bear externalities) and transfers legitimacy claims away from democratic, magisterial, and technocratic sources.
% ABSENT_VOICES: Communities experiencing algorithmic harm without market recourse; workers in monopsony labor markets who cannot individually negotiate; future generations bearing long-term AI risks; the global poor excluded from AI benefit streams. They are structurally excluded because this reading defines legitimacy exclusively through voluntary exchange, which they cannot access on equal terms.
% DISAPPEARANCE_RATIONALE: If the market-libertarian legitimacy framework vanished overnight, AI governance would reorganize around democratic regulation (EU AI Act model), magisterial/ethical frameworks (Catholic social teaching), or technocratic standards (IEEE, ISO). Property rights would remain but would be subordinated to collective mandates. The current allocation of authority to private contractors would be contested and partially reversed.
% FOUNDING_PROBLEM: The problem of legitimate authority over AI development without centralized coercion: how to govern a transformative general-purpose technology without violating property rights, voluntary exchange, and the presumption of liberty that treats individuals as self-owning agents.
% FOUNDING_PROBLEM_CORROBORATION: Classical liberal tradition (Locke, Nozick, Hayek, Friedman) attests the founding problem is live — property rights remain the only non-arbitrary basis for governance. Political philosophers (Rawls, Sen, Catholic social theorists) and democratic theorists attest the problem is substantially solved by democratic institutions that legitimately constrain property for common good. No neutral arbiter exists; the corroboration split maps exactly onto the kernel's sibling readings.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__market_libertarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__market_libertarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__market_libertarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_governance_legitimacy__market_libertarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__market_libertarian_reading, 0.25, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_governance_legitimacy__market_libertarian_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, ExtMetricName, E),
    domain_priors:suppression_score(ai_governance_legitimacy__market_libertarian_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ai_governance_legitimacy__market_libertarian_reading),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ai_governance_legitimacy__market_libertarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low but rising (0.15→0.25) as the property-rights regime expands into AI domains where market power concentrates (foundation models, data monopolies, algorithmic management). Suppression is moderate (0.35) — the constraint doesn't actively crush alternatives but structurally excludes collective governance by defining it as illegitimate. Theater ratio rises (0.10→0.30) as 'voluntary exchange' rhetoric increasingly covers power asymmetries (take-it-or-leave-it terms, monopsony labor markets). Accessibility collapse is high (0.70) — once you accept the premise that property rights are pre-political, collective alternatives appear as rights violations. Resistance is moderate (0.40) — democratic, magisterial, and technocratic alternatives actively contest this reading but haven't displaced it in key jurisdictions.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats, this is a genuine mountain — property rights protect innovation and dignity against political arbitrariness. From the victim seats, it operates as a tangled rope — a coordination mechanism (voluntary exchange) that extracts via monopsony power and coordination-failure externalities. From the excluded authority seats, it's a snare — a legitimacy claim that suppresses competing frameworks by definitional fiat. The engine computes this divergence; the authored claim (mountain) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (entrepreneurs, investors, high-autonomy individuals) have mobile/arbitrage exit and organized/powerful power — directionality near 0.0 (full beneficiary). Victims (market-powerless, coordination-failure communities, monopsony workers) have trapped/constrained exit and powerless/moderate power — directionality near 1.0 (full target). Excluded authorities (magisterial, democratic, technocratic) have analytical exit and institutional power — directionality ~0.5 (symmetric: they contest the constraint but aren't extracted from). The engine will compute per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legitimate authority without coercion) was live in 17th-18th century state-formation contexts. In 21st century AI governance, the problem is contested: libertarians argue it remains live (state capacity for AI regulation is low; capture risk high); democrats and magisterial actors argue it's dead (democratic institutions exist and legitimately govern). The reading persists not because the founding problem is solved but because beneficiaries have power to maintain it — a mandatrophy indicator. However, the reading itself denies mandatrophy by treating the problem as perennial natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_property_rights,
    'Are property rights in AI governance (model ownership, data control, algorithmic IP) genuine pre-political natural laws, or are they constructed legal fictions maintained by state enforcement?',
    'Historical-institutional analysis of how AI property rights emerged (legislative acts, judicial decisions, treaty regimes) vs. natural law philosophical demonstration. Cross-jurisdictional comparison: if property rights regimes vary systematically with political power, they are constructed.',
    'If constructed, the mountain claim collapses — the constraint becomes a tangled rope (coordination + extraction) or snare. The FSM signature would trigger reclassification. If natural law, the mountain claim holds and beneficiaries/victims reflect natural inequality, not structural extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_property_rights, conceptual, 'Whether the mountain''s ''emerges_naturally'' claim survives genealogical scrutiny.').

omega_variable(
    exit_options_reality_for_vulnerable,
    'Do the exit options this reading cites (switching platforms, jurisdictions, employers) actually exist for market-powerless, monopsony workers, and coordination-failure communities?',
    'Empirical measurement of switching costs, geographic mobility, and bargaining power in AI-mediated markets. Comparison of formal exit rights vs. effective exit capacity.',
    'If exit is largely theoretical for victims, their directionality shifts toward 1.0 (full target), raising effective extraction and making the constraint extractive in practice regardless of natural-law claim. Supports FSM reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_options_reality_for_vulnerable, empirical, 'Whether the reading''s core dignity-protection mechanism (exit) is real or rhetorical for vulnerable parties.').

omega_variable(
    coordination_extraction_boundary_in_markets,
    'Is the coordination function (voluntary exchange solving AI governance) structurally separable from the extraction function (monopsony rents, data appropriation, externalized coordination failures)?',
    'Counterfactual analysis: if property rights were enforced without permitting monopsony concentration (e.g., via antitrust, data portability, interoperability mandates), would coordination benefits persist while extraction falls? Natural experiments in jurisdictions with stronger platform regulation.',
    'If inseparable, the constraint is inherently a tangled rope — coordination and extraction are the same structure. If separable, the extraction is contingent policy failure, not structural feature. Determines whether FSM reclassification captures essence or accident.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary_in_markets, conceptual, 'Whether the mountain''s coordination story is a separable function or a cover for extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__market_libertarian_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_gov_lib_market_lib_tr_t0, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ai_gov_lib_market_lib_tr_t3, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 3, 0.15).
narrative_ontology:measurement(ai_gov_lib_market_lib_tr_t6, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 6, 0.2).
narrative_ontology:measurement(ai_gov_lib_market_lib_tr_t9, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 9, 0.25).
narrative_ontology:measurement(ai_gov_lib_market_lib_tr_t12, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement(ai_gov_lib_market_lib_tr_t15, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 15, 0.3).

% Extraction over time
narrative_ontology:measurement(ai_gov_lib_market_lib_be_t0, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(ai_gov_lib_market_lib_be_t3, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 3, 0.18).
narrative_ontology:measurement(ai_gov_lib_market_lib_be_t6, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 6, 0.21).
narrative_ontology:measurement(ai_gov_lib_market_lib_be_t9, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 9, 0.23).
narrative_ontology:measurement(ai_gov_lib_market_lib_be_t12, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 12, 0.24).
narrative_ontology:measurement(ai_gov_lib_market_lib_be_t15, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 15, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(ai_gov_lib_market_lib_su_t0, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(ai_gov_lib_market_lib_su_t3, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 3, 0.25).
narrative_ontology:measurement(ai_gov_lib_market_lib_su_t6, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 6, 0.3).
narrative_ontology:measurement(ai_gov_lib_market_lib_su_t9, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 9, 0.32).
narrative_ontology:measurement(ai_gov_lib_market_lib_su_t12, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 12, 0.34).
narrative_ontology:measurement(ai_gov_lib_market_lib_su_t15, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 15, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__market_libertarian_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(ai_governance_legitimacy__market_libertarian_reading, 0.15).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__magisterial_subsidiarity_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__technocratic_optimization_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__democratic_pluralist_reading).

% DUAL FORMULATION NOTE:
% This constraint and its three siblings form the ai_governance_legitimacy kernel family. Each reading instantiates a different constraint with distinct ε, beneficiary/victim structures, and claimed types. This reading claims mountain (ε~0.25); magisterial claims scaffold/tangled_rope (ε~0.40, sunset via synodality); technocratic claims rope (ε~0.15, coordination via expertise); democratic claims tangled_rope (ε~0.35, coordination via deliberation). The ε-invariance principle requires separate stories because the legitimacy referent changes with the reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
