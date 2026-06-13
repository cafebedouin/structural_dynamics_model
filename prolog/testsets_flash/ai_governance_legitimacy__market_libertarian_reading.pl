% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__market_libertarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_governance_legitimacy__market_libertarian_reading
 *   human_readable: AI Governance Legitimacy: Market Libertarian Reading
 *   domain: theological_ethics/technology_governance/political_theology
 *
 * SUMMARY:
 *   This constraint represents the market-libertarian reading of AI
 *   governance legitimacy, asserting that legitimacy derives from voluntary
 *   exchange and property rights, with innovation flourishing when
 *   unencumbered by collective mandates. Dignity is protected through exit
 *   options and competitive markets, not centralized oversight. This reading
 *   acknowledges the encyclical's subsidiarity principle as supporting
 *   decentralization but rejects its solidarity demands as illegitimate
 *   coercion. It claims property rights as pre-political, making it a
 *   Mountain from its own perspective.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__market_libertarian_reading, 0.25).
domain_priors:suppression_score(ai_governance_legitimacy__market_libertarian_reading, 0.15).
domain_priors:theater_ratio(ai_governance_legitimacy__market_libertarian_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__market_libertarian_reading, mountain).
narrative_ontology:human_readable(ai_governance_legitimacy__market_libertarian_reading, "AI Governance Legitimacy: Market Libertarian Reading").
narrative_ontology:topic_domain(ai_governance_legitimacy__market_libertarian_reading, "theological_ethics/technology_governance/political_theology").

domain_priors:emerges_naturally(ai_governance_legitimacy__market_libertarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__market_libertarian_reading, '39599521-3967-41ad-b87b-db4dc666d4ac').
narrative_ontology:cs_kernel_codification('39599521-3967-41ad-b87b-db4dc666d4ac', implicit).
narrative_ontology:cs_authority_grounding('39599521-3967-41ad-b87b-db4dc666d4ac', self_enforcing).
narrative_ontology:cs_reading_relation('39599521-3967-41ad-b87b-db4dc666d4ac', ai_governance_legitimacy__magisterial_subsidiarity_reading, forecloses).
narrative_ontology:cs_reading_relation('39599521-3967-41ad-b87b-db4dc666d4ac', ai_governance_legitimacy__technocratic_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('39599521-3967-41ad-b87b-db4dc666d4ac', ai_governance_legitimacy__democratic_pluralist_reading, coexists_with).
narrative_ontology:cs_axiom('39599521-3967-41ad-b87b-db4dc666d4ac', foundational, property_rights_are_pre_political).
narrative_ontology:cs_axiom_status(property_rights_are_pre_political, holdable).
narrative_ontology:cs_axiom_grounding('39599521-3967-41ad-b87b-db4dc666d4ac', property_rights_are_pre_political, deontological).
narrative_ontology:cs_axiom('39599521-3967-41ad-b87b-db4dc666d4ac', foundational, voluntary_exchange_is_the_basis_of_legitimacy).
narrative_ontology:cs_axiom_status(voluntary_exchange_is_the_basis_of_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('39599521-3967-41ad-b87b-db4dc666d4ac', voluntary_exchange_is_the_basis_of_legitimacy, deontological).
narrative_ontology:cs_reference_frame('39599521-3967-41ad-b87b-db4dc666d4ac', unencumbered_market_order).
narrative_ontology:cs_drift_state('39599521-3967-41ad-b87b-db4dc666d4ac', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('39599521-3967-41ad-b87b-db4dc666d4ac', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, entrepreneurs).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, investors).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, high_autonomy_individuals).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, those_lacking_market_power).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, communities_facing_coordination_failures).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, workers_in_monopsony_labor_markets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from minimal regulation and strong property rights, allowing them to innovate and commercialize AI technologies with fewer external constraints. They see market competition as the primary driver of progress and consumer benefit.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, entrepreneurs, beneficiary,
    powerful, biographical, mobile, global).

% Seek high returns on capital in AI ventures, favoring environments with low regulatory burdens and clear protections for intellectual property. They view collective mandates as risks to their investments and market efficiency.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, investors, beneficiary,
    institutional, generational, arbitrage, global).

% Value individual freedom and self-determination, preferring to engage with AI technologies through voluntary contracts and market choices. They distrust centralized authority and believe individual dignity is best protected by exit options.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, high_autonomy_individuals, beneficiary,
    moderate, biographical, mobile, global).

% Bear the costs of market failures, such as algorithmic bias, job displacement, or lack of access to essential AI services, without collective recourse. Their dignity is vulnerable to market forces they cannot influence.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, those_lacking_market_power, payer,
    powerless, immediate, trapped, local).

% Struggle to address collective action problems related to AI (e.g., environmental impact, public safety, data privacy) due to the absence of legitimate collective mandates. They experience the 'freedom' of the market as a barrier to shared well-being.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, communities_facing_coordination_failures, payer,
    organized, generational, constrained, regional).

% Are vulnerable to AI-driven automation and wage suppression by dominant employers, with limited bargaining power or exit options. Their 'voluntary' exchange is coerced by economic necessity, and their dignity is undermined by lack of alternatives.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, workers_in_monopsony_labor_markets, payer,
    powerless, biographical, identity_locked, national).

% Represents the Catholic Magisterium, which would advocate for AI governance rooted in common good and solidarity, principles explicitly rejected as 'illegitimate coercion' by this market-libertarian reading. Their voice is dismissed as infringing on economic freedom.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, magisterial_authority, excluded,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates economic activity and innovation in the AI sector by establishing clear property rights, enforcing contracts, and allowing market mechanisms to allocate resources and risks.
% TRANSFER_FUNCTION: Transfers wealth and power to those who own and control AI technologies and capital, while externalizing costs onto those lacking market power or collective organization.
% ABSENT_VOICES: The Magisterial authority and proponents of democratic or technocratic governance models are excluded, as their calls for collective mandates or centralized oversight are deemed illegitimate infringements on market freedom and property rights.
% DISAPPEARANCE_RATIONALE: If the principles of voluntary exchange and property rights vanished, the entire market-based system for AI innovation and deployment would collapse. Investment would cease, contracts would be unenforceable, and the framework for economic activity would fundamentally reorganize.
% FOUNDING_PROBLEM: The problem of inefficient resource allocation, stifled innovation, and infringement on individual liberty caused by centralized planning and collective mandates.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of this reading (entrepreneurs, investors) attest that the problem of over-regulation and state interference is an ongoing threat to innovation and freedom. Critics (those lacking market power, communities) argue that the 'problem' is a cover for unchecked power, but within the market-libertarian framework, the problem is considered live and fundamental.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__market_libertarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__market_libertarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__market_libertarian_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_governance_legitimacy__market_libertarian_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_governance_legitimacy__market_libertarian_reading_tests).

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
 *   The extractiveness is low (0.25) because the framework is presented as a natural order, with costs arising from market dynamics rather than deliberate extraction. Suppression is low (0.15) as it relies on contract law and private enforcement, not overt coercion. Theater ratio is minimal (0.05) as the principles are genuinely believed to be foundational. Accessibility collapse is high (0.88) because the market is seen as the natural and most accessible mechanism for resource allocation and innovation, with alternatives being inherently less efficient or legitimate. Resistance is low (0.1) from within this framework, as market outcomes are considered just.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of entrepreneurs and investors, this framework is a Mountain, representing the natural order of economic activity. For those lacking market power or facing coordination failures, it may compute as a Snare or Tangled Rope, as their 'voluntary' participation is constrained by necessity, and the 'natural' order extracts from them. The engine will compute these divergences based on stakeholder positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Entrepreneurs, investors, and high-autonomy individuals are beneficiaries (d near 0.0) as the constraint enables their activities and protects their gains. Those lacking market power, communities facing coordination failures, and workers in monopsony labor markets are victims (d near 1.0) as they bear the costs of market failures and power imbalances without recourse to collective mandates. The constraint subsidizes the former by legitimizing their market position and extracts from the latter by denying alternative forms of governance.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint, by claiming Mountain status based on pre-political property rights, inherently resists mandatrophy analysis. Its proponents would argue that its mandate is eternal and unchanging, rooted in natural law. However, the presence of identifiable beneficiaries and victims, coupled with the contestation from other readings, suggests that its 'naturalness' is a contested claim, which the FSM detection will highlight.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_market,
    'Is the market-libertarian framework for AI governance a genuine natural law (property rights as pre-political), or a constructed constraint that benefits identifiable agents?',
    'Philosophical analysis of foundational principles of property and contract, and empirical study of market outcomes in the absence of state enforcement.',
    'If genuinely natural law, its classification as Mountain holds. If constructed, it would reclassify as a Rope or Tangled Rope, depending on the degree of extraction and coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_market, conceptual, 'Ambiguity between natural law and constructed market order.').

omega_variable(
    market_libertarian_vs_magisterial_solidarity,
    'This constraint is the market-libertarian reading of the ''ai_governance_legitimacy'' kernel. Would the magisterial_subsidiarity_reading''s emphasis on solidarity and common good logically foreclose this reading''s core premise of unencumbered voluntary exchange?',
    'Analysis of the logical compatibility of absolute property rights and a common good principle that can subordinate economic freedom to collective mandates.',
    'If foreclosed, the two readings cannot coexist within a single coherent framework, highlighting a fundamental philosophical schism. If they can coexist, the conflict is one of preference or emphasis, not logical contradiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_libertarian_vs_magisterial_solidarity, conceptual, 'Logical compatibility of market libertarianism with magisterial solidarity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__market_libertarian_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ai_g_tr_t10, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(ai_g_tr_t20, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(ai_g_tr_t30, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 30, 0.05).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(ai_g_be_t10, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(ai_g_be_t20, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 20, 0.24).
narrative_ontology:measurement(ai_g_be_t30, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 30, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t0, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(ai_g_su_t10, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 10, 0.12).
narrative_ontology:measurement(ai_g_su_t20, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 20, 0.14).
narrative_ontology:measurement(ai_g_su_t30, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 30, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__market_libertarian_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__magisterial_subsidiarity_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__technocratic_optimization_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__democratic_pluralist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'ai_governance_legitimacy' kernel. Each reading represents a distinct structural claim about the source and nature of legitimate AI governance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
