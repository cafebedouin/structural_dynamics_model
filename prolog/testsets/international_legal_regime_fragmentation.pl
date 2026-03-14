% ============================================================================
% CONSTRAINT STORY: international_legal_regime_fragmentation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_international_legal_regime_fragmentation, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: international_legal_regime_fragmentation
 *   human_readable: International Legal Regime Fragmentation
 *   domain: international_law/governance
 *
 * SUMMARY:
 *   International legal regime fragmentation describes the condition where
 *   independent, non-hierarchical legal systems (trade regimes, environmental
 *   treaties, human rights frameworks, maritime law, investment arbitration,
 *   etc.) operate without overarching coordination mechanism. This creates a
 *   structural tension: powerful actors benefit from selective participation
 *   across regimes (forum shopping), while weak actors face conflicting
 *   obligations and asymmetric burdens. The fragmentation is not merely a
 *   coordination failure — it is actively maintained through extraction
 *   mechanisms. Powerful states resist unified legal architecture that would
 *   constrain their autonomy. Multinational corporations benefit from
 *   regulatory arbitrage across jurisdictions. Specialized regime operators
 *   (treaty secretariats, arbitral bodies) develop institutional interests in
 *   maintaining regime independence. Simultaneously, genuine coordination
 *   functions exist — environmental regimes address transboundary problems,
 *   trade law enables cross-border exchange, human rights frameworks
 *   constrain state violence. The constraint thus exhibits the signature of
 *   Tangled Rope: coordination and extraction are not separate functions but
 *   intertwined in the same institutional structure.
 *
 * KEY AGENTS:
 *   - Powerful States (institutional/arbitrage): Primary beneficiaries — can exit unfavorable regimes through withdrawal (US Paris withdrawal, ICC withdrawal threats) without enforcement consequences; capture regime rules through negotiation power
 *   - Multinational Corporations (institutional/arbitrage): Secondary beneficiaries — exploit fragmentation through forum shopping on tax treatment, labor standards, environmental regulation; benefit from Investor-State Dispute Settlement arbitration without parallel accountability mechanisms
 *   - Weak States (powerless/trapped): Primary victims — face conflicting legal obligations, lack capacity for coordinated negotiation, cannot exit regimes without economic/political cost
 *   - Developing States (moderate/constrained): Secondary victims — operate under differentiated but asymmetric responsibilities; constrained by technical capacity and financial barriers
 *   - International Civil Society Networks (organized/mobile): Organized actors building alternative accountability pathways through transnational litigation, corporate campaign networks, treaty monitoring collectives; have structural exit option (can shift to bypass state-centric regimes)
 *   - United Nations System (institutional/arbitrage): Performative coordinator maintaining legitimacy theater while actual authority resides in fragmented specialized regimes
 *   - Analytical Observer (analytical/analytical): Universal-scope position that risks naturalizing contingent institutional arrangements as inherent features of sovereignty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(international_legal_regime_fragmentation, 0.58).
domain_priors:suppression_score(international_legal_regime_fragmentation, 0.62).
domain_priors:theater_ratio(international_legal_regime_fragmentation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(international_legal_regime_fragmentation, extractiveness, 0.58).
narrative_ontology:constraint_metric(international_legal_regime_fragmentation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(international_legal_regime_fragmentation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(international_legal_regime_fragmentation, tangled_rope).
narrative_ontology:human_readable(international_legal_regime_fragmentation, "International Legal Regime Fragmentation").
narrative_ontology:topic_domain(international_legal_regime_fragmentation, "international_law/governance").

domain_priors:requires_active_enforcement(international_legal_regime_fragmentation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(international_legal_regime_fragmentation, powerful_states).
narrative_ontology:constraint_beneficiary(international_legal_regime_fragmentation, multinational_corporations).
narrative_ontology:constraint_beneficiary(international_legal_regime_fragmentation, specialized_regime_operators).
narrative_ontology:constraint_victim(international_legal_regime_fragmentation, weak_states).
narrative_ontology:constraint_victim(international_legal_regime_fragmentation, global_public_goods).
narrative_ontology:constraint_victim(international_legal_regime_fragmentation, marginalized_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WEAK STATE (SNARE) — Cannot exit fragmented regime structure; faces conflicting legal obligations across incompatible systems. Bears extraction through forum shopping by powerful actors, unequal treaty burdens, and inability to coordinate cross-regime compliance. No structural exit option available.
constraint_indexing:constraint_classification(international_legal_regime_fragmentation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING STATE (TANGLED ROPE) — Constrained by high costs of legal coordination and technical capacity requirements. Also benefits from differentiated responsibilities under climate/environmental regimes and technical assistance provisions. Mixed extraction and coordination — state has some agency but operates under asymmetric constraints.
constraint_indexing:constraint_classification(international_legal_regime_fragmentation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: POWERFUL STATE (ROPE) — Experiences fragmentation as beneficial coordination mechanism enabling selective participation. Can arbitrage across regimes, withdrawing from unfavorable commitments (Paris Agreement, International Criminal Court) without enforcement consequences. Net beneficiary — extraction runs toward this agent through institutional flexibility.
constraint_indexing:constraint_classification(international_legal_regime_fragmentation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MULTINATIONAL CORPORATION (ROPE) — Fragmented regimes enable forum shopping and regulatory arbitrage. Can select jurisdiction for legal treatment of labor, environmental, and tax obligations. Coordination across regimes benefits operational flexibility without constraining core business.
constraint_indexing:constraint_classification(international_legal_regime_fragmentation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL CIVIL SOCIETY NETWORK (SCAFFOLD) — Organized actors (NGOs, human rights networks, environmental movements) view fragmentation as a temporary coordination failure with generational sunset. Building alternative verification pathways (treaty monitoring, transnational litigation networks, corporate accountability mechanisms) that bypass state-centric fragmentation. Has exit path and clear sunset trajectory.
constraint_indexing:constraint_classification(international_legal_regime_fragmentation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: UNITED NATIONS SYSTEM (PITON) — Maintains performative coordination role through General Assembly and various bodies despite actual authority residing in fragmented specialized regimes. UN sees its own coordination function as substantially degraded — it coordinates coordination talk rather than substantive compliance. Persists through institutional inertia and legitimacy theater rather than functional necessity.
constraint_indexing:constraint_classification(international_legal_regime_fragmentation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From universal/civilizational scope, legal fragmentation appears as an inherent property of decentralized sovereignty: without a world government, multiple legal orders must coexist. This perspective naturalizes fragmentation as an immutable consequence of Westphalian state system. However, structural data contradicts this — fragmentation is contingent on absence of treaty coordination mechanisms and enforcement institutions, not inherent to sovereignty itself.
constraint_indexing:constraint_classification(international_legal_regime_fragmentation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(international_legal_regime_fragmentation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(international_legal_regime_fragmentation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(international_legal_regime_fragmentation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(international_legal_regime_fragmentation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(international_legal_regime_fragmentation, TR),
    TR >= 0.70.

:- end_tests(international_legal_regime_fragmentation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The fragmentation regime extracts through multiple mechanisms: forum shopping allows powerful actors to escape unfavorable legal obligations; weak states bear disproportionate compliance costs due to lower enforcement against powerful actors; technical barriers to regime participation concentrate benefits among institutional actors with legal capacity. However, extraction is not total because genuine coordination functions exist and some regimes (environmental, human rights) do constrain even powerful actors. Suppression (0.62): Moderate-high. Significant barriers to unified regime include: structural absence of enforcement authority above state system; material barriers to regime coordination (technical capacity, legal expertise, resource constraints); ideological barriers (sovereignty norms, national interest framing); institutional interests resisting unification. However, suppression is not total because some states actively participate and some cross-regime coordination mechanisms exist. Theater ratio (0.68): High and increasing. UN coordination bodies perform coordination theater while actual authority remains fragmented. Specialized regime secretariats maintain legitimacy narratives about universal coverage while jurisdiction gaps persist. Over the interval (45-year span), theater has increased from 0.52 to 0.68 as the gap has widened between coordination talk and enforcement reality. Extractiveness has increased from 0.42 to 0.58 as forum shopping tactics have become more sophisticated and regime interdependencies have created new arbitrage opportunities.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reflects a fundamental structural asymmetry: the same institutional arrangement that coordinates beneficial exchange (trade enabling growth) simultaneously enables extraction (forum shopping avoiding regulation). The beneficiary perspectives (powerful state, corporation, UN performative) experience this as functional coordination. The victim perspectives (weak state, developing state, global public goods) experience this as systematic extraction. The civil society perspective (scaffold) sees the fragmentation as transitional — the gap is expected to close as alternative accountability networks mature. The analytical observer (mountain) risks naturalizing this asymmetry as inherent to international relations.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is determined by structural position: beneficiary status with arbitrage options produces low d (powerful states, multinational corporations experience low/negative effective extraction); victim status with trapped options produces high d (weak states experience high extraction); constrained exit with mixed benefits produces moderate d (developing states); organized actors with exit paths produce moderate d despite victim status (civil society has capacity and alternatives). The powerful state's d ≈ 0.10 (beneficiary + arbitrage) → low f(d) → low χ despite moderate base extractiveness. The weak state's d ≈ 0.92 (victim + trapped) → high f(d) → high χ. The developing state's d ≈ 0.55 (victim + constrained) → moderate f(d) → moderate χ. Civil society's d ≈ 0.45 (victim + mobile, but organized) → moderate f(d). The piton classification derives from high theater ratio (0.68) rather than from extraction intensity.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by showing that fragmentation is neither pure coordination (rope) nor pure extraction (snare), but a genuine hybrid (tangled rope) in which the same institutional structure serves both functions. The coordination function is real — international legal regimes solve genuine problems (environmental spillovers, trade dispute resolution, human rights norm-setting). The extraction function is also real — fragmentation enables forum shopping, asymmetric burden distribution, and evasion of binding constraints. The temptation to misclassify arises because the coordination function is visible and normatively valued (law-and-order, peaceful dispute resolution, ecological protection), while the extraction function is obscured by institutional legitimacy narratives. The tangled rope classification captures both: beneficiaries (powerful states, corporations) genuinely benefit from coordination mechanisms while simultaneously extracting through selective participation. Victims (weak states, marginalized populations) bear both the coordination costs (treaty obligations, compliance burdens) and the extraction costs (forum shopping disadvantage, asymmetric enforcement). The false summit (mountain classification from the analytical observer) naturalizes fragmentation as an immutable feature of sovereign decentralization, obscuring that specialized regime creation is a choice made by powerful actors and that alternative unified architectures are institutionally feasible (though politically opposed by beneficiaries).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regime_incompatibility_definition,
    'What degree of doctrinal inconsistency constitutes genuine fragmentation vs. acceptable legal pluralism?',
    'Systematic cross-regime analysis of conflicting obligations (e.g., trade law vs. environmental law, investment arbitration vs. human rights); empirical measurement of compliance impossibility rate across regimes for specific state classes',
    'If threshold is low (any conflict = fragmentation): fragmentation appears endemic and systematic. If threshold is high (only irreconcilable conflicts count): fragmentation appears manageable. Classification sensitivity across all perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_incompatibility_definition, conceptual, 'Definition threshold for regime incompatibility').

omega_variable(
    forum_shopping_causality,
    'Does fragmentation enable forum shopping, or does strategic forum shopping drive regime creation and fragmentation?',
    'Historical case analysis of regime creation timeline vs. forum shopping incidents; counterfactual: would unified regime prevent strategic behavior or merely compress it into uniform evasion mechanisms?',
    'If fragmentation enables shopping: extraction mechanism is regime choice arbitrage. If shopping drives fragmentation: fragmentation is consequence of distribution conflict, not cause of extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(forum_shopping_causality, empirical, 'Causal direction: does fragmentation enable or result from forum shopping?').

omega_variable(
    weak_state_coalition_feasibility,
    'Can powerless states form effective coalitions within fragmented regime structure to increase bargaining power, or does fragmentation prevent coalition formation?',
    'Analysis of historical coalition successes (AOSIS, LDC bloc); identification of regime-specific coordination barriers; measurement of coalition voice effectiveness within vs. across regimes',
    'If coalitions viable: snare classification may be downgraded to constrained (agent has some organizing capacity). If fragmentation prevents coalitions: snare is confirmed — powerless trapped status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(weak_state_coalition_feasibility, empirical, 'Whether coalition formation is feasible within fragmented structure').

omega_variable(
    enforcement_institution_scalability,
    'Could a unified global legal regime''s enforcement institutions scale to manage the volume and complexity currently distributed across specialized regimes?',
    'Institutional capacity analysis of hypothetical unified regime; comparison with existing specialized regime enforcement models; technical assessment of information processing requirements',
    'If scalable: mountain classification is false; unified regime is institutionally feasible. If not scalable: fragmentation may be functional necessity rather than extractive arrangement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_institution_scalability, conceptual, 'Scalability of unified regime enforcement').

omega_variable(
    civil_society_bypass_effectiveness,
    'Do transnational litigation networks and corporate accountability mechanisms actually provide verification and enforcement comparable to state-centric regimes?',
    'Comparison of compliance rates: corporate accountability campaigns vs. state treaty enforcement; measurement of deterrence effects from investor lawsuits vs. diplomatic pressure; longitudinal tracking of NGO-driven compliance improvements',
    'If effective: scaffold perspective is realistic — civil society networks provide genuine alternative pathway with sunset. If ineffective: scaffold is aspirational rather than structural; fragmentation persists indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_society_bypass_effectiveness, empirical, 'Whether civil society mechanisms provide functional alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(international_legal_regime_fragmentation, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ilrf_tr_t0, international_legal_regime_fragmentation, theater_ratio, 0, 0.52).
narrative_ontology:measurement(ilrf_tr_t15, international_legal_regime_fragmentation, theater_ratio, 15, 0.6).
narrative_ontology:measurement(ilrf_tr_t30, international_legal_regime_fragmentation, theater_ratio, 30, 0.68).
narrative_ontology:measurement(ilrf_tr_t45, international_legal_regime_fragmentation, theater_ratio, 45, 0.72).

% Extraction over time
narrative_ontology:measurement(ilrf_be_t0, international_legal_regime_fragmentation, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ilrf_be_t15, international_legal_regime_fragmentation, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(ilrf_be_t30, international_legal_regime_fragmentation, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(ilrf_be_t45, international_legal_regime_fragmentation, base_extractiveness, 45, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(international_legal_regime_fragmentation, enforcement_mechanism).
narrative_ontology:affects_constraint(international_legal_regime_fragmentation, investment_arbitration_regime).
narrative_ontology:affects_constraint(international_legal_regime_fragmentation, climate_agreement_enforcement).
narrative_ontology:affects_constraint(international_legal_regime_fragmentation, international_tax_competition).
narrative_ontology:affects_constraint(international_legal_regime_fragmentation, human_rights_jurisdiction_conflict).

% DUAL FORMULATION NOTE:
% International legal regime fragmentation should be decomposed into domain-specific constraints for precision: trade regime fragmentation, environmental regime fragmentation, human rights regime fragmentation, etc. Each has different ε values and different coordination/extraction balances. This story represents the meta-level fragmentation structure affecting all specialized regimes. Downstream constraints measure domain-specific instantiations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(international_legal_regime_fragmentation, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
