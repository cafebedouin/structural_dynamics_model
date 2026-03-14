% ============================================================================
% CONSTRAINT STORY: central_american_sovereignty_constraints
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_central_american_sovereignty_constraints, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: central_american_sovereignty_constraints
 *   human_readable: Central American Sovereignty Constraints: Structural Extraction Within Regional Coordination
 *   domain: geopolitical/economic/institutional
 *
 * SUMMARY:
 *   Central American sovereignty constraints operate as a tangled hybrid of
 *   coordination and extraction, spanning geopolitical, economic, and
 *   institutional domains. The constraint manifests through multiple
 *   mechanisms: debt servicing obligations that consume fiscal capacity,
 *   structural adjustment conditionality that mandates privatization and
 *   market liberalization, military and security dependence on US power
 *   projection, trade agreements (CAFTA) that lock in market access on
 *   asymmetric terms, and institutional inheritance of colonial-era land
 *   concentration and oligarchic power structures. The constraint exhibits
 *   all six classification types from different perspectives, reflecting
 *   genuine structural ambiguity about whether the arrangement constitutes
 *   necessary regional coordination or pure extractive imperialism. The
 *   theater ratio (0.65) reflects the performative dimension: formal
 *   democratic institutions, property rights systems, and international law
 *   exist while substantive decision-making power over monetary policy,
 *   military strategy, trade terms, and resource extraction remains
 *   externally controlled or concentrated in domestic oligarchies. The
 *   extractiveness has increased from 0.35 (immediate post-independence era,
 *   limited integration) to 0.58 (current period, deep institutional
 *   embedding), indicating accumulating extraction alongside coordination.
 *   Key tension: the constraint bundles genuine coordination functions
 *   (security cooperation against transnational threats, trade frameworks
 *   enabling development) with asymmetric extraction (debt terms favoring
 *   creditors, structural adjustment mandates constraining policy autonomy,
 *   resource extraction benefiting multinational corporations at cost to
 *   indigenous communities).
 *
 * KEY AGENTS:
 *   - Working Poor and Landless Peasants: Primary victims (powerless/trapped) — bear suppression costs through lack of land access, labor coercion, wage depression, limited emigration options
 *   - Central American Nation-States (Guatemala, Honduras, El Salvador, Nicaragua, Costa Rica, Panama): Primary targets (institutional/constrained) — experience extraction through debt obligations, policy conditionality, military dependence, trade asymmetries; also receive coordination benefits through security cooperation and development finance
 *   - United States Government: Primary beneficiary (institutional/arbitrage) — captures geopolitical dominance, resource access, market control, ideological alignment; maintains high arbitrage capacity to shift strategy if payoffs decline
 *   - Multinational Corporations: Secondary beneficiary (institutional/arbitrage) — benefit from privatization mandates, labor access, resource extraction rights, intellectual property enforcement; can relocate to other regions
 *   - Regional Elite Classes: Tertiary beneficiary (organized/arbitrage) — retain land concentration, political power, oligarchic control; aligned with US framework through education, investment, family ties
 *   - Indigenous Communities: Tertiary victims (powerful/constrained or trapped, depending on autonomy status) — face land dispossession, cultural suppression, exclusion from decision-making; autonomy movements create constrained exit pathways
 *   - Regional Civil Society and Indigenous Movements: Organized agents (organized/constrained) — building alternative coordination mechanisms; represent structural exit pathway from constraint
 *   - Colonial Legacy Institutions: Institutional maintenance system (institutional/arbitrage) — perpetuate power structures through formal procedures without substantive reform
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(central_american_sovereignty_constraints, 0.58).
domain_priors:suppression_score(central_american_sovereignty_constraints, 0.72).
domain_priors:theater_ratio(central_american_sovereignty_constraints, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(central_american_sovereignty_constraints, extractiveness, 0.58).
narrative_ontology:constraint_metric(central_american_sovereignty_constraints, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(central_american_sovereignty_constraints, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(central_american_sovereignty_constraints, tangled_rope).
narrative_ontology:human_readable(central_american_sovereignty_constraints, "Central American Sovereignty Constraints: Structural Extraction Within Regional Coordination").
narrative_ontology:topic_domain(central_american_sovereignty_constraints, "geopolitical/economic/institutional").

domain_priors:requires_active_enforcement(central_american_sovereignty_constraints).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(central_american_sovereignty_constraints, united_states_government).
narrative_ontology:constraint_beneficiary(central_american_sovereignty_constraints, multinational_corporations).
narrative_ontology:constraint_beneficiary(central_american_sovereignty_constraints, regional_elite_classes).
narrative_ontology:constraint_victim(central_american_sovereignty_constraints, central_american_nation_states).
narrative_ontology:constraint_victim(central_american_sovereignty_constraints, indigenous_communities).
narrative_ontology:constraint_victim(central_american_sovereignty_constraints, working_poor_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WORKING POOR AND LANDLESS PEASANTS (SNARE) — Structurally trapped within systems of land concentration, debt bondage, and labor coercion. Possess no real exit from the constraint: emigration requires capital and networks; staying requires accepting exploitative wage terms. Suppression is structural and complete — alternative land access, cooperative ownership, or indigenous territorial rights are systematically foreclosed. No genuine coordination benefit accrues to this agent; the constraint exists purely to extract labor surplus.
constraint_indexing:constraint_classification(central_american_sovereignty_constraints, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: CENTRAL AMERICAN NATION-STATES (TANGLED ROPE) — These governments face constrained exit: they depend on US military aid, IMF/World Bank loans, and trade access for basic state function. Yet genuine coordination functions exist — regional security cooperation, trade coordination, infrastructure development. However, the coordination comes bundled with asymmetric extraction: structural adjustment mandates that privatize public goods, debt service obligations that consume fiscal capacity, and political conditionality that constrains sovereign decision-making. States bear disproportionate costs while benefiting minimally from the coordination mechanisms.
constraint_indexing:constraint_classification(central_american_sovereignty_constraints, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: UNITED STATES AND MULTINATIONAL CORPORATIONS (ROPE) — These institutional actors experience the constraint as pure coordination: establishing rule of law (property rights, contract enforcement), trade frameworks, and security cooperation that enable profitable operations and resource extraction. High arbitrage capacity — can shift investment to other regions if terms become unfavorable. The constraint appears to serve genuine coordination functions from this perspective. Effective extraction runs toward these agents; suppression enables their preferred order.
constraint_indexing:constraint_classification(central_american_sovereignty_constraints, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: COLONIAL LEGACY INSTITUTIONS (PITON) — Institutions inherited from the colonial period (plantation systems, hierarchical land ownership, militarized state forms, hierarchical oligarchies) persist despite degraded functionality. Theater ratio is high: land titling offices maintain formal procedures that reproduce colonial cadastral patterns without genuine land reform; elections occur within frameworks that ensure oligarchic control; constitutions declare equality while power structures remain unchanged. The constraint persists through institutional inertia and theatrical maintenance of legitimacy, not because it solves coordination problems effectively.
constraint_indexing:constraint_classification(central_american_sovereignty_constraints, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGIONAL CIVIL SOCIETY AND INDIGENOUS MOVEMENTS (SCAFFOLD) — Organized agents (indigenous autonomy movements, peasant unions, regional human rights frameworks like SICA, Central American Court of Justice) represent a structural exit pathway from the constraint. These movements are building alternative coordination mechanisms: indigenous territorial autonomy, food sovereignty networks, regional courts that challenge US-aligned governments, and transnational civil society accountability. Theater is moderate because some genuine institutional building occurs (IACHR judgments, indigenous land victories). The constraint appears temporary from this perspective — the sunset is indigenous self-determination and regional judicial independence displacing US-dominated coordination.
constraint_indexing:constraint_classification(central_american_sovereignty_constraints, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, this view holds that Central American states face inherent limitations: geographic proximity to a superpower, resource scarcity, lack of internal capital accumulation, and structural disadvantage in the global system constitute immutable constraints on sovereignty. The constraint is naturalized as 'geopolitical reality' or 'global capitalism's inexorable logic.' However, the structural data contradicts this naturalization — the constraint is maintained through specific institutional mechanisms (debt obligations, military aid conditionality, trade agreements, control of finance and technology) that are contingent policy choices, not laws of nature. The mountain classification is a false summit.
constraint_indexing:constraint_classification(central_american_sovereignty_constraints, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(central_american_sovereignty_constraints_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(central_american_sovereignty_constraints, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(central_american_sovereignty_constraints, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(central_american_sovereignty_constraints, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(central_american_sovereignty_constraints, TR),
    TR >= 0.70.

:- end_tests(central_american_sovereignty_constraints_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts labor surplus, resource rents, and policy autonomy from Central American populations and states. However, extraction is not maximal — genuine coordination functions (security cooperation, trade enabling, development finance) provide real benefits to participating states. The gap between what the constraint could extract (if fully coercive) and what it actually extracts reflects that coordination functions do deliver material benefits. The 1980 start point (0.35) reflects limited extraction when integration was less deep; the 2020 endpoint (0.58) reflects accumulating extraction as institutions embed more tightly. Suppression (0.72): High. Structural barriers to exit are substantial: land concentration prevents agricultural autonomy; debt obligations limit fiscal capacity; military dependence constrains foreign policy; CAFTA locks in trade terms; dollar dependence constrains monetary policy; lack of capital/technology access limits development alternatives. However, suppression is not complete (0.90+) because exit routes exist at high cost: indigenous autonomy movements create spaces of alternative governance; regional civil society builds institutional alternatives; some states have attempted ALBA participation; remittance income provides household resilience outside formal economy. Theater ratio (0.65): Moderate-high. Formal democratic institutions, property rights systems, international law frameworks all exist with genuine procedural content. But substantive decision-making power is heavily concentrated: US determines security doctrine; IMF/World Bank design macroeconomic frameworks; multinational corporations control key sectors; oligarchies control land and political access. The theater is not pure performance (0.85+) because some real institutional contestation occurs (constitutional courts occasionally challenge government overreach; indigenous autonomy communities govern themselves; regional courts make binding rulings). But enough is performative that the ratio is 0.65 rather than 0.40.
 *
 * PERSPECTIVAL GAP:
 *   The six perspectives reveal genuine structural ambiguity about whether the constraint is coordination or extraction. From the US beneficiary view, it is pure Rope — establishing stable rules that enable profitable engagement. From the Central American state view, it is mixed — the coordination (security, trade) is real and useful, but asymmetric (they get conditionality, the US gets arbitrage; they get loans, the US gets debt service; they get markets, the US gets resources). From the working poor view, it is pure Snare — the constraint exists entirely to extract their labor and land. From the civil society view, the constraint is a temporary stage (Scaffold) being displaced by indigenous autonomy and regional alternatives. From the institutional heritage view, the constraint is degraded performance (Piton) — it no longer solves coordination problems effectively, persisting through theater and inertia. From the analytical view, the constraint appears as immutable geopolitical reality (Mountain), but this is a false summit — the structural data shows contingent institutional arrangements, not laws of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies sharply by institutional actor and exit options. The US government as institutional/arbitrage beneficiary derives d ≈ 0.05 from benef. status + arbitrage exit → highly negative f(d) → negative χ (constraint subsidizes this agent). Central American states as institutional/constrained victims of policy conditionality derive d ≈ 0.65 from mixed beneficiary/victim status + constrained exit → f(d) ≈ 1.00 → χ ≈ 0.58 (moderate extraction, significant experienced cost). The working poor as powerless/trapped victims derive d ≈ 0.95 from victim status + trapped exit → f(d) ≈ 1.42 → χ ≈ 0.82 (if isolated from group action; significantly reduced if working poor organize into cooperative/union structures moving toward collective power). Regional elite classes as organized/arbitrage beneficiaries derive d ≈ 0.30 from mixed victim/beneficiary status (bear some suppression costs but gain oligarchic preservation) + arbitrage exit → f(d) ≈ 0.15 → low/negative χ (constraint preserves their power). Scope modifier σ(S) is continental (1.1) for US policy and regional (0.9) for state-level coordination, reflecting that extraction verification is harder at larger scope (more moving parts, more plausible deniability) but easier at regional scope (concentrated institutional actors visible to each other).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint legitimately exhibits multiple types from different perspectives because it genuinely combines coordination and extraction functions. The coordination is not illusory (security cooperation prevents drug trafficking spillover, trade enables growth, development finance builds infrastructure) but also not unilateral in benefit flow. The extraction is not coercive in the sense of requiring armed occupation (though military presence exists) but is structural in the sense that certain populations and states have constrained options and bear disproportionate costs. The Tangled Rope classification at the nation-state level correctly captures this hybrid: genuine coordination function + genuine asymmetric extraction + active enforcement (debt obligations, structural adjustment conditionality, military aid strings). The false summit (Mountain from analytical) is diagnostic: it reveals how naturalization of contingent institutional arrangements serves the beneficiary interests. If the constraint were truly immutable geopolitical law, it would not require such extensive institutional maintenance (IMF conditionality, CAFTA litigation, US military deployment, elite identity alignment). The effort to maintain it is evidence that it is not natural law. The true Snare classification from the working poor view is diagnostic of total exclusion — this agent has no voice in coordination design and experiences only extraction. The Scaffold classification from civil society reveals the constraint's fragility — it persists only as long as alternative institutions remain underdeveloped. The resolution of mandatrophy is the presheaf: the constraint is a hybrid coordination-extraction arrangement whose character is fundamentally perspective-dependent, and no single classification erases the others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_integration_framing,
    'Is the constraint best understood as loss of sovereignty through coercive mechanisms, or as voluntary integration into economic and security arrangements that constrain but also benefit participating states?',
    'Counterfactual analysis: if Central American states had refused IMF/World Bank loans and US military aid in 1980s and 1990s, what would have been their fiscal and security capacity? Do current economic indicators show net benefit from integration, or net extraction?',
    'If understood as coercive extraction: classification remains Snare/Tangled Rope. If understood as beneficial integration: classification shifts toward Rope/Scaffold. The frame determines whether we see imperialism or development partnership.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_integration_framing, conceptual, 'Whether constraint is coercive extraction or mutually beneficial integration').

omega_variable(
    land_concentration_mechanism,
    'Is land concentration in Central America a structural economic outcome, or is it actively maintained through legal/enforcement mechanisms that could be dismantled?',
    'Historical comparison of land reform attempts (Guatemala 1950s, Nicaragua 1980s) with current inequalities; analysis of land titling procedures and their enforcement capacity; measurement of indigenous autonomy zones where alternative property regimes function.',
    'If maintained through removable mechanisms: the working poor agent moves from trapped to constrained exit — policy intervention (land reform, indigenous autonomy) becomes viable. If structural to the economy: remains trapped. Movement affects classification from Snare toward Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(land_concentration_mechanism, empirical, 'Whether land concentration is maintained through removable institutional mechanisms').

omega_variable(
    regional_alternative_capacity,
    'Can Central American states credibly establish independent regional institutions (alternative to IMF/World Bank, CAFTA, US military frameworks) that would reduce extractive pressure without catastrophic economic contraction?',
    'Analysis of ALBA alternative framework, SICA judicial independence growth, regional development banks capacity; projection of counterfactual regional trade and financing without US-aligned institutions.',
    'If capacity is high: scaffold perspective is realistic — exit is possible at manageable cost, suppression can be reduced. If capacity is low: states remain trapped in current system — suppression persists. Affects the viability of the sunset clause.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_alternative_capacity, empirical, 'Whether credible independent regional institutions can provide alternative coordination').

omega_variable(
    identity_lock_mechanism,
    'To what extent is the constraint maintained because Central American elites and state officials have internalized the framework of US dependency and see no viable alternative, versus being maintained by structural barriers to exit?',
    'Comparative analysis of elite narratives and policy choices in different contexts (ALBA countries vs CAFTA countries); identification of cases where elites rejected US alignment despite structural pressure; measurement of elite education and career dependence on US institutions.',
    'If significant identity lock: classification includes identity_locked exit option for institutional agents — the constraint is perpetuated by captured perspectives, not purely structural barriers. Enables narrative pathways (new elite cohort) that don''t require structural reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether constraint is maintained by elite identity fusion with US-aligned frameworks').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(central_american_sovereignty_constraints, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(casov_tr_t0, central_american_sovereignty_constraints, theater_ratio, 0, 0.55).
narrative_ontology:measurement(casov_tr_t20, central_american_sovereignty_constraints, theater_ratio, 20, 0.62).
narrative_ontology:measurement(casov_tr_t40, central_american_sovereignty_constraints, theater_ratio, 40, 0.65).

% Extraction over time
narrative_ontology:measurement(casov_be_t0, central_american_sovereignty_constraints, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(casov_be_t20, central_american_sovereignty_constraints, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(casov_be_t40, central_american_sovereignty_constraints, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(central_american_sovereignty_constraints, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(central_american_sovereignty_constraints, 0.18).
narrative_ontology:affects_constraint(central_american_sovereignty_constraints, us_imperial_reach_projection).
narrative_ontology:affects_constraint(central_american_sovereignty_constraints, latin_american_debt_trap).
narrative_ontology:affects_constraint(central_american_sovereignty_constraints, indigenous_land_dispossession).
narrative_ontology:affects_constraint(central_american_sovereignty_constraints, regional_militarization).

% DUAL FORMULATION NOTE:
% Central American sovereignty constraints decompose into multiple structurally distinct constraints: land concentration (colonial institutional inheritance, ε≈0.72), debt servicing obligations (macroeconomic policy conditionality, ε≈0.58), military dependence (geopolitical security framework, ε≈0.48), trade asymmetries (CAFTA institutional lock-in, ε≈0.52). Each has distinct beneficiaries, suppression mechanisms, and exit pathways. This story captures the bundle; specific decomposition into individual stories enables precision analysis of which mechanisms drive extraction in each domain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(central_american_sovereignty_constraints, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
