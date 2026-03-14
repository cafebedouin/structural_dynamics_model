% ============================================================================
% CONSTRAINT STORY: bri_geopolitical_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bri_geopolitical_extraction, []).

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
 *   constraint_id: bri_geopolitical_extraction
 *   human_readable: Belt and Road Initiative Geopolitical Extraction Constraint
 *   domain: geopolitical/economic/development
 *
 * SUMMARY:
 *   The Belt and Road Initiative represents a large-scale capital
 *   mobilization mechanism that exhibits simultaneous coordination and
 *   extraction functions. Recipient nations access capital for infrastructure
 *   projects they structurally need (coordination benefit) while experiencing
 *   loss of fiscal sovereignty, asset control, and geopolitical autonomy
 *   (extraction cost). The constraint exhibits all six classification types
 *   depending on observer position: a snare from the perspective of fiscal
 *   sovereignty (trapped with no exit), a tangled rope from development
 *   planners and political establishments (mixed coordination and
 *   extraction), a rope from the Chinese state perspective (net beneficiary
 *   with arbitrage options), a scaffold from international development actors
 *   building alternative financing mechanisms, a piton from the
 *   historical-structural perspective (replicating colonial patterns through
 *   institutional inertia), and a false mountain from the analytical observer
 *   (treating structural patterns as immutable laws of capital flows).
 *   Extractiveness has increased from 0.35 to 0.62 over the measurement
 *   interval as early-stage BRI projects have matured and debt service
 *   dynamics have become apparent. Theater ratio has risen from 0.40 to 0.55
 *   as promotional narratives around development impact have diverged from
 *   economic outcome data.
 *
 * KEY AGENTS:
 *   - Chinese State: Primary beneficiary (institutional/arbitrage) — gains geopolitical influence, capital export markets, and strategic positioning across recipient nations
 *   - Chinese Construction and Manufacturing Firms: Beneficiary (institutional/arbitrage) — capture infrastructure contracts, export markets, and equipment sales
 *   - Recipient Nation's Fiscal Authorities: Primary victim (powerless/trapped) — bear debt service obligations and loss of collateralized assets; no alternative financing at comparable terms
 *   - Recipient Nation's Development Planners: Secondary victim (moderate/constrained) — need infrastructure capital but accept asymmetric control terms; see mixed coordination and extraction
 *   - Recipient Nation's Political Establishment: Tertiary actor (institutional/constrained) — benefit from capital and patronage but accept loss of decision-making autonomy; politically locked into Chinese dependency
 *   - International Development Institutions: Organized observer (organized/constrained) — perceive BRI as temporary coordination failure; building alternative pathways (green bonds, regional banks) with generational sunset logic
 *   - Global Infrastructure Governance: Victim (analytical/trapped) — abstract collective good; standards fragmentation and reduced transparency in international infrastructure oversight
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing capital asymmetry as immutable law rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bri_geopolitical_extraction, 0.58).
domain_priors:suppression_score(bri_geopolitical_extraction, 0.62).
domain_priors:theater_ratio(bri_geopolitical_extraction, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bri_geopolitical_extraction, extractiveness, 0.58).
narrative_ontology:constraint_metric(bri_geopolitical_extraction, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(bri_geopolitical_extraction, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bri_geopolitical_extraction, tangled_rope).
narrative_ontology:human_readable(bri_geopolitical_extraction, "Belt and Road Initiative Geopolitical Extraction Constraint").
narrative_ontology:topic_domain(bri_geopolitical_extraction, "geopolitical/economic/development").

domain_priors:requires_active_enforcement(bri_geopolitical_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bri_geopolitical_extraction, chinese_state_strategic_influence).
narrative_ontology:constraint_beneficiary(bri_geopolitical_extraction, chinese_construction_firms).
narrative_ontology:constraint_beneficiary(bri_geopolitical_extraction, chinese_manufacturing_exporters).
narrative_ontology:constraint_victim(bri_geopolitical_extraction, recipient_nation_fiscal_sovereignty).
narrative_ontology:constraint_victim(bri_geopolitical_extraction, recipient_nation_labor_standards).
narrative_ontology:constraint_victim(bri_geopolitical_extraction, global_infrastructure_governance).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RECIPIENT NATION FISCAL SOVEREIGNTY (SNARE) — Trapped by debt dependency with no exit option. Bears full cost of infrastructure locked to Chinese operational control and debt service obligations. Cannot restructure or walk away without triggering default cascades and loss of collateralized assets (ports, mining rights, Special Economic Zones).
constraint_indexing:constraint_classification(bri_geopolitical_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RECIPIENT NATION DEVELOPMENT PLANNERS (TANGLED ROPE) — Constrained by domestic capital shortage and political pressure to deliver infrastructure. Genuine coordination benefit exists (they need capital-intensive connectivity projects and cannot access alternative financing on comparable terms). But coordination is coupled with asymmetric extraction: Chinese firms control procurement, labor composition, and operational decision-making. Exit is costly (loss of projects, political disruption) but structurally possible over generational time.
constraint_indexing:constraint_classification(bri_geopolitical_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CHINESE STATE AND FIRMS (ROPE) — Experiences BRI as coordination mechanism: exporting surplus capital, manufacturing capacity, and construction expertise; solving geopolitical positioning and development diplomacy. Net beneficiary with arbitrage options — can reallocate capital and construction capacity to other markets if recipient nation policies shift. Extraction runs toward this agent, not away.
constraint_indexing:constraint_classification(bri_geopolitical_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL DEVELOPMENT COALITION (SCAFFOLD) — Organized agents (World Bank, IMF, regional development banks, NGOs) perceive BRI as a temporary coordination failure with potential sunset through institutional reform. Sunset logic: as recipient nations build capacity for alternative financing (green bonds, regional development banks, decentralized infrastructure governance), BRI's monopoly on capital provision weakens. Organized agents see this as a 10-20 year trajectory toward alternative pathways. Low effective extraction because coalition has agency and sees exit path.
constraint_indexing:constraint_classification(bri_geopolitical_extraction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: COLONIAL INFRASTRUCTURE PATTERN (PITON) — BRI replicates structural patterns from historical colonialism (resource extraction via infrastructure control, financial dependency, political capture through debt) but without formal political colonization. The pattern persists through institutional inertia and path dependence — recipient nations inherit colonial-era extraction infrastructure and BRI extends it. Theater ratio reflects performative development narratives (growth projections, GDP multipliers) masking fundamental extraction mechanisms. The pattern has degraded its own coordination function; primarily maintains extraction through historical precedent.
constraint_indexing:constraint_classification(bri_geopolitical_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: RECIPIENT NATION POLITICAL ESTABLISHMENT (TANGLED ROPE) — Constrained by domestic political economy of Chinese capital access. Genuine coordination benefit: BRI funds projects that satisfy political constituencies and generate patronage networks. But coordination couples with extraction: Chinese control over implementation decisions, labor contracts (favoring Chinese workers), and future operational control extracts rents from recipient nation's asset base. Exit is constrained by domestic political costs of losing Chinese support.
constraint_indexing:constraint_classification(bri_geopolitical_extraction, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / CAPITAL FLOW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some asymmetry in capital-for-control exchange is inherent to international development financing: capital-exporting nations always retain leverage over capital-importing nations. This perspective sees BRI as an immutable structural feature of global capital flows. However, the structural data contradicts the mountain classification — the engine will compute this as a false summit, revealing that capacity-building alternatives (regional development banks, green bonds, decentralized governance) are not laws of nature but institutional choices.
constraint_indexing:constraint_classification(bri_geopolitical_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bri_geopolitical_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bri_geopolitical_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bri_geopolitical_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bri_geopolitical_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bri_geopolitical_extraction, TR),
    TR >= 0.70.

:- end_tests(bri_geopolitical_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. BRI demonstrates measurable extraction across multiple dimensions: debt service obligations exceed baseline development benefit projections, Chinese firms capture 65-85% of infrastructure contracts, operational control of strategic assets (ports, energy) transfers to Chinese entities upon default, and geopolitical leverage enables policy concessions beyond the finance agreement. The value of 0.58 reflects that extraction is real and growing but not as severe as pure debt-trap schemes (which reach 0.72+) because some genuine infrastructure coordination benefit exists and recipient nations retain nominal sovereign control. Suppression (0.62): Moderate-high. Barriers to exit include: (1) debt lock-in with collateralized assets, (2) absence of alternative financing at comparable terms, (3) sunk costs in infrastructure projects requiring Chinese operational expertise, (4) political costs of defaulting and losing future Chinese investment, (5) asymmetric information about long-term operational costs, and (6) limited capacity to renegotiate terms once construction begins. But suppression is not total — some nations (Malaysia, Myanmar) have successfully renegotiated or suspended BRI projects, indicating that exit, while costly, is possible. Theater ratio (0.55): Moderate. Development impact narratives emphasize GDP growth multipliers and connectivity benefits, but empirical data shows more modest returns and significant asset losses in default scenarios. The theater has increased over time as gap between projections and outcomes has widened. Theater is not dominant (piton threshold 0.70) because the infrastructure itself remains partially functional; some coordination benefit is real, not purely performative.
 *
 * PERSPECTIVAL GAP:
 *   The maximum perspectival gap occurs between the beneficiary's Rope and the victim's Snare — a 5-type gap spanning the full classification spectrum. This gap is diagnostic: it reveals that BRI operates fundamentally through differential structural power rather than through symmetric coordination or uniform extraction. The Chinese state perceives coordination; recipient nations perceive entrapment. Neither is lying or mistaken — they occupy structurally different positions. The gap also appears between the analytical observer's Mountain (treating capital asymmetry as natural law) and the international coalition's Scaffold (treating it as a temporary institutional problem with alternatives). This second gap reveals the oracle problem: the analytical position risks naturalizing what the organized position correctly sees as contingent.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values (d) derived from structural position show stark asymmetry. The Chinese state benefits from BRI's capital export function and has arbitrage options (can reallocate capital globally), producing low d (~0.15) and negative/low χ — they experience the constraint as enabling, not constraining. Recipient nation fiscal authorities depend on the capital and have no exit, producing high d (~0.90) and high χ — they experience the constraint as severely constraining. Development planners occupy middle ground (d ~0.60) with real coordination benefits offset by control costs. The asymmetry is structural: the constraint is designed to move capital from one actor (with low d) to another (with high d) while enforcing asymmetric control. Political establishments occupy d ~0.55 (partial beneficiary through patronage, but constrained by political dependency). The international development coalition has analytical distance (d ~0.70) with some agency to build alternatives. The analytical observer at civilizational scope faces d ~0.75 (treats all actors as subject to immutable capital dynamics) — a false summit revealing that the observer's own frame is naturalizing contingency.
 *
 * MANDATROPHY ANALYSIS:
 *   BRI resolves mandatrophy by demonstrating that tangled rope classification is correct despite legitimate rope and snare interpretations. The snare perspective (fiscal authorities experiencing pure extraction with no exit) is the victim's true experience within the binding period. The rope perspective (Chinese state experiencing genuine coordination benefit) is the beneficiary's true experience. The tangled rope classification (development planners) captures the hybrid nature: the constraint genuinely coordinates capital provision while extracting geopolitical control and asset collateral. Active enforcement is required — default triggers asset seizure, credit sanctions, and political pressure. Beneficiaries are clearly identified (Chinese state, firms, exporters). Victims are clearly identified (fiscal sovereignty, labor standards, global governance). Without the tangled rope type, the classification would collapse either into rope (overemphasizing coordination) or snare (overemphasizing extraction). The tangled rope correctly holds both truths in tension: BRI is simultaneously a coordination solution to capital scarcity and an extraction mechanism concentrating geopolitical power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    debt_trap_intentionality,
    'Is BRI geopolitical extraction intentional strategic design or emergent outcome of fiscal asymmetry and market mechanisms?',
    'Analysis of Chinese state policy documents, evidence of deliberate debt-trap structuring vs opportunistic capital deployment; comparison of BRI loan terms to market rates for equivalent risk profiles',
    'If intentional: extraction is active enforcement (tangled rope confirmed). If emergent: constraint may be rope with unintended consequences. Classification outcome differs in sophistication judgment but not in measured extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debt_trap_intentionality, empirical, 'Whether extraction is deliberate strategic design or emergent outcome').

omega_variable(
    alternative_financing_availability,
    'Would recipient nations have access to comparable or better infrastructure financing without BRI? What are the counterfactual terms?',
    'Comparative analysis of alternative financing sources (multilateral banks, regional development banks, green bonds) available to recipient nations; reconstruction of counterfactual loan terms and conditionality',
    'If alternatives at comparable terms exist: BRI''s relative extractiveness is lower than measured. If alternatives severely constrained: BRI is filling genuine capital gap, tangled rope classification confirmed. If no alternatives: classification shifts toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_financing_availability, empirical, 'Availability of alternative infrastructure financing at comparable terms').

omega_variable(
    asset_seizure_enforcement,
    'Is extraction enforced through credible threat of asset seizure (ports, mines, SEZs), or is enforcement primarily through loss of future credit access?',
    'Case studies of default dynamics; examination of loan documents'' collateral and enforcement provisions; analysis of actual enforcement events (Sri Lanka Hambantota Port seizure being canonical case)',
    'If seizure-backed: suppression is structural and high (0.62 confirmed). If credit-loss backed: suppression may be lower and extraction more dependent on ongoing capital availability. Changes characterization of binding mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(asset_seizure_enforcement, empirical, 'Whether extraction relies on credible asset seizure enforcement').

omega_variable(
    labor_standards_decoupling,
    'Do Chinese labor practices on BRI projects constitute a separate constraint (labor extraction) or are they integral to the geopolitical extraction mechanism?',
    'Comparative analysis of labor standards on Chinese-implemented vs multilateral-financed infrastructure projects; examination of whether labor wage compression transfers value to Chinese firms or serves geopolitical positioning',
    'If separate constraint: write distinct story (bri_labor_extraction). If integral: current story captures both mechanisms correctly. Affects network decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_standards_decoupling, empirical, 'Whether labor extraction is integral to geopolitical extraction or separate constraint').

omega_variable(
    strategic_resilience_value,
    'Does BRI infrastructure genuinely improve recipient nation''s strategic autonomy and resilience, or does it primarily deepen dependency on Chinese supply chains and markets?',
    'Analysis of infrastructure utility: ports, rail, power systems — to whom do they export? Does infrastructure serve recipient nation''s diversification or primarily Chinese trade goals? Examination of post-project economic outcomes vs projections.',
    'If infrastructure serves recipient autonomy: coordination function is more genuine, snare classification overstated. If infrastructure deepens Chinese dependency: snare classification confirmed, coordination is theater. Changes assessment of theater_ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_resilience_value, empirical, 'Whether infrastructure improves recipient autonomy or deepens Chinese dependency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bri_geopolitical_extraction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bri_geo_tr_t0, bri_geopolitical_extraction, theater_ratio, 0, 0.4).
narrative_ontology:measurement(bri_geo_tr_t3, bri_geopolitical_extraction, theater_ratio, 3, 0.48).
narrative_ontology:measurement(bri_geo_tr_t6, bri_geopolitical_extraction, theater_ratio, 6, 0.55).
narrative_ontology:measurement(bri_geo_tr_t9, bri_geopolitical_extraction, theater_ratio, 9, 0.6).

% Extraction over time
narrative_ontology:measurement(bri_geo_be_t0, bri_geopolitical_extraction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bri_geo_be_t3, bri_geopolitical_extraction, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(bri_geo_be_t6, bri_geopolitical_extraction, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(bri_geo_be_t9, bri_geopolitical_extraction, base_extractiveness, 9, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bri_geopolitical_extraction, resource_allocation).
narrative_ontology:affects_constraint(bri_geopolitical_extraction, recipient_nation_debt_sustainability).
narrative_ontology:affects_constraint(bri_geopolitical_extraction, global_infrastructure_governance_fragmentation).
narrative_ontology:affects_constraint(bri_geopolitical_extraction, geopolitical_alignment_capture).

% DUAL FORMULATION NOTE:
% BRI decomposition: The geopolitical extraction constraint (this story, ε=0.58, Tangled Rope) is downstream of structural capital asymmetry (ε=0.08, Rope — natural capital flow disparity) and upstream of specific debt default dynamics (ε=0.72, Snare — individual recipient nation traps). The structural capital asymmetry creates the enabling condition; BRI's institutional design channels it into geopolitical extraction; specific defaults realize the extraction mechanism. Each story has distinct measurement parameters and perspectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bri_geopolitical_extraction, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
