% ============================================================================
% CONSTRAINT STORY: municipal_governance_fragmentation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_municipal_governance_fragmentation, []).

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
 *   constraint_id: municipal_governance_fragmentation
 *   human_readable: Municipal Governance Fragmentation
 *   domain: public_administration/urban_governance
 *
 * SUMMARY:
 *   Municipal governance fragmentation in metropolitan regions creates a
 *   structural tension between genuine coordination benefits (local control,
 *   service customization, participatory accountability) and systematic
 *   extraction through fiscal stratification and service inequality.
 *   Fragmentation enables wealthier jurisdictions to capture tax bases
 *   through fiscal zoning while immobilizing lower-income residents in
 *   declining municipalities. The constraint exhibits hybrid
 *   coordination-extraction dynamics: fragmentation solves real coordination
 *   problems at the local scale while simultaneously enabling extractive
 *   dynamics at the regional scale. Regional planning institutions (MPOs,
 *   COGs) were created to coordinate fragmentation but have largely become
 *   ceremonial bodies, reflecting a Piton degradation pattern. The
 *   extractiveness has increased over 30 years as housing market inequality
 *   has amplified municipal fiscal disparities. Theater ratio growth (0.52 to
 *   0.68) reflects increasing performative regional planning activity
 *   alongside declining actual coordination authority.
 *
 * KEY AGENTS:
 *   - Low-income urban residents: Primary victims (powerless/trapped) — locked in declining municipalities by housing market barriers; experience maximum service inequality and suppressed regional redistribution
 *   - Middle-income commuters: Secondary victims (moderate/constrained) — can exit to better-funded suburbs at significant cost; experience mixed coordination benefits and extraction costs
 *   - Wealthy suburban municipalities: Primary beneficiaries (institutional/arbitrage) — capture tax base concentration through fiscal zoning; experience fragmentation as enabling coordination and service optimization
 *   - Development interests: Secondary beneficiaries (powerful/mobile) — exploit fragmentation to play jurisdictions against each other; benefit from reduced environmental and labor enforcement
 *   - Regional planning institutions: Institutional actor (institutional/arbitrage) — created to coordinate fragmentation but degraded to ceremonial role; maintain theater of regional planning while actual authority remains localized
 *   - State government: Structural determiner (institutional/arbitrage) — enforces Dillon's Rule limiting local authority while relying on property tax (fragmentation-amplifying revenue source); sets rule structures enabling fragmentation dynamics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(municipal_governance_fragmentation, 0.58).
domain_priors:suppression_score(municipal_governance_fragmentation, 0.65).
domain_priors:theater_ratio(municipal_governance_fragmentation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(municipal_governance_fragmentation, extractiveness, 0.58).
narrative_ontology:constraint_metric(municipal_governance_fragmentation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(municipal_governance_fragmentation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(municipal_governance_fragmentation, tangled_rope).
narrative_ontology:human_readable(municipal_governance_fragmentation, "Municipal Governance Fragmentation").
narrative_ontology:topic_domain(municipal_governance_fragmentation, "public_administration/urban_governance").

domain_priors:requires_active_enforcement(municipal_governance_fragmentation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(municipal_governance_fragmentation, wealthy_suburban_municipalities).
narrative_ontology:constraint_beneficiary(municipal_governance_fragmentation, municipal_finance_professionals).
narrative_ontology:constraint_beneficiary(municipal_governance_fragmentation, development_interests).
narrative_ontology:constraint_victim(municipal_governance_fragmentation, low_income_urban_residents).
narrative_ontology:constraint_victim(municipal_governance_fragmentation, regional_public_goods).
narrative_ontology:constraint_victim(municipal_governance_fragmentation, environmental_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IMMOBILE URBAN RESIDENT (SNARE) — Residents locked in declining municipal jurisdictions face deteriorating services, underfunded schools, and degraded infrastructure. Exit requires financial capacity (housing market participation) they lack. Zero degrees of freedom. Fragmentation extracts through service inequality while suppressing regional governance that might redistribute. Maximum experienced extraction.
constraint_indexing:constraint_classification(municipal_governance_fragmentation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MIDDLE-INCOME COMMUTER (TANGLED ROPE) — Can relocate to suburban jurisdiction but at significant cost (housing, commute time, social capital loss). Genuine coordination function exists: fragmentation enables localized service optimization and community control. But asymmetric extraction emerges: commuters can exit to better-funded municipalities while trapped residents cannot. Mixed experience of both benefit and extraction.
constraint_indexing:constraint_classification(municipal_governance_fragmentation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: WEALTHY SUBURBAN MUNICIPALITY (ROPE) — Benefits from fragmentation through fiscal zoning (excluding low-cost housing), capturing tax base concentration. Experiences constraint as coordination mechanism: local control enables efficient service delivery aligned with resident preferences. Can arbitrage between jurisdictions (tax competition, service outsourcing). Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(municipal_governance_fragmentation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: REGIONAL DEVELOPMENT INTEREST (TANGLED ROPE) — Real estate developers and development coalitions benefit from fragmented governance: can play municipalities against each other in bidding wars for projects, negotiate site-specific exemptions. But also constrained by need to coordinate across jurisdictional boundaries for infrastructure, labor supply, supply chains. Both benefits from extraction (lower labor standards, reduced environmental enforcement through competition) and extraction of coordination capacity. Powerful but not fully arbitraged.
constraint_indexing:constraint_classification(municipal_governance_fragmentation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: STATE-LEVEL REGIONAL PLANNING (PITON) — Formal regional planning agencies (MPOs, COGs, state planning departments) were created to coordinate fragmented municipalities but have largely atrophied into ceremonial bodies. Theater ratio 0.68 reflects that planning meetings and regional visions persist but lack enforcement power — actual resource allocation remains localized. The apparatus persists through inertia (state mandates, federal grant requirements) while municipalities ignore regional coordination. Degraded institutional form with reduced functional capacity.
constraint_indexing:constraint_classification(municipal_governance_fragmentation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal scale, fragmented governance is structurally inevitable given U.S. constitutional federalism and historical settlement patterns. Local control is presented as an inalienable right emergent from the structure of democratic representation itself. However, this perspective risks naturalizing the contingent institutional choice (Tiebout sorting, Dillon's Rule state dominance, real estate taxation structures) that creates fragmentation. The engine will flag this as a false summit — the 'inevitability' claim should be tested against observed variation across jurisdictions with different institutional designs.
constraint_indexing:constraint_classification(municipal_governance_fragmentation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(municipal_governance_fragmentation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(municipal_governance_fragmentation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(municipal_governance_fragmentation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(municipal_governance_fragmentation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(municipal_governance_fragmentation, TR),
    TR >= 0.70.

:- end_tests(municipal_governance_fragmentation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Fragmentation enables systematic wealth stratification through fiscal zoning — municipalities with high property tax bases can fund superior services while excluding lower-income residents through housing restrictions. The extraction is not total (0.70+) because genuine coordination benefits exist: local control enables service customization and community participation. But extraction is substantial and grows over time as housing inequality amplifies municipal fiscal disparities. Suppression (0.65): High. Barriers to exit from declining municipalities include: housing market discrimination and affordability gaps (structural), limited information about opportunity differences (informational), and social capital loss from relocation (psychological). Suppression is not maximum (0.85+) because some middle-income residents can exit, creating a stratified mobility tier. Theater ratio (0.68): Moderate-high. Regional planning apparatus (MPOs, COGs) produces substantial ceremonial activity — comprehensive regional visions, coordination meetings, intergovernmental agreements — but lacks enforcement authority. Actual resource allocation remains localized. Growth in theater ratio (0.52→0.68) reflects federal mandate creep requiring regional planning for grant eligibility while real authority remains fragmented.
 *
 * PERSPECTIVAL GAP:
 *   The wealthy suburban municipality sees rope (coordination enabling efficient service delivery aligned with resident preferences). The low-income urban resident sees snare (no exit, systematic extraction through service inequality). The state-level planning apparatus sees its own degradation (piton — ceremonial activity lacking enforcement). The regional development interest sees tangled rope (both benefits and constraints from fragmentation). The civilizational analytical perspective risks mountain (federalism as inevitably requiring fragmentation) but must test whether institutional design choices rather than constitutional necessity drive fragmentation. The perspectival gap reveals that fragmentation creates genuine coordination benefits at municipal scale while simultaneously creating extractive inequality at regional scale — the coordination and extraction operate on different geographic levels, explaining why fragmentation can appear as pure rope (local scale) or pure snare (regional scale) depending on perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural position within fragmentation: (1) Low-income urban residents are victims with no exit → high d (0.95), maximum experienced extraction. (2) Wealthy suburban municipalities are beneficiaries with full arbitrage capacity → low d (0.05), negative experienced extraction (they benefit from the constraint). (3) Middle-income commuters are victims with constrained but available exit → moderate d (0.65), moderate experienced extraction. (4) Development interests are beneficiaries with powerful organizational capacity but constrained by coordination requirements → moderate-low d (0.35), resulting in tangled rope experience. (5) Regional planning institutions are institutional beneficiaries (through continued mandates and grants) with arbitrage options → low d (0.15), but theater ratio indicates degraded function. (6) Analytical observer at civilizational scale must account for contingency in institutional design — d is indeterminate until constitutional necessity vs contingent choice is resolved.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that fragmentation creates genuine coordination function (local control, service customization, participatory accountability) at municipal scale while simultaneously creating extraction function (fiscal stratification, service inequality, regional environmental commons degradation) at regional scale. The mandatrophy is not 'which function dominates?' but 'at what scale?' Fragmentation is rope at municipal level (pure coordination with benefits distributed relatively equally among participating municipalities) and snare at regional level (systematic extraction from immobilized low-income residents toward wealthier jurisdictions). The tangled_rope classification at moderate power and constrained exit reflects the hybrid scale-dependent nature: the constraint simultaneously enables and extracts. The piton classification at civilizational scale reveals that regional planning institutions were created as coordination-enforcement mechanisms (to prevent snare dynamics) but have degraded to ceremonial form, unable to enforce regional equity. The mountain perspective risks naturalizing what is actually a contingent institutional choice (Dillon's Rule state dominance, property tax reliance, local control doctrine) rather than a constitutional necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regional_coordination_counterfactual,
    'What would regional governance look like with actual enforcement authority? Would it reduce extractiveness or merely concentrate extraction at regional level?',
    'Historical analysis of strong regional governance systems (e.g., Toronto GTA, Copenhagen region, Stuttgart metropolitan council); comparison of service equity, environmental outcomes, and fiscal progression',
    'If effective: fragmentation classifies as snare with solvable extraction (scaffold path). If regional governance creates new extraction: fragmentation shifts to tangled_rope at regional level with no structural improvement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_coordination_counterfactual, empirical, 'Whether regional coordination authority would reduce or relocate extraction').

omega_variable(
    tiebout_sorting_volition,
    'To what extent does Tiebout mobility (voting with feet) represent genuine consumer choice versus structural coercion disguised as preference?',
    'Demographic analysis of relocation patterns; correlation between municipal service changes and migration; exit cost decomposition (housing market access, discrimination, information asymmetry)',
    'If genuine choice: fragmentation is rope with mobility benefits (weaker snare claims). If largely coercive: mobility is exit option fraud, strengthening snare classification for those unable to participate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tiebout_sorting_volition, empirical, 'Whether Tiebout mobility is genuine choice or structural coercion').

omega_variable(
    fiscal_baseline_contingency,
    'Are disparities in municipal finance inevitable products of fragmentation or artifacts of specific state revenue allocation rules (property tax reliance, intergovernmental aid formulas)?',
    'Cross-state comparison of fragmentation vs equity outcomes; analysis of jurisdictions with state-equalized revenue systems vs locally-dependent systems; historical change analysis when revenue rules shifted',
    'If fundamental to fragmentation: extractiveness ≥0.58 is structural. If contingent on state rules: fragmentation could be rope (coordination without extraction) under different fiscal regime.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fiscal_baseline_contingency, empirical, 'Whether fiscal disparities are inherent to fragmentation or contingent on state policy').

omega_variable(
    service_optimization_claims,
    'Do fragmented municipalities actually deliver superior service efficiency compared to regional consolidated governance, or is efficiency framed selectively?',
    'Benchmarking studies comparing cost-per-capita, service quality metrics, and administrative overhead across consolidated vs fragmented metro regions; analysis of measurement selection bias in efficiency claims',
    'If genuine efficiency: coordination benefit is real, justifying tangled_rope classification. If selectively measured: efficiency is theater covering extraction, supporting snare/piton diagnoses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(service_optimization_claims, empirical, 'Whether fragmented governance delivers superior service efficiency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(municipal_governance_fragmentation, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mgf_tr_t0, municipal_governance_fragmentation, theater_ratio, 0, 0.52).
narrative_ontology:measurement(mgf_tr_t15, municipal_governance_fragmentation, theater_ratio, 15, 0.6).
narrative_ontology:measurement(mgf_tr_t30, municipal_governance_fragmentation, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(mgf_be_t0, municipal_governance_fragmentation, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(mgf_be_t15, municipal_governance_fragmentation, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(mgf_be_t30, municipal_governance_fragmentation, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(municipal_governance_fragmentation, resource_allocation).
narrative_ontology:boltzmann_floor_override(municipal_governance_fragmentation, 0.18).
narrative_ontology:affects_constraint(municipal_governance_fragmentation, housing_market_stratification).
narrative_ontology:affects_constraint(municipal_governance_fragmentation, fiscal_zoning_exclusion).
narrative_ontology:affects_constraint(municipal_governance_fragmentation, environmental_commons_degradation).
narrative_ontology:affects_constraint(municipal_governance_fragmentation, public_transit_fragmentation).

% DUAL FORMULATION NOTE:
% Municipal governance fragmentation decomposes into multiple structurally distinct constraints operating at different scales: (1) Fiscal zoning exclusion (ε≈0.72, snare at neighborhood scale); (2) Service inequality accumulation (ε≈0.65, tangled rope at municipal scale); (3) Regional coordination failure (ε≈0.58, tangled rope at regional scale); (4) Environmental commons degradation (ε≈0.55, tangled rope at watershed scale). Each story has its own beneficiaries/victims and extractiveness value. The present story models fragmentation at regional scale as hybrid coordination-extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(municipal_governance_fragmentation, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
