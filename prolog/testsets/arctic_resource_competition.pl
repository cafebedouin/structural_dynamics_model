% ============================================================================
% CONSTRAINT STORY: arctic_resource_competition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_arctic_resource_competition, []).

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
 *   constraint_id: arctic_resource_competition
 *   human_readable: Arctic Resource Competition and Geopolitical Extraction
 *   domain: geopolitical/economic/environmental
 *
 * SUMMARY:
 *   Arctic resource competition represents a multi-layered constraint
 *   coupling genuine coordination problems (infrastructure development,
 *   maritime safety, territorial clarity) with asymmetric extraction of
 *   resources and decision-making power. Climate change has transformed the
 *   Arctic from a frozen geopolitical periphery into a frontier of resource
 *   extraction, shipping routes, and military strategic interest. The
 *   constraint exhibits stark perspectival gaps: extractive industries and
 *   Arctic rim states experience the framework as enabling coordination and
 *   capturing legitimate economic benefits. Indigenous Arctic communities
 *   experience it as entrapment — geographic location and colonial
 *   institutional inheritance eliminate exit options while environmental
 *   extraction directly harms their subsistence, health, and cultural
 *   continuity. International governance institutions (Arctic Council, Arctic
 *   Economic Council) perform coordination but lack enforcement mechanisms;
 *   state sovereignty routinely overrides environmental protection and
 *   Indigenous consent protocols. The constraint's extractiveness has
 *   increased over the decade (0.32 → 0.58) as climate-driven ice melt opens
 *   previously inaccessible resources and geopolitical competition among
 *   Arctic rim states intensifies. Theater ratio remains moderate (0.48) —
 *   genuine coordination functions exist (maritime navigation, infrastructure
 *   development) alongside performative elements (consultation processes,
 *   environmental impact assessments that do not halt extraction). The
 *   constraint is not purely extractive (not a Snare from all perspectives)
 *   because legitimate coordination benefits flow to multiple actors; nor is
 *   it pure coordination (not a Rope) because asymmetric power and structural
 *   traps create extraction mechanisms that concentrate costs on powerless
 *   agents.
 *
 * KEY AGENTS:
 *   - Indigenous Arctic Communities: Primary victim (powerless/trapped) — geographic location, colonial institutional inheritance, and resource dependence eliminate exit options; bear direct costs of environmental extraction
 *   - Northern Regional Governments: Secondary actor (moderate/constrained) — coordinate resource development while remaining subordinate to federal authorities and international treaties; constrained by Indigenous rights claims and capital pressure
 *   - Arctic Rim States & Extractive Industries: Primary beneficiary (institutional/arbitrage) — capture economic value, territorial claims, and strategic advantage; experience constraint as coordination mechanism; possess arbitrage through treaty renegotiation and capital mobility
 *   - Global Supply Chains & Consumer Markets: Secondary beneficiary (powerful/mobile) — benefit from low-cost Arctic resources; possess supply-chain alternatives but prefer Arctic access for cost and strategic reasons
 *   - International Indigenous Rights & Climate Movements: Organized victim (organized/constrained) — coordinate alternative governance models and accountability mechanisms; strategically constrained by state sovereignty and capital power
 *   - International Arctic Governance Framework: Institutional theater (institutional/arbitrage) — Arctic Council and treaties provide legitimacy and coordination appearance; enforcement weak; state sovereignty and capital pressure override environmental and Indigenous protections
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing state sovereignty and geopolitical competition as immutable laws; must critically examine whether thermodynamic inevitability masks contingent institutional arrangements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(arctic_resource_competition, 0.58).
domain_priors:suppression_score(arctic_resource_competition, 0.65).
domain_priors:theater_ratio(arctic_resource_competition, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(arctic_resource_competition, extractiveness, 0.58).
narrative_ontology:constraint_metric(arctic_resource_competition, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(arctic_resource_competition, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(arctic_resource_competition, tangled_rope).
narrative_ontology:human_readable(arctic_resource_competition, "Arctic Resource Competition and Geopolitical Extraction").
narrative_ontology:topic_domain(arctic_resource_competition, "geopolitical/economic/environmental").

domain_priors:requires_active_enforcement(arctic_resource_competition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(arctic_resource_competition, northern_extractive_industries).
narrative_ontology:constraint_beneficiary(arctic_resource_competition, arctic_rim_states).
narrative_ontology:constraint_beneficiary(arctic_resource_competition, military_strategic_interests).
narrative_ontology:constraint_victim(arctic_resource_competition, indigenous_arctic_communities).
narrative_ontology:constraint_victim(arctic_resource_competition, ecosystem_integrity).
narrative_ontology:constraint_victim(arctic_resource_competition, global_climate_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIGENOUS ARCTIC COMMUNITIES (SNARE) — Structurally trapped by geographic location, limited economic alternatives, and colonial-era institutional inheritance. Bear full costs of resource extraction (environmental degradation, cultural disruption, health impacts) while excluded from decision-making. Zero meaningful exit options. Maximum experienced extraction with high suppression.
constraint_indexing:constraint_classification(arctic_resource_competition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: NORTHERN REGIONAL GOVERNMENTS (TANGLED ROPE) — Coordinate infrastructure development and resource access (genuine coordination function) while extracting value through licensing, taxation, and environmental concessions. Constrained by federal authority, international agreements, and Indigenous rights claims. Experience both coordination benefits and extraction pressure from extractive industries and federal authorities.
constraint_indexing:constraint_classification(arctic_resource_competition, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXTRACTIVE INDUSTRIES & ARCTIC RIM STATES (ROPE) — Primary beneficiaries. Experience the constraint as pure coordination: establishing territorial claims, securing resource access, developing infrastructure, coordinating maritime routes. Arbitrage available through political influence, capital mobility, and international treaty renegotiation. Net extraction flows toward this agent. Low suppression experienced from their structural position.
constraint_indexing:constraint_classification(arctic_resource_competition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GLOBAL SUPPLY CHAIN & CONSUMER MARKETS (TANGLED ROPE) — Benefit from access to Arctic rare earth minerals, oil, and seafood at prices that do not reflect extraction costs borne by Arctic communities and ecosystems. Possess mobility (can source from alternatives) but benefit from Arctic access. Extraction is present but not maximal — supply chain alternatives exist, but Arctic resources are preferred for cost and strategic reasons.
constraint_indexing:constraint_classification(arctic_resource_competition, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INDIGENOUS RIGHTS & CLIMATE MOVEMENTS (TANGLED ROPE) — Organized agents (UN bodies, NGOs, Indigenous organizations) coordinate alternative governance models and climate accountability mechanisms while remaining constrained by state sovereignty and capital power. Limited exit options but strategic leverage through narrative framing and coalition-building. Moderate extraction experienced as political marginalization.
constraint_indexing:constraint_classification(arctic_resource_competition, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL ARCTIC GOVERNANCE (PITON) — Arctic Council, international treaties, and sovereignty agreements create a performative coordination layer. Theater ratio high (agreements signed, committees established, consultation processes initiated) but functional enforcement is weak — state sovereignty and capital pressure routinely override environmental and Indigenous protections. Mechanism persists through inertia and legitimacy theater rather than actual coordination capacity.
constraint_indexing:constraint_classification(arctic_resource_competition, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / THERMODYNAMIC VIEW (MOUNTAIN) — From a civilizational scale, Arctic resource competition appears immutable: planetary energy systems and economic structures necessarily drive expansion into frontier regions; climate-driven ice melt opens access to previously isolated resources; great-power competition for geopolitical advantage is an inherent feature of the international system. This perspective risks naturalizing contingent institutional arrangements (state sovereignty, colonial extraction models, market-driven resource valuation) as laws of geopolitics. The engine should flag this as a false summit.
constraint_indexing:constraint_classification(arctic_resource_competition, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(arctic_resource_competition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(arctic_resource_competition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(arctic_resource_competition, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(arctic_resource_competition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(arctic_resource_competition, TR),
    TR >= 0.70.

:- end_tests(arctic_resource_competition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The primary beneficiaries (extractive industries, Arctic rim states) capture economic value from resource access, territorial claims, and supply-chain positioning. The extraction is significant but not maximal (0.66+) because coordination functions are genuine and supply alternatives exist at higher cost. The measured value reflects the asymmetry: beneficiaries experience net positive return; victims experience net negative return. The trajectory from 0.32 to 0.58 reflects climate-driven acceleration — as ice melt opens resources, extraction intensity increases. Suppression (0.65): High. Multiple barriers constrain exit for Indigenous communities and ecosystem actors: geographic immobility, economic dependency on resource-extraction employment or subsistence resources, colonial institutional structures that subordinate Indigenous governance, international treaties that prioritize state sovereignty over Indigenous consent, and the physical constraint that Arctic regions are specific places with fixed inhabitants. Suppression is not total (0.80+) because international movements and Indigenous organizations have built political leverage through coalition-building and narrative framing. Theater ratio (0.48): Moderate. Substantial genuine coordination occurs (maritime routing, infrastructure development, resource access protocols), so theater is not dominant. However, significant performative elements exist: Arctic Council consultation processes often lack enforcement; environmental impact assessments proceed despite negative findings; Indigenous consent frameworks are consultation theater when state sovereignty overrides them. Theater has increased slightly (0.35 → 0.48) as governance institutions have proliferated without increasing enforcement capacity.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates divergent classification across power positions and exit options. Beneficiaries with arbitrage options (extractive industries, Arctic rim states) classify it as Rope — a coordination mechanism enabling economic development and geopolitical positioning. Organized but constrained agents (Indigenous movements, climate coalitions) classify it as Tangled Rope — genuine coordination exists alongside extraction that harms their interests; they possess agency but face structural barriers. Powerless, trapped agents (Indigenous communities dependent on Arctic subsistence and colonial governance structures) classify it as Snare — extraction with minimal coordination benefit and no exit option. The international governance framework classifies as Piton — performative coordination persisting through institutional inertia despite weak enforcement. The largest perspectival gap is between the beneficiary view (coordination enabling rational economic development) and the trapped victim view (extraction with no escape). The analytical observer risks false summit classification by naturalizing state sovereignty and geopolitical competition as immutable, when the actual structure is contingent on colonial institutions, market-driven valuation, and political power distribution.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural relationship to the constraint. Arctic rim states and extractive industries are beneficiaries with arbitrage options — they can shift capital and operations to alternative regions if Arctic extraction becomes unprofitable, but they benefit from Arctic access; their d value is low (0.10-0.20), producing negative or near-zero χ (they experience the constraint as beneficial). Northern regional governments are secondary beneficiaries with constrained exit — they depend on extraction revenue but are subordinate to federal authorities; their d value is moderate (0.35-0.45). Indigenous Arctic communities are victims with zero exit options (trapped) — they cannot relocate without losing cultural continuity and subsistence resources; their d value is high (0.90-0.95), producing maximum χ (maximum experienced extraction). International Indigenous movements are organized but constrained victims — they possess political leverage but face state sovereignty barriers; their d value is moderate-high (0.60-0.70). The global supply chain is a mobile beneficiary with weak preference for Arctic access — they could source elsewhere at higher cost; their d value is low-moderate (0.25-0.35). The piton institutional actor (Arctic governance framework) has arbitrage (can be bypassed if state cooperation withdraws) and experiences no extraction; d approaches zero.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that Arctic resource competition exhibits BOTH genuine coordination (establishing maritime safety, infrastructure development, resource allocation among competing claimants) AND asymmetric extraction (concentrated benefits for extractive industries and rim states, concentrated costs for Indigenous communities and global ecosystem). This combination defines Tangled Rope: the constraint cannot be classified as pure coordination (Rope) because the extraction is real and structural, not ephemeral. It cannot be classified as pure extraction (Snare) because the coordination functions are non-trivial — maritime routing, infrastructure development, and supply-chain coordination require cooperation and generate genuine collective benefits. The mandatrophy resolution is the explicit recognition that the constraint solves multiple problems simultaneously: (1) allocating frontier resources among competing claimants (coordination), (2) enabling extraction of value from those resources (extraction), (3) maintaining state sovereignty as the allocative principle (political choice that enables extraction by centralizing decision-making). The false natural law emerges when the analytical observer treats state sovereignty and geopolitical competition as immutable (Mountain perspective), when they are actually contingent institutional arrangements whose alternatives (stewardship-based commons, Indigenous co-governance, planetary climate prioritization) are structurally possible but politically resisted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indigenous_governance_capacity,
    'Do Indigenous governance models possess genuine veto power over resource extraction, or are they structurally subordinate to state sovereignty?',
    'Longitudinal analysis of Indigenous consent frameworks in Canada, Greenland, Norway, and Russia; comparison of Indigenous-led governance outcomes vs state-imposed consultation models',
    'If genuine veto: classification shifts from Snare (powerless) to Tangled Rope (organized). If subordinate: Indigenous communities remain trapped regardless of formal governance inclusion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(indigenous_governance_capacity, empirical, 'Whether Indigenous governance frameworks provide structural veto power').

omega_variable(
    climate_tipping_point_externality,
    'Does Arctic resource extraction accelerate climate tipping points whose costs exceed extraction value even at current market prices?',
    'Integration of climate impact modeling, permafrost thaw feedback cascades, and economic valuation of ecosystem services; comparison of extraction value against climate externality costs',
    'If true: suppression value should increase (ecosystem victims constrained by physical limits, not just political barriers). Tangled Rope classification solidifies. If false: extraction may be rational within market logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_tipping_point_externality, empirical, 'Whether Arctic extraction triggers climate tipping cascades exceeding value').

omega_variable(
    alternative_supply_pathway_sufficiency,
    'Can global demand for Arctic resources (rare earths, oil, seafood) be met through non-Arctic sources and circular economy mechanisms?',
    'Comparative analysis of rare earth mining alternatives (recycling, lateral sources), synthetic fuel viability, aquaculture and sustainable fishing capacity, renewable energy substitution',
    'If sufficient alternatives exist: Arctic rim states and industries experience high exit costs (losing monopoly rent). If alternatives insufficient: extraction logic is economically rational, shifting analysis from power asymmetry to coordination problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_supply_pathway_sufficiency, empirical, 'Whether non-Arctic supply pathways can satisfy global resource demand').

omega_variable(
    indigenous_identity_lock_versus_material_trap,
    'Are Indigenous Arctic communities trapped by material barriers (economic dependency, geographic isolation, colonial institutions) or identity-locked (self-conception constituted through Arctic belonging, cultural practice, and territorial relationship)?',
    'Qualitative analysis of Indigenous decision-making narratives; distinction between exit barriers (economic, legal, logistical) and identity barriers (cultural continuity, place-based worldview, relational ontology)',
    'If material trap: exit_options should be ''trapped'' (structural immobility). If identity-locked: exit_options should be ''identity_locked'' (structural mobility, cognitive immobility). Different mechanism suggests different intervention logic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(indigenous_identity_lock_versus_material_trap, conceptual, 'Whether Indigenous Arctic trap is material or identity-constituted').

omega_variable(
    state_sovereignty_versus_planetary_commons,
    'Can Arctic governance transition from state-centric sovereignty model to stewardship-based planetary commons model, or is state sovereignty an immutable structural feature of the international system?',
    'Historical precedent analysis (Antarctic Treaty, deep-sea mining agreements, atmospheric commons); modeling of coalition-formation costs for sovereignty transition; counterfactual analysis of institutional alternatives',
    'If transition possible: constraint''s structure is contingent and negotiable; piton classification is diagnostic (governance framework is performative precisely because alternatives haven''t crystallized yet). If sovereignty immutable: mountain perspective contains truth (geopolitical extraction is inherent). If uncertain: stays in tangled_rope/piton range.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_sovereignty_versus_planetary_commons, conceptual, 'Whether state sovereignty can transition to planetary commons stewardship').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(arctic_resource_competition, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arctic_tr_t0, arctic_resource_competition, theater_ratio, 0, 0.35).
narrative_ontology:measurement(arctic_tr_t5, arctic_resource_competition, theater_ratio, 5, 0.42).
narrative_ontology:measurement(arctic_tr_t10, arctic_resource_competition, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(arctic_be_t0, arctic_resource_competition, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(arctic_be_t5, arctic_resource_competition, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(arctic_be_t10, arctic_resource_competition, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(arctic_resource_competition, resource_allocation).
narrative_ontology:affects_constraint(arctic_resource_competition, indigenous_land_sovereignty).
narrative_ontology:affects_constraint(arctic_resource_competition, climate_tipping_points).
narrative_ontology:affects_constraint(arctic_resource_competition, geopolitical_great_power_competition).
narrative_ontology:affects_constraint(arctic_resource_competition, rare_earth_supply_dependency).

% DUAL FORMULATION NOTE:
% Arctic resource competition decomposes into multiple structurally distinct constraints: (1) resource allocation coordination (how to divide frontier resources — ε~0.20, Rope), (2) Indigenous land sovereignty (structural entrapment of Indigenous communities — ε~0.75, Snare), (3) climate externality extraction (shifting climate costs onto global south and future generations — ε~0.65, Tangled Rope), (4) geopolitical competition (state competition for strategic positioning — ε~0.45, Tangled Rope). This story models the constraint family collectively; decomposed stories are available for domain-specific analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(arctic_resource_competition, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
