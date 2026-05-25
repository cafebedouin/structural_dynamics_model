% ============================================================================
% CONSTRAINT STORY: dublin_nocturnal_infrastructure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dublin_nocturnal_infrastructure, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dublin_nocturnal_infrastructure
 *   human_readable: Dublin Nocturnal Infrastructure Constraint
 *   domain: urban_planning/municipal_governance
 *
 * SUMMARY:
 *   Dublin's nocturnal infrastructure constraint represents a structural
 *   tension between economic development through night-time leisure and
 *   hospitality activity, and residential amenity, public health, and worker
 *   protection. The constraint is the set of licensing, enforcement, and
 *   zoning rules that permit and regulate late-night venues, transport, and
 *   street activity. It exhibits all six DR types from different structural
 *   positions, creating a complex perspectival field. The extractiveness
 *   trajectory shows that the constraint has intensified over the measurement
 *   interval as the night-economy has grown faster than mitigating
 *   infrastructure (soundproofing, electric transport, noise regulation).
 *   Theater_ratio (0.68) reflects that municipal enforcement and licensing
 *   apparatus is substantially performative — stated noise and operational
 *   standards are selectively enforced, with tourist-facing areas receiving
 *   higher compliance pressure than residential districts. The constraint
 *   embeds genuine coordination (late-night transport coordination, venue
 *   licensing for public safety) alongside asymmetric extraction (residents
 *   bear noise and health costs; hospitality sector captures revenue). No
 *   single type describes the full constraint from the city's perspective.
 *
 * KEY AGENTS:
 *   - Night Shift Workers and Residential Communities: Primary victims (powerless/trapped) — trapped by Dublin economic dependence and housing market constraints; suppression is maximal; no coordination benefit
 *   - Evening Hospitality and Tourism Sector: Primary beneficiaries (institutional/arbitrage) — capture economic rents from licensed venues, tourism spending, and employment; experience constraint as coordination mechanism; low extraction experience due to high exit optionality
 *   - Emergency and Health Services: Secondary victims (moderate/constrained) — constrained by budget and nocturnal demand patterns; experience mixed coordination benefit and extractive strain; reduced capacity during peak entertainment hours creates safety costs
 *   - Municipal Government and Economic Development Authority: Mixed actor (powerful/mobile) — benefits from tax revenue and tourism metrics; bears costs of service strain and political pressure; mobile exit but constrained by competitive pressure from peer cities
 *   - Urban Planning and Environmental Advocates: Organized reformers (organized/constrained) — pushing sunset-clause alternatives (quieter technology, mixed-use zoning, dispersed entertainment districts); constrained by hospitality political opposition; see 10-15 year transition pathway
 *   - Municipal Licensing and Enforcement System: Institutional system (institutional/arbitrage) — performs licensing and inspection roles; sees own process as degraded; enforcement is selective and underfunded relative to stated standards
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements (zoning, licensing, enforcement priorities) as inherent features of urban density
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dublin_nocturnal_infrastructure, 0.58).
domain_priors:suppression_score(dublin_nocturnal_infrastructure, 0.65).
domain_priors:theater_ratio(dublin_nocturnal_infrastructure, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dublin_nocturnal_infrastructure, extractiveness, 0.58).
narrative_ontology:constraint_metric(dublin_nocturnal_infrastructure, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(dublin_nocturnal_infrastructure, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dublin_nocturnal_infrastructure, tangled_rope).
narrative_ontology:human_readable(dublin_nocturnal_infrastructure, "Dublin Nocturnal Infrastructure Constraint").
narrative_ontology:topic_domain(dublin_nocturnal_infrastructure, "urban_planning/municipal_governance").

domain_priors:requires_active_enforcement(dublin_nocturnal_infrastructure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dublin_nocturnal_infrastructure, evening_hospitality_sector).
narrative_ontology:constraint_beneficiary(dublin_nocturnal_infrastructure, late_night_transport_operators).
narrative_ontology:constraint_beneficiary(dublin_nocturnal_infrastructure, tourism_promotion_bodies).
narrative_ontology:constraint_victim(dublin_nocturnal_infrastructure, night_shift_workers).
narrative_ontology:constraint_victim(dublin_nocturnal_infrastructure, residential_communities).
narrative_ontology:constraint_victim(dublin_nocturnal_infrastructure, public_health_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NIGHT SHIFT WORKERS AND RESIDENTS (SNARE) — Trapped by geographic and economic dependence on Dublin employment. Cannot exit without major life disruption. Suppression is maximal: noise ordinances are selectively enforced, complaints are deprioritized, alternative quiet housing is prohibitively expensive. No coordination function benefits these agents — the constraint extracts from them with minimal reciprocal benefit.
constraint_indexing:constraint_classification(dublin_nocturnal_infrastructure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: EMERGENCY AND HEALTH SERVICES (TANGLED ROPE) — Constrained by municipal budget allocations and nocturnal demand patterns. The constraint coordinates genuine collective action (ambulance routing, hospital night operations) but embeds asymmetric extraction: services must operate at reduced staffing during peak entertainment hours, creating safety bottlenecks. Some benefit through visibility of night-economy contribution to GDP; significant costs through resource strain.
constraint_indexing:constraint_classification(dublin_nocturnal_infrastructure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EVENING HOSPITALITY AND TOURISM SECTOR (ROPE) — Benefits substantially from nocturnal infrastructure provision. Experiences the constraint as coordination mechanism: licensed venues, late-night transport, street lighting enable profitable operations. Primary beneficiary with high exit optionality (can relocate venue, franchise, or expand) — experiences low or negative effective extraction. The coordination serves this group's interests.
constraint_indexing:constraint_classification(dublin_nocturnal_infrastructure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: URBAN PLANNING AND ENVIRONMENTAL ADVOCATES (SCAFFOLD) — Organized agents pushing for 24-hour mixed-use zoning, noise impact assessment, and sustainable night-economy models. See the constraint as temporary coordination failure with sunset logic: quieter technology (electric transport, sound insulation standards), dispersed entertainment districts, and temporal zoning can replace the current extraction mechanism. Sunset clause: 10-15 years for transitional building codes and transport infrastructure to mature. Constrained by political opposition from hospitality interests but gaining force through environmental regulations.
constraint_indexing:constraint_classification(dublin_nocturnal_infrastructure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: MUNICIPAL LICENSING AND ENFORCEMENT SYSTEM (PITON) — The licensing apparatus for late-night venues and street trading is substantially performative. Enforcement is selective (tourist districts receive higher compliance; residential areas lower), theater_ratio reflects that the stated noise and operational standards are rarely applied uniformly. The system persists through institutional inertia: licensing fees generate revenue, regulatory theater satisfies political pressure to 'do something,' but enforcement mechanisms (noise monitoring, compliance audits) are underfunded. The system sees itself as degraded — maintained because alternatives haven't fully replaced it, not because enforcement works.
constraint_indexing:constraint_classification(dublin_nocturnal_infrastructure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CITY GOVERNMENT AND ECONOMIC DEVELOPMENT AUTHORITY (TANGLED ROPE) — Faces genuine coordination dilemma: growing night-economy tax revenue and employment compete with public health and safety obligations. Benefits from hospitality tax revenue and tourism metrics; bears costs of health service strain, noise complaint administration, and political pressure from residents. Mobile exit (can shift to day-economy prioritization or impose stricter regulations) but does not exercise it due to competitive pressure from other European cities for night-economy tourism. Active enforcement of licensing and venue standards required to maintain the extraction flow, making this tangled_rope rather than rope.
constraint_indexing:constraint_classification(dublin_nocturnal_infrastructure, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the tension between population density, economic activity, and sleep/health quality appears as an immutable constraint of urban existence: dense cities necessarily generate noise and activity spillover; individual rest requirements compete with collective economic benefit. This framing naturalizes the constraint as inevitable. However, structural data reveals this as false summit — the extractive pattern is contingent on licensing policy, zoning rules, and enforcement choices, not laws of physics.
constraint_indexing:constraint_classification(dublin_nocturnal_infrastructure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dublin_nocturnal_infrastructure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dublin_nocturnal_infrastructure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dublin_nocturnal_infrastructure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dublin_nocturnal_infrastructure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dublin_nocturnal_infrastructure, TR),
    TR >= 0.70.

:- end_tests(dublin_nocturnal_infrastructure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from residents and health services (noise, sleep disruption, service strain) while providing no reciprocal coordination benefit to these agents. However, extractiveness is not maximal (0.66+) because the extraction relies on institutional cooperation (municipal licensing) that could be withdrawn — the beneficiary groups (hospitality, tourism) do not unilaterally control the constraint. The trajectory shows growth from 0.38 to 0.58 as night-economy expansion has outpaced mitigation infrastructure, increasing the extraction rate. Suppression (0.65): High. Residents face multiple suppression mechanisms: noise ordinances are selectively enforced; relocation options are economically blocked by housing markets and employment lock; complaint mechanisms are deprioritized; political voice is concentrated in hospitality lobbying. Suppression is not total (0.80+) because some residents successfully organize, enforcement does occur in some contexts, and regulatory pathways exist (though rarely used). Theater ratio (0.68): Moderate-high. The licensing and enforcement apparatus is substantially performative. Noise limits are stated but selectively enforced; venue safety inspections occur periodically but not continuously; compliance monitoring is episodic rather than systematic. The theater has increased over the interval as bureaucratic documentation (compliance certificates, operational plans) has proliferated without corresponding enforcement capacity. Real enforcement would require substantially more inspector hours, noise monitoring equipment, and political will to constrain hospitality revenue.
 *
 * PERSPECTIVAL GAP:
 *   The six-type perspectival gap reveals how institutional design choices become naturalized as inevitabilities. Residents experience a snare (maximal extraction); hospitality experiences rope (coordination with benefits); government experiences tangled rope (mixed); health services experience tangled rope (constrained benefit); reformers experience scaffold (solvable problem with sunset); the licensing system experiences piton (degraded ritual); the civilizational observer risks seeing mountain (inevitable). The gap between resident-snare and hospitality-rope is not a difference in perception — it is a structural difference in who the constraint serves. The gap between government-tangled rope and resident-snare reveals that what government calls 'balanced policy' is experienced as pure extraction by those bearing the costs. The gap between piton (licensing system sees itself as degraded) and snare (residents see extraction) reveals a key diagnostic: when the system that enforces a constraint sees its own enforcement as theatrical, those bearing the extraction costs experience that theater as oppressive.
 *
 * DIRECTIONALITY LOGIC:
 *   Each actor's experienced extractiveness (χ) is computed from base extractiveness (ε=0.58), directionality d (derived from power + exit + beneficiary/victim status), and scope modifier σ(S). Trapped victims experience d~0.95, producing f(d)~1.42 and χ~0.58×1.42×0.8 (local scope)~0.66 — among the highest experienced extraction. Institutional beneficiaries with arbitrage exit experience d~0.05, producing f(d)~-0.12 and χ~0.58×(-0.12)×1.2 (global scope)~-0.08 — negative effective extraction (they experience benefit). Organized constrained actors experience d~0.50-0.60, producing f(d)~0.65-0.85 and χ~0.58×0.75×1.0~0.44 — moderate extraction. No directionality overrides are needed; the structural beneficiary/victim declarations and exit options produce internally consistent d values.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the classification depends entirely on structural position, not on ambiguity in the constraint definition. From the resident's position: snare (ε=0.58, χ~0.66, suppression=0.65). From the hospitality sector's position: rope (ε=0.58, χ~-0.08, suppression=0.65 perceived as low due to high exit optionality). From municipal government's position: tangled_rope (mixed benefits and costs, requires active enforcement to maintain extraction, coordination function genuine but asymmetric). The 'mandatrophy' — apparent contradiction between types — is not a problem to resolve but a feature to explain: the constraint is simultaneously extractive (snare) from the perspective of those bearing costs, and coordinative (rope) from the perspective of those organizing the system. The analytical observer risks seeing mountain (inevitable feature of urban density) but the engine's false summit detector should flag this: the contingency on zoning policy, licensing rules, and enforcement choices proves the mountain classification is naturalization, not natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_motivation_ambiguity,
    'Are selective enforcement patterns (higher compliance in tourist areas, lower in residential) deliberate policy or resource-driven triage?',
    'Comparative audit of compliance violation reports by district; analysis of licensing inspector time allocation; interviews with enforcement officials about prioritization criteria',
    'If deliberate: system is intentionally extractive from residents (increases χ). If resource-driven: piton classification is correct — system is degraded, not malicious. If mixed: both extraction and inertia are present.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_motivation_ambiguity, empirical, 'Whether selective enforcement reflects policy choice or resource constraint').

omega_variable(
    resident_mobility_threshold,
    'What proportion of affected residents actually have economically viable exit options (relocate, change employment) versus those structurally trapped?',
    'Longitudinal analysis of residential mobility in high-noise areas; income correlation with relocation capacity; employment dependence on Dublin location',
    'If exit options are real: reclassify from trapped to constrained, reducing experienced χ. If exit is illusory (housing cost, employment lock): confirms trapped classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resident_mobility_threshold, empirical, 'Residential exit capacity for night-noise affected populations').

omega_variable(
    health_service_extraction_causality,
    'How much of emergency service strain is caused by nocturnal infrastructure constraint versus other factors (aging population, chronic disease prevalence)?',
    'Time-series analysis of emergency department presentation rates; correlation with specific night-economy growth periods; comparison with non-entertainment-focused urban areas of similar size',
    'If strong correlation: supports tangled_rope classification of health services as victim. If weak: health service classification may be spurious.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(health_service_extraction_causality, empirical, 'Causal link between nocturnal activity and health service strain').

omega_variable(
    alternative_revenue_substitution,
    'Could day-economy development, tourism outside night-hours (conferences, cultural events), and business services generate equivalent municipal tax revenue without the extraction cost?',
    'Comparative economic analysis with peer cities (Edinburgh, Barcelona) that emphasize day-economy tourism; modeling of tax revenue under alternative economic models',
    'If yes: municipal government has genuine exit option (mobile classification is correct). If no: government is constrained by revenue dependence (reclassify exit as constrained).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_revenue_substitution, empirical, 'Availability of revenue-equivalent alternative economic models').

omega_variable(
    technology_sunset_feasibility,
    'Can electric transport, noise-suppression infrastructure, and building soundproofing standards realistically reduce night-economy extractiveness within the 10-15 year scaffold sunset window?',
    'Engineering feasibility analysis; pilot projects in other cities; cost-benefit modeling of infrastructure investment versus benefit realization',
    'If feasible: scaffold perspective is structural (sunset is real). If not: scaffold is aspirational, and constraint may harden into snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technology_sunset_feasibility, empirical, 'Technical feasibility of urban planning sunset clause').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dublin_nocturnal_infrastructure, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dublin_nocturnal_tr_t0, dublin_nocturnal_infrastructure, theater_ratio, 0, 0.52).
narrative_ontology:measurement(dublin_nocturnal_tr_t3, dublin_nocturnal_infrastructure, theater_ratio, 3, 0.6).
narrative_ontology:measurement(dublin_nocturnal_tr_t6, dublin_nocturnal_infrastructure, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(dublin_nocturnal_be_t0, dublin_nocturnal_infrastructure, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(dublin_nocturnal_be_t3, dublin_nocturnal_infrastructure, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(dublin_nocturnal_be_t6, dublin_nocturnal_infrastructure, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dublin_nocturnal_infrastructure, resource_allocation).
narrative_ontology:affects_constraint(dublin_nocturnal_infrastructure, dublin_transport_monopoly).
narrative_ontology:affects_constraint(dublin_nocturnal_infrastructure, irish_housing_supply_constraint).

% DUAL FORMULATION NOTE:
% Dublin nocturnal infrastructure is downstream of both transport infrastructure (late-night LUAS and bus availability) and housing supply constraints (which make residential exit economically infeasible). This story models the constraint as a coordination-extraction hybrid at the licensing/enforcement level. Upstream stories model transport monopoly and housing supply separately, each with their own ε values reflecting different observables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
