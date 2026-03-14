% ============================================================================
% CONSTRAINT STORY: roman_aqueduct_infrastructure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_roman_aqueduct_infrastructure, []).

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
 *   constraint_id: roman_aqueduct_infrastructure
 *   human_readable: Roman Aqueduct Infrastructure: Coordination and Extraction in Water Distribution
 *   domain: infrastructure/political_economy/engineering
 *
 * SUMMARY:
 *   Roman aqueduct infrastructure represents a foundational case study in how
 *   a single coordination mechanism becomes embedded with asymmetric
 *   extraction across different social strata. The aqueducts genuinely solved
 *   an engineering problem — moving water across long distances using gravity
 *   and engineered channels — and this solution enabled urban concentration,
 *   public health improvements, and military logistics. But the same
 *   infrastructure that coordinates water distribution also extracts value
 *   from rural populations (water diversion), from enslaved laborers
 *   (permanent maintenance duty), and from non-elite urban populations
 *   (controlled access through magistrates). The constraint exhibits all six
 *   DR types depending on observation position: pure extraction for trapped
 *   laborers and rural populations, mixed coordination-extraction for
 *   moderate urban dwellers, pure coordination for imperial administration,
 *   degraded ritual for post-imperial populations. The theater ratio rises
 *   dramatically in the late imperial and post-imperial periods as the
 *   political-administrative system that justified the infrastructure
 *   collapses, yet the physical structures persist through institutional
 *   inertia.
 *
 * KEY AGENTS:
 *   - Imperial Administration: Primary beneficiary (institutional/arbitrage) — controls water allocation, uses aqueducts for military logistics, tax collection, and demonstrating imperial power
 *   - Urban Elites (patricians, wealthy merchants): Primary beneficiary (powerful/arbitrage) — secure reliable water supply, prestige projects, control over local water distribution and pricing
 *   - Provincial Rural Populations: Primary victim (powerless/trapped) — water sources diverted to urban centers; no alternative access; cannot exit regional dependency
 *   - Enslaved Labor and Corvée Workers: Primary victim (powerless/trapped) — supply perpetual maintenance labor; trapped in legal status; no exit option
 *   - Urban Non-Elite Populations: Secondary actor (moderate/constrained) — benefit from public water access (coordination function) but face controlled distribution and cannot opt out of urban dependency
 *   - Provincial Municipalities and Local Elites: Secondary beneficiary-victim (powerful/mobile) — benefit from prestige and urban growth but constrained by imperial water policy directives
 *   - Post-Imperial Communities: Institutional actor (institutional/arbitrage) — inherit aqueduct infrastructure; maintain through inertia after coordination function degrades
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(roman_aqueduct_infrastructure, 0.38).
domain_priors:suppression_score(roman_aqueduct_infrastructure, 0.52).
domain_priors:theater_ratio(roman_aqueduct_infrastructure, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(roman_aqueduct_infrastructure, extractiveness, 0.38).
narrative_ontology:constraint_metric(roman_aqueduct_infrastructure, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(roman_aqueduct_infrastructure, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(roman_aqueduct_infrastructure, tangled_rope).
narrative_ontology:human_readable(roman_aqueduct_infrastructure, "Roman Aqueduct Infrastructure: Coordination and Extraction in Water Distribution").
narrative_ontology:topic_domain(roman_aqueduct_infrastructure, "infrastructure/political_economy/engineering").

domain_priors:requires_active_enforcement(roman_aqueduct_infrastructure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(roman_aqueduct_infrastructure, urban_elites).
narrative_ontology:constraint_beneficiary(roman_aqueduct_infrastructure, imperial_administration).
narrative_ontology:constraint_beneficiary(roman_aqueduct_infrastructure, military_water_users).
narrative_ontology:constraint_victim(roman_aqueduct_infrastructure, provincial_populations).
narrative_ontology:constraint_victim(roman_aqueduct_infrastructure, rural_water_access).
narrative_ontology:constraint_victim(roman_aqueduct_infrastructure, maintenance_labor).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROVINCIAL RURAL POPULATIONS (SNARE) — Water diverted to urban centers through aqueduct infrastructure eliminates local water autonomy. Rural communities trapped by resource dependency; cannot exit or organize collective resistance. Full extraction with minimal coordination benefit — water infrastructure serves distant urban centers, not local populations. Suppression enforced through administrative control and military power.
constraint_indexing:constraint_classification(roman_aqueduct_infrastructure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MAINTENANCE AND SLAVE LABOR (SNARE) — Aqueduct construction and maintenance demands massive labor force, sustained through slavery and corvée obligations. Workers trapped in perpetual maintenance duty with no exit option and minimal benefit. Pure extraction — labor extracted for infrastructure benefiting others. High suppression through legal status and coercive punishment.
constraint_indexing:constraint_classification(roman_aqueduct_infrastructure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: URBAN NON-ELITE POPULATIONS (TANGLED ROPE) — Urban commons benefit from aqueduct-supplied water — public fountains, bathing facilities — enabling public health and social coordination. Genuine coordination function exists. But extraction embedded: access controlled through magistrates, water rights commodified, poor populations distant from premium water distribution. Constrained exit — without aqueduct infrastructure, urban living becomes infeasible; cannot opt out. Mixed benefit and cost.
constraint_indexing:constraint_classification(roman_aqueduct_infrastructure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: IMPERIAL ADMINISTRATION AND URBAN ELITES (ROPE) — Aqueducts are perceived as pure coordination: solving the engineering problem of water supply to cities, enabling urban growth, supporting military logistics, facilitating tax collection and administrative control. Net beneficiaries with arbitrage options — can redirect aqueducts, reallocate water, reposition power. Experience the constraint as coordination mechanism, not extraction.
constraint_indexing:constraint_classification(roman_aqueduct_infrastructure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: PROVINCIAL ELITES AND MUNICIPALITIES (TANGLED ROPE) — Local elites benefit from prestige aqueduct projects and enhanced urban status but are constrained by imperial direction of resources and water allocation. Genuine coordination with asymmetric extraction — local elites gain authority and population concentration but remain subordinate to imperial water policy. Mobile exit options (can relocate, build alternative infrastructure) but strategically constrained by imperial authority.
constraint_indexing:constraint_classification(roman_aqueduct_infrastructure, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: POST-IMPERIAL TECHNICAL INERTIA (PITON) — From civilizational perspective, Roman aqueducts persist through institutional inertia long after the administrative system they served collapses. Medieval populations maintain aqueducts not for imperial water policy but because the infrastructure exists and carries water — the functional purpose degrades into performative maintenance. Theater ratio rises in post-imperial period as maintenance becomes ritualized rather than effective.
constraint_indexing:constraint_classification(roman_aqueduct_infrastructure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, gravity-fed water distribution through engineered channels is an immutable natural law of hydraulic engineering: moving water from source to distant sink requires understanding gradient, pressure, and friction loss. This perspective risks naturalizing the contingent political arrangements (who controls the water, who pays, who benefits) as inherent to 'what infrastructure is.' The engine will flag this as false summit.
constraint_indexing:constraint_classification(roman_aqueduct_infrastructure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(roman_aqueduct_infrastructure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(roman_aqueduct_infrastructure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(roman_aqueduct_infrastructure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(roman_aqueduct_infrastructure, TR),
    TR >= 0.70.

:- end_tests(roman_aqueduct_infrastructure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint exhibits genuine coordination (solving water distribution engineering problem, enabling urban sanitation) but embeds systematic extraction (diversion of rural water, unpaid maintenance labor, control of public water access through elites). The mid-range value reflects that neither pure coordination nor pure extraction accurately describes the constraint. Extractiveness rises from 0.22 at the constraint's origin (early imperial period, when coordination function dominates and infrastructure is novel) to 0.41-0.45 in later periods as administrative overhead and rent-extraction accumulate. Suppression (0.52): Moderate-high. Significant barriers to exit include administrative control over water allocation, legal status of enslaved laborers, geographic dependence on aqueduct-supplied regions, and military enforcement of imperial property claims. But suppression is not absolute — some alternative water technologies (wells, local channels) persist, and some populations can relocate. Theater ratio (0.35): Low-moderate in classical period (infrastructure is functionally maintained, genuinely solves coordination problem), but rises to 0.72 in post-imperial period (maintenance becomes ritualized, functional purpose degraded to preserving inherited structure).
 *
 * PERSPECTIVAL GAP:
 *   The original research group sees coordination (Rope) — they are solving the legitimate problem of communicating findings. The open science coalition sees a temporary problem with a sunset (Scaffold) — arXiv and registered reports are building alternative pathways. The journal editorial system sees its own degraded ritual (Piton) — peer review persists through inertia, not function. Replication groups see mixed coordination and extraction (Tangled Rope) — the system both enables and constrains their work. The field's epistemic reliability sees pure extraction (Snare) — premature claims contaminate the literature with no self-correction mechanism. The civilizational analytical observer risks seeing an immutable natural law (Mountain) — verification lag is inherent to science — but the structural data reveals this as a false summit: the contingent institutional arrangements (career incentives, funding concentration, publication bias) are not laws of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural position. Enslaved laborers are full victims with no exit: d ≈ 0.95 → high extraction experienced. Rural populations are victims with trapped exit: d ≈ 0.92 → high extraction. Urban non-elites are partial beneficiaries but constrained: d ≈ 0.55 → moderate extraction. Provincial elites are partial beneficiaries with mobile exit: d ≈ 0.48 → near-symmetric experience. Imperial administration are full beneficiaries with arbitrage options: d ≈ 0.05 → minimal/negative extraction (coordination benefits them). The perspectival gap between enslaved laborers (d ≈ 0.95) and imperial administration (d ≈ 0.05) is 0.90 — the maximum possible range. This generates the full spectrum of classification types from a single constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that tangled rope classification is correct at the aggregate level precisely because the constraint contains genuine coordination (water distribution engineering, public sanitation) alongside systematic extraction (labor coercion, water diversion, access control). The mandatrophy error would be to classify this as pure rope (ignoring the extraction) or pure snare (ignoring the coordination benefit). The tangled rope classification correctly models that the same infrastructure that solves a real coordination problem simultaneously enables asymmetric extraction from different social strata. The false mountain perspective (naturalizing water engineering as immutable law) is explicitly revealed as false because the coordination function depends entirely on contingent political arrangements (imperial control, administrative allocation) that could be organized differently.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    water_rights_ownership_ambiguity,
    'Is water treated as a natural commons with customary access rights, or as imperial property subject to administrative allocation?',
    'Analysis of provincial water law, edicts on water rights, litigation records over water access disputes; comparison of written law vs local practice',
    'If commons: extractiveness lower (0.25-0.30), rope classification more legitimate. If imperial property: extractiveness higher (0.55-0.65), snare classification dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(water_rights_ownership_ambiguity, empirical, 'Whether water is commons or imperial property').

omega_variable(
    coordination_benefit_distribution,
    'How much public health and sanitation benefit flows to non-elite urban populations vs private villa supply for elites?',
    'Archaeological survey of aqueduct terminus points, water storage capacity, fountain location mapping; epidemiological data on water-borne disease rates in supplied vs non-supplied areas',
    'If benefit concentrated (>70% to elites): tangled rope classification confirmed at snare intensity. If distributed (40-60%): genuine tangled rope with more legitimate coordination component.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_benefit_distribution, empirical, 'Distribution of public health benefits across social strata').

omega_variable(
    labor_coercion_mechanism_intensity,
    'What proportion of aqueduct maintenance labor is enslaved vs free wage labor vs corvée obligation?',
    'Analysis of construction records, labor inscriptions, manumission data, correlation of aqueduct expansion with slave import rates and military campaigns',
    'If >80% enslaved: maintenance labor is pure snare (0.85+ extractiveness). If >50% free/wage: labor becomes more constrained than trapped, shifting to tangled rope (0.45-0.55).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_coercion_mechanism_intensity, empirical, 'Proportion of enslaved vs free labor in aqueduct maintenance').

omega_variable(
    provincial_water_access_baseline,
    'What water access did provincial populations have before aqueduct construction in their region?',
    'Geological survey of local water sources, settlement patterns pre- and post-aqueduct, changes in population distribution, analysis of alternative water technology (wells, cisterns, local channels)',
    'If robust local access displaced: strong victimization narrative. If aqueduct enabled settlement in water-poor regions: genuine coordination function emerges, tangled rope classification strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(provincial_water_access_baseline, empirical, 'Pre-aqueduct water access baseline in provinces').

omega_variable(
    imperial_vs_local_control_boundary,
    'What degree of local control do municipalities exercise over aqueduct routing, water allocation, and maintenance priorities?',
    'Analysis of municipal charters, aqueduct management records, provincial edicts on water policy; case studies of conflicts between imperial procurators and local magistrates over water',
    'If imperial control absolute: provincial elites experience snare (0.70+). If meaningful local authority: provincial elites experience tangled rope (0.40-0.60) or even rope (0.20-0.35).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(imperial_vs_local_control_boundary, empirical, 'Degree of municipal autonomy in aqueduct management').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(roman_aqueduct_infrastructure, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aqua_tr_t0, roman_aqueduct_infrastructure, theater_ratio, 0, 0.18).
narrative_ontology:measurement(aqua_tr_t75, roman_aqueduct_infrastructure, theater_ratio, 75, 0.32).
narrative_ontology:measurement(aqua_tr_t150, roman_aqueduct_infrastructure, theater_ratio, 150, 0.35).
narrative_ontology:measurement(aqua_tr_t200, roman_aqueduct_infrastructure, theater_ratio, 200, 0.72).

% Extraction over time
narrative_ontology:measurement(aqua_be_t0, roman_aqueduct_infrastructure, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(aqua_be_t75, roman_aqueduct_infrastructure, base_extractiveness, 75, 0.38).
narrative_ontology:measurement(aqua_be_t150, roman_aqueduct_infrastructure, base_extractiveness, 150, 0.41).
narrative_ontology:measurement(aqua_be_t200, roman_aqueduct_infrastructure, base_extractiveness, 200, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(roman_aqueduct_infrastructure, resource_allocation).
narrative_ontology:affects_constraint(roman_aqueduct_infrastructure, roman_military_logistics).
narrative_ontology:affects_constraint(roman_aqueduct_infrastructure, provincial_tribute_extraction).
narrative_ontology:affects_constraint(roman_aqueduct_infrastructure, urban_sanitation_public_health).

% DUAL FORMULATION NOTE:
% Roman aqueduct infrastructure operates as a unified physical system but contains three structurally distinct constraints: (1) water engineering coordination (ε ≈ 0.08, rope), (2) resource allocation to elites (ε ≈ 0.55, snare), and (3) labor extraction for maintenance (ε ≈ 0.72, snare). This story represents the aggregate constraint (tangled rope, ε = 0.38) that emerges from these components. Full decomposition into separate constraint stories would enable precise tracking of how the coordination function and extraction mechanisms evolved independently over the 200-year interval.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(roman_aqueduct_infrastructure, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
