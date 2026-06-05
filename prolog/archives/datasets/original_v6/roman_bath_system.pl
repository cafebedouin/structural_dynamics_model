% ============================================================================
% CONSTRAINT STORY: roman_bath_system
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_roman_bath_system, []).

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
 *   constraint_id: roman_bath_system
 *   human_readable: The Roman System of Public Baths
 *   domain: technological/social/economic
 *
 * SUMMARY:
 *   The Roman system of public baths (thermae) represents a complex
 *   institutional arrangement combining genuine coordination infrastructure
 *   with systematic wealth extraction and political legitimacy maintenance.
 *   Beginning in the 1st century CE and reaching peak sophistication in the
 *   2nd-3rd centuries, the bath system provided hygiene, recreation, and
 *   social integration for urban populations across the empire at heavily
 *   subsidized cost. The constraint operates simultaneously as a coordination
 *   mechanism (solving collective hygiene and social gathering), an
 *   extraction mechanism (concentrating provincial tax revenue to urban
 *   centers), a legitimacy mechanism (maintaining imperial popular support),
 *   and increasingly as theatrical performance (sinecures and prestige
 *   spending replacing functional infrastructure). The system's
 *   extractiveness increased over its interval as administrative overhead
 *   grew and functional efficiency declined, while its theater ratio rose as
 *   patronage and architectural prestige supplanted maintenance and
 *   operations. The constraint decomposed into distinct structural tensions:
 *   imperial administration required both genuine public goods provision and
 *   political control; provincial magistrates balanced civic amenity
 *   provision against other infrastructure needs; contractors shifted from
 *   technical roles to patronage positions; and the underlying aqueduct
 *   infrastructure faced mounting scarcity pressures that eventually
 *   undermined the system's sustainability.
 *
 * KEY AGENTS:
 *   - Imperial Administration: Primary beneficiary (organized/constrained) — derives political loyalty, tax extraction, and prestige from bath system
 *   - Provincial Taxpayers: Primary victim (powerless/trapped) — bear tax burden for bath subsidies with no exit
 *   - Urban Bath Users: Secondary beneficiary (powerless/mobile) — receive hygiene and social benefits from subsidized baths
 *   - Bath Contracting Elite: Institutional actor (institutional/arbitrage) — originally performed technical coordination, increasingly received patronage sinecures
 *   - Provincial Magistrates: Mediating actor (moderate/constrained) — must balance imperial mandates against local resource allocation
 *   - Water Infrastructure Coalition: Organized actors (organized/constrained) — manage aqueduct systems that support bath sustainability
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as urban necessities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(roman_bath_system, 0.35).
domain_priors:suppression_score(roman_bath_system, 0.25).
domain_priors:theater_ratio(roman_bath_system, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(roman_bath_system, extractiveness, 0.35).
narrative_ontology:constraint_metric(roman_bath_system, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(roman_bath_system, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(roman_bath_system, tangled_rope).
narrative_ontology:human_readable(roman_bath_system, "The Roman System of Public Baths").
narrative_ontology:topic_domain(roman_bath_system, "technological/social/economic").

domain_priors:requires_active_enforcement(roman_bath_system).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(roman_bath_system, imperial_authority).
narrative_ontology:constraint_beneficiary(roman_bath_system, urban_populace).
narrative_ontology:constraint_beneficiary(roman_bath_system, bath_contractors).
narrative_ontology:constraint_victim(roman_bath_system, provincial_tax_base).
narrative_ontology:constraint_victim(roman_bath_system, water_infrastructure_sustainability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROVINCIAL TAXPAYER (SNARE) — Bears the tax burden for bath construction and maintenance without meaningful alternative. Trapped within the imperial fiscal system. Extraction flows from provinces to urban centers through bath subsidies. No exit from the constraint: refusal to pay taxes incurs severe punishment.
constraint_indexing:constraint_classification(roman_bath_system, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: URBAN BATH USER (ROPE) — Benefits from public bathing infrastructure at minimal cost. Coordination function: baths solve collective hygiene and social gathering problems. Mobile exit option: can migrate to other cities with baths or use private arrangements. Moderate suppression of alternatives (private baths are expensive, public options limited), but genuine welfare benefit from coordination.
constraint_indexing:constraint_classification(roman_bath_system, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: IMPERIAL ADMINISTRATION (TANGLED ROPE) — Coordinates urban infrastructure (genuine coordination benefit) while extracting tax revenue and maintaining political loyalty. Constrained by need to maintain the bath system's legitimacy and functionality. Derives both coordination function (public goods provision) and asymmetric extraction (taxation, prestige, political control). Active enforcement required: baths must be maintained and operated, tax collection enforced.
constraint_indexing:constraint_classification(roman_bath_system, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: BATH CONTRACTING ELITE (PITON) — Originally performed genuine technical coordination (engineering, maintenance, operations). Over time, bath contracts became sinecures and patronage positions. Theater ratio high (0.55): much spending goes to prestige architecture and administrative overhead rather than functional bathing infrastructure. The functional role (ensuring water, heating, cleanliness) has degraded into theatrical display and political favor distribution. Institutional actors with arbitrage options — can redirect into other public works or private ventures.
constraint_indexing:constraint_classification(roman_bath_system, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: WATER INFRASTRUCTURE COALITION (SCAFFOLD) — Organized actors (engineers, municipal magistrates, water board officials) see the bath system as temporary coordination mechanism dependent on sustained aqueduct investment and maintenance. Suppression is moderate but declining: as water scarcity increases and aqueduct maintenance costs rise, the sustainability of the subsidy model decreases. Sunset logic emerges: the constraint is expected to degrade as resource pressures mount. Theater ratio moderate (0.40) — the infrastructure has genuine functional requirements.
constraint_indexing:constraint_classification(roman_bath_system, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: PROVINCIAL MAGISTRATE (TANGLED ROPE) — Must allocate local tax revenue to imperial bath mandates while also maintaining roads, defenses, and grain supply. Constrained by imperial directives but also benefits from reputation as provider of civic amenities. Experiences both coordination (solving public hygiene, social integration) and extraction (mandatory spending diverts from other infrastructure). Active enforcement of the constraint: refusal to fund baths incurs imperial displeasure.
constraint_indexing:constraint_classification(roman_bath_system, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, some form of public hygiene infrastructure is a natural requirement of large urban settlements. The constraint appears as an immutable property of urban organization: dense populations require coordinated bathing/hygiene systems or disease becomes unmanageable. However, the structural data contradicts true mountain status — the extractiveness value (0.35) and the organizational specificity reveal this as a false summit: the constraint is a contingent institutional choice (state subsidy, specific bath architecture, centralized control), not a law of nature. Alternative hygiene systems exist (private baths, river bathing, communal wells).
constraint_indexing:constraint_classification(roman_bath_system, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(roman_bath_system_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(roman_bath_system, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(roman_bath_system, TypeOther, context(agent_power(powerless), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(roman_bath_system, TR),
    TR >= 0.70.

:- end_tests(roman_bath_system_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The system functions as both coordination (genuine public hygiene) and extraction (concentrated taxation, prestige capture). The extractiveness value reflects that the coordination benefits are substantial — cities with baths genuinely improved public health — but they are distributed unevenly (urban populations benefit more than provincial taxpayers; elites access better facilities). Suppression (0.25): Low to moderate. Alternatives exist (private baths for wealthy, communal wells, river bathing) but are suppressed through sumptuary norms, legal restrictions, and infrastructure concentration. Theater ratio (0.40): Moderate, increasing. Early baths (1st-2nd century) emphasized functional efficiency; by the 3rd century, architectural prestige and administrative overhead dominate spending, approaching piton-level theater (0.55) for the contracting elite. The measured trajectory shows extractiveness rising from 0.18 to 0.35 and theater rising from 0.25 to 0.40 as the system matured, indicating Goodhart drift: the primary function (hygiene coordination) was increasingly supplanted by secondary functions (political legitimacy, patronage distribution).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how identical infrastructure can be experienced as coordination or extraction depending on structural position. The imperial administration's Rope (coordination benefit from loyal, healthy population) is the provincial taxpayer's Snare (forced subsidy with no exit). The urban beneficiary's mobile Rope (can relocate to other bath-cities, benefits from coordination) is unavailable to the trapped taxpayer (cannot refuse tax). The piton observation (degraded ritual replacing function) coexists with genuine coordination (hygiene infrastructure that improved public health). The scaffold view (sunset approaching as aqueduct scarcity increases) is invisible to early-period beneficiaries (1st-2nd century baths) who see only stable coordination. The false mountain view (baths as inherent urban necessity) collapses when historical and archaeological evidence reveals that Roman cities of the republican period had no public bath system and functioned adequately without it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies dramatically across agent types. Imperial administration: beneficiary with arbitrage options → low d → low experienced extraction → Rope classification. Provincial taxpayers: victims with trapped exit → high d → high experienced extraction → Snare classification. Urban users: moderate beneficiaries with mobile exit → moderate d → moderate extraction → Rope with coordination recognition. Provincial magistrates: both beneficiaries (prestige) and constrained victims (budgetary pressure) → moderate d → mixed classification (Tangled Rope). Bath contractors: nominal institutional status but increasingly dependent on patronage → low baseline d overridden by structural degradation → Piton classification. The engine derives d from beneficiary/victim declarations and exit options; for most agents, the derived values produce appropriate classifications. No overrides required — the structural relationships cleanly map to directionality outcomes.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by distinguishing the genuine coordination function (hygiene provision, social integration) from the extraction mechanism (taxation, prestige concentration). The classification Tangled Rope correctly captures that the system is neither pure coordination nor pure extraction. The measured theater_ratio increase (0.25 → 0.40) and extractiveness increase (0.18 → 0.35) demonstrate Goodhart drift: as the system matured, administrative overhead grew and patronage sinecures multiplied, reducing the functional efficiency of coordination. The piton perspective (contractor elite) shows degraded functionality at high theater. The scaffold perspective reveals that the extraction mechanism is only sustainable while resource constraints remain manageable — once aqueduct scarcity bites, the rent-seeking overlay cannot be maintained. The snare perspective (taxpayers) is structural: absent from the narrative and trapped without exit, they bear maximum extraction. The analytical mountain view is a false summit: public bathing infrastructure is a contingent institutional choice, not a law of nature, as evidenced by republican Rome's absence of public baths and the eventual abandonment of the system as aqueduct scarcity forced reallocation of resources. The mandatrophy is fully resolved by showing that all six types are legitimate readings of different structural relationships to the same constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fiscal_sustainability_threshold,
    'At what level of aqueduct maintenance costs does the bath subsidy model become fiscally unsustainable?',
    'Historical analysis of provincial tax records, aqueduct maintenance budgets, and bath operating costs across the 3rd-4th centuries CE',
    'If threshold low (< 15% of provincial revenue): constraint degrades rapidly, becoming snare for taxpayers. If threshold high (> 30%): system persists longer, maintaining mixed extraction-coordination character.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_sustainability_threshold, empirical, 'Fiscal sustainability threshold for the bath subsidy model').

omega_variable(
    alternative_hygiene_availability,
    'To what degree did private baths, communal wells, or river bathing provide functional alternatives that suppressed the bath system''s monopoly on public hygiene?',
    'Archaeological evidence of private bath distribution; literary sources on water access; analysis of suppression mechanisms (legal restrictions on private wells, sumptuary laws on bathing)',
    'If alternatives readily available: suppression is lower, classification shifts toward Rope. If alternatives suppressed: high suppression supports Snare classification for taxpayers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_hygiene_availability, empirical, 'Availability of alternative hygiene systems').

omega_variable(
    extraction_vs_loyalty_mechanism,
    'Was the bath subsidy primarily a tax extraction mechanism or a legitimacy/loyalty mechanism for imperial authority?',
    'Political analysis: correlation between bath investment and provincial stability, rebellion rates, and imperial military expenditure. Comparison of bath subsidies to other legitimacy mechanisms (grain doles, circus games).',
    'If extraction-dominant: snare classification strengthens. If loyalty-dominant: rope classification dominates (genuine coordination for social stability).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_vs_loyalty_mechanism, conceptual, 'Whether baths function primarily as extraction or legitimacy mechanism').

omega_variable(
    water_scarcity_timeline,
    'When did provincial aqueducts first face scarcity pressures severe enough to threaten bath system sustainability?',
    'Historical chronology of aqueduct failures, drought records, and bath closure dates across provinces. Construction vs abandonment timeline.',
    'If early (2nd century): scaffold sunset is real structural feature. If late (4th-5th century): sunset logic is aspirational, not structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(water_scarcity_timeline, empirical, 'Timeline of water scarcity pressures on the bath system').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(roman_bath_system, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bath_tr_t0, roman_bath_system, theater_ratio, 0, 0.25).
narrative_ontology:measurement(bath_tr_t100, roman_bath_system, theater_ratio, 100, 0.35).
narrative_ontology:measurement(bath_tr_t200, roman_bath_system, theater_ratio, 200, 0.4).

% Extraction over time
narrative_ontology:measurement(bath_be_t0, roman_bath_system, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(bath_be_t100, roman_bath_system, base_extractiveness, 100, 0.28).
narrative_ontology:measurement(bath_be_t200, roman_bath_system, base_extractiveness, 200, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(roman_bath_system, resource_allocation).
narrative_ontology:affects_constraint(roman_bath_system, roman_aqueduct_infrastructure).
narrative_ontology:affects_constraint(roman_bath_system, roman_tax_collection_system).
narrative_ontology:affects_constraint(roman_bath_system, roman_imperial_legitimacy).

% DUAL FORMULATION NOTE:
% The bath system is downstream of aqueduct infrastructure (which has its own ε and constraint properties) and feeds into the imperial legitimacy mechanism. Decomposition justified because aqueduct engineering constraints have different ε (~0.10, Mountain) than the bath system's institutional/fiscal arrangement (ε ~0.35, Tangled Rope).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
