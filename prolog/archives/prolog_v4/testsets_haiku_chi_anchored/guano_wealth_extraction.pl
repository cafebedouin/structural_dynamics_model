% ============================================================================
% CONSTRAINT STORY: guano_wealth_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_guano_wealth_extraction, []).

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
 *   constraint_id: guano_wealth_extraction
 *   human_readable: Guano-based Wealth Extraction in the Chincha Islands
 *   domain: economic/colonial_resource_extraction
 *
 * SUMMARY:
 *   The Chincha Islands guano deposits represent a rare ecological abundance
 *   that enabled a non-imperial culture to accumulate significant regional
 *   wealth and power without agricultural expansion or conquest-based
 *   resource extraction. From approximately 800 AD to 1400 AD, the Chincha
 *   people leveraged massive bird populations (cormorants, boobies, pelicans)
 *   that produce nutrient-rich guano suitable for trade and prestige-good
 *   manufacture. This constraint story measures how institutional extraction
 *   overlaid onto an ecological abundance creates a snare: the guano itself
 *   is a coordination resource, but the mechanisms for wealth distribution
 *   and labor organization progressively concentrates benefits while
 *   expanding coercive labor demands. The transition from early Chincha
 *   (perhaps rope-like coordination) to late Chincha (snare-like extraction
 *   with piton characteristics) represents institutional entrenchment and
 *   ecological degradation. Spanish conquest in 1532 intensified the
 *   extraction mechanism, but the underlying structural dynamics — finite
 *   ecological productivity, institutional wealth concentration, labor
 *   suppression — were already operative. This constraint is a diagnostic
 *   case for understanding how ecological abundance can be weaponized into
 *   institutional extraction.
 *
 * KEY AGENTS:
 *   - Guano Harvesting Laborers: Primary victims (powerless/trapped) — bear hazardous labor, coerced extraction, no alternative livelihood. d≈0.95.
 *   - Chincha Commoners: Secondary victims/beneficiaries (moderate/constrained) — benefit from guano-driven prosperity but bear tribute and labor obligations. d≈0.50.
 *   - Chincha Nobility: Primary beneficiary (institutional/arbitrage) — control guano wealth, organize trade networks, extract tribute. d≈0.05.
 *   - Spanish Colonial Administration: Secondary beneficiary/extractor (organized/constrained) — monopolize guano trade after 1532, intensify labor systems. d≈0.40.
 *   - Ecological System: Passive victim (piton perspective) — bird populations provide productivity, but institutional extraction approaches carrying capacity limits. No exit option.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional extraction as inherent ecological scarcity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(guano_wealth_extraction, 0.68).
domain_priors:suppression_score(guano_wealth_extraction, 0.72).
domain_priors:theater_ratio(guano_wealth_extraction, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(guano_wealth_extraction, extractiveness, 0.68).
narrative_ontology:constraint_metric(guano_wealth_extraction, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(guano_wealth_extraction, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(guano_wealth_extraction, snare).
narrative_ontology:human_readable(guano_wealth_extraction, "Guano-based Wealth Extraction in the Chincha Islands").
narrative_ontology:topic_domain(guano_wealth_extraction, "economic/colonial_resource_extraction").

domain_priors:requires_active_enforcement(guano_wealth_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(guano_wealth_extraction, chincha_nobility).
narrative_ontology:constraint_beneficiary(guano_wealth_extraction, spanish_colonial_administration).
narrative_ontology:constraint_victim(guano_wealth_extraction, guano_harvesting_laborers).
narrative_ontology:constraint_victim(guano_wealth_extraction, chincha_commoners).
narrative_ontology:constraint_victim(guano_wealth_extraction, ecological_sustainability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GUANO HARVESTER (SNARE) — Trapped in labor-intensive, hazardous guano extraction with no exit option. Experiences intense extraction: 0.68 base × 1.42 f(d) × 0.8 σ(local) ≈ 0.77 effective χ. Suppression through coercion, scarcity of alternatives, and institutional mechanisms (Chincha nobility or colonial administration control all trade routes). d≈0.95, powerless + trapped.
constraint_indexing:constraint_classification(guano_wealth_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: CHINCHA COMMONER (TANGLED ROPE) — Benefits from ecological abundance and regional trade networks enabled by guano wealth, but constrained by labor obligations and wealth concentration. Experiences mixed extraction and coordination: 0.68 × 1.00 f(d) × 0.9 σ(regional) ≈ 0.61 χ. Trapped between benefiting from guano-driven prosperity and bearing extraction costs through tribute and labor.
constraint_indexing:constraint_classification(guano_wealth_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CHINCHA NOBILITY (ROPE) — Primary beneficiary of guano wealth extraction. Experiences constraint as coordination mechanism: organizing trade networks, setting labor tribute, establishing regional hegemony. d≈0.05, institutional + arbitrage → f(d)≈-0.12 → χ≈-0.06. Net beneficiary. Sees guano as a coordination resource enabling social hierarchy and regional power.
constraint_indexing:constraint_classification(guano_wealth_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: SPANISH COLONIAL ADMINISTRATION (SNARE) — After conquest (1532 onward), colonial powers intensify extraction mechanisms: forced labor systems (encomienda, later guano extraction under contract), monopoly control of guano trade, and external commodity markets. d≈0.40, organized + constrained → f(d)≈0.40 → χ≈0.19 (constrained by colonial logistics, disease, resistance). But effective extraction remains high due to global scope: 0.68 × 0.40 × 1.2 σ(global) ≈ 0.33. Colonial snare operates at different scale than pre-conquest extraction.
constraint_indexing:constraint_classification(guano_wealth_extraction, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ECOLOGICAL SYSTEM (PITON) — Guano deposits represent extraordinary bird population productivity (cormorants, boobies, pelicans). The 'constraint' from ecology's perspective is that guano abundance is performatively maintained through institutional harvesting practices, but the underlying ecological function (nutrient recycling, bird habitat) degrades. theater_ratio=0.35 suggests functional extraction rather than pure performance, but piton classification emerges from the long-term view: by 1400 AD, intensive harvesting has maintained institutional wealth while ecological carrying capacity silently declines. The system appears to coordinate abundance but is actually exploiting accumulated capital.
constraint_indexing:constraint_classification(guano_wealth_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / ECOLOGICAL LIMITS (MOUNTAIN) — From a civilizational/universal perspective, guano extraction embodies an immutable constraint: finite nutrient recycling capacity. The birds can only produce so much guano per year; the limit is a natural law. ε=0.05 (measuring the constraint as 'bird productivity'), emerges_naturally=true, accessibility_collapse=0.92 (effort to harvest guano scales nonlinearly with depletion), resistance=0.08 (little human agency over bird population dynamics). However, the base_properties (ε=0.68, suppression=0.72) measure a different constraint: the institutional extraction system, not the ecological limit. This reveals constraint decomposition: the ecological limit is a mountain; the institutional rent-extraction system overlaid on the ecology is a snare.
constraint_indexing:constraint_classification(guano_wealth_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(guano_wealth_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(guano_wealth_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(guano_wealth_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(guano_wealth_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(guano_wealth_extraction, TR),
    TR >= 0.70.

:- end_tests(guano_wealth_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The Chincha nobility and later colonial powers extract significant wealth from guano labor relative to the material inputs (labor, transportation). Measurement shows progression from 0.35 (early Chincha, mixed coordination-extraction) to 0.68 (late Chincha and colonial, pure extraction). The trajectory reflects institutional entrenchment: early access to guano wealth enables luxuries and prestige goods, incentivizing specialization; specialization concentrates knowledge and control; concentration enables monopoly rents. By 1400 AD, the constraint operates as a pure snare: labor-intensive harvest, wealth concentration in nobility, no exit for commoners. Suppression (0.72): High. Multiple suppression mechanisms: (1) Geographic isolation of Chincha Islands limits alternative income sources. (2) Ecological specialization in guano harvesting makes alternative livelihoods costly. (3) Institutional monopoly on guano trade prevents independent merchants. (4) Labor obligations (tributary or coercive) prevent exit. (5) Knowledge barriers — guano harvesting requires specialized techniques. Theater ratio (0.35): Moderate-low. The constraint is functionally extractive, not primarily performative. Guano is genuinely valuable; labor is genuinely necessary; wealth concentration is a genuine institutional outcome. The relatively low theater suggests a snare with material extraction, not a piton with theatrical maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The Chincha nobility see rope or arbitrage-based rope: organizing guano trade solves a coordination problem (getting guano to markets, establishing prestige hierarchies). The commoners see tangled rope: they benefit from guano-driven abundance but pay labor tributes. The harvesters see snare: dangerous work with no exit. The ecological system (piton perspective) sees degradation masquerading as function — the system appears productive but carrying capacity silently declines. The analytical observer (mountain perspective) risks naturalizing the institutional snare as an immutable ecological limit — 'guano is scarce, so extraction is necessary' — when actually the scarcity is constructed by institutional monopoly, not ecology. Decomposition: The ecological limit (mountain: bird productivity is finite) is separate from the institutional extraction system (snare: labor is coercively organized). The perspectival gap reveals this decomposition.
 *
 * DIRECTIONALITY LOGIC:
 *   Guano Harvesting Laborers: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction. No alternative income, no mobility, coerced labor. χ = 0.68 × 1.42 × 0.8 (local scope) ≈ 0.77. Snare classification confirmed. Chincha Commoners: Victim + constrained, but also benefit from guano-driven abundance. Beneficiary + constrained is ambiguous → d≈0.50, f(d)≈0.65. χ = 0.68 × 0.65 × 0.9 (regional scope) ≈ 0.40. Mixed extraction justifies tangled_rope. Chincha Nobility: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.12. χ = 0.68 × (-0.12) × 0.9 ≈ -0.07. Net beneficiary, rope classification. Spanish Colonial Administration: Organized + constrained (by logistics, disease, indigenous resistance). d≈0.40, f(d)≈0.40. χ = 0.68 × 0.40 × 1.2 (global scope) ≈ 0.33. Moderate extraction; maintains snare classification but with organized resistance.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY NOT RESOLVED (ε=0.68 < 0.70, no mandate for resolution). However, the constraint exhibits potential mandatrophy: early Chincha (800-1000 AD) may have experienced rope-like coordination with guano wealth enabling broad prosperity. Late Chincha (1200-1400 AD) exhibits snare-like extraction with wealth concentration and labor coercion. The question is whether institutional extraction emerged gradually (degradation from rope to snare) or was always present (snare misidentified as rope early on). The measurement trajectory (theater_ratio=0.28→0.35, extractiveness=0.35→0.68) suggests degradation: as guano wealth concentrates, institutional mechanisms shift from coordination to extraction. This is not a false identification (snare wrongly labeled rope) but a genuine institutional shift. The theater_ratio remains low (functional extraction) throughout, suggesting the snare is not performative — the extracted wealth is real. Mandatrophy would arise if the analytical observer tried to naturalize this institutional extraction as a mountain (ecological necessity), claiming finite guano supply forces coercive labor organization. This is false: the Chincha could have organized guano harvesting through cooperative mechanisms or more distributed wealth. The institutional snare is contingent, not inherent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ecological_depletion_timing,
    'At what guano extraction rate do ecological carrying capacities collapse, and did the Chincha reach that threshold before Spanish conquest?',
    'Stratigraphic analysis of guano deposits; paleornithological evidence of bird population density 800-1400 AD; nutrient flux reconstruction; comparison with known collapse events in other island guano systems (Chincha, Humboldt Current).',
    'If collapse imminent before 1532: institutional snare is operating near ecological ceiling; institutional extraction accelerates pre-existing degradation. If collapse would persist beyond 1532: Spanish intensification causes the collapse. Alters mandatrophy classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecological_depletion_timing, empirical, 'Whether Chincha extraction approached ecological collapse before conquest').

omega_variable(
    labor_coercion_mechanism,
    'Was guano harvesting labor extracted through military coercion (snare), voluntary specialization enabled by guano wealth (rope), or mixed tributary obligation (tangled_rope)?',
    'Archaeological evidence of labor organization, settlement patterns, skeletal stress markers; ethnohistoric accounts of pre-conquest vs colonial labor systems; comparative analysis with other Andean tribute systems.',
    'If military coercion: snare classification from pre-conquest perspective. If voluntary: rope classification shifts toward beneficiary view. If tributary obligation: tangled_rope is primary type. Determines whether suppression (0.72) represents coercive constraint or institutional coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_coercion_mechanism, empirical, 'Mechanism of labor obligation in guano harvesting').

omega_variable(
    wealth_distribution_span,
    'What fraction of Chincha population directly benefited from guano wealth, and did that fraction narrow or widen over the 800-1400 AD interval?',
    'Archaeological distribution of prestige goods, housing types, burial assemblages; settlement hierarchy; faunal assemblage indicators of dietary access to marine resources.',
    'If narrow beneficiary class + widening extraction: snare deepens over time, theater_ratio declines as performance necessity increases. If broad-based benefit: tangled_rope classification strengthens; constraint operates more as coordination mechanism than extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(wealth_distribution_span, empirical, 'Distribution of guano wealth benefits across Chincha population').

omega_variable(
    alternative_wealth_pathways,
    'Were there viable alternative pathways to regional power and prosperity that did not depend on guano extraction, and were these pathways suppressed or abandoned?',
    'Comparative regional development analysis; archaeological evidence of other subsistence/trade specializations; ethnographic parallels from other Andean groups. Did Chincha pursue alternative economic strategies that guano wealth rendered unnecessary?',
    'If alternatives were viable but suppressed: suppression metric (0.72) is accurately measuring institutional coercion. If alternatives were genuinely nonexistent: suppression reflects structural scarcity (higher natural law component). Determines whether constraint is contingent or inherent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_wealth_pathways, empirical, 'Availability of alternative wealth pathways to guano extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(guano_wealth_extraction, 0, 600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(guano_tr_t0, guano_wealth_extraction, theater_ratio, 0, 0.28).
narrative_ontology:measurement(guano_tr_t400, guano_wealth_extraction, theater_ratio, 400, 0.31).
narrative_ontology:measurement(guano_tr_t600, guano_wealth_extraction, theater_ratio, 600, 0.35).

% Extraction over time
narrative_ontology:measurement(guano_be_t0, guano_wealth_extraction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(guano_be_t400, guano_wealth_extraction, base_extractiveness, 400, 0.52).
narrative_ontology:measurement(guano_be_t600, guano_wealth_extraction, base_extractiveness, 600, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(guano_wealth_extraction, resource_allocation).
narrative_ontology:affects_constraint(guano_wealth_extraction, andean_tributary_system).
narrative_ontology:affects_constraint(guano_wealth_extraction, colonial_encomienda_labor).
narrative_ontology:affects_constraint(guano_wealth_extraction, nutrient_depletion_humboldt_current).

% DUAL FORMULATION NOTE:
% Guano-based wealth extraction decomposes into two structurally distinct constraints: (1) ECOLOGICAL LIMIT (mountain): finite bird productivity per year sets absolute ceiling on guano availability. ε≈0.05, emerges_naturally=true. (2) INSTITUTIONAL EXTRACTION (snare): labor organization, wealth concentration, and monopoly control over guano trade create coercive rent extraction. ε≈0.68, requires_active_enforcement=true. The present story addresses the institutional constraint. The ecological limit is upstream but distinct — it is a mountain that enabled but did not necessitate the snare.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(guano_wealth_extraction, organized, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
