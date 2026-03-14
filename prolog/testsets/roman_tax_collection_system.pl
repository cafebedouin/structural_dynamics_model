% ============================================================================
% CONSTRAINT STORY: roman_tax_collection_system
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_roman_tax_collection_system, []).

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
 *   constraint_id: roman_tax_collection_system
 *   human_readable: Roman Tax Collection System (Publicani Extraction and Coordination)
 *   domain: economic_political/ancient_governance
 *
 * SUMMARY:
 *   The Roman tax collection system represents a hybrid
 *   coordination-extraction constraint spanning the Republican and Imperial
 *   periods. Rome lacked the administrative infrastructure for direct state
 *   tax collection, so it contracted with private tax farmers (publicani) who
 *   bid for collection rights in specific provinces. This solved a
 *   coordination problem: how to fund the military and state apparatus
 *   without building a permanent bureaucracy. However, the system embedded
 *   asymmetric extraction: publicani could collect above the contracted
 *   amount and retain the difference; provincial populations bore suppression
 *   (legal disabilities, military coercion, threat of collective punishment)
 *   that prevented organized resistance; and the imperial administration
 *   faced moral hazard (no incentive to prevent excessive extraction if the
 *   revenue flowed to Rome). The constraint exhibits classic tangled-rope
 *   dynamics: genuine coordination function (efficient resource mobilization
 *   for empire-scale governance) coupled with systematic extraction toward
 *   beneficiaries (publicani and imperial elites). The theater_ratio
 *   increased over time (from 0.35 to 0.68 across 200 years) as the system
 *   became increasingly ritualized: publicani contracts became hereditary
 *   rather than competitive, the pretense of 'service provision' wore thin,
 *   and the mechanism's legitimacy rested primarily on Roman military
 *   dominance rather than perceived fairness. This degradation signature
 *   suggests piton dynamics at the institutional level — the system persists
 *   through inertia and coercion, not because it functions efficiently.
 *
 * KEY AGENTS:
 *   - Provincial Peasant: Primary victim (powerless/trapped) — bears extraction with no exit or coordination benefit. No legal recourse against arbitrary assessment; subject to military enforcement of collection.
 *   - Local Magistrate: Secondary institutional actor (moderate/constrained) — constrained by Rome's ultimate authority; benefits from infrastructure funded by taxes but also bears pressure from publicani and quota demands from Rome.
 *   - Publicani Tax Farmer: Primary beneficiary (institutional/arbitrage) — captures profit margin between contracted amount and actual collection; operates through established networks; can arbitrage between provinces.
 *   - Imperial Administration: Secondary beneficiary (institutional/arbitrage) — receives tax revenue to finance military and state apparatus; experiences coordination benefit but also moral hazard (incentive to ignore excessive extraction).
 *   - Provincial Resistance Coalition: Organized actors (organized/mobile) — city councils, merchant guilds, landowner associations that can coordinate collective action or tax resistance; face organized suppression but retain some mobility.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing tax extraction as inherent to empire, masking contingent institutional choices.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(roman_tax_collection_system, 0.58).
domain_priors:suppression_score(roman_tax_collection_system, 0.72).
domain_priors:theater_ratio(roman_tax_collection_system, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(roman_tax_collection_system, extractiveness, 0.58).
narrative_ontology:constraint_metric(roman_tax_collection_system, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(roman_tax_collection_system, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(roman_tax_collection_system, tangled_rope).
narrative_ontology:human_readable(roman_tax_collection_system, "Roman Tax Collection System (Publicani Extraction and Coordination)").
narrative_ontology:topic_domain(roman_tax_collection_system, "economic_political/ancient_governance").

domain_priors:requires_active_enforcement(roman_tax_collection_system).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(roman_tax_collection_system, publicani_tax_farmers).
narrative_ontology:constraint_beneficiary(roman_tax_collection_system, roman_military).
narrative_ontology:constraint_beneficiary(roman_tax_collection_system, imperial_administration).
narrative_ontology:constraint_victim(roman_tax_collection_system, provincial_populations).
narrative_ontology:constraint_victim(roman_tax_collection_system, small_local_economies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROVINCIAL PEASANT (SNARE) — Trapped within territorial jurisdiction; subject to arbitrary tax assessment by publicani with no legal recourse or exit. Bears extraction with no coordination benefit. Maximum experienced extractiveness due to trapped status + high suppression of alternatives.
constraint_indexing:constraint_classification(roman_tax_collection_system, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: LOCAL MAGISTRATE (TANGLED ROPE) — Constrained by Rome's authority but also benefits from the infrastructure that tax revenue provides (roads, military protection, legal framework). Experiences both extraction (quota pressure from publicani) and coordination (integration into imperial administrative system). Can theoretically resist but faces career termination and military coercion.
constraint_indexing:constraint_classification(roman_tax_collection_system, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PUBLICANI TAX FARMER (ROPE) — Experiences the constraint as pure coordination with high benefit. Contract specifies tax base; excess collection is arbitrage profit. Institutional actors with established networks; can exit to other provinces or adjust collection methods. Net beneficiary from the system — extraction flows toward this agent.
constraint_indexing:constraint_classification(roman_tax_collection_system, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: IMPERIAL ADMINISTRATION (ROPE) — Views tax collection as essential coordination mechanism: gathering resources to pay legions, fund public works, and maintain territorial control. The system solves a collective action problem (how to finance empire without direct state apparatus). Extraction flows toward this actor but is experienced as necessary coordination cost. Low chi for institutional beneficiary with arbitrage options.
constraint_indexing:constraint_classification(roman_tax_collection_system, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: TAX FARMING CONTRACT SYSTEM (PITON) — The institutional arrangement itself shows degradation over time. Initially an efficient private collection mechanism (theater_ratio ~0.45 in early Imperial period), it evolved into a theatrical performance of legitimacy (theater_ratio ~0.68 in later period) as publicani became entrenched and contracts became hereditary rather than competitive. The system persists through institutional inertia — Rome cannot easily replace it without building state apparatus, but the mechanism's original function (efficient collection) has atrophied.
constraint_indexing:constraint_classification(roman_tax_collection_system, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: PROVINCIAL RESISTANCE COALITION (TANGLED ROPE) — Organized groups (city councils, merchant guilds, landowner associations) can coordinate collective action or tax strikes, creating genuine coordination benefit (mutual insurance, collective negotiation) while also experiencing extraction through organized suppression (military response, collective punishment). Chi moderate due to organized power and mobile exit options (some provinces can reduce cooperation or redirect trade).
constraint_indexing:constraint_classification(roman_tax_collection_system, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some form of resource extraction to maintain state apparatus is inherent to empire: any centralized power structure requires funding mechanisms, and the gap between local autonomy and centralized authority is a structural necessity. This perspective risks naturalizing the tax farming system as immutable to political organization itself. However, the structural data contradicts pure mountain classification — alternatives (direct state collection, fixed tribute) existed and were used elsewhere.
constraint_indexing:constraint_classification(roman_tax_collection_system, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(roman_tax_collection_system_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(roman_tax_collection_system, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(roman_tax_collection_system, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(roman_tax_collection_system, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(roman_tax_collection_system, TR),
    TR >= 0.70.

:- end_tests(roman_tax_collection_system_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The publicani system extracts value significantly above direct collection costs, with evidence suggesting 40-60% profit margins in many provinces. However, extraction is not maximal (snare level 0.66+) because the system does provide genuine service (collection, transport, security) and because some provinces can reduce cooperation or manage collections internally, reducing effective extraction. The measurement shows extractiveness increasing over time (0.42 → 0.58) as the system becomes more entrenched and competition for contracts declines. Suppression (0.72): High. Provincial populations face multiple coercive mechanisms: they cannot legally refuse payment, they have no appeal mechanism for arbitrary assessments, they face military enforcement of collection, they experience collective punishment for resistance, and they have limited information about tax rates and assessment methodologies. However, suppression is not absolute (would require 0.85+) because some local magistrates can negotiate, some provinces have traditional exemptions, and organized groups can sometimes coordinate collective resistance. Theater ratio (0.68): Moderate-high. The system justified itself through claims of efficient service delivery and legitimate profit-taking. Over time, as contracts became hereditary and publicani consolidation increased, the performative element grew — the theatrical legitimacy became increasingly thin as the extraction mechanism became visible. The theater_ratio increase over the interval (0.35 → 0.68) reflects this degradation: the mechanism persists through habit and military backing rather than perceived legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. Publicani see a rope — they solve a real coordination problem (funding empire without state bureaucracy) and are well-compensated for the service. Roman administration sees a rope — they acquire the resources necessary to maintain territorial control and military capability. Local magistrates see tangled rope — they coordinate local administration but experience pressure from above and below. Provincial resistance coalitions see tangled rope with organizing potential — they experience extraction but can mobilize collective defense. Provincial peasants see a snare — they bear costs with no exit and no benefit. The analytical observer risks seeing a mountain (tax extraction is inherent to empire) but the structural data shows this is naturalization: alternatives existed (direct collection, fixed tribute, in-kind requisition) and were used in comparable empires and in some Roman contexts. The false summit here is claiming immutability for what is contingent institutional design.
 *
 * DIRECTIONALITY LOGIC:
 *   Publicani (institutional/arbitrage): Derive d ≈ 0.10 from beneficiary status + arbitrage exit options. They experience minimal effective extraction (negative chi) because they are the primary extractors. They can move between provinces, negotiate contract terms, and shift collection methods. Roman administration (institutional/arbitrage): Derives d ≈ 0.15 from beneficiary status + arbitrage options. They receive the revenues but face moral hazard: they could modify the system but choose not to because extraction flows to them. Local magistrates (moderate/constrained): Derive d ≈ 0.50 from mixed position (both coordinate local administration and enforce collection) + constrained exit. They experience moderate extraction because they can sometimes negotiate with publicani but ultimately cannot refuse Roman authority. Provincial peasant (powerless/trapped): Derives d ≈ 0.95 from victim status + trapped exit. They experience maximum extraction because they cannot negotiate, cannot refuse, cannot appeal, and cannot exit the territory. The directionality spread across these actors (0.10 to 0.95) explains the perspectival gap: the same constraint is rope for beneficiaries, tangled-rope for moderates, and snare for victims.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The tax farming system is a genuine tangled rope, not a false dichotomy. It solves a real coordination problem (empire-scale resource mobilization) while embedding systematic asymmetric extraction. The mandatrophy is resolved by recognizing that both functions are real: the system cannot be dismissed as pure extraction (snare) because it genuinely coordinates the financial infrastructure of empire, nor can it be whitewashed as pure coordination (rope) because the extraction is systematic and asymmetric. The key insight is that coordination and extraction are not mutually exclusive — the system was designed precisely to achieve both: mobilize resources (coordination) while allowing private profit (extraction). The theater_ratio trajectory (increasing over time) reveals how mandatrophy can degrade: as the system persists through inertia rather than efficiency, the coordination component atrophies and the extraction component becomes more visible and controversial. By the late Imperial period, the system approaches piton (performative persistence) because the coordination benefit has been eclipsed by the extraction visibility, yet the system persists because Rome cannot easily replace it without building state apparatus. This is a case study in how tangled ropes degrade into pitons: coordination atrophies, theater increases, structural binding weakens but institutional inertia persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_collection_mechanisms,
    'Did alternative tax collection mechanisms (direct state collection, fixed tribute, in-kind requisition) constitute genuine structural alternatives or merely impractical fantasies given Roman technological capacity?',
    'Comparative historical analysis of tax systems in contemporary empires (Ptolemaic Egypt, Parthian Persia, Mauryan India); assessment of collection costs and efficiency under different mechanisms in Roman territories that experimented with alternatives',
    'If alternatives were genuinely viable: the mountain classification is false naturalization; extractiveness reflects contingent institutional choice. If alternatives were structurally impossible: some mountain component is legitimate. Central to mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_collection_mechanisms, empirical, 'Whether alternative tax mechanisms were structurally viable').

omega_variable(
    publicani_profit_rates_and_justification,
    'What percentage of publicani profits represent legitimate service delivery (collection, transport, security) versus extractive margin over necessary costs?',
    'Historical accounting of publicani operations (rare surviving records); comparison of profit rates against documented collection costs and security expenses; analysis of price changes for publicani contracts over time (rising prices indicate competition for rents, not efficiency gains)',
    'If profits ≥ 60% above documented costs: extractiveness increases toward snare territory. If profits ≤ 20% above costs: more rope-like (payment for genuine service). Central to distinguishing coordination from extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(publicani_profit_rates_and_justification, empirical, 'Publicani profit margins vs. documented service costs').

omega_variable(
    identity_lock_in_imperial_administration,
    'Did the imperial administration''s commitment to tax farming represent genuine belief that it was the only viable mechanism (identity lock) or pragmatic acceptance of constraints they recognized as contingent?',
    'Textual analysis of administrative records, imperial correspondence, and technical writings; examination of reform proposals and debates about alternative systems; analysis of whether administrators experimented with alternatives during crises',
    'If identity-locked: the institutional perspective becomes identity_locked (analytical/cognitive capture), revealing that the rope classification masks trapped structural dependence. If pragmatic: the rope classification holds; extraction is experienced as necessary coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_imperial_administration, conceptual, 'Whether imperial commitment to tax farming was identity-locked or pragmatic').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Was provincial suppression primarily structural (military coercion, legal disability, lack of information) or internalized (belief in Roman superiority, identification with imperial project, resignation as natural order)?',
    'Analysis of rebellion frequency and triggers; examination of provincial literary records and inscriptions for expressions of legitimacy vs. resistance; tracking of suppression costs over time (increasing costs suggest internalization is failing; stable/declining costs suggest structural suppression holds)',
    'If structural: suppression depends on continuous coercion; alternative military configurations could reduce binding. If internalized: suppression persists after structural constraints removed; requires cognitive reframing to break. Critical for understanding constraint persistence across regime changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is structural or internalized in provincial populations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(roman_tax_collection_system, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rtcs_tr_t0, roman_tax_collection_system, theater_ratio, 0, 0.35).
narrative_ontology:measurement(rtcs_tr_t100, roman_tax_collection_system, theater_ratio, 100, 0.55).
narrative_ontology:measurement(rtcs_tr_t200, roman_tax_collection_system, theater_ratio, 200, 0.68).

% Extraction over time
narrative_ontology:measurement(rtcs_be_t0, roman_tax_collection_system, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(rtcs_be_t100, roman_tax_collection_system, base_extractiveness, 100, 0.52).
narrative_ontology:measurement(rtcs_be_t200, roman_tax_collection_system, base_extractiveness, 200, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(roman_tax_collection_system, resource_allocation).
narrative_ontology:boltzmann_floor_override(roman_tax_collection_system, 0.18).
narrative_ontology:affects_constraint(roman_tax_collection_system, roman_provincial_military_garrisoning).
narrative_ontology:affects_constraint(roman_tax_collection_system, roman_slavery_and_debt_bondage).
narrative_ontology:affects_constraint(roman_tax_collection_system, roman_infrastructure_investment).

% DUAL FORMULATION NOTE:
% The tax collection system decomposes into distinct constraints: (1) the publicani contract mechanism itself (extractiveness ≈ 0.58, tangled rope), (2) the provincial obligation system that enforces collection (extractiveness ≈ 0.72, snare for peasants), and (3) the imperial accounting and distribution of revenue (extractiveness ≈ 0.42, rope). This story focuses on the publicani mechanism as primary. The suppression mechanisms and provincial obligation stories should be documented separately with different ε values and different beneficiary/victim declarations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(roman_tax_collection_system, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
