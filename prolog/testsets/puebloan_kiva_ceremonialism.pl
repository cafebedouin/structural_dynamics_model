% ============================================================================
% CONSTRAINT STORY: puebloan_kiva_ceremonialism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_puebloan_kiva_ceremonialism, []).

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
 *   constraint_id: puebloan_kiva_ceremonialism
 *   human_readable: Puebloan Kiva Ceremonialism and Community Authority
 *   domain: anthropology/religious_practice/indigenous_governance
 *
 * SUMMARY:
 *   Puebloan kiva ceremonialism represents a constraint that has structured
 *   social authority, knowledge transmission, and community identity across
 *   the Indigenous peoples of the Southwest for centuries. The kiva system
 *   coordinates multiple functions: seasonal agricultural calendar, water
 *   management authority, male initiation hierarchy, healing practices, and
 *   community dispute resolution. These genuine coordination functions are
 *   interwoven with asymmetric extraction mechanisms: knowledge gatekeeping
 *   that concentrates authority in specific clans/moieties, gender-based
 *   exclusion from certain ceremonies, suppression of alternative knowledge
 *   sources, and identity-fusion that makes exit from the system equivalent
 *   to abandonment of Pueblo identity itself. The constraint exhibits all six
 *   DR types depending on structural position: the non-initiated experience
 *   snare (extraction without coordination benefit), initiated subordinates
 *   experience tangled rope (mixed coordination and extraction), ceremonial
 *   authorities experience rope (coordination mechanism), inter-pueblo
 *   networks experience tangled rope (coordinating across communities with
 *   embedded prestige asymmetries), and external protections appear as
 *   scaffold (temporary relief from suppression). The analytical observer
 *   risks naturalizing the constraint as an immutable feature of Pueblo
 *   ecology or indigenous worldview, when the structural data reveals
 *   significant identity-based suppression and knowledge gatekeeping that are
 *   institutional rather than ecological necessities.
 *
 * KEY AGENTS:
 *   - Non-initiated community members (including women excluded from male kivas, younger members awaiting initiation, outsiders/non-community members): Primary victims (powerless/identity_locked) — identity fused with community membership yet structurally excluded from ceremonial knowledge and decision-making authority
 *   - Initiated but subordinate members (low-rank kachina society members, younger initiates, members of non-dominant moieties): Secondary victims/mixed beneficiaries (moderate/constrained) — participate in coordination but bear disproportionate labor and deference burdens
 *   - Ceremonial specialists and authority groups (Bear clan leaders, Two Horn society chiefs, war chiefs, primary moiety heads): Primary beneficiaries (institutional/arbitrage) — hold authority, prestige, and knowledge monopolies; minimal burden from coordination mechanisms they control
 *   - Inter-pueblo ceremonial networks (regional exchange networks, traveling specialists, pueblo confederacies): Organized participants (organized/mobile) — coordinate across communities with some pueblos/groups extracting prestige from others
 *   - Federal/American Indian legal protection movements (AIRFM, NAGPRA advocates): External powerful actors (powerful/arbitrage) — provide temporary scaffolding protecting ceremonialism from external suppression
 *   - Spanish colonial religious apparatus (now degraded): Historical institutional actor (institutional/arbitrage) — imposed Catholic framing; now persists through syncretism inertia
 *   - Analytical observer: Universal perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as ecological/spiritual necessities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(puebloan_kiva_ceremonialism, 0.35).
domain_priors:suppression_score(puebloan_kiva_ceremonialism, 0.72).
domain_priors:theater_ratio(puebloan_kiva_ceremonialism, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(puebloan_kiva_ceremonialism, extractiveness, 0.35).
narrative_ontology:constraint_metric(puebloan_kiva_ceremonialism, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(puebloan_kiva_ceremonialism, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(puebloan_kiva_ceremonialism, tangled_rope).
narrative_ontology:human_readable(puebloan_kiva_ceremonialism, "Puebloan Kiva Ceremonialism and Community Authority").
narrative_ontology:topic_domain(puebloan_kiva_ceremonialism, "anthropology/religious_practice/indigenous_governance").

domain_priors:requires_active_enforcement(puebloan_kiva_ceremonialism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(puebloan_kiva_ceremonialism, religious_authority_groups).
narrative_ontology:constraint_beneficiary(puebloan_kiva_ceremonialism, ceremonial_specialists).
narrative_ontology:constraint_victim(puebloan_kiva_ceremonialism, marginalized_community_members).
narrative_ontology:constraint_victim(puebloan_kiva_ceremonialism, non_initiates).
narrative_ontology:constraint_victim(puebloan_kiva_ceremonialism, cultural_transmission_system).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-INITIATED MEMBER (SNARE) — Identity fused with community/clan membership yet structurally excluded from ceremonial knowledge. Exclusion is enforced through family/clan structures and cultural identity framing. The non-initiate cannot exit without abandoning community identity. High suppression from social sanctions and identity cohesion. Snare classification reflects that the constraint extracts deference and labor (food preparation, ceremonial support) without meaningful coordination benefit to the non-initiate.
constraint_indexing:constraint_classification(puebloan_kiva_ceremonialism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: INITIATED BUT SUBORDINATE (TANGLED ROPE) — Initiated into lower ceremonial ranks (moiety, kachina society) but excluded from highest authority positions (Bear clan, Two Horn society, war chief council). Structurally mobile (could leave, migrate, or defect to another pueblo) but exit carries high social cost. Experience both genuine coordination (participation in ceremonial cycle maintains community cohesion) and asymmetric extraction (labor and deference concentrated downward from authority, benefits concentrated upward).
constraint_indexing:constraint_classification(puebloan_kiva_ceremonialism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: CEREMONIAL SPECIALIST/AUTHORITY (ROPE) — High-ranked initiate holding positions in war chief council, Two Horn society, or primary clan authority. Experience the constraint primarily as coordination: maintaining ceremonial calendar, mediating disputes through ceremonial authority, and preserving calendrical knowledge. Benefits from authority position and prestige. Minimal extraction burden — they are the beneficiary. See constraint as coordination mechanism protecting cultural continuity.
constraint_indexing:constraint_classification(puebloan_kiva_ceremonialism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: INTER-PUEBLO CEREMONIAL NETWORK (TANGLED ROPE) — Regional network of pueblos sharing ceremonial forms (kiva architecture, kachina cult, Two Horn initiation) across Tewa, Keres, and other language groups. Organized through exchange of dancers, ceremonial specialists, and ritual knowledge. Mobile exit option: pueblos can adopt/adapt ceremonies from neighbors or create novel forms. Mixed experience: genuine coordination of seasonal calendars and spiritual ecology across communities (collective benefit) with asymmetric extraction of specialized knowledge (secrets concentrated in certain pueblo lineages, other pueblos pay prestige/labor for access to authoritative versions).
constraint_indexing:constraint_classification(puebloan_kiva_ceremonialism, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: AIRFM/LEGAL PROTECTION REGIME (SCAFFOLD) — Federal protections (American Indian Religious Freedom Act Amendments 1994, Native American Graves Protection and Repatriation Act) create temporary relief from external constraints (suppression of kiva use, forced assimilation, police raids on ceremonies). The scaffold has a sunset: as indigenous legal standing solidifies, the protective framework becomes institutionalized and loses its temporary, emergency character. High beneficiary base (all pueblos) with declining suppression as legal regime matures. Theater ratio relatively low (genuine legal protection, not performative acknowledgment) because the threat was real and removal removes it.
constraint_indexing:constraint_classification(puebloan_kiva_ceremonialism, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: SPANISH COLONIAL SYNCRETISM OVERLAY (PITON) — Post-contact overlay of Catholic saints, Spanish language, and Christian narrative onto indigenous ceremonialism. Kivas ostensibly became Catholic sodality halls; kachinas became masked dancers performing for saints. The syncretism was initially an enforcement mechanism (Spanish priests required Christian facade) but persists largely through institutional inertia despite diminished external pressure. Theater ratio high (Catholic veneer covering indigenous practice) because the primary function (preserving indigenous ceremony under colonial repression) has atrophied while the performance (Catholic framing) persists. Piton classification derives from high theater, not from high extraction — the constraint is maintained more by habit and institutional continuity than by active enforcement.
constraint_indexing:constraint_classification(puebloan_kiva_ceremonialism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / ECOLOGICAL VIEW (MOUNTAIN) — From a civilizational perspective, seasonal ceremonialism coordinating human activity to ecological cycles (planting calendars, water management, game migration patterns) is an irreducible constraint: any community managing resources in a cyclical environment must coordinate ritual and ecological knowledge. The kiva system is the Puebloan solution to the universal problem of encoding ecological information in ceremony. From this view, the extraction and suppression are contingent institutional arrangements layered on top of an immutable coordination need. However, the base_properties reveal this as a false summit — the structural data shows significant asymmetric extraction and identity-based suppression that do not flow from the ecological problem itself.
constraint_indexing:constraint_classification(puebloan_kiva_ceremonialism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(puebloan_kiva_ceremonialism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(puebloan_kiva_ceremonialism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(puebloan_kiva_ceremonialism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(puebloan_kiva_ceremonialism, TR),
    TR >= 0.70.

:- end_tests(puebloan_kiva_ceremonialism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The kiva system does coordinate genuine public goods (agricultural calendar, conflict resolution, community cohesion) that benefit all participants. However, these benefits flow primarily through the authority hierarchy — those with ceremonial knowledge and status positions capture prestige, decision-making power, and resource allocation authority that subordinate members do not receive. The extractiveness is not as severe as pure snares (0.70+) because the coordination is real and beneficial to all, not merely theatrical or coercive. The value reflects that extraction is embedded in an otherwise-functional coordination mechanism. Suppression (0.72): High. Multiple layers: (1) Gender-based exclusion from male kivas (enforced through family structure and cultural authority); (2) Age-based exclusion of uninitiated members (enforced through ceremonial gates); (3) Knowledge gatekeeping preventing non-initiates from understanding ceremonial content (enforced through secrecy norms and social sanctions against teaching); (4) Identity-fusion making exit equivalent to cultural death (enforced through internalized identity rather than material barriers); (5) Legal/external historical suppression (police raids, forced assimilation pressure — now reduced by AIRFM but historically severe). Theater ratio (0.58): Moderate-high. Significant performative content in public kachina dances (framed for audience and aesthetic effect), Spanish Catholic veneer (saints integrated with kachina ceremonies), and maintenance of ceremonial forms for cultural continuity even when knowledge transmission has degraded. However, the core kiva initiation, water management decisions, and calendrical coordination have substantial functional content — not pure theater.
 *
 * PERSPECTIVAL GAP:
 *   The critical gap is between the non-initiated/subordinate perspective (snare/tangled rope) and the authority perspective (rope). Both groups participate in the same ceremonial system, but one experiences it as pure coordination solving collective problems while the other experiences it as extraction of knowledge and labor with minimal benefit. This gap arises from the identity-locked exit option for non-initiates — they cannot leave without abandoning community identity, so the suppression they experience is internalized and naturalized. For initiates with more mobility, the gap arises from the moiety/rank hierarchy — they can see that benefits and labor burdens are asymmetrically distributed, but their initiation gives them stake in the system's continuation. The inter-pueblo perspective creates a second gap: pueblos benefiting from prestige and knowledge-monopoly positions (typically Tewa or Keres groups) see rope, while pueblos importing ceremonial specialists see tangled rope. The analytical observer's gap is meta-level: the natural law / ecological necessity framing makes the constraint appear immutable from civilizational perspective, but the biographical perspectives reveal it as contingent institutional arrangement maintained through identity fusion and knowledge gatekeeping.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural relationship to the extraction flow. Non-initiated members are full victims with identity-locked exit: d ≈ 0.89 (very high extraction experienced). Initiated subordinates are mixed (some benefits from coordination, high extraction burden): d ≈ 0.55 (moderate extraction). Ceremonial specialists are beneficiaries with arbitrage options: d ≈ 0.15 (low/negative extraction experienced — they benefit). Inter-pueblo groups have mixed positions depending on dominance ranking: d ranges 0.40-0.60 (moderate extraction). External legal protections reduce overall d by removing suppression mechanism (extractiveness decreases over time as legal framework solidifies). The piton perspective has institutionalized beneficiaries (pueblo authorities maintaining Catholic syncretism for cultural continuity) with arbitrary beneficiaries: d ≈ 0.05-0.20 (low extraction because function has atrophied). The analytical observer with civilizational scope is not party to the extraction: d ≈ 0.72 (analytical canonical value), but risks naturalizing the structure and thus failing to detect the extraction present in the shorter time horizons.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through perspectival decomposition: the constraint is simultaneously (1) a genuine coordination mechanism (the snare/rope/tangled rope gap reflects real coordination benefits to all parties), (2) an extraction mechanism (the non-initiated experience pure extraction; the authorities experience pure benefit), and (3) an identity-fusion system (the identity-locked exit makes the suppression self-enforcing). The classical mandatrophy (is it coordination or extraction?) is resolved by noting that both are present, and their relative weight depends on the observer's structural position. The rope classification from the authority perspective is not 'wrong' — authorities genuinely are solving coordination problems. The snare classification from the non-initiated perspective is not 'wrong' — they genuinely do experience extraction. The tangled rope classification at the analytical level (biographical time, moderate power, constrained exit, local scope) captures the hybrid nature at the point where both mechanisms are visible. The false summit risk (naturalizing contingent institutional arrangements as ecological/spiritual necessity) is flagged by the analytical observer perspective producing mountain while biographical perspectives show extraction. This gap signals that the constraint's stability rests on identity fusion and knowledge gatekeeping rather than on immutable ecological necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_mechanism_strength,
    'How much of the non-initiate''s exclusion from ceremony is structural (legal/economic barriers preventing exit) versus internalized (identity-fused with exclusion, making exit unthinkable)?',
    'Longitudinal study of community members who leave; post-exit psychological measures (identity satisfaction, reconceptualization of community role); comparison with individuals forced into exile by external pressure vs voluntary migration',
    'If predominantly structural: classify non-initiate perspective as trapped (external barriers) rather than identity_locked. If predominantly internalized: identity_locked classification confirmed — the binding is cognitive rather than material, suggesting the constraint could dissolve with identity reframing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_strength, empirical, 'Structural vs internalized suppression in non-initiate exclusion').

omega_variable(
    ceremonial_knowledge_gatekeeping_necessity,
    'Is strict hierarchical secrecy of kiva ceremonialism functionally necessary for preserving knowledge transmission, or does it serve primarily to concentrate authority?',
    'Comparative analysis with other indigenous knowledge systems (Pacific Northwest potlatch, Navajo weaving, Hawaiian hula); assessment of whether knowledge loss follows from decreased secrecy (e.g., after publication of Some Tewa Tales, anthropological documentation of ceremonialism); examination of whether open documentation in other pueblos or among exilic communities reduces transmission fidelity',
    'If necessary: suppression level reflects genuine coordination cost rather than extraction mechanism — revise extractiveness downward. If authority-driven: suppression is extracted value (gatekeeping rent) rather than coordination requirement — maintain or revise extractiveness upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ceremonial_knowledge_gatekeeping_necessity, empirical, 'Whether ceremonial secrecy is functionally necessary or authority-concentrating').

omega_variable(
    syncretism_persistence_mechanism,
    'Does the Catholic veneer (saints, Spanish language, Christian narrative) persist because external pressure remains, because internal identity has genuinely fused Catholic and indigenous framing, or purely through institutional inertia?',
    'Ethnographic documentation of individual pueblo practices; analysis of where syncretism is performed (in public contexts with external audience) vs where pure indigenous ceremony occurs (kiva-internal); measurement of whether younger generations maintain syncretism belief vs performatively maintain it for community continuity',
    'If external pressure: syncretism is structural suppression mechanism. If fused identity: syncretism is identity_coordination type with genuine dual belonging. If inertia: piton classification confirmed — constraint persists despite minimal function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(syncretism_persistence_mechanism, empirical, 'Persistence mechanism of Catholic-indigenous syncretism').

omega_variable(
    inter_pueblo_knowledge_exchange_asymmetry,
    'Do inter-pueblo ceremonial exchanges benefit all communities symmetrically, or do certain pueblos (e.g., Tewa, Keres dominant groups) extract prestige and authority disproportionately?',
    'Historical analysis of which pueblos ''export'' ceremonial specialists vs which ''import'' them; tracking of authority positions held by specialists from each pueblo in shared ceremonies; comparison of demographic and economic outcomes for pueblo groups in high vs low ceremonial prestige positions',
    'If symmetric: inter-pueblo perspective is pure rope (coordination across communities). If asymmetric: inter-pueblo perspective is tangled rope (genuine coordination with embedded extraction of prestige/labor from less-dominant groups).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inter_pueblo_knowledge_exchange_asymmetry, empirical, 'Symmetry of inter-pueblo ceremonial knowledge exchange').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(puebloan_kiva_ceremonialism, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kiva_tr_t0, puebloan_kiva_ceremonialism, theater_ratio, 0, 0.75).
narrative_ontology:measurement(kiva_tr_t40, puebloan_kiva_ceremonialism, theater_ratio, 40, 0.62).
narrative_ontology:measurement(kiva_tr_t80, puebloan_kiva_ceremonialism, theater_ratio, 80, 0.58).
narrative_ontology:measurement(kiva_tr_t120, puebloan_kiva_ceremonialism, theater_ratio, 120, 0.55).

% Extraction over time
narrative_ontology:measurement(kiva_be_t0, puebloan_kiva_ceremonialism, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(kiva_be_t40, puebloan_kiva_ceremonialism, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(kiva_be_t80, puebloan_kiva_ceremonialism, base_extractiveness, 80, 0.35).
narrative_ontology:measurement(kiva_be_t120, puebloan_kiva_ceremonialism, base_extractiveness, 120, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(puebloan_kiva_ceremonialism, identity_coordination).
narrative_ontology:affects_constraint(puebloan_kiva_ceremonialism, pueblo_gender_exclusion_ceremonial).
narrative_ontology:affects_constraint(puebloan_kiva_ceremonialism, native_american_knowledge_protection).
narrative_ontology:affects_constraint(puebloan_kiva_ceremonialism, southwest_water_rights_governance).

% DUAL FORMULATION NOTE:
% Puebloan kiva ceremonialism decomposes into structurally distinct constraints with different ε values: (1) kiva initiation hierarchy (extractiveness ≈ 0.35, this story) — identity-based authority system; (2) gender exclusion from male kivas (extractiveness ≈ 0.62) — pure asymmetric extraction without coordination function; (3) ceremonial knowledge gatekeeping (extractiveness ≈ 0.48) — coordination with embedded knowledge monopoly. These are linked through network affects but analyzed separately because their base properties and victim/beneficiary structures differ.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(puebloan_kiva_ceremonialism, organized, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
