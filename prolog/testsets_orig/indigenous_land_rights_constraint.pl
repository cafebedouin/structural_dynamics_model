% ============================================================================
% CONSTRAINT STORY: indigenous_land_rights_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indigenous_land_rights_constraint, []).

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
 *   constraint_id: indigenous_land_rights_constraint
 *   human_readable: Indigenous Land Rights Constraint
 *   domain: political_economy/indigenous_rights
 *
 * SUMMARY:
 *   Indigenous land rights constraints represent a global structural
 *   phenomenon in which state systems, legal frameworks, and economic systems
 *   built on colonial dispossession systematically extract land sovereignty,
 *   resource access, and cultural continuity from indigenous nations while
 *   maintaining legitimacy through law, international frameworks, and
 *   recognition of limited rights. This constraint exhibits characteristics
 *   of pure extraction (snare) from the perspective of indigenous nations and
 *   communities, but appears as coordination mechanism (rope) to states and
 *   as mixed dynamics (tangled rope) to local settler communities and
 *   organized indigenous movements. The constraint's evolution shows
 *   decreasing extractiveness (0.85 → 0.68) but increasing theater (0.25 →
 *   0.58): actual material dispossession has modestly declined due to
 *   indigenous resistance and some legal recognition, but performative
 *   acknowledgment of indigenous rights has increased dramatically.
 *   International legal frameworks (UNDRIP, ILO 169) and state consultation
 *   protocols are largely theatrical — commitment without transfer of actual
 *   power. The constraint persists across centuries and jurisdictions,
 *   indicating fundamental structural asymmetry rather than coordination
 *   failure. Indigenous communities face binding mechanisms that are
 *   simultaneously material (dispossession, legal exclusion, economic
 *   marginalization), cognitive (identity fusion with ancestral lands,
 *   cultural framing that exit is unthinkable), and institutional (state
 *   monopoly on legitimate land claims).
 *
 * KEY AGENTS:
 *   - Indigenous Nations: Primary victim (powerless/trapped) — dispossessed of ancestral lands through colonization and legal systems designed post-hoc to legitimize dispossession
 *   - Tribal Communities and Individual Members: Primary victim (powerless/identity_locked) — face both structural barriers to exit (no alternative land base, economic dependency) and identity fusion with territory (kinship, spiritual connection, cultural continuity constituted through land)
 *   - State Apparatus: Primary beneficiary (institutional/arbitrage) — maintains territorial monopoly, resource extraction authorization, and tax base; experiences constraint as coordination mechanism that allows recognition of limited rights at minimal cost
 *   - Extraction Industries: Primary beneficiary (powerful/arbitrage) — access to resources (minerals, timber, water, agricultural land) at below-market rates; minimal consultation obligations; externalized environmental costs
 *   - Settler Populations: Secondary beneficiary (variable power/constrained to mobile) — access to cheap land, labor, and resources; some local groups face coordination problems with indigenous populations
 *   - Indigenous Rights Movements: Organized resistance (organized/constrained) — perceive genuine coordination function (equitable land frameworks are possible) while experiencing the constraint as extractive (current frameworks protect settler interests)
 *   - International Legal Framework: Institutional (institutional/arbitrage) — maintains performative commitment through treaties and declarations while enforcement remains delegated to states; persists through institutional inertia
 *   - Analytical Observer: Systemwide view (analytical/analytical) — recognizes pure extraction pattern across jurisdictions and centuries; identifies that coordination framing is asymmetric
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indigenous_land_rights_constraint, 0.68).
domain_priors:suppression_score(indigenous_land_rights_constraint, 0.75).
domain_priors:theater_ratio(indigenous_land_rights_constraint, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indigenous_land_rights_constraint, extractiveness, 0.68).
narrative_ontology:constraint_metric(indigenous_land_rights_constraint, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(indigenous_land_rights_constraint, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indigenous_land_rights_constraint, snare).
narrative_ontology:human_readable(indigenous_land_rights_constraint, "Indigenous Land Rights Constraint").
narrative_ontology:topic_domain(indigenous_land_rights_constraint, "political_economy/indigenous_rights").

domain_priors:requires_active_enforcement(indigenous_land_rights_constraint).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(indigenous_land_rights_constraint, state_apparatus).
narrative_ontology:constraint_beneficiary(indigenous_land_rights_constraint, extraction_industries).
narrative_ontology:constraint_beneficiary(indigenous_land_rights_constraint, settler_populations).
narrative_ontology:constraint_victim(indigenous_land_rights_constraint, indigenous_nations).
narrative_ontology:constraint_victim(indigenous_land_rights_constraint, tribal_communities).
narrative_ontology:constraint_victim(indigenous_land_rights_constraint, land_sovereignty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIGENOUS NATIONS (SNARE) — Structurally trapped by historical dispossession, legal frameworks written by colonizers, and geopolitical dependencies. Exit options are material impossibilities: land cannot be recovered through markets; legal systems are designed to exclude indigenous claims; relocation is economically and culturally catastrophic. The constraint extracts sovereignty, resource access, and cultural continuity with minimal coordination function. No alternatives visible from within the trapped position.
constraint_indexing:constraint_classification(indigenous_land_rights_constraint, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INDIVIDUAL COMMUNITY MEMBERS (SNARE via identity_locked) — Structurally mobile (could migrate, adopt settler identity, seek individual property rights) but identity-locked by kinship to land, spiritual connection to territory, and communal identity that is constituted through the relationship to ancestral lands. Exit would require becoming a different person — abandoning the identity framework that makes them who they are within the community. The constraint extracts labor, cultural continuity, and participation in dispossession. Identity lock deepens suppression because the agent carries the binding with them if they physically leave.
constraint_indexing:constraint_classification(indigenous_land_rights_constraint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 3: STATE APPARATUS (ROPE) — Benefits from the constraint through territorial monopoly, tax base expansion, resource extraction authorization, and political stability (indigenous political threats neutralized through legal exclusion). Experiences the constraint as coordination mechanism: recognizing limited indigenous rights (land trusts, consultation protocols, token revenue-sharing) enables state legitimacy and reduces confrontation costs. The state has arbitrage options — it can reformulate the constraint (e.g., via treaty recognition) without losing core benefits, making exit cheap. From this perspective, the constraint solves a coordination problem (recognizing limited claims reduces conflict costs).
constraint_indexing:constraint_classification(indigenous_land_rights_constraint, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LOCAL SETTLER/MIXED COMMUNITIES (TANGLED ROPE) — Face genuine coordination needs (resource management, boundary maintenance, conflict reduction) alongside extraction dynamics. Some groups benefit from cheap indigenous labor and land access; others are constrained by indigenous competition and cultural friction. Mixed benefits create a tangled structure: coordination mechanism (managing coexistence) and asymmetric extraction (indigenous groups bear disproportionate costs). Exit is constrained by property investments and community ties but not impossible at significant cost.
constraint_indexing:constraint_classification(indigenous_land_rights_constraint, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: INTERNATIONAL LEGAL FRAMEWORK (PITON) — UN Declaration on Rights of Indigenous Peoples, ILO Convention 169, and similar instruments exist as established international commitments but are largely performative at the implementation level. States pay lip service while maintaining structural dispossession. The framework persists through institutional inertia (treaty obligations, bureaucratic compliance) despite low functional enforcement. Theater ratio increases as indigenous advocacy groups produce reports confirming non-compliance, which governments acknowledge while maintaining the same extraction mechanisms. The legal framework is degraded ritual, not active coordination.
constraint_indexing:constraint_classification(indigenous_land_rights_constraint, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INDIGENOUS RIGHTS MOVEMENTS (TANGLED ROPE) — Organized agents (tribal councils, international indigenous networks, NGO coalitions) perceive genuine coordination function: land rights frameworks CAN coordinate coexistence and mutual recognition. They also perceive the constraint as extractive: current frameworks extract recognition without redistributing material power. The movements are constrained by legal systems designed to exclude them but have agency through coalition, litigation, and international advocacy. Classification is tangled rope because movements see both a coordination problem that CAN be solved (equitable land frameworks exist in theory) and an extraction mechanism that CURRENTLY operates (existing frameworks protect settler interests).
constraint_indexing:constraint_classification(indigenous_land_rights_constraint, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From civilizational/global scope, the constraint appears as pure extraction: land dispossession from indigenous nations, structural suppression via legal systems designed post-hoc to legitimize colonization, minimal coordination function (coexistence mechanisms serve settler interests disproportionately). The analytical position reveals that the 'coordination' framing from the state and local perspectives is asymmetric — coordination that extracts all genuine benefits to one party. The constraint's existence across centuries and continents despite continuous resistance indicates fundamental structural asymmetry: the constraint is maintained by power differentials, not by genuine shared interest in coordination.
constraint_indexing:constraint_classification(indigenous_land_rights_constraint, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indigenous_land_rights_constraint_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(indigenous_land_rights_constraint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indigenous_land_rights_constraint, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(indigenous_land_rights_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(indigenous_land_rights_constraint, TR),
    TR >= 0.70.

:- end_tests(indigenous_land_rights_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High-extraction constraint that has declined from 0.85 over the interval. The decline reflects genuine indigenous resistance successes (some land recovery, treaty recognition, legal victories) but not fundamental redistribution of power. The remaining 0.68 reflects continued material dispossession of resource-rich territories, maintenance of legal systems that exclude indigenous claims, and economic structures that benefit extraction industries and settler states. Suppression (0.75): Very high suppression. Mechanisms include: (1) Material barriers — no alternative land base, economic dependency on state services, geographic isolation; (2) Legal exclusion — state monopoly on legitimate property claims, colonizer legal systems applied retroactively; (3) Cognitive — identity fusion with territory makes exit unthinkable; (4) Institutional — states designed to prevent indigenous political power, international frameworks toothless without state enforcement. Theater ratio (0.58, increasing from 0.25): Moderate-high theater reflecting the growing gap between performative international commitments (UNDRIP, consultation protocols, land acknowledgments) and actual material power transfer. The theater has increased as indigenous advocacy has forced states to develop rhetorical compliance mechanisms that do not change underlying extraction. Mandatrophy resolved: The constraint clearly exhibits pure extraction (snare classification) from the victim perspective, mixed dynamics (tangled rope) from organized indigenous movements, and coordination benefit (rope) from state beneficiaries. The mandatrophy is resolved by recognizing that the constraint's primary function is extraction (land and sovereignty dispossession), not coordination — the 'coordination' elements are asymmetric and serve primarily to legitimize ongoing extraction.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival divergence. Indigenous nations see pure extraction (snare) with no coordination function — the constraint takes everything and offers nothing. State apparatus sees primarily coordination (rope) — recognizing limited rights enables state legitimacy and reduces confrontation costs at minimal sacrifice. Organized indigenous movements see tangled rope — genuine coordination function (equitable frameworks exist in theory) alongside extraction (current frameworks protect settler interests). The international legal framework sees itself as rope (coordination via treaty obligation) while actually serving as piton (performative commitment without enforcement). The analytical observer sees pure extraction masked by coordination rhetoric. This gap reveals that the constraint's legitimacy depends on different stakeholders perceiving different primary functions: the state claims coordination, indigenous nations experience extraction, international frameworks perform coordination while enabling extraction. No single perspective captures the full structure — the presheaf (all perspectives together) shows systematic asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) flow from structural position relative to extraction. Indigenous nations face d ≈ 0.95 (full targets of extraction): trapped (no material exit options) + victim status (bear all costs) = maximum experienced extractiveness. Individual community members face d ≈ 0.89 (identity-locked targets): structurally mobile (could migrate) but identity-fused (exit unthinkable) + victim status = high experienced extraction with cognitive component. State apparatus faces d ≈ 0.05 (full beneficiary): institutional power + arbitrage exit options (can reformulate constraints at low cost) + beneficiary status = negative experienced extractiveness (constraint subsidizes the state). Extraction industries face d ≈ 0.10 (strong beneficiary with minor constrained elements): powerful actors + arbitrage exit (can operate elsewhere) + beneficiary status = low extracted cost. Organized indigenous movements face d ≈ 0.55 (moderate targets with agency): organized power + constrained exit (legal system barriers, but coalition capacity) + mixed victim-beneficiary status (benefit from rights frameworks in theory, bear costs in practice) = moderate experienced extraction. Local settler communities face d ≈ 0.45 (mixed): moderate power + constrained exit (property investment, community ties) + mixed beneficiary-victim status = moderate experienced extraction. The analytical observer faces d ≈ 0.72 (systematic victimhood of coordination itself being extracted): analytical position that sees the extraction is global pattern, not coordination problem.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by rejecting the false equivalence between perspectives. This is NOT a case where 'it depends on your viewpoint' — it is a case where the constraint serves one party's extraction and another party's survival. Perspectives that classify the constraint as 'coordination' (state apparatus, rope) are generated by agents who benefit from the extraction. Perspectives that classify it as 'snare' (indigenous nations, powerless/trapped) are generated by agents who bear the extraction. The analytical observer's snare classification is structurally justified: across jurisdictions and centuries, the constraint exhibits pure extraction characteristics (high asymmetric material flow, minimal genuine coordination function, suppression mechanisms designed to prevent indigenous agency). The state's rope classification is revealed as beneficiary-side framing that naturalizes extraction as coordination. RESOLUTION: The constraint's ground truth classification is snare (pure extraction), not rope (coordination). The perspectives that call it rope or tangled rope are documenting the extraction mechanism itself — how beneficiaries frame extraction as coordination to maintain legitimacy. The constraint resolves mandatrophy by showing that indexical classification correctly surfaces this: perspectives organized to benefit from extraction report coordination; perspectives organized to resist extraction report pure extraction. The engine's task is to identify which is structural reality. The evidence (centuries of dispossession despite indigenous resistance, continued asymmetry despite international legal frameworks, theater increase as material change declines) indicates structural reality is extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    land_claim_legitimacy_standard,
    'What standard determines legitimate indigenous land claims vs. competing settler property claims when both are legally established within the state system?',
    'Historical analysis of land acquisition methods; determination of whether indigenous claims rest on prior occupation vs. settler claims rest on state-authorized purchase/allocation; cross-national comparison of recognition criteria',
    'If indigenous prior-occupation carries stronger moral/legal weight: current constraint classification as snare is vindicated (dispossession is illegitimate extraction). If settler legal purchase carries equal weight: constraint might reclassify toward tangled_rope (competing legitimate claims requiring coordination). This is the crux omega.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(land_claim_legitimacy_standard, conceptual, 'Standard for determining legitimate land claim priority').

omega_variable(
    extraction_vs_coordination_tipping_point,
    'At what point does a land rights framework transition from extractive (primarily dispossessing indigenous nations) to coordinative (genuinely sharing authority and benefits)?',
    'Comparative analysis of indigenous autonomy in different regimes (e.g., Sami in Scandinavia vs. Aboriginal in Australia vs. Native American in USA); measurement of indigenous veto power over resource extraction, land use decisions, and policy affecting their territories; tracking revenue flows to indigenous governments vs. settlers',
    'If tipping point is reached in any jurisdiction: scaffold or rope classifications become viable (constraint can evolve toward coordination). If no jurisdiction has crossed the threshold: global snare classification is confirmed. Current evidence suggests no jurisdiction has genuine parity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_tipping_point, empirical, 'Tipping point for extraction-to-coordination transition in land rights regimes').

omega_variable(
    identity_lock_persistence_post_exit,
    'For indigenous individuals who physically relocate away from ancestral territory, does the identity-lock binding persist or dissolve?',
    'Longitudinal studies of indigenous diaspora populations; interviews tracking identity persistence, return motivations, and intergenerational transmission; measurement of whether post-exit suppression (cultural isolation, identity dissolution pressure) persists or fades',
    'If identity lock persists: suppression is partially internalized and post-exit support/cultural infrastructure is critical for genuine exit capacity. If identity lock dissolves: exit is structurally possible at the cost of identity transformation; constraint is constraining rather than fully trapping at individual level. Current evidence suggests identity persistence and post-exit suppression (diaspora communities maintain strong connection to homeland), supporting identity_locked classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence_post_exit, empirical, 'Whether identity lock persists or dissolves post-exit from territory').

omega_variable(
    state_arbitrage_window,
    'Can the state maintain full benefits of land dispossession while genuinely recognizing indigenous sovereignty and resource control?',
    'Analysis of state revenue dependencies on extractive industries operating on indigenous lands; identification of scenarios where indigenous veto power could be granted without threatening state fiscal capacity; modeling of revenue-sharing regimes that might satisfy both parties',
    'If true arbitrage exists: state perspective might shift from rope to scaffold (temporary recognition regime leading to transition). If no arbitrage exists: state benefits fundamentally depend on maintained suppression of indigenous claims; state would never genuinely exit. Current evidence suggests no arbitrage — state and extraction industry profits depend on continued access.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_arbitrage_window, empirical, 'Whether state can maintain benefits while granting indigenous sovereignty').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indigenous_land_rights_constraint, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ilr_tr_t0, indigenous_land_rights_constraint, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ilr_tr_t50, indigenous_land_rights_constraint, theater_ratio, 50, 0.45).
narrative_ontology:measurement(ilr_tr_t100, indigenous_land_rights_constraint, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(ilr_be_t0, indigenous_land_rights_constraint, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(ilr_be_t50, indigenous_land_rights_constraint, base_extractiveness, 50, 0.72).
narrative_ontology:measurement(ilr_be_t100, indigenous_land_rights_constraint, base_extractiveness, 100, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indigenous_land_rights_constraint, resource_allocation).
narrative_ontology:affects_constraint(indigenous_land_rights_constraint, colonial_institutional_legacies).
narrative_ontology:affects_constraint(indigenous_land_rights_constraint, environmental_extraction_asymmetry).
narrative_ontology:affects_constraint(indigenous_land_rights_constraint, indigenous_political_exclusion).

% DUAL FORMULATION NOTE:
% Indigenous land rights constraint decomposes into three related stories: (1) colonial_institutional_legacies: the legal and administrative systems designed to legitimize dispossession (piton/mountain depending on emergence framing); (2) environmental_extraction_asymmetry: resource extraction industries' access to indigenous territories at below-market rates (snare specific to extractive industries); (3) indigenous_political_exclusion: state monopoly on legitimate political authority in territory claimed by indigenous nations (snare specific to sovereignty). These three constraints share the same beneficiaries (states, extraction industries) and victims (indigenous nations) but operate through different mechanisms (institutional, economic, political). All three are affected by and reinforce indigenous_land_rights_constraint at the summary level.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(indigenous_land_rights_constraint, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
