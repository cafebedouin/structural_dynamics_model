% ============================================================================
% CONSTRAINT STORY: turkish_kurdish_regional_conflict
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_turkish_kurdish_regional_conflict, []).

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
 *   constraint_id: turkish_kurdish_regional_conflict
 *   human_readable: Turkish-Kurdish Regional Conflict Constraint
 *   domain: geopolitical/ethnic_conflict
 *
 * SUMMARY:
 *   The Turkish-Kurdish regional conflict represents a structural constraint
 *   where military force, political repression, and institutional control
 *   mechanisms extract costs from Kurdish civilian populations and constrain
 *   political autonomy, while concentrating benefits to Turkish state
 *   security apparatus and nationalist political factions. The constraint
 *   exhibits characteristics of a pure extraction snare from the perspective
 *   of trapped civilian populations, but appears as tangled
 *   coordination-extraction hybrid from organized Kurdish political actors'
 *   perspective, as tactical coordination mechanism from the state security
 *   apparatus, and as a false natural law from the analytical civilizational
 *   perspective that naturalizes ethnic territoriality as immutable. The
 *   extractiveness has increased over the 30-year interval (0.42 → 0.68) as
 *   security operations have intensified, while theater_ratio has remained
 *   moderate (0.38 → 0.55), indicating that while significant performative
 *   components exist (ritualistic checkpoint operations, ceremonial military
 *   deployments), the constraint also maintains functional extraction
 *   mechanisms (genuine displacement, economic marginalization, political
 *   restrictions). The moderate theater ratio distinguishes this from pure
 *   piton degradation — the constraint is not primarily maintained by
 *   institutional inertia, but by active enforcement of asymmetric
 *   extraction.
 *
 * KEY AGENTS:
 *   - Turkish State Security Apparatus: Primary beneficiary (institutional/arbitrage) — benefits from territorial control, counter-insurgency justification for military budgets, and monopoly on legitimate force. Can adjust enforcement tactics and negotiate without exiting state structure.
 *   - Nationalist Political Parties: Secondary beneficiary (powerful/mobile) — gain electoral and ideological leverage from perpetuating securitization narrative; can shift political position while maintaining nationalist positioning.
 *   - Kurdish Civilian Population: Primary victim (powerless/trapped) — bears extraction through military operations, restricted movement, displacement risk, and economic marginalization. Geographically trapped with no exit option.
 *   - Border Region Communities: Secondary victim (powerless/trapped) — trapped in militarized zones with restricted economic activity. Face extraction through security operations and institutional neglect.
 *   - Kurdish Political Movements: Organized victim (organized/constrained) — experience both genuine coordination function (representation, advocacy) and asymmetric extraction (party closures, leadership detention, legal restrictions). Have constrained but not impossible exit options.
 *   - International Community (NATO/EU): Interested third party (powerful/mobile) — have genuine coordination interests (counter-terrorism, stability, alliance maintenance) alongside strategic leverage extraction. Mobile exit options provide high agency.
 *   - Human Rights Organizations: Partial beneficiary of alternatives (organized/constrained) — experience constraint as solvable through political settlement and international mechanisms. Constrained by funding and political influence but experience low direct extraction.
 *   - Analytical Observer: Risks naturalizing contingent arrangements — may perceive ethnic territoriality as immutable natural law rather than institutional construct.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(turkish_kurdish_regional_conflict, 0.68).
domain_priors:suppression_score(turkish_kurdish_regional_conflict, 0.78).
domain_priors:theater_ratio(turkish_kurdish_regional_conflict, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(turkish_kurdish_regional_conflict, extractiveness, 0.68).
narrative_ontology:constraint_metric(turkish_kurdish_regional_conflict, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(turkish_kurdish_regional_conflict, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(turkish_kurdish_regional_conflict, snare).
narrative_ontology:human_readable(turkish_kurdish_regional_conflict, "Turkish-Kurdish Regional Conflict Constraint").
narrative_ontology:topic_domain(turkish_kurdish_regional_conflict, "geopolitical/ethnic_conflict").

domain_priors:requires_active_enforcement(turkish_kurdish_regional_conflict).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(turkish_kurdish_regional_conflict, turkish_state_security_apparatus).
narrative_ontology:constraint_beneficiary(turkish_kurdish_regional_conflict, nationalist_political_parties).
narrative_ontology:constraint_victim(turkish_kurdish_regional_conflict, kurdish_populations_southeast).
narrative_ontology:constraint_victim(turkish_kurdish_regional_conflict, civilian_communities_border_regions).
narrative_ontology:constraint_victim(turkish_kurdish_regional_conflict, regional_economic_development).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: KURDISH CIVILIAN POPULATION (SNARE) — Structurally trapped within geographic region with no exit options. Faces extraction through military operations, displacement risk, restricted movement, and economic marginalization. Cannot exit region without abandoning land, property, and community. High suppression through armed presence and institutional barriers to autonomous governance.
constraint_indexing:constraint_classification(turkish_kurdish_regional_conflict, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: BORDER REGION COMMUNITIES (SNARE) — Trapped within militarized zones with restricted economic activity and movement. Face extraction through economic constraints, security operations, and institutional neglect. High suppression through security state apparatus and closure of normal commerce routes.
constraint_indexing:constraint_classification(turkish_kurdish_regional_conflict, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: KURDISH POLITICAL MOVEMENTS (TANGLED ROPE) — Organized agents (political parties, civil society) face constrained but not impossible exit options. Experience genuine coordination function (representation of community interests, advocacy for autonomy) alongside asymmetric extraction (legal restrictions, party closures, leadership detention). Active enforcement required to maintain the extraction layer.
constraint_indexing:constraint_classification(turkish_kurdish_regional_conflict, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: TURKISH STATE SECURITY APPARATUS (ROPE) — Primary beneficiary with arbitrage options (can shift enforcement tactics, negotiate ceasefires, adjust military posture without exiting the state structure). Experiences the constraint as coordination: managing territorial integrity, controlling security threats, and maintaining state monopoly on force. Net beneficiary with high agency.
constraint_indexing:constraint_classification(turkish_kurdish_regional_conflict, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL ACTORS (TANGLED ROPE) — NATO and EU actors have genuine coordination interests (counter-terrorism, regional stability) alongside asymmetric extraction (geopolitical leverage, conditional aid, strategic advantage). Mobile exit options enable influence without direct extraction; constrained by alliance relationships and strategic interests.
constraint_indexing:constraint_classification(turkish_kurdish_regional_conflict, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: HUMAN RIGHTS AND PEACE ORGANIZATIONS (SCAFFOLD) — Organized agents with sunset logic: peace agreements, constitutional reforms protecting minority rights, and international monitoring mechanisms represent exit pathways. Low extraction experienced because these actors see alternatives and have partial agency through advocacy and documentation.
constraint_indexing:constraint_classification(turkish_kurdish_regional_conflict, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: BORDER CONTROL RITUALS (PITON) — Institutional enforcement patterns (security protocols, checkpoint operations, military patrols) persist largely through historical inertia. Theater ratio high because much of the visible enforcement serves performative functions (demonstrating state control, ritualistic securitization) rather than solving underlying coordination problem. The institutional apparatus maintains itself despite diminishing functional necessity.
constraint_indexing:constraint_classification(turkish_kurdish_regional_conflict, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER - FALSE NATURAL LAW (MOUNTAIN) — This perspective risks naturalizing the constraint as an immutable law of geopolitics: that ethnic groups inevitably compete for territory, that states inherently resist internal fragmentation, that regional conflicts are permanent features of the landscape. The engine's false summit detector will flag this: the structural data shows contingent institutional arrangements (state construction, security doctrine, political ideology) rather than natural laws. This perspective instantiates the oracle gap.
constraint_indexing:constraint_classification(turkish_kurdish_regional_conflict, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(turkish_kurdish_regional_conflict_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(turkish_kurdish_regional_conflict, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(turkish_kurdish_regional_conflict, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(turkish_kurdish_regional_conflict, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(turkish_kurdish_regional_conflict, TR),
    TR >= 0.70.

:- end_tests(turkish_kurdish_regional_conflict_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts substantial costs from Kurdish populations through security operations, movement restrictions, economic marginalization, and political disenfranchisement. The trajectory from 0.42 to 0.68 over 30 years reflects intensification of security operations (1990s through post-2015 period). The beneficiaries (state security apparatus, nationalist politicians) accrue career advancement, institutional resources, and political leverage from perpetuation of the conflict. This is not maximal extraction (0.72+) because some coordination function exists: the state genuinely addresses security threats (though their scale is partially constructed), and international actors coordinate counter-terrorism. Suppression (0.78): Very high. Significant barriers to exit include geographic immobility, military presence, restricted movement across borders, institutional discrimination, political restrictions on Kurdish political expression, and psychological trauma from violence. Suppression is not total (some exit occurs through migration, internal displacement, political participation at constrained levels) but remains severe. Theater ratio (0.55): Moderate-high. Significant performative components exist: ceremonial military deployments, ritualistic checkpoint operations, public security announcements that serve domestic political functions. But the theater does not dominate — actual military operations, detention, and economic restriction mechanisms are functionally extractive, not primarily performative. This moderates the piton classification.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates dramatic perspectival divergence. The trapped Kurdish civilian population experiences pure extraction (Snare) — no perceived coordination benefit, maximum experienced extraction. Organized Kurdish political movements experience tangled coordination-extraction hybrid (Tangled Rope) — they coordinate representation of community interests and advance political autonomy claims, but simultaneously face restrictions and asymmetric legal/political constraints. The Turkish state security apparatus experiences genuine coordination (Rope) — the constraint solves the problem of territorial control and counter-insurgency; the state perceives itself as solving a security coordination problem, not extracting from civilians. International actors experience Tangled Rope with lower extraction experienced — they coordinate counter-terrorism while leveraging strategic advantage, but have sufficient agency and alternatives to prevent maximum extraction. Human rights and peace organizations experience a Scaffold with visible sunset pathways (constitutional reform, political settlement, international monitoring). The analytical observer at civilizational scale risks a false Mountain classification — naturalizing ethnic territoriality and state-ethnic group conflict as immutable features of geopolitics. The perspectival gap between the powerless victim (Snare), the state beneficiary (Rope), and the civilizational observer (Mountain) reveals how single-position analysis (state perspective) produces false natural law conclusions about ethnic conflict.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by structural position, exit options, and benefit flow. Turkish state security apparatus benefits from the constraint and has arbitrage options (can adjust tactics, negotiate, shift security doctrine without exiting state structure) → low d → experienced as coordination/low extraction. Kurdish civilians are victims, trapped geographically and institutionally, with no exit option → high d → maximum experienced extraction (f(d) at ~0.95). Kurdish political movements are victims but organized with constrained exit options (can negotiate, shift strategy) → moderate-high d (0.65-0.75) → moderate experienced extraction. International actors are neither pure beneficiaries nor pure victims, with mobile exit options → moderate d (0.50-0.60) → moderate experienced extraction with perception of agency. The identity_locked vs constrained distinction is critical: if Kurdish populations experience the constraint as identity-locked (self-concept fused with territorial claim and ethnic nationalism), the biographical-time perspective shifts from Snare (constrained exit) toward potential Rope visibility only if identity frame breaks. If the constraint is primarily constrained (high material costs to exit), then the classification remains Snare at biographical time but Rope becomes visible at generational horizon as costs diminish. The omega variable on this ambiguity will determine whether exit barriers are primarily structural or cognitive.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandatrophy is resolved by recognizing that multiple legitimate classifications emerge from different structural positions and time horizons, and that the false natural law (Mountain) at the civilizational analytical perspective reveals institutional contingency rather than geopolitical necessity. The state beneficiary perceives Rope (coordination function: territorial control, counter-insurgency). The trapped victim perceives Snare (pure extraction). The organized political actor perceives Tangled Rope (mixed coordination and extraction). The international actor perceives Tangled Rope with lower extraction (sufficient agency and alternatives). The peace organization perceives Scaffold (visible exit pathways through political settlement). The analytical observer risks Mountain (naturalizing ethnic conflict as inevitable). The engine's mandatrophy resolution: (1) Flag the Mountain perspective as a false summit — the structural data (moderate theater ratio, identified beneficiary class, absence of accessibility collapse and resistance metrics required for NL certification) contradicts NL classification. (2) Recognize that the constraint's type depends on which position one occupies: this is not a defect but an insight into how extractive constraints appear differently to beneficiaries vs victims. (3) Note that the Scaffold classification (via human rights organizations and constitutional reform pathways) provides a genuine exit vector: the constraint's classification could shift if political settlement mechanisms mature. The mandatrophy is not resolved by picking one 'correct' type, but by acknowledging that indexical classification reveals the constraint's asymmetry: the same structure appears as coordination to beneficiaries, extraction to victims, and natural law to those who benefit from naturalizing it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pkk_insurgency_status_ambiguity,
    'Is the PKK (Kurdistan Workers Party) a nationalist liberation movement, a terrorist organization pursuing separatism, or a hybrid actor with legitimate political grievances channeled through insurgent tactics?',
    'Analysis of PKK organizational evolution, documented civilian targeting patterns, political program changes over time, international designation inconsistencies, and comparison with other recognized liberation movements',
    'If primarily nationalist: constraint is ethnic conflict requiring political settlement (classification shifts toward scaffold). If primarily terrorist: constraint is state counter-terrorism requiring security dominance (validates snare from state perspective). If hybrid: constraint is tangled_rope with genuine asymmetric grievance alongside security threats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pkk_insurgency_status_ambiguity, empirical, 'Classification of PKK organizational nature and political status').

omega_variable(
    state_decentralization_feasibility,
    'Could Turkish state maintain territorial integrity and democratic governance through constitutional decentralization granting substantial autonomy to Kurdish regions?',
    'Comparative institutional analysis: case studies of federal systems managing ethnic-national minorities (Spain/Catalonia, Belgium/Flanders, India/regional states); modeling of Turkish state capacity for devolved governance; analysis of separatist vs autonomy demands in Kurdish movement',
    'If feasible: constraint is institutional design problem (Scaffold classification gains credibility — sunset pathway exists). If infeasible: constraint is structural incompatibility (Snare from victim perspective remains dominant). If partially feasible: Tangled Rope classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_decentralization_feasibility, empirical, 'Whether constitutional decentralization could resolve the constraint').

omega_variable(
    extraction_beneficiary_ambiguity,
    'Who structurally benefits from perpetuation of the Turkish-Kurdish conflict? Is it primarily state security apparatus, nationalist political factions, military-industrial interests, or a coalition of actors with different benefit structures?',
    'Analysis of career advancement patterns in security establishment during high-conflict vs peaceful periods; funding flows to nationalist vs progressive parties correlated with conflict intensity; military procurement cycles and domestic defense spending patterns; international arms sales and strategic leverage gains',
    'If security apparatus primarily benefits: Snare classification confirmed. If nationalist political factions primarily benefit: classification shifts to tangled_rope with political extraction layer. If distributed coalition: classification remains snare but benefits layer becomes more complex.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_beneficiary_ambiguity, empirical, 'Identity and motivation structure of extraction beneficiaries').

omega_variable(
    identity_locked_vs_constrained_exit_ambiguity,
    'For Kurdish populations, is the structural lock primarily identity-based (self-concept fused with territorial claim and national identity) or constrained (high material cost of exit)? Can exit occur within identity frame (stay Turkish while acknowledging Kurdish identity) or does it require identity transformation?',
    'Ethnographic and survey analysis of Turkish and Kurdish identity frames; study of individuals and communities that have shifted identity positions; examination of whether political solutions recognizing dual identity (Turkish-Kurdish) reduce perceived exit costs',
    'If primarily identity_locked: constraint persists even if material barriers removed (Snare remains). If primarily constrained: political settlement and material investment could reduce extraction (Tangled Rope becomes visible). Classification at biographical time shifts between Mountain (identity_locked) and Rope (constrained).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_vs_constrained_exit_ambiguity, conceptual, 'Whether exit barriers are structural or identity-based for Kurdish populations').

omega_variable(
    international_pressure_effectiveness,
    'Do international pressure mechanisms (EU conditionality, NATO alliance terms, sanctions threat, ICC investigation) effectively constrain Turkish state extraction behavior, or do they lack enforcement credibility given strategic importance of Turkey?',
    'Time series analysis of state behavior changes correlated with international pressure; comparison of pressure effectiveness on strategic vs non-strategic actors; documentation of enforcement consistency across similar cases',
    'If effective: international scaffold actors gain real leverage (Scaffold classification gains empirical support). If ineffective: international pressure becomes theatrical (Piton classification; theater_ratio increases). If selective: extractiveness becomes partially platform-dependent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_pressure_effectiveness, empirical, 'Effectiveness of international pressure mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(turkish_kurdish_regional_conflict, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tkrc_tr_t0, turkish_kurdish_regional_conflict, theater_ratio, 0, 0.38).
narrative_ontology:measurement(tkrc_tr_t10, turkish_kurdish_regional_conflict, theater_ratio, 10, 0.48).
narrative_ontology:measurement(tkrc_tr_t20, turkish_kurdish_regional_conflict, theater_ratio, 20, 0.55).
narrative_ontology:measurement(tkrc_tr_t30, turkish_kurdish_regional_conflict, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(tkrc_be_t0, turkish_kurdish_regional_conflict, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(tkrc_be_t10, turkish_kurdish_regional_conflict, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(tkrc_be_t20, turkish_kurdish_regional_conflict, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(tkrc_be_t30, turkish_kurdish_regional_conflict, base_extractiveness, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(turkish_kurdish_regional_conflict, enforcement_mechanism).
narrative_ontology:affects_constraint(turkish_kurdish_regional_conflict, middle_east_regional_instability).
narrative_ontology:affects_constraint(turkish_kurdish_regional_conflict, eu_turkey_relations).
narrative_ontology:affects_constraint(turkish_kurdish_regional_conflict, nato_alliance_cohesion).

% DUAL FORMULATION NOTE:
% The Turkish-Kurdish conflict decomposes into multiple structurally distinct constraints: (1) security counter-terrorism coordination (genuine state function), (2) ethnic-national political autonomy (structural incompatibility between centralized state and autonomous region), (3) human rights enforcement (institutional compliance gap), (4) economic marginalization (development constraint). Each has distinct ε value and beneficiary/victim structure. This story captures the highest-extractiveness reading (security apparatus extraction); separate stories should decompose the political autonomy and economic development constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(turkish_kurdish_regional_conflict, organized, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
