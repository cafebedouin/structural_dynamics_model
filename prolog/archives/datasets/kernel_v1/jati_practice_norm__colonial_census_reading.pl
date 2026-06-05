% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__colonial_census_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__colonial_census_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jati_practice_norm__colonial_census_reading
 *   human_readable: Jati Categories Reified via Colonial Census Administration
 *   domain: social_anthropology/colonial_governance/religious_studies
 *
 * SUMMARY:
 *   The colonial census transformed jati from a locally-negotiated, fluid
 *   category system into a fixed, enumerated, legally-codified apparatus for
 *   administrative control. Pre-colonial jati categories existed as ritual
 *   status frameworks, occupational associations, and endogamous units, but
 *   with considerable local variation and individual mobility across
 *   boundaries through occupational change, inter-caste marriage, and ritual
 *   innovation. The British Census of India (initiated 1871) froze these
 *   categories into discrete, heritable, mutually-exclusive units assigned to
 *   individuals at birth and linked to legal rights, occupational
 *   restrictions, and tax obligations. This reading privileges the census
 *   apparatus as the reifying mechanism — it is the colonial administrative
 *   apparatus that creates the constraint, not the pre-existing jati practice
 *   itself. The constraint exhibits genuine tangled rope structure: the
 *   apparatus provides coordination benefits (administrative legibility,
 *   legal status, collective recognition in colonial bureaucracy) while
 *   extracting through category fixity (occupational foreclosure, marriage
 *   restriction, mobility suppression, autonomy loss). The extractiveness
 *   profile shows dramatic increase across the colonial period (0.28→0.52) as
 *   census machinery matured, then partial decline post-independence (0.48)
 *   as the post-colonial state both dismantled formal enforcement and
 *   repurposed categories for affirmative action. Theater ratio increases
 *   sharply as enforcement becomes more abstract and performative — the
 *   colonial apparatus required active enumeration and occupational tracking;
 *   the post-colonial apparatus maintains categories through constitutional
 *   law and administrative convenience without the same direct enforcement
 *   labor.
 *
 * KEY AGENTS:
 *   - Colonial Census Bureaucracy (institutional/arbitrage): Primary beneficiary — achieves administrative efficiency by freezing categories into discrete enumerable units; benefits from reduced governance complexity
 *   - British Colonial Administration (institutional/arbitrage): Primary beneficiary — uses census categories to extract revenue, organize occupational labor control, and implement divide-and-rule strategies
 *   - Reified Jati Subjects (powerless/trapped): Primary victims — individuals born into census-assigned categories with no exit; occupational mobility and marriage autonomy foreclosed
 *   - Local Jati Communities (moderate/constrained): Secondary beneficiary and victim simultaneously — gain legal collective recognition but lose category fluidity and internal authority
 *   - Anti-Colonial Independence Movements (organized/constrained): Tertiary actor — recognize census apparatus as colonial imposition, mobilize around category dissolution, but find post-colonial states continue the apparatus
 *   - Post-Colonial State Bureaucracies (institutional/arbitrage): Re-beneficiary — inherit and maintain census categories for administrative convenience and affirmative action implementation
 *   - Affirmative Action Beneficiaries (moderate/constrained): Tertiary beneficiary and victim — gain compensatory allocations based on census categories but remain trapped in the reified categories that justify allocation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__colonial_census_reading, 0.52).
domain_priors:suppression_score(jati_practice_norm__colonial_census_reading, 0.68).
domain_priors:theater_ratio(jati_practice_norm__colonial_census_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__colonial_census_reading, tangled_rope).
narrative_ontology:human_readable(jati_practice_norm__colonial_census_reading, "Jati Categories Reified via Colonial Census Administration").
narrative_ontology:topic_domain(jati_practice_norm__colonial_census_reading, "social_anthropology/colonial_governance/religious_studies").

domain_priors:requires_active_enforcement(jati_practice_norm__colonial_census_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__colonial_census_reading, '5a500516-d859-456f-a7de-ceb8da6b3728').
narrative_ontology:cs_kernel_codification('5a500516-d859-456f-a7de-ceb8da6b3728', fixed_text).
narrative_ontology:cs_authority_grounding('5a500516-d859-456f-a7de-ceb8da6b3728', extraction).
narrative_ontology:cs_interpretation_layer_present('5a500516-d859-456f-a7de-ceb8da6b3728').
narrative_ontology:cs_reading_relation('5a500516-d859-456f-a7de-ceb8da6b3728', jati_practice_norm__orthodox_textual_reading, influences).
narrative_ontology:cs_reading_relation('5a500516-d859-456f-a7de-ceb8da6b3728', jati_practice_norm__localized_practice_reading, forecloses).
narrative_ontology:cs_axiom('5a500516-d859-456f-a7de-ceb8da6b3728', foundational, administrative_enumeration_creates_legal_status).
narrative_ontology:cs_axiom_status(administrative_enumeration_creates_legal_status, holdable).
narrative_ontology:cs_axiom_grounding('5a500516-d859-456f-a7de-ceb8da6b3728', administrative_enumeration_creates_legal_status, conventional).
narrative_ontology:cs_axiom('5a500516-d859-456f-a7de-ceb8da6b3728', foundational, fixity_enables_differential_governance).
narrative_ontology:cs_axiom_status(fixity_enables_differential_governance, holdable).
narrative_ontology:cs_axiom_grounding('5a500516-d859-456f-a7de-ceb8da6b3728', fixity_enables_differential_governance, instrumental).
narrative_ontology:cs_reference_frame('5a500516-d859-456f-a7de-ceb8da6b3728', enumerated_discrete_categories_administratively_fixed).
narrative_ontology:cs_drift_state('5a500516-d859-456f-a7de-ceb8da6b3728', post_colonial_constitutional_era, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('5a500516-d859-456f-a7de-ceb8da6b3728', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__colonial_census_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, colonial_administrative_apparatus).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, census_bureaucracy).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, community_category_autonomy).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, caste_mobility_practices).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, local_jati_fluidity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REIFIED JATI SUBJECT (SNARE) — Individual born into a census-fixed jati category has no exit: legal status, occupation restrictions, marriage rules, ritual obligations all crystallized by colonial record. The trap is total — administrative fixity prevents the fluidity that pre-colonial practice permitted. Maximum extraction experienced through occupational foreclosure and social mobility suppression.
constraint_indexing:constraint_classification(jati_practice_norm__colonial_census_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: LOCAL JATI COMMUNITY (TANGLED ROPE) — Benefits from census codification that grants collective legal recognition, access to colonial administrative resources, and defined group identity in the colonial legal system. Simultaneously constrained by enforced rigidity that eliminates previous practices of occupational switching, inter-jati marriage, and fluid category boundaries. Mixed extraction: governance access at cost of autonomy.
constraint_indexing:constraint_classification(jati_practice_norm__colonial_census_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: COLONIAL CENSUS ADMINISTRATION (ROPE) — Pure coordination function from administrative view: census categories create taxable units, occupational registries, and governable populations. Reduces complexity of multi-layered community organization into discrete, countable categories. Experiences the constraint as efficient sorting mechanism enabling resource allocation and political control. Net beneficiary through administrative simplification.
constraint_indexing:constraint_classification(jati_practice_norm__colonial_census_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: ANTI-COLONIAL INDEPENDENCE MOVEMENTS (SCAFFOLD) — See the census reification as a temporary imposed apparatus with a natural sunset: independence dismantles the colonial administrative machinery, restores community self-categorization, and permits return to local governance autonomy. Theater remains high (constitutional provisions, legal codes still reference census categories) but coalition has exit path and agents of change. Sunset clause: formal end of colonial rule theoretically ends the enforcement mechanism, though path-dependency persists.
constraint_indexing:constraint_classification(jati_practice_norm__colonial_census_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: POST-COLONIAL STATE ADMINISTRATION (PITON) — Independent nation-states continue to use census jati categories for administrative convenience despite rhetoric of category fluidity and constitutional equality. The apparatus persists through inertia — the categories are now maintained by post-colonial bureaucracies that inherited colonial infrastructure. Theater is high (constitutional silence on caste, progressive rhetoric) but functional reification continues. The constraint degrades into performative equality while material reification persists.
constraint_indexing:constraint_classification(jati_practice_norm__colonial_census_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: POST-COLONIAL STATE / AFFIRMATIVE ACTION READING (TANGLED ROPE) — The census categories, now reframed as historical disadvantage markers, become basis for compensatory justice schemes (reservations, targeted development). State benefits from administrative efficiency of fixed categories; subordinated groups benefit from affirmative action allocations but remain trapped in the reified categories that justify the allocation. Extraction is inverted but category fixity persists as enforcement mechanism.
constraint_indexing:constraint_classification(jati_practice_norm__colonial_census_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From universalizing human science perspective, caste/jati is an irreducible feature of South Asian social structure: endogamy, occupational heredity, and ritual status hierarchies are treated as inevitable cultural constants rather than contingent administrative artifacts. This perspective risks naturalizing what is actually a reified categorical system. The engine's false summit detector will identify this as naturalization of colonial administrative choice.
constraint_indexing:constraint_classification(jati_practice_norm__colonial_census_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__colonial_census_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(jati_practice_norm__colonial_census_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(jati_practice_norm__colonial_census_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(jati_practice_norm__colonial_census_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(jati_practice_norm__colonial_census_reading, TR),
    TR >= 0.70.

:- end_tests(jati_practice_norm__colonial_census_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base Extractiveness (0.52): Moderate-high. The census apparatus creates genuine coordination benefits (collective legal status, definable resource allocation groups, bureaucratic simplicity) alongside significant extraction (occupational foreclosure, marriage restriction, category immobility). The 0.52 value reflects that this is a mixed constraint: the apparatus solves a real coordination problem (how does a centralizing state govern a heterogeneous population?) while extracting through category fixity. If this were pure coordination without extraction, extractiveness would be ~0.10-0.15 (Rope). If this were pure extraction without coordination, extractiveness would be ~0.72+ (Snare). At 0.52, the Tangled Rope classification is structurally justified: both genuine coordination function and asymmetric extraction are present. Suppression (0.68): High. The census apparatus actively suppresses alternatives: local category negotiation becomes legally irrelevant; occupational mobility becomes legally impossible; marriage outside assigned jati becomes legally restricted. The suppression mechanism is the state monopoly on legal status — you cannot simply declare yourself a different jati; only state authority can reclassify. Suppression is not absolute (social practice sometimes deviates from legal category) but is substantial. Theater Ratio (0.58): Moderate-high and rising. Initially (t0=0.22) the census apparatus has high functional content — actual occupational recording, labor tracking, revenue assignment. By maturity (t30=0.58, t150=0.71) the apparatus becomes increasingly performative: constitutional rhetoric of equality clashes with administrative reliance on categories; categories persist despite stated opposition to caste; the machinery continues through inertia rather than active enforcement. The rising theater ratio is diagnostic: the constraint is degrading from functional extraction to inertial reproduction (piton-like trajectory in late period, though not yet piton-classified because some beneficiary still actively maintains the system).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. The Colonial Census Administration (institutional/arbitrage) sees a pure coordination mechanism: categories enable efficient governing. The Reified Jati Subject (powerless/trapped) sees a pure extraction mechanism: the categories foreclose all alternatives. The Local Jati Community (moderate/constrained) sees mixed experience: legal recognition enables community status but rigidity eliminates negotiation. The Post-Colonial State (institutional/arbitrage) sees an inherited apparatus to maintain for convenience, degrading into theater. The Anti-Colonial Movement (organized/constrained) sees a sunset apparatus dissolving with independence. The Analytical Observer (analytical/analytical) risks seeing jati itself as a natural feature of South Asian social structure (mountain perspective). This gap reveals that the classification outcome is entirely determined by the observer's structural position: there is no single true answer, only correct answers relative to each agent's power, exit options, and temporal horizon. The false summit detector will flag the analytical mountain perspective as naturalization of a contingent administrative choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent is derived from beneficiary/victim status and exit options. Colonial census bureaucracy (beneficiary + arbitrage) derives low d ≈ 0.15, producing negative f(d) ≈ -0.01 — they experience no extraction, only benefit. Reified jati subjects (victim + trapped) derive high d ≈ 0.95, producing f(d) ≈ 1.42 — maximum experienced extraction. Local jati communities (both beneficiary through collective recognition and victim through category loss + constrained exit) derive moderate d ≈ 0.55, producing f(d) ≈ 0.75 — mixed experience. Post-colonial state (beneficiary but constrained by constitutional rhetoric + arbitrage capacity) derives d ≈ 0.25-0.35 — reduced beneficiary status relative to colonial administration due to legitimacy constraints. The directionality logic shows why different perspectives classify the same constraint differently: same base extractiveness (0.52) but different experienced χ because different d values feed into the sigmoid. Beneficiary perspectives see lower effective extraction (rope); victim perspectives see higher effective extraction (snare); moderate beneficiary-victim perspectives see mixed experience (tangled rope).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by establishing that Tangled Rope is the correct classification at the PRIMARY analytical level (institutional/generational/constrained/continental perspective — the perspective that sees both the coordination function and the extraction mechanism in their genuine tension). The other classifications (Snare from the powerless perspective, Rope from the beneficiary perspective, Scaffold from the organized perspective) are legitimate but secondary — they are what different agents experience, but they are not the structural classification of the apparatus itself. The apparatus IS tangled: it coordinates administrative capability while extracting through category fixity. The Piton perspective (post-colonial state degrading the machinery) and the Mountain perspective (naturalizing the constraint as eternal jati structure) are both misclassifications from the reading's own internal logic: the Piton perspective misses that the post-colonial state actively maintains the apparatus for affirmative action purposes (not purely inertial); the Mountain perspective misses that this is an administered reification, not a natural social law. By establishing that Tangled Rope is the reading's central classification, mandatrophy is avoided: the constraint is not trying to be both pure extraction and pure coordination; it is genuinely both, with the tension being the essence of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pre_colonial_jati_fluidity_counterfactual,
    'To what extent was pre-colonial jati practice actually fluid in occupational membership, ritual status, and category boundaries? Or does the ''fluidity'' narrative itself rely on fragmentary colonial sources and post-colonial romantic reconstruction?',
    'Comparative analysis of pre-1750 local records (temple inscriptions, revenue documents, family genealogies) vs colonial census records; examination of intra-jati occupational variation and inter-jati boundary crossing in regions with dense administrative documentation',
    'If pre-colonial rigidity was comparable to colonial: the census reading''s extractiveness drops (0.52→0.35); constraint reclassifies as Rope across more perspectives. If fluidity was genuine: extractiveness estimate is validated; the colonial intervention represents genuine structural change from mobility to fixity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pre_colonial_jati_fluidity_counterfactual, empirical, 'Degree of pre-colonial jati category fluidity').

omega_variable(
    census_category_fixity_mechanism,
    'Did the census categories themselves create fixity, or did they merely codify and weaponize existing local hierarchies that were already semi-rigid? What portion of post-census rigidity is attributable to the administrative apparatus vs. pre-existing community structures?',
    'Time-series analysis of occupational mobility rates, inter-jati marriage rates, and category-switching practices in regions with high vs. low census enumeration infrastructure; control for independent variables (economic change, urbanization) vs. administrative apparatus alone',
    'If apparatus-driven: extractiveness is structural (0.52 holds); apparatus removal permits return to fluidity. If apparatus merely codified existing structures: post-colonial persistence reflects path-dependency rather than path-creation; piton classification is more accurate than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(census_category_fixity_mechanism, empirical, 'Whether census apparatus created or merely codified category fixity').

omega_variable(
    post_colonial_category_persistence_mechanism,
    'Why have independent nation-states continued to use census jati categories for administration (affirmative action, electoral reservations, development schemes) despite anti-caste constitutional rhetoric? Is this path-dependency, bureaucratic inertia, or deliberate re-extraction using the colonial apparatus for post-colonial governance?',
    'Archival analysis of early post-colonial state constitutional debates and policy choices; comparison of states that attempted category abolition vs. those that retained categories; longitudinal measurement of state investment in maintaining vs. dismantling census infrastructure',
    'If path-dependent: piton classification is accurate — degraded apparatus maintained by inertia. If deliberate re-extraction: the reading actually describes multiple constraints layered over time (colonial tangled_rope → post-colonial snare/piton hybrid). If bureaucratic convenience: post-colonial state occupies beneficiary role similar to colonial apparatus.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(post_colonial_category_persistence_mechanism, empirical, 'Mechanism driving post-colonial persistence of census categories').

omega_variable(
    affirmative_action_extraction_paradox,
    'Does affirmative action based on census jati categories represent genuine compensatory justice for historical extraction, or does it perpetuate the reified categories that enable extraction? Does category fixity serve both the historical oppressor and the post-colonial beneficiary differently?',
    'Comparative analysis of mobility trajectories and category boundary crossing before/after affirmative action implementation; assessment of whether reservations reduce or entrench category-based identity and boundary maintenance; longitudinal measurement of whether beneficiary groups seek category dissolution or category expansion',
    'If justice-enabling: the constraint is reclassified from tangled_rope to rope from beneficiary perspective (affirmative action perspective). If category-perpetuating: extractiveness increases for powerless agents locked in categories by affirmative action system; piton classification becomes dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(affirmative_action_extraction_paradox, preference, 'Whether affirmative action resolves or perpetuates category reification extraction').

omega_variable(
    reading_vs_material_jati,
    'This reading privileges the colonial census as the reifying apparatus. But is the real distinction between the orthodox textual reading (Brahminical scriptural jati), the localized practice reading (community-specific occupational/ritual roles), and this colonial census reading actually located in the apparatus, or in whose definitions and classifications are being codified and enforced?',
    'Comparative analysis of what each reading actually reifies: scriptures fix ritual status and occupation in text; local practice fixes them in community enforcement; census fixes them in law and administration. Which medium is most binding? Which is easiest to change? Which produces greatest behavioral conformity?',
    'If apparatus is the critical variable: this reading correctly identifies colonial census as the distinctive reifying force (extractiveness ~0.52). If medium of codification is the variable: the three readings have more similar extractiveness and differ primarily in beneficiary/victim configuration rather than structural binding force. If enforcement capacity is the variable: post-colonial state enforcement may exceed colonial enforcement despite identical categories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_material_jati, conceptual, 'What aspect of the reading actually creates reification: apparatus, codification medium, or enforcement capacity?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__colonial_census_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_census_theater_t0, jati_practice_norm__colonial_census_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(jati_census_theater_t30, jati_practice_norm__colonial_census_reading, theater_ratio, 30, 0.58).
narrative_ontology:measurement(jati_census_theater_t150, jati_practice_norm__colonial_census_reading, theater_ratio, 150, 0.71).

% Extraction over time
narrative_ontology:measurement(jati_census_extract_t0, jati_practice_norm__colonial_census_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(jati_census_extract_t30, jati_practice_norm__colonial_census_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(jati_census_extract_t150, jati_practice_norm__colonial_census_reading, base_extractiveness, 150, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(jati_census_suppress_t0, jati_practice_norm__colonial_census_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(jati_census_suppress_t30, jati_practice_norm__colonial_census_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(jati_census_suppress_t150, jati_practice_norm__colonial_census_reading, suppression_requirement, 150, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__colonial_census_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jati_practice_norm__colonial_census_reading, 0.12).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, jati_practice_norm__orthodox_textual_reading).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, jati_practice_norm__localized_practice_reading).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, caste_based_affirmative_action).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, post_colonial_category_persistence).

% DUAL FORMULATION NOTE:
% The jati_practice_norm kernel instantiates three distinct constraints depending on which reading is adopted: orthodox textual (scripture-based reification, mountain-classified), localized practice (community-negotiated, rope-classified), and colonial census (apparatus-reified, tangled rope-classified). These are NOT different observations of one constraint; they are different constraints with different ε values and different beneficiary/victim structures. This reading (colonial_census_reading) is downstream of the localized_practice_reading (which it claims to supersede) and feeds into caste_based_affirmative_action (which repurposes the census categories for post-colonial justice). The constraint family requires three separate JSON files plus the downstream constraints that depend on census categories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jati_practice_norm__colonial_census_reading, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
