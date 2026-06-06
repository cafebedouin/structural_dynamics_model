% ============================================================================
% CONSTRAINT STORY: settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_settler_colonial_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: settler_colonial_reading
 *   human_readable: Zionism as Settler-Colonial Displacement Structure
 *   domain: political_history/nationalism/settler_colonialism
 *
 * SUMMARY:
 *   The settler-colonial reading of Zionism frames the movement as a European
 *   colonial project that established an ethno-state through systematic
 *   displacement of the indigenous Palestinian Arab population. This reading
 *   emphasizes structural parallels with other settler-colonial cases: land
 *   acquisition via legal mechanisms that delegitimize indigenous tenure,
 *   demographic transformation through sponsored immigration, and
 *   institutional structures that privilege settlers over indigenous
 *   populations. The constraint's extractiveness increased sharply at
 *   critical junctures (1917 Balfal Declaration, 1948 Nakba, 1967 occupation)
 *   and has remained high since. Theater ratio reflects the gap between
 *   stated coordination goals (refuge, development, security) and actual
 *   displacement mechanisms. Suppression is high and persistent: military
 *   occupation, settlement expansion, legal exclusion, and denial of return.
 *   This reading is one of three sibling readings of the contested
 *   zionist_legitimacy_basis kernel; the others frame the same historical
 *   process as national liberation or religious restoration.
 *
 * KEY AGENTS:
 *   - Palestinian Arab Population: Primary victim (powerless/trapped) — experienced land expropriation, legal exclusion, forced displacement, ongoing military control
 *   - Palestinian Citizens of Israel: Secondary victim (moderate/constrained) — experience legal discrimination and structural inequality but possess formal citizenship
 *   - Indigenous Land Tenure Systems: Structural victim (powerless/trapped) — Ottoman and customary land systems systematically delegitimized and replaced
 *   - Jewish Immigrant Population: Mixed beneficiary-victim (moderate/constrained) — benefits from land access and state protection but constrained by militarization and conflict
 *   - Zionist Institutional Apparatus: Primary beneficiary (institutional/arbitrage) — captures land, state formation, international support
 *   - Western Imperial Sponsors: Mixed beneficiary-victim (organized/constrained) — Britain and US experience strategic benefits and diplomatic costs
 *   - Analytical Observer: Settler-colonial framework (analytical/analytical) — sees displacement as constitutive mechanism, not incidental consequence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(settler_colonial_reading, 0.78).
domain_priors:suppression_score(settler_colonial_reading, 0.82).
domain_priors:theater_ratio(settler_colonial_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(settler_colonial_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(settler_colonial_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(settler_colonial_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(settler_colonial_reading, snare).
narrative_ontology:human_readable(settler_colonial_reading, "Zionism as Settler-Colonial Displacement Structure").
narrative_ontology:topic_domain(settler_colonial_reading, "political_history/nationalism/settler_colonialism").

domain_priors:requires_active_enforcement(settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(settler_colonial_reading, '2cf92639-1b65-4d64-93dd-32c58239d31e').
narrative_ontology:cs_kernel_codification('2cf92639-1b65-4d64-93dd-32c58239d31e', distributed).
narrative_ontology:cs_authority_grounding('2cf92639-1b65-4d64-93dd-32c58239d31e', distributed).
narrative_ontology:cs_reading_relation('2cf92639-1b65-4d64-93dd-32c58239d31e', settler_colonial_reading__national_liberation_reading, coexists_with).
narrative_ontology:cs_reading_relation('2cf92639-1b65-4d64-93dd-32c58239d31e', settler_colonial_reading__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('2cf92639-1b65-4d64-93dd-32c58239d31e', foundational, displacement_as_constitutive_mechanism).
narrative_ontology:cs_axiom_status(displacement_as_constitutive_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('2cf92639-1b65-4d64-93dd-32c58239d31e', displacement_as_constitutive_mechanism, empirically_contingent).
narrative_ontology:cs_axiom('2cf92639-1b65-4d64-93dd-32c58239d31e', foundational, colonial_structure_determines_legitimacy).
narrative_ontology:cs_axiom_status(colonial_structure_determines_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('2cf92639-1b65-4d64-93dd-32c58239d31e', colonial_structure_determines_legitimacy, deontological).
narrative_ontology:cs_reference_frame('2cf92639-1b65-4d64-93dd-32c58239d31e', pre_zionist_ottoman_land_regime).
narrative_ontology:cs_drift_state('2cf92639-1b65-4d64-93dd-32c58239d31e', contemporary_post_nakba_regime, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('2cf92639-1b65-4d64-93dd-32c58239d31e', '').
narrative_ontology:cs_kernel_id(settler_colonial_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(settler_colonial_reading, jewish_immigrant_population).
narrative_ontology:constraint_beneficiary(settler_colonial_reading, zionist_institutional_apparatus).
narrative_ontology:constraint_beneficiary(settler_colonial_reading, western_imperial_sponsors).
narrative_ontology:constraint_victim(settler_colonial_reading, palestinian_arab_population).
narrative_ontology:constraint_victim(settler_colonial_reading, indigenous_land_tenure_systems).
narrative_ontology:constraint_victim(settler_colonial_reading, regional_political_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED PALESTINIAN POPULATION (SNARE) — Trapped by military control, legal exclusion, and territorial fragmentation. Experiences maximum extraction: land expropriation, legal subordination, mobility restriction. No exit option — cannot return to expropriated land, cannot achieve political sovereignty within the constraint's structure. The coordination story (security, development, modernization) is cover for systematic displacement.
constraint_indexing:constraint_classification(settler_colonial_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PALESTINIAN CITIZENS OF ISRAEL (TANGLED ROPE) — Constrained by legal discrimination and structural inequality but possess formal citizenship and some institutional access. Experience both coordination (civic infrastructure, economic integration) and extraction (land confiscation via absentee property laws, unequal resource allocation, exclusion from national narrative). Can participate in some institutions but cannot exit the ethno-national hierarchy.
constraint_indexing:constraint_classification(settler_colonial_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ZIONIST INSTITUTIONAL APPARATUS (ROPE) — Benefits from land acquisition, state formation, and international support. Experiences the constraint as coordination: building national institutions, absorbing immigration, establishing sovereignty. Net beneficiary — extraction flows toward this agent. Arbitrage exit: can leverage diaspora resources, international alliances, and institutional mobility.
constraint_indexing:constraint_classification(settler_colonial_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: JEWISH IMMIGRANT POPULATION (TANGLED ROPE) — Benefits from land access, citizenship, and state protection but also constrained by militarization, ongoing conflict, and economic dependency on state apparatus. Experiences coordination (refugee absorption, community building) and extraction (mandatory military service, settlement incentives, perpetual security mobilization). Constrained exit: can emigrate but faces identity costs and loss of state benefits.
constraint_indexing:constraint_classification(settler_colonial_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: INDIGENOUS LAND TENURE SYSTEMS (SNARE) — Trapped by legal erasure and territorial transformation. Ottoman and customary land systems were systematically delegitimized and replaced. No exit: the legal-institutional framework that recognized communal and unregistered land claims was abolished. Maximum extraction: entire systems of property, inheritance, and communal resource management were eliminated.
constraint_indexing:constraint_classification(settler_colonial_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 6: WESTERN IMPERIAL SPONSORS (TANGLED ROPE) — Britain, later the US, experience both coordination (regional ally, strategic foothold, cultural affinity) and extraction (entanglement in regional conflict, diplomatic costs, resource commitment). Constrained exit: can reduce support but face domestic political costs and alliance commitments. Organized power but not full arbitrage.
constraint_indexing:constraint_classification(settler_colonial_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From the settler-colonial analytical framework, this constraint is a snare: the coordination story (national liberation, refuge, development) is cover for a displacement structure. The constraint's persistence depends on suppressing alternatives (binational state, equal citizenship, return) and on active enforcement (military occupation, settlement expansion, legal exclusion). High extraction, high suppression, identifiable victims.
constraint_indexing:constraint_classification(settler_colonial_reading, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(settler_colonial_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(settler_colonial_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(settler_colonial_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High. The constraint extracts land, sovereignty, and mobility from the Palestinian population while concentrating these resources in the Jewish population and Zionist institutions. The extraction is not total (some Palestinians retain land, citizenship, or residual rights) but is severe and persistent. Suppression (0.82): Very high. Military occupation, settlement expansion, legal exclusion, mobility restrictions, and denial of return create a comprehensive suppression regime. Alternatives (binational state, equal citizenship, return) are actively suppressed. Theater ratio (0.68): High. The gap between stated goals (security, development, refuge) and actual mechanisms (displacement, territorial control, demographic engineering) is substantial. Peace processes and development rhetoric mask ongoing settlement expansion and legal exclusion.
 *
 * PERSPECTIVAL GAP:
 *   The settler-colonial reading produces a snare classification from the analytical perspective: high extraction, high suppression, identifiable victims, coordination story as cover. The displaced Palestinian population experiences pure snare. The Zionist institutional apparatus experiences rope — they are the net beneficiaries. The Jewish immigrant population experiences tangled rope — benefits and costs intertwined. The perspectival gap is extreme: what appears as national liberation and refuge from one position appears as systematic displacement from another. The gap is not resolvable within a single framework — it requires adjudicating between competing readings of the kernel (settler-colonial vs national liberation vs religious restoration).
 *
 * DIRECTIONALITY LOGIC:
 *   The Palestinian Arab population is the primary victim with trapped exit options — they experience maximum extraction. The Zionist institutional apparatus is the primary beneficiary with arbitrage exit — they experience the constraint as coordination. The Jewish immigrant population is a mixed case: benefits from land access and state protection (beneficiary) but constrained by militarization and conflict (victim). The settler-colonial reading emphasizes that the immigrant population's benefits are structurally dependent on Palestinian displacement — the coordination and extraction are inseparable. Western imperial sponsors experience both coordination (strategic ally) and extraction (entanglement costs). Palestinian citizens of Israel experience tangled rope: formal inclusion with structural subordination.
 *
 * MANDATROPHY ANALYSIS:
 *   The settler-colonial reading resolves mandatrophy by identifying displacement as the constraint's constitutive function rather than an incidental side effect. The mandate (establishing a Jewish national home) has not outlived its function — the function IS ongoing territorial transformation and demographic engineering. The constraint is not a degraded coordination mechanism (piton) but an active extraction structure (snare). However, this resolution is reading-dependent: the national liberation reading would frame the same structure as a scaffold (temporary security measures) or tangled rope (tragic but necessary costs of self-determination). The mandatrophy question cannot be resolved without first resolving the kernel reading ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a settler-colonial displacement structure (this reading), a national liberation movement (sibling reading), or a religious restoration project (sibling reading)?',
    'The readings differ on what structural element determines legitimacy. Settler-colonial reading: displacement is constitutive, not incidental — the constraint''s function IS territorial transformation. National liberation reading: displacement is tragic side effect of legitimate self-determination. Religious restoration reading: displacement is secondary to fulfillment of covenantal claim. Resolution requires adjudicating which structural element (colonial mechanism, national sovereignty, religious mandate) is primary.',
    'If settler-colonial: constraint is snare (extraction via displacement). If national liberation: constraint is tangled rope or scaffold (coordination with tragic costs). If religious restoration: constraint is mountain or rope (fulfillment of transcendent mandate).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Which reading of the zionist_legitimacy_basis kernel is structurally primary').

omega_variable(
    displacement_necessity_threshold,
    'At what threshold does population displacement shift from incidental consequence to constitutive mechanism?',
    'Comparative analysis of settler-colonial cases: proportion of indigenous population displaced, legal mechanisms of land transfer, institutional structures of exclusion, temporal persistence of displacement. Threshold identification via pattern matching across cases (Australia, Algeria, South Africa, North America).',
    'If threshold low (e.g., >20% displacement + legal exclusion): many national movements reclassify as settler-colonial. If threshold high (e.g., >80% displacement + explicit elimination): only extreme cases qualify, and this constraint may fall below threshold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_necessity_threshold, empirical, 'Empirical threshold for constitutive vs incidental displacement').

omega_variable(
    return_feasibility_counterfactual,
    'Would a right of return for displaced Palestinians dissolve the constraint or merely redistribute extraction?',
    'Counterfactual institutional analysis: demographic modeling of return scenarios, legal analysis of property restitution mechanisms, comparative cases of post-conflict return (Bosnia, Rwanda, post-apartheid South Africa). Does return eliminate the ethno-national hierarchy or create new extraction patterns?',
    'If return dissolves constraint: the displacement structure is reversible, supporting scaffold or tangled rope classification from some perspectives. If return redistributes extraction: the constraint is deeper than displacement alone, supporting snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(return_feasibility_counterfactual, empirical, 'Whether return would dissolve or redistribute the extraction structure').

omega_variable(
    imperial_sponsorship_necessity,
    'Is Western imperial sponsorship constitutive of the constraint or merely enabling?',
    'Counterfactual analysis: could the Zionist project have succeeded without British Mandate facilitation and US support? Comparative analysis of national movements with and without great power sponsorship. Identification of critical junctures where sponsorship was decisive (1917 Balfour Declaration, 1947 UN partition, 1967 war, ongoing military aid).',
    'If constitutive: constraint is tangled rope from imperial perspective (coordination + extraction). If merely enabling: constraint is rope from imperial perspective (voluntary coordination). Affects network topology: if constitutive, imperial sponsorship is a structural dependency; if enabling, it is a contingent historical factor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imperial_sponsorship_necessity, empirical, 'Whether imperial sponsorship is structurally necessary or historically contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(settler_colonial_reading, 0, 128).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(settler_col_theater_1897, settler_colonial_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(settler_col_theater_1917, settler_colonial_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(settler_col_theater_1948, settler_colonial_reading, theater_ratio, 51, 0.55).
narrative_ontology:measurement(settler_col_theater_1967, settler_colonial_reading, theater_ratio, 70, 0.62).
narrative_ontology:measurement(settler_col_theater_1993, settler_colonial_reading, theater_ratio, 96, 0.68).
narrative_ontology:measurement(settler_col_theater_2025, settler_colonial_reading, theater_ratio, 128, 0.68).

% Extraction over time
narrative_ontology:measurement(settler_col_extract_1897, settler_colonial_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(settler_col_extract_1917, settler_colonial_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(settler_col_extract_1948, settler_colonial_reading, base_extractiveness, 51, 0.72).
narrative_ontology:measurement(settler_col_extract_1967, settler_colonial_reading, base_extractiveness, 70, 0.76).
narrative_ontology:measurement(settler_col_extract_1993, settler_colonial_reading, base_extractiveness, 96, 0.78).
narrative_ontology:measurement(settler_col_extract_2025, settler_colonial_reading, base_extractiveness, 128, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(settler_col_suppress_1897, settler_colonial_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(settler_col_suppress_1917, settler_colonial_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(settler_col_suppress_1948, settler_colonial_reading, suppression_requirement, 51, 0.78).
narrative_ontology:measurement(settler_col_suppress_1967, settler_colonial_reading, suppression_requirement, 70, 0.85).
narrative_ontology:measurement(settler_col_suppress_1993, settler_colonial_reading, suppression_requirement, 96, 0.82).
narrative_ontology:measurement(settler_col_suppress_2025, settler_colonial_reading, suppression_requirement, 128, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(settler_colonial_reading, identity_coordination).
narrative_ontology:affects_constraint(settler_colonial_reading, national_liberation_reading).
narrative_ontology:affects_constraint(settler_colonial_reading, religious_restoration_reading).

% DUAL FORMULATION NOTE:
% The settler_colonial_reading is one of three readings of the zionist_legitimacy_basis kernel. Each reading instantiates a structurally distinct constraint with different ε values, beneficiary/victim structures, and classification types. The readings are linked via network.affects_constraints because they compete for legitimacy in overlapping institutional and discursive spaces. The settler-colonial reading's high extraction and suppression values reflect the displacement mechanism this reading takes as primary; the national liberation reading's lower extraction reflects its framing of displacement as incidental; the religious restoration reading's mountain-like immutability reflects its transcendent grounding.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
