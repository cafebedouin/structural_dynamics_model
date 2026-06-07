% ============================================================================
% CONSTRAINT STORY: demographic_engineering_imperative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-06-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_demographic_engineering_imperative, []).

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
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: demographic_engineering_imperative
 *   human_readable: Demographic Engineering Imperative in Zionist State-Building
 *   domain: political_history/nationalism/settler_colonialism
 *
 * SUMMARY:
 *   The demographic engineering imperative emerged from the Zionist
 *   movement's foundational commitment to establishing a Jewish-majority
 *   state in Palestine, where Jews constituted roughly 30% of the population
 *   in 1947. This imperative structured immigration policy, land acquisition
 *   strategy, military planning, and ultimately the displacement of
 *   approximately 750,000 Palestinian Arabs in 1948. The constraint exhibits
 *   tangled_rope structure: it coordinates genuine refugee absorption (Jewish
 *   survivors of European persecution, Middle Eastern Jewish refugees) while
 *   simultaneously extracting from the Arab population through displacement,
 *   denial of return, and legal subordination. The imperative's persistence
 *   depends on active enforcement — military control, permit regimes,
 *   settlement expansion, and legal barriers to Arab demographic growth.
 *   Theater ratio (0.42) reflects substantial but not dominant performative
 *   content: security justifications are partly genuine (threat perception is
 *   real) and partly cover (demographic dominance pursued beyond security
 *   requirements). The constraint is a paradigmatic case of how a
 *   coordination mechanism (state-building, refugee absorption) can be
 *   structurally inseparable from an extraction mechanism (indigenous
 *   displacement, permanent subordination).
 *
 * KEY AGENTS:
 *   - Palestinian Arab Population: Primary victim (powerless/trapped) — bears maximum extraction through displacement, statelessness, and denial of return; no exit options
 *   - Zionist Movement Leadership: Primary beneficiary (institutional/arbitrage) — coordinates state-building project; experiences imperative as legitimate security requirement
 *   - Jewish Immigrant Population: Secondary beneficiary (moderate/constrained) — benefits from absorption infrastructure but instrumentalized as demographic instruments
 *   - State Security Apparatus: Institutional actor (institutional/constrained) — benefits from expanded mandate but embedded in perpetual conflict structure
 *   - Refugee Communities: Secondary victim (powerless/trapped) — descendants of displaced persons, stateless and denied return across multiple generations
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees both genuine coordination and substantial extraction as structurally inseparable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(demographic_engineering_imperative, 0.78).
domain_priors:suppression_score(demographic_engineering_imperative, 0.85).
domain_priors:theater_ratio(demographic_engineering_imperative, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(demographic_engineering_imperative, extractiveness, 0.78).
narrative_ontology:constraint_metric(demographic_engineering_imperative, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(demographic_engineering_imperative, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(demographic_engineering_imperative, tangled_rope).
narrative_ontology:human_readable(demographic_engineering_imperative, "Demographic Engineering Imperative in Zionist State-Building").
narrative_ontology:topic_domain(demographic_engineering_imperative, "political_history/nationalism/settler_colonialism").

domain_priors:requires_active_enforcement(demographic_engineering_imperative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(demographic_engineering_imperative, 'c20d255b-f8ab-40af-ba3b-95653b1fe67a').
narrative_ontology:cs_kernel_codification('c20d255b-f8ab-40af-ba3b-95653b1fe67a', distributed).
narrative_ontology:cs_authority_grounding('c20d255b-f8ab-40af-ba3b-95653b1fe67a', lineage).
narrative_ontology:cs_interpretation_layer_present('c20d255b-f8ab-40af-ba3b-95653b1fe67a').
narrative_ontology:cs_reading_relation('c20d255b-f8ab-40af-ba3b-95653b1fe67a', demographic_engineering_imperative__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('c20d255b-f8ab-40af-ba3b-95653b1fe67a', demographic_engineering_imperative__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('c20d255b-f8ab-40af-ba3b-95653b1fe67a', foundational, persecution_driven_return_necessity).
narrative_ontology:cs_axiom_status(persecution_driven_return_necessity, holdable).
narrative_ontology:cs_axiom_grounding('c20d255b-f8ab-40af-ba3b-95653b1fe67a', persecution_driven_return_necessity, empirically_contingent).
narrative_ontology:cs_axiom('c20d255b-f8ab-40af-ba3b-95653b1fe67a', foundational, indigenous_continuity_claim).
narrative_ontology:cs_axiom_status(indigenous_continuity_claim, holdable).
narrative_ontology:cs_axiom_grounding('c20d255b-f8ab-40af-ba3b-95653b1fe67a', indigenous_continuity_claim, conventional).
narrative_ontology:cs_axiom('c20d255b-f8ab-40af-ba3b-95653b1fe67a', secondary, self_determination_primacy).
narrative_ontology:cs_axiom_status(self_determination_primacy, holdable).
narrative_ontology:cs_axiom_grounding('c20d255b-f8ab-40af-ba3b-95653b1fe67a', self_determination_primacy, deontological).
narrative_ontology:cs_reference_frame('c20d255b-f8ab-40af-ba3b-95653b1fe67a', diaspora_persecution_necessity).
narrative_ontology:cs_drift_state('c20d255b-f8ab-40af-ba3b-95653b1fe67a', post_1967_territorial_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c20d255b-f8ab-40af-ba3b-95653b1fe67a', '2026-06-06T03:31:32.502268+00:00').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(demographic_engineering_imperative, zionist_movement_leadership).
narrative_ontology:constraint_beneficiary(demographic_engineering_imperative, jewish_immigrant_population).
narrative_ontology:constraint_beneficiary(demographic_engineering_imperative, state_security_apparatus).
narrative_ontology:constraint_victim(demographic_engineering_imperative, palestinian_arab_population).
narrative_ontology:constraint_victim(demographic_engineering_imperative, internally_displaced_persons).
narrative_ontology:constraint_victim(demographic_engineering_imperative, refugee_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PALESTINIAN ARAB POPULATION (SNARE) — Trapped by military control, legal restrictions on movement and return, and statelessness. Bears maximum extraction: land expropriation, forced displacement, denial of citizenship rights, and systematic demographic subordination. No exit options — cannot return to pre-1948 villages, cannot achieve majority status within the state framework, cannot exit the territorial control regime. The coordination story (security necessity, population exchange precedent) is experienced as pure cover for extraction.
constraint_indexing:constraint_classification(demographic_engineering_imperative, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: JEWISH IMMIGRANT POPULATION (TANGLED ROPE) — Constrained by the imperative's dual nature: genuine beneficiaries of state-sponsored immigration, land allocation, and citizenship rights, but also instrumentalized as demographic instruments in an ongoing territorial contest. Benefits from coordination (absorption infrastructure, housing, employment) but also bears costs (settlement in contested areas, security burden, moral complicity in displacement). Exit options exist but are costly — emigration means abandoning state support and community ties.
constraint_indexing:constraint_classification(demographic_engineering_imperative, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ZIONIST MOVEMENT LEADERSHIP (ROPE) — Primary beneficiary with arbitrage-level exit options. Experiences the demographic imperative as legitimate coordination: solving the genuine problem of establishing a Jewish-majority state as refuge from persecution. The imperative coordinates immigration policy, land acquisition, settlement planning, and security doctrine into a coherent state-building project. Extraction toward Arab population is perceived as unfortunate necessity or externalized entirely (Arab states should absorb refugees as population exchange).
constraint_indexing:constraint_classification(demographic_engineering_imperative, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE SECURITY APPARATUS (TANGLED ROPE) — Institutional actor with constrained exit (embedded in state structure, professional identity fused with demographic security doctrine). Benefits from the imperative through expanded authority, resource allocation, and operational mandate, but also bears costs: perpetual conflict, international censure, moral injury among personnel. Genuine coordination function (border control, immigration processing, threat assessment) coexists with extractive enforcement (displacement operations, permit regime, settlement security).
constraint_indexing:constraint_classification(demographic_engineering_imperative, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REFUGEE COMMUNITIES (SNARE) — Descendants of 1948 and 1967 displaced persons, trapped in camps across Lebanon, Syria, Jordan, and occupied territories. Stateless, denied return, and excluded from the demographic calculus that produced their displacement. Maximum extraction with zero exit: cannot return, cannot integrate into host states (legal restrictions), cannot achieve political resolution (right of return blocked by demographic imperative logic). The constraint's persistence depends entirely on suppressing their return.
constraint_indexing:constraint_classification(demographic_engineering_imperative, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the demographic imperative exhibits both genuine coordination (refugee absorption, immigrant integration, state-building infrastructure) and substantial extraction (systematic displacement, denial of return, legal subordination of non-Jewish population). The imperative solves a real collective action problem for Jewish refugees while creating a permanent extraction mechanism targeting the Arab population. The analytical classification is tangled_rope because both functions are structurally present and neither can be removed without dissolving the constraint.
constraint_indexing:constraint_classification(demographic_engineering_imperative, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(demographic_engineering_imperative_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(demographic_engineering_imperative, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(demographic_engineering_imperative, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(demographic_engineering_imperative, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(demographic_engineering_imperative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High. The demographic imperative extracts systematically from the Arab population through land expropriation (1948 Absentee Property Law, ongoing settlement expansion), forced displacement (1948 Nakba, 1967 occupation, ongoing home demolitions), denial of return (Law of Return applies only to Jews, Palestinian right of return blocked), and legal subordination (differential citizenship rights, permit regimes, planning restrictions in Arab areas). The extraction is not incidental but structural — the imperative requires either mass Jewish immigration or Arab population reduction, and both mechanisms have been pursued. However, extractiveness is not maximal (not 0.9+) because genuine coordination functions exist: refugee absorption infrastructure, immigrant integration, state-building. The value reflects that extraction and coordination are inseparable rather than extraction being pure cover. Suppression (0.85): Very high. The imperative's persistence depends on suppressing alternatives: Palestinian return is blocked by military control and legal barriers; Arab political power is constrained by structural majority requirements; binational or confederal alternatives are rejected; international law principles (right of return, self-determination) are overridden. Suppression spiked in 1948 (0.88) during active displacement and has remained high (0.82-0.85) through military occupation, permit regimes, and settlement expansion. Theater ratio (0.42): Moderate. Security justifications are partly genuine — threat perception is real, historical persecution created legitimate refuge need, regional hostility is not fabricated. But theater is substantial: demographic dominance is pursued beyond security requirements (settlement expansion in areas with no security rationale, rejection of proposals that would provide security without majority control), and the framing of displacement as 'population exchange' or 'voluntary flight' obscures systematic expulsion. Theater has increased modestly over time as the gap between stated security needs and actual demographic policies has widened.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival divergence rooted in structural position. The Zionist leadership sees coordination (Rope) — solving the genuine problem of refugee absorption and state-building for a persecuted people. The Palestinian population sees pure extraction (Snare) — systematic displacement and permanent subordination with no coordination benefit. The Jewish immigrant population sees mixed coordination and extraction (Tangled Rope) — genuine benefits from absorption infrastructure coexisting with moral costs of displacement. The analytical observer sees tangled_rope as the structurally accurate classification — both coordination and extraction are present and inseparable. The gap is not resolvable through better information or communication; it reflects genuinely different structural relationships to the same constraint. The leadership's coordination experience is real (refugee absorption infrastructure is functional), and the Palestinian population's extraction experience is real (displacement and denial of return are systematic). The constraint's tangled_rope structure means both perspectives are accurate descriptions of different aspects of the same mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   The Palestinian Arab population is the primary victim with trapped exit options, yielding maximum directionality (d ≈ 0.95) and maximum effective extraction. They cannot return to pre-1948 villages (prevented by law and military control), cannot achieve majority status within the state framework (structural impossibility given immigration policy), and cannot exit the territorial control regime (statelessness, regional host states restrict integration). The Zionist movement leadership is the primary beneficiary with arbitrage exit options, yielding minimum directionality (d ≈ 0.10) and negative effective extraction (net subsidy). They coordinate the state-building project, control immigration and settlement policy, and can exit to diaspora communities if needed. The Jewish immigrant population has moderate power and constrained exit, yielding mid-range directionality (d ≈ 0.40) — they benefit from absorption infrastructure but are also instrumentalized and bear security costs. The state security apparatus is institutional with constrained exit (embedded in state structure, professional identity fused with demographic security doctrine), yielding directionality (d ≈ 0.35) — benefits from expanded mandate but bears perpetual conflict burden. Refugee communities are powerless with trapped exit, yielding maximum directionality (d ≈ 0.95) — stateless, denied return, excluded from political resolution. The analytical observer has analytical power and exit, yielding low directionality (d ≈ 0.20) — sees the full structure but is not embedded in it.
 *
 * MANDATROPHY ANALYSIS:
 *   The demographic engineering imperative resolves mandatrophy by demonstrating that coordination and extraction can be structurally inseparable rather than one being cover for the other. The mandate (establish Jewish-majority state as refuge from persecution) was genuine — European Jewish refugees needed asylum, and the Zionist movement provided it. But fulfilling this mandate structurally required either Arab displacement or permanent Arab subordination, given the demographic starting conditions (70% Arab population in 1947). The coordination function (refugee absorption) and the extraction function (displacement) are not separable — the same policies (immigration preference, land allocation, settlement planning) that coordinate Jewish refugee absorption simultaneously extract from the Arab population. This is the tangled_rope signature: active enforcement is required (military control, legal barriers to return), beneficiaries exist (Jewish immigrants, state apparatus), victims exist (Palestinian Arabs, refugees), and neither function can be removed without dissolving the constraint. The imperative has not resolved into pure extraction (snare) because the coordination function remains active (ongoing immigration absorption), nor has it resolved into pure coordination (rope) because the extraction mechanism remains structural (denial of return, settlement expansion). The mandatrophy is resolved by recognizing that some constraints coordinate for one group by extracting from another, and this structure is stable rather than transitional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_vs_extraction_primacy,
    'Is the demographic imperative primarily a security requirement (Jewish majority necessary to prevent persecution) or primarily an extraction mechanism (majority status enables permanent political dominance)?',
    'Counterfactual analysis: Would alternative security arrangements (binational state, confederation, international guarantees) have been rejected even if they provided equivalent security? Historical analysis of rejected proposals (1947 partition alternatives, 1949 Lausanne Protocol, Oslo final status).',
    'If security-primary: coordination function is genuine and extraction is side effect (tangled_rope confirmed). If extraction-primary: security framing is cover and constraint is snare from more perspectives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(security_vs_extraction_primacy, conceptual, 'Whether demographic imperative is security requirement or extraction mechanism').

omega_variable(
    transfer_inevitability,
    'Was large-scale Arab displacement structurally inevitable given the demographic imperative, or was it contingent on specific decisions and military circumstances?',
    'Historical analysis of pre-1948 transfer proposals (Peel Commission, Biltmore Program, Plan Dalet); comparison of displacement patterns in different geographic areas; analysis of explicit vs implicit transfer policies.',
    'If inevitable: the demographic imperative structurally requires displacement (higher extractiveness, snare from more perspectives). If contingent: displacement was policy choice rather than structural necessity (lower extractiveness, tangled_rope more defensible).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_inevitability, empirical, 'Whether displacement was structurally inevitable or contingent').

omega_variable(
    demographic_threshold_sufficiency,
    'What Jewish majority percentage is actually required for the coordination function (state viability, security, refugee absorption) vs what percentage is pursued for extraction function (permanent political dominance)?',
    'Comparative analysis: other multi-ethnic democracies'' stability thresholds; analysis of Israeli policy responses to demographic projections (settlement expansion when Arab birth rates rise); examination of whether 60% majority would suffice or 80%+ is pursued.',
    'If lower threshold suffices: pursuit of higher majority reveals extraction motive (higher extractiveness). If high threshold necessary: coordination function is genuine (lower extractiveness, tangled_rope confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_threshold_sufficiency, empirical, 'What demographic threshold is structurally necessary vs pursued for dominance').

omega_variable(
    reading_foreclosure_mechanism,
    'Does the national_liberation_reading logically foreclose the settler_colonial_reading, or do they coexist as competing frameworks held by different parties?',
    'Logical analysis: Can a single coherent framework hold both ''indigenous return'' and ''externally-originated displacement''? Historical analysis: Do proponents of one reading acknowledge the other as a live possibility or treat it as categorically false?',
    'If foreclosure: one reading''s axioms make the other incoherent within a single framework (reading_relations should include ''forecloses''). If coexistence: readings are held by different parties simultaneously (reading_relations should be ''coexists_with'').',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_mechanism, conceptual, 'Whether national_liberation and settler_colonial readings foreclose each other').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(demographic_engineering_imperative, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(demog_eng_theater_1947, demographic_engineering_imperative, theater_ratio, 0, 0.25).
narrative_ontology:measurement(demog_eng_theater_1948, demographic_engineering_imperative, theater_ratio, 1, 0.35).
narrative_ontology:measurement(demog_eng_theater_1957, demographic_engineering_imperative, theater_ratio, 10, 0.38).
narrative_ontology:measurement(demog_eng_theater_1967, demographic_engineering_imperative, theater_ratio, 20, 0.4).
narrative_ontology:measurement(demog_eng_theater_1977, demographic_engineering_imperative, theater_ratio, 30, 0.42).
narrative_ontology:measurement(demog_eng_theater_1997, demographic_engineering_imperative, theater_ratio, 50, 0.42).
narrative_ontology:measurement(demog_eng_theater_2022, demographic_engineering_imperative, theater_ratio, 75, 0.42).

% Extraction over time
narrative_ontology:measurement(demog_eng_extract_1947, demographic_engineering_imperative, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(demog_eng_extract_1948, demographic_engineering_imperative, base_extractiveness, 1, 0.82).
narrative_ontology:measurement(demog_eng_extract_1957, demographic_engineering_imperative, base_extractiveness, 10, 0.78).
narrative_ontology:measurement(demog_eng_extract_1967, demographic_engineering_imperative, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(demog_eng_extract_1977, demographic_engineering_imperative, base_extractiveness, 30, 0.77).
narrative_ontology:measurement(demog_eng_extract_1997, demographic_engineering_imperative, base_extractiveness, 50, 0.78).
narrative_ontology:measurement(demog_eng_extract_2022, demographic_engineering_imperative, base_extractiveness, 75, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(demog_eng_suppress_1947, demographic_engineering_imperative, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(demog_eng_suppress_1948, demographic_engineering_imperative, suppression_requirement, 1, 0.88).
narrative_ontology:measurement(demog_eng_suppress_1957, demographic_engineering_imperative, suppression_requirement, 10, 0.82).
narrative_ontology:measurement(demog_eng_suppress_1967, demographic_engineering_imperative, suppression_requirement, 20, 0.85).
narrative_ontology:measurement(demog_eng_suppress_1977, demographic_engineering_imperative, suppression_requirement, 30, 0.83).
narrative_ontology:measurement(demog_eng_suppress_1997, demographic_engineering_imperative, suppression_requirement, 50, 0.85).
narrative_ontology:measurement(demog_eng_suppress_2022, demographic_engineering_imperative, suppression_requirement, 75, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(demographic_engineering_imperative, identity_coordination).
narrative_ontology:affects_constraint(demographic_engineering_imperative, law_of_return_asymmetry).
narrative_ontology:affects_constraint(demographic_engineering_imperative, settlement_expansion_doctrine).
narrative_ontology:affects_constraint(demographic_engineering_imperative, absentee_property_regime).

% DUAL FORMULATION NOTE:
% The demographic engineering imperative is the upstream constraint that structures multiple downstream policies: Law of Return (immigration asymmetry), settlement expansion (territorial control), and absentee property laws (land expropriation). Each downstream constraint has its own extractiveness value, but all derive their structural logic from the demographic imperative. Decomposition follows the ε-invariance principle: the imperative itself (population ratio targets) has different extractiveness than specific implementing mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
