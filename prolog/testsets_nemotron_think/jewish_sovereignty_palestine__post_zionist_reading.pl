% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__post_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__post_zionist_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jewish_sovereignty_palestine__post_zionist_reading
 *   human_readable: Ethnic-National Framework of Jewish Sovereignty in Palestine (Post-Zionist Reading)
 *   domain: political/nationalism/postcolonial
 *
 * SUMMARY:
 *   The post-Zionist reading identifies the ethnic-national framework of the
 *   Israeli state — anchored in the Law of Return (1950), the Jewish
 *   Nation-State Law (2018), the JNF/ILA land regime, and military rule over
 *   Palestinians — as a constraint that once coordinated Jewish collective
 *   survival but now extracts privileges for Jewish citizens at the expense
 *   of Palestinian citizens and occupied populations. The framework presents
 *   itself as the realization of Jewish self-determination (coordination
 *   story) while structurally requiring Palestinian subordination
 *   (extraction). The claimed_type is tangled_rope because genuine
 *   coordination (Jewish immigration absorption, defense, cultural revival)
 *   coexists with asymmetric extraction (land, rights, resources transferred
 *   from Palestinians to Jews), and the constraint requires active
 *   enforcement (military occupation, legal discrimination, demographic
 *   engineering) to persist.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__post_zionist_reading, 0.72).
domain_priors:suppression_score(jewish_sovereignty_palestine__post_zionist_reading, 0.85).
domain_priors:theater_ratio(jewish_sovereignty_palestine__post_zionist_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__post_zionist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__post_zionist_reading, "Ethnic-National Framework of Jewish Sovereignty in Palestine (Post-Zionist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__post_zionist_reading, "political/nationalism/postcolonial").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__post_zionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__post_zionist_reading, '72daa34c-469c-4a44-8723-ebabb7bf505c').
narrative_ontology:cs_kernel_codification('72daa34c-469c-4a44-8723-ebabb7bf505c', formalized).
narrative_ontology:cs_authority_grounding('72daa34c-469c-4a44-8723-ebabb7bf505c', extraction).
narrative_ontology:cs_interpretation_layer_present('72daa34c-469c-4a44-8723-ebabb7bf505c').
narrative_ontology:cs_reading_relation('72daa34c-469c-4a44-8723-ebabb7bf505c', jewish_sovereignty_palestine__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('72daa34c-469c-4a44-8723-ebabb7bf505c', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('72daa34c-469c-4a44-8723-ebabb7bf505c', jewish_sovereignty_palestine__settler_colonial_reading, influences).
narrative_ontology:cs_reading_relation('72daa34c-469c-4a44-8723-ebabb7bf505c', jewish_sovereignty_palestine__cultural_zionist_reading, coexists_with).
narrative_ontology:cs_axiom('72daa34c-469c-4a44-8723-ebabb7bf505c', foundational, ethnic_national_framework_obstructs_civic_equality).
narrative_ontology:cs_axiom_status(ethnic_national_framework_obstructs_civic_equality, holdable).
narrative_ontology:cs_axiom_grounding('72daa34c-469c-4a44-8723-ebabb7bf505c', ethnic_national_framework_obstructs_civic_equality, empirically_contingent).
narrative_ontology:cs_axiom('72daa34c-469c-4a44-8723-ebabb7bf505c', secondary, dezionization_required_for_regional_integration).
narrative_ontology:cs_axiom_status(dezionization_required_for_regional_integration, holdable).
narrative_ontology:cs_axiom_grounding('72daa34c-469c-4a44-8723-ebabb7bf505c', dezionization_required_for_regional_integration, instrumental).
narrative_ontology:cs_reference_frame('72daa34c-469c-4a44-8723-ebabb7bf505c', liberal_zionist_settlement).
narrative_ontology:cs_drift_state('72daa34c-469c-4a44-8723-ebabb7bf505c', post_oslo_nation_state_law_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('72daa34c-469c-4a44-8723-ebabb7bf505c', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__post_zionist_reading, jewish_citizens).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, palestinian_citizens_of_israel).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, occupied_palestinians).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__post_zionist_reading, jewish_self_determination_historical_achievement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the ethnic-national framework through Basic Laws (Law of Return, Jewish Nation-State Law), land allocation authorities (JNF/ILA), military rule in occupied territories, and demographic engineering policies. Sets the agenda for what counts as Jewish sovereignty and enforces the boundary between privileged and subordinated populations. Could reform the framework but chooses to intensify it.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Collect structural privileges via Law of Return (automatic citizenship), preferential land access (JNF land leased only to Jews), institutional preference in housing/education/employment, and demographic majority protection. Many experience identity-locked attachment to the framework (Zionist education, military service, historical narrative), but material exit (emigration) is legally and economically feasible. The constraint subsidizes their collective position.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, jewish_citizens, beneficiary,
    organized, biographical, mobile, national).

% Hold Israeli citizenship but face legal discrimination (Nation-State Law declaring Jewish self-determination exclusive, Admissions Committees Law, land allocation exclusion), institutional underinvestment, and political marginalization. Exit is constrained: they cannot exercise right of return for displaced relatives, face barriers to emigration, and their citizenship is conditional on Jewish demographic majority. Bear extraction through taxes funding settlements and institutions that exclude them.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, palestinian_citizens_of_israel, payer,
    moderate, generational, constrained, national).

% Live under military occupation (West Bank) or blockade (Gaza) without citizenship, civil rights, or self-determination. Subject to land expropriation for settlements, movement restrictions, administrative detention, and separate legal systems. No exit: cannot leave permanently (revoked residency), cannot return if displaced, cannot access Israeli courts meaningfully. Bear the highest extraction — their land, labor, and autonomy sustain the framework.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, occupied_palestinians, payer,
    powerless, generational, trapped, regional).

% Descendants of those displaced in 1948 and 1967, denied right of return by the same Law of Return that grants automatic citizenship to any Jew globally. Stateless or host-country residents with no voice in the framework that permanently excludes them. Would object to the ethnic-national framework as the structural cause of their displacement but are structurally absent from the conversation.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, palestinian_refugees, excluded,
    powerless, generational, trapped, global).

% UN bodies, human rights NGOs, foreign states, and international courts that document apartheid findings, settlement illegality, and democratic deficits. They analyze the constraint's operation and can impose reputational/legal costs but lack enforcement power. Their analytical seat sees the full structure: coordination function (Jewish collective survival) fused with extraction apparatus (Palestinian subordination).
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, international_observers, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a collective survival framework for a persecuted people: immigration absorption, defense, Hebrew revival, and state-building after the Holocaust. The ethnic-national framework solved the coordination problem of gathering a dispersed population and establishing defensible sovereignty.
% TRANSFER_FUNCTION: Moves land, water, budgetary resources, legal protections, and political power from Palestinian citizens and occupied populations to Jewish citizens and the state apparatus. The Law of Return converts Jewishness into citizenship+land rights; the planning regime converts Palestinian land into Jewish settlements; the tax system funds institutions that serve the privileged group.
% ABSENT_VOICES: Palestinian refugees (denied return and voice), Gaza population (under blockade, no political representation), diaspora Palestinians excluded from any institutional channel. They would demand right of return, equality, and dismantling of the ethnic framework but are kept out by the same laws the framework enforces.
% DISAPPEARANCE_RATIONALE: If the ethnic-national framework vanished overnight (Law of Return repealed, Nation-State Law nullified, military occupation ended, land allocation equalized), the Israeli state would face immediate constitutional crisis: Jewish demographic majority could not be guaranteed, settlement enterprise would lose legal basis, resource allocation would require new distributive logic. The region would reorganize around civic equality or partition — the current arrangement depends entirely on the constraint's enforcement.
% FOUNDING_PROBLEM: Jewish statelessness and persecution in Europe culminating in the Holocaust created an existential need for a sovereign refuge where Jews could exercise collective self-determination and control immigration.
% FOUNDING_PROBLEM_CORROBORATION: Post-Zionist historians (Shlaim, Pappé, Sand) and Palestinian scholars attest the founding problem (statelessness/persecution) was resolved by 1948 statehood but the ethnic framework persisted as extraction. Liberal Zionists (Oz, Grossman) attest the problem remains live due to ongoing antisemitism and regional hostility. Israeli state institutions attest the problem is live and requires the ethnic framework. No consensus exists outside the beneficiary framework.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__post_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__post_zionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__post_zionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__post_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__post_zionist_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__post_zionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__post_zionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__post_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the framework systematically transfers land, water, budgets, and political power from Palestinians to Jews — the Law of Return alone converts Jewish identity into citizenship and land rights denied to Palestinian refugees. Suppression is very high (0.85) because the framework's persistence depends on military occupation, separate legal systems, blockade, and legal barriers to equality (Admissions Committees, Nation-State Law). Theater ratio is moderate-high (0.58) because democratic rituals (elections, Supreme Court, Knesset) are maintained while substantive equality is denied — the 'Jewish and democratic' slogan performs legitimacy for an arrangement that is structurally Jewish-supremacist. Accessibility collapse (0.62) reflects that alternatives (one-state equality, two-state partition, binationalism) are legally and politically blocked but not unimaginable. Resistance (0.71) is high: Palestinian sumud, BDS, legal challenges, and internal dissent (refuseniks, human rights orgs) meet the constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (Israeli state), the constraint appears as necessary coordination: Jewish survival requires demographic majority, which requires Law of Return and land control. From the payer seats (Palestinian citizens, occupied Palestinians), the same structure appears as enforced extraction: their land, rights, and autonomy are the price of Jewish demographic engineering. The engine computes this divergence — the post-Zionist reading does not adjudicate it but authors the structural data that makes it visible.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish citizens are structural beneficiaries (d near 0.0): the constraint subsidizes their collective position via immigration monopoly, land access, and demographic protection. Israeli state apparatus is agenda_setter with arbitrage-grade exit (could reform but chooses intensification). Palestinian citizens of Israel are payers with constrained exit (citizenship traps them, emigration difficult, no right of return for kin) — d near 0.8. Occupied Palestinians are payers with trapped exit (no citizenship, no mobility, no political outlet) — d near 1.0. Palestinian refugees are excluded (would object but structurally absent) — d effectively 1.0 but no seat at the table. International observers are analytical (d = 0.5, symmetric costs/benefits of analysis). The engine will compute per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Jewish statelessness/persecution) was substantially resolved by 1948 statehood and 1967 military dominance. The ethnic-national framework persists not because the founding problem remains unsolved, but because it now serves as an extraction apparatus for Jewish citizens and an entrenchment mechanism for the state apparatus. The constraint exhibits mandatrophy: its mandate (Jewish refuge) has atrophied into a privilege structure (Jewish supremacy). The founding_problem_status = contested captures this: beneficiaries claim the problem is live; victims and observers attest it is substantially solved but the framework persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ethnic_framework_inseparability,
    'Is the ethnic-national framework structurally inseparable from Jewish collective self-determination, or can Jewish self-determination be realized through civic equality?',
    'Historical test: if a Jewish-majority state with full civic equality for Palestinians emerges and maintains Jewish cultural autonomy, the framework is separable. If every Jewish-majority arrangement converges on ethnic privilege, the framework may be inseparable from the self-determination claim.',
    'If inseparable, the constraint is a mountain for Jewish self-determination (no alternative coordination exists) — the post-Zionist critique is utopian. If separable, the constraint is a tangled_rope/snare where extraction is a choice, not a necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ethnic_framework_inseparability, conceptual, 'Whether Jewish self-determination requires ethnic privilege or can coexist with civic equality.').

omega_variable(
    suppression_mechanism_ambiguity_occupied,
    'Is the suppression of occupied Palestinians primarily structural (military rule, legal barriers) or partially internalized (Palestinian Authority security coordination, normalized dependency on Israeli permits)?',
    'Post-exit suppression trajectory: if Palestinian Authority were dissolved and direct military rule expanded, would suppression intensity change? Compare First Intifada (direct rule) vs. Oslo era (PA mediation) suppression patterns.',
    'If internalized component is significant, the constraint''s effective suppression is higher than structural measures suggest — the occupied population participates in its own containment. This would increase the constraint''s snare-like character.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity_occupied, empirical, 'Structural vs. internalized suppression mechanisms in the occupied territories.').

omega_variable(
    demographic_engineering_necessity,
    'Is Jewish demographic majority (the core coordination claim) actually threatened by civic equality, or is the threat constructed to justify the extraction framework?',
    'Demographic modeling: compare Jewish population trends with/without Law of Return, with/without occupied territory annexation. Test whether a civic-equal one-state would produce Jewish minority within 1-2 generations.',
    'If demographic threat is constructed (e.g., Jewish fertility now exceeds Palestinian fertility in Israel proper, immigration declining), the extraction is not coordination-necessary but privilege-preserving — strengthening snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_engineering_necessity, empirical, 'Whether demographic engineering is a genuine coordination requirement or a cover for extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__post_zionist_reading, 0, 76).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(jewi_tr_t19, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 19, 0.35).
narrative_ontology:measurement(jewi_tr_t45, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 45, 0.45).
narrative_ontology:measurement(jewi_tr_t52, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 52, 0.52).
narrative_ontology:measurement(jewi_tr_t70, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 70, 0.56).
narrative_ontology:measurement(jewi_tr_t76, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 76, 0.58).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(jewi_be_t19, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 19, 0.58).
narrative_ontology:measurement(jewi_be_t45, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 45, 0.62).
narrative_ontology:measurement(jewi_be_t52, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 52, 0.68).
narrative_ontology:measurement(jewi_be_t70, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 70, 0.71).
narrative_ontology:measurement(jewi_be_t76, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 76, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(jewi_su_t19, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 19, 0.75).
narrative_ontology:measurement(jewi_su_t45, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 45, 0.78).
narrative_ontology:measurement(jewi_su_t52, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 52, 0.81).
narrative_ontology:measurement(jewi_su_t70, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 70, 0.83).
narrative_ontology:measurement(jewi_su_t76, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 76, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__post_zionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_sovereignty_palestine__post_zionist_reading, 0.08).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, palestinian_national_movement).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, middle_east_regional_order).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, international_law_occupation_framework).

% DUAL FORMULATION NOTE:
% This constraint (post_zionist_reading) is one of five readings of kernel jewish_sovereignty_palestine. The liberal_nationalist_reading and religious_zionist_reading treat the ethnic framework as legitimate coordination; the settler_colonial_reading treats it as pure extraction (snare); the cultural_zionist_reading treats political sovereignty as unnecessary. This reading identifies the framework as a tangled_rope: genuine historical coordination now fused with asymmetric extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_sovereignty_palestine__post_zionist_reading, organized, 0.15).
constraint_indexing:directionality_override(jewish_sovereignty_palestine__post_zionist_reading, moderate, 0.75).
constraint_indexing:directionality_override(jewish_sovereignty_palestine__post_zionist_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
