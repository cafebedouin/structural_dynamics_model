% ============================================================================
% CONSTRAINT STORY: british_mandate_scaffolding
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_british_mandate_scaffolding, []).

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
 *   constraint_id: british_mandate_scaffolding
 *   human_readable: British Mandate Scaffolding for Zionist State-Building
 *   domain: political_history/nationalism/settler_colonialism
 *
 * SUMMARY:
 *   The British Mandate for Palestine (1920-1948) functioned as imperial
 *   scaffolding enabling Zionist state-building that could not have occurred
 *   autonomously against Ottoman opposition, Arab majority resistance, and
 *   resource constraints. The Balfour Declaration (1917) and subsequent
 *   Mandate terms privileged Jewish immigration and settlement, provided
 *   legal framework and military protection, and facilitated land acquisition
 *   — creating conditions for demographic transformation from ~10% Jewish
 *   (1920) to ~33% Jewish (1947) population. The constraint exhibits scaffold
 *   structure from the intended beneficiary's perspective (Zionist
 *   institutions experienced temporary support with explicit sunset) while
 *   operating as snare from the excluded majority's perspective (Palestinian
 *   Arabs experienced pure extraction with no exit and no access to the
 *   coordination function). The scaffold terminated on schedule (1948) when
 *   British withdrew, having fulfilled its function for the intended
 *   beneficiary: a militarily viable Jewish quasi-state existed where none
 *   could have developed without imperial protection. Mandatrophy resolved:
 *   the mandate's function (enable Zionist state-building) outlived its
 *   formal justification (orderly transition to self-governance for all
 *   inhabitants) — the structure persisted in service of one population's
 *   political project rather than the declared universal coordination goal.
 *
 * KEY AGENTS:
 *   - Zionist Quasi-State Institutions: Primary beneficiary (institutional/arbitrage) — Jewish Agency, Haganah, Histadrut developed under British protection; experienced scaffold as intended transitional support
 *   - Palestinian Arabs Under Mandate: Primary victim (powerless/trapped) — majority population subjected to demographic engineering, land dispossession, and military suppression with no exit; experienced pure extraction
 *   - British Imperial Administration: Secondary beneficiary (institutional/arbitrage) — maintained Middle East influence post-Ottoman collapse through Mandate framework; genuine coordination function for imperial interests
 *   - Arab Nationalist Movements: Constrained actors (moderate/constrained) — could organize and petition but faced military suppression; experienced hybrid coordination-extraction
 *   - League of Nations Mandate System: Institutional framework (institutional/mobile) — oversight mechanism that atrophied into theatrical cover; piton perspective
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes genuine scaffold structure with asymmetric beneficiary distribution and declared sunset
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(british_mandate_scaffolding, 0.68).
domain_priors:suppression_score(british_mandate_scaffolding, 0.82).
domain_priors:theater_ratio(british_mandate_scaffolding, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(british_mandate_scaffolding, extractiveness, 0.68).
narrative_ontology:constraint_metric(british_mandate_scaffolding, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(british_mandate_scaffolding, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(british_mandate_scaffolding, scaffold).
narrative_ontology:human_readable(british_mandate_scaffolding, "British Mandate Scaffolding for Zionist State-Building").
narrative_ontology:topic_domain(british_mandate_scaffolding, "political_history/nationalism/settler_colonialism").

domain_priors:requires_active_enforcement(british_mandate_scaffolding).
narrative_ontology:has_sunset_clause(british_mandate_scaffolding).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(british_mandate_scaffolding, '27967d3f-e6c2-4c12-8cb3-6ac8bb2f3be8').
narrative_ontology:cs_kernel_codification('27967d3f-e6c2-4c12-8cb3-6ac8bb2f3be8', fixed_text).
narrative_ontology:cs_authority_grounding('27967d3f-e6c2-4c12-8cb3-6ac8bb2f3be8', lineage).
narrative_ontology:cs_interpretation_layer_present('27967d3f-e6c2-4c12-8cb3-6ac8bb2f3be8').
narrative_ontology:cs_reading_relation('27967d3f-e6c2-4c12-8cb3-6ac8bb2f3be8', british_mandate_scaffolding__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('27967d3f-e6c2-4c12-8cb3-6ac8bb2f3be8', british_mandate_scaffolding__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('27967d3f-e6c2-4c12-8cb3-6ac8bb2f3be8', foundational, persecution_justifies_return).
narrative_ontology:cs_axiom_status(persecution_justifies_return, holdable).
narrative_ontology:cs_axiom_grounding('27967d3f-e6c2-4c12-8cb3-6ac8bb2f3be8', persecution_justifies_return, deontological).
narrative_ontology:cs_axiom('27967d3f-e6c2-4c12-8cb3-6ac8bb2f3be8', foundational, historical_presence_establishes_right).
narrative_ontology:cs_axiom_status(historical_presence_establishes_right, holdable).
narrative_ontology:cs_axiom_grounding('27967d3f-e6c2-4c12-8cb3-6ac8bb2f3be8', historical_presence_establishes_right, conventional).
narrative_ontology:cs_axiom('27967d3f-e6c2-4c12-8cb3-6ac8bb2f3be8', secondary, self_determination_overrides_demographic_majority).
narrative_ontology:cs_axiom_status(self_determination_overrides_demographic_majority, holdable).
narrative_ontology:cs_axiom_grounding('27967d3f-e6c2-4c12-8cb3-6ac8bb2f3be8', self_determination_overrides_demographic_majority, deontological).
narrative_ontology:cs_reference_frame('27967d3f-e6c2-4c12-8cb3-6ac8bb2f3be8', ancient_jewish_sovereignty_in_eretz_yisrael).
narrative_ontology:cs_drift_state('27967d3f-e6c2-4c12-8cb3-6ac8bb2f3be8', post_mandate_establishment_1948, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('27967d3f-e6c2-4c12-8cb3-6ac8bb2f3be8', '2026-06-06T03:34:05.061231+00:00').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(british_mandate_scaffolding, zionist_quasi_state).
narrative_ontology:constraint_beneficiary(british_mandate_scaffolding, british_imperial_administration).
narrative_ontology:constraint_victim(british_mandate_scaffolding, palestinian_arabs_under_mandate).
narrative_ontology:constraint_victim(british_mandate_scaffolding, regional_arab_sovereignty).
narrative_ontology:constraint_vindicates(british_mandate_scaffolding, balfour_declaration_legitimacy).
narrative_ontology:constraint_vindicates(british_mandate_scaffolding, jewish_national_home_doctrine).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PALESTINIAN ARABS (SNARE) — Trapped population experiencing pure extraction. British military suppression of resistance (1936-39 revolt crushed), land sales facilitated despite opposition, immigration quotas imposed over majority objection, no exit from territorial jurisdiction. The scaffold's coordination function (orderly transition to self-governance per Mandate terms) was never extended to this population — only its extractive apparatus operated. Maximum experienced extraction.
constraint_indexing:constraint_classification(british_mandate_scaffolding, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ZIONIST INSTITUTIONS (SCAFFOLD) — Primary beneficiary experiencing temporary support structure. Jewish Agency, Haganah, Histadrut, and settlement infrastructure developed under British protection with explicit sunset logic: Mandate provisions anticipated eventual self-governance. Arbitrage exit options (could negotiate with British, appeal to League of Nations, build parallel institutions). Experienced the constraint as intended — transitional coordination enabling state-building that could not occur autonomously. Low effective extraction because benefits flow toward this agent.
constraint_indexing:constraint_classification(british_mandate_scaffolding, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: BRITISH ADMINISTRATION (ROPE) — Secondary beneficiary. Mandate provided legal framework for maintaining Middle East influence post-Ottoman collapse, strategic position for Suez/India route, and management of competing claims without direct annexation costs. Coordination function genuine from this perspective: managing imperial transition while balancing Zionist and Arab demands. Theater_ratio reflects that 'even-handed' administration rhetoric increasingly diverged from structural favoritism, but coordination function remained operative for British interests.
constraint_indexing:constraint_classification(british_mandate_scaffolding, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: ARAB NATIONALIST MOVEMENTS (TANGLED ROPE) — Constrained actors experiencing mixed coordination and extraction. Mandate system provided formal framework for eventual self-determination (coordination function) but simultaneously facilitated demographic transformation that undermined that self-determination (extraction function). Could organize resistance and appeal to international bodies but faced military suppression and legal constraints. Genuine hybrid: the same structure both promised sovereignty and enabled its foreclosure.
constraint_indexing:constraint_classification(british_mandate_scaffolding, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: LEAGUE MANDATE SYSTEM (PITON) — Degraded coordination mechanism. Original function (orderly transition from Ottoman rule to self-governance for 'peoples not yet able to stand by themselves') atrophied into legal cover for continued imperial control and demographic engineering. By 1930s, Mandate oversight was largely theatrical — League could not enforce its own terms, and the 'sacred trust of civilization' language became performance masking extraction. High theater_ratio from this institutional perspective.
constraint_indexing:constraint_classification(british_mandate_scaffolding, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SCAFFOLD) — Recognizes genuine transitional structure with declared sunset (1948 termination) and asymmetric beneficiary distribution. The scaffold functioned as designed for its intended beneficiary (Zionist institutions) while operating as snare for excluded population. This is not a false summit — the temporary support structure was real, the sunset occurred, and the dependency relationship (Zionist state-building required external imperial power) is historically verified. Classification as scaffold does not adjudicate legitimacy of the project scaffolded, only the structural relationship.
constraint_indexing:constraint_classification(british_mandate_scaffolding, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(british_mandate_scaffolding_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(british_mandate_scaffolding, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(british_mandate_scaffolding, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(british_mandate_scaffolding, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(british_mandate_scaffolding, TR),
    TR >= 0.70.

:- end_tests(british_mandate_scaffolding_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): Substantial. The Mandate enabled demographic transformation against majority opposition through immigration facilitation (575,000 Jewish immigrants 1920-1945 vs. restricted Arab immigration), land acquisition privileges (Jewish National Fund purchases protected from reversal), and military suppression of resistance (1936-39 Arab Revolt crushed with ~5,000 Arab deaths, collective punishments, village demolitions). Extraction accumulated over interval as Jewish population grew and land transfers accelerated, peaking during 1936-39 suppression, then declining slightly as British faced insurgency costs. Suppression (0.82): High. British military force was essential — Yishuv could not have defended settlements or suppressed Arab resistance autonomously until late 1940s. Suppression intensified dramatically during 1936-39 revolt (20,000+ British troops deployed, emergency regulations, collective punishments) then remained high through 1948. Theater_ratio (0.45): Moderate. Mandate rhetoric emphasized even-handed administration and preparing all inhabitants for self-governance, but structural favoritism toward Zionist project was embedded in Balfour Declaration and Mandate terms. Theater increased through 1930s as gap between rhetoric and practice widened (Peel Commission 1937 acknowledged Mandate contradictions), then decreased slightly in 1940s as British abandoned pretense. The coordination function was genuine for British imperial interests and Zionist institutions, but theatrical for Palestinian Arabs who were never intended beneficiaries despite Mandate language.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how scaffold classification is observer-dependent when beneficiary distribution is asymmetric. Zionist institutions experienced genuine temporary support with declared sunset — the scaffold functioned as designed from their perspective. Palestinian Arabs experienced snare — pure extraction with no access to coordination function and no exit. British administration experienced rope — coordination of imperial interests. Arab nationalists experienced tangled rope — the same structure both promised and foreclosed sovereignty. The analytical observer recognizes scaffold structure (temporary support, declared sunset, dependency relationship verified) while acknowledging the asymmetric beneficiary distribution. The perspectival gap is not a classification error — it reveals that scaffolds can operate as snares for excluded populations. The mandate's function (enable one population's state-building) and its justification (prepare all inhabitants for self-governance) diverged structurally.
 *
 * DIRECTIONALITY LOGIC:
 *   Zionist institutions: Full beneficiaries with arbitrage exit options. The Mandate's legal framework, military protection, and immigration facilitation flowed toward this agent. Low d → negative or near-zero chi (experienced as coordination/subsidy). Palestinian Arabs: Full victims with trapped exit options. Demographic engineering, land dispossession, and suppression of resistance extracted from this population with no alternative. High d → maximum chi (experienced as pure extraction). British administration: Secondary beneficiary. Maintained imperial influence through Mandate framework at lower cost than direct rule. Moderate-low d → low chi (experienced as coordination). Arab nationalist movements: Mixed position — formal framework promised self-determination (beneficiary aspect) but facilitated foreclosure of that promise (victim aspect). Constrained exit options. Moderate d → moderate chi (experienced as tangled rope). The directionality derivation captures the scaffold's asymmetric structure: one population received the transitional support, another bore its costs.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The British Mandate's formal justification (Article 22 of League Covenant: 'sacred trust of civilization' to prepare inhabitants for self-governance) outlived its actual function (provide imperial scaffolding for Zionist state-building against majority opposition). By 1930s, the contradiction was explicit: Peel Commission (1937) acknowledged Mandate obligations to Arabs and Jews were irreconcilable, recommended partition. The structure persisted not because it was preparing all inhabitants for self-governance (the declared mandate) but because it was enabling one population's political project (the operational function). Mandatrophy resolved in 1948 when British withdrew and the scaffolded structure (Jewish quasi-state) achieved autonomy. The mandate's sunset was on schedule from the intended beneficiary's perspective but forced abandonment from the excluded population's perspective — British faced rising costs (Jewish insurgency, Arab resistance, international pressure) and withdrew without fulfilling the universal coordination mandate. The scaffold-to-piton trajectory the CS system predicts did not fully materialize because the structure terminated before complete functional atrophy, but the gap between justification and function widened throughout the interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_autonomy,
    'Could Zionist state-building have succeeded without British imperial scaffolding, given Ottoman opposition, Arab majority resistance, and resource constraints?',
    'Comparative analysis of nationalist movements succeeding against majority opposition without external imperial sponsor; assessment of Yishuv resource base and military capacity absent British protection 1920-1948',
    'If autonomous success plausible: scaffold classification weakens toward rope (mere coordination). If imperial dependency structural: scaffold classification confirmed — the temporary support was necessary, not incidental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_autonomy, empirical, 'Whether Zionist state-building required British imperial scaffolding').

omega_variable(
    mandate_evenhandedness,
    'Did British administration genuinely attempt even-handed governance between Jewish and Arab populations, or was structural favoritism toward Zionist project inherent from Balfour Declaration forward?',
    'Quantitative analysis of: land sale regulations and enforcement asymmetry, immigration quota setting vs. Arab petitions, military response to Jewish vs. Arab violence, resource allocation to Jewish vs. Arab institutions, British personnel statements in internal documents',
    'If even-handed attempt: higher coordination function, lower extraction (more rope-like). If structural favoritism: coordination function was cover, extraction was design (more snare-like from Arab perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_evenhandedness, empirical, 'Whether British Mandate administration was structurally even-handed').

omega_variable(
    sunset_inevitability,
    'Was 1948 termination an intended sunset (Mandate designed to end upon self-governance achievement) or forced abandonment (British withdrew due to cost and violence, not design completion)?',
    'Analysis of: original Mandate terms on self-governance timeline, British policy documents 1920-1948 on exit conditions, financial and military cost trajectories, role of Jewish insurgency (Irgun, Lehi) and Arab revolt in forcing withdrawal decision',
    'If intended sunset: scaffold classification strengthened (transitional by design). If forced abandonment: scaffold classification weakens (structure persisted beyond intended function, closer to piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_inevitability, empirical, 'Whether Mandate termination was designed sunset or forced abandonment').

omega_variable(
    cs_framing_underdetermination,
    'Is the kernel ''historical right to Palestine'' or the institutional claim ''Zionist leadership interprets Jewish collective needs''? Does the reading instantiate a claim about ancient presence or about 20th-century political authority?',
    'Distinguish two framings: (A) kernel = ancient Jewish presence + biblical covenant, authority = Zionist leadership''s interpretation of that history''s implications; (B) kernel = Zionist movement''s political program, authority = movement leadership''s organizational control. Framing A produces national_liberation_reading vs settler_colonial_reading as contested interpretations of the same historical kernel. Framing B produces organizational authority contest (who speaks for Jews?) rather than historical legitimacy contest.',
    'Framing A: cs_pattern matches lineage + distributed (competing interpretations of historical kernel). Framing B: cs_pattern matches practice + extraction (organizational authority grounded in movement control). The constraint story adopts Framing A because the SCOPE manifest''s kernel_description emphasizes historical continuity, but Framing B is defensible and would change axiom structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Kernel framing: historical legitimacy vs organizational authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(british_mandate_scaffolding, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mandate_theater_1920, british_mandate_scaffolding, theater_ratio, 0, 0.25).
narrative_ontology:measurement(mandate_theater_1925, british_mandate_scaffolding, theater_ratio, 5, 0.3).
narrative_ontology:measurement(mandate_theater_1930, british_mandate_scaffolding, theater_ratio, 10, 0.38).
narrative_ontology:measurement(mandate_theater_1935, british_mandate_scaffolding, theater_ratio, 15, 0.48).
narrative_ontology:measurement(mandate_theater_1940, british_mandate_scaffolding, theater_ratio, 20, 0.42).
narrative_ontology:measurement(mandate_theater_1945, british_mandate_scaffolding, theater_ratio, 25, 0.45).

% Extraction over time
narrative_ontology:measurement(mandate_extract_1920, british_mandate_scaffolding, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(mandate_extract_1925, british_mandate_scaffolding, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(mandate_extract_1930, british_mandate_scaffolding, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(mandate_extract_1935, british_mandate_scaffolding, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(mandate_extract_1940, british_mandate_scaffolding, base_extractiveness, 20, 0.72).
narrative_ontology:measurement(mandate_extract_1945, british_mandate_scaffolding, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(mandate_suppress_1920, british_mandate_scaffolding, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(mandate_suppress_1928, british_mandate_scaffolding, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(mandate_suppress_1936, british_mandate_scaffolding, suppression_requirement, 16, 0.88).
narrative_ontology:measurement(mandate_suppress_1939, british_mandate_scaffolding, suppression_requirement, 19, 0.92).
narrative_ontology:measurement(mandate_suppress_1945, british_mandate_scaffolding, suppression_requirement, 25, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(british_mandate_scaffolding, enforcement_mechanism).
narrative_ontology:affects_constraint(british_mandate_scaffolding, demographic_engineering_imperative).

% DUAL FORMULATION NOTE:
% The British Mandate scaffolding is upstream of the demographic engineering imperative. The Mandate provided the legal and military framework that made demographic transformation possible; the imperative (maintain Jewish majority) is the downstream constraint that emerged once that transformation was achieved. Separate stories because they have different epsilon values: the Mandate's extractiveness reflects imperial facilitation of settlement (0.68), while demographic engineering's extractiveness reflects the ongoing maintenance of achieved demographic facts (separate story, higher epsilon). The Mandate terminated in 1948; demographic engineering persists.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
