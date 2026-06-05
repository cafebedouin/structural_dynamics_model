% ============================================================================
% CONSTRAINT STORY: postwar_constitutionalism__indian_constitution_1950
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_postwar_constitutionalism__indian_constitution_1950, []).

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
 *   constraint_id: postwar_constitutionalism__indian_constitution_1950
 *   human_readable: Indian Constitution 1950: Postwar Constitutionalism as Social Revolution
 *   domain: political/legal/postcolonial
 *
 * SUMMARY:
 *   The Indian Constitution of 1950 is postwar constitutionalism at
 *   subcontinental scale — an attempt at social revolution by exhaustive
 *   constitutional text in a society structured by millennia of inherited
 *   caste hierarchy. Drafted by a constituent assembly led by B.R. Ambedkar
 *   (a Dalit intellectual) and Pandit Jawaharlal Nehru (from the nationalist
 *   elite), the Constitution simultaneously establishes liberal democratic
 *   governance for a subcontinental polity and attempts to suppress the
 *   enumerated hierarchies (caste, religious discrimination, feudal
 *   privilege) that predate and structure Indian society. The document is
 *   extraordinarily detailed (446 articles, numerous schedules) precisely
 *   because it attempts to specify abolition and redirection of extraction
 *   from the old hierarchy to the new state's redistributive mandate. The
 *   constraint exhibits tangled-rope structure: genuine coordination enabling
 *   democratic participation, rule of law, and pluralistic governance
 *   coexists with embedded extraction redirected from caste hierarchy toward
 *   state-controlled redistribution (land reform, reservations, affirmative
 *   action). The suppression metric reflects the structural difficulty of
 *   overriding inherited hierarchies through text alone — the document
 *   prohibits discrimination but cannot unilaterally eliminate the social
 *   practices and economic structures that reproduce caste. The theater ratio
 *   is moderate (0.48) because the Constitution's enforcement relies partly
 *   on performative ritual (court proceedings, legislative debates) but also
 *   on genuine institutional mechanisms (judiciary, administrative
 *   enforcement, electoral participation). The extractiveness trajectory
 *   shows rise from 1950 (0.42) to peak (0.64 by 1985, when new group
 *   conflicts emerged over intermediate-caste reservations) and then
 *   stabilization (0.60 by 2005 as elite capture mechanisms routinized and
 *   enforcement plateaued).
 *
 * KEY AGENTS:
 *   - Scheduled Castes and Tribes: Primary intended beneficiary (powerless/trapped) — liberation text is written for them, but enforcement remains contingent on political will they do not fully control. Simultaneously beneficiary and victim of enforcement variability.
 *   - Brahminical and Upper-Caste Elites: Primary victims of the old hierarchy's suppression (powerful/constrained) — their inherited privileges (ritual authority, land monopoly, occupational control, marriage rules) are directly prohibited by enumerated text. Experience high suppression of traditional mechanisms.
 *   - Intermediate Castes and Urban Professionals: Mixed beneficiaries-victims (moderate/constrained, powerful/mobile) — benefit from education access and occupational mobility enabled by the Constitution; bear costs of competition and affirmative action policies targeting scheduled castes.
 *   - Constitutional Authority and State Apparatus: Implementing institutions (institutional/arbitrage) — the courts, legislatures, and bureaucracy that interpret and enforce the document. Experience it as coordination mechanism, not extraction.
 *   - Post-Independence Congress Party and Political Leadership: Executive enforcement agents (institutional/arbitrage) — control implementation of land reform, reservation policy, and caste-based discrimination enforcement. Their political will determines whether the Constitution's suppressive intent is realized.
 *   - Analytical Observer: Cross-position perspective (analytical/analytical) — sees the full tangled structure: coordination function enabling plural democracy, extraction function redirecting hierarchy benefits toward state control, partial enforcement contingent on coalition politics.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(postwar_constitutionalism__indian_constitution_1950, 0.58).
domain_priors:suppression_score(postwar_constitutionalism__indian_constitution_1950, 0.72).
domain_priors:theater_ratio(postwar_constitutionalism__indian_constitution_1950, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(postwar_constitutionalism__indian_constitution_1950, extractiveness, 0.58).
narrative_ontology:constraint_metric(postwar_constitutionalism__indian_constitution_1950, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(postwar_constitutionalism__indian_constitution_1950, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(postwar_constitutionalism__indian_constitution_1950, tangled_rope).
narrative_ontology:human_readable(postwar_constitutionalism__indian_constitution_1950, "Indian Constitution 1950: Postwar Constitutionalism as Social Revolution").
narrative_ontology:topic_domain(postwar_constitutionalism__indian_constitution_1950, "political/legal/postcolonial").

domain_priors:requires_active_enforcement(postwar_constitutionalism__indian_constitution_1950).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(postwar_constitutionalism__indian_constitution_1950, 'c23f781e-2b71-4862-86a2-f0e82a91bfe9').
narrative_ontology:cs_kernel_codification('c23f781e-2b71-4862-86a2-f0e82a91bfe9', formalized).
narrative_ontology:cs_authority_grounding('c23f781e-2b71-4862-86a2-f0e82a91bfe9', lineage).
narrative_ontology:cs_interpretation_layer_present('c23f781e-2b71-4862-86a2-f0e82a91bfe9').
narrative_ontology:cs_reading_relation('c23f781e-2b71-4862-86a2-f0e82a91bfe9', postwar_constitutionalism__german_basic_law, coexists_with).
narrative_ontology:cs_reading_relation('c23f781e-2b71-4862-86a2-f0e82a91bfe9', postwar_constitutionalism__japanese_constitution_1947, coexists_with).
narrative_ontology:cs_axiom('c23f781e-2b71-4862-86a2-f0e82a91bfe9', foundational, caste_hierarchy_suppressible_by_text).
narrative_ontology:cs_axiom_status(caste_hierarchy_suppressible_by_text, holdable).
narrative_ontology:cs_axiom_grounding('c23f781e-2b71-4862-86a2-f0e82a91bfe9', caste_hierarchy_suppressible_by_text, deontological).
narrative_ontology:cs_axiom('c23f781e-2b71-4862-86a2-f0e82a91bfe9', foundational, state_redistribution_corrects_inherited_inequality).
narrative_ontology:cs_axiom_status(state_redistribution_corrects_inherited_inequality, holdable).
narrative_ontology:cs_axiom_grounding('c23f781e-2b71-4862-86a2-f0e82a91bfe9', state_redistribution_corrects_inherited_inequality, instrumental).
narrative_ontology:cs_reference_frame('c23f781e-2b71-4862-86a2-f0e82a91bfe9', constituent_assembly_sovereignty).
narrative_ontology:cs_drift_state('c23f781e-2b71-4862-86a2-f0e82a91bfe9', contemporary_enforcement_plateau, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c23f781e-2b71-4862-86a2-f0e82a91bfe9', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(postwar_constitutionalism__indian_constitution_1950, postwar_constitutionalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(postwar_constitutionalism__indian_constitution_1950, historically_subordinated_castes).
narrative_ontology:constraint_beneficiary(postwar_constitutionalism__indian_constitution_1950, scheduled_castes_and_tribes).
narrative_ontology:constraint_beneficiary(postwar_constitutionalism__indian_constitution_1950, religious_minorities).
narrative_ontology:constraint_victim(postwar_constitutionalism__indian_constitution_1950, brahminical_hierarchy_beneficiaries).
narrative_ontology:constraint_victim(postwar_constitutionalism__indian_constitution_1950, landed_upper_caste_elites).
narrative_ontology:constraint_victim(postwar_constitutionalism__indian_constitution_1950, feudal_power_structures).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SCHEDULED CASTES/TRIBES (SNARE WITHIN INTENDED LIBERATION) — Formally liberated by the text but structurally trapped by millennia of cultural suppression, economic dispossession, and enforcement failures. The Constitution enumerates abolition and reservations but cannot unilaterally override inherited hierarchies embedded in local practice, land ownership, ritual authority, and social enforcement. Experiences the constraint as simultaneously liberatory (the document is on their side) and extractive (the document's enforcement remains contingent on political will they do not control). High suppression persists despite the text.
constraint_indexing:constraint_classification(postwar_constitutionalism__indian_constitution_1950, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INTERMEDIATE CASTES/URBAN PROFESSIONALS (TANGLED ROPE) — Experience the Constitution as both coordinating their upward mobility through reservation systems and extracting from them through merit-based competition and caste-based affirmative action targeting. Constrained by implementation politics and economic dependence on state employment. Experience genuine coordination (the Constitution enables education, mobility, governance participation) alongside extraction (they bear the zero-sum cost of downward mobility pressure from upper castes and upward pressure from scheduled castes claiming reservations).
constraint_indexing:constraint_classification(postwar_constitutionalism__indian_constitution_1950, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSTITUTIONAL AUTHORITY / STATE APPARATUS (ROPE) — The document itself and the institutions charged with implementing it (courts, legislatures, bureaucracy) experience the Constitution as a pure coordination mechanism: it provides the legal framework for governing a diverse, hierarchical society; enables multiple communities to participate in the political process; coordinates resource allocation through reservations and affirmative action. No extraction experienced at this level — the state benefits from the legitimacy of constitutionalism and the functional cooperation the document facilitates. Zero-sum costs (to upper-caste elites) are not experienced by the state as extraction FROM the state, but as coordination OF resource distribution.
constraint_indexing:constraint_classification(postwar_constitutionalism__indian_constitution_1950, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: BRAHMINICAL/UPPER-CASTE ELITES (SNARE) — Experience the Constitution as pure extraction and suppression of their inherited privileges. Legally prohibited from caste-based discrimination, economically displaced by land reform and reservation systems, culturally delegitimized by the text's repudiation of ritual hierarchy. Constrained exit: they cannot renounce Indian citizenship or the constitutional order (which is legally binding), but they experience it as fundamentally extractive of their structural position. High suppression: their traditional authority mechanisms (ritual jurisdiction, land monopoly, marriage rules, occupational control) are directly prohibited by the enumerated text.
constraint_indexing:constraint_classification(postwar_constitutionalism__indian_constitution_1950, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: URBAN UPPER-CASTE PROFESSIONALS (TANGLED ROPE) — Over generational timescale, this agent benefits from the Constitution's coordination function (rule of law, property rights, educational access) while experiencing localized extraction through affirmative action policies and caste-based mobility pressure. Mobile exit is available (emigration, private-sector careers, investment abroad) but costly. The constraint is experienced as a mix: genuine coordination enabling the market economy and professional services, plus targeted extraction through preferential policies. This is not snare-level extraction because structural mobility exists and the coordination function is real.
constraint_indexing:constraint_classification(postwar_constitutionalism__indian_constitution_1950, tangled_rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: GLOBAL POSTWAR CONSTITUTIONAL FRAMEWORK (ROPE) — From the perspective of the international legal order and postwar constitutionalism paradigm, the Indian Constitution is a pure coordination mechanism: it demonstrates that postwar social-revolutionary constitutionalism can be implemented at subcontinental scale; it establishes rule-of-law legitimacy for a new independent state; it coordinates international recognition and participation in the UN system. Zero extraction experienced at this level — the global order benefits from Indian constitutionalism as a successful model.
constraint_indexing:constraint_classification(postwar_constitutionalism__indian_constitution_1950, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a fully analytical position, the Indian Constitution is structurally a tangled rope: it coordinates a diverse polity under rule of law (rope function) while simultaneously redirecting extraction from the old hierarchy toward institutional enforcement of new hierarchies (tangled with snare dynamics). The text attempts to invert the extraction vector — from upper-caste extraction of lower-caste labor/dignity to state extraction of upper-caste privilege for redistribution. Success of this inversion is partial and contingent on enforcement will. The constraint exhibits both genuine coordination (enabling democratic participation, property protection, pluralism) and embedded extraction (implementation failures, localized suppression persistence, elite capture of reservation mechanisms).
constraint_indexing:constraint_classification(postwar_constitutionalism__indian_constitution_1950, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(postwar_constitutionalism__indian_constitution_1950_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(postwar_constitutionalism__indian_constitution_1950, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(postwar_constitutionalism__indian_constitution_1950, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(postwar_constitutionalism__indian_constitution_1950, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(postwar_constitutionalism__indian_constitution_1950_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The Constitution redirects extraction from the old caste hierarchy (where upper castes extracted labor, dignity, and ritual service from lower castes) toward the new state apparatus (which extracts via taxation, labor conscription, compliance with law, participation in bureaucratic hierarchies). This is not pure extraction because the redistribution is intentional and coordinated — the document explicitly specifies that extraction should be redirected toward correcting inherited inequality. However, extractiveness remains substantial because enforcement of the new extraction vector is contingent on political will, and the document itself cannot overcome the deep institutional embeddedness of the old hierarchy. The intermediate estimate (0.58 rather than 0.75) reflects the genuine coordination function: the Constitution enables participation, property protection, and pluralism that did not exist under pure colonial hierarchy. Suppression (0.72): High. The document must suppress multiple mechanisms simultaneously: ritual authority (caste-based discrimination prohibited), economic mechanisms (land reform, reservations), legal mechanisms (constitution-as-supreme-law overrides customary law), social mechanisms (electoral participation circumvents traditional authority). Initial suppression requirement was 0.88 (1950) because the legal/textual suppression was new and lacked institutional precedent. Over time, suppression has declined slightly (0.72 by 2005) as some inherited hierarchies have been normalized into new institutional forms (intermediate-caste intermediate castes through affirmative action, bureaucratic gatekeeping replacing ritual gatekeeping). Theater ratio (0.48): Moderate. The Constitution is not purely performative — courts actually implement it, land reforms actually redistribute property (albeit partially), reservations actually change occupational access. But implementation contains significant theater: court proceedings that legitimize the system without implementing full enforcement, legislative debates that reaffirm principles without full budget allocation, electoral participation that gives voice without full power redistribution. The theater has increased slightly over time (0.35 in 1950 to 0.48 by 2005) as implementation has routinized and performative elements have become more standardized.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap on this constraint is extraordinarily large. The scheduled castes experience the Constitution as simultaneously liberatory and extractive — they are the intended beneficiaries but trapped by enforcement failures and persistence of inherited hierarchies. The upper-caste elites experience it as pure snare — their inherited privileges are directly suppressed and they have no real exit. The urban professionals experience it as tangled rope — real coordination benefits alongside targeted extraction. The state apparatus experiences it as pure rope — coordination enabling governance without experiencing extraction. The global perspective experiences it as rope — a successful model of postwar constitutionalism. The analytical perspective sees tangled rope — genuine coordination coexisting with embedded extraction contingent on enforcement will. This gap reveals that the classification depends entirely on position: the same constitutional text is liberatory, extractive, coordinating, and suppressive depending on the agent's structural relationship to the old hierarchy, the new state, and the enforcement apparatus.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) derives from the agent's structural position relative to the Constitution's extraction-redirection mechanism. Scheduled castes (d ≈ 0.95): intended beneficiaries but trapped — high d because they are the text's nominally favored agents but lack control of enforcement. Their structural position is inverted: they are beneficiaries in intention but victims in enforcement contingency. Upper-caste elites (d ≈ 0.85): powerful but constrained — their traditional extraction mechanisms are directly prohibited; they experience high extraction of privilege. Intermediate castes (d ≈ 0.55): both beneficiaries and victims — they benefit from coordination and education access but bear costs of competition and affirmative action pressure. Urban professionals (d ≈ 0.45): powerful with mobile exit — they experience moderate extraction but have alternative career paths (emigration, private sector). State apparatus (d ≈ 0.15): beneficiary with arbitrage options — the Constitution enables their authority and they benefit from the legitimacy it provides. Global perspective (d ≈ 0.20): institutional beneficiary — postwar constitutionalism legitimizes the international order. The schema enforces that these directionality differences should produce different effective extractiveness (χ) calculations at different perspectives, which they do: powerless agents experience higher χ, institutional beneficiaries experience lower or negative χ.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The Indian Constitution exemplifies how tangled-rope classification prevents misclassification as either pure rope (missing the extraction-redirection mechanism) or pure snare (missing the coordination function). The rope reading is tempting — the Constitution does establish a functioning democratic system, rule of law, and pluralistic participation. The snare reading is also tempting — it suppresses inherited privileges, extracts compliance with new hierarchies, and maintains state control through law. The tangled-rope classification insists on holding both: the Constitution genuinely coordinates a diverse polity under rule of law (rope function) while simultaneously redirecting extraction from the old hierarchy toward the new state's redistributive mandate (snare function embedded in coordination). The mandatrophy is resolved by recognizing that the classification is not uniform across perspectives: upper-caste elites experience snare, scheduled castes experience both snare (enforcement contingency) and rope (liberatory intent), the state experiences rope, and the analytical perspective captures the tangled structure. The classification is tangled-rope at the analytical level because the base properties show both genuine coordination (theater_ratio 0.48, not extreme) and genuine extraction (suppression 0.72 of inherited mechanisms, extractiveness 0.58 directed toward state redistribution).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_will_contingency,
    'Does the Constitution''s liberatory intent depend on political will outside the document itself, or does the written text constitute sufficient enforcement mechanism?',
    'Historical analysis of implementation: correlation between constitutional clarity and actual enforcement; identification of periods where enforcement will was present vs absent; measurement of outcomes (SC/ST advancement rates, caste-based discrimination prosecutions, land reform implementation) against constitutional guarantees',
    'If enforcement contingent on will: the constraint''s extractiveness is contingent on coalition politics, and the suppression metric must account for enforcement variability. If text is self-enforcing: extractiveness drops and suppression is lower (courts will implement).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_will_contingency, empirical, 'Whether constitutional enforcement depends on political will outside the text').

omega_variable(
    inherited_hierarchy_depth,
    'Can a document alone suppress social hierarchies embedded in millennia of practice, ritual authority, and cultural reproduction?',
    'Ethnographic and sociological studies of caste practice post-1950; measurement of ritual hierarchy persistence despite legal abolition; analysis of effectiveness of land reform, education access, and occupational mobility relative to constitutional intent',
    'If hierarchies persist regardless of text: the Constitution is performative (theater_ratio higher, actual suppression lower). If text reshapes practice over generational timescale: suppression is real but delayed (confirm current metric of 0.72).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inherited_hierarchy_depth, empirical, 'Suppression depth of inherited caste hierarchy by constitutional text alone').

omega_variable(
    extraction_vector_inversion,
    'Does the Constitution successfully invert the extraction vector from upper-caste extraction to state-enforced redistribution, or does it embed new extraction mechanisms (bureaucratic predation, capture of affirmative action by intermediate elites)?',
    'Analysis of who benefits from reservation systems over time; measurement of wealth/income redistribution from upper castes to scheduled castes; identification of intermediate-elite capture mechanisms (backward classes misrepresentation, creamy-layer gaming); comparison of aggregate zero-sum transfers pre- vs post-Constitution',
    'If inversion is successful: the constraint is truly tangled rope (coordination + redirected extraction). If new extraction emerges (bureaucratic predation, elite capture): the constraint drifts toward snare at institutional level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vector_inversion, empirical, 'Success of constitutional extraction vector inversion from hierarchy to state redistribution').

omega_variable(
    reading_foreclosure_test,
    'Does the Indian Constitution foreclose the German Basic Law''s paradigm of militant democracy entrenching human dignity beyond amendment? Or can both readings coexist?',
    'Textual analysis: German Basic Law prohibits amendment of human dignity (Art. 79(3)); Indian Constitution permits amendment of all provisions except the basic structure (per Supreme Court doctrine). Test: can a single postwar constitutionalism paradigm accommodate both unamendable dignity (Germany) and amendable fundamentals (India)?',
    'If foreclosed: the readings are incompatible — one reading must be rejected. If coexist: both are valid instantiations of postwar constitutionalism with different commitments to constitutional entrenchment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_test, conceptual, 'Whether this reading forecloses the German unamendability paradigm').

omega_variable(
    imposed_vs_indigenous_legitimacy,
    'Is the Indian Constitution''s legitimacy grounded in indigenous constituent assembly authorship (Ambedkar, Nehru, the drafting process) or in postwar international constitutionalism paradigm imposed by decolonization context?',
    'Historical analysis of drafting process autonomy; identification of international vs indigenous intellectual sources; measurement of constituent assembly independence from British governance legacy; comparison with Japanese imposed constitution (where imposer is more explicit)',
    'If indigenous: the Constitution''s authority is self-grounding (lineage from Indian constituent power). If imposed: the constraint exhibits false indigenous legitimacy (actually derivative from postwar paradigm).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imposed_vs_indigenous_legitimacy, conceptual, 'Legitimacy source: indigenous constituent assembly vs postwar international paradigm').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(postwar_constitutionalism__indian_constitution_1950, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(indcon_theater_1950, postwar_constitutionalism__indian_constitution_1950, theater_ratio, 0, 0.35).
narrative_ontology:measurement(indcon_theater_1965, postwar_constitutionalism__indian_constitution_1950, theater_ratio, 15, 0.42).
narrative_ontology:measurement(indcon_theater_1985, postwar_constitutionalism__indian_constitution_1950, theater_ratio, 35, 0.48).
narrative_ontology:measurement(indcon_theater_2005, postwar_constitutionalism__indian_constitution_1950, theater_ratio, 55, 0.48).

% Extraction over time
narrative_ontology:measurement(indcon_extractiveness_1950, postwar_constitutionalism__indian_constitution_1950, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(indcon_extractiveness_1965, postwar_constitutionalism__indian_constitution_1950, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(indcon_extractiveness_1985, postwar_constitutionalism__indian_constitution_1950, base_extractiveness, 35, 0.64).
narrative_ontology:measurement(indcon_extractiveness_2005, postwar_constitutionalism__indian_constitution_1950, base_extractiveness, 55, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(indcon_suppression_1950, postwar_constitutionalism__indian_constitution_1950, suppression_requirement, 0, 0.88).
narrative_ontology:measurement(indcon_suppression_1965, postwar_constitutionalism__indian_constitution_1950, suppression_requirement, 15, 0.82).
narrative_ontology:measurement(indcon_suppression_1985, postwar_constitutionalism__indian_constitution_1950, suppression_requirement, 35, 0.74).
narrative_ontology:measurement(indcon_suppression_2005, postwar_constitutionalism__indian_constitution_1950, suppression_requirement, 55, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(postwar_constitutionalism__indian_constitution_1950, enforcement_mechanism).
narrative_ontology:affects_constraint(postwar_constitutionalism__indian_constitution_1950, postwar_constitutionalism__german_basic_law).
narrative_ontology:affects_constraint(postwar_constitutionalism__indian_constitution_1950, postwar_constitutionalism__japanese_constitution_1947).
narrative_ontology:affects_constraint(postwar_constitutionalism__indian_constitution_1950, caste_affirmative_action_capture).
narrative_ontology:affects_constraint(postwar_constitutionalism__indian_constitution_1950, land_reform_implementation_failure).

% DUAL FORMULATION NOTE:
% The Indian Constitution as social-revolutionary text is analytically distinct from its enforcement mechanisms (land reform, reservations, caste-discrimination courts). This story models the Constitution as a constraint on its own (the document as written, attempting suppression through text). Downstream constraints model specific implementation pathways: affirmative action capture (how intermediate elites game reservation systems) and land reform (how redistribution actually occurs or fails). The network links show that constitutional text influences but does not determine these downstream constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(postwar_constitutionalism__indian_constitution_1950, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
