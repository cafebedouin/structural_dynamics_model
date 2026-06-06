% ============================================================================
% CONSTRAINT STORY: ideological_diversity_convergence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-06-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ideological_diversity_convergence, []).

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
 *   constraint_id: ideological_diversity_convergence
 *   human_readable: Ideological Diversity Convergence in Zionist Territorial Policy
 *   domain: political_history/nationalism/settler_colonialism
 *
 * SUMMARY:
 *   The ideological diversity convergence constraint describes how multiple,
 *   genuinely distinct Zionist ideologies — Labor socialism, Revisionist
 *   nationalism, Cultural Zionism, Religious Zionism — produced unified
 *   territorial maximalism despite offering different justifications. Labor
 *   Zionists justified settlement through socialist 'conquest of labor' and
 *   'facts on the ground' pragmatism. Revisionists invoked historical rights
 *   to both banks of the Jordan. Cultural Zionists emphasized Hebrew cultural
 *   revival requiring territorial base. Religious Zionists (especially
 *   post-1967) sanctified land as divine inheritance. Each ideology mobilized
 *   different constituencies: socialist kibbutzim, militant nationalist
 *   youth, secular intellectuals, religious settlers. Yet all converged on
 *   territorial expansion and suppression of partition alternatives. The
 *   constraint exhibits both genuine coordination (unifying diverse Jewish
 *   communities behind state-building) and substantial extraction (systematic
 *   displacement of Palestinian population, suppression of binational
 *   alternatives). The theater ratio (0.58) reflects that ideological
 *   justifications became increasingly performative over time — by the Oslo
 *   period, 'security' rhetoric had largely replaced ideological specificity,
 *   yet territorial expansion continued. The constraint's extractiveness
 *   increased sharply at three inflection points: 1947 partition rejection
 *   (0.45→0.58), 1967 occupation (0.58→0.65), and 1977 Likud victory
 *   formalizing settlement policy (0.65→0.67). Suppression requirement peaked
 *   post-1967 as binational alternatives were structurally foreclosed by
 *   demographic engineering.
 *
 * KEY AGENTS:
 *   - State-Building Apparatus: Primary beneficiary (institutional/arbitrage) — captures territorial control, resource access, strategic depth through ideologically diverse settlement infrastructure
 *   - Palestinian Population: Primary victim (powerless/trapped) — bears displacement, land loss, subordination regardless of which Zionist ideology dominates; no exit from convergent territorial logic
 *   - Settlement Movement: Secondary beneficiary (organized/mobile) — benefits from state support across ideological transitions; can shift justifications (socialist→nationalist→religious) while maintaining territorial gains
 *   - Partition Advocates: Secondary victim (moderate/constrained) — Zionist doves, binational proponents, territorial compromise advocates face ideological marginalization and career costs; benefit from state coordination while bearing costs of perpetual conflict
 *   - International Two-State Coalition: Organized external actors (organized/mobile) — UN agencies, peace NGOs, mediators see convergence as temporary obstacle with sunset logic; maintain agency through exit options
 *   - Labor Zionist Apparatus Post-1977: Institutional actor experiencing piton dynamics (institutional/constrained) — original ideology atrophied into performance while settlement infrastructure continues serving territorial maximalism
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both genuine coordination function (diverse constituency mobilization) and substantial extraction (systematic displacement, alternative suppression)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ideological_diversity_convergence, 0.68).
domain_priors:suppression_score(ideological_diversity_convergence, 0.75).
domain_priors:theater_ratio(ideological_diversity_convergence, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ideological_diversity_convergence, extractiveness, 0.68).
narrative_ontology:constraint_metric(ideological_diversity_convergence, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ideological_diversity_convergence, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ideological_diversity_convergence, tangled_rope).
narrative_ontology:human_readable(ideological_diversity_convergence, "Ideological Diversity Convergence in Zionist Territorial Policy").
narrative_ontology:topic_domain(ideological_diversity_convergence, "political_history/nationalism/settler_colonialism").

domain_priors:requires_active_enforcement(ideological_diversity_convergence).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ideological_diversity_convergence, '7ef54534-15c4-4f52-91f7-33ad343154e5').
narrative_ontology:cs_kernel_codification('7ef54534-15c4-4f52-91f7-33ad343154e5', distributed).
narrative_ontology:cs_authority_grounding('7ef54534-15c4-4f52-91f7-33ad343154e5', lineage).
narrative_ontology:cs_interpretation_layer_present('7ef54534-15c4-4f52-91f7-33ad343154e5').
narrative_ontology:cs_reading_relation('7ef54534-15c4-4f52-91f7-33ad343154e5', ideological_diversity_convergence__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('7ef54534-15c4-4f52-91f7-33ad343154e5', ideological_diversity_convergence__religious_restoration_reading, influences).
narrative_ontology:cs_axiom('7ef54534-15c4-4f52-91f7-33ad343154e5', foundational, persecution_justifies_territorial_sovereignty).
narrative_ontology:cs_axiom_status(persecution_justifies_territorial_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('7ef54534-15c4-4f52-91f7-33ad343154e5', persecution_justifies_territorial_sovereignty, empirically_contingent).
narrative_ontology:cs_axiom('7ef54534-15c4-4f52-91f7-33ad343154e5', foundational, historical_presence_establishes_territorial_right).
narrative_ontology:cs_axiom_status(historical_presence_establishes_territorial_right, holdable).
narrative_ontology:cs_axiom_grounding('7ef54534-15c4-4f52-91f7-33ad343154e5', historical_presence_establishes_territorial_right, conventional).
narrative_ontology:cs_axiom('7ef54534-15c4-4f52-91f7-33ad343154e5', secondary, self_determination_overrides_existing_population_claims).
narrative_ontology:cs_axiom_status(self_determination_overrides_existing_population_claims, holdable).
narrative_ontology:cs_axiom_grounding('7ef54534-15c4-4f52-91f7-33ad343154e5', self_determination_overrides_existing_population_claims, deontological).
narrative_ontology:cs_reference_frame('7ef54534-15c4-4f52-91f7-33ad343154e5', historical_right_to_ancestral_homeland).
narrative_ontology:cs_drift_state('7ef54534-15c4-4f52-91f7-33ad343154e5', post_1967_territorial_maximalism, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7ef54534-15c4-4f52-91f7-33ad343154e5', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ideological_diversity_convergence, state_building_apparatus).
narrative_ontology:constraint_beneficiary(ideological_diversity_convergence, settlement_movement).
narrative_ontology:constraint_beneficiary(ideological_diversity_convergence, territorial_maximalist_coalition).
narrative_ontology:constraint_victim(ideological_diversity_convergence, palestinian_territorial_claims).
narrative_ontology:constraint_victim(ideological_diversity_convergence, palestinian_population).
narrative_ontology:constraint_victim(ideological_diversity_convergence, partition_advocates).
narrative_ontology:constraint_victim(ideological_diversity_convergence, binational_vision_proponents).
narrative_ontology:constraint_vindicates(ideological_diversity_convergence, territorial_maximalism_doctrine).
narrative_ontology:constraint_vindicates(ideological_diversity_convergence, land_redemption_ideology).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PALESTINIAN POPULATION (SNARE) — Trapped by the convergent territorial logic regardless of which Zionist ideology dominates. No exit from the structural displacement mechanism. All ideological variants — Labor's 'facts on the ground,' Revisionist maximalism, Religious Zionist sanctification — produce the same territorial outcome. Maximum extraction: land loss, displacement, subordination. The ideological diversity is theater from this position; the extraction mechanism is unified.
constraint_indexing:constraint_classification(ideological_diversity_convergence, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PARTITION ADVOCATES (TANGLED ROPE) — Constrained by the convergent logic but not entirely powerless. Benefit from state-building coordination (security, institutions, international recognition) while bearing costs of territorial maximalism (perpetual conflict, moral compromise, international isolation). Could advocate for territorial compromise but face career costs, ideological marginalization, and accusations of betrayal. The constraint coordinates state-building while extracting via suppression of partition alternatives.
constraint_indexing:constraint_classification(ideological_diversity_convergence, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE-BUILDING APPARATUS (ROPE) — Primary beneficiary. The ideological convergence solves a genuine coordination problem: unifying diverse factions (socialist kibbutzim, revisionist militants, religious settlers, secular nationalists) behind territorial expansion. Each ideology provides different constituencies and justifications for the same policy. Net beneficiary: territorial control, resource access, demographic engineering, strategic depth. Experiences the constraint as functional coordination enabling state consolidation.
constraint_indexing:constraint_classification(ideological_diversity_convergence, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL TWO-STATE COALITION (SCAFFOLD) — Organized actors (UN agencies, peace NGOs, international mediators) see the convergence as a temporary obstacle to inevitable partition. Sunset logic: demographic realities, international law norms, unsustainability of occupation will force territorial compromise. Mobile exit: can redirect advocacy efforts to other conflicts if this one proves intractable. Low effective extraction because they maintain agency and see a resolution path, even if distant.
constraint_indexing:constraint_classification(ideological_diversity_convergence, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LABOR ZIONIST APPARATUS POST-1977 (PITON) — The original Labor Zionist ideology (socialist settlement, 'Hebrew labor,' partition pragmatism) has atrophied into performance. After losing power in 1977, Labor maintains ideological rhetoric about peace and partition while its settlement infrastructure (kibbutzim, moshavim, land acquisition mechanisms) continues serving territorial maximalism. The ideology persists through institutional inertia and identity maintenance, not functional coordination. Theater ratio reflects this: the socialist justification is vestigial, but the territorial outcome remains.
constraint_indexing:constraint_classification(ideological_diversity_convergence, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global perspective, the constraint exhibits both genuine coordination (unifying diverse Jewish communities behind state-building) and substantial extraction (systematic displacement of Palestinian population). The ideological diversity is not mere theater — each ideology genuinely mobilizes different constituencies and provides distinct legitimation — but the convergence on territorial maximalism reveals structural extraction beneath the coordination function. The constraint requires active enforcement (suppression of partition alternatives, delegitimization of binationalism) to maintain the convergence.
constraint_indexing:constraint_classification(ideological_diversity_convergence, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ideological_diversity_convergence_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ideological_diversity_convergence, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ideological_diversity_convergence, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ideological_diversity_convergence, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ideological_diversity_convergence, TR),
    TR >= 0.70.

:- end_tests(ideological_diversity_convergence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): Substantial. The constraint extracts systematically from Palestinian territorial claims through convergent expansion logic, and from partition advocates within the Zionist movement through suppression of alternatives. The extraction is not total (0.68 not 0.85+) because genuine coordination functions exist: the ideological diversity does mobilize different constituencies, provide legitimation flexibility, and enable coalition-building. But the coordination is inseparable from extraction — the same settlement infrastructure that coordinates diverse Jewish communities displaces Palestinian population. Suppression (0.75): High. Binational alternatives (Brit Shalom, Ihud, Mapam left) were actively suppressed through ideological delegitimization, institutional exclusion, and structural foreclosure via demographic engineering. Partition advocates face career costs and social ostracism. Palestinian resistance is met with military force. The suppression intensified post-1967 as territorial maximalism became institutionally entrenched. Theater ratio (0.58): Moderate-high. The ideological diversity is not pure theater — each ideology genuinely mobilizes different constituencies and provides distinct legitimation. But the convergence on territorial outcomes reveals performative elements: by the Oslo period, specific ideological justifications (socialist labor, revisionist history, religious sanctity) had largely collapsed into generic 'security' rhetoric, yet expansion continued. Labor Zionism post-1977 is particularly theatrical: socialist ideology persists as identity performance while serving territorial maximalism. The theater increased over the interval as ideological specificity gave way to security justification.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same structural phenomenon — ideological diversity producing territorial convergence — appears as snare, tangled rope, rope, scaffold, or piton depending on the observer's position. The Palestinian population sees pure extraction (snare): the ideological diversity is irrelevant theater; all variants produce displacement. Partition advocates see mixed coordination and extraction (tangled rope): they benefit from state-building while bearing costs of maximalism. The state apparatus sees functional coordination (rope): ideological diversity solves the genuine problem of unifying diverse factions. International mediators see a temporary obstacle with sunset logic (scaffold): demographic realities will force compromise. The Labor Zionist apparatus sees its own degraded ritual (piton): socialist ideology persists as performance while serving territorial outcomes. The analytical observer sees tangled rope: genuine coordination (constituency mobilization) inseparable from substantial extraction (displacement, alternative suppression). The perspectival gap is not a measurement error — it is the constraint's structure. Each perspective is valid from its structural position. The mandatrophy is resolved by recognizing that the presheaf over observation sites IS the answer, not any single type.
 *
 * DIRECTIONALITY LOGIC:
 *   The state-building apparatus is the primary beneficiary: it captures territorial control, resource access, and strategic depth through the convergent settlement infrastructure. Directionality flows toward this agent (low d, negative or low chi) — the constraint subsidizes state consolidation. The Palestinian population is the primary victim: directionality flows away from this agent (high d, high chi) — trapped exit options and powerless status amplify extraction to maximum. Partition advocates within the Zionist movement experience mixed directionality: they benefit from state coordination (security, institutions, international recognition) but bear costs of territorial maximalism (perpetual conflict, moral compromise, international isolation). Their moderate power and constrained exit produce intermediate chi — substantial extraction but not maximal. The settlement movement benefits across ideological transitions: mobile exit options (can shift justifications) and organized power produce low chi — they experience the constraint as coordination enabling their project. International actors maintain low chi through mobile exit and organized power — they can redirect efforts if this conflict proves intractable. The Labor Zionist apparatus post-1977 experiences the constraint as degraded coordination (piton) — constrained exit and institutional power produce moderate chi, but the classification derives from theater gate rather than extraction level. The analytical observer's tangled rope classification reflects both coordination function (genuine) and extraction mechanism (substantial) — the constraint requires active enforcement to maintain convergence, confirming the tangled rope structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that coordination and extraction are not mutually exclusive — they are structurally entangled in this case. The ideological diversity genuinely coordinates: Labor mobilizes socialist kibbutzim, Revisionism mobilizes nationalist militants, Religious Zionism mobilizes settlers, Cultural Zionism mobilizes secular intellectuals. Each provides different legitimation for international audiences. This is real coordination solving a real collective action problem (unifying diverse Jewish communities behind state-building). Simultaneously, the convergence extracts: Palestinian territorial claims are systematically displaced, binational alternatives are suppressed, partition advocates are marginalized. The extraction is not incidental to the coordination — it is constitutive. The settlement infrastructure that coordinates diverse constituencies IS the mechanism that displaces Palestinian population. The constraint cannot be decomposed into 'coordination part' and 'extraction part' — they are the same structure viewed from different positions. This is the tangled rope's defining feature: BOTH coordination function AND asymmetric extraction, requiring active enforcement to hold. The analytical observer's tangled rope classification is not a compromise or average — it is the structurally accurate type when coordination and extraction are inseparable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_partition_acceptance,
    'Would acceptance of the 1947 UN Partition Plan have produced a structurally different constraint, or would internal territorial pressures have generated convergent expansion regardless?',
    'Historical analysis of territorial dynamics in accepted partition scenarios (India-Pakistan, Ireland); examination of Zionist movement''s internal territorial debates 1937-1947; assessment of whether ''facts on the ground'' logic was contingent on rejection or structural to the movement',
    'If partition acceptance would have stabilized borders: the convergence is contingent on conflict dynamics (lower extractiveness, more rope-like). If internal pressures would have driven expansion anyway: the convergence is structural to the ideological framework (higher extractiveness, more snare-like).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_partition_acceptance, conceptual, 'Whether partition acceptance would have prevented territorial convergence').

omega_variable(
    ideological_diversity_functionality,
    'Is the ideological diversity genuinely functional (mobilizing different constituencies, providing legitimation flexibility) or primarily theatrical (masking unified extraction)?',
    'Analysis of policy divergence vs convergence across ideological periods; examination of whether ideology predicts territorial policy or merely justifies it post-hoc; assessment of whether ideological transitions (Labor to Likud 1977, secular to religious post-1967) changed territorial outcomes',
    'If functional: tangled rope classification confirmed (genuine coordination + extraction). If theatrical: reclassify toward snare (extraction with coordination cover story).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ideological_diversity_functionality, empirical, 'Whether ideological diversity provides genuine coordination function').

omega_variable(
    religious_zionist_necessity,
    'Was the post-1967 rise of Religious Zionism necessary for territorial maximalism, or did it merely provide new justification for pre-existing secular territorial logic?',
    'Comparison of territorial expansion patterns pre- and post-1967; analysis of settlement location decisions (strategic vs religiously significant sites); examination of whether secular Labor settlements (1948-1967) followed different territorial logic than religious settlements (post-1967)',
    'If Religious Zionism was necessary: the constraint''s character changed substantially in 1967 (two distinct constraints). If it merely re-justified existing logic: the constraint is continuous with stable extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_zionist_necessity, empirical, 'Whether Religious Zionism changed or merely re-justified territorial logic').

omega_variable(
    binational_suppression_mechanism,
    'What specific mechanisms suppressed binational alternatives (Brit Shalom, Ihud, Mapam left wing) — ideological delegitimization, institutional exclusion, or structural impossibility given demographic engineering?',
    'Historical analysis of binational movement decline; examination of whether suppression was active (exclusion from coalitions, funding denial, social ostracism) or passive (demographic facts making binationalism unviable); assessment of whether suppression intensity varied by ideological period',
    'If active suppression: higher suppression metric, confirms tangled rope (enforcement required). If passive structural impossibility: lower suppression, more rope-like (coordination without coercion).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(binational_suppression_mechanism, empirical, 'Whether binational alternative suppression was active or structural').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ideological_diversity_convergence, 0, 104).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(idc_theater_1920, ideological_diversity_convergence, theater_ratio, 0, 0.35).
narrative_ontology:measurement(idc_theater_1947, ideological_diversity_convergence, theater_ratio, 27, 0.42).
narrative_ontology:measurement(idc_theater_1967, ideological_diversity_convergence, theater_ratio, 47, 0.48).
narrative_ontology:measurement(idc_theater_1977, ideological_diversity_convergence, theater_ratio, 57, 0.55).
narrative_ontology:measurement(idc_theater_1993, ideological_diversity_convergence, theater_ratio, 73, 0.58).
narrative_ontology:measurement(idc_theater_2024, ideological_diversity_convergence, theater_ratio, 104, 0.58).

% Extraction over time
narrative_ontology:measurement(idc_extract_1920, ideological_diversity_convergence, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(idc_extract_1947, ideological_diversity_convergence, base_extractiveness, 27, 0.58).
narrative_ontology:measurement(idc_extract_1967, ideological_diversity_convergence, base_extractiveness, 47, 0.65).
narrative_ontology:measurement(idc_extract_1977, ideological_diversity_convergence, base_extractiveness, 57, 0.67).
narrative_ontology:measurement(idc_extract_1993, ideological_diversity_convergence, base_extractiveness, 73, 0.68).
narrative_ontology:measurement(idc_extract_2024, ideological_diversity_convergence, base_extractiveness, 104, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(idc_suppress_1920, ideological_diversity_convergence, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(idc_suppress_1947, ideological_diversity_convergence, suppression_requirement, 27, 0.55).
narrative_ontology:measurement(idc_suppress_1967, ideological_diversity_convergence, suppression_requirement, 47, 0.7).
narrative_ontology:measurement(idc_suppress_1977, ideological_diversity_convergence, suppression_requirement, 57, 0.75).
narrative_ontology:measurement(idc_suppress_1993, ideological_diversity_convergence, suppression_requirement, 73, 0.75).
narrative_ontology:measurement(idc_suppress_2024, ideological_diversity_convergence, suppression_requirement, 104, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ideological_diversity_convergence, identity_coordination).
narrative_ontology:affects_constraint(ideological_diversity_convergence, partition_plan_rejection_1947).
narrative_ontology:affects_constraint(ideological_diversity_convergence, settlement_enterprise_expansion).
narrative_ontology:affects_constraint(ideological_diversity_convergence, oslo_process_structural_asymmetry).

% DUAL FORMULATION NOTE:
% The ideological diversity convergence is upstream of specific territorial decisions (1947 partition rejection, post-1967 settlement expansion, Oslo territorial asymmetry) but represents a distinct structural constraint. The downstream constraints have their own extractiveness values reflecting specific policy choices; this constraint's extractiveness reflects the meta-level convergence mechanism that made those choices structurally overdetermined across ideological transitions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
