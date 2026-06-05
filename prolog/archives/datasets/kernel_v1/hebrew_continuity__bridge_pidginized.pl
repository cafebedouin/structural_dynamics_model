% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__bridge_pidginized
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__bridge_pidginized, []).

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
 *   constraint_id: hebrew_continuity__bridge_pidginized
 *   human_readable: Hebrew as Diaspora Contact Language: Bridge Pidginized Reading
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   The bridge-pidginized reading of Hebrew continuity describes the
 *   constraint that emerges when Hebrew functions as a contact language for
 *   diaspora Jewish coordination in the absence of native speaker contexts
 *   and against the backdrop of institutional framing that valorizes both
 *   liturgical authenticity and modern utility. This reading occupies a
 *   structural middle ground: diaspora communities cannot access Hebrew as a
 *   fully native language (separation from Israel, intergenerational
 *   discontinuity, host-language dominance) and do not engage it primarily as
 *   a liturgical register (prayer and sacred study are typically conducted in
 *   translation or transliteration, or code-switch between Hebrew and host
 *   language). Instead, Hebrew becomes a pidginized bridge — sparse,
 *   high-register in institutional contexts, marketplace-practical in actual
 *   use, carrying identity-load while lacking native competence. This reading
 *   is one of three competing framings of the same kernel (Hebrew
 *   continuity): the liturgical_preservation reading sees diaspora Hebrew
 *   through its sacred continuity function; the native_generative reading
 *   sees it as a stage to be transcended through immersion and return
 *   migration; the bridge-pidginized reading treats the intermediate state
 *   itself as the structural reality that institutions manage and communities
 *   navigate. The constraint's extractiveness (0.52) reflects that
 *   institutional gatekeepers benefit from the bridge model (it legitimizes
 *   their pedagogical and standardization work), while second-generation
 *   learners bear significant identity-based suppression (forced into
 *   intermediate competence, neither native nor fully liturgical, trapped by
 *   identity fusion with 'being Hebrew-speaking Jews'). The rising
 *   theater_ratio (0.40 → 0.61) models the increasing disconnect between
 *   institutional claims (Hebrew as continuous tradition, as identity marker)
 *   and actual practice (code-switching, transliteration, instrumental use).
 *   The suppression_requirement rising from 0.42 to 0.48 reflects
 *   strengthening institutional standards around Hebrew competence even as
 *   actual diaspora practice becomes less native-generative and more
 *   pidginized.
 *
 * KEY AGENTS:
 *   - Second-Generation Diaspora Youth: Primary victim (powerless/identity_locked) — structurally mobile but identity-fused; the constraint extracts through internalized obligation to maintain Hebrew despite neither native nor liturgical competence
 *   - Diaspora Coordination Communities: Secondary victim and partial beneficiary (moderate/constrained) — communities benefit from the bridge's coordination function but suppress native-language use and bear social pressure to 'improve'
 *   - Institutional Hebraists: Primary beneficiary (institutional/arbitrage) — universities, language organizations, Zionist institutions arbitrage between liturgical and modern registers; control gatekeeping and standardization
 *   - Native Speaker Initiatives: Organized exit-pathway (organized/mobile) — ulpan, immersion, return migration; see the bridge as a temporary stage to be transcended; low extraction because they have clear functional goal and sunset logic
 *   - Liturgical Institutional Framing: Institutional theater maintainer (institutional/constrained) — traditional Jewish institutions frame Hebrew through sacred continuity while accepting pidginized reality; maintain performative authenticity
 *   - Analytical Observer: Cross-societal perspective (analytical/analytical) — risks naturalizing the bridge-pidginized outcome as inevitable diaspora ecology rather than recognizing it as a contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__bridge_pidginized, 0.52).
domain_priors:suppression_score(hebrew_continuity__bridge_pidginized, 0.48).
domain_priors:theater_ratio(hebrew_continuity__bridge_pidginized, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, extractiveness, 0.52).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__bridge_pidginized, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__bridge_pidginized, "Hebrew as Diaspora Contact Language: Bridge Pidginized Reading").
narrative_ontology:topic_domain(hebrew_continuity__bridge_pidginized, "sociolinguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_continuity__bridge_pidginized).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__bridge_pidginized, '2d7f8162-ba5e-4c39-9110-bf936a9033e2').
narrative_ontology:cs_kernel_codification('2d7f8162-ba5e-4c39-9110-bf936a9033e2', formalized).
narrative_ontology:cs_authority_grounding('2d7f8162-ba5e-4c39-9110-bf936a9033e2', lineage).
narrative_ontology:cs_interpretation_layer_present('2d7f8162-ba5e-4c39-9110-bf936a9033e2').
narrative_ontology:cs_reading_relation('2d7f8162-ba5e-4c39-9110-bf936a9033e2', hebrew_continuity__liturgical_preservation, coexists_with).
narrative_ontology:cs_reading_relation('2d7f8162-ba5e-4c39-9110-bf936a9033e2', hebrew_continuity__native_generative, coexists_with).
narrative_ontology:cs_axiom('2d7f8162-ba5e-4c39-9110-bf936a9033e2', foundational, hebrew_occupies_kernel_through_instrumental_diaspora_coordination).
narrative_ontology:cs_axiom_status(hebrew_occupies_kernel_through_instrumental_diaspora_coordination, holdable).
narrative_ontology:cs_axiom_grounding('2d7f8162-ba5e-4c39-9110-bf936a9033e2', hebrew_occupies_kernel_through_instrumental_diaspora_coordination, conventional).
narrative_ontology:cs_axiom('2d7f8162-ba5e-4c39-9110-bf936a9033e2', foundational, native_speaker_absence_in_diaspora_is_structural_not_failure).
narrative_ontology:cs_axiom_status(native_speaker_absence_in_diaspora_is_structural_not_failure, holdable).
narrative_ontology:cs_axiom_grounding('2d7f8162-ba5e-4c39-9110-bf936a9033e2', native_speaker_absence_in_diaspora_is_structural_not_failure, empirically_contingent).
narrative_ontology:cs_reference_frame('2d7f8162-ba5e-4c39-9110-bf936a9033e2', diaspora_hebrew_as_intermediate_register).
narrative_ontology:cs_drift_state('2d7f8162-ba5e-4c39-9110-bf936a9033e2', contemporary_institutional_gatekeeping, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2d7f8162-ba5e-4c39-9110-bf936a9033e2', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__bridge_pidginized, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, diaspora_coordination_agents).
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, institutional_hebraists).
narrative_ontology:constraint_victim(hebrew_continuity__bridge_pidginized, native_speaker_intergenerational_continuity).
narrative_ontology:constraint_victim(hebrew_continuity__bridge_pidginized, liturgical_register_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SECOND-GENERATION DIASPORA YOUTH (SNARE) — Structurally mobile (speak host language natively, could exit Hebrew entirely) but identity-fused with the language as marker of Jewish belonging. Hebrew is neither communicatively native nor liturgically learned; it is a pidginized bridge they cannot exit without fracturing their identity as diaspora Jews. Maximum experienced extraction: forced into an intermediate linguistic register that serves institutional coordination (Shabbat, camp, youth group) at cost of neither native competence nor liturgical depth.
constraint_indexing:constraint_classification(hebrew_continuity__bridge_pidginized, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: DIASPORA COORDINATION COMMUNITIES (TANGLED ROPE) — Hebrew as a pidginized bridge serves genuine coordination function: enables inter-diaspora communication (Argentina-Israel, France-US Jewish communities) and creates shared liturgical-adjacent ritual space. Communities benefit from this coordination but face significant suppression: members must suppress native-language dominance to participate, encounter social pressure to 'improve' Hebrew, and experience perpetual inadequacy relative to native and liturgical standards. Active enforcement: institutional valorization of 'Hebrew competence' alongside ridicule of accented or incomplete speech.
constraint_indexing:constraint_classification(hebrew_continuity__bridge_pidginized, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL HEBRAISTS (ROPE) — Benefit from the pidginized bridge model: it creates demand for Hebrew education, standardization work, and institutional legitimacy. Universities, Jewish organizations, and Zionist bodies arbitrage between liturgical authority and modern utility, positioning themselves as gatekeepers of 'correct' or 'authentic' Hebrew. Experience the constraint as coordination: the pidginized bridge is a tool they control and deploy. Net beneficiary of the extraction flow.
constraint_indexing:constraint_classification(hebrew_continuity__bridge_pidginized, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: NATIVE SPEAKER INITIATIVES (SCAFFOLD) — Organized efforts (ulpan, kibbutz immersion, Israel return migration) to convert the pidginized bridge into native generative Hebrew. These initiatives see the constraint as a temporary stage to be transcended: suppress the pidgin, elevate to native competence in Israel or diaspora immersion settings. Low effective extraction because these organized agents have a clear exit path (the 'native_generative' reading) and sunset logic: once native speakers are generated, the constraint dissolves. Theater low because the functional goal (native fluency) is clear and measurable.
constraint_indexing:constraint_classification(hebrew_continuity__bridge_pidginized, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LITURGICAL INSTITUTIONAL FRAMING (PITON) — Traditional Jewish institutions frame Hebrew as 'Lashon Hakodesh' (the Holy Tongue), presenting it through liturgical register and connecting diaspora contact-language use to sacred continuity. This framing is substantially theatrical: it valorizes Hebrew competence while the actual institutional practice (prayer services with transliteration, youth group conversations with English code-switching) accepts and relies on the pidginized bridge. The piton emerges as the traditional frame degrades under the weight of the bridge's real function — institutions must suppress the gap between liturgical framing and pidginized reality, producing inertial performance of authenticity that participants recognize as theater.
constraint_indexing:constraint_classification(hebrew_continuity__bridge_pidginized, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN CANDIDATE) — From a sociolinguistic universal perspective, all diaspora languages undergo pidginization when separated from native speaker communities and repurposed for inter-group coordination. This perspective sees the bridge-pidginized Hebrew as an immutable fact of diaspora language ecology: any minority language used for coordination without native speaker density will pidginize. The structural data (beneficiaries, suppression mechanisms, extractive gatekeeping) suggests this is a false summit — the pidginization is not a natural law but a contingent institutional choice to suppress native generative capacity while valorizing the bridge itself.
constraint_indexing:constraint_classification(hebrew_continuity__bridge_pidginized, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__bridge_pidginized_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hebrew_continuity__bridge_pidginized, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hebrew_continuity__bridge_pidginized, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_continuity__bridge_pidginized, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hebrew_continuity__bridge_pidginized, TR),
    TR >= 0.70.

:- end_tests(hebrew_continuity__bridge_pidginized_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The bridge-pidginized constraint benefits institutional hebraists (pedagogical authority, standardization work, legitimacy as gatekeepers of continuity) while imposing asymmetric burden on second-generation learners (identity-fusion suppression, perpetual inadequacy). The extraction is not as severe as a pure snare (0.66+) because the bridge does provide genuine coordination function and some participants benefit from the linguistic toolkit it provides. The measured value reflects that the beneficiaries' arbitrage (controlling when and how Hebrew is deployed) extracts value from the targets' suppression, but not catastrophically — there is genuine coordination beneath the extraction. Suppression (0.48): Moderate. Barriers to exit include social identity (being a 'Hebrew-speaking Jew' is a constitutive identity marker for diaspora youth), institutional expectations (schools, camps, youth groups demand Hebrew participation), and internalized inadequacy (the target internalizes the measuring stick — 'I don't really speak Hebrew'). But suppression is not total (0.60+) because the pidginized register is actually functional; it works for marketplace, casual conversation, and ritual contexts. Targets can and do code-switch, use English, transliterate — the suppression is of full nativeness and full liturgical depth, not of the ability to participate at all. Theater ratio (0.61): High-moderate. Institutional framing (Hebrew as tradition, continuity, identity) is substantially theatrical when measured against actual diaspora practice (code-switching, transliteration, instrumental use). The theater has risen over the measurement interval as institutions have strengthened standards around 'proper' Hebrew while actual diaspora competence has become less native-generative. The gap between framing and practice is the source of the piton perspective.
 *
 * PERSPECTIVAL GAP:
 *   The bridge-pidginized reading reveals six structurally distinct positions on what appears to be a single constraint. Second-generation youth experience snare-level extraction (identity-locked, no exit, maximum asymmetry). Diaspora communities experience tangled-rope reality (genuine coordination benefit mixed with suppression). Institutional hebraists experience rope-level coordination (they control the register and benefit from its existence). Immersion initiatives experience scaffold-level (low extraction, clear sunset, organized exit path). Traditional institutions experience piton-level (performative theater masking degradation of actual linguistic function). The analytical observer risks imposing mountain-level (inevitable diaspora ecology) when the structural data reveals institutional contingency. The perspectival gap is not disagreement about facts but structural difference in how agents are positioned relative to the constraint: beneficiaries see coordination, targets see extraction, observers risk naturalizing what is institutional choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position: institutional hebraists are beneficiaries with arbitrage options (low d, negative effective extraction), second-generation learners are victims with identity-locked exits (high d, high effective extraction), diaspora communities are mixed (moderate d, moderate extraction). The engine derives d from the beneficiary/victim declarations plus exit_modulation: beneficiaries + arbitrage exit → d ≈ 0.15 → f(d) ≈ -0.01 (negative chi, coordination perception); victims + identity_locked exit → d ≈ 0.89 → f(d) ≈ 1.28 (high chi, extraction perception); mixed agents + constrained exit → d ≈ 0.55 → f(d) ≈ 0.75 (moderate chi, hybrid perception). The spatial scope (global for coordination agents, regional for diaspora communities) scales the effective extraction upward via σ(S): global scope (1.2) amplifies institutional benefits; regional scope (0.9) moderates diaspora extraction. The directionality structure captures why the same linguistic constraint appears as coordination to beneficiaries and extraction to targets.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by refusing the binary choice between 'is this coordination or extraction?' The bridge-pidginized reading answers: it is both. It coordinates diaspora communities across geography while extracting from second-generation youth through identity-lock. It benefits institutions while suppressing targets. The mandatrophy resolution is perspectival: from the institutional beneficiary position, it is rope (coordination). From the target position, it is snare (extraction). The tangled_rope classification accepts the hybrid and measures the asymmetry: genuine coordination function (what makes it rope-like) + asymmetric extraction (what makes it snare-like) + active enforcement (what sustains it) = tangled rope. The constraint does not collapse into a single type; the presheaf of perspectives IS the constraint's structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    native_speaker_capacity_constraint,
    'Is sparse native speaker density in diaspora an immutable structural property or a policy outcome (school language, immigration selection, institutional prioritization)?',
    'Comparative analysis: diaspora communities with active native speaker cultivation (Israeli-founded schools, return migration incentives) vs. those without. Historical counterfactuals: would Yiddish-dominant communities have maintained stronger Hebrew generativity if institutions had prioritized it?',
    'If structural/immutable: the bridge-pidginized reading is inevitable, and the constraint''s extraction is coordinative overhead. If policy outcome: the reading is chosen, and the extraction is a form of institutional gatekeeping that suppresses the native_generative reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(native_speaker_capacity_constraint, empirical, 'Whether sparse native speakers reflect immutable diaspora ecology or institutional choice').

omega_variable(
    kernel_occupation_mechanism,
    'Does the bridge-pidginized reading genuinely occupy the Hebrew continuity kernel, or does it displace it by reducing Hebrew to instrumental utility while the kernel''s sacred-continuity meaning is preserved separately?',
    'Discourse analysis: how diaspora Jews themselves describe Hebrew''s role. Do they frame pidginized bridge Hebrew as ''keeping Hebrew alive'' or as ''maintaining connection to Jewishness'' (different kernel)? Institutional documentation: what continuity claims are made for diaspora contact-language Hebrew vs. liturgical vs. native registers?',
    'If genuinely occupying kernel: the three readings coexist within a single commitment framework. If displacing kernel: the bridge reading is actually occupying a different kernel (diaspora identity-coordination) while the original kernel (Hebrew continuity) is being occupied by liturgical and native readings separately.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_occupation_mechanism, conceptual, 'Whether bridge-pidginized Hebrew occupies the same continuity kernel or a different kernel altogether').

omega_variable(
    suppression_internalization_mechanism,
    'Is the measured suppression (0.48) primarily structural (institution-imposed standards, social ridicule of accented speech) or internalized (second-generation learners internalizing inadequacy despite objective communicative competence)?',
    'Longitudinal study: speakers'' self-assessed competence vs. functional communicative ability. Exit trajectories: do speakers who exit diaspora communities retain or shed internalized suppression regarding their Hebrew? Comparison with other contact languages: is suppression of this magnitude typical or distinctive?',
    'If structural: suppression reduces when institutional standards relax (e.g., spaces that normalize code-switching). If internalized: suppression persists after institutional barriers are removed, indicating the constraint is partially sustained by the target''s own identity frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Proportion of suppression that is structural versus internalized in diaspora Hebrew speakers').

omega_variable(
    reading_boundary_specificity,
    'What demarcates the bridge-pidginized reading from the native_generative and liturgical_preservation readings in actual diaspora practice? Where is the boundary linguistically and socially?',
    'Linguistic analysis: phonological, morphosyntactic, and pragmatic features marking each register. Institutional discourse: how do institutions distinguish ''good'' Hebrew (which reading''s standard), and do they enforce the boundary? Diaspora speaker self-categorization: do speakers see themselves as occupying one reading or multiple?',
    'If the boundary is sharp: the three readings are indeed distinct constraints. If fuzzy or overlapping: the constraint may be misframed, and the reading decomposition needs refinement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_boundary_specificity, empirical, 'Linguistic and social boundaries distinguishing bridge-pidginized Hebrew from native and liturgical registers').

omega_variable(
    institutional_enforcement_persistence,
    'Do diaspora Hebrew institutions actively enforce the bridge-pidginized register, or does it emerge passively from the absence of native speaker contexts?',
    'Ethnographic documentation: institutional curricula and their standards. Comparative analysis: does the pidginized bridge persist in contexts where native speakers are available (e.g., Israel-diaspora comparison)? Historical analysis: did earlier diaspora generations (with more Yiddish dominance) develop different Hebrew registers?',
    'If actively enforced: the ''requires_active_enforcement'' flag is justified, and institutions are sustaining the constraint. If passive emergence: enforcement may be lower, and the constraint is structurally different from what the tangled_rope classification assumes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_enforcement_persistence, empirical, 'Whether diaspora institutions actively enforce the bridge-pidginized register or it emerges passively').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__bridge_pidginized, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_pidg_tr_t0, hebrew_continuity__bridge_pidginized, theater_ratio, 0, 0.4).
narrative_ontology:measurement(hebr_pidg_tr_t20, hebrew_continuity__bridge_pidginized, theater_ratio, 20, 0.55).
narrative_ontology:measurement(hebr_pidg_tr_t40, hebrew_continuity__bridge_pidginized, theater_ratio, 40, 0.61).

% Extraction over time
narrative_ontology:measurement(hebr_pidg_be_t0, hebrew_continuity__bridge_pidginized, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(hebr_pidg_be_t20, hebrew_continuity__bridge_pidginized, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(hebr_pidg_be_t40, hebrew_continuity__bridge_pidginized, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(hebr_pidg_su_t0, hebrew_continuity__bridge_pidginized, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(hebr_pidg_su_t20, hebrew_continuity__bridge_pidginized, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(hebr_pidg_su_t40, hebrew_continuity__bridge_pidginized, suppression_requirement, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__bridge_pidginized, identity_coordination).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, hebrew_liturgical_authenticity).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, diaspora_language_shift).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, jewish_institutional_gatekeeping).

% DUAL FORMULATION NOTE:
% The Hebrew continuity kernel decomposes into three structurally distinct constraint stories: liturgical_preservation (ε≈0.15, mountain/rope), native_generative (ε≈0.35, scaffold/rope), and bridge_pidginized (ε≈0.52, tangled_rope). Each reading occupies the same kernel through different mechanisms. The bridge-pidginized reading is downstream of institutional gatekeeping (institutional hebraists control standardization) and upstream of diaspora language shift (the pidginized bridge either accelerates or stabilizes diaspora Hebrew use depending on whether immersion initiatives succeed).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
