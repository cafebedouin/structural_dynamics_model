% ============================================================================
% CONSTRAINT STORY: native_daily_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_native_daily_reading, []).

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
 *   constraint_id: native_daily_reading
 *   human_readable: Hebrew Native Daily Speech Requirement for Linguistic Vitality
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   Hebrew revitalization from liturgical/literary preservation into native
 *   daily vernacular represents a constraint on linguistic life — the claim
 *   that 'Hebrew is alive only when used as primary native speech in daily
 *   life.' This constraint is ONE reading of the contested kernel
 *   'hebrew_living_language.' The native-daily-reading instantiates a
 *   specific claim about what constitutes linguistic vitality: active
 *   native-speaker communities using the language as vernacular, not merely
 *   preserving texts or liturgical registers. This reading emerged during
 *   Zionist nation-building (late 19th–early 20th century) and was enforced
 *   through education, state policy, and social pressure. It benefited the
 *   state-building project (which required linguistic unity) and the Hebrew
 *   revivalist movement (which gained authority as definers of authentic
 *   Jewishness). It extracted costs from Yiddish-speaking communities and
 *   diaspora Hebrew literacy traditions, which were reframed as inauthentic
 *   or insufficient. The constraint exhibits tangled-rope structure: genuine
 *   coordination function (building shared vernacular for emerging nation)
 *   intertwined with asymmetric extraction (suppression of alternatives,
 *   identity-locking of diaspora speakers into shame about parent languages).
 *
 * KEY AGENTS:
 *   - Yiddish Speakers in Palestine/Israel: Primary victims (powerless/identity_locked) — structurally mobile but identity-fused into believing Yiddish is diaspora weakness; forced to acquire Hebrew native speech or face social devaluation
 *   - Second-Generation Diaspora Learners: Secondary victims (moderate/constrained) — constrained by educational and social pressure; bear cost of linguistic transition while experiencing coordination benefit of shared language
 *   - Hebrew Revivalist Movement: Primary beneficiary (institutional/arbitrage) — gains authority as definers of linguistic authenticity; arbitrage options available (could accept alternative registers; chooses not to)
 *   - State-Building Project: Primary beneficiary (institutional/arbitrage) — requires linguistic unity for nation-building; extracts political legitimacy from successful vernacular establishment
 *   - Diaspora Cultural Institutions (Yiddish academies, liturgical scholars): Secondary victims (institutional/constrained) — see their domain (preservation of diaspora traditions, classical Hebrew study) marginalized and subordinated to vernacular requirement
 *   - Analytical Observer: Civilizational vantage (analytical/analytical) — risks naturalizing the native-speech requirement as immutable linguistic law; structural data reveals it as contingent historical commitment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(native_daily_reading, 0.52).
domain_priors:suppression_score(native_daily_reading, 0.48).
domain_priors:theater_ratio(native_daily_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(native_daily_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(native_daily_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(native_daily_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(native_daily_reading, tangled_rope).
narrative_ontology:human_readable(native_daily_reading, "Hebrew Native Daily Speech Requirement for Linguistic Vitality").
narrative_ontology:topic_domain(native_daily_reading, "sociolinguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(native_daily_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(native_daily_reading, 'f69328e6-3c3f-49f1-9631-0bfd17f0696a').
narrative_ontology:cs_kernel_codification('f69328e6-3c3f-49f1-9631-0bfd17f0696a', formalized).
narrative_ontology:cs_authority_grounding('f69328e6-3c3f-49f1-9631-0bfd17f0696a', extraction).
narrative_ontology:cs_interpretation_layer_present('f69328e6-3c3f-49f1-9631-0bfd17f0696a').
narrative_ontology:cs_reading_relation('f69328e6-3c3f-49f1-9631-0bfd17f0696a', native_daily_reading__liturgical_reading, forecloses).
narrative_ontology:cs_reading_relation('f69328e6-3c3f-49f1-9631-0bfd17f0696a', native_daily_reading__continuity_narrative_reading, coexists_with).
narrative_ontology:cs_axiom('f69328e6-3c3f-49f1-9631-0bfd17f0696a', foundational, native_speech_constitutive_of_linguistic_life).
narrative_ontology:cs_axiom_status(native_speech_constitutive_of_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('f69328e6-3c3f-49f1-9631-0bfd17f0696a', native_speech_constitutive_of_linguistic_life, empirically_contingent).
narrative_ontology:cs_axiom('f69328e6-3c3f-49f1-9631-0bfd17f0696a', secondary, diaspora_multilingualism_incompatible_with_nationhood).
narrative_ontology:cs_axiom_status(diaspora_multilingualism_incompatible_with_nationhood, overridden).
narrative_ontology:cs_axiom_grounding('f69328e6-3c3f-49f1-9631-0bfd17f0696a', diaspora_multilingualism_incompatible_with_nationhood, instrumental).
narrative_ontology:cs_reference_frame('f69328e6-3c3f-49f1-9631-0bfd17f0696a', hebrew_as_native_vernacular).
narrative_ontology:cs_drift_state('f69328e6-3c3f-49f1-9631-0bfd17f0696a', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f69328e6-3c3f-49f1-9631-0bfd17f0696a', '').
narrative_ontology:cs_kernel_id(native_daily_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(native_daily_reading, state_building_project).
narrative_ontology:constraint_beneficiary(native_daily_reading, ashkenazi_hebrew_revivalists).
narrative_ontology:constraint_victim(native_daily_reading, yiddish_cultural_continuity).
narrative_ontology:constraint_victim(native_daily_reading, diaspora_jewish_literacy_traditions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: YIDDISH SPEAKER (SNARE) — Structurally mobile (could theoretically maintain Yiddish), but identity-locked through linguistic nationalism ideology that frames Yiddish as diaspora weakness. The constraint extracts: Yiddish speakers must acquire Hebrew daily speech or face social devaluation and educational exclusion. Their linguistic heritage becomes socially inadmissible in the space where identity is constituted (school, public sphere). The binding mechanism is internalized — the speaker cannot see exit as legitimate because their identity frame has incorporated the premise that Hebrew native speech is the prerequisite for authentic Jewish belonging.
constraint_indexing:constraint_classification(native_daily_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: SECOND-GENERATION LEARNER (TANGLED ROPE) — Constrained by social and educational pressure to acquire native-level Hebrew speech while maintaining diaspora cultural literacy. The constraint coordinates: establishes common linguistic ground for Jewish nationhood. It extracts: requires abandonment of parental language and cultural transmission norms; career and social mobility depend on native Hebrew competence. Mixed experience — genuine coordination function (nation-building requires shared vernacular) alongside asymmetric extraction (cost borne disproportionately by diaspora families).
constraint_indexing:constraint_classification(native_daily_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REVIVALIST MOVEMENT / STATE (ROPE) — Benefits from the constraint's operation. Net beneficiary. The movement experiences the requirement for native daily Hebrew speech as solving a genuine coordination problem: building national linguistic unity from diaspora heterogeneity. The movement has arbitrage options (could accept multilingual or liturgical Hebrew as sufficient; chooses not to). Frames the constraint as pure coordination — unified Jewish nation requires unified vernacular. Extraction toward this agent is substantial but perceived as fair reward for the intellectual and political labor of revitalization.
constraint_indexing:constraint_classification(native_daily_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DIASPORA CULTURAL PRESERVATION (SCAFFOLD) — Organized agents (Yiddish cultural institutes, diaspora Jewish communities, scholars of Hebrew liturgical tradition) experience this constraint as a temporary phase with a sunset: as Hebrew becomes natively spoken (majority fluency achieved within 2-3 generations), the coordination function loosens and multilingual coexistence becomes feasible. From this perspective, the constraint's extractive phase is transition toward a terminal state where Hebrew native speech is common enough that the coercive requirement fades. Extraction is high during transition but structurally intended to be temporary.
constraint_indexing:constraint_classification(native_daily_reading, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: LITURGICAL HEBREW INSTITUTIONS (PITON) — Traditional Hebrew-literacy institutions (yeshivas, synagogues maintaining liturgical Hebrew study) continue to operate and claim authority, but their function has atrophied from 'primary mode of Jewish linguistic life' to 'secondary ritual maintenance'. The constraint that 'Hebrew must be liturgically preserved' persists through institutional inertia — rabbinical academies still teach classical Hebrew — but is largely performative relative to the dominant native-speech coordination requirement. These institutions have mobile exit options (could shift focus to preservation of classical texts without claiming they constitute living language), but persist through tradition and theological commitment rather than functional necessity.
constraint_indexing:constraint_classification(native_daily_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / LINGUISTIC NATURALISM (MOUNTAIN) — From a civilizational vantage, some version of the claim appears as a natural linguistic law: 'living languages require daily native-speaker use; literary or liturgical registers alone cannot sustain linguistic vitality across generations.' This perspective sees the native-speech requirement as an immutable property of how languages persist biologically in communities. However, the structural data contradicts the mountain classification: the engine will compute this as a false summit (beneficiaries exist, enforcement is active, alternatives were suppressed) revealing that linguistic vitality standards are contingent historical commitments, not immutable facts about language.
constraint_indexing:constraint_classification(native_daily_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(native_daily_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(native_daily_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(native_daily_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(native_daily_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(native_daily_reading, TR),
    TR >= 0.70.

:- end_tests(native_daily_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint's operation transfers substantial resources and status from diaspora communities to the state-building/revivalist project. Yiddish speakers must acquire new primary language; diaspora literacy traditions are devalued; cultural transmission to second generation is disrupted. However, the extraction is not maximal because a genuine coordination function exists: new communities in Palestine genuinely needed shared vernacular. The blending of legitimate coordination with extraction-through-suppression produces the tangled-rope structure. Suppression (0.48): Moderate. Active suppression through education policy, social penalties, and cultural stigma was substantial (peaked at 0.68 during 1948-1960s). Current baseline reflects residual mechanism: alternatives are no longer actively excluded but remain de facto excluded through normalization of Hebrew nativity. Theater ratio (0.38): Moderate-low. The constraint operates with functional core (shared language is genuinely needed) but significant performative layer (nationalist ideology, linguistic purism discourse, state ceremonies). Theater is lower than in fully degraded pitons because the coordination function remains real — the theater wraps genuine coordination rather than replacing it.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces maximum perspectival disagreement. The revivalist movement sees Rope (solving coordination problem of shared language). The state sees Rope or Scaffold (coordination with nationalist sunset logic — once Hebrew native speech is established, the coercive requirement can fade). The Yiddish speaker sees Snare (extraction with no alternative presented as legitimate). The second-generation learner sees Tangled Rope (genuine mixed experience of coordination and extraction). The diaspora cultural institutions see Piton (the claim that liturgical Hebrew constitutes living language is now performative — maintained by inertia, not function). The civilizational analyst sees Mountain (linguistic vitality 'naturally' requires native speech) but the structural data reveals false summit (beneficiaries and enforcement are present). The gap is not merely different empirical assessments but different frameworks for evaluating what constitutes linguistic life.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is computed per agent from power, exit options, and beneficiary/victim status. Yiddish speakers (powerless/identity_locked/victim) experience maximum directionality toward target (d ≈ 0.8-0.9): they bear extraction and lack exit capacity due to identity-lock. State and revivalist movement (institutional/arbitrage/beneficiary) experience maximum directionality toward beneficiary (d ≈ 0.1-0.2): they collect from the constraint and have exit options. Second-generation learners (moderate/constrained/mixed victim-beneficiary) experience intermediate directionality (d ≈ 0.55-0.65): they bear substantial cost but also gain coordination benefit and social access. The identity-lock mechanism is critical: Yiddish speakers are structurally mobile (could theoretically maintain Yiddish) but cannot exercise mobility because their identity frame has incorporated the premise that Hebrew native speech = authentic Jewish belonging. This produces the identity_locked exit option and high experienced extraction despite structural mobility.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED. The constraint's mandate ('preserve/revitalize Hebrew linguistic tradition') has evolved into a function ('establish Hebrew as primary vernacular for Jewish nation'). The original mandate emphasized cultural-historical continuity; the executed function emphasizes political-national unity. The mandate is not dead (Hebrew remains a living language and continues to carry cultural-historical significance), but it has outlived its stated justification — the constraint now persists largely to maintain the coordination of Hebrew nativity as normalized baseline, not to achieve the revitalization that was already accomplished by mid-20th century. The identity-locking function (making diaspora speakers feel shame about parent languages) persists as mechanism even though the coordination function has stabilized. Mandatrophy is unresolved because: (1) the constraint successfully completed its coordination task (Hebrew is now natively spoken), (2) but its suppressive mechanisms (identity-lock, alternatives-exclusion) persist as inertial overlay, (3) and the possibility of sustainable diglossia or multilingualism (which could decompose the constraint) was never tested due to identity-fusion engineering. A resolved mandatrophy would require either: accepting that the constraint succeeded and should be sunsetted (Scaffold logic), or accepting that the coordination function persists and identity-lock is necessary (Tangled Rope), or recognizing that suppression was never necessary for coordination and redesigning as Rope (separable).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liturgical_sufficiency_boundary,
    'Can liturgical and literary Hebrew preserve linguistic vitality without native daily speech, or does the native-speech requirement represent an empirical truth about language transmission?',
    'Historical comparison: examine whether liturgical-only Hebrew maintenance in diaspora communities (medieval Yemenite, European yeshiva contexts) constituted linguistic life or linguistic preservation. Distinguish between: (a) text-based literacy without living speech, (b) restricted speech registers (scholars only), (c) living speech in limited domains (prayer, study). Track intergenerational fluency and dialect evolution across these models.',
    'If liturgical Hebrew alone could sustain vitality: the native-speech requirement is revealed as ideological choice favoring nationalism over preservation. Reclassifies from Tangled Rope to Snare (the coordination story is cover). If liturgical Hebrew alone cannot sustain vitality: the requirement is empirically grounded coordination logic. Remains Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(liturgical_sufficiency_boundary, empirical, 'Whether liturgical/literary Hebrew can sustain linguistic vitality without native daily speech').

omega_variable(
    modernization_cost_necessity,
    'Was the suppression of Yiddish and diaspora language use a necessary cost of Hebrew revitalization, or a contingent choice to impose linguistic uniformity?',
    'Counterfactual analysis: model bilingual/multilingual scenarios where Hebrew native speech was promoted WITHOUT suppression of parent languages (Yiddish, Ladino, Arabic). Examine whether such scenarios were theoretically viable or politically incompatible with state-building requiring singular national identity. Track whether suppression was enforced through education policy (curriculum exclusion, social penalties) or emerged from status competition.',
    'If suppression was necessary: extraction is justified as transition cost. Classification stable as Tangled Rope. If suppression was contingent choice: extraction is revealed as the primary function, not a side effect. Reclassifies to Snare, victims list expands. Mandatrophy shifts from unresolved to resolved-as-false (the mandate was stated as linguistic vitality but functioned as cultural assimilation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modernization_cost_necessity, conceptual, 'Necessity vs contingency of Yiddish suppression in Hebrew revitalization').

omega_variable(
    native_speaker_identity_lock_mechanism,
    'Is the identity-locking function (making Yiddish speakers internalize Hebrew-native speech as prerequisite for Jewish belonging) an inherent feature of the revitalization process, or a deployable mechanism that could be decoupled from language acquisition?',
    'Comparative study: contrast revitalization scenarios with and without identity-fusion enforcement. Examine communities where Hebrew was adopted as shared language WITHOUT accompanying ideology that native speech = authentic Jewish identity. Track whether linguistic adoption succeeds in absence of identity-lock mechanism, or whether identity-fusion accelerates acquisition at the cost of suppressing alternatives.',
    'If identity-lock is inherent to revitalization: the constraint is structurally tangled (coordination + extraction inseparable). If identity-lock is deployable mechanism: separation is possible, revealing that extraction was contingent political choice. The constraint could be redesigned as pure Rope (Hebrew adoption without cultural shame for alternatives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(native_speaker_identity_lock_mechanism, conceptual, 'Whether identity-lock is inherent to or contingent in language revitalization').

omega_variable(
    reading_vs_sibling_foreclosure,
    'Does the native-daily-reading''s core premise (linguistic life requires native vernacular use) logically foreclose the liturgical-reading''s premise (classical/liturgical Hebrew constitutes living linguistic tradition), or do these coexist as different definitions held by different communities?',
    'Examine whether the readings share a single framework where one must be chosen (foreclosure), or occupy different authority structures where both can be held simultaneously (coexistence). Test: can a single community/institution coherently maintain both claims, or does adoption of native-daily requirement necessarily entail rejection of liturgical-sufficiency claim?',
    'If foreclosed: the readings represent incompatible kernels or incompatible readings of one kernel requiring exclusive commitment. If coexisting: the kernel permits multiple readings, each valid within its own authority frame.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_sibling_foreclosure, conceptual, 'Foreclosure vs coexistence of native-daily vs liturgical readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(native_daily_reading, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ndr_tr_t0, native_daily_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ndr_tr_t1, native_daily_reading, theater_ratio, 1, 0.35).
narrative_ontology:measurement(ndr_tr_t2, native_daily_reading, theater_ratio, 2, 0.48).
narrative_ontology:measurement(ndr_tr_t3, native_daily_reading, theater_ratio, 3, 0.38).

% Extraction over time
narrative_ontology:measurement(ndr_be_t0, native_daily_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ndr_be_t1, native_daily_reading, base_extractiveness, 1, 0.48).
narrative_ontology:measurement(ndr_be_t2, native_daily_reading, base_extractiveness, 2, 0.62).
narrative_ontology:measurement(ndr_be_t3, native_daily_reading, base_extractiveness, 3, 0.58).
narrative_ontology:measurement(ndr_be_t4, native_daily_reading, base_extractiveness, 4, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(ndr_su_t0, native_daily_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(ndr_su_t1, native_daily_reading, suppression_requirement, 1, 0.55).
narrative_ontology:measurement(ndr_su_t2, native_daily_reading, suppression_requirement, 2, 0.68).
narrative_ontology:measurement(ndr_su_t3, native_daily_reading, suppression_requirement, 3, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(native_daily_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(native_daily_reading, 0.12).
narrative_ontology:affects_constraint(native_daily_reading, liturgical_reading).
narrative_ontology:affects_constraint(native_daily_reading, continuity_narrative_reading).
narrative_ontology:affects_constraint(native_daily_reading, yiddish_cultural_extinction).
narrative_ontology:affects_constraint(native_daily_reading, hebrew_linguistic_nationalism).

% DUAL FORMULATION NOTE:
% The native_daily_reading decomposes from the natural-language claim 'Hebrew revitalization' because different readings (native-daily vs liturgical vs continuity) have structurally distinct ε values and beneficiary/victim structures. The native-daily reading emphasizes extractiveness through suppression and identity-lock. Sibling readings would show different extraction profiles: liturgical might be lower extraction (Rope or Piton rather than Tangled Rope); continuity might be higher extraction if grounded in nationalist ideology. All three stories are part of the hebrew_living_language constraint family and affect each other through legitimacy competition and resource allocation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(native_daily_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
