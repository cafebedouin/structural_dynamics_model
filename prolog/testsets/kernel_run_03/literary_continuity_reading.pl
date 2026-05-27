% ============================================================================
% CONSTRAINT STORY: literary_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literary_continuity_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: literary_continuity_reading
 *   human_readable: Literary Continuity as Living Language Criterion (Haskalah Reading)
 *   domain: sociolinguistics/religious_studies/nationalism
 *
 * SUMMARY:
 *   The literary-continuity reading of 'living language' defines a language's
 *   vitality through its capacity to generate new literary and intellectual
 *   work, regardless of native-speaker population. This reading emerged
 *   within the Haskalah (Jewish Enlightenment) and gained institutional force
 *   through Hebrew-language periodicals, secular literature, and Zionist
 *   educational policy. The reading was contestable from its inception:
 *   traditional authorities maintained that Hebrew's vitality derived from
 *   its role in liturgy and religious study; later observers argued that
 *   native-speaker generation (children born to the language) was the true
 *   criterion. This constraint story instantiates ONE of these three
 *   competing definitions of what makes a language 'living.' The
 *   literary-continuity reading benefits secular intellectuals (maskilim) by
 *   granting cultural authority to those who produce literary work in Hebrew,
 *   and it extracts value by rendering non-literary speakers' contributions
 *   invisible. It coordinates multiple actors around Hebrew language policy
 *   while asymmetrically distributing recognition and authority.
 *
 * KEY AGENTS:
 *   - Maskilim Intellectuals (institutional/arbitrage): Primary beneficiaries — gain cultural authority and linguistic legitimacy for Hebrew-language periodicals, novels, philosophy. Coordinate literary production as evidence of vitality.
 *   - Non-Literary Speakers (powerless/trapped): Primary victims — oral fluency does not count as evidence under this criterion. Excluded from the definition of vitality itself. No exit option from the exclusion.
 *   - Traditional Religious Authorities (moderate/constrained): Secondary victims — their maintenance of Hebrew through liturgy and study is subordinated as evidence compared to secular literary production. Constrained exit: shifting legitimacy criteria incurs institutional and cultural cost.
 *   - Diaspora Jewish Communities (organized/constrained): Secondary agents — maintained Hebrew through multilingual literacy in prayer and study; their contribution is recognized but subordinated. Constrained exit: language shift to local vernaculars incurs religious identity cost.
 *   - Zionist Nationalist Movement (powerful/mobile): Structural beneficiary — the literary-continuity reading provides evidence that Hebrew can support modern secular national culture, disconnected from religious authority.
 *   - Modern Hebrew Educational System (organized/constrained): Temporary coordinator — deliberately constructs native-speaker communities whose emergence will make explicit revival apparatus obsolete. Sunset horizon.
 *   - Analytical Observer (analytical/analytical): Sees the criterion as potentially natural or potentially constructed — risks naturalizing a contestable reading as inherent linguistic reality.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literary_continuity_reading, 0.38).
domain_priors:suppression_score(literary_continuity_reading, 0.52).
domain_priors:theater_ratio(literary_continuity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literary_continuity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(literary_continuity_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(literary_continuity_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literary_continuity_reading, tangled_rope).
narrative_ontology:human_readable(literary_continuity_reading, "Literary Continuity as Living Language Criterion (Haskalah Reading)").
narrative_ontology:topic_domain(literary_continuity_reading, "sociolinguistics/religious_studies/nationalism").

domain_priors:requires_active_enforcement(literary_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literary_continuity_reading, '79950ad5-f2ed-436b-a52d-096ebcf33fa7').
narrative_ontology:cs_created_at('79950ad5-f2ed-436b-a52d-096ebcf33fa7', '').
narrative_ontology:cs_kernel_codification('79950ad5-f2ed-436b-a52d-096ebcf33fa7', formalized).
narrative_ontology:cs_authority_grounding('79950ad5-f2ed-436b-a52d-096ebcf33fa7', lineage).
narrative_ontology:cs_interpretation_layer_present('79950ad5-f2ed-436b-a52d-096ebcf33fa7').
narrative_ontology:cs_kernel_id(literary_continuity_reading, living_language_status).
narrative_ontology:cs_reading_relation('79950ad5-f2ed-436b-a52d-096ebcf33fa7', liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('79950ad5-f2ed-436b-a52d-096ebcf33fa7', native_generation_reading, coexists_with).
narrative_ontology:cs_axiom('79950ad5-f2ed-436b-a52d-096ebcf33fa7', foundational, literary_production_defines_vitality).
narrative_ontology:cs_axiom_status(literary_production_defines_vitality, holdable).
narrative_ontology:cs_axiom_grounding('79950ad5-f2ed-436b-a52d-096ebcf33fa7', literary_production_defines_vitality, empirically_contingent).
narrative_ontology:cs_axiom('79950ad5-f2ed-436b-a52d-096ebcf33fa7', foundational, secular_intellectual_authority_primary).
narrative_ontology:cs_axiom_status(secular_intellectual_authority_primary, holdable).
narrative_ontology:cs_axiom_grounding('79950ad5-f2ed-436b-a52d-096ebcf33fa7', secular_intellectual_authority_primary, conventional).
narrative_ontology:cs_reference_frame('79950ad5-f2ed-436b-a52d-096ebcf33fa7', haskalah_hebrew_literary_production).
narrative_ontology:cs_drift_state('79950ad5-f2ed-436b-a52d-096ebcf33fa7', contemporary_digital_vernacular, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literary_continuity_reading, maskilim_intellectuals).
narrative_ontology:constraint_beneficiary(literary_continuity_reading, secular_cultural_authority).
narrative_ontology:constraint_victim(literary_continuity_reading, illiterate_speakers).
narrative_ontology:constraint_victim(literary_continuity_reading, non_literary_populations).
narrative_ontology:constraint_victim(literary_continuity_reading, traditional_liturgical_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-LITERARY SPEAKER (SNARE) — Speakers with oral fluency but no literacy access face structural exclusion from the criterion itself. Their language use does not count as evidence of vitality under this reading. Maximum extraction: vitality is defined in ways that make their participation invisible. No exit option; the definition controls whether their language-use counts.
constraint_indexing:constraint_classification(literary_continuity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: TRADITIONAL LITURGICAL AUTHORITY (TANGLED ROPE) — Hebrew liturgy and prayer provided continuity through diaspora for centuries. This reading recognizes literary vitality but subordinates liturgical continuity as subordinate evidence. Mixed experience: some coordination (all parties acknowledge Hebrew continuity), but asymmetric power (the literary criterion elevates secular intellectuals over religious authorities who maintained the language through other means). Constrained exit: shifting legitimacy criteria incurs institutional cost.
constraint_indexing:constraint_classification(literary_continuity_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MASKILIM INTELLECTUALS (ROPE) — Secular intellectuals coordinating Hebrew literary production (periodicals, novels, philosophy) experience this criterion as pure coordination: it enables their cultural project, recognizes their authority, and legitimates Hebrew as a vehicle for modern secular thought. Arbitrage: they can shift to Yiddish or European languages but choose Hebrew literature as culturally valuable. Net beneficiary.
constraint_indexing:constraint_classification(literary_continuity_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: NATIONALIST MOVEMENT (ROPE) — Zionist and Hebrew-revivalist movements benefit structurally from the literary continuity reading: it provides evidence that Hebrew can be a modern national language, disconnected from religious authority. They coordinate multiple actors around Hebrew language policy. Mobile exit: can promote Yiddish or other alternatives but find Hebrew more strategically valuable for nation-building.
constraint_indexing:constraint_classification(literary_continuity_reading, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: DIASPORA JEWISH COMMUNITIES (TANGLED ROPE) — These communities maintained Hebrew through liturgy and study while speaking local languages. Under this reading, their Hebrew literacy counts as evidence of vitality, but their lived multilingualism is not the primary criterion. Mixed coordination (all parties acknowledge Hebrew persistence) and extraction (the reading privileges secular literary production over the liturgical/educational maintenance that diaspora communities performed). Constrained exit: language shift to local vernaculars incurs religious and cultural identity cost.
constraint_indexing:constraint_classification(literary_continuity_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: MODERN HEBREW EDUCATIONAL SYSTEM (SCAFFOLD) — The educational infrastructure that teaches Hebrew to non-native speakers (Zionist schools, kibbutz education, mandatory instruction) has a sunset horizon in this reading. Once Hebrew becomes natively spoken by Israel-born populations, the explicit revival apparatus becomes vestigial. Temporary coordination: the system deliberately constructs native-speaker communities, making itself structurally obsolete. Low extraction: the educational system serves a clear goal with declining overhead as goals are achieved.
constraint_indexing:constraint_classification(literary_continuity_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL-LAW VIEW (MOUNTAIN) — From a civilizational perspective, literary production is an inherent feature of any living language; it is as natural and inevitable as grammar itself. Languages that produce literary work are simply being what languages do. This perspective risks naturalizing what is actually a sociolinguistic choice about which evidence counts. The engine's false-summit detector will identify this as naturalization of a contestable criterion.
constraint_indexing:constraint_classification(literary_continuity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literary_continuity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(literary_continuity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(literary_continuity_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(literary_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The reading benefits elite intellectuals by granting authority to literary production, but the extraction is not severe because (1) some coordination genuinely exists (all parties acknowledge Hebrew's textual tradition), and (2) non-literary speakers retain language use even if their vitality-status is questioned. The extraction is cultural and epistemic, not material or coercive. Suppression (0.52): Moderate-high. Significant barriers exist to challenging the criterion: institutional education treats literary production as the standard; academia rewards literary scholarship; cultural prestige attaches to written work. But suppression is not total — oral traditions are still practiced and valued in some communities; the criterion is debated rather than enforced by violence. Theater ratio (0.58): Moderate-high. The criterion includes performative elements: literary journals establish cultural legitimacy through publication format; academic study of Hebrew literature creates institutional recognition; the Haskalah emphasis on periodicals and books reflects partly epistemic and partly theatrical investment in the written medium as evidence of cultural vitality. Theater has increased over the measurement interval as educational institutions institutionalized the literary criterion, making it less contestable.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals the reading's asymmetry. Maskilim intellectuals experience this as pure coordination (Rope) — a mechanism that enables their cultural work. Non-literary speakers experience it as extraction (Snare) — a definition that makes their language use invisible. Traditional authorities experience mixed coordination and extraction (Tangled Rope) — Hebrew continuity is acknowledged but their role in maintaining it is subordinated. The Zionist movement experiences coordination (Rope) — the criterion provides evidence for Hebrew's modernity. The diaspora communities experience constrained coordination (Tangled Rope) — their multilingual literacy is recognized but their oral fluency is not counted. The educational system experiences temporary coordination (Scaffold) — it deliberately builds the conditions for its own obsolescence. The analytical observer risks a false summit (Mountain) — treating literary productivity as an inherent natural feature of language rather than a constructed criterion. The gap between how different agents experience this single constraint demonstrates why the literary-continuity reading is contestable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position. Maskilim intellectuals (institutional/arbitrage) are beneficiaries with exit options: they can shift to Yiddish, European languages, or other media, but they choose Hebrew literary production as culturally valuable. Their d-value is low (~0.15); they experience negative or minimal extraction. Non-literary speakers (powerless/trapped) have no exit: they speak Hebrew but cannot make their language use count as evidence of vitality under this criterion. Their d-value is high (~0.95); they experience maximum extraction. Traditional authorities (moderate/constrained) are mixed beneficiaries and victims: they coordinated Hebrew maintenance through liturgy, but the new criterion subordinates their role. Their d-value is moderate-high (~0.65). The Zionist movement (powerful/mobile) benefits structurally from the criterion but has exit options (could promote Yiddish); their d-value is low-moderate (~0.25). The diaspora communities (organized/constrained) coordinated Hebrew maintenance but face high costs to recognize alternatives; their d-value is moderate (~0.55). The educational system (organized/constrained) coordinates around a goal it is designed to supersede; its d-value reflects constrained agency within a sunset structure (~0.50). These d-values derive from the beneficiary/victim declarations and exit options; the engine computes them without explicit override.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in this constraint is resolved by recognizing that the 'living language' kernel has multiple readings, not a single truth. The literary-continuity reading is not the only valid definition — it is one political choice among competing choices about what counts as evidence of language vitality. The Tangled Rope classification captures this: the reading coordinates multiple actors around Hebrew language policy (genuine coordination function) while asymmetrically distributing authority (beneficiaries gain epistemic and cultural status; victims lose visibility). The mandatrophy is dissolved by the committer frame: this is not 'is Hebrew living or not?' but 'which reading of livingness are we using?' The false-summit perspective (Mountain) represents the naturalization risk — treating this constructed criterion as an inherent feature of language itself. The other perspectives show how the same structural arrangement produces different experienced classifications depending on agent position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literary_production_visibility,
    'Does the criterion of literary productivity exclude non-written or oral literary traditions (storytelling, poetry recitation, liturgical composition) that were historically significant to Hebrew maintenance?',
    'Historical analysis of Hebrew oral tradition maintenance; comparison of written vs. oral literary contribution to language vitality across diaspora communities; documentation of pre-Haskalah Hebrew literary activity',
    'If oral tradition counts: beneficiary set expands to include traditional authorities and illiterate poets; extractiveness drops (ε → 0.25). If only written tradition counts: the criterion is a structural exclusion of non-elite populations. Current classification assumes written-only primacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(literary_production_visibility, empirical, 'Whether oral literary traditions constitute evidence of language vitality').

omega_variable(
    vitality_criterion_reading_ambiguity,
    'Is this reading of ''living language'' a genuine semantic discovery (literary production is what linguistically defines vitality) or a constructed criterion that serves nationalist and secularist interests?',
    'Comparative historical analysis across language revivals (Irish, Basque, Sanskrit, Welsh): does literary productivity precede or follow speaker population growth? Does the criterion apply uniformly or only to languages with elite intellectual movements?',
    'If genuine discovery: the reading is empirically grounded; classification as Tangled Rope holds. If constructed criterion: the reading is better classified as Snare (pure extraction dressed as linguistic science); ε → 0.62. This is the core ambiguity of the kernel decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vitality_criterion_reading_ambiguity, conceptual, 'Whether literary productivity is a linguistic discovery or a constructed criterion').

omega_variable(
    sibling_reading_foreclosure_test,
    'Does the literary-continuity reading logically foreclose the native-generation reading, or can both readings coexist in different communities'' frameworks?',
    'Examine how Israeli Hebrew speakers (native generation) relate to the literary-continuity criterion. Do native speakers accept the criterion, or do they regard it as irrelevant to their lived language vitality? Historical examination of how the readings competed in early 20th-century debates.',
    'If foreclosure: the readings cannot both be held within a single framework (rare); update reading_relations to forecloses. If coexistence: the readings are held by different parties (intellectuals vs. speakers) without logical contradiction; reading_relations remains coexists_with.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_test, empirical, 'Whether literary-continuity reading forecloses native-generation reading').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of non-literary voices (suppression = 0.52) primarily structural (institutional gatekeeping of what counts as evidence) or internalized (non-literary speakers accept the criterion and view their own speech as less vital)?',
    'Discourse analysis of non-literary Hebrew speakers'' self-assessment of language vitality; examination of educational curricula that naturalize the literary criterion; comparison of suppression levels before vs. after institutionalization of the criterion in schools and academies',
    'If structural: the suppression is an enforcement mechanism that could be removed by changing institutional criteria; omegas for decomposition exist. If internalized: the suppression operates as identity capture; some agents become unable to recognize their own language use as evidence of vitality. Suggests identity_locked exit option for some agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is structural gatekeeping or internalized devaluation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literary_continuity_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literary_continuity_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(lite_tr_t30, literary_continuity_reading, theater_ratio, 30, 0.51).
narrative_ontology:measurement(lite_tr_t60, literary_continuity_reading, theater_ratio, 60, 0.58).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literary_continuity_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(lite_be_t30, literary_continuity_reading, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(lite_be_t60, literary_continuity_reading, base_extractiveness, 60, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literary_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(literary_continuity_reading, liturgical_preservation_reading).
narrative_ontology:affects_constraint(literary_continuity_reading, native_generation_reading).

% DUAL FORMULATION NOTE:
% The living_language_status kernel decomposes into three structurally distinct constraints with different ε-values and different beneficiary/victim structures. Literary-continuity-reading (this file, ε=0.38) focuses on intellectual production. Liturgical-preservation-reading (ε~0.15, Rope) focuses on continuity through religious practice. Native-generation-reading (ε~0.05, Rope) focuses on intergenerational transmission. All three affect language policy and educational decisions but operate through different mechanisms. Do not collapse them into a single story — the ε-variance violates the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
