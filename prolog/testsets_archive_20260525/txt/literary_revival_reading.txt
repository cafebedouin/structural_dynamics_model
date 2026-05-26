% ============================================================================
% CONSTRAINT STORY: literary_revival_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literary_revival_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: literary_revival_reading
 *   human_readable: Hebrew Literary Revival Through Haskalah: Written Generative Competence Without Native Daily Speech
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   Hebrew during the Haskalah (Jewish Enlightenment, roughly 1750–1850)
 *   existed as a written literary medium with no native daily speakers. The
 *   maskilim (enlightened intellectuals) produced sophisticated Hebrew
 *   literature — novels, poetry, essays, philosophical works — in a language
 *   learned from texts and religious tradition rather than from native
 *   acquisition. This constraint represents the coordination mechanism that
 *   enabled Hebrew intellectual production across dispersed diaspora
 *   communities without requiring or enabling native speech. The constraint
 *   is structurally distinct from two sibling readings: the
 *   liturgical_continuity_reading (Hebrew maintained through daily prayer in
 *   synagogues) and the native_generation_reading (the later,
 *   early-20th-century project of teaching Hebrew as a native language to
 *   children in Palestine). This story describes ONLY the literary reading —
 *   written generative competence without native daily speech, functioning as
 *   a pure coordination mechanism for the Jewish intellectual elite. The
 *   theater_ratio shows a slight increase over the century (0.25→0.45)
 *   reflecting later appropriation of Haskalah achievements by nationalist
 *   ideology, but remains moderate because the original constraint's function
 *   was genuine elite coordination, not performative status maintenance.
 *
 * KEY AGENTS:
 *   - Haskalah Literary Elite (maskilim): Institutional/arbitrage beneficiaries — compose and circulate Hebrew texts; benefit from unified intellectual medium across diaspora; arbitrage through multilingual sophistication
 *   - Jewish Intellectual Continuity: Beneficiary collective — Hebrew literary tradition maintains chain of textual reference, interpretation, and debate across generations and communities
 *   - Ordinary Literate Communities: Moderate power/constrained exit — can read and appreciate Hebrew literature but do not natively speak; experience constraint as temporary coordination support with generational sunset as modern vernaculars develop
 *   - Later Zionist Revivalists: Appropriators — retroactively reframe Haskalah literary success as proof of Hebrew 'aliveness,' instrumentalizing the constraint for native revival project
 *   - Liturgical Communities: Parallel mechanism — maintain Hebrew continuity through daily prayer independently of literary production; two non-interfering channels of survival
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literary_revival_reading, 0.08).
domain_priors:suppression_score(literary_revival_reading, 0.12).
domain_priors:theater_ratio(literary_revival_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literary_revival_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(literary_revival_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(literary_revival_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literary_revival_reading, rope).
narrative_ontology:human_readable(literary_revival_reading, "Hebrew Literary Revival Through Haskalah: Written Generative Competence Without Native Daily Speech").
narrative_ontology:topic_domain(literary_revival_reading, "historical_linguistics/language_revitalization/commitment_systems").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literary_revival_reading, hebrew_literary_elite).
narrative_ontology:constraint_beneficiary(literary_revival_reading, jewish_intellectual_continuity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HASKALAH LITERARY ELITE (ROPE) — The maskilim (Enlightenment intellectuals) benefit from Hebrew written production as a coordination mechanism for Jewish intellectual identity and literary expression across diaspora communities. They experience low extraction because the constraint enables their core activity (literary creation) and provides arbitrage through multilingual sophistication. The written standard serves coordination: it allows Hebrew texts to circulate, be understood, and build cumulative tradition.
constraint_indexing:constraint_classification(literary_revival_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER (ROPE) — From a civilizational/global analytical perspective, Hebrew literary production during Haskalah is a pure coordination mechanism: written standard, shared lexicon, interpretive tradition, and textual circulation enabling geographically dispersed communities to participate in a unified intellectual commons. Minimal extraction, high functional value. The constraint exists to solve the collective action problem of maintaining Jewish intellectual continuity in the absence of daily vernacular speech.
constraint_indexing:constraint_classification(literary_revival_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 3: ORDINARY LITERATE COMMUNITY MEMBER (SCAFFOLD) — For educated but non-elite Jews with Hebrew literacy, the constraint is temporary coordination support: Hebrew literary texts (novels, poetry, essays) create a unified intellectual sphere, but with an implicit sunset. As modern national languages develop and secular education spreads, reliance on Hebrew as the sole literary lingua franca declines. The constraint has bounded duration — it coordinates Jewish intellectual culture during a transitional period (18th-19th centuries) when traditional Yiddish and modern vernaculars are not yet adequate for intellectual production.
constraint_indexing:constraint_classification(literary_revival_reading, scaffold,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: SECULAR NATIONALIST MOVEMENT (PITON) — The Zionist and Hebrew revivalist movements later appropriate Haskalah literary production as evidence that Hebrew 'was alive' and 'was always living,' instrumentalizing the literary constraint for nationalist ideology. The theater ratio here is high: the claim that Haskalah Hebrew represents a 'living' language is largely performative, serving to justify the later political project of native-speaker revival. The constraint's original coordination function (intellectual elite connection) is reframed as proof of linguistic vitality for purposes it was never designed to serve.
constraint_indexing:constraint_classification(literary_revival_reading, piton,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literary_revival_reading_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(literary_revival_reading, TR),
    TR >= 0.70.

:- end_tests(literary_revival_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The Haskalah literary constraint is a pure coordination mechanism with minimal asymmetric extraction. The maskilim create and share texts; readers access them; a unified intellectual sphere results. There is no identifiable victim group and no coercive mechanism. The only potential 'cost' is the effort of learning classical Hebrew from texts, but this is voluntary and enables the core activity all participants value. The constraint is purely beneficial to those who participate. Suppression (0.12): Very low. The constraint has no external barriers, coercive mechanisms, or alternative-suppression elements. Hebrew literacy is voluntary; communities and individuals can exit by switching to modern vernacular languages (which indeed happens over the 19th century). The low suppression reflects that the constraint has no enforcement mechanism — it persists by providing genuine coordination value, not by preventing alternatives. Theater (0.35): Moderate and rising. In the original Haskalah period (1750–1800), the theater ratio is lower (~0.25) because the constraint's function is transparently coordination: maskilim are solving the real collective action problem of shared intellectual medium. As time progresses and especially as nationalist appropriation increases post-1850, the theater ratio rises (~0.45 by 1850) because the constraint begins to be instrumentalized as 'proof' of Hebrew vitality. By the early 20th century (outside this story's interval), nationalist claims about Haskalah would push theater further (0.70+ in piton territory). The story captures the transition point: the original coordination function is intact, but narrative appropriation is beginning.
 *
 * PERSPECTIVAL GAP:
 *   The elite literary producer (institutional/arbitrage) sees pure coordination: Hebrew enables their creative practice and intellectual participation. The analytical observer agrees: rope is the correct classification — a coordination mechanism with low extraction and high functional value. The ordinary literate community member (moderate/constrained) also perceives coordination but with bounded duration — Hebrew literary production is supporting transitional intellectual needs during a period when traditional languages (Yiddish, Aramaic) have declined but modern national vernaculars (German, Russian, Polish) have not yet fully replaced Hebrew's intellectual role. This perspective hints at scaffold rather than pure rope: 'This works for now, but we know it's temporary.' The later nationalist movement (powerful/mobile) retroactively reframes the constraint as evidence of Hebrew 'aliveness,' adding theatrical appropriation that was not present in the original constraint. No agent experiences extraction or victimhood: the gap is between original function (coordination) and later appropriation (status claim / nationalist mythology).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is uniformly low across all perspectives because no agent experiences extraction. The beneficiaries (literary elite, intellectual continuity) have derived d ≈ 0.05–0.15 reflecting arbitrage exit options and genuine benefit. There are no victim groups, so the high-d tail of the directionality distribution is empty. The constraint's effectiveness (χ) remains low across all contexts because f(d) stays in the negative-to-low range throughout, and the base extraction ε is very low. This produces rope classification uniformly: low extraction + high coordination function. The slight rise in theater_ratio over time does not increase d for beneficiaries (they continue to benefit); it only increases the narrative confusion about what the constraint is (coordination vs. proof-of-aliveness). This is a diagnostic signal that the original constraint (literary coordination) is being colonized by a secondary constraint (nationalist appropriation narrative) — the two would be structurally distinct stories if we extended the analysis post-1850.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING AS COMMITTER FRAME: This constraint instantiates the literary_revival_reading of the hebrew_living_language kernel. The kernel is the contested claim 'Hebrew was a living language during and after the Haskalah.' Three structurally distinct readings produce three different constraints with different ε values and causal relationships: (1) literary_revival_reading (this story): ε=0.08, rope, written generative competence without native speech, pure coordination. (2) liturgical_continuity_reading (sibling): ε≤0.05, mountain or rope, Hebrew maintained through religious practice with native fossilization in prayer contexts, minimal or absent coordination function. (3) native_generation_reading (sibling): ε=0.35–0.55, tangled_rope or scaffold, early-20th-century deliberate project to teach Hebrew as native language to children, requiring enforcement and producing mixed coordination/extraction. These are NOT three perspectives on one constraint. They are three separate constraints sharing a kernel (the 'aliveness' claim) but with different base extraction values, victim sets, mechanisms, and temporal scope. The mandatrophy is resolved by decomposition: the kernel is contested not because observers disagree on classification, but because the underlying structural claims are different. To understand Hebrew language survival, one must analyze all three readings as separate constraint stories linked by network.affects_constraints.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_alternative_reading_liturgical,
    'Is Hebrew continuity through Haskalah literary production the primary reading of Hebrew language survival, or is liturgical continuity (Hebrew as daily prayer language in religious communities) the more fundamental constraint?',
    'Comparative analysis of which channel (liturgical recitation vs. literary production) maintained greater community participation, lexical stability, and intergenerational transmission during the Haskalah period and afterward.',
    'If liturgical continuity is primary: literary production is a secondary phenomenon built atop religious practice. The constraint''s extractiveness and type would shift depending on whether one privileges the liturgical or literary observable. If literary production is primary: the rope classification (coordination among elite intellectuals) holds. If neither is primary (both are artifacts of deeper mechanism): the constraint framing requires decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_alternative_reading_liturgical, conceptual, 'Whether literary production or liturgical continuity is the primary mechanism of Hebrew survival during Haskalah').

omega_variable(
    kernel_alternative_reading_native_generation,
    'Does Haskalah literary Hebrew represent a genuinely ''living'' constraint on native-speaker acquisition, or is it an elite-only written tradition with no direct causal pathway to native generational transmission?',
    'Historical evidence of whether Haskalah literary production directly enabled (a) children of maskilim to acquire Hebrew as a native language, (b) broader community adoption of Hebrew for daily life, or (c) was a purely elite literary phenomenon without vernacularization.',
    'If literary production enabled native acquisition: the constraint is a causal prerequisite for the native_generation_reading. If literary and native streams were independent: the two constraints (literary_revival_reading and native_generation_reading) are structurally distinct with different ε values and causal relationships. This determines whether the constraint family is linear (literary→native) or parallel (literary ∥ native).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_alternative_reading_native_generation, empirical, 'Whether Haskalah literary production directly caused native-speaker acquisition or remained elite-only').

omega_variable(
    generative_competence_scope,
    'Does ''written generative competence'' constitute genuine linguistic constraint or merely technical facility (the ability to compose coherent texts without phonological/pragmatic native embodiment)?',
    'Linguistic analysis of Haskalah texts for evidence of native-like spontaneous generation (productivity, creativity, error patterns) vs. learned composition (formulaic diction, calquing, explicit grammar-reference patterns). Comparison to attested living language productivity.',
    'If genuine competence: the constraint is a valid coordinate point for language continuity. If technical facility only: extractiveness is even lower (near-zero), and the constraint is almost purely performative — theater_ratio should be ≥ 0.70 (piton territory).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generative_competence_scope, empirical, 'Whether written generative competence represents authentic linguistic competence or learned facility').

omega_variable(
    elite_dispersion_coordination,
    'Is the Haskalah literary constraint genuinely solving a coordination problem across dispersed communities, or is it serving internal elite status differentiation (the constraint''s value is exclusivity, not coordination)?',
    'Historical evidence of cross-community circulation of Haskalah texts, evidence of unified intellectual reference frames across diaspora regions, vs. evidence of local elite prestige-signaling through Hebrew literacy.',
    'If coordination: rope classification is correct. If status differentiation: extractiveness rises toward snare territory (0.46+) because the constraint''s mechanism is exclusionary rather than inclusive, and benefits concentrate on the literary elite while broader communities are excluded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_dispersion_coordination, empirical, 'Whether Haskalah literary constraint functions as cross-community coordination or elite status differentiation').

omega_variable(
    theatrical_revival_appropriation,
    'To what extent is the Haskalah literary constraint''s later characterization as ''Hebrew living'' a retroactive narrative imposed by 20th-century Zionist ideology rather than an accurate representation of the constraint''s original function and scope?',
    'Comparison of Haskalah-era self-descriptions of Hebrew literary work (what did maskilim say they were doing?) vs. 20th-century revivalist narratives (what did they claim Haskalah represented?). Examination of whether nationalist ideology added theater ratio post-hoc.',
    'If appropriation is substantial: theater_ratio during original period (18th-19th centuries) is lower (~0.25, pure coordination), but post-hoc theater from 20th-century reinterpretation is high (~0.70+). This suggests measuring the constraint at different temporal points yields different types (rope when first-order, piton when mediated through revivalist appropriation). May require constraint decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theatrical_revival_appropriation, conceptual, 'Degree to which later Zionist appropriation imposed ''living language'' narrative on Haskalah literary practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literary_revival_reading, 1750, 1850).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(literary_theater_t0, literary_revival_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(literary_theater_t50, literary_revival_reading, theater_ratio, 50, 0.35).
narrative_ontology:measurement(literary_theater_t100, literary_revival_reading, theater_ratio, 100, 0.45).

% Extraction over time
narrative_ontology:measurement(literary_extract_t0, literary_revival_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(literary_extract_t50, literary_revival_reading, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(literary_extract_t100, literary_revival_reading, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literary_revival_reading, information_standard).
narrative_ontology:affects_constraint(literary_revival_reading, liturgical_continuity_reading).
narrative_ontology:affects_constraint(literary_revival_reading, native_generation_reading).

% DUAL FORMULATION NOTE:
% The hebrew_living_language kernel decomposes into three structurally distinct constraint stories: (1) literary_revival_reading (this file) — ε=0.08, written intellectual coordination without native speech; (2) liturgical_continuity_reading — ε≤0.05, religious practice preservation with fossilized forms; (3) native_generation_reading — ε=0.35–0.55, 20th-century deliberate native-speaker acquisition project. These are three readings of one kernel, not three perspectives on one constraint. The literary reading is upstream (prerequisite network position) relative to the native reading insofar as Haskalah intellectual prestige may have created conditions favoring later native revival, but causality is contested (see omega_theatrical_revival_appropriation). The liturgical and literary readings are parallel and independent — each maintained Hebrew continuity through different mechanisms without direct causal interaction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
