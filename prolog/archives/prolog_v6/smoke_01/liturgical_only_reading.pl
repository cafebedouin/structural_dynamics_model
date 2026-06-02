% ============================================================================
% CONSTRAINT STORY: liturgical_only_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liturgical_only_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: liturgical_only_reading
 *   human_readable: Hebrew Living Language (Liturgical-Only Reading)
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   The liturgical-only reading claims that Hebrew was a 'living language'
 *   during the 2000-year diaspora (70 CE – 1880s) because it was continuously
 *   used in prayer, study, and sacred contexts, despite the absence of
 *   vernacular native speakers who used it for daily communication. This
 *   reading instantiates one response to a contested kernel: the claim
 *   'Hebrew is a living language.' Three structural readings are possible —
 *   this constraint models ONLY the liturgical-only reading. The sibling
 *   readings (native-daily and scholarly-written) are separate constraints
 *   with different ε values and different authority structures. This reading
 *   is defined by the axiom that continuous use in any domain
 *   (liturgical/sacred) preserves a language's living status, against the
 *   structural evidence that productive speech generation was suppressed. The
 *   constraint exhibits Rope classification from the rabbinic institutional
 *   perspective (coordination mechanism for diaspora Jewish identity) but
 *   Snare from the vernacular innovation perspective (suppression of speech
 *   generation) and Piton from the modern revival perspective (degraded
 *   institutional form). The analytical observer risks a false-summit
 *   Mountain classification (treating the suppression as a linguistic law
 *   rather than a policy choice).
 *
 * KEY AGENTS:
 *   - Rabbinic Authorities: Institutional beneficiaries (institutional/arbitrage) — control interpretive authority, define what counts as proper Hebrew use, maintain the constraint through legal and cultural enforcement
 *   - Vernacular Innovation (Agents seeking to use Hebrew for daily speech): Primary victims (powerless/trapped) — suppressed by religious-legal prohibitions and social stigma against non-sacred Hebrew use
 *   - Jewish Diaspora Communities: Secondary actors (moderate/constrained) — benefit from Hebrew as a unifying liturgical lingua franca but constrained by the immutability rule from naturalizing Hebrew as everyday speech
 *   - Hebrew Revival Movement (19th-20th centuries): Organized challengers (organized/mobile) — successfully revived Hebrew as a living vernacular by rejecting the liturgical-only constraint; demonstrate that the suppression was policy, not linguistic necessity
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the suppression as an immutable linguistic feature rather than recognizing it as a contingent institutional policy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liturgical_only_reading, 0.12).
domain_priors:suppression_score(liturgical_only_reading, 0.48).
domain_priors:theater_ratio(liturgical_only_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liturgical_only_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(liturgical_only_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(liturgical_only_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liturgical_only_reading, rope).
narrative_ontology:human_readable(liturgical_only_reading, "Hebrew Living Language (Liturgical-Only Reading)").
narrative_ontology:topic_domain(liturgical_only_reading, "historical_linguistics/language_revitalization/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liturgical_only_reading, '91203694-5594-4d54-aa42-0972c6d90d2b').
narrative_ontology:cs_created_at('91203694-5594-4d54-aa42-0972c6d90d2b', '').
narrative_ontology:cs_kernel_codification('91203694-5594-4d54-aa42-0972c6d90d2b', fixed_text).
narrative_ontology:cs_authority_grounding('91203694-5594-4d54-aa42-0972c6d90d2b', lineage).
narrative_ontology:cs_interpretation_layer_present('91203694-5594-4d54-aa42-0972c6d90d2b').
narrative_ontology:cs_kernel_id(liturgical_only_reading, hebrew_living_language).
narrative_ontology:cs_reading_relation('91203694-5594-4d54-aa42-0972c6d90d2b', native_daily_reading, forecloses).
narrative_ontology:cs_reading_relation('91203694-5594-4d54-aa42-0972c6d90d2b', scholarly_written_reading, coexists_with).
narrative_ontology:cs_axiom('91203694-5594-4d54-aa42-0972c6d90d2b', foundational, liturgical_continuity_preserves_liveness).
narrative_ontology:cs_axiom_status(liturgical_continuity_preserves_liveness, holdable).
narrative_ontology:cs_axiom('91203694-5594-4d54-aa42-0972c6d90d2b', foundational, vernacular_innovation_is_profanation).
narrative_ontology:cs_axiom_status(vernacular_innovation_is_profanation, overridden).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liturgical_only_reading, rabbinic_authorities).
narrative_ontology:constraint_victim(liturgical_only_reading, vernacular_innovation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RABBINIC AUTHORITY (ROPE) — Institutional actor coordinating the fixed liturgical corpus across diaspora communities. Beneficiary from the constraint: controls interpretive authority through textual immutability. Experiences the constraint as pure coordination — maintaining Hebrew as the sacred language of worship solves the collective action problem of preserving Jewish identity across geographic dispersal and linguistic assimilation. No meaningful extraction from this position; the constraint is the coordination mechanism itself.
constraint_indexing:constraint_classification(liturgical_only_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 2: VERNACULAR INNOVATION (SNARE) — Agents seeking to adapt, innovate, or naturalize Hebrew for daily use experience maximum suppression. The liturgical-only reading forecloses vernacular legitimacy: any attempt to use Hebrew outside the sacred domain is defined as profanation or dilution of the holy language. Trapped by religious-legal prohibition and social stigma. Suppression ≥ 0.48 enforces the constraint against drift toward living speech.
constraint_indexing:constraint_classification(liturgical_only_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: JEWISH DIASPORA COMMUNITY (ROPE) — Benefits from Hebrew as a liturgical lingua franca that unites communities across language boundaries and generations. The constraint solves the genuine coordination problem: without a fixed sacred language, liturgical coherence fragments. Constrained exit (breaking from liturgical Hebrew means cultural alienation, not material deprivation) but real coordination function. Experiences the constraint as enabling collective worship despite linguistic diversity.
constraint_indexing:constraint_classification(liturgical_only_reading, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — From a linguistic-naturalist perspective, a language locked into liturgical use only is by definition not 'living' in the linguistic sense — no productive speech, no generative capacity, no adaptation to speakers' needs. The designation 'living language' appears to denote an immutable property: a language either generates new sentences or it does not. From this view, the constraint is a Mountain — the liturgical-only reading naturalizes what is actually a policy choice (suppression of vernacular) as a linguistic fact. However, false summit detection will trigger: the constraint has identifiable beneficiaries (rabbinic authorities) who benefit from the fixed-corpus policy.
constraint_indexing:constraint_classification(liturgical_only_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: HEBREW REVIVAL MOVEMENT (PITON) — The 20th-century revival of Hebrew as a vernacular/daily language represents the degradation of the liturgical-only constraint. The revival's success (Israeli Hebrew, modern speakers) shows that the 'living language' claim was contingent on institutional suppression, not linguistic immutability. The piton perspective sees the liturgical-only reading as a degraded institutional form: it once functioned as THE coordination mechanism for diaspora Judaism, but modern revitalization has replaced it with vernacular living speech. Theater ratio 0.65 reflects that much of modern liturgical Hebrew invokes 'purity of the ancient language' while the actual practice adapts pronunciation, vocabulary, and syntactic flexibility to speakers' vernacular background.
constraint_indexing:constraint_classification(liturgical_only_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liturgical_only_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(liturgical_only_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(liturgical_only_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(liturgical_only_reading, TR),
    TR >= 0.70.

:- end_tests(liturgical_only_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The constraint is primarily a coordination mechanism (unifying diaspora Judaism through fixed sacred language), not an extraction mechanism. Rabbinic beneficiaries gain authority and interpretive control, but the constraint itself solves a genuine coordination problem: maintaining liturgical coherence across dispersed, multilingual communities. The low extractiveness reflects that this is a Rope, not a Snare or Tangled Rope. Suppression (0.48): Moderate-high. The constraint requires active suppression of vernacular innovation to maintain the immutability rule. Halakic prohibitions against non-sacred Hebrew use, cultural stigma against 'corrupting' the holy language, and institutional control over which Hebrew forms are acceptable all constitute suppression machinery. Theater ratio (0.65): Moderate-high. The liturgical performance itself is high-theater (formal prayer, cantorial chant, ritual correctness). Additionally, the claim that recitation alone constitutes 'living language' status involves theatrical framing — invoking 'language preservation' to describe a practice that actually suppresses language change. Modern liturgical practice often invokes pseudo-linguistic arguments about 'purity' and 'authenticity' while accommodating pronunciation variants and syntactic flexibility. The theater has increased over time (interval 0–1000) as the constraint faced pressure from modernization: by the early 20th century, maintaining the liturgical-only reading required increasingly elaborate theater to explain why Hebrew could be 'living' without native speakers.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between the rabbinic institutional view (Rope) and the vernacular innovation view (Snare) is the core diagnostic signal. The institution sees coordination; the suppressed agent sees extraction. This gap reveals that the 'living language' claim is definitionally contestable: if living requires productive speech generation, the constraint is false (Hebrew was not living). If living requires only continuous use in any domain, the constraint is true (Hebrew was living liturgically). The piton perspective adds a temporal dimension: the constraint was legitimate during the early diaspora (when no alternative preservation mechanism existed) but degraded into theatrical maintenance as modern nation-states and mass education made vernacular Hebrew revival feasible. The analytical observer's mountain perspective is a false summit — it naturalizes the suppression as linguistic immutability rather than recognizing it as a contingent institutional choice.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint exhibits a perspectival inversion: from the rabbinic institutional perspective with arbitrage options, directionality is low (d ≈ 0.15) — the institution benefits from the constraint and experiences it as coordination. From the vernacular innovation perspective with trapped exit, directionality is high (d ≈ 0.95) — the agent is suppressed and experiences maximum extraction (Snare). From the community perspective with constrained exit, directionality is moderate (d ≈ 0.50) — the community both benefits from the liturgical coordination and is constrained from naturalizing Hebrew. The analytical perspective at civilizational scope (d ≈ 0.72) risks over-interpreting the suppression as natural linguistic law. No overrides are declared because the derived directionality values accurately reflect the structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandate-atrophy tension by showing that the 'living language' kernel admits multiple incompatible readings. The liturgical-only reading suppresses generative speech to preserve liturgical fixity — it cannot accommodate the native-daily reading (continuous productive speech in Hebrew) without dissolving. The native-daily reading (exemplified by modern Israeli Hebrew) demonstrates that productive speech was always possible, not linguistically blocked — the suppression was institutional policy. The scholarly-written reading (Medieval Hebrew philosophy and poetry) shows a third possibility: generative language use within the elite intellectual domain, separate from both daily vernacular and ritual liturgy. No single reading is 'correct' because the kernel is unstable across these three structural configurations. The mandate (preserve Hebrew) can be achieved through any of these mechanisms; atrophy occurs when one reading is enforced to the exclusion of others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    living_language_definition_ambiguity,
    'What defines ''living language'' — productive generation of new utterances by native speakers, or continuous use in any domain (including liturgical recitation only)?',
    'Definitional analysis of linguistic ''liveness'' in comparative literature on liturgical languages (Latin, Classical Chinese, Qur''anic Arabic). Examination of whether recitation alone, without productive speech generation, satisfies criteria for ''living'' status. Cross-reference linguistic anthropology standards.',
    'If ''living'' requires productive speech: liturgical-only reading is false (Hebrew was NOT living during 2000-year diaspora). If ''living'' permits recitation-only: reading is valid (continuous liturgical use sustains the language). Classification shifts from Rope/Piton to Mountain (false summit) depending on definition choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(living_language_definition_ambiguity, conceptual, 'Definitional ambiguity in ''living language'' criterion').

omega_variable(
    suppression_mechanism_intentionality,
    'Is the suppression of vernacular innovation an intentional policy enforced by rabbinic authorities, or an emergent property of treating Hebrew as inherently sacred and thus immutable?',
    'Textual analysis of halakic sources on Hebrew speech; examination of rabbinic prohibitions against vernacular Hebrew; comparison with unintentional drift in other liturgical languages (Latin in Catholic tradition). Determine whether authorities explicitly forbade vernacular or simply treated sacred language as off-limits to innovation.',
    'If intentional: constraint is actively maintained coordination (Rope, requires_active_enforcement=true). If emergent: constraint is a natural consequence of sanctity framing (Mountain). If mixed: Tangled Rope with enforcement layer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_intentionality, empirical, 'Whether suppression is intentional policy or emergent property of sacred status').

omega_variable(
    generative_capacity_and_liveness,
    'If Hebrew speakers in the diaspora had generated new poetry, philosophy, or daily speech in Hebrew within the liturgical framework (rather than suppressing vernacular), would the language have been ''living'' under this reading?',
    'Counterfactual: examine cases of liturgical languages that did develop productive speech within sacred contexts (e.g., Medieval Latin philosophical and theological innovation). If such cases count as ''living,'' the constraint is definitional (Rope). If they do not, the constraint requires suppression of innovation (Snare).',
    'If productive speech within liturgical domain would count: this reading is compatible with generative language and becomes less extractive. If not: this reading is fundamentally non-generative and the suppression of vernacular is structural to the claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generative_capacity_and_liveness, conceptual, 'Whether generative innovation within liturgical domain would satisfy ''living language'' criterion').

omega_variable(
    kernel_reading_contest,
    'What structural forces determine which reading of the ''Hebrew as living language'' kernel dominates at any given historical moment — liturgical-only, native-daily, or scholarly-written?',
    'Historical analysis of reading dominance: Talmudic period (scholarly-written emerging), Medieval diaspora (liturgical-only dominant), Enlightenment (scholarly-written resurging), 19th-20th centuries (native-daily emerging), modern Israel (native-daily dominant). Correlate dominance shifts with institutional power (rabbinic authority, secular nationalism, academic establishments) and material conditions (dispersion, nation-state formation, education accessibility).',
    'If dominance is power-determined: readings are incommensurable and each represents a different constraint instantiation (no single ''Hebrew living language'' — three separate constraints linked by network). If dominance is evidence-determined: readings are falsifiable and one may be correct. If dominance is path-dependent: readings form a temporal sequence with no final arbiter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Structural determinants of reading dominance in Hebrew language contests').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liturgical_only_reading, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(litu_tr_t0, liturgical_only_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(litu_tr_t500, liturgical_only_reading, theater_ratio, 500, 0.62).
narrative_ontology:measurement(litu_tr_t1000, liturgical_only_reading, theater_ratio, 1000, 0.68).

% Extraction over time
narrative_ontology:measurement(litu_be_t0, liturgical_only_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(litu_be_t500, liturgical_only_reading, base_extractiveness, 500, 0.1).
narrative_ontology:measurement(litu_be_t1000, liturgical_only_reading, base_extractiveness, 1000, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liturgical_only_reading, identity_coordination).
narrative_ontology:affects_constraint(liturgical_only_reading, native_daily_reading).
narrative_ontology:affects_constraint(liturgical_only_reading, scholarly_written_reading).

% DUAL FORMULATION NOTE:
% The 'Hebrew living language' kernel decomposes into three structurally distinct constraints, each with different ε values and different authority structures. The liturgical-only reading (this file, ε=0.12) is a Rope coordination mechanism. The native-daily reading (separate file, expected ε~0.08) is pure Rope (generative speech without extraction). The scholarly-written reading (separate file, expected ε~0.18) is Tangled Rope (coordination of elite textual innovation with controlled boundaries). All three inherit from the same kernel but instantiate different readings. The network edges capture the historical pressure between readings: vernacular revival (native-daily) invalidated the liturgical-only reading's claim to necessity; scholarly innovation (scholarly-written) demonstrated that generative Hebrew was possible within constrained domains. Each reading constrains the others' interpretive scope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
