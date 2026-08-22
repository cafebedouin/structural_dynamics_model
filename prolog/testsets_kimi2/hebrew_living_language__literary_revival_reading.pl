% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__literary_revival_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_living_language__literary_revival_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: hebrew_living_language__literary_revival_reading
 *   human_readable: Hebrew Living Language â Haskalah Literary Revival Reading
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   The Haskalah (Jewish Enlightenment, late 18thâ19th centuries) produced
 *   a substantial body of secular Hebrew literatureânewspapers, novels,
 *   poetry, and essaysâcreating a modern literary standard without a
 *   community of native daily speakers. This constraint story instantiates
 *   the 'literary revival' reading of the Hebrew living-language kernel: the
 *   claim that generative written competence in a networked literary public
 *   is sufficient for linguistic life. The reading treats the Haskalah not as
 *   a prelude to spoken revival but as a genuine locus of vitality.
 *   Extractiveness is minimal; the arrangement is voluntary elite
 *   coordination with no identifiable victim set.
 *
 * KEY AGENTS:
 *   - haskalah_literati: Primary agenda-setters (organized/mobile) â produce literature and sustain the generative network
 *   - hebrew_reading_public: Primary beneficiary (moderate/constrained) â receives modern Hebrew texts, sustains readership
 *   - traditional_liturgical_communities: Excluded voice (organized/identity_locked) â maintain competing liturgical continuity reading, sidelined in secular framework
 *   - native_generation_advocates: Excluded voice (organized/mobile) â later Zionist linguists who argue only mother-tongue speech counts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__literary_revival_reading, 0.1).
domain_priors:suppression_score(hebrew_living_language__literary_revival_reading, 0.1).
domain_priors:theater_ratio(hebrew_living_language__literary_revival_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__literary_revival_reading, rope).
narrative_ontology:human_readable(hebrew_living_language__literary_revival_reading, "Hebrew Living Language â Haskalah Literary Revival Reading").
narrative_ontology:topic_domain(hebrew_living_language__literary_revival_reading, "historical_linguistics/language_revitalization/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__literary_revival_reading, '2f6c7b36-9707-4902-a1af-4808618add0a').
narrative_ontology:cs_kernel_codification('2f6c7b36-9707-4902-a1af-4808618add0a', distributed).
narrative_ontology:cs_authority_grounding('2f6c7b36-9707-4902-a1af-4808618add0a', practice).
narrative_ontology:cs_reading_relation('2f6c7b36-9707-4902-a1af-4808618add0a', hebrew_living_language__liturgical_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('2f6c7b36-9707-4902-a1af-4808618add0a', hebrew_living_language__native_generation_reading, influences).
narrative_ontology:cs_axiom('2f6c7b36-9707-4902-a1af-4808618add0a', foundational, written_generative_competence_constitutes_linguistic_life).
narrative_ontology:cs_axiom_status(written_generative_competence_constitutes_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('2f6c7b36-9707-4902-a1af-4808618add0a', written_generative_competence_constitutes_linguistic_life, conventional).
narrative_ontology:cs_reference_frame('2f6c7b36-9707-4902-a1af-4808618add0a', haskalah_literary_praxis).
narrative_ontology:cs_drift_state('2f6c7b36-9707-4902-a1af-4808618add0a', post_zionist_native_establishment, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2f6c7b36-9707-4902-a1af-4808618add0a', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__literary_revival_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, haskalah_literati).
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, hebrew_reading_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produced novels, newspapers, and essays in modern Hebrew, inventing neologisms and secular genres. They coordinated a dispersed literary network across European cities, choosing Hebrew over German or Yiddish for ideological and prestige reasons. Exit meant assimilation into German culture or retreat into traditional study.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, haskalah_literati, agenda_setter,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__literary_revival_reading, haskalah_literati, beneficiary).

% Read Haskalah periodicals and books, gaining access to secular science, philosophy, and literature in Hebrew rather than European languages. Their participation sustained the market for Hebrew publication. Exit meant reading in Yiddish or German and losing access to the Jewish Enlightenment's particular cultural synthesis.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, hebrew_reading_public, beneficiary,
    moderate, biographical, constrained, continental).

% Maintained Hebrew for prayer, halakhic study, and liturgical poetry across the diaspora. Under the literary revival reading, their practice is recognized as continuity but not as the locus of linguistic life; they are not the intended beneficiaries of the secular modern literature project and would argue that Hebrew's vitality has always resided in Torah study.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, traditional_liturgical_communities, excluded,
    organized, generational, identity_locked, global).

% Later Zionist language planners and educators who argued that Hebrew could only be truly living when acquired by children as a mother tongue. They are structurally excluded from this reading's framework because the Haskalah model deliberately bypassed native speech in favor of written cultivation.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, native_generation_advocates, excluded,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables a dispersed Jewish intelligentsia to maintain a shared high-register language for secular modern discourseâphilosophy, science, journalism, and fictionâwithout relying on a co-territorial native speaker community.
% TRANSFER_FUNCTION: Moves cultural prestige and generative linguistic energy from traditional rabbinic study and from Yiddish/German vernaculars into a newly coined secular Hebrew literary register; transfers readership attention and intellectual production among a voluntary elite network.
% ABSENT_VOICES: Traditional liturgical communities who define Hebrew vitality through unbroken religious study and prayer; later Zionist educators who insist on mother-tongue acquisition as the sole criterion for a living language. Both are absent from the Haskalah reading's beneficiary structure.
% DISAPPEARANCE_RATIONALE: If the doctrine that written generative competence suffices for linguistic life vanished, the Haskalah cultural project would lose its defining self-justification; Hebrew would likely be reclassified as a liturgical or dead language in the secular sphere, and the literati would have lacked the ideological coordination to sustain modern production against the pull of German and Yiddish.
% FOUNDING_PROBLEM: Hebrew lacked a modern secular literary vehicle; Jewish Enlightenment intellectuals needed a high-status language of European-style discourse that was not Yiddish (seen as low-register) and not German (seen as assimilationist).
% FOUNDING_PROBLEM_CORROBORATION: Literary historians and Zionist linguists outside the immediate Haskalah beneficiary circle attest that modern Hebrew literature was indeed created; however, they contest whether the founding problem was 'Hebrew lacks modern literature' or 'Jews lack a modern national language,' with the latter framing rendering the Haskalah solution incomplete.
narrative_ontology:disappearance_verdict(hebrew_living_language__literary_revival_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__literary_revival_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__literary_revival_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_living_language__literary_revival_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__literary_revival_reading, 0.1, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language__literary_revival_reading_tests).
:- end_tests(hebrew_living_language__literary_revival_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.10) because the Haskalah literary network operated through voluntary participation and prestige rather than material extraction. Suppression is minimal (0.10): there was ideological friction with traditionalists and Yiddishists but no coercive apparatus enforcing Hebrew literary production. Theater ratio is low-moderate (0.18): some performative archaism in Haskalah writing (biblical imitation), but genuine semantic and syntactic innovation occurred. Accessibility collapse is moderate (0.25) because mastery of rabbinic and biblical Hebrew was required to participate, yet alternatives (Yiddish, German, traditional study) remained open. Resistance (0.30) reflects rabbinic opposition to secularization of the holy tongue, not extraction resistance.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (Haskalah writers) experiences the constraint as creative liberation and cultural mission; the reading public experiences it as edification and entertainment; excluded traditionalist and Zionist seats experience it as a misallocation of Hebrew legitimacy. The engine will compute low directionality for beneficiaries and higher for excluded observers, but because no party is structurally trapped or materially extracted, the divergence is perspectival rather than adversarial.
 *
 * DIRECTIONALITY LOGIC:
 *   Haskalah literati are beneficiaries with high mobility (could write in German/Yiddish) but choose Hebrewâdirectionality near the beneficiary end. Hebrew reading public benefits from access to modern literature but is constrained by literacy requirementsâdirectionality slightly above symmetric. Traditional liturgical communities and native-generation advocates are not victims (no extraction flows from them) but are structurally excluded from the beneficiary set of this reading; their directionality is not computed as target because no victim declaration exists. The absence of victims keeps effective extraction low across all seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this coordination as extraction because there is no enforcement mechanism, no concentrated rent, and no trapped population. It also prevents mislabeling it as a mountain: Hebrew literary life is a constructed cultural project, not a natural law. The rope classification captures that the Haskalah solved a genuine collective-action problemâmaintaining a modern Hebrew register among dispersed elitesâwithout coercive overhead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    written_competence_reachability_ambiguity,
    'Does written generative competence in an elite literary circle constitute strict linguistic reachability (the ability to generate novel expressions in real communicative contexts), or does it produce only a parasitic or simulacral vitality dependent on source-text memorization?',
    'Corpus-linguistic analysis of Haskalah texts measuring neologism rate, syntactic innovation, and pragmatic range against contemporaneous spoken languages; ethnographic reconstruction of whether texts were composed with native-like fluency or heavy reliance on biblical templates.',
    'If the competence is strictly generative and novel, the reading qualifies as rope-like coordination sustaining genuine linguistic life; if it is primarily mimetic or template-bound, the reading approaches a theatrical performance of vitality (piton-like) and the extraction metric should be revised upward for the cultural prestige extracted under false pretenses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(written_competence_reachability_ambiguity, empirical, 'Whether Haskalah written competence was genuinely generative or mimetic.').

omega_variable(
    kernel_reading_sibling_boundary,
    'Does the literary revival reading foreclose the native generation reading, or do they describe different temporal phases of Hebrew vitality?',
    'Historical analysis of Haskalah self-understanding: did Haskalah writers claim Hebrew was fully living, or merely being revived/prepped for future native life?',
    'If they claimed full life, the readings are logically competitive; if they claimed preparation, the native generation reading is a successor scaffold and this reading is transitional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_sibling_boundary, conceptual, 'Structural relationship between literary and native readings.').

omega_variable(
    living_language_definition_ontology,
    'Is ''living language'' a natural-kind category discoverable by linguistics, or a normative status assigned by cultural commitment?',
    'Cross-linguistic comparison of revitalization cases (e.g., Sanskrit, Latin, Cornish) to see if ''living'' admits stable extensional definition.',
    'If natural-kind, the reading makes an empirical claim; if normative, the constraint is a commitment system and classification should weight conventionality over physical-law metrics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(living_language_definition_ontology, conceptual, 'Ontological status of the living language category.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__literary_revival_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_living_language__literary_revival_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hebr_tr_t20, hebrew_living_language__literary_revival_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(hebr_tr_t40, hebrew_living_language__literary_revival_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(hebr_tr_t60, hebrew_living_language__literary_revival_reading, theater_ratio, 60, 0.18).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_living_language__literary_revival_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(hebr_be_t20, hebrew_living_language__literary_revival_reading, base_extractiveness, 20, 0.11).
narrative_ontology:measurement(hebr_be_t40, hebrew_living_language__literary_revival_reading, base_extractiveness, 40, 0.13).
narrative_ontology:measurement(hebr_be_t60, hebrew_living_language__literary_revival_reading, base_extractiveness, 60, 0.1).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hebrew_living_language__literary_revival_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__literary_revival_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_living_language__literary_revival_reading, hebrew_living_language__liturgical_continuity_reading).
narrative_ontology:affects_constraint(hebrew_living_language__literary_revival_reading, hebrew_living_language__native_generation_reading).

% DUAL FORMULATION NOTE:
% The hebrew_living_language kernel decomposes into three structurally distinct constraints: the liturgical_continuity reading (ritual maintenance), the literary_revival reading (Haskalah written generative competence), and the native_generation reading (mother-tongue speech). Each has a different epsilon, different beneficiary structure, and different scope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
