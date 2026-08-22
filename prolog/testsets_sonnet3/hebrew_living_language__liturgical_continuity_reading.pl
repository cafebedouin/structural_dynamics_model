% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__liturgical_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_living_language__liturgical_continuity_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: hebrew_living_language__liturgical_continuity_reading
 *   human_readable: Hebrew as Living Language via Unbroken Liturgical Recitation and Textual Study
 *   domain: historical_linguistics/religious_practice/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates one reading of the contested 'Hebrew living
 *   language' kernel: the claim that Hebrew never ceased to be a living
 *   language because unbroken liturgical recitation and textual study across
 *   diaspora communities constituted continuous, generation-spanning
 *   transmission and use — distinct from the literary-revival reading
 *   (Haskalah written production) and the native-generation reading (which
 *   requires native daily speech). Under this reading's own lights, the
 *   standing arrangement is low-extraction voluntary coordination:
 *   communities recite and study without coercion, without rent extraction,
 *   and without a victim class. The constraint's ε is authored low and stable
 *   because recitation-based transmission genuinely functioned as claimed
 *   across the interval, with no significant drift toward extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__liturgical_continuity_reading, 0.08).
domain_priors:suppression_score(hebrew_living_language__liturgical_continuity_reading, 0.05).
domain_priors:theater_ratio(hebrew_living_language__liturgical_continuity_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__liturgical_continuity_reading, rope).
narrative_ontology:human_readable(hebrew_living_language__liturgical_continuity_reading, "Hebrew as Living Language via Unbroken Liturgical Recitation and Textual Study").
narrative_ontology:topic_domain(hebrew_living_language__liturgical_continuity_reading, "historical_linguistics/religious_practice/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__liturgical_continuity_reading, '0f6e4763-0203-4038-89c3-52994225d2fd').
narrative_ontology:cs_kernel_codification('0f6e4763-0203-4038-89c3-52994225d2fd', distributed).
narrative_ontology:cs_authority_grounding('0f6e4763-0203-4038-89c3-52994225d2fd', practice).
narrative_ontology:cs_interpretation_layer_present('0f6e4763-0203-4038-89c3-52994225d2fd').
narrative_ontology:cs_reading_relation('0f6e4763-0203-4038-89c3-52994225d2fd', hebrew_living_language__literary_revival_reading, coexists_with).
narrative_ontology:cs_reading_relation('0f6e4763-0203-4038-89c3-52994225d2fd', hebrew_living_language__native_generation_reading, coexists_with).
narrative_ontology:cs_axiom('0f6e4763-0203-4038-89c3-52994225d2fd', foundational, recitation_and_study_constitute_linguistic_life).
narrative_ontology:cs_axiom_status(recitation_and_study_constitute_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('0f6e4763-0203-4038-89c3-52994225d2fd', recitation_and_study_constitute_linguistic_life, conventional).
narrative_ontology:cs_axiom('0f6e4763-0203-4038-89c3-52994225d2fd', secondary, generative_daily_speech_not_required_for_vitality).
narrative_ontology:cs_axiom_status(generative_daily_speech_not_required_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('0f6e4763-0203-4038-89c3-52994225d2fd', generative_daily_speech_not_required_for_vitality, conventional).
narrative_ontology:cs_reference_frame('0f6e4763-0203-4038-89c3-52994225d2fd', post_temple_diaspora_liturgical_standard).
narrative_ontology:cs_drift_state('0f6e4763-0203-4038-89c3-52994225d2fd', contemporary_israeli_revival_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('0f6e4763-0203-4038-89c3-52994225d2fd', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, diaspora_jewish_communities).
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, rabbinic_scholarly_tradition).
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, liturgical_prayer_leaders).
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, textual_transmission_chain).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Recite the same liturgical Hebrew texts across generations and geographies without a shared vernacular. Participation is voluntary, communally reinforced, and requires no coercive apparatus; a community or individual can lapse into non-observance without penalty beyond social distance from the practice.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, diaspora_jewish_communities, beneficiary,
    organized, civilizational, mobile, global).

% Maintains the textual study apparatus (Talmud, midrash, commentary) that keeps Hebrew in continuous scholarly use. Their professional and religious identity is constituted through this study; exiting the tradition would mean abandoning the vocation itself, not merely a job.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, rabbinic_scholarly_tradition, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__liturgical_continuity_reading, rabbinic_scholarly_tradition, beneficiary).

% Lead recitation in synagogues; their communal role and modest status derive from fluency in liturgical Hebrew. They could stop leading services without material harm; the practice offers status and continuity, not rent extraction.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, liturgical_prayer_leaders, beneficiary,
    moderate, generational, mobile, regional).

% The unbroken chain of manuscript copying, printing, and memorization that has carried the liturgical corpus intact since antiquity. Not an actor itself, but the mechanism whose integrity the constraint is about.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, textual_transmission_chain, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(hebrew_living_language__liturgical_continuity_reading, textual_transmission_chain).

% Historical linguists and Hebraists who argue that recitation-without-generative-speech does not constitute a living language in the technical sense. They are not addressed by liturgical practice, which does not depend on their assent and proceeds independently of the linguistic-classification debate.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, native_generation_linguists, excluded,
    moderate, biographical, mobile, national).

% Study the kernel dispute itself — whether liturgical continuity, literary production, or native generative speech is the relevant criterion for language vitality — without a stake in any single reading's success.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, comparative_linguists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_living_language__liturgical_continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(hebrew_living_language__liturgical_continuity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sustains a shared textual and liturgical medium across geographically dispersed, politically fragmented communities for over two millennia, allowing continuity of prayer, law, and communal identity without requiring any single political authority or territory.
% TRANSFER_FUNCTION: No material transfer occurs. What moves is symbolic and mnemonic content — texts, cantillation, exegetical practice — passed from teacher to student and generation to generation. No party pays for another's benefit; participation is non-rivalrous.
% ABSENT_VOICES: Native-generation linguists and some Haskalah-descended secular Hebraists would object that recitation without generative daily speech does not meet the bar for 'living language' in a technical sense; they are not silenced, merely outside the liturgical community's frame of reference, and their objection does not bear on whether liturgical practice itself continues.
% DISAPPEARANCE_RATIONALE: If unbroken liturgical recitation and study vanished, diaspora communities would lose their primary continuous link to classical and rabbinic Hebrew; the textual transmission chain that fed both the Haskalah literary revival and the eventual native-speech revival in Palestine/Israel would be severed at its root — both sibling readings depend causally on this reading's persistence as their source material.
% FOUNDING_PROBLEM: After the loss of Hebrew as a majority vernacular in antiquity, diaspora communities needed a mechanism to keep the language of scripture, law, and prayer intact and mutually intelligible across dispersed and linguistically diverse communities, without a shared territory or state to enforce standardization.
% FOUNDING_PROBLEM_CORROBORATION: Comparative linguists and historians of the Hebrew language (outside the liturgical community itself) corroborate that continuous recitation and study preserved phonology, morphology, and lexicon in a form usable by later revivalists — this is treated as established in historical linguistics scholarship, not merely asserted by practitioners.
narrative_ontology:disappearance_verdict(hebrew_living_language__liturgical_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__liturgical_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__liturgical_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_living_language__liturgical_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__liturgical_continuity_reading, 0.08, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language__liturgical_continuity_reading_tests).
:- end_tests(hebrew_living_language__liturgical_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near-floor (0.05-0.08) because no party is compelled to participate and no party collects rents from others' participation; the practice is non-rivalrous symbolic transmission. Suppression is near-zero because exit (lapsing from observance) carries no institutional penalty. Theater ratio is modest and slowly rising (0.10-0.15) reflecting that some recitation, especially in later diaspora centuries, becomes rote/performative relative to full comprehension — a mild but real drift, not a reclassification driver. Accessibility collapse is moderate-low (0.2): alternatives to liturgical transmission (secular literacy, vernacular prayer) existed throughout diaspora history and were not suppressed, they were simply not chosen by the practicing core.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic/liturgical seat, the arrangement is pure coordination — a rope, not a snare or tangled rope, because there is no asymmetric extraction to detect. An observer applying the native-generation reading's criteria would classify the same historical record very differently (as failing to constitute a 'living' language at all), but that is a different constraint with a different claim, not a divergent seat-computation within this one.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora communities and rabbinic scholars are declared beneficiaries because the practice sustains what they value (continuity, identity, textual access) without extracting from any third party. There is no victim set under this reading — the expected structural delta explicitly specifies no victims, and none is authored here. Native-generation linguists sit outside the beneficiary/victim structure entirely; they are excluded from the conversation this reading is having, not harmed by it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining textual/linguistic continuity without territorial sovereignty) remains live by this reading's own corroborated account — comparative linguists independent of the liturgical community confirm the phonological and lexical continuity that later revival efforts drew upon. This forecloses a mandatrophy read: the mandate has not outlived its function, since the mechanism it enabled (source material for later revival) continued to matter after its immediate liturgical purpose might have seemed satisfied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    recitation_vs_generative_competence,
    'Does memorized/recited liturgical Hebrew, without generative daily production, constitute the language ''remaining living,'' or is this a preservation-of-symbol claim distinct from linguistic vitality in the technical sense?',
    'Historical linguistic analysis of whether liturgical Hebrew users across the diaspora period retained generative competence (ability to produce novel utterances) versus purely reproductive competence (recitation of fixed texts); comparison with documented cases of liturgical-only language communities elsewhere.',
    'If recitation is found to be purely reproductive with no generative competence, this reading''s continuity claim weakens relative to the native_generation_reading, which would then be the sole locus of ''living language'' status; if generative competence persisted in scholarly commentary and legal responsa (which do produce novel Hebrew text), the liturgical_continuity_reading''s claim strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recitation_vs_generative_competence, conceptual, 'Whether recitation-based transmission meets the bar for linguistic vitality or only for symbolic/cultural continuity.').

omega_variable(
    beneficiary_or_natural_fact,
    'Is the continuity of liturgical Hebrew best modeled as a genuine coordination benefit for identifiable communities (as authored here), or does it function closer to an inevitable byproduct of religious practice with no distinct ''beneficiary'' class at all?',
    'Examine whether communities that abandoned liturgical Hebrew (for vernacular prayer) suffered any documented loss of coordination benefit relative to those that retained it, controlling for other factors.',
    'If no measurable coordination benefit differential exists, the beneficiary declarations here should be read as weak/diffuse rather than concentrated, which would not change the rope classification but would affect confidence in the directionality derivation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_or_natural_fact, empirical, 'Whether the declared beneficiaries capture a real differential benefit or merely describe universal participants in a diffuse practice.').

omega_variable(
    sibling_reading_causal_priority,
    'Given that this reading is causally upstream of both sibling readings (literary revival drew on liturgical/scholarly Hebrew corpora; native generative revival drew on both), should the kernel network model this reading as foundational rather than co-equal?',
    'Trace documented citation and pedagogical lineages: did Haskalah writers and early Zionist revivalists (e.g. Ben-Yehuda) explicitly draw on liturgical/rabbinic corpora as their source material?',
    'If causal priority is established, the network edge direction (liturgical_continuity_reading -> literary_revival_reading -> native_generation_reading) should be authored as directional influence rather than symmetric coexistence, though the reading_relations here are authored as coexists_with per Rule 4''s diagnostic question (no logical foreclosure exists).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_causal_priority, conceptual, 'Whether causal/historical priority among readings should be reflected as directional influence in the network graph.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__liturgical_continuity_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hebr_tr_t400, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 400, 0.11).
narrative_ontology:measurement(hebr_tr_t800, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 800, 0.12).
narrative_ontology:measurement(hebr_tr_t1200, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1200, 0.13).
narrative_ontology:measurement(hebr_tr_t1600, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1600, 0.14).
narrative_ontology:measurement(hebr_tr_t2000, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 2000, 0.15).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(hebr_be_t400, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 400, 0.06).
narrative_ontology:measurement(hebr_be_t800, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 800, 0.06).
narrative_ontology:measurement(hebr_be_t1200, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1200, 0.07).
narrative_ontology:measurement(hebr_be_t1600, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1600, 0.08).
narrative_ontology:measurement(hebr_be_t2000, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 2000, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hebrew_living_language__liturgical_continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__liturgical_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_living_language__liturgical_continuity_reading, 0.06).
narrative_ontology:affects_constraint(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language__literary_revival_reading).
narrative_ontology:affects_constraint(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language__native_generation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the hebrew_living_language kernel (per the ε-invariance principle: 'Hebrew lives' conflates structurally distinct claims). liturgical_continuity_reading authors the lowest ε (0.08) and no victim set, reflecting a voluntary, non-extractive coordination story. literary_revival_reading and native_generation_reading will author their own ε and beneficiary/victim structures reflecting stricter vitality criteria and, potentially, different accessibility_collapse profiles (e.g. exclusion of non-literate or non-native-speaking Jews from the 'counts as living' status under stricter readings). All three share the kernel_id hebrew_living_language and are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
