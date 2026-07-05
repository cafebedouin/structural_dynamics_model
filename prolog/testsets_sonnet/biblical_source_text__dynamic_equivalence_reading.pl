% ============================================================================
% CONSTRAINT STORY: biblical_source_text__dynamic_equivalence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_source_text__dynamic_equivalence_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: biblical_source_text__dynamic_equivalence_reading
 *   human_readable: Dynamic Equivalence Reading of the Biblical Source Text Kernel
 *   domain: religious/linguistic/institutional
 *
 * SUMMARY:
 *   This constraint represents the dynamic-equivalence reading of the
 *   contested 'biblical source text' kernel: the commitment that a
 *   translation's primary obligation is to reproduce the communicative EFFECT
 *   of the source text in the receptor language, subordinating morphological
 *   and syntactic structure to intelligibility and pastoral/evangelistic
 *   mission. This is one of three structurally distinct readings sharing the
 *   same kernel — the formal-equivalence reading inverts the priority
 *   (structure primary, intelligibility a teaching responsibility), and the
 *   critical-reconstructive reading defers both questions pending
 *   establishment of the most probable original text through textual
 *   criticism. These are not three measurements of one constraint; they are
 *   three different constraints instantiated from the same underlying
 *   commitment to 'the source text,' each with its own ε, beneficiary/victim
 *   structure, and institutional apparatus. This story covers ONLY the
 *   dynamic-equivalence reading.
 *
 * KEY AGENTS:
 *   - missionary_translation_agencies: institutional agenda-setter administering methodology and training
 *   - lay_readers: powerless beneficiaries receiving intelligible text
 *   - evangelistic_publishing_houses: organized beneficiary/agenda-setter with market incentive toward readability
 *   - philological_scholars: moderate-power payers whose word-study precision is degraded
 *   - liturgical_traditionalist_communities: moderate-power payers losing fixed textual anchoring
 *   - minority_language_communities_with_thin_review_capacity: powerless, trapped payers with least capacity to contest interpretive choices made on their behalf
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_source_text__dynamic_equivalence_reading, 0.42).
domain_priors:suppression_score(biblical_source_text__dynamic_equivalence_reading, 0.31).
domain_priors:theater_ratio(biblical_source_text__dynamic_equivalence_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_source_text__dynamic_equivalence_reading, tangled_rope).
narrative_ontology:human_readable(biblical_source_text__dynamic_equivalence_reading, "Dynamic Equivalence Reading of the Biblical Source Text Kernel").
narrative_ontology:topic_domain(biblical_source_text__dynamic_equivalence_reading, "religious/linguistic/institutional").

domain_priors:requires_active_enforcement(biblical_source_text__dynamic_equivalence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_source_text__dynamic_equivalence_reading, 'ae6dd91a-f82d-4e2f-9d95-65f9ebff09a8').
narrative_ontology:cs_kernel_codification('ae6dd91a-f82d-4e2f-9d95-65f9ebff09a8', distributed).
narrative_ontology:cs_authority_grounding('ae6dd91a-f82d-4e2f-9d95-65f9ebff09a8', practice).
narrative_ontology:cs_interpretation_layer_present('ae6dd91a-f82d-4e2f-9d95-65f9ebff09a8').
narrative_ontology:cs_reading_relation('ae6dd91a-f82d-4e2f-9d95-65f9ebff09a8', biblical_source_text__formal_equivalence_reading, coexists_with).
narrative_ontology:cs_reading_relation('ae6dd91a-f82d-4e2f-9d95-65f9ebff09a8', biblical_source_text__critical_reconstructive_reading, influences).
narrative_ontology:cs_axiom('ae6dd91a-f82d-4e2f-9d95-65f9ebff09a8', foundational, communicative_effect_constitutes_fidelity).
narrative_ontology:cs_axiom_status(communicative_effect_constitutes_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('ae6dd91a-f82d-4e2f-9d95-65f9ebff09a8', communicative_effect_constitutes_fidelity, instrumental).
narrative_ontology:cs_axiom('ae6dd91a-f82d-4e2f-9d95-65f9ebff09a8', secondary, receptor_intelligibility_is_pastoral_obligation).
narrative_ontology:cs_axiom_status(receptor_intelligibility_is_pastoral_obligation, holdable).
narrative_ontology:cs_axiom_grounding('ae6dd91a-f82d-4e2f-9d95-65f9ebff09a8', receptor_intelligibility_is_pastoral_obligation, conventional).
narrative_ontology:cs_reference_frame('ae6dd91a-f82d-4e2f-9d95-65f9ebff09a8', nida_functional_equivalence_paradigm).
narrative_ontology:cs_drift_state('ae6dd91a-f82d-4e2f-9d95-65f9ebff09a8', contemporary_translation_studies_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('ae6dd91a-f82d-4e2f-9d95-65f9ebff09a8', '').
narrative_ontology:cs_kernel_id(biblical_source_text__dynamic_equivalence_reading, biblical_source_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, lay_readers).
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, missionary_translation_agencies).
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, evangelistic_publishing_houses).
narrative_ontology:constraint_victim(biblical_source_text__dynamic_equivalence_reading, philological_scholars).
narrative_ontology:constraint_victim(biblical_source_text__dynamic_equivalence_reading, liturgical_traditionalist_communities).
narrative_ontology:constraint_victim(biblical_source_text__dynamic_equivalence_reading, minority_language_communities_with_thin_review_capacity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set translation methodology (functional/dynamic equivalence protocols such as those descended from Nida's work), train field translators, fund and administer translation projects across thousands of language communities, and adjudicate which renderings are 'faithful enough' for publication. They control the institutional apparatus that decides whose readings become the received text for a given language.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, missionary_translation_agencies, agenda_setter,
    institutional, generational, arbitrage, global).

% Receive a text in idiomatic, comprehensible language without needing training in Koine Greek, Biblical Hebrew, or historical philology. Comprehension and devotional/pastoral use are served directly. They have almost no capacity to evaluate what has been smoothed over or interpretively resolved on their behalf, and typically no exit to an alternative rendering in their own language.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, lay_readers, beneficiary,
    powerless, biographical, constrained, local).

% Produce and market dynamic-equivalence translations at scale, benefiting commercially and institutionally from wide readability and adoption. They shape which translation philosophy dominates a market by funding, distributing, and promoting texts that read easily, and can shift resources toward whichever methodology sells or evangelizes best.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, evangelistic_publishing_houses, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__dynamic_equivalence_reading, evangelistic_publishing_houses, agenda_setter).

% Require access to morphological, syntactic, and lexical detail preserved in the source languages for word studies, textual argument, and theological precision. Dynamic equivalence renderings systematically discard or collapse these features in favor of sense-for-sense readability, degrading the text as a scholarly instrument. Their exit is to work from source-language originals or formal-equivalence translations, but the dominant devotional and liturgical text in most languages is dynamic-equivalence, which shapes lay theological vocabulary they must then work against.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, philological_scholars, payer,
    moderate, biographical, constrained, global).

% Depend on precise, stable, repeatable wording for liturgy, memorization, and doctrinal formulation across generations. Dynamic equivalence's variability across editions and its subordination of exact phrasing to contemporary intelligibility erodes the fixed textual anchor their practice requires. They can resist by adopting formal-equivalence liturgical texts, but this fragments the community from more broadly circulated popular editions.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, liturgical_traditionalist_communities, payer,
    moderate, generational, constrained, national).

% Receive translations produced under dynamic-equivalence methodology by agencies with far greater institutional resources and interpretive authority than any local reviewer can independently check. Interpretive choices smoothing ambiguity or resolving theological difficulty are made largely outside their control, and they lack the comparative linguistic infrastructure to contest specific renderings or demand a more literal alternative.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, minority_language_communities_with_thin_review_capacity, payer,
    powerless, generational, trapped, local).

% The sibling reading of the same kernel that treats structural fidelity as primary. It is not a party to this constraint's operation but is displaced from institutional dominance wherever dynamic-equivalence methodology captures translation committees, funding, and market share.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, formal_equivalence_reading, excluded,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(biblical_source_text__dynamic_equivalence_reading, formal_equivalence_reading).

% Studies translation methodology comparatively, publishes critiques of both dynamic- and formal-equivalence approaches, and can document where dynamic equivalence's interpretive choices embed particular theological or cultural assumptions not present in the source.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, biblical_scholarship_community, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine problem that a source text in Koine Greek, Biblical Hebrew, and Aramaic is unintelligible to the overwhelming majority of readers and hearers; dynamic equivalence coordinates translators, reviewers, and publishers around a shared methodology for producing readable text quickly across many languages, enabling access and evangelistic/pastoral use at a scale formal philological training could never support.
% TRANSFER_FUNCTION: Moves interpretive authority from the reader (who could in principle wrestle with ambiguity in a more literal rendering) to the translation committee, which resolves ambiguity, smooths syntax, and selects among possible senses on the reader's behalf. It also moves institutional and market authority toward agencies and publishers whose methodology dominates, away from scholarly communities and traditionalist liturgical bodies whose textual precision requirements are subordinated.
% ABSENT_VOICES: Speakers of minority languages receiving a translation for the first time have essentially no voice in the methodological choice or in reviewing the specific interpretive resolutions made in their text; local church leadership is typically consulted for cultural sensitivity but rarely has the comparative-linguistic standing to contest specific renderings against the source.
% DISAPPEARANCE_RATIONALE: If dynamic-equivalence methodology vanished as an institutional practice, the vast majority of translation projects into lower-resource languages would either stall (lacking translators trained in formal-equivalence philological method) or default to interlinear/highly literal renderings unintelligible to ordinary readers; missionary and lay devotional access to scripture would contract sharply, while scholarly and liturgical-traditionalist communities would regain a cleaner text for their purposes.
% FOUNDING_PROBLEM: Mid-20th-century linguists and missionaries (notably Eugene Nida) observed that literal, word-for-word translations produced grammatically foreign, often incomprehensible or misleading text in receptor languages with different structures, idioms, and cultural referents — intelligibility itself, not just accuracy, was failing.
% FOUNDING_PROBLEM_CORROBORATION: Field linguists and literacy researchers outside missionary translation agencies (in general translation studies and sociolinguistics) corroborate that structurally dissimilar target languages genuinely produce unintelligible calques under strict formal equivalence — this is not solely attested by the agencies that benefit from the dynamic-equivalence market. However, the SPECIFIC degree of interpretive smoothing practiced by any given translation committee is attested mainly by the agencies and publishers themselves; independent comparative philologists outside those institutions frequently contest particular resolutions as exceeding what intelligibility required.
narrative_ontology:disappearance_verdict(biblical_source_text__dynamic_equivalence_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_source_text__dynamic_equivalence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_source_text__dynamic_equivalence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_source_text__dynamic_equivalence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_source_text__dynamic_equivalence_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_source_text__dynamic_equivalence_reading_tests).
:- end_tests(biblical_source_text__dynamic_equivalence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than high because the coordination function is genuine and substantial — comprehension gains for the overwhelming majority of readers are real and large, not merely cover for extraction. But it is not negligible: interpretive resolution of ambiguity is inherently a transfer of authority from reader to committee, and the loss of morphological/syntactic precision is a real, structural cost systematically borne by scholarly and liturgical-traditionalist communities who did not choose this trade-off. Suppression is lower (0.31) than extraction because dynamic equivalence does not typically foreclose access to formal-equivalence alternatives in well-resourced languages (English readers can choose an ESV or NASB) — suppression is concentrated specifically in minority-language contexts where only one translation exists and no comparative check is possible, which is a scope-and-resource effect rather than an inherent design suppression. Theater ratio is low-moderate and rising slowly (0.22), reflecting some accretion of institutional self-justification (translation philosophy conferences, methodology defenses) without dominant performative capture. The measurement series shows all three metrics on a shared 1960-2025 grid, rising modestly as dynamic equivalence institutionalized (Nida's theoretical work in the 1960s, its adoption by major Bible societies through the late 20th century, and its consolidation as the dominant paradigm for missionary translation by the 2000s).
 *
 * PERSPECTIVAL GAP:
 *   From the missionary agency and lay-reader seats, this reading is experienced as pure coordination — solving a real access problem with minimal apparent cost. From the philological-scholar and minority-language-community seats, the same structure is experienced as an imposed interpretive filter whose costs (lost precision, unreviewable resolution of ambiguity) are structural rather than incidental. The engine computes these divergent seat classifications from the declared power/exit/beneficiary data; this story does not adjudicate which seat is 'right.'
 *
 * DIRECTIONALITY LOGIC:
 *   Missionary translation agencies and evangelistic publishers are structural beneficiaries: they administer the methodology, capture institutional legitimacy and market share from it, and bear little of its cost (their exit options are arbitrage/mobile — they can shift methodology or markets). Lay readers are beneficiaries in the sense that intelligibility is delivered to them, but they are powerless and have essentially no capacity to evaluate what has been altered, which keeps them structurally dependent rather than in control. Philological scholars and liturgical traditionalists are payers: real costs (loss of precision, loss of textual stability) land on them specifically because the methodology's design goal is orthogonal to their needs. Minority-language communities are the most severely positioned payers: trapped exit options combined with powerlessness means the interpretive choices embedded in their translation are essentially unreviewable by them, which is the sharpest asymmetry in the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — literal translation producing unintelligible or misleading text in structurally dissimilar receptor languages — remains genuinely live wherever new translation work targets low-resource or typologically distant languages; this is not a case of an obsolete mandate persisting by inertia. Classifying this as tangled_rope rather than snare or rope prevents two mislabelings: calling it a pure snare would ignore the real, large, and continuing coordination benefit to lay comprehension and evangelistic access, which the founding-problem corroboration from outside missionary agencies supports as genuine; calling it a pure rope would ignore the asymmetric, structural cost concentrated on scholars, liturgical communities, and especially minority-language populations who bear interpretive decisions made without their meaningful participation. The tangled_rope classification requires both: genuine coordination (intelligibility, present) and asymmetric extraction requiring active enforcement (methodological gatekeeping by agencies, present) — both conditions hold here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intelligibility_gain_vs_interpretive_cost_tradeoff,
    'Is the comprehension gain delivered by dynamic equivalence to lay and minority-language readers structurally proportionate to the interpretive authority transferred away from the reader and the precision lost to scholarly/liturgical use, or does the methodology systematically over-resolve ambiguity beyond what intelligibility strictly requires?',
    'Comparative studies (already underway in translation studies literature) measuring specific renderings against source-language ambiguity: cases where a single interpretive choice was imposed where the source genuinely supports multiple readings, versus cases where the source is genuinely univocal and dynamic equivalence merely clarified syntax.',
    'If systematic over-resolution is documented at scale, this reading''s extraction is higher than currently authored and it moves closer to snare; if resolution choices track genuine source ambiguity requiring some choice regardless of method, the coordination function is more nearly load-bearing and extraction is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intelligibility_gain_vs_interpretive_cost_tradeoff, empirical, 'Whether interpretive smoothing exceeds what intelligibility genuinely requires.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is ''the biblical source text'' as an authority object best modeled as a single kernel with three competing readings (this story''s framing), or are formal-equivalence, dynamic-equivalence, and critical-reconstructive approaches actually answering three different questions (what does the text SAY structurally, what does it MEAN communicatively, what IS the original text) such that they are not truly in competition but address orthogonal concerns?',
    'Examine whether translation committees and denominational bodies that adopt one reading treat the others as wrong (competitive framing) or as complementary tools for different purposes (orthogonal framing) — institutional practice (e.g., study Bibles combining formal-equivalence text with dynamic-equivalence notes) suggests some orthogonality.',
    'If the readings are genuinely orthogonal rather than competing, the cs_pattern classification of foreclosure/coexistence should weight toward coexists_with even more strongly than authored, and the extraction attributed to any one reading should be discounted by the extent institutions use multiple readings jointly rather than choosing one exclusively.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the three kernel readings are genuinely competing or address different, complementary questions.').

omega_variable(
    minority_language_review_capacity_ambiguity,
    'For minority-language communities with thin review capacity, is the suppression measured here better characterized as structural (no comparative text exists to check against) or as a resource/capacity gap that could in principle be closed without changing the dynamic-equivalence methodology itself?',
    'Track whether translation consultant programs and back-translation review processes (already used by some agencies) measurably reduce contested-rendering rates in a given language community over time; a durable reduction would indicate the suppression is capacity-driven and remediable, not intrinsic to the methodology.',
    'If capacity-driven, targeted investment in local review infrastructure could substantially reduce this reading''s effective extraction on the most vulnerable stakeholder without abandoning dynamic equivalence; if structural, the suppression is inherent to the methodology''s reliance on centralized interpretive authority regardless of resourcing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_language_review_capacity_ambiguity, empirical, 'Whether suppression on minority-language communities is a remediable capacity gap or intrinsic to the methodology.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_source_text__dynamic_equivalence_reading, 1960, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t1960, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(bibl_tr_t1975, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 1975, 0.13).
narrative_ontology:measurement(bibl_tr_t1990, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 1990, 0.16).
narrative_ontology:measurement(bibl_tr_t2005, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 2005, 0.19).
narrative_ontology:measurement(bibl_tr_t2015, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 2015, 0.21).
narrative_ontology:measurement(bibl_tr_t2025, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(bibl_be_t1960, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 1960, 0.28).
narrative_ontology:measurement(bibl_be_t1975, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 1975, 0.33).
narrative_ontology:measurement(bibl_be_t1990, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 1990, 0.37).
narrative_ontology:measurement(bibl_be_t2005, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 2005, 0.4).
narrative_ontology:measurement(bibl_be_t2015, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 2015, 0.41).
narrative_ontology:measurement(bibl_be_t2025, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t1960, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 1960, 0.18).
narrative_ontology:measurement(bibl_su_t1975, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 1975, 0.22).
narrative_ontology:measurement(bibl_su_t1990, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 1990, 0.26).
narrative_ontology:measurement(bibl_su_t2005, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 2005, 0.29).
narrative_ontology:measurement(bibl_su_t2015, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 2015, 0.3).
narrative_ontology:measurement(bibl_su_t2025, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 2025, 0.31).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_source_text__dynamic_equivalence_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_source_text__dynamic_equivalence_reading, biblical_source_text__formal_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__dynamic_equivalence_reading, biblical_source_text__critical_reconstructive_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'fidelity to the biblical source text' per the ε-invariance principle: the label conflates a claim about structural correspondence (formal_equivalence_reading), a claim about communicative effect (this story, dynamic_equivalence_reading), and a claim about establishing the correct original text prior to either question (critical_reconstructive_reading). Each has a distinct ε, distinct beneficiary/victim sets, and distinct institutional apparatus; they are linked here rather than merged because measuring 'fidelity to the source text' by different observables (structural correspondence vs. communicative effect vs. textual-critical warrant) yields different ε values, which by the ε-invariance principle means they are different constraints, not one constraint viewed three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
