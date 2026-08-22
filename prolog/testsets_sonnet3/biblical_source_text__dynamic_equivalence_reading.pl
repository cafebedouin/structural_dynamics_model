% ============================================================================
% CONSTRAINT STORY: biblical_source_text__dynamic_equivalence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: biblical_source_text__dynamic_equivalence_reading
 *   human_readable: Dynamic Equivalence Reading of the Biblical Source Text Kernel
 *   domain: religious/linguistic/institutional
 *
 * SUMMARY:
 *   This constraint instantiates the dynamic-equivalence reading of the
 *   contested 'biblical source text' kernel: the commitment that a
 *   translation's primary obligation is communicative effect in the
 *   receiver's language and pastoral usability, with source-language
 *   structural features (word order, morphological repetition, grammatical
 *   form) subordinated whenever they impede intelligibility. This reading is
 *   institutionalized in major Bible societies and translation training
 *   pipelines (Nida's functional-equivalence tradition and descendants). It
 *   is one of three structurally distinct readings of the same underlying
 *   kernel — the formal-equivalence reading and the critical-reconstructive
 *   reading are separate constraints with their own ε values, not alternate
 *   measurements of this one.
 *
 * KEY AGENTS:
 *   - missionary_translation_agencies: institutional agenda-setter administering the methodology
 *   - lay_readers and unreached_language_communities: primary beneficiaries of the comprehensibility gain
 *   - academic_biblical_scholars and word_study_dependent_clergy: bear the cost of lost morphological precision
 *   - formal_equivalence_translation_bodies: excluded rival authority within the same institutional space
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_source_text__dynamic_equivalence_reading, 0.45).
domain_priors:suppression_score(biblical_source_text__dynamic_equivalence_reading, 0.38).
domain_priors:theater_ratio(biblical_source_text__dynamic_equivalence_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_source_text__dynamic_equivalence_reading, tangled_rope).
narrative_ontology:human_readable(biblical_source_text__dynamic_equivalence_reading, "Dynamic Equivalence Reading of the Biblical Source Text Kernel").
narrative_ontology:topic_domain(biblical_source_text__dynamic_equivalence_reading, "religious/linguistic/institutional").

domain_priors:requires_active_enforcement(biblical_source_text__dynamic_equivalence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_source_text__dynamic_equivalence_reading, 'b6384e71-6d6a-4f39-ad9d-4bc841baef24').
narrative_ontology:cs_kernel_codification('b6384e71-6d6a-4f39-ad9d-4bc841baef24', distributed).
narrative_ontology:cs_authority_grounding('b6384e71-6d6a-4f39-ad9d-4bc841baef24', practice).
narrative_ontology:cs_interpretation_layer_present('b6384e71-6d6a-4f39-ad9d-4bc841baef24').
narrative_ontology:cs_reading_relation('b6384e71-6d6a-4f39-ad9d-4bc841baef24', biblical_source_text__formal_equivalence_reading, coexists_with).
narrative_ontology:cs_reading_relation('b6384e71-6d6a-4f39-ad9d-4bc841baef24', biblical_source_text__critical_reconstructive_reading, influences).
narrative_ontology:cs_axiom('b6384e71-6d6a-4f39-ad9d-4bc841baef24', foundational, communicative_effect_constitutes_fidelity).
narrative_ontology:cs_axiom_status(communicative_effect_constitutes_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('b6384e71-6d6a-4f39-ad9d-4bc841baef24', communicative_effect_constitutes_fidelity, instrumental).
narrative_ontology:cs_axiom('b6384e71-6d6a-4f39-ad9d-4bc841baef24', foundational, pastoral_accessibility_overrides_formal_correspondence).
narrative_ontology:cs_axiom_status(pastoral_accessibility_overrides_formal_correspondence, holdable).
narrative_ontology:cs_axiom_grounding('b6384e71-6d6a-4f39-ad9d-4bc841baef24', pastoral_accessibility_overrides_formal_correspondence, instrumental).
narrative_ontology:cs_reference_frame('b6384e71-6d6a-4f39-ad9d-4bc841baef24', nida_functional_equivalence_paradigm).
narrative_ontology:cs_drift_state('b6384e71-6d6a-4f39-ad9d-4bc841baef24', contemporary_translation_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b6384e71-6d6a-4f39-ad9d-4bc841baef24', '').
narrative_ontology:cs_kernel_id(biblical_source_text__dynamic_equivalence_reading, biblical_source_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, lay_readers).
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, missionary_translation_agencies).
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, unreached_language_communities).
narrative_ontology:constraint_victim(biblical_source_text__dynamic_equivalence_reading, academic_biblical_scholars).
narrative_ontology:constraint_victim(biblical_source_text__dynamic_equivalence_reading, word_study_dependent_clergy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets translation methodology (Nida-derived functional/dynamic equivalence) as the operating standard for field translation projects, trains translators in it, and funds ongoing production across thousands of language groups. Justifies the approach by pastoral urgency: getting a comprehensible text into a community's hands is prioritized over preserving source-language morphology. Controls which methodology gets institutional resources and staffing.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, missionary_translation_agencies, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__dynamic_equivalence_reading, missionary_translation_agencies, beneficiary).

% Read the translated text in their own idiom without formal theological training. Gain immediate comprehension of narrative and exhortation but have no independent way to check whether the receptor-language phrasing has smoothed over ambiguities, wordplay, or grammatical structures present in the source. Their access to the text depends entirely on the translator's choices; they cannot evaluate what was subordinated to achieve readability.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, lay_readers, beneficiary,
    powerless, biographical, trapped, local).

% Previously had no scripture at all in their language. A dynamic-equivalence translation gives them a first text where a formally-equivalent one might remain unintelligible or never get produced given resource constraints. The pastoral mission's success is measured partly by reaching these communities, which structurally requires the methodology this reading endorses.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, unreached_language_communities, beneficiary,
    powerless, generational, trapped, local).

% Rely on precise morphological and syntactic reproduction to conduct word studies, trace intertextual echoes, and adjudicate exegetical disputes. When dynamic-equivalence translations dominate popular use and shape lay theological vocabulary, the scholarly apparatus built on formal correspondence becomes harder to communicate to congregations already formed by paraphrase-level renderings. They cannot simply exit the discourse — their work depends on engaging texts and readerships already shaped by the dominant translation philosophy.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, academic_biblical_scholars, payer,
    organized, civilizational, constrained, global).

% Preach and teach using close lexical analysis of source terms. When congregations carry dynamic-equivalence Bibles that render a single source term multiple different ways for readability, sermons built on verbal repetition or key-word tracing become harder to ground in the pew text, forcing extra translation work in every teaching moment.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, word_study_dependent_clergy, payer,
    moderate, biographical, constrained, national).

% Advocate that fidelity to source structure should be primary and that intelligibility is properly the responsibility of teaching and community formation, not the translation itself. They compete for the same funding, denominational endorsement, and pew space as dynamic-equivalence products but are not the authority within this reading's institutional apparatus.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, formal_equivalence_translation_bodies, excluded,
    organized, generational, constrained, global).

% Evaluate competing translation philosophies when selecting or endorsing a pew Bible, weighing pastoral accessibility against fidelity claims, and can shift institutional endorsement between methodologies.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, denominational_publishing_committees, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_source_text__dynamic_equivalence_reading, missionary_translation_agencies).
narrative_ontology:fixing_cost_class(biblical_source_text__dynamic_equivalence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine problem of making scripture comprehensible to readers without access to the source languages or extensive theological training, especially in missionary contexts producing a community's first-ever translation.
% TRANSFER_FUNCTION: Moves interpretive authority from the reader's direct encounter with source-language structure to the translator's judgment about what target-language rendering best conveys the intended effect — shifting precision-dependent authority away from scholars and toward translation committees and the missionary institutions that train them.
% ABSENT_VOICES: Formal-equivalence advocates and text-critical scholars are structurally present in academic debate but largely absent from the committees and field-training pipelines that actually produce dynamic-equivalence translations; lay readers, who bear the consequence of smoothed ambiguity, have no mechanism to know what was subordinated on their behalf.
% DISAPPEARANCE_RATIONALE: If dynamic-equivalence methodology vanished, missionary agencies argue entire unreached communities would remain without accessible scripture, arresting the pastoral mission; scholars and formal-equivalence advocates argue the world would rearrange toward more textually disciplined translations and stronger connection between lay reading and the source, disputing that comprehensibility genuinely requires this degree of structural subordination.
% FOUNDING_PROBLEM: Mid-20th-century missionary linguistics (principally Eugene Nida's work at the American Bible Society) confronted languages and cultures where literal, structure-preserving translation produced texts that were technically accurate but functionally incomprehensible or actively misleading to receptor communities.
% FOUNDING_PROBLEM_CORROBORATION: Missionary linguists and Bible societies attest the problem remains fully live across thousands of under-translated languages. Independent corroboration from comparative linguistics and anthropology (outside the missionary institutions) supports that receptor-language intelligibility is a genuine translation problem, not a manufactured one — but text-critical scholars, from outside the beneficiary set, contest that the problem justifies the degree of structural subordination now standard in dynamic-equivalence practice, arguing the pendulum overcorrected past what intelligibility actually requires.
narrative_ontology:disappearance_verdict(biblical_source_text__dynamic_equivalence_reading, contested).
narrative_ontology:founding_problem_status(biblical_source_text__dynamic_equivalence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_source_text__dynamic_equivalence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_source_text__dynamic_equivalence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_source_text__dynamic_equivalence_reading, 0.45, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is moderate (0.45) because the reading genuinely delivers a working coordination good (comprehension for a first-time or under-resourced readership) while measurably degrading a resource (structural precision) that a specific, organized group depends on. Suppression is moderate (0.38): scholars and formal-equivalence advocates are not silenced, but the institutional funding and training apparatus concentrates resources on the dynamic-equivalence pipeline, narrowing the practical space for competing methodologies in field translation. Accessibility collapse is moderate (0.4) — formal-equivalence translations remain available for those who seek them, so alternatives are not eliminated, only relatively disadvantaged. Resistance is comparatively high (0.55) because this is a genuinely contested methodological question within a live, organized academic and confessional discourse, not a settled fact.
 *
 * DIRECTIONALITY LOGIC:
 *   Missionary translation agencies sit closest to the beneficiary end: they set the methodology, train practitioners in it, and capture the institutional legitimacy and funding that follows from claimed missional success. Lay readers and unreached communities are also structural beneficiaries by the reading's own account, though their exit options are trapped rather than mobile — they receive the good but cannot evaluate what was traded away to produce it. Academic scholars and word-study-dependent clergy are targets: their professional practice depends on a resource (structural fidelity) this reading treats as subordinate, and their exit is constrained because they must still engage the dominant translations shaping lay theological vocabulary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — genuine unintelligibility of literal translation in many receptor languages — remains partly live, which prevents classifying this reading as pure inertial extraction (piton) or as a fully resolved constraint. But the contested corroboration (scholars arguing the degree of subordination has overshot the original problem) documents a live mandatrophy question: has the pastoral-urgency justification outlived the narrower cases it was built to solve, while the institutional apparatus built around it persists and expands regardless? The tangled_rope classification holds both truths at once rather than forcing a single verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly does the dynamic-equivalence reading''s core premise conflict with its siblings — is it a disagreement about what translation IS FOR (communicative effect vs. structural correspondence vs. textual recovery), or merely a disagreement about degree/method within a shared goal?',
    'Comparative analysis of foundational translation-theory texts (Nida vs. Beekman/Callow vs. formal-equivalence manifestos vs. text-critical method statements) to locate whether the disagreement is definitional (what counts as faithful translation) or purely technical (how best to achieve an agreed faithfulness).',
    'If purely technical, the three readings could in principle converge with better method; if definitional, they remain permanently coexisting commitments held by different institutional communities, which supports treating them as genuinely separate constraints rather than points on one continuum.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locates the structural disagreement among the three kernel readings.').

omega_variable(
    morphological_loss_magnitude,
    'How much genuine exegetical information is lost when a dynamic-equivalence translation collapses multiple source-language forms into a single receptor-language rendering, versus how much of the scholarly objection is professional-territory defense?',
    'Systematic comparison of specific disputed passages across formal- and dynamic-equivalence translations, cross-checked against independent (non-translator, non-missionary) linguistic analysis of what information the receptor language could or could not have preserved.',
    'If loss is substantial and recoverable through better translation technique, ε should be revised upward toward the higher end of moderate; if loss is largely unavoidable given genuine cross-linguistic incommensurability, the coordination function is stronger than the extraction reading suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(morphological_loss_magnitude, empirical, 'Whether the morphological cost attributed to this reading is real translation loss or professional gatekeeping.').

omega_variable(
    pastoral_urgency_scope_creep,
    'Was the original pastoral-urgency justification meant to apply only to first-translation contexts for genuinely unreached languages, or has it been generalized to justify dynamic-equivalence as the default even where formal-equivalence alternatives are readily available and already exist in the target language?',
    'Historical review of Bible society translation-selection criteria over time, tracking whether dynamic-equivalence has become the default choice even in languages with existing formal-equivalence translations and established literate readerships.',
    'If scope has crept beyond the founding justification, the founding_problem_status of ''contested'' should shift toward ''partially dead'' for the expanded domain, strengthening the mandatrophy reading in well-resourced language contexts even while remaining ''live'' in genuinely under-resourced ones.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pastoral_urgency_scope_creep, empirical, 'Whether the founding justification has been generalized beyond its original scope.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_source_text__dynamic_equivalence_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bibl_tr_t12, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement(bibl_tr_t24, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 24, 0.16).
narrative_ontology:measurement(bibl_tr_t36, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 36, 0.18).
narrative_ontology:measurement(bibl_tr_t48, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 48, 0.2).
narrative_ontology:measurement(bibl_tr_t60, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 60, 0.22).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(bibl_be_t12, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 12, 0.34).
narrative_ontology:measurement(bibl_be_t24, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 24, 0.38).
narrative_ontology:measurement(bibl_be_t36, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 36, 0.41).
narrative_ontology:measurement(bibl_be_t48, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 48, 0.43).
narrative_ontology:measurement(bibl_be_t60, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 60, 0.45).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(biblical_source_text__dynamic_equivalence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_source_text__dynamic_equivalence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_source_text__dynamic_equivalence_reading, 0.1).
narrative_ontology:affects_constraint(biblical_source_text__dynamic_equivalence_reading, biblical_source_text__formal_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__dynamic_equivalence_reading, biblical_source_text__critical_reconstructive_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the biblical_source_text kernel. formal_equivalence_reading treats structural fidelity as primary and expects a different, lower-victim-count structure with scholars as beneficiaries rather than payers. critical_reconstructive_reading treats textual-critical reconstruction as prior to either fidelity claim and has its own distinct beneficiary set (text critics) and victim set (both translation camps, who it argues proceed on unsettled textual ground). Each carries its own ε; none is a measurement of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
