% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__reformist_spiritual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_corpus_social_prescription__reformist_spiritual_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: vedic_corpus_social_prescription__reformist_spiritual_reading
 *   human_readable: Vedic Texts as Spiritual Unity (Reformist Reading)
 *   domain: religious_studies/hermeneutics
 *
 * SUMMARY:
 *   This constraint represents the 'reformist spiritual reading' of Vedic
 *   texts, which interprets them as primarily concerned with spiritual unity
 *   and metaphorical cosmology, explicitly denying any prescriptive social
 *   content, such as the Varna system. This reading emerged in response to
 *   both orthodox interpretations that justified social hierarchies and
 *   colonial interpretations that sought to codify 'Hindu law' for
 *   administrative purposes. It functions as a coordination mechanism for
 *   spiritual seekers and reformist movements, offering an inclusive and
 *   ethically aligned engagement with ancient scriptures.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.15).
domain_priors:suppression_score(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.1).
domain_priors:theater_ratio(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__reformist_spiritual_reading, rope).
narrative_ontology:human_readable(vedic_corpus_social_prescription__reformist_spiritual_reading, "Vedic Texts as Spiritual Unity (Reformist Reading)").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__reformist_spiritual_reading, "religious_studies/hermeneutics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__reformist_spiritual_reading, '842adac7-aac5-4177-b7c2-73ce1ea32faf').
narrative_ontology:cs_kernel_codification('842adac7-aac5-4177-b7c2-73ce1ea32faf', fixed_text).
narrative_ontology:cs_authority_grounding('842adac7-aac5-4177-b7c2-73ce1ea32faf', expertise).
narrative_ontology:cs_interpretation_layer_present('842adac7-aac5-4177-b7c2-73ce1ea32faf').
narrative_ontology:cs_reading_relation('842adac7-aac5-4177-b7c2-73ce1ea32faf', vedic_corpus_social_prescription__orthodox_varna_reading, forecloses).
narrative_ontology:cs_reading_relation('842adac7-aac5-4177-b7c2-73ce1ea32faf', vedic_corpus_social_prescription__colonial_orientalist_reading, forecloses).
narrative_ontology:cs_axiom('842adac7-aac5-4177-b7c2-73ce1ea32faf', foundational, vedic_texts_spiritual_metaphorical).
narrative_ontology:cs_axiom_status(vedic_texts_spiritual_metaphorical, holdable).
narrative_ontology:cs_axiom_grounding('842adac7-aac5-4177-b7c2-73ce1ea32faf', vedic_texts_spiritual_metaphorical, deontological).
narrative_ontology:cs_axiom('842adac7-aac5-4177-b7c2-73ce1ea32faf', foundational, social_stratification_human_construct).
narrative_ontology:cs_axiom_status(social_stratification_human_construct, holdable).
narrative_ontology:cs_axiom_grounding('842adac7-aac5-4177-b7c2-73ce1ea32faf', social_stratification_human_construct, conventional).
narrative_ontology:cs_reference_frame('842adac7-aac5-4177-b7c2-73ce1ea32faf', universal_spiritual_unity).
narrative_ontology:cs_drift_state('842adac7-aac5-4177-b7c2-73ce1ea32faf', contemporary_discourse, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('842adac7-aac5-4177-b7c2-73ce1ea32faf', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, spiritual_seekers).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, reformist_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__reformist_spiritual_reading, orthodox_priestly_classes).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__reformist_spiritual_reading, traditionalist_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Find a path to spiritual understanding and community that emphasizes universal unity and personal growth, free from rigid social hierarchies or prescriptive rules. They benefit from the accessibility and inclusivity of this interpretation.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, spiritual_seekers, beneficiary,
    moderate, biographical, mobile, global).

% Actively interpret and promote the Vedic texts as primarily spiritual and metaphorical, challenging traditional or colonial readings that impose social stratification or administrative law. They shape the discourse and provide intellectual grounding for this reading.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, reformist_scholars, agenda_setter,
    organized, generational, constrained, global).

% Bear the cost of diminished authority and legitimacy as their claims to divinely mandated social hierarchy (Varna) are challenged by this reading. Their traditional role as interpreters and enforcers of social order is undermined.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, orthodox_priestly_classes, payer,
    powerful, generational, identity_locked, national).

% Experience a challenge to their established social norms and practices, which may have been historically justified by orthodox interpretations of Vedic texts. They face pressure to adapt or defend their traditions against reformist critiques.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, traditionalist_communities, payer,
    moderate, generational, identity_locked, local).

% Historically imposed a unified 'Hindu law' system based on their interpretation of Vedic and Dharmashastra texts for administrative control. Their interpretive framework is actively rejected and excluded by the reformist reading, which views it as a distortion.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, colonial_administrators, excluded,
    institutional, civilizational, analytical, global).

% Study the historical development of Vedic interpretations and their social impact, providing critical analysis of how different readings have shaped religious and social structures over time. They observe the contest without direct participation.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, analytical_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for individual spiritual practice and community formation based on shared metaphorical understanding of Vedic texts, emphasizing universal unity and personal growth, free from rigid social hierarchies.
% TRANSFER_FUNCTION: Transfers spiritual insight and a sense of universal unity to practitioners; transfers interpretive authority from traditional hierarchical structures to individual or reformist scholarship, democratizing access to spiritual wisdom.
% ABSENT_VOICES: Those who benefit from the orthodox or colonial readings, particularly those whose power or social status is derived from a divinely sanctioned hierarchy, would object. Their voices are actively being challenged and marginalized by the reformist discourse.
% DISAPPEARANCE_RATIONALE: If this reformist reading vanished, spiritual communities and individual seekers would lose a unifying, inclusive framework for engaging with Vedic texts. The contest over interpretation would revert to more traditional or colonial framings, potentially re-entrenching social inequalities and limiting spiritual accessibility. The social order itself, however, would not collapse, as this reading explicitly denies the Vedas' role in prescribing it.
% FOUNDING_PROBLEM: To reconcile ancient spiritual texts with modern ethical sensibilities, rejecting interpretations that justify social inequality, caste discrimination, or colonial administration, and to make spiritual wisdom accessible universally.
% FOUNDING_PROBLEM_CORROBORATION: Independent scholars of religion, human rights advocates, and interfaith dialogue initiatives corroborate the ongoing need for interpretations that promote equality and universal spiritual values, actively challenging historically extractive or discriminatory readings. This corroboration comes from outside the direct beneficiaries of this specific reading.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__reformist_spiritual_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__reformist_spiritual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__reformist_spiritual_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(vedic_corpus_social_prescription__reformist_spiritual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_corpus_social_prescription__reformist_spiritual_reading_tests).
:- end_tests(vedic_corpus_social_prescription__reformist_spiritual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Rope because it facilitates coordination (spiritual practice, community building) with very low extraction. Its core function is to provide an interpretive framework that benefits spiritual seekers without imposing costs or suppressing alternatives, beyond challenging competing interpretations. Extractiveness, suppression, and theater ratio are low, reflecting its nature as a non-coercive, spiritual framework. Resistance is moderate because this reading actively challenges deeply entrenched orthodox and colonial interpretations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of spiritual seekers and reformist scholars, this reading is a liberating and unifying force. From the perspective of orthodox groups, it is a challenge to tradition and established order. The engine's classification as a Rope reflects the internal coherence and low-extraction nature of this specific reading, while acknowledging the external contestation.
 *
 * DIRECTIONALITY LOGIC:
 *   Spiritual seekers and reformist scholars are clear beneficiaries, gaining an accessible and ethically aligned spiritual path. Orthodox priestly classes and traditionalist communities are positioned as payers, as this reading undermines their traditional authority and social structures. Colonial administrators are excluded, as their interpretive framework is explicitly rejected. Analytical historians serve as observers, documenting the interpretive contest.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading actively seeks to resolve the mandatrophy inherent in older interpretations that justified social stratification. By denying prescriptive social content, it prevents the mislabeling of genuine spiritual coordination as a cover for social extraction. The 'founding problem' of reconciling texts with modern ethics remains live, indicating the constraint is actively fulfilling its mandate, rather than atrophying.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint is the ''reformist_spiritual_reading'' of the ''vedic_corpus_social_prescription'' kernel. What are the full implications of its relationship to sibling readings?',
    'Ongoing hermeneutic and sociological analysis of the reception and impact of all readings within the broader discourse.',
    'The classification of this reading as a Rope depends on its successful rejection of prescriptive social content. If its ''forecloses'' relationship to other readings is weakened, its classification could shift if it is found to implicitly enable or tolerate extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Identifies this constraint as one reading of a contested kernel, with sibling readings ''orthodox_varna_reading'' and ''colonial_orientalist_reading''.').

omega_variable(
    social_prescription_ambiguity,
    'Is the absence of prescriptive social content an inherent, discoverable feature of the Vedic texts themselves, or primarily an interpretive choice and ethical stance of this reformist reading?',
    'Comparative textual analysis across diverse ancient Indian textual traditions and cross-cultural studies of sacred text interpretation, alongside philosophical arguments for textual intent.',
    'If it is purely an interpretive choice, the ''emerges_naturally'' aspect of its non-prescriptive nature is weaker, potentially increasing its ''resistance'' metric from those who hold alternative interpretations. If inherent, its ''rope'' classification is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_prescription_ambiguity, conceptual, 'Ambiguity regarding the source of non-prescriptive social content in Vedic texts.').

omega_variable(
    historical_impact_vs_textual_intent,
    'Does the historical use of Vedic texts to justify social hierarchy (as in the orthodox reading) imply an inherent prescriptive capacity or ambiguity within the texts, even if this reformist reading denies it?',
    'Detailed historical-critical scholarship on the evolution of Vedic interpretation and its correlation with social structures, distinguishing between textual content and its reception history.',
    'If historical impact suggests an inherent ambiguity that enables extractive readings, the ''accessibility_collapse'' for alternative, non-extractive interpretations might be higher than currently assessed, reflecting the persistent challenge of overcoming historical precedent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_impact_vs_textual_intent, empirical, 'Relationship between historical use of texts and their inherent interpretive potential.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__reformist_spiritual_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t1900, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1900, 0.04).
narrative_ontology:measurement(vedi_tr_t1930, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1930, 0.04).
narrative_ontology:measurement(vedi_tr_t1960, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(vedi_tr_t1990, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(vedi_tr_t2024, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(vedi_be_t1900, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1900, 0.12).
narrative_ontology:measurement(vedi_be_t1930, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1930, 0.13).
narrative_ontology:measurement(vedi_be_t1960, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1960, 0.14).
narrative_ontology:measurement(vedi_be_t1990, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1990, 0.15).
narrative_ontology:measurement(vedi_be_t2024, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t1900, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 1900, 0.08).
narrative_ontology:measurement(vedi_su_t1930, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 1930, 0.09).
narrative_ontology:measurement(vedi_su_t1960, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 1960, 0.1).
narrative_ontology:measurement(vedi_su_t1990, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 1990, 0.1).
narrative_ontology:measurement(vedi_su_t2024, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__reformist_spiritual_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
