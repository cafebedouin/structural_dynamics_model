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
    narrative_ontology:affects_constraint/2,
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
 *   domain: religious_studies/social_stratification/hermeneutics
 *
 * SUMMARY:
 *   This constraint represents the 'reformist spiritual reading' of the Vedic
 *   corpus, which interprets the texts as describing spiritual unity and
 *   metaphorical cosmology, devoid of prescriptive social content like the
 *   Varna system. This reading emerged and gained prominence as a
 *   counter-narrative to both orthodox hierarchical interpretations and
 *   colonial-orientalist constructions of 'Hindu law.' It functions as a
 *   Rope, coordinating spiritual understanding and ethical conduct without
 *   coercion or extraction, and actively challenges extractive
 *   interpretations.
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
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__reformist_spiritual_reading, rope).
narrative_ontology:human_readable(vedic_corpus_social_prescription__reformist_spiritual_reading, "Vedic Texts as Spiritual Unity (Reformist Reading)").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__reformist_spiritual_reading, "religious_studies/social_stratification/hermeneutics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__reformist_spiritual_reading, 'e0dd2a44-c844-4173-97fc-1ba46307f19b').
narrative_ontology:cs_kernel_codification('e0dd2a44-c844-4173-97fc-1ba46307f19b', fixed_text).
narrative_ontology:cs_authority_grounding('e0dd2a44-c844-4173-97fc-1ba46307f19b', expertise).
narrative_ontology:cs_interpretation_layer_present('e0dd2a44-c844-4173-97fc-1ba46307f19b').
narrative_ontology:cs_reading_relation('e0dd2a44-c844-4173-97fc-1ba46307f19b', vedic_corpus_social_prescription__orthodox_varna_reading, forecloses).
narrative_ontology:cs_reading_relation('e0dd2a44-c844-4173-97fc-1ba46307f19b', vedic_corpus_social_prescription__colonial_orientalist_reading, forecloses).
narrative_ontology:cs_axiom('e0dd2a44-c844-4173-97fc-1ba46307f19b', foundational, vedic_texts_are_spiritual_metaphorical).
narrative_ontology:cs_axiom_status(vedic_texts_are_spiritual_metaphorical, holdable).
narrative_ontology:cs_axiom_grounding('e0dd2a44-c844-4173-97fc-1ba46307f19b', vedic_texts_are_spiritual_metaphorical, deontological).
narrative_ontology:cs_axiom('e0dd2a44-c844-4173-97fc-1ba46307f19b', foundational, social_hierarchy_is_not_divinely_prescribed).
narrative_ontology:cs_axiom_status(social_hierarchy_is_not_divinely_prescribed, holdable).
narrative_ontology:cs_axiom_grounding('e0dd2a44-c844-4173-97fc-1ba46307f19b', social_hierarchy_is_not_divinely_prescribed, deontological).
narrative_ontology:cs_reference_frame('e0dd2a44-c844-4173-97fc-1ba46307f19b', vedic_spiritual_non_duality).
narrative_ontology:cs_drift_state('e0dd2a44-c844-4173-97fc-1ba46307f19b', contemporary_hermeneutic_contest, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('e0dd2a44-c844-4173-97fc-1ba46307f19b', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, spiritual_seekers).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, advocates_for_social_equality).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, reformist_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__reformist_spiritual_reading, orthodox_interpreters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who find spiritual meaning and guidance in the Vedic texts through this non-prescriptive, unity-focused interpretation. They benefit from a framework that promotes personal spiritual growth and ethical conduct without endorsing social hierarchy.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, spiritual_seekers, beneficiary,
    moderate, biographical, mobile, global).

% Academics and religious leaders who actively promote and articulate this interpretation of Vedic texts. They shape the discourse, publish commentaries, and teach, thereby setting the agenda for how these texts are understood in a modern context.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, reformist_scholars, agenda_setter,
    powerful, generational, analytical, global).

% Social activists and organizations working to dismantle caste-based discrimination and other forms of social hierarchy. They use this reading to argue that Vedic tradition inherently supports equality and that hierarchical interpretations are later corruptions or misreadings.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, advocates_for_social_equality, beneficiary,
    organized, generational, mobile, national).

% Traditional religious authorities and scholars who adhere to a literal interpretation of Vedic texts, including the divine mandate of Varna (social hierarchy). This reformist reading challenges their authority and interpretive monopoly, forcing them to defend their positions.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, orthodox_interpreters, payer,
    institutional, civilizational, identity_locked, global).

% Historical and contemporary scholars whose work established the 'Vedic/Dharmashastra as Hindu law' framework. This reformist reading directly refutes their foundational premise, effectively excluding their interpretive lens from its own internal coherence, though their influence persists in broader discourse.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, colonial_orientalist_scholars, excluded,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates individual spiritual practice and ethical conduct based on a non-hierarchical, unity-focused understanding of Vedic wisdom, fostering a sense of shared spiritual identity.
% TRANSFER_FUNCTION: Transfers spiritual insight, a framework for social equality, and a sense of inclusive religious identity to practitioners. It transfers interpretive authority away from literalist or colonial readings.
% ABSENT_VOICES: Those who benefit from social hierarchy justified by religious texts (e.g., traditional elites, institutions built on caste privilege) are structurally excluded from the interpretive conversation of this reformist reading, as their core premises are directly challenged.
% DISAPPEARANCE_RATIONALE: If this reformist reading vanished, spiritual movements focused on unity and social equality would lose a crucial textual grounding within the Vedic tradition. The interpretive landscape would revert to more hierarchical or colonial understandings, impacting social reform efforts and individual spiritual paths.
% FOUNDING_PROBLEM: The misinterpretation and appropriation of Vedic texts to justify social stratification, exclusion, and colonial administrative control, leading to spiritual disunity and social injustice.
% FOUNDING_PROBLEM_CORROBORATION: Independent historical analysis of social reform movements, contemporary sociological studies of caste and religious identity, and interfaith dialogues corroborate the ongoing problem of textual misinterpretation and its social impact, supporting the need for reformist readings.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__reformist_spiritual_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__reformist_spiritual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__reformist_spiritual_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   The extractiveness is low (0.15) because this reading primarily offers spiritual guidance and a framework for social equality, not material gain. Suppression is low (0.10) as it's an interpretive stance, not enforced coercively, though it faces intellectual resistance. Theater ratio is low (0.05) because its proponents genuinely believe in and practice its tenets. Accessibility collapse is low (0.20) as other interpretations and spiritual paths remain readily available. Resistance is moderate (0.30) due to ongoing intellectual and social contestation from orthodox and traditionalist groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of spiritual seekers and social equality advocates, this reading is a liberating and unifying force. From the perspective of orthodox interpreters, it is a deviation or corruption of tradition. The engine's classification of 'rope' reflects its internal structure, while the 'payer' role for orthodox interpreters captures the cost they bear in the interpretive contest.
 *
 * DIRECTIONALITY LOGIC:
 *   Spiritual seekers and advocates for social equality are clear beneficiaries, gaining a non-hierarchical framework. Reformist scholars act as agenda-setters, shaping the interpretation. Orthodox interpreters are 'payers' in the sense that their traditional authority and interpretive monopoly are challenged and diminished by this reading. Colonial-orientalist scholars are 'excluded' as their foundational premises are directly refuted.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint accurately representing the ''reformist_spiritual_reading'' of the Vedic corpus, distinct from its sibling readings?',
    'Comparative textual analysis of primary sources and secondary scholarship from each interpretive tradition, focusing on core hermeneutic principles and social implications.',
    'If the distinctions are blurred, the classification may misrepresent the unique structural properties of this specific reading, potentially conflating its low extraction with the higher extraction of other readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms the distinct identity of this kernel reading.').

omega_variable(
    textual_ambiguity_vs_interpretation,
    'Is the non-prescriptive nature of Vedic texts regarding social content an inherent textual property, or is it primarily an interpretive choice of the reformist reading?',
    'Linguistic and historical analysis of the earliest Vedic recensions, comparing their explicit content with later commentaries and social practices. If later texts introduce social prescriptions not present in the earliest layers, it supports the interpretive choice argument.',
    'If inherent, the ''rope'' classification is more robustly grounded in the text itself. If primarily interpretive, the constraint''s persistence depends more on the ongoing advocacy of reformist scholars and less on an ''emerges_naturally'' quality of the text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_ambiguity_vs_interpretation, empirical, 'Distinguishes textual properties from interpretive choices.').

omega_variable(
    social_impact_vs_discourse,
    'Does this reformist reading genuinely lead to tangible social equality and reduced discrimination, or is its impact largely confined to intellectual and spiritual discourse?',
    'Sociological studies measuring changes in social attitudes, caste-based discrimination, and access to resources in communities where this reading is prominent, compared to those dominated by orthodox interpretations.',
    'If the impact is limited to discourse, the ''rope'' classification might overstate its real-world coordination function for social equality, suggesting a higher ''theater_ratio'' or lower ''resistance'' from affected groups than currently assessed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(social_impact_vs_discourse, empirical, 'Assesses the real-world social impact of the reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__reformist_spiritual_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t1900, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1900, 0.03).
narrative_ontology:measurement(vedi_tr_t1930, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1930, 0.04).
narrative_ontology:measurement(vedi_tr_t1960, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1960, 0.04).
narrative_ontology:measurement(vedi_tr_t1990, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(vedi_tr_t2024, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(vedi_be_t1900, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1900, 0.1).
narrative_ontology:measurement(vedi_be_t1930, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1930, 0.12).
narrative_ontology:measurement(vedi_be_t1960, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1960, 0.13).
narrative_ontology:measurement(vedi_be_t1990, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1990, 0.14).
narrative_ontology:measurement(vedi_be_t2024, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t1900, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 1900, 0.08).
narrative_ontology:measurement(vedi_su_t1930, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 1930, 0.09).
narrative_ontology:measurement(vedi_su_t1960, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 1960, 0.09).
narrative_ontology:measurement(vedi_su_t1990, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 1990, 0.1).
narrative_ontology:measurement(vedi_su_t2024, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__reformist_spiritual_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription__orthodox_varna_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription__colonial_orientalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'vedic_corpus_social_prescription' kernel. This 'reformist_spiritual_reading' emphasizes spiritual unity and metaphorical cosmology, rejecting prescriptive social content, in contrast to the 'orthodox_varna_reading' (literal social hierarchy) and the 'colonial_orientalist_reading' (unified 'Hindu law').

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
