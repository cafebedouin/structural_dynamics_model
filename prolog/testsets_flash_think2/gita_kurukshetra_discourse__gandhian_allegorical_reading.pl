% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__gandhian_allegorical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__gandhian_allegorical_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: gita_kurukshetra_discourse__gandhian_allegorical_reading
 *   human_readable: Gandhian Allegorical Reading of Kurukshetra
 *   domain: religious_studies/textual_hermeneutics/ethical_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the Gandhian allegorical reading of
 *   the Bhagavad Gita's Kurukshetra discourse, where the battlefield is
 *   interpreted as a metaphor for internal ethical struggle, and violence is
 *   understood as spiritual, not physical. This reading repudiates literal
 *   interpretations that justify physical warfare or caste-based duties,
 *   instead elevating ahimsa (non-violence) and individual moral conscience
 *   as supreme principles. The constraint itself, as a moral framework, is
 *   low in extraction for its adherents but actively suppresses alternative,
 *   literal interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.15).
domain_priors:suppression_score(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.7).
domain_priors:theater_ratio(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__gandhian_allegorical_reading, rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__gandhian_allegorical_reading, "Gandhian Allegorical Reading of Kurukshetra").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__gandhian_allegorical_reading, "religious_studies/textual_hermeneutics/ethical_philosophy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__gandhian_allegorical_reading, '1996c1ac-1d8a-4550-b9b9-7ab487c21a62').
narrative_ontology:cs_kernel_codification('1996c1ac-1d8a-4550-b9b9-7ab487c21a62', fixed_text).
narrative_ontology:cs_authority_grounding('1996c1ac-1d8a-4550-b9b9-7ab487c21a62', practice).
narrative_ontology:cs_interpretation_layer_present('1996c1ac-1d8a-4550-b9b9-7ab487c21a62').
narrative_ontology:cs_reading_relation('1996c1ac-1d8a-4550-b9b9-7ab487c21a62', gita_kurukshetra_discourse__orthodox_literal_reading, forecloses).
narrative_ontology:cs_reading_relation('1996c1ac-1d8a-4550-b9b9-7ab487c21a62', gita_kurukshetra_discourse__universalist_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('1996c1ac-1d8a-4550-b9b9-7ab487c21a62', foundational, kurukshetra_as_internal_dharma_yuddha).
narrative_ontology:cs_axiom_status(kurukshetra_as_internal_dharma_yuddha, holdable).
narrative_ontology:cs_axiom_grounding('1996c1ac-1d8a-4550-b9b9-7ab487c21a62', kurukshetra_as_internal_dharma_yuddha, deontological).
narrative_ontology:cs_axiom('1996c1ac-1d8a-4550-b9b9-7ab487c21a62', foundational, ahimsa_as_supreme_dharma).
narrative_ontology:cs_axiom_status(ahimsa_as_supreme_dharma, holdable).
narrative_ontology:cs_axiom_grounding('1996c1ac-1d8a-4550-b9b9-7ab487c21a62', ahimsa_as_supreme_dharma, deontological).
narrative_ontology:cs_reference_frame('1996c1ac-1d8a-4550-b9b9-7ab487c21a62', gandhian_moral_framework).
narrative_ontology:cs_drift_state('1996c1ac-1d8a-4550-b9b9-7ab487c21a62', contemporary_religious_discourse, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('1996c1ac-1d8a-4550-b9b9-7ab487c21a62', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, adherents_of_ahimsa).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, victims_of_structural_violence).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, orthodox_literalists).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, proponents_of_violence).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, proponents_of_caste_hierarchy).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__gandhian_allegorical_reading, principle_of_ahimsa).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__gandhian_allegorical_reading, moral_autonomy).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__gandhian_allegorical_reading, internal_ethical_struggle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Find a moral framework for non-violent action and internal ethical development, aligning their spiritual understanding with their ethical commitments. They are empowered by this reading to challenge injustice non-violently.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, adherents_of_ahimsa, beneficiary,
    moderate, biographical, mobile, global).

% Benefit from the delegitimization of religiously sanctioned violence and caste-based discrimination, finding a textual basis for their liberation struggles. This reading offers a path to challenge their oppression.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, victims_of_structural_violence, beneficiary,
    powerless, immediate, trapped, local).

% Their interpretive authority and traditional understanding of the Gita, which may include justifications for caste or righteous violence, are directly challenged and undermined by this allegorical reading. They bear the cost of losing interpretive dominance.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, orthodox_literalists, payer,
    institutional, generational, constrained, national).

% Lose a significant religious justification for physical conflict or aggression, as the allegorical reading reinterprets 'war' as internal struggle. Their ability to mobilize support for violence based on religious texts is curtailed.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, proponents_of_violence, payer,
    powerful, biographical, constrained, regional).

% Their claims of divine mandate for social stratification are repudiated by a reading that emphasizes universal spiritual equality and internal ethical development over external social roles. They face a loss of legitimacy for their social order.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, proponents_of_caste_hierarchy, payer,
    institutional, generational, constrained, national).

% Actively promoted and disseminated this allegorical interpretation, shaping a moral and political movement around its principles of non-violence and self-purification. They set the interpretive agenda for this reading.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, gandhi_and_followers, agenda_setter,
    organized, biographical, mobile, global).

% Analyze and critique various interpretations of the Gita, including the Gandhian reading, assessing its hermeneutical validity, historical impact, and ethical implications. They do not directly benefit or pay but provide critical analysis.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, academic_scholars_of_gita, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gita_kurukshetra_discourse__gandhian_allegorical_reading, diffuse).
narrative_ontology:fixing_cost_class(gita_kurukshetra_discourse__gandhian_allegorical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates moral action and spiritual understanding around the principles of non-violence (ahimsa) and the internal struggle for righteousness (dharma-yuddha), providing a shared ethical framework for adherents.
% TRANSFER_FUNCTION: Transfers moral authority from external, literal interpretations of religious texts to individual conscience and ethical practice; it transfers the justification for physical violence to its repudiation.
% ABSENT_VOICES: Those who benefit from literal interpretations that justify caste-based duties or righteous violence are structurally excluded from the interpretive conversation of this reading. They would object to the reinterpretation of key passages and the repudiation of their traditional authority.
% DISAPPEARANCE_RATIONALE: If this allegorical reading vanished, a significant textual grounding for non-violent resistance and internal ethical struggle would be lost. This could lead to a resurgence of literal interpretations justifying violence or caste, and a weakening of moral arguments for social justice rooted in the Gita.
% FOUNDING_PROBLEM: The problem of religious texts being used to justify physical violence, war, and oppressive social hierarchies like the caste system, particularly in the context of the Bhagavad Gita's battlefield narrative.
% FOUNDING_PROBLEM_CORROBORATION: Human rights organizations, peace activists, and critical religious scholars (outside orthodox institutions) consistently corroborate the ongoing problem of religiously justified violence and hierarchy in various contexts, supporting the continued relevance of this reading's founding problem.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__gandhian_allegorical_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__gandhian_allegorical_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__gandhian_allegorical_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(gita_kurukshetra_discourse__gandhian_allegorical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__gandhian_allegorical_reading_tests).
:- end_tests(gita_kurukshetra_discourse__gandhian_allegorical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` is low (0.15) because for its adherents, this reading provides a liberating moral framework that reduces, rather than imposes, external burdens. `suppression` is high (0.70) because the reading actively challenges and seeks to delegitimize literal interpretations of the Gita that justify violence or caste, effectively suppressing their moral authority. `theater_ratio` is low (0.05) as the reading is primarily a genuine hermeneutic and ethical guide, with minimal performative maintenance. `accessibility_collapse` is high (0.80) for adherents, as the literal interpretation becomes morally inaccessible. `resistance` is high (0.75) due to strong opposition from orthodox literalists whose traditional authority and interpretations are directly challenged. The `claimed_type` of 'rope' reflects the reading's self-conception as a coordinating, beneficial framework, even as its metrics reveal its suppressive stance towards alternatives.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of its adherents, this reading is a liberating moral guide (low extraction, high benefit). From the perspective of orthodox literalists, it is a subversive reinterpretation that extracts their traditional authority and undermines their social order (high extraction, high suppression). The engine's computation of per-seat classifications will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Adherents of ahimsa and victims of structural violence are beneficiaries, as the reading empowers them and delegitimizes their oppression. Orthodox literalists, proponents of violence, and proponents of caste hierarchy are victims, as their interpretive authority and justifications are undermined. Gandhi and his followers are the agenda-setters, actively shaping and propagating this interpretation. Academic scholars serve as observers, analyzing its impact and validity.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling a moral framework as pure extraction by clarifying that its 'extraction' is from alternative, harmful interpretations, not from its own adherents. It also highlights how a 'rope' (coordination) can be highly suppressive towards competing frameworks, which is a key insight for lifecycle drift detection.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hermeneutic_imposition_vs_discovery,
    'Is the Gandhian allegorical reading an inherent, latent meaning discovered within the Gita, or a moral imposition onto the text driven by external ethical commitments?',
    'Detailed philological and historical analysis of pre-Gandhian allegorical interpretations, alongside a critical examination of Gandhi''s own interpretive methodology and its consistency with traditional hermeneutics.',
    'If an imposition, its legitimacy as a ''reading'' of the Gita might be weakened, potentially increasing its perceived ''suppression'' of other readings. If a discovery, its moral authority is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hermeneutic_imposition_vs_discovery, conceptual, 'Whether the allegorical reading is an intrinsic textual meaning or an external ethical framework applied to the text.').

omega_variable(
    suppression_of_literalism_mechanism,
    'Is the suppression of literal interpretations structural (e.g., through institutional delegitimization) or internalized (e.g., through moral persuasion leading to self-rejection of literalism)?',
    'Sociological studies of interpretive communities: if literal interpretations persist strongly despite moral arguments, suppression is more structural; if adherents genuinely abandon literalism, it''s internalized.',
    'If primarily structural, the ''suppression'' metric accurately reflects external pressure. If primarily internalized, the effective suppression is higher, as individuals carry the interpretive shift within them, making ''exit'' from the allegorical reading more difficult.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_literalism_mechanism, empirical, 'Structural vs. internalized suppression mechanism for literal interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__gandhian_allegorical_reading, 1900, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t1900, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 1900, 0.02).
narrative_ontology:measurement(gita_tr_t1920, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 1920, 0.03).
narrative_ontology:measurement(gita_tr_t1940, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 1940, 0.05).
narrative_ontology:measurement(gita_tr_t1960, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 1960, 0.04).
narrative_ontology:measurement(gita_tr_t1980, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 1980, 0.04).
narrative_ontology:measurement(gita_tr_t2000, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(gita_tr_t2020, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 2020, 0.05).

% Extraction over time
narrative_ontology:measurement(gita_be_t1900, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 1900, 0.1).
narrative_ontology:measurement(gita_be_t1920, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 1920, 0.12).
narrative_ontology:measurement(gita_be_t1940, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 1940, 0.15).
narrative_ontology:measurement(gita_be_t1960, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 1960, 0.14).
narrative_ontology:measurement(gita_be_t1980, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 1980, 0.13).
narrative_ontology:measurement(gita_be_t2000, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 2000, 0.14).
narrative_ontology:measurement(gita_be_t2020, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 2020, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t1900, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 1900, 0.4).
narrative_ontology:measurement(gita_su_t1920, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 1920, 0.55).
narrative_ontology:measurement(gita_su_t1940, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 1940, 0.7).
narrative_ontology:measurement(gita_su_t1960, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 1960, 0.68).
narrative_ontology:measurement(gita_su_t1980, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(gita_su_t2000, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(gita_su_t2020, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 2020, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__gandhian_allegorical_reading, identity_coordination).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse__orthodox_literal_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse__universalist_devotional_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'gita_kurukshetra_discourse' kernel. Each reading has a unique ε value and structural profile, reflecting different interpretations of the same foundational text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
