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
 *   human_readable: Gita Kurukshetra Discourse (Gandhian Allegorical Reading)
 *   domain: religious_studies/textual_hermeneutics/ethical_philosophy
 *
 * SUMMARY:
 *   This constraint represents the Gandhian allegorical reading of the
 *   Bhagavad Gita's Kurukshetra discourse, where the battlefield is
 *   understood as a metaphor for internal moral struggle, and violence is
 *   spiritual rather than physical. This reading fundamentally repudiates
 *   interpretations that legitimate caste hierarchy or physical warfare. The
 *   metrics reflect the highly extractive and suppressive nature of the
 *   *literal interpretation* of the Gita, which this Gandhian reading
 *   contests and seeks to overcome. The 'claimed_type' of 'rope' refers to
 *   the coordination function of the Gandhian reading itself, guiding
 *   individuals towards non-violent moral action.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.85).
domain_priors:suppression_score(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.9).
domain_priors:theater_ratio(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__gandhian_allegorical_reading, rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__gandhian_allegorical_reading, "Gita Kurukshetra Discourse (Gandhian Allegorical Reading)").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__gandhian_allegorical_reading, "religious_studies/textual_hermeneutics/ethical_philosophy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__gandhian_allegorical_reading, '4ec7b95f-bc44-4e36-95de-521d2a4c890e').
narrative_ontology:cs_kernel_codification('4ec7b95f-bc44-4e36-95de-521d2a4c890e', fixed_text).
narrative_ontology:cs_authority_grounding('4ec7b95f-bc44-4e36-95de-521d2a4c890e', practice).
narrative_ontology:cs_interpretation_layer_present('4ec7b95f-bc44-4e36-95de-521d2a4c890e').
narrative_ontology:cs_reading_relation('4ec7b95f-bc44-4e36-95de-521d2a4c890e', gita_kurukshetra_discourse__orthodox_literal_reading, forecloses).
narrative_ontology:cs_reading_relation('4ec7b95f-bc44-4e36-95de-521d2a4c890e', gita_kurukshetra_discourse__universalist_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('4ec7b95f-bc44-4e36-95de-521d2a4c890e', foundational, ahimsa_as_supreme_dharma).
narrative_ontology:cs_axiom_status(ahimsa_as_supreme_dharma, holdable).
narrative_ontology:cs_axiom_grounding('4ec7b95f-bc44-4e36-95de-521d2a4c890e', ahimsa_as_supreme_dharma, deontological).
narrative_ontology:cs_axiom('4ec7b95f-bc44-4e36-95de-521d2a4c890e', foundational, moral_autonomy_of_individual).
narrative_ontology:cs_axiom_status(moral_autonomy_of_individual, holdable).
narrative_ontology:cs_axiom_grounding('4ec7b95f-bc44-4e36-95de-521d2a4c890e', moral_autonomy_of_individual, deontological).
narrative_ontology:cs_reference_frame('4ec7b95f-bc44-4e36-95de-521d2a4c890e', universal_moral_conscience).
narrative_ontology:cs_drift_state('4ec7b95f-bc44-4e36-95de-521d2a4c890e', contemporary_global_ethics, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('4ec7b95f-bc44-4e36-95de-521d2a4c890e', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, individual_moral_agents).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, advocates_of_non_violence).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, dalits_and_lower_castes).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, victims_of_war).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__gandhian_allegorical_reading, ahimsa_as_supreme_dharma).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__gandhian_allegorical_reading, moral_autonomy_of_individual).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who interpret the Gita's battlefield as an internal struggle against vice and ego, finding a path to ethical living and self-realization through non-violence. They benefit from a framework that empowers their moral conscience.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, individual_moral_agents, beneficiary,
    moderate, biographical, mobile, local).

% Those who actively promote non-violence (ahimsa) as a universal ethical principle, drawing inspiration from this allegorical reading to challenge physical and structural violence. They shape the discourse around this interpretation.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, advocates_of_non_violence, agenda_setter,
    organized, generational, mobile, global).

% Historically subjected to the structural violence and suppression of the caste system, which the literal reading of the Gita can be used to justify. From the Gandhian perspective, they are victims of the literal interpretation's social consequences.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, dalits_and_lower_castes, payer,
    powerless, generational, trapped, national).

% Those directly harmed by physical conflict, which a literal reading of the Gita's Kurukshetra narrative can be invoked to legitimate as 'righteous war'. This reading seeks to alleviate their suffering by repudiating such justifications.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, victims_of_war, payer,
    powerless, immediate, trapped, local).

% Traditional interpreters who often uphold a literal reading of the Gita, emphasizing caste-based duties (dharma) and the legitimacy of righteous warfare. Their interpretive authority is challenged and effectively excluded by the Gandhian allegorical framework.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, orthodox_brahminical_scholars, excluded,
    institutional, generational, identity_locked, national).

% Individuals or groups who might invoke a literal interpretation of the Gita to justify physical violence or conflict, believing it to be a divine mandate. This reading directly repudiates their justification for action.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, literalist_warriors, excluded,
    powerful, immediate, identity_locked, local).

% Scholars who analyze the philosophical and ethical implications of different Gita interpretations, including the Gandhian allegorical reading, without necessarily endorsing a particular path. They assess its coherence and impact.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, analytical_philosophers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gita_kurukshetra_discourse__gandhian_allegorical_reading, diffuse).
narrative_ontology:fixing_cost_class(gita_kurukshetra_discourse__gandhian_allegorical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates individual moral agents towards non-violent action and internal ethical struggle, providing a framework for reconciling spiritual teachings with universal ethical principles like ahimsa.
% TRANSFER_FUNCTION: Transfers interpretive authority from external, caste-bound traditions to individual moral conscience; shifts focus from physical warfare to internal spiritual discipline; transfers responsibility for ethical action to the individual.
% ABSENT_VOICES: Those who benefit from the literal interpretation of the Gita, such as proponents of caste hierarchy or those who justify violence as dharmic duty, are structurally excluded from the interpretive authority of this reading. They would argue for the divine mandate of social order and righteous war.
% DISAPPEARANCE_RATIONALE: If this allegorical reading vanished, the literal interpretations of the Gita, which can justify caste hierarchy and physical violence, would gain unchallenged prominence. This would likely lead to a resurgence of arguments for social stratification and potentially legitimate conflict, altering ethical discourse and social structures.
% FOUNDING_PROBLEM: To reconcile the narrative of a violent battlefield in a sacred text (Bhagavad Gita) with universal ethical principles, particularly non-violence (ahimsa), and to provide a moral framework for social and political action.
% FOUNDING_PROBLEM_CORROBORATION: Mahatma Gandhi's extensive writings and activism attest to the problem's live status, as do contemporary peace movements, human rights advocates, and scholars of non-violent resistance who continue to engage with the Gita's ethical challenges. These sources corroborate the need for an interpretation that aligns the text with universal ethics, from outside the immediate beneficiaries of this reading.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__gandhian_allegorical_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__gandhian_allegorical_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__gandhian_allegorical_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(gita_kurukshetra_discourse__gandhian_allegorical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__gandhian_allegorical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gita_kurukshetra_discourse__gandhian_allegorical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gita_kurukshetra_discourse__gandhian_allegorical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high 'extractiveness' (0.85) and 'suppression' (0.90) reflect the Gandhian reading's assessment of the *literal interpretation's* historical and ongoing impact: the caste system and justifications for war are seen as profoundly extractive and suppressive. 'Accessibility collapse' is high (0.92) because for those trapped by caste or war, alternatives are severely limited. 'Resistance' is high (0.70) due to historical and ongoing movements against caste discrimination and violence, often inspired by this very allegorical reading. 'Theater ratio' is low (0.10) because the literal interpretation, when applied, is often genuinely believed and enforced, not merely performative. The measurements show a slight decrease in extractiveness and suppression over the early 20th century (reflecting Gandhi's influence) followed by a stabilization, indicating ongoing contestation.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between the Gandhian allegorical reading and the orthodox literal reading. From the perspective of the literal reading, the Gita mandates social order (caste) and righteous action (war), which it views as coordination. From the Gandhian perspective, these same structures are profoundly extractive and suppressive, and the text must be reinterpreted to align with universal ethics. The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual moral agents and advocates of non-violence are beneficiaries of this reading, as it empowers their conscience and provides a framework for ethical action. Dalits, lower castes, and victims of war are identified as victims, as they are the ones from whom the literal interpretation extracts obedience and justifies harm. Orthodox scholars and literalist warriors are 'excluded' from the interpretive authority of this reading, as their positions are directly challenged.
 *
 * MANDATROPHY ANALYSIS:
 *   The Gandhian reading actively works to resolve the mandatrophy inherent in literal interpretations that justify violence or caste. It argues that the original 'mandate' (if interpreted literally) has outlived any legitimate function and now serves as a cover for extraction. By reinterpreting the text, it seeks to shift the constraint from a potentially extractive 'snare' (literal interpretation) to a 'rope' of moral coordination, thus preventing mislabeling extraction as coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine, independent moral framework, or primarily an interpretive strategy for the Gita kernel?',
    'Analysis of its application in contexts beyond the Gita, or its adoption by non-Hindu ethical systems.',
    'If independent, its classification as ''rope'' is more robust; if primarily interpretive, its stability is more dependent on the Gita''s continued relevance and contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is the ''Gandhian allegorical reading'' of the ''gita_kurukshetra_discourse'' kernel.').

omega_variable(
    structural_delta_caste,
    'How would a literal reading of the Gita reintroduce caste hierarchy as a divinely mandated social constraint?',
    'Comparative textual analysis of orthodox commentaries and their historical social impact, contrasted with the Gandhian repudiation of caste.',
    'A literal reading would re-establish a highly extractive and suppressive ''snare'' for lower castes, which this allegorical reading directly challenges.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_delta_caste, empirical, 'The literal reading''s reintroduction of caste hierarchy.').

omega_variable(
    structural_delta_violence,
    'How would a literal reading of the Gita legitimate physical violence in the context of ''dharmic war''?',
    'Analysis of historical and contemporary justifications for conflict that explicitly cite the Gita''s literal battlefield narrative, contrasted with the Gandhian emphasis on spiritual struggle.',
    'A literal reading would legitimate a ''tangled_rope'' or ''snare'' of violence, which this allegorical reading seeks to dismantle by reinterpreting the conflict as internal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_delta_violence, empirical, 'The literal reading''s legitimation of physical violence.').

omega_variable(
    interpretive_authority_shift,
    'To what extent has interpretive authority truly shifted from traditional Brahminical scholars to individual moral conscience in practice?',
    'Sociological studies of religious practice and ethical decision-making among adherents, assessing the actual influence of traditional vs. individual interpretations.',
    'If the shift is incomplete, the ''rope'' classification for individual moral agents is weaker, as external authorities may still exert significant influence, potentially reintroducing extractive elements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_shift, empirical, 'Ambiguity regarding the actual shift in interpretive authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__gandhian_allegorical_reading, 1900, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t1900, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(gita_tr_t1925, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 1925, 0.08).
narrative_ontology:measurement(gita_tr_t1950, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(gita_tr_t1975, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 1975, 0.12).
narrative_ontology:measurement(gita_tr_t2000, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 2000, 0.11).
narrative_ontology:measurement(gita_tr_t2025, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(gita_be_t1900, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 1900, 0.95).
narrative_ontology:measurement(gita_be_t1925, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 1925, 0.9).
narrative_ontology:measurement(gita_be_t1950, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 1950, 0.85).
narrative_ontology:measurement(gita_be_t1975, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 1975, 0.8).
narrative_ontology:measurement(gita_be_t2000, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 2000, 0.83).
narrative_ontology:measurement(gita_be_t2025, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 2025, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t1900, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 1900, 0.98).
narrative_ontology:measurement(gita_su_t1925, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 1925, 0.95).
narrative_ontology:measurement(gita_su_t1950, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 1950, 0.9).
narrative_ontology:measurement(gita_su_t1975, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 1975, 0.88).
narrative_ontology:measurement(gita_su_t2000, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 2000, 0.89).
narrative_ontology:measurement(gita_su_t2025, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 2025, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__gandhian_allegorical_reading, identity_coordination).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse__orthodox_literal_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse__universalist_devotional_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'gita_kurukshetra_discourse' kernel, each with its own structural properties and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
