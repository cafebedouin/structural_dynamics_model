% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__symbolic_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__symbolic_transformation, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: temple_sacrifice_commitment__symbolic_transformation
 *   human_readable: Temple Sacrifice Commitment: Symbolic Transformation Reading
 *   domain: religious_law/halakhic_tradition/commitment_system_theory
 *
 * SUMMARY:
 *   This constraint represents the 'symbolic transformation' reading of the
 *   Temple sacrifice commitment within Halakhic tradition. Following the
 *   destruction of the Second Temple, rabbinic authority declared that prayer
 *   and study now serve as the primary means of fulfilling the divine command
 *   previously met by material sacrifice. This reading asserts that this is
 *   not a temporary substitute, but an authorized, permanent re-instantiation
 *   of the commitment. The constraint is claimed as a Tangled Rope because it
 *   genuinely coordinates communal religious life but also extracts from
 *   those who hold a more literal or material understanding of the original
 *   command, requiring active enforcement of the interpretive shift.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__symbolic_transformation, 0.65).
domain_priors:suppression_score(temple_sacrifice_commitment__symbolic_transformation, 0.7).
domain_priors:theater_ratio(temple_sacrifice_commitment__symbolic_transformation, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, extractiveness, 0.65).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__symbolic_transformation, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__symbolic_transformation, "Temple Sacrifice Commitment: Symbolic Transformation Reading").
narrative_ontology:topic_domain(temple_sacrifice_commitment__symbolic_transformation, "religious_law/halakhic_tradition/commitment_system_theory").

domain_priors:requires_active_enforcement(temple_sacrifice_commitment__symbolic_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__symbolic_transformation, '061a1a8a-f9e9-4559-bb8d-83d68e968f30').
narrative_ontology:cs_kernel_codification('061a1a8a-f9e9-4559-bb8d-83d68e968f30', fixed_text).
narrative_ontology:cs_authority_grounding('061a1a8a-f9e9-4559-bb8d-83d68e968f30', lineage).
narrative_ontology:cs_interpretation_layer_present('061a1a8a-f9e9-4559-bb8d-83d68e968f30').
narrative_ontology:cs_reading_relation('061a1a8a-f9e9-4559-bb8d-83d68e968f30', temple_sacrifice_commitment__study_as_exercise, influences).
narrative_ontology:cs_reading_relation('061a1a8a-f9e9-4559-bb8d-83d68e968f30', temple_sacrifice_commitment__performance_only, forecloses).
narrative_ontology:cs_reading_relation('061a1a8a-f9e9-4559-bb8d-83d68e968f30', temple_sacrifice_commitment__hybrid_preparatory, influences).
narrative_ontology:cs_axiom('061a1a8a-f9e9-4559-bb8d-83d68e968f30', foundational, divine_command_is_adaptable).
narrative_ontology:cs_axiom_status(divine_command_is_adaptable, holdable).
narrative_ontology:cs_axiom_grounding('061a1a8a-f9e9-4559-bb8d-83d68e968f30', divine_command_is_adaptable, theological).
narrative_ontology:cs_axiom('061a1a8a-f9e9-4559-bb8d-83d68e968f30', foundational, rabbinic_authority_to_redefine_halakha).
narrative_ontology:cs_axiom_status(rabbinic_authority_to_redefine_halakha, holdable).
narrative_ontology:cs_axiom_grounding('061a1a8a-f9e9-4559-bb8d-83d68e968f30', rabbinic_authority_to_redefine_halakha, conventional).
narrative_ontology:cs_reference_frame('061a1a8a-f9e9-4559-bb8d-83d68e968f30', post_temple_rabbinic_consensus).
narrative_ontology:cs_drift_state('061a1a8a-f9e9-4559-bb8d-83d68e968f30', contemporary_messianic_movements, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('061a1a8a-f9e9-4559-bb8d-83d68e968f30', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, community_leaders).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, traditionalist_adherents).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, messianic_restorationists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary interpreters and enforcers of Halakha, who have authorized the transformation of material sacrifice into prayer and study. They derive legitimacy from this interpretive power and maintain the coherence of the tradition in exile. Their authority is bound to the continuity of the law.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority, agenda_setter,
    institutional, generational, identity_locked, global).

% Benefit from the stability and adaptability of the tradition, which allows for continued religious practice without a physical Temple. They reinforce the rabbinic interpretation within their local communities, ensuring communal cohesion and continuity.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, community_leaders, beneficiary,
    organized, biographical, constrained, local).

% Bear the cost of accepting a transformed practice that deviates from the literal, material performance of sacrifice. Their identity is deeply tied to the historical practice, and they may feel a sense of loss or compromise, but are bound by communal and authoritative pressures.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, traditionalist_adherents, payer,
    moderate, biographical, identity_locked, local).

% Are victims of this reading, as it redefines the core commitment in a way that diminishes the urgency and necessity of material Temple restoration. They are often marginalized for advocating a return to literal performance, finding their core identity and purpose challenged by the prevailing interpretation.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, messianic_restorationists, payer,
    powerless, generational, trapped, global).

% Study the evolution of Halakhic interpretation and the sociological dynamics of religious authority. They analyze the structural implications of such transformations on religious communities and the nature of divine command.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, analytical_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent and adaptable framework for Jewish religious practice in the absence of the Temple, allowing adherents to fulfill divine commands through prayer and study, thereby maintaining communal identity and continuity across generations and geographies.
% TRANSFER_FUNCTION: Transfers the locus of divine service from material animal sacrifice to intellectual and spiritual engagement (prayer and study), reallocating communal resources and individual devotional effort towards these new forms of practice. It also transfers interpretive authority to the rabbinic class.
% ABSENT_VOICES: Ancient priestly classes and literalist interpreters who would insist on the non-negotiable material performance of sacrifice are absent from the contemporary interpretive discourse, their views largely superseded or marginalized by the rabbinic consensus.
% DISAPPEARANCE_RATIONALE: If this interpretive constraint vanished, the entire structure of post-Temple Jewish religious life would collapse. Communities would lose their framework for worship, identity, and legal continuity, leading to profound fragmentation and an existential crisis for the tradition.
% FOUNDING_PROBLEM: The destruction of the Second Temple rendered the central act of Jewish worship—material animal sacrifice—impossible, threatening the continuity of divine service and the coherence of the covenant.
% FOUNDING_PROBLEM_CORROBORATION: The problem of Temple absence remains live, as attested by the ongoing prayers for its rebuilding and the continued study of sacrifice laws. While the rabbinic authority asserts the transformation as a valid solution, traditionalist adherents and messianic restorationists corroborate the problem's persistence by their continued longing for and efforts towards material restoration, indicating the solution is not universally accepted as fully resolving the original problem.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__symbolic_transformation, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__symbolic_transformation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__symbolic_transformation, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(temple_sacrifice_commitment__symbolic_transformation, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__symbolic_transformation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_commitment__symbolic_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(temple_sacrifice_commitment__symbolic_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because the interpretive shift redefines a core religious obligation, imposing a new form of practice that may not fully satisfy those committed to the original material form. Suppression (0.7) is also high, as rabbinic authority actively enforces this interpretation, marginalizing dissenting views that advocate for literal material sacrifice. The theater ratio is low (0.2) because the transformed practices (prayer and study) are genuinely functional and central to religious life, not merely performative maintenance of a defunct system. The increasing extractiveness and suppression over time reflect the hardening of this interpretive consensus and the increasing marginalization of alternative views.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinic authority, this is a necessary and divinely sanctioned adaptation, a Rope that ensures the continuity of the covenant. From the perspective of traditionalist adherents and messianic restorationists, it is a Snare that redefines their core obligations and suppresses their longing for literal restoration, extracting their adherence to a transformed practice.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authority and community leaders are beneficiaries (d near 0.0) as they gain legitimacy, stability, and a functional framework for religious life. Traditionalist adherents and messianic restorationists are victims (d near 1.0) as they bear the cost of accepting a redefinition of a core religious practice that may conflict with their deeply held beliefs and identity, with limited exit options due to identity-lock within the tradition.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy by actively re-interpreting and re-instantiating the original mandate, rather than letting it atrophy. The 'founding problem' (absence of the Temple) is still 'live,' but the 'solution' (symbolic transformation) is 'contested.' This contestation prevents the constraint from becoming a Piton, as there are still active parties (traditionalists, restorationists) who feel its extractive force and resist its full normalization, even if their resistance is suppressed. The high extractiveness and suppression indicate it is a Tangled Rope, not a Piton, as it is actively maintained and extracts from identifiable victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authority_of_transformation,
    'Is the rabbinic authority''s re-interpretation of sacrifice as prayer and study a divinely authorized transformation or an adaptive human innovation?',
    'Theological and historical analysis of the scope of rabbinic interpretive power in relation to divine command, and comparative study of similar transformations in other religious traditions.',
    'If divinely authorized, the constraint''s extractiveness is a legitimate cost of adaptation; if human innovation, it represents an unauthorized claim to redefine divine will, increasing its effective extraction and potentially reclassifying it as a Snare from the perspective of those who reject the authority''s claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_of_transformation, conceptual, 'Ambiguity of interpretive authority''s scope.').

omega_variable(
    identity_lock_strength,
    'To what extent is the ''identity_locked'' exit option for traditionalist adherents and messianic restorationists a genuine internal commitment versus a structural suppression of alternatives?',
    'Sociological studies of ex-adherents and those who have adopted alternative interpretations, examining the psychological and social costs of deviation from the mainstream rabbinic consensus.',
    'If primarily internal, the suppression is less coercive; if primarily structural, the constraint''s effective suppression is higher, indicating a more coercive Tangled Rope or even a Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_strength, empirical, 'Structural vs. internalized suppression mechanism for identity-locked agents.').

omega_variable(
    founding_problem_status_objectivity,
    'Is the ''live'' status of the founding problem (absence of Temple) an objective reality or a narrative maintained by the rabbinic authority to justify the transformed practice?',
    'Analysis of the actual impact of Temple absence on contemporary religious life, independent of interpretive claims, and the motivations behind maintaining the ''live'' status.',
    'If objectively ''dead'' but claimed ''live,'' the constraint''s theater_ratio would be higher, and its classification would lean more towards Piton or Snare, as the justification for its existence would be performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_status_objectivity, empirical, 'Objectivity of the founding problem''s ''live'' status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__symbolic_transformation, 70, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t70, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 70, 0.1).
narrative_ontology:measurement(temp_tr_t500, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 500, 0.12).
narrative_ontology:measurement(temp_tr_t1000, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1000, 0.15).
narrative_ontology:measurement(temp_tr_t1500, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1500, 0.18).
narrative_ontology:measurement(temp_tr_t2024, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(temp_be_t70, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 70, 0.5).
narrative_ontology:measurement(temp_be_t500, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 500, 0.55).
narrative_ontology:measurement(temp_be_t1000, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1000, 0.6).
narrative_ontology:measurement(temp_be_t1500, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1500, 0.63).
narrative_ontology:measurement(temp_be_t2024, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t70, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 70, 0.6).
narrative_ontology:measurement(temp_su_t500, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 500, 0.63).
narrative_ontology:measurement(temp_su_t1000, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1000, 0.66).
narrative_ontology:measurement(temp_su_t1500, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1500, 0.68).
narrative_ontology:measurement(temp_su_t2024, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
