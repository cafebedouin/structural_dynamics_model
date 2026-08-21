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
 *   constraint_id: temple_sacrifice_commitment__symbolic_transformation
 *   human_readable: Temple Sacrifice Commitment: Symbolic Transformation Reading
 *   domain: religious_law/halakhic_tradition/commitment_system_theory
 *
 * SUMMARY:
 *   This constraint represents the 'symbolic transformation' reading of the
 *   Temple sacrifice commitment within Halakhic tradition. It asserts that
 *   rabbinic authority has legitimately transformed the divine command for
 *   material sacrifice into an obligation for prayer and study, which are now
 *   considered the primary instantiation of the commitment. This reading is
 *   distinct from those that view study as mere preparation or the commitment
 *   as suspended. The constraint is classified as a Tangled Rope because it
 *   provides a coordination function for communal religious life while
 *   extracting from those who adhere to a more literal or material
 *   understanding of the original command.
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
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__symbolic_transformation, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__symbolic_transformation, "Temple Sacrifice Commitment: Symbolic Transformation Reading").
narrative_ontology:topic_domain(temple_sacrifice_commitment__symbolic_transformation, "religious_law/halakhic_tradition/commitment_system_theory").

domain_priors:requires_active_enforcement(temple_sacrifice_commitment__symbolic_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__symbolic_transformation, 'a3df50f4-a1d7-41e7-92f9-5537441df6ff').
narrative_ontology:cs_kernel_codification('a3df50f4-a1d7-41e7-92f9-5537441df6ff', fixed_text).
narrative_ontology:cs_authority_grounding('a3df50f4-a1d7-41e7-92f9-5537441df6ff', lineage).
narrative_ontology:cs_interpretation_layer_present('a3df50f4-a1d7-41e7-92f9-5537441df6ff').
narrative_ontology:cs_reading_relation('a3df50f4-a1d7-41e7-92f9-5537441df6ff', temple_sacrifice_commitment__study_as_exercise, influences).
narrative_ontology:cs_reading_relation('a3df50f4-a1d7-41e7-92f9-5537441df6ff', temple_sacrifice_commitment__performance_only, forecloses).
narrative_ontology:cs_reading_relation('a3df50f4-a1d7-41e7-92f9-5537441df6ff', temple_sacrifice_commitment__hybrid_preparatory, coexists_with).
narrative_ontology:cs_axiom('a3df50f4-a1d7-41e7-92f9-5537441df6ff', foundational, rabbinic_authority_to_transform_divine_commands).
narrative_ontology:cs_axiom_status(rabbinic_authority_to_transform_divine_commands, holdable).
narrative_ontology:cs_axiom_grounding('a3df50f4-a1d7-41e7-92f9-5537441df6ff', rabbinic_authority_to_transform_divine_commands, conventional).
narrative_ontology:cs_axiom('a3df50f4-a1d7-41e7-92f9-5537441df6ff', foundational, prayer_study_as_full_halakhic_fulfillment).
narrative_ontology:cs_axiom_status(prayer_study_as_full_halakhic_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('a3df50f4-a1d7-41e7-92f9-5537441df6ff', prayer_study_as_full_halakhic_fulfillment, theological).
narrative_ontology:cs_reference_frame('a3df50f4-a1d7-41e7-92f9-5537441df6ff', post_temple_rabbinic_halakha).
narrative_ontology:cs_drift_state('a3df50f4-a1d7-41e7-92f9-5537441df6ff', contemporary_pluralistic_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a3df50f4-a1d7-41e7-92f9-5537441df6ff', '').
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

% The primary interpreters and adjudicators of Halakha. They assert the authority to reinterpret and transform divine commands in response to changed historical conditions, thereby maintaining the continuity and relevance of the tradition. They benefit from the stability and adaptability of the religious system under their guidance.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority, agenda_setter,
    institutional, generational, identity_locked, global).

% Implement the directives of the rabbinic authority, promoting prayer and study as the contemporary fulfillment of the sacrifice commitment. They benefit from the coherence and communal unity fostered by this interpretation, which provides a clear path for religious observance in the absence of the Temple.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, community_leaders, beneficiary,
    organized, biographical, constrained, local).

% Adhere strictly to the literal interpretation of divine commands, viewing material sacrifice as non-negotiable and irreplaceable. They bear the cost of cognitive dissonance and marginalization within the broader community, as their literalist commitment is deemed outdated or incomplete by the dominant interpretive framework. Their identity is deeply tied to the historical practice.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, traditionalist_adherents, payer,
    powerless, generational, identity_locked, local).

% Actively anticipate and prepare for the rebuilding of the Temple and the resumption of material sacrifices. They view the symbolic transformation as a temporary expedient, not a permanent redefinition, and bear the cost of being seen as outside the mainstream, or even as challenging rabbinic authority. Their commitment to future material performance is a source of tension.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, messianic_restorationists, payer,
    moderate, civilizational, constrained, regional).

% Analyze the evolution of religious law and practice from a sociological or historical perspective. They observe the mechanisms of reinterpretation and the social dynamics of adherence and resistance without direct participation in the religious commitment.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, secular_observers, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent and accessible framework for Jewish religious observance in the post-Temple era, ensuring continuity of divine command fulfillment through prayer and study, thereby maintaining communal identity and practice.
% TRANSFER_FUNCTION: Transfers the locus of divine command fulfillment from material sacrifice to intellectual and spiritual engagement (prayer and study), thereby reallocating communal resources and individual devotional effort.
% ABSENT_VOICES: Ancient priestly lineages and those who believe only a direct divine command can alter the sacrificial requirements are structurally absent from the interpretive discourse; they would argue that human authority cannot unilaterally transform a divine commandment, and that prayer and study are insufficient substitutes.
% DISAPPEARANCE_RATIONALE: If the symbolic transformation interpretation vanished, the vast majority of contemporary Jewish religious practice would lose its halakhic grounding. Communities would fragment, individual observance would become incoherent, and the continuity of the tradition would be severely challenged, leading to a profound reorganization of religious life.
% FOUNDING_PROBLEM: The destruction of the Second Temple rendered the central act of Jewish worship—material animal sacrifice—impossible, creating a crisis of religious observance and continuity for a divinely commanded practice.
% FOUNDING_PROBLEM_CORROBORATION: The problem of Temple destruction and the inability to perform sacrifices is universally acknowledged across all Jewish denominations and historical scholarship. The rabbinic authority's solution is widely accepted as the means to maintain religious life in exile, corroborated by centuries of communal practice and theological discourse.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__symbolic_transformation, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__symbolic_transformation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__symbolic_transformation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(temple_sacrifice_commitment__symbolic_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__symbolic_transformation, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.65) because the transformation redefines a core religious obligation, imposing a new form of observance on adherents, particularly those who might prefer or expect material performance. Suppression (0.7) is also high, as the rabbinic authority actively enforces this interpretation, marginalizing dissenting views and limiting alternative forms of religious expression. The theater ratio is low (0.2) because the reinterpretation is genuinely functional for maintaining religious life, not merely performative. The accessibility collapse is moderate (0.4) as alternatives (like literal preparation for sacrifice) are not entirely foreclosed but are significantly disincentivized or deemed illegitimate by the dominant authority. Resistance is moderate (0.5) from traditionalist and messianic groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinic authority, this is a necessary and legitimate adaptation (Rope-like coordination). From the perspective of traditionalist adherents, it is an imposition that extracts their original understanding of divine command (Snare-like extraction). The engine's per-seat classification will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authority and community leaders are beneficiaries (low d) as they gain legitimacy and maintain communal coherence through this interpretation. Traditionalist adherents and messianic restorationists are targets (high d) as they bear the cost of having their literal understanding of the commitment redefined or superseded, and their preferred forms of observance marginalized. Their identity is often 'identity_locked' to the original, material understanding of the command.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the transformation as a pure Mountain (natural evolution of tradition) or a pure Snare (unjustified imposition). By identifying it as a Tangled Rope, the framework acknowledges both the genuine coordination function (maintaining religious life post-Temple) and the asymmetric extraction from those whose understanding of the commitment is superseded by the authorized reinterpretation. The 'live' status of the founding problem (Temple destruction) confirms the ongoing need for a solution, but the 'contested' corroboration highlights the dispute over the nature of that solution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authority_to_redefine_divine_command,
    'Does rabbinic authority possess the inherent power to fundamentally redefine the instantiation of a divine command, or only to interpret its application under changed circumstances?',
    'Theological and jurisprudential analysis of historical precedents for halakhic transformation versus interpretation, and the explicit articulation of the scope of rabbinic legislative power in foundational texts.',
    'If the authority to redefine is limited, this reading''s legitimacy is weakened, increasing its effective extractiveness for traditionalists and potentially reclassifying it closer to a Snare. If the authority is absolute, its coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_to_redefine_divine_command, conceptual, 'The scope of rabbinic authority in transforming divine commands.').

omega_variable(
    material_vs_symbolic_equivalence,
    'Are prayer and study truly equivalent in spiritual efficacy and halakhic fulfillment to material animal sacrifice, or are they merely the best available substitute?',
    'Empirical (theological) evidence of divine acceptance, or a consensus among diverse theological schools regarding the spiritual equivalence. This is a question within the religious framework itself.',
    'If they are not truly equivalent, the ''transformation'' is a form of extraction from those who believe in the unique efficacy of material sacrifice, increasing the constraint''s extractiveness. If they are equivalent, the coordination function is pure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(material_vs_symbolic_equivalence, empirical, 'Spiritual equivalence of symbolic and material religious acts.').

omega_variable(
    kernel_reading_identity,
    'This constraint is the ''symbolic_transformation'' reading of the ''temple_sacrifice_commitment'' kernel. What structural elements would change if a sibling reading were adopted?',
    'Comparative analysis of the ''study_as_exercise'', ''performance_only'', and ''hybrid_preparatory'' readings, identifying their distinct axioms and beneficiary/victim sets.',
    'Each sibling reading would alter the claimed type, extractiveness, and beneficiary/victim structure. For example, ''performance_only'' would likely classify as a Piton (defunct practice) or Snare (if actively suppressed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural differences between kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__symbolic_transformation, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 0, 0.2).
narrative_ontology:measurement(temp_tr_t20, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 20, 0.2).
narrative_ontology:measurement(temp_tr_t40, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 40, 0.2).
narrative_ontology:measurement(temp_tr_t60, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 60, 0.2).
narrative_ontology:measurement(temp_tr_t80, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 80, 0.2).
narrative_ontology:measurement(temp_tr_t100, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(temp_be_t20, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(temp_be_t40, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 40, 0.63).
narrative_ontology:measurement(temp_be_t60, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 60, 0.64).
narrative_ontology:measurement(temp_be_t80, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 80, 0.65).
narrative_ontology:measurement(temp_be_t100, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 100, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(temp_su_t20, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 20, 0.67).
narrative_ontology:measurement(temp_su_t40, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(temp_su_t60, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 60, 0.69).
narrative_ontology:measurement(temp_su_t80, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 80, 0.7).
narrative_ontology:measurement(temp_su_t100, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 100, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__symbolic_transformation, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
