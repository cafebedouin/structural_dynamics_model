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
 *   constraint_id: temple_sacrifice_commitment__symbolic_transformation
 *   human_readable: Temple Sacrifice Commitment: Symbolic Transformation Reading
 *   domain: religious_law/halakhic_tradition/commitment_system_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'symbolic_transformation' reading
 *   of the Temple sacrifice commitment kernel. From this perspective, the
 *   destruction of the Temple necessitated an authorized reinterpretation of
 *   divine command, where prayer and study became the new, legitimate
 *   instantiation of the sacrificial commitment, rather than mere substitutes
 *   for a suspended practice. This reading asserts the rabbinic authority's
 *   power to redefine core religious obligations to ensure continuity of
 *   faith. While it provides a functional path for the majority, it extracts
 *   from those who adhere strictly to the material performance of sacrifices.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__symbolic_transformation, 0.65).
domain_priors:suppression_score(temple_sacrifice_commitment__symbolic_transformation, 0.55).
domain_priors:theater_ratio(temple_sacrifice_commitment__symbolic_transformation, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, extractiveness, 0.65).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__symbolic_transformation, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__symbolic_transformation, "Temple Sacrifice Commitment: Symbolic Transformation Reading").
narrative_ontology:topic_domain(temple_sacrifice_commitment__symbolic_transformation, "religious_law/halakhic_tradition/commitment_system_theory").

domain_priors:requires_active_enforcement(temple_sacrifice_commitment__symbolic_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__symbolic_transformation, '47da4722-bf09-40fd-98ee-572fff024d43').
narrative_ontology:cs_kernel_codification('47da4722-bf09-40fd-98ee-572fff024d43', fixed_text).
narrative_ontology:cs_authority_grounding('47da4722-bf09-40fd-98ee-572fff024d43', lineage).
narrative_ontology:cs_interpretation_layer_present('47da4722-bf09-40fd-98ee-572fff024d43').
narrative_ontology:cs_reading_relation('47da4722-bf09-40fd-98ee-572fff024d43', temple_sacrifice_commitment__study_as_exercise, influences).
narrative_ontology:cs_reading_relation('47da4722-bf09-40fd-98ee-572fff024d43', temple_sacrifice_commitment__performance_only, forecloses).
narrative_ontology:cs_reading_relation('47da4722-bf09-40fd-98ee-572fff024d43', temple_sacrifice_commitment__hybrid_preparatory, forecloses).
narrative_ontology:cs_axiom('47da4722-bf09-40fd-98ee-572fff024d43', foundational, divine_command_is_adaptable_by_authority).
narrative_ontology:cs_axiom_status(divine_command_is_adaptable_by_authority, holdable).
narrative_ontology:cs_axiom_grounding('47da4722-bf09-40fd-98ee-572fff024d43', divine_command_is_adaptable_by_authority, theological).
narrative_ontology:cs_axiom('47da4722-bf09-40fd-98ee-572fff024d43', foundational, prayer_study_as_sacrifice_equivalent).
narrative_ontology:cs_axiom_status(prayer_study_as_sacrifice_equivalent, holdable).
narrative_ontology:cs_axiom_grounding('47da4722-bf09-40fd-98ee-572fff024d43', prayer_study_as_sacrifice_equivalent, theological).
narrative_ontology:cs_reference_frame('47da4722-bf09-40fd-98ee-572fff024d43', post_temple_halakhic_adaptation).
narrative_ontology:cs_drift_state('47da4722-bf09-40fd-98ee-572fff024d43', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('47da4722-bf09-40fd-98ee-572fff024d43', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, adherent_community).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, traditionalist_factions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The central body that authorized and codified the transformation of sacrifice into prayer and study. They gain legitimacy and control over religious practice by providing a viable path for commitment in the absence of the Temple, but are constrained by the need to maintain continuity with tradition.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority, agenda_setter,
    institutional, generational, constrained, global).

% The majority of the religious community that accepts and practices the transformed forms of commitment (prayer and study). They benefit from having a clear, accessible path to fulfill divine commands, but are constrained by the authority's interpretation.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, adherent_community, beneficiary,
    organized, biographical, constrained, global).

% Groups or individuals who reject the symbolic transformation, insisting that only material Temple sacrifices fulfill the divine command. They bear the cost of marginalization and the inability to perform what they believe is true worship, often feeling identity-locked to the original practice.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, traditionalist_factions, payer,
    moderate, generational, identity_locked, local).

% Groups who view the current practices as temporary until the rebuilding of the Temple and the restoration of material sacrifices. They observe the current constraint but anticipate its eventual supersession, which shapes their engagement.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, messianic_movements, observer,
    moderate, civilizational, mobile, global).

% Academics and theologians who analyze the historical, legal, and theological development of the sacrifice commitment and its transformation. They provide critical and historical context but do not directly participate in enforcing or resisting the constraint.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, scholarly_interpreters, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority).
narrative_ontology:fixing_cost_class(temple_sacrifice_commitment__symbolic_transformation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the religious community around new, accessible forms of divine worship and commitment (prayer and study) in the absence of the Temple, ensuring continuity of religious life.
% TRANSFER_FUNCTION: Transfers the locus of religious practice from material sacrifice to spiritual and intellectual engagement, reallocating communal effort, focus, and resources towards these new forms of commitment.
% ABSENT_VOICES: Those who insist on the literal, material performance of sacrifices as the only valid form of commitment, and those who reject the authority of the transforming rabbinic bodies. They are often marginalized or form dissenting communities.
% DISAPPEARANCE_RATIONALE: If the symbolic transformation and its rabbinic authorization vanished overnight, the entire structure of post-Temple Judaism would collapse. The vast majority of current religious practice would be rendered invalid, leading to profound theological and communal disarray.
% FOUNDING_PROBLEM: The destruction of the Second Temple (70 CE) rendered the performance of material sacrifices, central to divine worship and atonement, impossible, threatening the continuity of Jewish religious life.
% FOUNDING_PROBLEM_CORROBORATION: Historical texts (e.g., Josephus, Talmud), archaeological evidence of Temple destruction, and the continued absence of the Temple corroborate the founding problem. The ongoing theological discourse around messianic restoration further attests to its live status.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__symbolic_transformation, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__symbolic_transformation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__symbolic_transformation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates the religious community around new practices (prayer and study) in the absence of the Temple, providing a means to fulfill divine commands. However, it simultaneously extracts from traditionalist factions who believe only material sacrifices are valid, effectively marginalizing their mode of commitment. Extractiveness is moderate-high because it redefines a core religious obligation, imposing a new framework. Suppression is moderate as the rabbinic authority actively enforces this interpretation, making dissent costly. Theater ratio is low because the new practices are genuinely performed and central to religious life, not merely performative maintenance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the rabbinic authority and the adherent community, this constraint is a necessary and divinely sanctioned adaptation, providing continuity and meaning. From the perspective of traditionalist factions, it represents a deviation from divine law and an illegitimate redefinition of core obligations, imposing a heavy cost on their faith. The engine's classification as Tangled Rope captures this inherent asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The rabbinic authority is a beneficiary, gaining enhanced legitimacy and control by providing a solution to a profound religious crisis. The adherent community also benefits from having a clear path to commitment. Traditionalist factions are victims, as their preferred mode of worship is rendered impossible or illegitimate by this transformation, leading to identity-locked exit options. Messianic movements and scholarly interpreters act as observers, analyzing the constraint without direct participation in its enforcement or resistance.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_identity_symbolic_transformation,
    'Is this constraint a valid instantiation of the ''symbolic_transformation'' reading of the ''temple_sacrifice_commitment'' kernel?',
    'Comparison with historical rabbinic texts and theological arguments that explicitly articulate the redefinition of sacrifice into prayer and study as a legitimate, authorized transformation, rather than a temporary substitute.',
    'If confirmed, the analysis of this reading proceeds as authored. If disconfirmed, and the reading is found to be a mischaracterization, the constraint would need to be re-authored under a more accurate reading, potentially altering its extractiveness and claimed type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_symbolic_transformation, conceptual, 'Confirms the specific reading being instantiated.').

omega_variable(
    authority_legitimacy_of_transformation,
    'Is the rabbinic authority''s claim to redefine divine command for the sacrifice commitment genuinely legitimate, or does it represent an unauthorized drift?',
    'Analysis of the historical and theological precedents for rabbinic interpretive authority, and the degree of consensus or dissent among contemporary and historical religious scholars outside the immediate benefiting parties.',
    'If the authority''s claim is widely seen as unauthorized, the constraint''s effective extractiveness would be significantly higher, and its classification might shift towards a Snare, as the coordination story would be undermined by a lack of legitimate grounding. If confirmed as legitimate, the Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_legitimacy_of_transformation, conceptual, 'Assesses the legitimacy of the authority''s power to transform core religious obligations.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of traditionalist factions structural (institutional marginalization) or internalized (identity-locked adherence to old forms)?',
    'Post-exit suppression trajectory: if traditionalist adherence persists even when institutional barriers to alternative practices are removed (e.g., in new, less structured communities), reclassify as partially internalized. If it only persists due to active institutional pressure, it is structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the traditionalist carries the suppression with them after exit, making true exit more difficult. This would amplify the effective extraction for these victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for traditionalist factions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__symbolic_transformation, 70, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t70, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 70, 0.1).
narrative_ontology:measurement(temp_tr_t120, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 120, 0.1).
narrative_ontology:measurement(temp_tr_t170, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 170, 0.1).
narrative_ontology:measurement(temp_tr_t220, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 220, 0.1).
narrative_ontology:measurement(temp_tr_t270, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 270, 0.1).
narrative_ontology:measurement(temp_tr_t300, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 300, 0.1).

% Extraction over time
narrative_ontology:measurement(temp_be_t70, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 70, 0.55).
narrative_ontology:measurement(temp_be_t120, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 120, 0.6).
narrative_ontology:measurement(temp_be_t170, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 170, 0.63).
narrative_ontology:measurement(temp_be_t220, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 220, 0.64).
narrative_ontology:measurement(temp_be_t270, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 270, 0.65).
narrative_ontology:measurement(temp_be_t300, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 300, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t70, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 70, 0.45).
narrative_ontology:measurement(temp_su_t120, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 120, 0.5).
narrative_ontology:measurement(temp_su_t170, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 170, 0.53).
narrative_ontology:measurement(temp_su_t220, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 220, 0.54).
narrative_ontology:measurement(temp_su_t270, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 270, 0.55).
narrative_ontology:measurement(temp_su_t300, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 300, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__symbolic_transformation, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__hybrid_preparatory).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('symbolic_transformation') of the 'temple_sacrifice_commitment' kernel. Its ε value differs significantly from other readings due to its assertion of authorized redefinition versus mere suspension or archival preservation. This reading directly forecloses the 'performance_only' reading and the 'hybrid_preparatory' reading, and influences 'study_as_exercise' by providing its theological grounding.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
