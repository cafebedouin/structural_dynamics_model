% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__behavioral_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hoa_covenant_scope__behavioral_control_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: hoa_covenant_scope__behavioral_control_reading
 *   human_readable: HOA Covenant â Behavioral Control and Aesthetic Conformity Reading
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   This constraint is the behavioral_control_reading of the
 *   hoa_covenant_scope kernel. It models homeowners association covenants as
 *   mechanisms whose operative function is the enforcement of aesthetic
 *   uniformity and behavioral conformity, justified under the cover of
 *   property value maximization. The covenant text (deed restrictions) is
 *   interpreted expansively to reach subjective aesthetic judgments,
 *   lifestyle restrictions, and suppression of political speech (yard signs,
 *   flags). The claim/metric independence is maintained: the constraint is
 *   claimed as snare (pure extraction with coordination cover) while metrics
 *   are authored to the actual observed operation â moderate but rising
 *   extraction, high suppression, and moderate theater.
 *
 * KEY AGENTS:
 *   - hoa_board: Agenda setter (moderate/constrained) â interprets vague covenant language and administers the enforcement apparatus.
 *   - conformist_majority: Primary beneficiary (organized/mobile) â captures diffuse property value protection and social control without paying direct enforcement costs.
 *   - nonconformist_homeowners: Primary target (moderate/constrained) â bears fines, legal threats, and loss of expressive autonomy.
 *   - aesthetic_minority: Secondary target (moderate/constrained) â pays through forced remediation and conformity costs for divergent aesthetic choices.
 *   - civil_liberties_advocates: Analytical observer (organized/analytical) â litigates and tracks the pattern across communities.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__behavioral_control_reading, 0.44).
domain_priors:suppression_score(hoa_covenant_scope__behavioral_control_reading, 0.72).
domain_priors:theater_ratio(hoa_covenant_scope__behavioral_control_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, extractiveness, 0.44).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__behavioral_control_reading, snare).
narrative_ontology:human_readable(hoa_covenant_scope__behavioral_control_reading, "HOA Covenant â Behavioral Control and Aesthetic Conformity Reading").
narrative_ontology:topic_domain(hoa_covenant_scope__behavioral_control_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__behavioral_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__behavioral_control_reading, '17f5c1b4-d7fc-4817-a1ae-5889cd489ad5').
narrative_ontology:cs_kernel_codification('17f5c1b4-d7fc-4817-a1ae-5889cd489ad5', fixed_text).
narrative_ontology:cs_authority_grounding('17f5c1b4-d7fc-4817-a1ae-5889cd489ad5', lineage).
narrative_ontology:cs_interpretation_layer_present('17f5c1b4-d7fc-4817-a1ae-5889cd489ad5').
narrative_ontology:cs_reading_relation('17f5c1b4-d7fc-4817-a1ae-5889cd489ad5', hoa_covenant_scope__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('17f5c1b4-d7fc-4817-a1ae-5889cd489ad5', hoa_covenant_scope__extraction_reading, influences).
narrative_ontology:cs_axiom('17f5c1b4-d7fc-4817-a1ae-5889cd489ad5', foundational, conformity_maximizes_property_value).
narrative_ontology:cs_axiom_status(conformity_maximizes_property_value, holdable).
narrative_ontology:cs_axiom_grounding('17f5c1b4-d7fc-4817-a1ae-5889cd489ad5', conformity_maximizes_property_value, empirically_contingent).
narrative_ontology:cs_axiom('17f5c1b4-d7fc-4817-a1ae-5889cd489ad5', foundational, subjective_aesthetic_judgments_legitimate).
narrative_ontology:cs_axiom_status(subjective_aesthetic_judgments_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('17f5c1b4-d7fc-4817-a1ae-5889cd489ad5', subjective_aesthetic_judgments_legitimate, conventional).
narrative_ontology:cs_reference_frame('17f5c1b4-d7fc-4817-a1ae-5889cd489ad5', deed_restricted_aesthetic_order).
narrative_ontology:cs_drift_state('17f5c1b4-d7fc-4817-a1ae-5889cd489ad5', contemporary_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('17f5c1b4-d7fc-4817-a1ae-5889cd489ad5', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, conformist_majority).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, nonconformist_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, aesthetic_minority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces covenant provisions, issues violation notices, levies fines, and controls the aesthetic agenda of the community. Derives institutional authority and social deference from the enforcement apparatus, with discretion to apply subjective aesthetic standards.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, hoa_board, agenda_setter,
    moderate, biographical, constrained, local).

% Homeowners who support strict covenant enforcement because it protects their vision of neighborhood character and, they believe, their property values. They benefit from the suppression of aesthetically divergent choices without bearing direct enforcement costs, and they form the voting bloc that sustains the board's mandate.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, conformist_majority, beneficiary,
    organized, biographical, mobile, local).

% Homeowners who wish to display political signs, fly flags, paint homes unconventional colors, or maintain nonstandard landscaping. They receive violation notices, fines, and legal threats, and must choose between compliance, costly legal challenge, or selling in a market potentially discounted by their documented disputes.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, nonconformist_homeowners, payer,
    moderate, biographical, constrained, local).

% Residents whose aesthetic preferences, lifestyle choices, or political expression fall outside the covenant's narrow standards. They bear the direct costs of fines, mandatory remediation, and forced conformity, with limited recourse because the covenant runs with the land.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, aesthetic_minority, payer,
    moderate, biographical, constrained, local).

% Legal and advocacy organizations that challenge covenant overreach, particularly restrictions on political speech and subjective aesthetic judgments. They observe the structural pattern across multiple communities and litigate test cases without being bound by any single HOA's rules.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, civil_liberties_advocates, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hoa_covenant_scope__behavioral_control_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Purports to coordinate homeowners around a shared aesthetic standard to prevent individual choices from depressing adjacent property values, though the enforcement scope has expanded far beyond objective externalities into subjective lifestyle regulation.
% TRANSFER_FUNCTION: Moves behavioral autonomy, aesthetic choice, and political expression from nonconformist homeowners to the conformist majority and the enforcing board, converting difference into fines, compliance costs, attorney fees, and suppressed self-expression.
% ABSENT_VOICES: Nonconformist homeowners facing selective enforcement, renters and non-property-owning residents who live under the rules but have no vote in covenant amendment, and municipal housing policy advocates who would argue for individual property rights over collective aesthetic control.
% DISAPPEARANCE_RATIONALE: If the covenant vanished overnight, nonconformists would alter properties immediately, yard signs and flags would appear, aesthetic diversity would increase, and the conformist majority would lose the coercive mechanism that enforces their preferred neighborhood character; property value arguments would shift to market mechanisms rather than enforced homogeneity.
% FOUNDING_PROBLEM: Preventing incompatible land uses and physical neglect that could degrade a residential area's desirability and property values through collectively binding deed restrictions.
% FOUNDING_PROBLEM_CORROBORATION: Municipal land-use historians and early subdivision developers attest the original covenants targeted land use and building standards. Civil liberties attorneys and nonconformist homeowners attest the original problem is substantially solved by municipal zoning and the covenant now persists as behavioral control; no external neutral party unanimously corroborates the live-problem claim without noting scope creep.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__behavioral_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__behavioral_control_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__behavioral_control_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hoa_covenant_scope__behavioral_control_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__behavioral_control_reading, 0.44, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hoa_covenant_scope__behavioral_control_reading_tests).
:- end_tests(hoa_covenant_scope__behavioral_control_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.44) is moderate but rising because the covenant's enforcement scope has drifted from objective land-use controls into subjective behavioral and aesthetic regulation, converting lifestyle difference into a taxable event. Suppression (0.72) is high because the constraint actively suppresses alternatives: political speech is banned, paint colors are regulated, and exit is costly because the covenant runs with the land. Theater ratio (0.25) is moderate-low â the enforcement is functionally real (fines, liens), though a growing share of activity pursues aesthetic conformity rather than genuine property protection. Accessibility collapse (0.68) is high because once a homeowner is in the HOA, the alternative (individual property choice) is legally foreclosed without collective amendment. Resistance (0.48) is moderate because isolated nonconformists challenge selectively, but organized opposition is fragmented.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter and beneficiary seats experience the constraint as legitimate community self-defense against free-riding aesthetic choices. The payer seats experience the same structure as coercive social control that extracts autonomy and money under the cover of property law. The engine computes this divergence from the structural asymmetry in power, exit, and role â the authored claim does not adjudicate the divergence, it names it.
 *
 * DIRECTIONALITY LOGIC:
 *   The hoa_board and conformist_majority sit on the beneficiary side: the board gains institutional authority and the majority gains enforced homogeneity and perceived property value protection. Nonconformist_homeowners and aesthetic_minority are the targets: they pay in money, autonomy, and expressive constraint. The civil_liberties_advocates seat is analytical and outside the directionality derivation. The high suppression and constrained exit options push the victim seats toward the full-target end of the directionality spectrum, amplifying their effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling as tangled_rope because the coordination function (genuine externality control) has atrophied: modern municipal zoning handles the original land-use problem, while the covenant machinery has been repurposed for lifestyle regulation. It is not a rope because there are identifiable victims whose costs exceed any benefit, and alternatives (individual choice) are actively suppressed rather than genuinely inferior. It is not a scaffold because it carries no sunset clause and shows no transitional intent. It is not a piton because the conformist majority and the board actively benefit from and maintain the constraint; it is not merely inertial performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity_behavioral,
    'Is the covenant''s primary function behavioral control for property value, coordination of shared infrastructure, or extraction via fines and power consolidation?',
    'Comparative analysis of enforcement patterns: if enforcement concentrates on aesthetic and speech violations over infrastructure maintenance, the behavioral control reading is supported; if fine revenue dominates the HOA budget, the extraction reading is supported; if maintenance outcomes and genuine externality reduction are the primary measurable outputs, the coordination reading is supported.',
    'Determines which sibling constraint story best models the actual operation of this kernel, with direct consequences for computed classification and directionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity_behavioral, conceptual, 'Which reading of the HOA covenant kernel is structurally true').

omega_variable(
    aesthetic_judgment_objectivity,
    'Are the covenant''s aesthetic standards objective and value-linked, or subjective tools for enforcing social conformity?',
    'Blind assessment of violation photos by external appraisers to test inter-rater reliability; correlation analysis between violation types and documented market value impacts.',
    'If standards are objectively value-linked, the extraction measure is lower; if subjective, the constraint operates as social control and the effective extraction is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aesthetic_judgment_objectivity, empirical, 'Objectivity of aesthetic standards in covenant enforcement').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of nonconformity achieved primarily through structural penalties or internalized social pressure?',
    'Post-exit trajectory analysis: do nonconformists who sell and leave HOA communities resume expressive behavior immediately, or does suppression persist in new non-HOA contexts?',
    'If internalized, the constraint''s effective suppression exceeds the structural measure â the target carries the suppression with them after physical exit, raising the true extraction level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__behavioral_control_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa_behavioral_tr_t0, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hoa_behavioral_tr_t5, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(hoa_behavioral_tr_t10, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(hoa_behavioral_tr_t15, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(hoa_behavioral_tr_t20, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(hoa_behavioral_tr_t25, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 25, 0.25).

% Extraction over time
narrative_ontology:measurement(hoa_behavioral_be_t0, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(hoa_behavioral_be_t5, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 5, 0.34).
narrative_ontology:measurement(hoa_behavioral_be_t10, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(hoa_behavioral_be_t15, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(hoa_behavioral_be_t20, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(hoa_behavioral_be_t25, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 25, 0.44).

% Suppression requirement over time
narrative_ontology:measurement(hoa_behavioral_su_t0, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(hoa_behavioral_su_t5, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(hoa_behavioral_su_t10, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(hoa_behavioral_su_t15, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(hoa_behavioral_su_t20, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(hoa_behavioral_su_t25, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


% DUAL FORMULATION NOTE:
% This constraint is one member of the hoa_covenant_scope family. The epsilon values differ across siblings: coordination_reading would carry low epsilon and rope classification; extraction_reading would carry high epsilon and snare or tangled_rope classification. This behavioral_control_reading occupies the middle range, where the coordination story has become cover for social control.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
