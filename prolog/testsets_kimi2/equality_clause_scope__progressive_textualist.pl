% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__progressive_textualist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__progressive_textualist, []).

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
 *   constraint_id: equality_clause_scope__progressive_textualist
 *   human_readable: Progressive Textualist Equality Clause (Amendment-Driven Expansion)
 *   domain: constitutional_law/political_philosophy/civil_rights_history
 *
 * SUMMARY:
 *   This constraint instantiates the progressive_textualist reading of the
 *   equality_clause_scope kernel: the constitutional text contains a general
 *   equality principle, but the legitimate expansion of its application scope
 *   must occur through the democratic amendment process (Article V
 *   supermajorities), not through judicial reinterpretation. It competes with
 *   restrictive_originalist (equality limited to propertied white males
 *   within the 18th-century social contract) and expansive_universalist
 *   (equality as self-evident universal truth applying immediately regardless
 *   of textual history). The claim/metric gap is deliberate: the constraint
 *   is claimed as a legitimate coordination mechanism (democratic
 *   constitutionalism) while the metrics acknowledge substantial extraction
 *   from excluded minorities who must wait for supermajority consent.
 *
 * KEY AGENTS:
 *   - Democratic amending majorities: agenda_setter (organized/constrained) â control the formal gate for equality expansion.
 *   - Enfranchised citizenry: beneficiary (moderate/constrained) â retain democratic gatekeeping authority over constitutional meaning.
 *   - Excluded minorities: payer (powerless/trapped) â bear the cost of supermajoritarian barriers to inclusion.
 *   - Federal judiciary: observer (institutional/analytical) â constrained from expanding equality through interpretation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__progressive_textualist, 0.62).
domain_priors:suppression_score(equality_clause_scope__progressive_textualist, 0.55).
domain_priors:theater_ratio(equality_clause_scope__progressive_textualist, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, extractiveness, 0.62).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__progressive_textualist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__progressive_textualist, "Progressive Textualist Equality Clause (Amendment-Driven Expansion)").
narrative_ontology:topic_domain(equality_clause_scope__progressive_textualist, "constitutional_law/political_philosophy/civil_rights_history").

domain_priors:requires_active_enforcement(equality_clause_scope__progressive_textualist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__progressive_textualist, 'd7a441d6-cb01-4b7e-b99f-32f2e5b4920b').
narrative_ontology:cs_kernel_codification('d7a441d6-cb01-4b7e-b99f-32f2e5b4920b', fixed_text).
narrative_ontology:cs_authority_grounding('d7a441d6-cb01-4b7e-b99f-32f2e5b4920b', lineage).
narrative_ontology:cs_interpretation_layer_present('d7a441d6-cb01-4b7e-b99f-32f2e5b4920b').
narrative_ontology:cs_reading_relation('d7a441d6-cb01-4b7e-b99f-32f2e5b4920b', equality_clause_scope__restrictive_originalist, forecloses).
narrative_ontology:cs_reading_relation('d7a441d6-cb01-4b7e-b99f-32f2e5b4920b', equality_clause_scope__expansive_universalist, coexists_with).
narrative_ontology:cs_axiom('d7a441d6-cb01-4b7e-b99f-32f2e5b4920b', foundational, general_equality_principle_in_text).
narrative_ontology:cs_axiom_status(general_equality_principle_in_text, holdable).
narrative_ontology:cs_axiom_grounding('d7a441d6-cb01-4b7e-b99f-32f2e5b4920b', general_equality_principle_in_text, conventional).
narrative_ontology:cs_axiom('d7a441d6-cb01-4b7e-b99f-32f2e5b4920b', foundational, amendment_process_supremacy).
narrative_ontology:cs_axiom_status(amendment_process_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('d7a441d6-cb01-4b7e-b99f-32f2e5b4920b', amendment_process_supremacy, conventional).
narrative_ontology:cs_reference_frame('d7a441d6-cb01-4b7e-b99f-32f2e5b4920b', founding_textual_equality).
narrative_ontology:cs_drift_state('d7a441d6-cb01-4b7e-b99f-32f2e5b4920b', contemporary_civil_rights_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d7a441d6-cb01-4b7e-b99f-32f2e5b4920b', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__progressive_textualist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, enfranchised_citizenry).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, excluded_minorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the formal Article V amendment process and determine when the constitutional equality principle expands to new groups; cannot act unilaterally and must assemble supermajority consent, but hold the procedural gatekeeping power over inclusion.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, democratic_amending_majorities, agenda_setter,
    organized, generational, constrained, national).

% Benefit from a constitutional order in which the meaning of equality cannot be redefined by courts without their democratic consent; retain gatekeeping authority over constitutional change through representative and state-level ratification structures.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, enfranchised_citizenry, beneficiary,
    moderate, generational, constrained, national).

% Bear the cost of a constitutional promise of equality that does not yet apply to them; must wait for supermajority democratic consent to gain textual inclusion, with no judicial shortcut available under this reading, and cannot readily exit the constitutional system.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, excluded_minorities, payer,
    powerless, biographical, trapped, national).

% Interprets the constitutional text but is structurally constrained from expanding the scope of equality through case law; legitimacy derives from enforcing the fixed text and the outcomes of the amendment process, not from independent moral reasoning about universal rights.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, federal_judiciary, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equality_clause_scope__progressive_textualist, enfranchised_citizenry).
narrative_ontology:fixing_cost_class(equality_clause_scope__progressive_textualist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates legitimate constitutional change by requiring broad democratic consensus through supermajoritarian amendment, preventing unelected judges from redefining fundamental rights and ensuring that expansions of equality carry durable political legitimacy.
% TRANSFER_FUNCTION: Moves authority to define the scope of constitutional equality from the judiciary to supermajoritarian political coalitions, and moves the cost of continued exclusion onto groups not yet incorporated by formal amendment.
% ABSENT_VOICES: Groups seeking immediate judicial recognition of universal equality are structurally sidelined by the amendment-only pathway; legal scholars and advocates who view judicial review as the proper engine of rights expansion are excluded from the legitimating conversation.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, constitutional equality would either ossify at its original narrow scope (restrictive originalism) or expand through judicial decree (expansive universalism), fundamentally altering the separation of powers and the democratic legitimacy structure of the constitutional order.
% FOUNDING_PROBLEM: How to maintain a stable written constitution while permitting legitimate democratic revision of its principles without empowering unelected judges to redefine fundamental rights according to transient moral consensus.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and political theorists outside the immediate beneficiary class attest to the founding concern with democratic legitimacy and the fear of judicial supremacy; however, civil rights advocates from outside the benefiting parties argue the amendment track is now moribund for equality expansion and judicial incorporation has effectively superseded it.
narrative_ontology:disappearance_verdict(equality_clause_scope__progressive_textualist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__progressive_textualist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__progressive_textualist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equality_clause_scope__progressive_textualist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__progressive_textualist, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__progressive_textualist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equality_clause_scope__progressive_textualist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equality_clause_scope__progressive_textualist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderately high (0.62) because the supermajoritarian amendment requirement forces excluded minorities to bear the cost of democratic delay; suppression is moderate (0.55) because the constraint structurally blocks judicial reinterpretation as an alternative path to inclusion. Theater is moderate-low (0.30): appeals to popular sovereignty are genuine but partially performative in masking majoritarian gatekeeping. Accessibility collapse is moderate (0.45): judicial reinterpretation is closed off, but the amendment track remains theoretically open. Resistance is substantial (0.60) because civil rights movements and legal scholars consistently challenge the supermajoritarian barrier. The measurement series share a single time grid to prevent misaligned substitution.
 *
 * PERSPECTIVAL GAP:
 *   The amending majority and enfranchised citizenry seats should compute as moderate coordination (low effective extraction, high legitimacy), while excluded minorities should compute as asymmetric extraction (high effective extraction) due to their trapped exit options and powerless status. The federal judiciary sits near symmetric: structurally constrained, neither collecting nor paying.
 *
 * DIRECTIONALITY LOGIC:
 *   Enfranchised citizenry and democratic amending majorities are structural beneficiaries (low d): they control the amendment machinery and derive legitimacy from it. Excluded minorities are structural targets (high d): they pay through delayed recognition and supermajoritarian exclusion. The federal judiciary is an analytical observer with near-symmetric directionality because it is bound by the text and amendment outcomes without independent creative authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was built to solve the founding problem of legitimate constitutional change without judicial tyranny. Over time, as the amendment process atrophied into near-impossibility for equality expansions, the constraint risked becoming a piton (performative maintenance of a dead amendment track). However, the progressive textualist reading resists this drift by insisting on the amendment mechanism as the sole legitimate path, even when costly. The metrics do not show a high theater ratio because the democratic commitment is still functionally operative, albeit with severe friction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    supermajority_barrier_nature,
    'Is the Article V supermajority requirement a legitimate democratic safeguard or an extraction mechanism that privileges existing majorities against excluded minorities?',
    'Comparative constitutional analysis of amendment rates and inclusion timelines; empirical study of whether supermajority rules correlate with delayed rights recognition for disempowered groups.',
    'If the barrier systematically privileges majority status quo, effective extraction is higher than structural metrics suggest and the coordination story is partially cover; if it merely ensures broad consensus, the extraction is the necessary cost of democratic legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supermajority_barrier_nature, empirical, 'Whether the supermajority requirement is democratic safeguard or majoritarian extraction.').

omega_variable(
    judicial_alternative_viability,
    'Could judicial reinterpretation actually deliver equality expansion, or is the progressive textualist correct that only amendments provide durable legitimacy?',
    'Historical case study of judicially-expanded rights (e.g., Brown, Obergefell) versus amendment-expanded rights (Reconstruction, Nineteenth Amendment): measure stability, backlash, and democratic acceptance.',
    'If judicial expansion proves durable and accepted, the progressive textualist reading''s closure of the judicial path is extractive overreach; if judicial expansion consistently produces instability, the reading is vindicated as genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_alternative_viability, empirical, 'Whether judicial reinterpretation is a viable alternative to amendment-driven expansion.').

omega_variable(
    textual_equality_existence,
    'Does the constitutional text genuinely encode a general equality principle, or is this an anachronistic projection onto a historically limited provision?',
    'Historical-linguistic analysis of the equality language at ratification, including contemporary dictionaries, ratification debates, and early statutory construction.',
    'If the text lacks a general principle, the progressive textualist reading rests on a constructed rather than textual foundation, shifting its classification toward identity_coordination or conventional legitimacy rather than textual constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textual_equality_existence, conceptual, 'Whether the text contains a general equality principle or a historically bounded one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__progressive_textualist, 0, 230).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equality_clause_scope__progressive_textualist, theater_ratio, 0, 0.2).
narrative_ontology:measurement(equa_tr_t40, equality_clause_scope__progressive_textualist, theater_ratio, 40, 0.22).
narrative_ontology:measurement(equa_tr_t80, equality_clause_scope__progressive_textualist, theater_ratio, 80, 0.28).
narrative_ontology:measurement(equa_tr_t120, equality_clause_scope__progressive_textualist, theater_ratio, 120, 0.35).
narrative_ontology:measurement(equa_tr_t160, equality_clause_scope__progressive_textualist, theater_ratio, 160, 0.32).
narrative_ontology:measurement(equa_tr_t200, equality_clause_scope__progressive_textualist, theater_ratio, 200, 0.31).
narrative_ontology:measurement(equa_tr_t230, equality_clause_scope__progressive_textualist, theater_ratio, 230, 0.3).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equality_clause_scope__progressive_textualist, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(equa_be_t40, equality_clause_scope__progressive_textualist, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(equa_be_t80, equality_clause_scope__progressive_textualist, base_extractiveness, 80, 0.65).
narrative_ontology:measurement(equa_be_t120, equality_clause_scope__progressive_textualist, base_extractiveness, 120, 0.45).
narrative_ontology:measurement(equa_be_t160, equality_clause_scope__progressive_textualist, base_extractiveness, 160, 0.55).
narrative_ontology:measurement(equa_be_t200, equality_clause_scope__progressive_textualist, base_extractiveness, 200, 0.6).
narrative_ontology:measurement(equa_be_t230, equality_clause_scope__progressive_textualist, base_extractiveness, 230, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equality_clause_scope__progressive_textualist, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(equa_su_t40, equality_clause_scope__progressive_textualist, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(equa_su_t80, equality_clause_scope__progressive_textualist, suppression_requirement, 80, 0.6).
narrative_ontology:measurement(equa_su_t120, equality_clause_scope__progressive_textualist, suppression_requirement, 120, 0.55).
narrative_ontology:measurement(equa_su_t160, equality_clause_scope__progressive_textualist, suppression_requirement, 160, 0.52).
narrative_ontology:measurement(equa_su_t200, equality_clause_scope__progressive_textualist, suppression_requirement, 200, 0.54).
narrative_ontology:measurement(equa_su_t230, equality_clause_scope__progressive_textualist, suppression_requirement, 230, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__progressive_textualist, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
