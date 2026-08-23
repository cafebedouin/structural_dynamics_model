% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__naturalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__naturalization_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: qwerty_persistence_mechanism__naturalization_reading
 *   human_readable: QWERTY Keyboard Layout Persistence (Naturalization Reading)
 *   domain: economic_history/technology_studies/path_dependence
 *
 * SUMMARY:
 *   The naturalization reading of QWERTY persistence argues that the layout's
 *   dominance is the result of genuine adequacy and fair market competition,
 *   not pathological lock-in or incumbent extraction. QWERTY won typing
 *   contests, was adopted by the largest typing schools, and became the
 *   standard because it worked well enough that no alternative could displace
 *   it despite numerous attempts. Switching costs exist but reflect real
 *   human capital investment; the Dvorak advantage is empirically contested
 *   and, even if real, is too small to overcome coordination benefits. The
 *   constraint is a Rope: a coordination standard that solves the
 *   interoperability problem with minimal coercion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__naturalization_reading, 0.12).
domain_priors:suppression_score(qwerty_persistence_mechanism__naturalization_reading, 0.08).
domain_priors:theater_ratio(qwerty_persistence_mechanism__naturalization_reading, 0.07).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 0.07).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__naturalization_reading, rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__naturalization_reading, "QWERTY Keyboard Layout Persistence (Naturalization Reading)").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__naturalization_reading, "economic_history/technology_studies/path_dependence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__naturalization_reading, '88c42482-60fa-481b-94cd-dbe55b009adf').
narrative_ontology:cs_kernel_codification('88c42482-60fa-481b-94cd-dbe55b009adf', distributed).
narrative_ontology:cs_authority_grounding('88c42482-60fa-481b-94cd-dbe55b009adf', distributed).
narrative_ontology:cs_reading_relation('88c42482-60fa-481b-94cd-dbe55b009adf', qwerty_persistence_mechanism__lock_in_reading, coexists_with).
narrative_ontology:cs_reading_relation('88c42482-60fa-481b-94cd-dbe55b009adf', qwerty_persistence_mechanism__beneficiary_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('88c42482-60fa-481b-94cd-dbe55b009adf', foundational, qwerty_is_efficient).
narrative_ontology:cs_axiom_status(qwerty_is_efficient, holdable).
narrative_ontology:cs_axiom_grounding('88c42482-60fa-481b-94cd-dbe55b009adf', qwerty_is_efficient, empirically_contingent).
narrative_ontology:cs_axiom('88c42482-60fa-481b-94cd-dbe55b009adf', foundational, market_competition_selects_efficient_standards).
narrative_ontology:cs_axiom_status(market_competition_selects_efficient_standards, holdable).
narrative_ontology:cs_axiom_grounding('88c42482-60fa-481b-94cd-dbe55b009adf', market_competition_selects_efficient_standards, empirically_contingent).
narrative_ontology:cs_reference_frame('88c42482-60fa-481b-94cd-dbe55b009adf', competitive_standard_selection).
narrative_ontology:cs_drift_state('88c42482-60fa-481b-94cd-dbe55b009adf', lock_in_narrative_dominance, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('88c42482-60fa-481b-94cd-dbe55b009adf', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__naturalization_reading, keyboard_users).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__naturalization_reading, keyboard_manufacturers).
narrative_ontology:constraint_vindicates(qwerty_persistence_mechanism__naturalization_reading, qwerty_efficiency_hypothesis).
narrative_ontology:constraint_vindicates(qwerty_persistence_mechanism__naturalization_reading, market_competition_selects_efficient_standards).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Typists and computer users who have invested skill in QWERTY. They benefit from a universal layout that enables skill transfer across devices and employers. Switching costs are real but reflect genuine human capital investment, not artificial barriers. No viable alternative layout has demonstrated sufficient advantage to justify retraining.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, keyboard_users, beneficiary,
    moderate, biographical, constrained, global).

% Manufacturers produce keyboards to the QWERTY standard because it is what users expect and demand. They benefit from economies of scale and avoid retooling costs. They could produce alternative layouts if demand existed, but market signals show no sustained demand. Their role in setting the agenda is passive: they follow the de facto standard.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, keyboard_manufacturers, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_mechanism__naturalization_reading, keyboard_manufacturers, agenda_setter).

% Advocates for layouts like Dvorak or Colemak who argue for technical superiority (speed, ergonomics). They are excluded from mainstream adoption not by coercion but by the coordination value of the incumbent standard. Their investments in alternative layouts have not been rewarded by the market.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, alternative_layout_proponents, excluded,
    powerless, biographical, trapped, global).

% Scholars who study the QWERTY case as a test of path dependence versus market efficiency. They evaluate empirical evidence on typing speeds, switching costs, and historical adoption dynamics. Their analysis informs the contested classification of this constraint.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, economic_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal keyboard layout standard that enables interoperability across devices, transferability of typing skills, and economies of scale in manufacturing and training.
% TRANSFER_FUNCTION: Concentrates the cost of learning a keyboard layout on individuals (who invest in QWERTY skill) while distributing the benefit of a common standard across all users, manufacturers, and employers. No monetary transfer occurs; the transfer is in human capital investment.
% ABSENT_VOICES: Ergonomics researchers and occupational health specialists who argue that QWERTY's layout contributes to repetitive strain injuries and that a more ergonomic standard would reduce long-term health costs. Their voices are marginalized because the health externality is diffuse and not priced in the market.
% DISAPPEARANCE_RATIONALE: If the QWERTY standard vanished overnight, massive disruption would occur: billions of users would lose their typing proficiency, manufacturers would face chaotic demand for multiple layouts, training systems would collapse, and digital interfaces would need redesign. The world would rearrange around a new standard or fragmentation.
% FOUNDING_PROBLEM: The need for a standardized keyboard arrangement on mechanical typewriters that balanced typing speed with mechanical reliability (preventing typebar jams) and enabled the emergence of touch typing as a transferable skill.
% FOUNDING_PROBLEM_CORROBORATION: Historical records from Remington and early typewriter manufacturers confirm the anti-jamming rationale for QWERTY's design. However, economic historians (Liebowitz & Margolis, 1990) and subsequent researchers argue that the anti-jamming problem was quickly solved and that QWERTY's dominance was cemented by fair competition in typing contests and market adoption, not by the original mechanical constraint. The corroboration comes from outside the beneficiary set: independent historical analysis of typing contest records and market share data.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__naturalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__naturalization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__naturalization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence_mechanism__naturalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__naturalization_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__naturalization_reading_tests).
:- end_tests(qwerty_persistence_mechanism__naturalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because no party extracts rents from the standard; manufacturers and users both benefit from the coordination. Suppression is negligible (0.08) because alternatives are not actively suppressed — they are free to compete and have competed (Dvorak, Colemak, etc.) but failed to gain traction. Theater ratio is near zero (0.07) because the standard's maintenance requires no performative enforcement; it persists because it works. Accessibility collapse is moderate (0.35) because alternatives exist and can be learned, but the coordination value of QWERTY makes switching individually irrational. Resistance is low (0.15) because there is no organized opposition to the standard itself, only niche advocacy for alternatives.
 *
 * PERSPECTIVAL GAP:
 *   From the keyboard_user seat, the constraint is a beneficial coordination standard (Rope). From the alternative_layout_proponent seat, it appears as a barrier to innovation (potentially Snare), but the naturalization reading denies this appearance reflects structural extraction. The engine will compute per-seat types from the structural data: users have constrained exit but low directionality (beneficiaries), proponents have trapped exit but no extraction mechanism targeting them. The divergence between seats is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Keyboard users and manufacturers are beneficiaries (d near 0.0) because they gain coordination value without paying extractive rents. Alternative layout proponents are excluded (d not applicable) — they bear costs of advocating for alternatives but are not targeted by the constraint. Economic historians are observers (d=0.5). No party has high directionality toward this constraint because it does not extract.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate (standardization for interoperability) remains live. The constraint has not outlived its function; it continues to solve the coordination problem it was adopted for. Mandatrophy is not resolved because the function persists. The classification as Rope (not Piton) reflects this: the theater ratio is low and the coordination function is active.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the persistence of QWERTY best explained by genuine adequacy (naturalization), path-dependent lock-in (lock_in_reading), or incumbent rent-seeking (beneficiary_extraction_reading)?',
    'Comparative historical analysis of typing contest data, switching cost measurements, and manufacturer behavior; resolution of the empirical debate on Dvorak superiority.',
    'If naturalization is correct, the constraint is a Rope with low extraction. If lock-in is correct, it is a Tangled Rope or Piton with moderate extraction from coordination failure. If beneficiary_extraction is correct, it is a Snare with high extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, empirical, 'The fundamental empirical disagreement among the three readings of the QWERTY persistence kernel.').

omega_variable(
    dvorak_advantage_contestation,
    'Does the Dvorak layout offer a genuine, significant advantage over QWERTY for typical users, and if so, is that advantage sufficient to justify switching costs?',
    'Controlled longitudinal studies of typist retraining with modern measurement of speed, error rate, and ergonomic outcomes; meta-analysis of existing studies.',
    'A confirmed large Dvorak advantage would support the lock-in reading (coordination failure). A negligible or contested advantage supports naturalization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dvorak_advantage_contestation, empirical, 'Empirical contestation of the key claim that alternatives are inferior.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__naturalization_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwerty_nat_tr_t0, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(qwerty_nat_tr_t50, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 50, 0.06).
narrative_ontology:measurement(qwerty_nat_tr_t100, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 100, 0.07).
narrative_ontology:measurement(qwerty_nat_tr_t150, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 150, 0.07).

% Extraction over time
narrative_ontology:measurement(qwerty_nat_be_t0, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(qwerty_nat_be_t50, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(qwerty_nat_be_t100, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 100, 0.12).
narrative_ontology:measurement(qwerty_nat_be_t150, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 150, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(qwerty_nat_su_t0, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(qwerty_nat_su_t50, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 50, 0.07).
narrative_ontology:measurement(qwerty_nat_su_t100, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 100, 0.08).
narrative_ontology:measurement(qwerty_nat_su_t150, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 150, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__naturalization_reading, information_standard).
narrative_ontology:boltzmann_floor_override(qwerty_persistence_mechanism__naturalization_reading, 0.02).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism__lock_in_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism__beneficiary_extraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the qwerty_persistence_mechanism kernel. The naturalization reading claims QWERTY is a Rope (efficient coordination standard). The lock_in_reading claims it is a Tangled Rope/Piton (coordination failure). The beneficiary_extraction_reading claims it is a Snare (incumbent rent extraction). They differ in ε (extractiveness), beneficiary/victim structure, and suppression. Linked via affects_constraints to enable family-level analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
