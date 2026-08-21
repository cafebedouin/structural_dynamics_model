% ============================================================================
% CONSTRAINT STORY: qwerty_persistence__lapsed_alternatives_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence__lapsed_alternatives_reading, []).

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
 *   constraint_id: qwerty_persistence__lapsed_alternatives_reading
 *   human_readable: QWERTY Keyboard Layout Persistence (Lapsed Alternatives Reading)
 *   domain: technology_history/industrial_standards
 *
 * SUMMARY:
 *   This constraint describes the persistence of the QWERTY keyboard layout
 *   from the perspective that its dominance is maintained by coordination
 *   value and the high switching costs that prevent alternatives from
 *   reaching critical mass. It is a reading of the 'qwerty_persistence'
 *   kernel, focusing on the self-reinforcing nature of standards once
 *   adopted, rather than active incumbent defense. The constraint is
 *   classified as a Rope because it provides a genuine coordination function
 *   (universal compatibility) with minimal active enforcement, but it imposes
 *   a symmetric, low-level 'extraction' in the form of suboptimal efficiency
 *   and foregone benefits of superior alternatives, which all participants
 *   bear equally.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence__lapsed_alternatives_reading, 0.15).
domain_priors:suppression_score(qwerty_persistence__lapsed_alternatives_reading, 0.05).
domain_priors:theater_ratio(qwerty_persistence__lapsed_alternatives_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__lapsed_alternatives_reading, rope).
narrative_ontology:human_readable(qwerty_persistence__lapsed_alternatives_reading, "QWERTY Keyboard Layout Persistence (Lapsed Alternatives Reading)").
narrative_ontology:topic_domain(qwerty_persistence__lapsed_alternatives_reading, "technology_history/industrial_standards").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__lapsed_alternatives_reading, '3c93909f-d398-45d5-9f08-841c167512da').
narrative_ontology:cs_kernel_codification('3c93909f-d398-45d5-9f08-841c167512da', implicit).
narrative_ontology:cs_authority_grounding('3c93909f-d398-45d5-9f08-841c167512da', practice).
narrative_ontology:cs_reading_relation('3c93909f-d398-45d5-9f08-841c167512da', qwerty_persistence__incumbent_preservation_reading, coexists_with).
narrative_ontology:cs_axiom('3c93909f-d398-45d5-9f08-841c167512da', foundational, coordination_value_outweighs_suboptimality).
narrative_ontology:cs_axiom_status(coordination_value_outweighs_suboptimality, holdable).
narrative_ontology:cs_axiom_grounding('3c93909f-d398-45d5-9f08-841c167512da', coordination_value_outweighs_suboptimality, conventional).
narrative_ontology:cs_axiom('3c93909f-d398-45d5-9f08-841c167512da', foundational, alternatives_fail_to_reach_critical_mass).
narrative_ontology:cs_axiom_status(alternatives_fail_to_reach_critical_mass, holdable).
narrative_ontology:cs_axiom_grounding('3c93909f-d398-45d5-9f08-841c167512da', alternatives_fail_to_reach_critical_mass, empirically_contingent).
narrative_ontology:cs_reference_frame('3c93909f-d398-45d5-9f08-841c167512da', self_reinforcing_network_effect).
narrative_ontology:cs_drift_state('3c93909f-d398-45d5-9f08-841c167512da', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('3c93909f-d398-45d5-9f08-841c167512da', '').
narrative_ontology:cs_kernel_id(qwerty_persistence__lapsed_alternatives_reading, qwerty_persistence).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(qwerty_persistence__lapsed_alternatives_reading, computer_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce keyboards using the QWERTY layout due to established tooling and market demand. While they could technically switch to alternative layouts, the cost of retooling and convincing consumers to adopt a new standard is prohibitive without a coordinated industry-wide shift.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, keyboard_manufacturers, agenda_setter,
    organized, generational, constrained, global).

% Learn and use the QWERTY layout as the default. Switching to an alternative layout would require relearning muscle memory and potentially using non-standard hardware, incurring personal switching costs. They bear the cost of suboptimal typing efficiency but benefit from universal compatibility.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, computer_users, payer,
    moderate, biographical, constrained, global).

% Promote more efficient keyboard layouts (e.g., Dvorak, Colemak) but face immense inertia from the installed base and manufacturing ecosystem. Their efforts are largely academic or niche, unable to overcome the coordination barrier for mass adoption.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, alternative_layout_advocates, excluded,
    powerless, generational, trapped, global).

% Study the ergonomic and speed implications of various keyboard layouts, consistently finding QWERTY to be suboptimal. Their findings highlight the technical superiority of alternatives but have little impact on market adoption due to coordination failure.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, typing_efficiency_researchers, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures universal compatibility and ease of learning for keyboard users and manufacturers by standardizing on a single layout, even if suboptimal.
% TRANSFER_FUNCTION: No direct financial transfer. The 'cost' is borne symmetrically by all participants as suboptimal efficiency and foregone benefits of superior alternatives, in exchange for universal interoperability.
% ABSENT_VOICES: Advocates for technically superior alternative layouts are effectively excluded from mainstream adoption due to the network effects and switching costs embedded in the QWERTY standard. They would argue for a coordinated transition to a better layout.
% DISAPPEARANCE_RATIONALE: If the QWERTY standard vanished overnight, there would be immense chaos as no single alternative would immediately take its place. Users would struggle to type, manufacturers would face retooling dilemmas, and a new, likely fragmented, set of standards would emerge over time.
% FOUNDING_PROBLEM: Early typewriters needed a layout that prevented key jamming and allowed for rapid, two-handed typing, given the mechanical constraints of the era.
% FOUNDING_PROBLEM_CORROBORATION: Historical accounts and engineering analyses confirm the original mechanical problem. Typing efficiency researchers and alternative layout advocates corroborate that the original mechanical problem is long dead, and QWERTY persists due to network effects and coordination failure, not its inherent superiority for modern digital typing.
narrative_ontology:disappearance_verdict(qwerty_persistence__lapsed_alternatives_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence__lapsed_alternatives_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence__lapsed_alternatives_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(qwerty_persistence__lapsed_alternatives_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence__lapsed_alternatives_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence__lapsed_alternatives_reading_tests).
:- end_tests(qwerty_persistence__lapsed_alternatives_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the 'cost' is primarily the opportunity cost of not using a more efficient layout, distributed across all users and manufacturers. There's no concentrated beneficiary collecting this 'extraction.' Suppression is very low (0.05) as there's no active coercion to use QWERTY; rather, it's the absence of viable alternatives due to coordination failure that maintains its dominance. Theater ratio is negligible (0.01) as there's no performative maintenance; the standard simply persists due to inertia and network effects. Accessibility collapse is high (0.85) because, for most users, practical alternatives are effectively non-existent due to the lack of widespread support and high individual switching costs. Resistance is low (0.02) because while some advocate for alternatives, their efforts are marginal against the entrenched standard.
 *
 * PERSPECTIVAL GAP:
 *   This reading emphasizes the coordination problem and symmetric costs. A sibling reading (incumbent_preservation_reading) would likely classify this as a Tangled Rope or Snare, arguing that manufacturers actively defend QWERTY to protect their sunk investments, thereby extracting rents from users. The engine's classification will highlight this divergence based on the different structural declarations in each reading.
 *
 * DIRECTIONALITY LOGIC:
 *   All participants (manufacturers, users) are symmetrically positioned. They all benefit from the coordination (universal compatibility) and all bear the symmetric cost of suboptimal efficiency. There is no single beneficiary or victim group in this reading; the 'extraction' is diffuse and inherent to the coordination problem itself. Alternative layout advocates are 'excluded' not by active suppression, but by the structural barriers to coordinating a mass switch.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_incumbent_defense,
    'Is QWERTY''s persistence primarily due to coordination failure and network effects (this reading), or active defense by incumbents protecting sunk capital (incumbent_preservation_reading)?',
    'Economic analysis of manufacturer investment in QWERTY-specific tooling vs. general keyboard manufacturing, and the lobbying efforts (if any) against alternative layouts. If significant active defense is found, the incumbent_preservation_reading gains empirical support.',
    'If incumbent defense is primary, the constraint would reclassify towards Tangled Rope or Snare, with higher extractiveness and identifiable beneficiaries (manufacturers). If coordination failure is primary, the Rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_vs_incumbent_defense, empirical, 'Distinguishes between passive coordination and active incumbent defense as the primary driver of QWERTY''s persistence.').

omega_variable(
    symmetric_vs_asymmetric_cost_distribution,
    'Are the costs of QWERTY''s suboptimality truly symmetric across all participants, or do some groups (e.g., professional typists) bear a disproportionately higher cost?',
    'Detailed ergonomic and economic studies on different user groups'' efficiency losses and switching costs. If significant asymmetry is found, the ''victim'' set might need to be refined.',
    'If costs are found to be significantly asymmetric, the constraint might lean towards a Tangled Rope, even without a clear beneficiary, as some are coordinated at a higher cost than others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symmetric_vs_asymmetric_cost_distribution, empirical, 'Examines the distribution of costs from QWERTY''s suboptimality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence__lapsed_alternatives_reading, 1873, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1873, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 1873, 0.0).
narrative_ontology:measurement(qwer_tr_t1900, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 1900, 0.0).
narrative_ontology:measurement(qwer_tr_t1950, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 1950, 0.0).
narrative_ontology:measurement(qwer_tr_t2000, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 2000, 0.01).
narrative_ontology:measurement(qwer_tr_t2024, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 2024, 0.01).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1873, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1873, 0.05).
narrative_ontology:measurement(qwer_be_t1900, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1900, 0.08).
narrative_ontology:measurement(qwer_be_t1950, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1950, 0.12).
narrative_ontology:measurement(qwer_be_t2000, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 2000, 0.14).
narrative_ontology:measurement(qwer_be_t2024, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1873, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 1873, 0.01).
narrative_ontology:measurement(qwer_su_t1900, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 1900, 0.02).
narrative_ontology:measurement(qwer_su_t1950, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 1950, 0.03).
narrative_ontology:measurement(qwer_su_t2000, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 2000, 0.04).
narrative_ontology:measurement(qwer_su_t2024, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 2024, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence__lapsed_alternatives_reading, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
