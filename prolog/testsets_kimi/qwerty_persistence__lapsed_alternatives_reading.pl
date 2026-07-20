% ============================================================================
% CONSTRAINT STORY: qwerty_persistence__lapsed_alternatives_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: QWERTY Persistence via Lapsed Alternatives (Coordination Reading)
 *   domain: technology history / industrial standards / path dependence
 *
 * SUMMARY:
 *   This constraint story instantiates the lapsed_alternatives reading of the
 *   qwerty_persistence kernel. Under this reading, the QWERTY keyboard layout
 *   persists not because identifiable incumbents actively defend it, but
 *   because the standard solves a genuine coordination problem: any trained
 *   user can operate any compatible machine. Alternative layouts (Dvorak,
 *   Colemak) lapsed because they failed to reach the critical mass required
 *   to overcome network effects. The constraint is symmetric â all parties
 *   bear coordination costs (switching inertia) and benefits
 *   (interoperability) equally. There is no concentrated beneficiary
 *   extracting rents from the standard's persistence.
 *
 * KEY AGENTS:
 *   - mass_typists_and_users: Symmetrically coordinated agents who bear no extraction but face switching costs
 *   - hardware_manufacturers: Commodity producers assembling to the dominant standard without capturing rents from its persistence
 *   - alternative_layout_advocates: Agents promoting ergonomic alternatives who remain structurally excluded from manufacturing scale due to coordination failure, not active suppression
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence__lapsed_alternatives_reading, 0.25).
domain_priors:suppression_score(qwerty_persistence__lapsed_alternatives_reading, 0.1).
domain_priors:theater_ratio(qwerty_persistence__lapsed_alternatives_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__lapsed_alternatives_reading, rope).
narrative_ontology:human_readable(qwerty_persistence__lapsed_alternatives_reading, "QWERTY Persistence via Lapsed Alternatives (Coordination Reading)").
narrative_ontology:topic_domain(qwerty_persistence__lapsed_alternatives_reading, "technology history / industrial standards / path dependence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__lapsed_alternatives_reading, '4b4cf2b3-3c3a-4300-ae15-c693092f7f4c').
narrative_ontology:cs_kernel_codification('4b4cf2b3-3c3a-4300-ae15-c693092f7f4c', distributed).
narrative_ontology:cs_authority_grounding('4b4cf2b3-3c3a-4300-ae15-c693092f7f4c', distributed).
narrative_ontology:cs_reading_relation('4b4cf2b3-3c3a-4300-ae15-c693092f7f4c', qwerty_persistence__incumbent_preservation_reading, coexists_with).
narrative_ontology:cs_axiom('4b4cf2b3-3c3a-4300-ae15-c693092f7f4c', foundational, network_effects_sufficient_for_lock_in).
narrative_ontology:cs_axiom_status(network_effects_sufficient_for_lock_in, holdable).
narrative_ontology:cs_axiom_grounding('4b4cf2b3-3c3a-4300-ae15-c693092f7f4c', network_effects_sufficient_for_lock_in, empirically_contingent).
narrative_ontology:cs_axiom('4b4cf2b3-3c3a-4300-ae15-c693092f7f4c', foundational, incumbent_agency_unnecessary_for_persistence).
narrative_ontology:cs_axiom_status(incumbent_agency_unnecessary_for_persistence, holdable).
narrative_ontology:cs_axiom_grounding('4b4cf2b3-3c3a-4300-ae15-c693092f7f4c', incumbent_agency_unnecessary_for_persistence, empirically_contingent).
narrative_ontology:cs_reference_frame('4b4cf2b3-3c3a-4300-ae15-c693092f7f4c', de_facto_coordination_equilibrium).
narrative_ontology:cs_drift_state('4b4cf2b3-3c3a-4300-ae15-c693092f7f4c', contemporary_digital_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('4b4cf2b3-3c3a-4300-ae15-c693092f7f4c', '').
narrative_ontology:cs_kernel_id(qwerty_persistence__lapsed_alternatives_reading, qwerty_persistence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared interface protocol (keyboard layout) allowing any trained user to operate any compatible machine without relearning, solving the coordination problem of heterogeneous equipment and transferable operator skill.
% TRANSFER_FUNCTION: No asymmetric transfer; all parties bear symmetric coordination costs in the form of foregone alternative layouts and the inertia of maintaining compatibility with the dominant standard.
% ABSENT_VOICES: Advocates of alternative keyboard layouts (Dvorak, Colemak, Workman) and ergonomic researchers who argue for superior arrangements are present in niche communities but excluded from mainstream manufacturing scale by coordination failure rather than by deliberate suppression.
% DISAPPEARANCE_RATIONALE: If the QWERTY layout vanished overnight, global typing infrastructure, software input methods, manufacturing tooling, and embodied human capital would require massive re-coordination toward a replacement standard. The rearrangement would be costly but symmetric.
% FOUNDING_PROBLEM: The need for a standardized mechanical typing interface to allow interchangeable operator skill across typewriting equipment in the late 19th century.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and ergonomics researchers outside any manufacturing beneficiary class attest that the original mechanical coordination problem was genuine; computer scientists note that the live problem is general input standardization rather than this specific layout. No concentrated beneficiary group self-asserts the founding myth.
narrative_ontology:disappearance_verdict(qwerty_persistence__lapsed_alternatives_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence__lapsed_alternatives_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence__lapsed_alternatives_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence__lapsed_alternatives_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence__lapsed_alternatives_reading, 0.25, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is low (0.25) because the constraint extracts only via switching costs â the opportunity cost of maintaining compatibility with the dominant standard. Suppression is minimal (0.10) because persistence requires no active enforcement; alternatives die from coordination failure, not coercion. Theater ratio is low (0.10) because there is little performative maintenance of the standard. Accessibility collapse is moderate-high (0.65): once the standard is understood, alternatives collapse due to network effects, but this is a natural feature of coordination goods rather than constructed barriers. Resistance is low (0.15) because most users and producers accept the arrangement as mutually beneficial. The measurement series shows a slow rise in base_extractiveness as network effects hardened over the 20th century, but the constraint remains within rope parameters.
 *
 * PERSPECTIVAL GAP:
 *   Because the constraint is structurally symmetric â no beneficiaries or victims are declared â the engine should compute similar directionality and classification across all power and exit levels. The primary divergence is between agents with high switching costs (biographical identity locked in muscle memory) and those with low switching costs (new entrants), but both are coordination costs rather than extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality asymmetry is declared: the base_properties.beneficiaries and victims arrays are intentionally empty. All agents are structurally symmetric with respect to this constraint. The engine will derive directionality near 0.5 for all seats, reflecting the coordination-cost symmetry. No overrides are needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as rope prevents mislabeling symmetric coordination costs as extraction. The founding problem â standardized typing interface â is contested regarding this specific layout but live in general form. Because there is no active enforcement, no concentrated beneficiary, and no suppressed alternative, the constraint does not satisfy the gates for tangled_rope or snare. If the constraint were to develop active incumbent defense (e.g., manufacturers suing alternative-layout competitors), it would drift toward tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    qwerty_kernel_reading_position,
    'Does the persistence of the QWERTY standard reflect pure coordination lock-in, or does it mask incumbent preservation dynamics?',
    'Historical analysis of typewriter manufacturer patent litigation, keyboard supplier contracting practices, and OEM bundling decisions versus pure user-side adoption data.',
    'If incumbent preservation is dominant, the constraint reclassifies as tangled_rope or snare with identifiable beneficiaries; if pure coordination, it remains rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qwerty_kernel_reading_position, conceptual, 'Whether QWERTY persistence is a contested kernel with two structurally distinct readings.').

omega_variable(
    switching_cost_symmetry,
    'Are the switching costs of QWERTY abandonment borne symmetrically across all users and producers, or do they fall asymmetrically on specific classes?',
    'Empirical measurement of retraining costs, hardware retooling expenses, and software compatibility losses across user demographics and manufacturer tiers.',
    'Asymmetric cost bearing would introduce a victim set and shift classification toward tangled_rope; symmetric costs support rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_symmetry, empirical, 'Symmetry of switching cost distribution across agents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence__lapsed_alternatives_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(qwer_tr_t30, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement(qwer_tr_t60, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 60, 0.06).
narrative_ontology:measurement(qwer_tr_t90, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 90, 0.07).
narrative_ontology:measurement(qwer_tr_t120, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 120, 0.08).
narrative_ontology:measurement(qwer_tr_t150, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 150, 0.1).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(qwer_be_t30, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 30, 0.1).
narrative_ontology:measurement(qwer_be_t60, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 60, 0.14).
narrative_ontology:measurement(qwer_be_t90, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 90, 0.18).
narrative_ontology:measurement(qwer_be_t120, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 120, 0.22).
narrative_ontology:measurement(qwer_be_t150, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 150, 0.25).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(qwerty_persistence__lapsed_alternatives_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence__lapsed_alternatives_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence__lapsed_alternatives_reading, incumbent_preservation_reading).

% DUAL FORMULATION NOTE:
% The qwerty_persistence kernel decomposes into two structurally distinct constraints. This reading (lapsed_alternatives) treats persistence as symmetric coordination with no beneficiaries and low extraction; the sibling (incumbent_preservation) treats persistence as asymmetric extraction with active beneficiaries and enforcement. They share the same empirical phenomenon (QWERTY dominance) but instantiate different constraints due to different epsilon values and causal mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
