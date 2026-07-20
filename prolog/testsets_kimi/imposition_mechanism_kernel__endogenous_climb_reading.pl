% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__endogenous_climb_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: imposition_mechanism_kernel__endogenous_climb_reading
 *   human_readable: Endogenous Climb Mechanism of Norm Legitimation
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint instantiates the endogenous_climb_reading of the
 *   imposition_mechanism_kernel. The kernel asks how new norms achieve
 *   legitimate authority in state-formation contexts. This reading holds that
 *   legitimacy emerges from bottom-up popular adoption, with the state acting
 *   as a late coordinator that mandates only after cultural acceptance is
 *   already established. Sibling readings include the
 *   exogenous_override_reading (state imposes by monopoly on violence) and
 *   the hybrid_legitimation_reading (symbolic authority transfer combined
 *   with institutional incentives). The authored metrics describe a
 *   low-extraction, low-suppression coordination mechanism; the claimed type
 *   is rope.
 *
 * KEY AGENTS:
 *   - norm_adopting_communities: Primary beneficiary (organized/mobile) â decentralized source of normative convergence
 *   - state_coordinator: Agenda-setter (institutional/constrained) â codifies and ratifies adopted norms
 *   - norm_entrepreneurs: Secondary beneficiary (moderate/mobile) â initiates normative innovation
 *   - marginal_dissenters: Excluded voice (powerless/constrained) â non-adopters rendered invisible by consensus narrative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__endogenous_climb_reading, 0.18).
domain_priors:suppression_score(imposition_mechanism_kernel__endogenous_climb_reading, 0.12).
domain_priors:theater_ratio(imposition_mechanism_kernel__endogenous_climb_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__endogenous_climb_reading, rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__endogenous_climb_reading, "Endogenous Climb Mechanism of Norm Legitimation").
narrative_ontology:topic_domain(imposition_mechanism_kernel__endogenous_climb_reading, "historical_sociology/state_formation/cultural_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__endogenous_climb_reading, 'b5d7e840-875f-48b1-852c-ee2f545f8597').
narrative_ontology:cs_kernel_codification('b5d7e840-875f-48b1-852c-ee2f545f8597', distributed).
narrative_ontology:cs_authority_grounding('b5d7e840-875f-48b1-852c-ee2f545f8597', practice).
narrative_ontology:cs_reading_relation('b5d7e840-875f-48b1-852c-ee2f545f8597', imposition_mechanism_kernel__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('b5d7e840-875f-48b1-852c-ee2f545f8597', imposition_mechanism_kernel__hybrid_legitimation_reading, influences).
narrative_ontology:cs_axiom('b5d7e840-875f-48b1-852c-ee2f545f8597', foundational, legitimacy_precedes_codification).
narrative_ontology:cs_axiom_status(legitimacy_precedes_codification, holdable).
narrative_ontology:cs_axiom_grounding('b5d7e840-875f-48b1-852c-ee2f545f8597', legitimacy_precedes_codification, empirically_contingent).
narrative_ontology:cs_axiom('b5d7e840-875f-48b1-852c-ee2f545f8597', foundational, state_as_late_recognizer).
narrative_ontology:cs_axiom_status(state_as_late_recognizer, holdable).
narrative_ontology:cs_axiom_grounding('b5d7e840-875f-48b1-852c-ee2f545f8597', state_as_late_recognizer, empirically_contingent).
narrative_ontology:cs_reference_frame('b5d7e840-875f-48b1-852c-ee2f545f8597', organic_normative_consensus).
narrative_ontology:cs_drift_state('b5d7e840-875f-48b1-852c-ee2f545f8597', modern_state_formation_debate, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b5d7e840-875f-48b1-852c-ee2f545f8597', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__endogenous_climb_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__endogenous_climb_reading, norm_adopting_communities).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__endogenous_climb_reading, norm_entrepreneurs).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel__endogenous_climb_reading, bottom_up_legitimation_thesis).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel__endogenous_climb_reading, cultural_authority_precedes_political_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adopt and reproduce social norms through everyday decentralized interaction; gain state recognition and legal codification of practices they already follow without being compelled by external enforcement; their convergence is driven by mutual expectation and repeated coordination rather than by threat of punishment.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, norm_adopting_communities, beneficiary,
    organized, generational, mobile, national).

% Issues mandates and formalizes law only after observing widespread popular adoption of a norm; gains legitimacy cheaply by aligning state form with existing cultural practice; cannot effectively impose norms that have not already achieved organic uptake, so its policy toolkit is constrained to ratification rather than initiation.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, state_coordinator, agenda_setter,
    institutional, generational, constrained, national).

% Introduce and propagate novel behavioral standards within local networks; benefit when their innovations achieve sufficient mass to trigger state codification; operate without institutional backing and rely entirely on persuasion and demonstration for uptake.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, norm_entrepreneurs, beneficiary,
    moderate, biographical, mobile, regional).

% Do not adopt the emerging normative consensus; remain invisible within the legitimating narrative of uniform popular acceptance; when state codification eventually ratifies the dominant norm, their non-conformity becomes legally or socially salient, yet they were never part of the adoption conversation.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, marginal_dissenters, excluded,
    powerless, biographical, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_mechanism_kernel__endogenous_climb_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective normative expectations across a population so that convergent behavior emerges without centralized direction; the state later ratifies what has already stabilized in practice, reducing the need for coercive enforcement.
% TRANSFER_FUNCTION: Transfers the burden of norm enforcement from state coercion to distributed social expectations; transfers legitimacy from popular practice to the state mandate that follows it.
% ABSENT_VOICES: Marginal communities and dissenting subgroups whose practices do not converge with the emerging norm are absent from the legitimating consensus; they would object to the claim of unanimous popular acceptance but are invisible by construction in the endogenous climb narrative.
% DISAPPEARANCE_RATIONALE: If the endogenous climb mechanism vanished, states would attempt to impose norms ahead of adoption and face higher resistance; norm convergence would slow or fragment; the historical pattern of low-cost state legitimacy would reorganize around explicit coercion or hybrid mechanisms, and the autonomy of adopting communities would be curtailed.
% FOUNDING_PROBLEM: How to achieve widespread norm compliance and state legitimacy without the cost and instability of top-down coercion in contexts where centralized enforcement capacity is limited or expensive.
% FOUNDING_PROBLEM_CORROBORATION: Historical anthropologists attest to cases of bottom-up norm convergence preceding codification, yet institutional historians and military historians provide evidence that state capacity and elite signaling were often stronger than the pure climb model assumes; no source entirely outside the benefiting parties unambiguously corroborates the pure endogenous framing over the hybrid alternative.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__endogenous_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__endogenous_climb_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imposition_mechanism_kernel__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__endogenous_climb_reading, 0.18, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__endogenous_climb_reading_tests).
:- end_tests(imposition_mechanism_kernel__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the mechanism transfers enforcement burden to distributed social expectations rather than extracting concentrated rents; suppression is very low (0.12) because persistence does not depend on coercive overhead; theater_ratio is minimal (0.08) because state ratification is functional recognition rather than performative maintenance. Accessibility collapse is moderate (0.40): once a norm achieves organic dominance, alternatives become socially inaccessible, but this is through coordination advantage rather than active barrier construction. Resistance is minimal (0.08), consistent with the historical record of rapid, low-friction adoption.
 *
 * PERSPECTIVAL GAP:
 *   The state seat experiences the constraint as a limitation on its policy autonomy â it cannot legislate ahead of culture â but also as a source of cheap legitimacy. The community seat experiences the same constraint as autonomous self-organization that the state merely recognizes. The engine computes this divergence from structural data: the agenda_setter is constrained (high exit barrier to override) while the beneficiaries are mobile (low exit barrier from the norm itself). The excluded seat experiences invisibility rather than targeted extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Norm-adopting communities and norm entrepreneurs are structural beneficiaries (low d) because the constraint subsidizes their practices with state recognition and requires no tribute. The state coordinator sits near symmetric but slightly toward beneficiary (moderate-low d) because it gains legitimacy without paying enforcement costs, though it is constrained in its legislative timing. Marginal dissenters are not declared victims because the mechanism does not actively target them; their costs are structural exclusion from a consensus they did not join, captured by the excluded role and moderate accessibility collapse.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by requiring victim absence and low suppression for rope certification. If enforcement costs were high, if the state initiated rather than followed, or if identifiable agents bore concentrated costs from the mandate, the mechanism would compute as tangled_rope or snare. Here the historical signature is rapid adoption preceding codification, minimal coercive infrastructure, and diffuse rather than concentrated costs â the absence of a theater ratio above 0.5 and the absence of victims keeps the classification from drifting toward piton or snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    selection_vs_genuine_climb,
    'Does observed bottom-up norm adoption reflect genuine endogenous cultural selection, or is it manufactured by elite signaling and invisible institutional nudging that precedes apparent mass adoption?',
    'Historical network analysis of norm transmission pathways; detection of elite seeding or resource channeling before the recorded adoption curve.',
    'If elite seeding is found, the mechanism is actually hybrid or exogenous, and the authored extractiveness and suppression values understate the true coercive content.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selection_vs_genuine_climb, empirical, 'Whether mass adoption is organically endogenous or elite-orchestrated.').

omega_variable(
    state_following_vs_capturing,
    'Does state codification track independent popular adoption timelines, or does the state selectively recognize only norm-clusters that reduce its governance costs or expand its control?',
    'Comparative historical analysis of adoption timelines versus codification timelines across multiple norm domains and polities.',
    'If the state is selective, it extracts governance efficiency through the mechanism rather than purely coordinating; the directionality of the state seat shifts toward beneficiary in an extraction sense, and the constraint may compute as tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_following_vs_capturing, conceptual, 'Whether state recognition is neutral coordination or selective extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__endogenous_climb_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(impo_tr_t10, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 10, 0.06).
narrative_ontology:measurement(impo_tr_t20, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 20, 0.06).
narrative_ontology:measurement(impo_tr_t30, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 30, 0.07).
narrative_ontology:measurement(impo_tr_t40, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 40, 0.08).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(impo_be_t10, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 10, 0.12).
narrative_ontology:measurement(impo_be_t20, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 20, 0.14).
narrative_ontology:measurement(impo_be_t30, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 30, 0.16).
narrative_ontology:measurement(impo_be_t40, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 40, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(imposition_mechanism_kernel__endogenous_climb_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__endogenous_climb_reading, identity_coordination).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__endogenous_climb_reading, exogenous_override_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__endogenous_climb_reading, hybrid_legitimation_reading).

% DUAL FORMULATION NOTE:
% The imposition_mechanism_kernel decomposes into three structurally distinct readings: endogenous_climb (bottom-up legitimation), exogenous_override (coercive imposition), and hybrid_legitimation (symbolic transfer plus institutional incentives). Each reading instantiates a different constraint with different epsilon, beneficiary structure, and enforcement profile. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
