% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__military_custodian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__military_custodian_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: july_charter_sovereign_legitimacy__military_custodian_reading
 *   human_readable: Charter Military Guardian Clause — Military Custodian Reading
 *   domain: constitutional_law/political_transitions
 *
 * SUMMARY:
 *   A post-revolutionary charter establishes the military as the permanent
 *   guardian of the revolution and the state, granting it veto authority over
 *   civilian governance. The military custodian reading holds that this
 *   arrangement is a genuine coordination mechanism: without a unified
 *   security command, the revolution would have fragmented into warlordism or
 *   succumbed to external intervention. The same structure, however, operates
 *   as asymmetric extraction — the military and security apparatus collect
 *   institutional autonomy, budget priority, and immunity from
 *   accountability, while civilian institutions, political parties, and civil
 *   society pay in constrained sovereignty and bounded contestation. The
 *   claim/metric divergence is deliberate: the constraint is CLAIMED as
 *   tangled_rope (coordination + extraction) while the metrics describe
 *   substantially extractive, actively enforced operation with rising theater
 *   ratio — the engine measures this divergence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__military_custodian_reading, 0.75).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__military_custodian_reading, 0.8).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__military_custodian_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__military_custodian_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__military_custodian_reading, "Charter Military Guardian Clause — Military Custodian Reading").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__military_custodian_reading, "constitutional_law/political_transitions").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__military_custodian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__military_custodian_reading, 'c4ddf62e-8149-4ef6-8e09-abff0e77a5a7').
narrative_ontology:cs_kernel_codification('c4ddf62e-8149-4ef6-8e09-abff0e77a5a7', formalized).
narrative_ontology:cs_authority_grounding('c4ddf62e-8149-4ef6-8e09-abff0e77a5a7', extraction).
narrative_ontology:cs_interpretation_layer_present('c4ddf62e-8149-4ef6-8e09-abff0e77a5a7').
narrative_ontology:cs_reading_relation('c4ddf62e-8149-4ef6-8e09-abff0e77a5a7', july_charter_sovereign_legitimacy__secular_democratic_reading, forecloses).
narrative_ontology:cs_reading_relation('c4ddf62e-8149-4ef6-8e09-abff0e77a5a7', july_charter_sovereign_legitimacy__guided_nationalism_reading, influences).
narrative_ontology:cs_axiom('c4ddf62e-8149-4ef6-8e09-abff0e77a5a7', foundational, military_as_permanent_guardian).
narrative_ontology:cs_axiom_status(military_as_permanent_guardian, holdable).
narrative_ontology:cs_axiom_grounding('c4ddf62e-8149-4ef6-8e09-abff0e77a5a7', military_as_permanent_guardian, conventional).
narrative_ontology:cs_axiom('c4ddf62e-8149-4ef6-8e09-abff0e77a5a7', foundational, stability_requires_subordination).
narrative_ontology:cs_axiom_status(stability_requires_subordination, holdable).
narrative_ontology:cs_axiom_grounding('c4ddf62e-8149-4ef6-8e09-abff0e77a5a7', stability_requires_subordination, instrumental).
narrative_ontology:cs_reference_frame('c4ddf62e-8149-4ef6-8e09-abff0e77a5a7', revolutionary_stability_settlement).
narrative_ontology:cs_drift_state('c4ddf62e-8149-4ef6-8e09-abff0e77a5a7', post_reform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c4ddf62e-8149-4ef6-8e09-abff0e77a5a7', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, military_institution).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, security_apparatus).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, autonomous_political_parties).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, student_movement).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, security_apparatus).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, civilian_government).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__military_custodian_reading, military_guardianship_doctrine).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__military_custodian_reading, stability_over_liberty_principle).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__military_custodian_reading, revolutionary_continuity_through_unified_command).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds constitutionally entrenched veto authority over civilian government decisions on national security, foreign policy, and key appointments. Justifies this role as the guarantor of state continuity and revolutionary gains. Collects institutional prerogatives, budget autonomy, and immunity from civilian oversight directly from the charter provision.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, military_institution, agenda_setter,
    institutional, generational, arbitrage, national).

% Operationalizes the military's guardian mandate through intelligence, internal security, and border control. Gains institutional expansion, resource priority, and legal cover for operations. Also bears operational risks and institutional rigidity — career paths and professional identity are fused to the guardian mission, making exit structurally costly.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, security_apparatus, beneficiary,
    powerful, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__military_custodian_reading, security_apparatus, payer).

% Formally administers the state but operates within boundaries set by military veto. Must seek approval for security-sensitive decisions, limiting policy autonomy. Gains legitimacy from the charter framework but pays in constrained sovereignty. Exit would mean constitutional crisis or extra-legal confrontation.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, civilian_government, payer,
    organized, biographical, constrained, national).

% Contest elections and parliament but cannot challenge military prerogatives or the security apparatus's domestic role. Platform proposals on defense, intelligence, or civil-military relations are effectively pre-empted. Their organizational survival depends on navigating the bounded political space without triggering suppression.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, autonomous_political_parties, payer,
    moderate, biographical, constrained, national).

% Mobilizes for democratic expansion and civilian supremacy but faces direct repression from the security apparatus operating under the guardian mandate. No institutional pathway to reform the charter; protest is met with arrest, surveillance, and university purges. Exit means exile, silence, or co-optation.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, student_movement, payer,
    powerless, immediate, trapped, national).

% Monitor elections, human rights, and constitutional compliance. Document the gap between charter text and practice. Their assessments shape aid conditionality and diplomatic pressure but cannot directly alter the domestic power balance.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, international_observers, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single authoritative guarantor of state continuity and territorial integrity, preventing fragmentation and civil conflict by subordinating political competition to a unified security command.
% TRANSFER_FUNCTION: Moves political autonomy and decision-making authority from civilian institutions and civil society to the military institution, as the price of guaranteed stability and prevention of state collapse.
% ABSENT_VOICES: Autonomous political parties and student movements are structurally bounded by the security apparatus; they would contest the military's veto authority but operate within boundaries set by the constraint itself. Diaspora opposition and banned organizations are physically excluded from the political arena.
% DISAPPEARANCE_RATIONALE: If the military guardian clause vanished overnight, civilian institutions would immediately contest previously settled security prerogatives, political competition would expand into previously forbidden domains (foreign policy, appointments, budget), and the security apparatus would lose its constitutional anchor — the post-revolutionary settlement would reorganize around civilian supremacy.
% FOUNDING_PROBLEM: Post-revolutionary fragmentation and the threat of state collapse — competing revolutionary factions, weak civilian institutions, and external threats created a vacuum where no single actor could guarantee continuity.
% FOUNDING_PROBLEM_CORROBORATION: The military institution attests the founding problem remains live, citing persistent internal and external threats. Civilian political actors and independent historians attest the founding problem (state collapse) was resolved decades ago and the arrangement persists as institutional self-preservation; transitional justice commissions and constitutional scholars from outside the military establishment support the shifted-function reading.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__military_custodian_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__military_custodian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__military_custodian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__military_custodian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__military_custodian_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__military_custodian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__military_custodian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__military_custodian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75) because the military's veto power extracts real decision-making authority from civilian institutions without commensurate accountability. Suppression is higher (0.8) because the constraint's persistence depends on actively bounding political contestation through the security apparatus, not on participant consent. Theater ratio rises from 0.25 to 0.4 over the interval: the revolutionary-emergency justification decays while performative stability-maintenance (parades, ceremonies, institutional rhetoric) expands. The measurement series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   From the military's position, the arrangement is genuine coordination it built and maintains — without it, state collapse. From the civilian government's position, the same structure operates as enforced extraction with constrained exit. From the student movement's position, it is a snare — suppression without coordination benefit. The engine computes per-seat classifications from these structural asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   The military institution is the structural beneficiary (collects veto authority, budget autonomy, immunity — d near beneficiary end). The security apparatus is a secondary beneficiary with identity-locked exit (professional identity fused to guardian mission). Civilian government, political parties, and student movement are targets (pay in constrained sovereignty, bounded contestation, direct repression — d near target end). International observers sit at analytical remove. The engine computes this divergence from the structural data.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-revolutionary fragmentation) was substantially resolved by the 1990s, but the arrangement persists because the military institution extracts enough benefit to defend it, while no single civilian actor bears enough concentrated cost to overturn it alone — coalition among victims is structurally prevented by the suppression apparatus. This is mandatrophy: the mandate (guarantee stability against collapse) has outlived its function, but the constraint persists through extraction and suppression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is the military_custodian_reading a genuine stability coordination mechanism, or is it an institutional capture that uses the stability narrative as cover?',
    'Counterfactual analysis: if the military veto were removed, would state fragmentation actually occur, or would civilian institutions develop functional coordination? Historical comparison with parallel transitions where military guardianship was absent or removed.',
    'If coordination is genuine, the constraint remains tangled_rope; if the stability narrative is cover for extraction, it reclassifies toward snare. The sibling readings'' coexistence depends on this boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the military guardian function is structurally necessary or a self-justifying extraction narrative.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (security apparatus barriers) or partially internalized (political actors self-censoring because they''ve internalized the guardian frame)?',
    'Post-reform suppression trajectory: if political contestation expands rapidly after a reduction in overt repression, internalized suppression was significant. Survey experiments measuring perceived vs. actual boundaries of permissible speech.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint''s extraction persists even if formal enforcement relaxes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the bounded political space.').

omega_variable(
    coordination_extraction_separability,
    'Is the military''s veto authority structurally inseparable from the stability coordination function, or can stability be maintained through civilian democratic institutions?',
    'Natural experiment from periods of civilian-led governance (e.g., reformist presidencies): if stability held without military veto, the functions are separable. Comparative analysis with regional transitions.',
    'If separable, the veto is pure extraction riding on a real coordination function; if inseparable, part of measured extraction is the price of coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether military veto and stability coordination are structurally separable functions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__military_custodian_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(july_tr_t9, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 9, 0.28).
narrative_ontology:measurement(july_tr_t18, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 18, 0.32).
narrative_ontology:measurement(july_tr_t27, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 27, 0.36).
narrative_ontology:measurement(july_tr_t36, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 36, 0.38).
narrative_ontology:measurement(july_tr_t45, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 45, 0.4).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(july_be_t9, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 9, 0.68).
narrative_ontology:measurement(july_be_t18, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 18, 0.7).
narrative_ontology:measurement(july_be_t27, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 27, 0.72).
narrative_ontology:measurement(july_be_t36, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 36, 0.74).
narrative_ontology:measurement(july_be_t45, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 45, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(july_su_t9, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 9, 0.72).
narrative_ontology:measurement(july_su_t18, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 18, 0.75).
narrative_ontology:measurement(july_su_t27, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 27, 0.77).
narrative_ontology:measurement(july_su_t36, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 36, 0.79).
narrative_ontology:measurement(july_su_t45, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 45, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__military_custodian_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(july_charter_sovereign_legitimacy__military_custodian_reading, 0.1).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy__secular_democratic_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy__guided_nationalism_reading).

% DUAL FORMULATION NOTE:
% This constraint and its two siblings form the july_charter_sovereign_legitimacy constraint family. Each reading instantiates a different constraint from the same kernel text with different ε values, beneficiary/victim structures, and claimed types. The military_custodian_reading claims tangled_rope (coordination + extraction); the secular_democratic_reading claims rope (coordination with civilian supremacy); the guided_nationalism_reading claims scaffold (transitional religious framework). They are linked because the upstream charter text is cited as evidence for all three downstream operational constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(july_charter_sovereign_legitimacy__military_custodian_reading, institutional, 0.15).
constraint_indexing:directionality_override(july_charter_sovereign_legitimacy__military_custodian_reading, powerful, 0.35).
constraint_indexing:directionality_override(july_charter_sovereign_legitimacy__military_custodian_reading, moderate, 0.7).
constraint_indexing:directionality_override(july_charter_sovereign_legitimacy__military_custodian_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
