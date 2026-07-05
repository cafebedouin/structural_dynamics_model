% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__stateless_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__stateless_coordination_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: feud_obligation_kernel__stateless_coordination_reading
 *   human_readable: Blood-Feud Obligation as Self-Enforcing Justice Coordination
 *   domain: legal_anthropology/comparative_political_systems
 *
 * SUMMARY:
 *   In stateless or weakly-centralized societies (medieval Iceland, early
 *   medieval Ireland, highland Albania, pre-state Montenegro), blood-feud
 *   obligation operates as a kin-based liability system: an injury to a
 *   kin-group member obligates the group to seek redress, either through
 *   counter-violence or negotiated settlement (wergild). This story
 *   instantiates the reading under which feud obligation is a genuine,
 *   low-overhead coordination mechanism that substitutes for absent state
 *   enforcement capacity — providing both retrospective justice (redress for
 *   the injured party's kin) and prospective deterrence (the credible threat
 *   of retaliation discourages initial aggression), while wergild remains
 *   available as a lower-cost settlement path that most disputes actually
 *   use. This is ONE of three readings of the feud_obligation_kernel; the
 *   sibling christianized_pacification_reading holds that legitimate violence
 *   authority belongs to God/ecclesiastical-royal institutions and feud
 *   vengeance is itself illegitimate, while the sibling
 *   extraction_cycle_reading holds that feud is a destructive,
 *   self-perpetuating cycle that depletes productive capacity. Each sibling
 *   is a separate constraint story with its own ε and its own stakeholder
 *   set; this story does not describe or average over them.
 *
 * KEY AGENTS:
 *   - kin_groups_seeking_redress: Primary beneficiary (moderate/constrained) — receive justice/deterrence value from the credible threat of retaliation
 *   - feud_defectors_facing_honor_loss: Primary target of the mechanism's internal enforcement (powerless/identity_locked) — bear the cost of declining to participate
 *   - wergild_arbiters: Secondary coordination actor (moderate/mobile) — provide the lower-cost settlement alternative that keeps the system from escalating
 *   - comparative_legal_historian: Analytical observer — sees the full structure across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__stateless_coordination_reading, 0.32).
domain_priors:suppression_score(feud_obligation_kernel__stateless_coordination_reading, 0.28).
domain_priors:theater_ratio(feud_obligation_kernel__stateless_coordination_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__stateless_coordination_reading, rope).
narrative_ontology:human_readable(feud_obligation_kernel__stateless_coordination_reading, "Blood-Feud Obligation as Self-Enforcing Justice Coordination").
narrative_ontology:topic_domain(feud_obligation_kernel__stateless_coordination_reading, "legal_anthropology/comparative_political_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__stateless_coordination_reading, 'bf329a78-84d7-4107-87b3-d67920e17382').
narrative_ontology:cs_kernel_codification('bf329a78-84d7-4107-87b3-d67920e17382', distributed).
narrative_ontology:cs_authority_grounding('bf329a78-84d7-4107-87b3-d67920e17382', practice).
narrative_ontology:cs_interpretation_layer_present('bf329a78-84d7-4107-87b3-d67920e17382').
narrative_ontology:cs_reading_relation('bf329a78-84d7-4107-87b3-d67920e17382', feud_obligation_kernel__christianized_pacification_reading, coexists_with).
narrative_ontology:cs_reading_relation('bf329a78-84d7-4107-87b3-d67920e17382', feud_obligation_kernel__extraction_cycle_reading, coexists_with).
narrative_ontology:cs_axiom('bf329a78-84d7-4107-87b3-d67920e17382', foundational, kin_group_self_help_is_legitimate_justice).
narrative_ontology:cs_axiom_status(kin_group_self_help_is_legitimate_justice, holdable).
narrative_ontology:cs_axiom_grounding('bf329a78-84d7-4107-87b3-d67920e17382', kin_group_self_help_is_legitimate_justice, conventional).
narrative_ontology:cs_axiom('bf329a78-84d7-4107-87b3-d67920e17382', secondary, wergild_availability_demonstrates_coordination_not_coercion).
narrative_ontology:cs_axiom_status(wergild_availability_demonstrates_coordination_not_coercion, holdable).
narrative_ontology:cs_axiom_grounding('bf329a78-84d7-4107-87b3-d67920e17382', wergild_availability_demonstrates_coordination_not_coercion, empirically_contingent).
narrative_ontology:cs_reference_frame('bf329a78-84d7-4107-87b3-d67920e17382', kin_based_self_help_justice).
narrative_ontology:cs_drift_state('bf329a78-84d7-4107-87b3-d67920e17382', post_royal_court_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bf329a78-84d7-4107-87b3-d67920e17382', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, kin_groups_seeking_redress).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, potential_future_victims_deterred_by_reputation).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, community_dispute_resolution_order).
narrative_ontology:constraint_victim(feud_obligation_kernel__stateless_coordination_reading, feud_defectors_facing_honor_loss).
narrative_ontology:constraint_victim(feud_obligation_kernel__stateless_coordination_reading, kinship_expelled_non_avengers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% When a member is injured or killed, the kin group is entitled to seek redress via retaliation or negotiated wergild. They receive the coordination value of a credible deterrent and a socially sanctioned path to justice in a system with no police or courts to appeal to; their exit option is constrained because leaving the kinship network entirely would strip them of the protection the same network provides.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, kin_groups_seeking_redress, beneficiary,
    organized, generational, constrained, regional).

% A kin member who declines to support the group's retaliation or fails to participate as obligated loses honor standing and risks expulsion from the kinship network's protection. Their identity as a member of the kin group is bound up with fulfilling the obligation, making exit costly not merely materially but in terms of social selfhood; they bear the mechanism's internal enforcement cost.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, feud_defectors_facing_honor_loss, payer,
    powerless, biographical, identity_locked, local).

% Individuals actually expelled from kin-group protection for failing to avenge are left without the network's defense in a society with no alternative centralized protection; their situation after expulsion is materially worse and their exit options collapse further, since they cannot easily join another kin group.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, kinship_expelled_non_avengers, payer,
    powerless, biographical, trapped, local).

% Respected local figures (chieftains, elders, assembly officials) who negotiate wergild settlements as an alternative to escalating retaliation. They administer the lower-cost settlement path that keeps the feud system from spiraling; their continued relevance depends on disputing parties finding settlement preferable to continued violence, which they have some capacity to steer.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, wergild_arbiters, agenda_setter,
    moderate, biographical, mobile, regional).

% Community members whose potential aggressors are deterred by the credible threat that the victim's kin group would retaliate. They never see the mechanism activate in their favor but benefit from the baseline reduction in violence that a credible feud-retaliation norm produces community-wide.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, potential_future_victims_deterred_by_reputation, beneficiary,
    moderate, generational, constrained, regional).

% Studies feud-society law codes (Icelandic sagas, Irish Brehon law, Albanian Kanun) to assess whether feud obligation functioned as low-overhead coordination, coercive extraction, or illegitimate usurpation of a properly centralized violence-authority — the three contested readings of this kernel.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, comparative_legal_historian, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feud_obligation_kernel__stateless_coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(feud_obligation_kernel__stateless_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides justice (redress for injury to kin) and deterrence (credible threat of retaliation discourages initial aggression) in a society lacking centralized enforcement capacity, with wergild available as a lower-cost settlement alternative to actual violence.
% TRANSFER_FUNCTION: Moves compensation (wergild) or retaliatory harm from an aggressor's kin group to an injured party's kin group, roughly proportional to the original injury; internally, moves honor/status away from kin members who decline to fulfill the retaliation or support obligation.
% ABSENT_VOICES: Individuals expelled from kin-group protection for non-participation have no forum within the feud system itself to contest their expulsion; a kin group too weak to credibly threaten retaliation has no recourse but submission or flight, and its voice is absent from the deterrence equation entirely.
% DISAPPEARANCE_RATIONALE: If feud obligation vanished overnight in a genuinely stateless society with no substitute enforcement mechanism, the deterrent function underlying local order would disappear with it; absent an alternative (state courts, effective wergild-only norms), aggression against weaker kin groups would likely rise until some substitute coordination mechanism re-emerged — the arrangement is load-bearing for local order under this reading, not decorative.
% FOUNDING_PROBLEM: In the absence of a state with a monopoly on legitimate violence and the administrative capacity to investigate, adjudicate, and enforce judgments, kin groups needed a self-enforcing mechanism to deter aggression and secure redress for injuries without appeal to any higher authority.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians studying societies with functioning royal or ecclesiastical courts (external to the feuding kin groups) attest that in those same regions, once centralized adjudication becomes genuinely available and effective, feud obligation's practical necessity visibly declines and wergild-only or royal-court settlement increasingly displaces retaliatory violence — external corroboration that the founding problem is period- and polity-specific rather than perpetually live everywhere the norm is invoked.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__stateless_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__stateless_coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__stateless_coordination_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(feud_obligation_kernel__stateless_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__stateless_coordination_reading, 0.32, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__stateless_coordination_reading_tests).
:- end_tests(feud_obligation_kernel__stateless_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.32) because, under this reading, the mechanism's primary transfer is retributive/deterrent, not rent-extractive: value flows from aggressor's kin to victim's kin roughly in proportion to the original injury, and wergild caps escalation. Suppression is authored low-moderate (0.28) because wergild and other settlement paths remain genuinely open alongside feud — the coordination story does not depend on foreclosing alternatives, which is precisely the structural delta distinguishing this reading from its siblings. Theater ratio is low (0.15) because the mechanism's violence and settlement activity is functionally load-bearing, not performative. Accessibility collapse is moderate (0.35): once inside a kin group, the obligation is hard to escape, but the mechanism does not collapse alternatives at the societal level (multiple kin groups, multiple settlement paths coexist).
 *
 * PERSPECTIVAL GAP:
 *   From the kin-group beneficiary seat, the system reads as functional justice-and-deterrence coordination sustaining social order in the absence of a state. From the defector seat, the same structure reads as coercive: honor loss and kinship expulsion are real costs imposed for non-compliance, regardless of whether a court or king imposes them. The engine should compute divergent per-seat types from these structural facts; this divergence is exactly what the mandatrophy/perspectival apparatus exists to surface, not something this story should resolve by fiat.
 *
 * DIRECTIONALITY LOGIC:
 *   Kin groups seeking redress and future potential victims deterred by the reputation-and-retaliation system sit near the beneficiary end: the mechanism functions for them roughly as insurance plus deterrence. Feud defectors — kin members who decline to avenge or support avenging — sit near the target end: their exit from the obligation costs them honor and kinship standing, which is the mechanism's internal enforcement lever. This is NOT identical to the coercion of a centralized state; it is closer to a reputational/relational sanction within a voluntary-in-origin kinship structure, hence identity_locked rather than trapped for the defector seat.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading treats the founding problem (justice and deterrence absent centralized enforcement) as substantially still live in the societies where feud obligation actually operates without state alternatives — hence founding_problem_status is authored 'contested' rather than 'dead', because whether the problem remains live depends heavily on which historical period and polity one examines (a saga-era Iceland with no king differs from a later period where royal courts become available and feud obligation persists past its functional necessity — which is exactly the terrain the extraction_cycle and pacification siblings claim).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_framing,
    'Is blood-feud obligation best modeled as a genuine coordination mechanism substituting for absent state capacity, or as a self-perpetuating extraction cycle that consumes the productive surplus it claims to protect?',
    'Comparative study of feud-society economic output and violence rates against contemporaneous centralized-enforcement polities of similar resource base; track whether feud cycles terminate via wergild settlement (coordination signature) or escalate indefinitely (extraction signature).',
    'If feud cycles systematically fail to terminate via wergild and instead escalate across generations, the coordination reading collapses toward the extraction_cycle_reading sibling; this story''s low suppression/extraction values would need revision in that sibling constraint, not this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_framing, conceptual, 'Whether feud obligation is a stable coordination equilibrium or a disguised extraction spiral — the central contest between this reading and its extraction_cycle sibling.').

omega_variable(
    kernel_reading_selection_evidence,
    'What in the ethnographic/historical record justifies selecting the stateless-coordination reading over the christianized-pacification or extraction-cycle readings as the operative structural account for a given feud society at a given time?',
    'Cross-reference saga/law-code evidence for (a) presence and uptake rate of wergild as an accepted substitute, (b) rate of feud termination without escalation, (c) ecclesiastical/royal intervention frequency and success rate in the same period.',
    'High wergild uptake and low escalation support this reading; high ecclesiastical intervention success supports the pacification reading; high multi-generational escalation supports the extraction reading. Different societies/periods may genuinely instantiate different readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, empirical, 'Evidentiary basis for treating this kernel reading as the structurally correct one for a given case, versus its two siblings.').

omega_variable(
    defector_victim_status_ambiguity,
    'Is the ''victim'' status of feud-obligation defectors (honor loss, kinship expulsion) itself evidence of coercive suppression internal to the coordination mechanism, or is it the ordinary cost of declining to participate in any voluntary reciprocal institution?',
    'Examine whether defectors retain access to alternative dispute-resolution or protection mechanisms (wergild, sanctuary, patron protection) after defection — if alternatives remain open, defection cost is closer to ordinary opportunity cost; if defectors become unprotectable (outlawry), the cost is closer to structural suppression.',
    'If defectors are rendered wholly unprotectable, suppression should be scored materially higher than authored here, pushing this reading toward tangled_rope; if alternatives remain genuinely open, the low suppression score is well-grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(defector_victim_status_ambiguity, empirical, 'Whether the cost borne by non-avenging kin constitutes coercive suppression or ordinary non-participation cost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__stateless_coordination_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(feud_tr_t10, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(feud_tr_t20, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(feud_tr_t30, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 30, 0.13).
narrative_ontology:measurement(feud_tr_t40, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 40, 0.14).
narrative_ontology:measurement(feud_tr_t50, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(feud_be_t10, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(feud_be_t20, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(feud_be_t30, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 30, 0.32).
narrative_ontology:measurement(feud_be_t40, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 40, 0.32).
narrative_ontology:measurement(feud_be_t50, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 50, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(feud_su_t10, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 10, 0.24).
narrative_ontology:measurement(feud_su_t20, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 20, 0.25).
narrative_ontology:measurement(feud_su_t30, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 30, 0.27).
narrative_ontology:measurement(feud_su_t40, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 40, 0.28).
narrative_ontology:measurement(feud_su_t50, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 50, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__stateless_coordination_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(feud_obligation_kernel__stateless_coordination_reading, 0.12).
narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, extraction_cycle_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, christianized_pacification_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of feud_obligation_kernel. extraction_cycle_reading models the same underlying practice as a destructive rent/violence spiral depleting productive capacity; christianized_pacification_reading models it as an illegitimate usurpation of violence-authority properly held by God/ecclesiastical-royal institutions. All three share the kernel (the feud obligation itself) but diverge in beneficiary/victim structure, suppression profile, and claimed type. Linked here per the ε-invariance decomposition principle; each carries its own stable ε rather than a single averaged value.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
