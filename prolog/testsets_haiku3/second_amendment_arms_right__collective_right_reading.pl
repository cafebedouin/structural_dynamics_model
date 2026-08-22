% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__collective_right_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: second_amendment_arms_right__collective_right_reading
 *   human_readable: Second Amendment as State Militia Authority Protection (Collective Right Reading)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the collective-right reading of the
 *   Second Amendment: the right protects state militia authority to maintain,
 *   organize, and equip armed forces without federal interference, and does
 *   not protect individual gun ownership outside organized militia context.
 *   Under this reading, the prefatory clause ('A well regulated Militia,
 *   being necessary to the security of a free State') defines the operative
 *   scope of the right, and the operative clause protects the militia's right
 *   to arms, not a freestanding individual liberty. This reading constrains
 *   federal power but leaves individual firearms regulation entirely to state
 *   legislatures. The constraint is CLAIMED as rope (genuine coordination of
 *   federal-state militia authority distribution) while the extraction
 *   metrics describe a modest but real asymmetry: state institutions benefit
 *   from plenary regulatory authority while individuals lose a federal
 *   constitutional backstop. The measurement series tracks how theater-ratio
 *   rose from the Founding through modern constitutional practice as states
 *   and courts performed deference to militia organization while the actual
 *   functional concern (state military independence from federal control)
 *   became increasingly moot with federal military dominance.
 *
 * KEY AGENTS:
 *   - state_governments: institutional beneficiary; hold militia authority; interpret the Amendment as protecting their constitutional prerogative
 *   - individual_gun_owners: powerless payer; lack federal constitutional protection under this reading; depend on state legislative grace for any ownership rights
 *   - organized_militia_members: moderate beneficiary; receive constitutional protection precisely because their bearing arms serves state militia function
 *   - federal_government: institutional observer; constrained in militia regulation but operates firearms policy under Commerce Clause authority, not Second Amendment
 *   - individual_right_advocates: organized excluded parties; contest the prefatory-clause primacy and argue for individual liberty protection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__collective_right_reading, 0.28).
domain_priors:suppression_score(second_amendment_arms_right__collective_right_reading, 0.15).
domain_priors:theater_ratio(second_amendment_arms_right__collective_right_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__collective_right_reading, rope).
narrative_ontology:human_readable(second_amendment_arms_right__collective_right_reading, "Second Amendment as State Militia Authority Protection (Collective Right Reading)").
narrative_ontology:topic_domain(second_amendment_arms_right__collective_right_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(second_amendment_arms_right__collective_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__collective_right_reading, '4fa8551b-05c6-4710-8771-92f940f54313').
narrative_ontology:cs_kernel_codification('4fa8551b-05c6-4710-8771-92f940f54313', fixed_text).
narrative_ontology:cs_authority_grounding('4fa8551b-05c6-4710-8771-92f940f54313', lineage).
narrative_ontology:cs_interpretation_layer_present('4fa8551b-05c6-4710-8771-92f940f54313').
narrative_ontology:cs_reading_relation('4fa8551b-05c6-4710-8771-92f940f54313', second_amendment_arms_right__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('4fa8551b-05c6-4710-8771-92f940f54313', second_amendment_arms_right__civic_republican_reading, coexists_with).
narrative_ontology:cs_axiom('4fa8551b-05c6-4710-8771-92f940f54313', foundational, prefatory_clause_scope_limiting).
narrative_ontology:cs_axiom_status(prefatory_clause_scope_limiting, holdable).
narrative_ontology:cs_axiom_grounding('4fa8551b-05c6-4710-8771-92f940f54313', prefatory_clause_scope_limiting, deontological).
narrative_ontology:cs_axiom('4fa8551b-05c6-4710-8771-92f940f54313', foundational, militia_authority_reserved_to_states).
narrative_ontology:cs_axiom_status(militia_authority_reserved_to_states, holdable).
narrative_ontology:cs_axiom_grounding('4fa8551b-05c6-4710-8771-92f940f54313', militia_authority_reserved_to_states, conventional).
narrative_ontology:cs_reference_frame('4fa8551b-05c6-4710-8771-92f940f54313', state_militia_independence_from_federal_consolidation).
narrative_ontology:cs_drift_state('4fa8551b-05c6-4710-8771-92f940f54313', contemporary_federal_military_dominance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4fa8551b-05c6-4710-8771-92f940f54313', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, state_governments).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, militia_regulatory_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, organized_militia_members).
narrative_ontology:constraint_victim(second_amendment_arms_right__collective_right_reading, individual_gun_owners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, hold the constitutional right to maintain, organize, and deploy militia forces without federal interference in their internal organization or armament. They interpret the Second Amendment as protecting their institutional prerogative to determine who may bear arms in militia service and under what conditions. The reading legitimizes their regulatory authority over individual weapons possession outside militia context.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, state_governments, beneficiary,
    institutional, civilizational, analytical, national).

% Under this reading, claim no constitutional protection for firearm ownership outside membership in an organized militia. Their ownership rights depend entirely on state legislative grace and regulation. Any prohibition or licensing requirement imposed by states is constitutionally unreviewable under the Second Amendment. They bear the cost of having no federal constitutional backstop against state regulation.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, individual_gun_owners, payer,
    powerless, biographical, constrained, national).

% Members of state-authorized militia units (National Guard, state defense forces) receive constitutional protection for bearing arms in militia service. They benefit from the reading because their arms-bearing is understood as a protected right precisely because it serves state militia function, not private interest.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, organized_militia_members, beneficiary,
    moderate, biographical, constrained, national).

% Under this reading, lacks enumerated power to regulate individual gun ownership as a Second Amendment matter. The Amendment constrains federal authority by reserving militia authority to the states. Federal action in firearms regulation (e.g., bans, licensing, background checks) operates under Commerce Clause authority, not Second Amendment constraint, and is not subject to constitutional Second Amendment review.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, federal_government, observer,
    institutional, civilizational, analytical, national).

% Constitutional scholars and political advocates who hold the individual-right reading are structurally absent from the decision-making authority under this collective-right reading. They would contest the prefatory-clause primacy and the exclusion of individual rights from constitutional protection, but this reading's authority structure (state legislative power, historical militia-centered text reading) systematically privileges the collective frame and marginalizes their interpretive voice.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, individual_right_advocates, excluded,
    organized, biographical, trapped, national).

% Scholars studying the Founding-era understanding of militia, state power, and the prefatory clause provide interpretive evidence for or against this reading. Under this reading, the weight of historical evidence supports the collective framing; the interpreter's role is to vindicate that historical meaning against modern individualist reinterpretation.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, historical_interpreters, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_arms_right__collective_right_reading, state_governments).
narrative_ontology:fixing_cost_class(second_amendment_arms_right__collective_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Protects state militia authority as an institutional check against federal military monopoly: the Second Amendment coordinates the distribution of armed force between federal and state sovereigns by reserving militia authority to the states and protecting their right to maintain armed forces independent of federal control.
% TRANSFER_FUNCTION: Moves constitutional interpretive authority from individual rights claimants to state legislative bodies: under this reading, the benefit of a Second Amendment right accrues to state institutions (their regulatory prerogative), and the cost falls on individual gun owners who lose federal constitutional protection against state prohibition and regulation.
% ABSENT_VOICES: Individual-right and civic-republican interpretations would contest the prefatory-clause primacy and the exclusion of individual liberty. Gun-rights advocates and constitutional scholars holding the individual reading are not parties to the authority structure that instantiates this collective reading — they would argue for different constitutional meaning but operate outside the decision-making framework.
% DISAPPEARANCE_RATIONALE: Under this reading, if the collective-right interpretation disappeared (and individual-right or civic-republican readings prevailed constitutionally), state regulatory authority would face federal constitutional constraints on firearms prohibition, and individuals would gain a federal constitutional backstop against state overreach. The arrangement would dramatically rearrange because the distribution of constitutional authority would shift from states to individuals. However, the contest turns on which reading correctly interprets the surviving constitutional text — the Amendment itself does not disappear, only the reading of it.
% FOUNDING_PROBLEM: The constitutional text needed to preserve state militia authority as a check on federal military monopoly and prevent federal disarmament of state militias. The prefatory clause ('A well regulated Militia, being necessary to the security of a free State') identifies the problem: protecting the institutional capacity of states to maintain and deploy armed forces against federal consolidation.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Founding era (Garry Wills, Michael Bellesiles in their militia-focused work; some scholarship reading the prefatory clause as primary) attest that state militia independence was a live concern in the founding context. Federal government sources and historical scholars emphasizing nationalist intent (Alexander Hamilton, James Madison in Federalist framing) attest that federal military authority was always supreme and state militias were understood as subordinate. The contest is not over historical facts but over what the historical concern tells us about the Amendment's operative scope — whether it protects institutional state power, individual liberty, or both.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__collective_right_reading, contested).
narrative_ontology:founding_problem_status(second_amendment_arms_right__collective_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__collective_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_arms_right__collective_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__collective_right_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__collective_right_reading_tests).
:- end_tests(second_amendment_arms_right__collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28) because the constraint solves a genuine structural coordination problem (distribution of militia authority between sovereigns) and the 'cost' to individuals (lack of federal constitutional protection) is read as outside the scope of the coordinated right, not as extraction from within it. However, it is not zero because state governments do gain asymmetric interpretive and regulatory authority — they benefit from the reading's framing in ways individuals do not. Suppression is low (0.15) because the constraint does not require active coercion to maintain; it operates through constitutional interpretation and the judiciary's acceptance of the prefatory-clause-primary reading. Theater is modest (0.22) because the actual functional concern (state military independence from federal control) has become attenuated as federal military capacity vastly exceeds state militia capacity — modern recitation of militia purpose increasingly performs institutional deference to that historical concern rather than addressing a live structural threat. The measurement series models this trajectory: theater rises as the functional problem atrophies but constitutional language persists. Accessibility of alternatives is moderate (0.42) because the individual-right reading and civic-republican reading remain live in scholarly and political discourse, and some state legislatures could legislatively establish stronger protections for individual ownership without constitutional contradiction under this reading (states can be more protective than the federal floor, but cannot go less than zero protection). Resistance is high (0.78) because the individual-right and civic-republican communities mount substantial constitutional and political opposition to the collective reading — the reading persists despite persistent, organized resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the state-government seat, this reading is a genuine coordination solution: it reserves militia authority to states and protects institutional prerogative. From the individual-gun-owner seat, the same structure operates as disempowerment — loss of federal constitutional protection. From the federal seat, the reading constrains federal militia regulation but leaves federal commerce-clause authority intact, so the perspectival gap depends on whether one reads the Second Amendment as the relevant constraint (state militia authority) or whether one reads federal firearms regulation under Commerce Clause as a separate constraint. The engine computes these divergent readings from the same structural data — the authored claim (rope, coordination) and the authored metrics (modest extraction, low suppression) reflect the reading's own frame; the computed type for each seat depends on that seat's directionality and power.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments are structural beneficiaries (d near 0.0): they gain interpretive and regulatory authority, have high power, and enjoy the reading's primacy in their institutional interest. Individual gun owners are targets (d near 1.0): they lose federal constitutional protection, have low power, and face the constraint's suppression of their claimed right through interpretive exclusion from the operative scope. Organized militia members sit closer to symmetric (d near 0.5): they benefit from the reading's protection of their militia-service bearing, but are also constrained by state regulatory authority. Federal government is analytical/observer (d not computed: power=institutional, exit=analytical, making d undefined in the directionality engine — the federal government's position is not 'target' or 'beneficiary' but 'external referee constrained by interpretation').
 *
 * MANDATROPHY ANALYSIS:
 *   This reading faces a mandatrophy test: was the founding problem (state militia independence from federal military consolidation) solved, and if so, does the constraint persist only from institutional inertia? The historical claim is that at the Founding, state militias were a genuine check on federal power and militia independence was a live structural concern. By the 20th century, federal military capacity vastly exceeded state militia capacity, rendering the structural check obsolete. The reading persists through constitutional interpretation and judicial acceptance of the prefatory-clause primacy, not because the founding problem is still active. Theater-ratio rise (0.08 → 0.22) models this atrophy: modern recitation of militia purpose in constitutional argument increasingly performs deference to a historical concern rather than addressing a live structural threat. However, mandatrophy resolution requires both: (1) founding problem dead, AND (2) constraint persists by inertia with no concentrated beneficiary maintaining it. State governments do benefit from the reading (it reserves regulatory authority to them), so the constraint persists because a concentrated institutional beneficiary defends it, not because of pure inertia. This reading has not yet crossed into piton territory — it is a rope where the functional coordination problem has attenuated but institutional beneficiaries actively sustain the interpretive frame.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prefatory_clause_scope_determination,
    'Does the prefatory clause define the scope of the operative clause (the right protects militia-service bearing only), or does it identify a purpose without limiting scope (the right protects all arms-bearing, motivated by militia concerns)?',
    'Originalist analysis of prefatory-clause grammar and constitutional drafting conventions; comparison to contemporaneous constitutions and state militia laws; examination of Founders'' own usage of the prefatory-clause structure.',
    'If prefatory clause defines scope, the collective reading is vindicated and individual ownership outside militia context is unrestricted by the Second Amendment (states regulate completely). If prefatory clause identifies motivation only, the operative clause protects individual bearing-arms generally, and the individual reading is vindicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(prefatory_clause_scope_determination, conceptual, 'Whether the prefatory clause constrains the operative clause''s scope or merely describes motivation.').

omega_variable(
    militia_institutional_independence_atrophy,
    'Has the founding problem (state militia as structural check on federal military monopoly) genuinely become obsolete given modern federal military dominance, or does the concern remain live in decentralized armed resistance to tyranny?',
    'Historical and strategic analysis: can state militias meaningfully check federal military power today? Are Founders'' concerns about standing armies vindicated or rendered moot by technological and organizational change?',
    'If the problem is truly obsolete, the constraint faces mandatrophy: state regulatory authority persists while the structural rationale atrophies. If the concern remains live, the constraint is still solving a real problem and is not yet degraded. This affects classification toward piton if theater-ratio rises without functional grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_institutional_independence_atrophy, empirical, 'Whether state militia independence remains a live structural concern or is obsolete.').

omega_variable(
    interpretive_authority_vs_individual_rights_contest,
    'Can the same constitutional text simultaneously protect both state militia authority (the collective reading''s frame) and individual liberty (the individual reading''s frame), or do the readings genuinely foreclose one another such that accepting one requires rejecting the other?',
    'Constitutional theory of pluralistic interpretation; examination of whether courts could apply both readings contextually (militia service vs. individual possession in different legal contexts) or whether the readings make incompatible claims about what the text means.',
    'If the readings can coexist (applied to different domains), the contest is a legislative/policy choice, not a logical foreclosure — both readings remain live and the kernel is genuinely contested. If one reading logically forecloses the other (to hold both is to contradict yourself), then the engine''s foreclosure computation correctly identifies which reading is logically defeated. This affects how the three constraint stories relate via reading_relations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interpretive_authority_vs_individual_rights_contest, conceptual, 'Whether the collective and individual readings logically foreclose each other or can coexist as applied to different domains.').

omega_variable(
    suppression_mechanism_interpretive_vs_structural,
    'Is the suppression of individual-right claims under this reading structural (external barriers preventing gun ownership) or interpretive (the text''s constitutional meaning, as read, excludes individual liberty from protection)?',
    'Examine the mechanism of enforcement: under this reading, states CAN prohibit individual gun ownership because no federal constitutional right protects it. Suppression comes from states exercising that regulatory authority (structural), not from the interpretation itself. Post-interpretation, if an individual gun owner leaves the jurisdiction, does the suppression persist (internalized) or lift (structural)?',
    'If suppression is purely interpretive/structural, the reading''s suppression metric (0.15) is accurate. If it is substantially internalized (individuals accept the interpretation and carry the sense of no right even after exit), effective suppression is higher than the metric suggests. This affects how individual-gun-owner directionality is computed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_interpretive_vs_structural, empirical, 'Whether suppression operates through interpretive exclusion (structural) or through internalized acceptance of the reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__collective_right_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_arms_right__collective_right_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(seco_tr_t5, second_amendment_arms_right__collective_right_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(seco_tr_t10, second_amendment_arms_right__collective_right_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(seco_tr_t15, second_amendment_arms_right__collective_right_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(seco_tr_t20, second_amendment_arms_right__collective_right_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(seco_tr_t25, second_amendment_arms_right__collective_right_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement(seco_tr_t30, second_amendment_arms_right__collective_right_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(seco_tr_t35, second_amendment_arms_right__collective_right_reading, theater_ratio, 35, 0.22).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_arms_right__collective_right_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(seco_be_t5, second_amendment_arms_right__collective_right_reading, base_extractiveness, 5, 0.2).
narrative_ontology:measurement(seco_be_t10, second_amendment_arms_right__collective_right_reading, base_extractiveness, 10, 0.23).
narrative_ontology:measurement(seco_be_t15, second_amendment_arms_right__collective_right_reading, base_extractiveness, 15, 0.26).
narrative_ontology:measurement(seco_be_t20, second_amendment_arms_right__collective_right_reading, base_extractiveness, 20, 0.27).
narrative_ontology:measurement(seco_be_t25, second_amendment_arms_right__collective_right_reading, base_extractiveness, 25, 0.28).
narrative_ontology:measurement(seco_be_t30, second_amendment_arms_right__collective_right_reading, base_extractiveness, 30, 0.28).
narrative_ontology:measurement(seco_be_t35, second_amendment_arms_right__collective_right_reading, base_extractiveness, 35, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_arms_right__collective_right_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(seco_su_t5, second_amendment_arms_right__collective_right_reading, suppression_requirement, 5, 0.13).
narrative_ontology:measurement(seco_su_t10, second_amendment_arms_right__collective_right_reading, suppression_requirement, 10, 0.14).
narrative_ontology:measurement(seco_su_t15, second_amendment_arms_right__collective_right_reading, suppression_requirement, 15, 0.15).
narrative_ontology:measurement(seco_su_t20, second_amendment_arms_right__collective_right_reading, suppression_requirement, 20, 0.15).
narrative_ontology:measurement(seco_su_t25, second_amendment_arms_right__collective_right_reading, suppression_requirement, 25, 0.15).
narrative_ontology:measurement(seco_su_t30, second_amendment_arms_right__collective_right_reading, suppression_requirement, 30, 0.15).
narrative_ontology:measurement(seco_su_t35, second_amendment_arms_right__collective_right_reading, suppression_requirement, 35, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__collective_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_arms_right__collective_right_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right__civic_republican_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel second_amendment_arms_right. It shares the constitutional text with the individual_right_reading and civic_republican_reading but instantiates a structurally different constraint: different ε (0.28 vs. estimated 0.15 for individual reading, 0.35 for civic-republican reading), different beneficiary structure (state governments vs. individual gun owners vs. armed citizens), different classification (rope coordinating federal-state authority vs. snare extracting from powerless vs. tangled-rope hybrid). Each reading authoritatively claims to represent the true constitutional meaning. The three stories form a constraint family; a consumer reading any one must trace the network edges to understand the sibling claims and their structural differences. The reading contest turns on interpretive choice, not empirical discovery — each reading's ε measures the constraint it instantiates (the standing arrangement under contest, as that reading sees it), not the constitutional text itself (which is fixed).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_arms_right__collective_right_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
