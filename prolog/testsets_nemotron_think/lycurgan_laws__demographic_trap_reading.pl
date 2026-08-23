% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__demographic_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__demographic_trap_reading, []).

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
 *   constraint_id: lycurgan_laws__demographic_trap_reading
 *   human_readable: Lycurgan Constitutional Immutability as Demographic Trap
 *   domain: political_philosophy/constitutional_theory
 *
 * SUMMARY:
 *   The Lycurgan constitutional order (the Great Rhetra) presents itself as
 *   an immutable divine ordinance delivered by Apollo at Delphi. This reading
 *   treats that immutability as a structural snare: the laws' unrevisability
 *   prevents adaptation to demographic collapse caused by the inalienable
 *   kleros system, the citizenship property threshold, and the
 *   helot-dependent economy. The constraint extracts adaptive capacity from
 *   the Spartiate citizen body and labor from the helots, while the
 *   oligarchic elite (ephors, gerousia, landed families) benefit from the
 *   concentration of land and power. The engine will compute per-seat types
 *   from the structural data; the claimed type (snare) is the author's
 *   structural judgment.
 *
 * KEY AGENTS:
 *   - spartan_oligarchic_elite: Primary beneficiary (powerful/arbitrage) — concentrates land and political control
 *   - spartiates: Primary payer (moderate/identity_locked) — bears demographic collapse, cannot exit the citizen identity
 *   - helots: Excluded victim (powerless/trapped) — provides the economic base, no voice
 *   - ephors: Dual agenda_setter/payer (institutional/constrained) — enforces the trap while being trapped by it
 *   - kings: Beneficiary (powerful/constrained) — sacral legitimacy but bound by the same immutable laws
 *   - modern_scholar: Observer (analytical/analytical) — sees the structural death spiral
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__demographic_trap_reading, 0.85).
domain_priors:suppression_score(lycurgan_laws__demographic_trap_reading, 0.9).
domain_priors:theater_ratio(lycurgan_laws__demographic_trap_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__demographic_trap_reading, snare).
narrative_ontology:human_readable(lycurgan_laws__demographic_trap_reading, "Lycurgan Constitutional Immutability as Demographic Trap").
narrative_ontology:topic_domain(lycurgan_laws__demographic_trap_reading, "political_philosophy/constitutional_theory").

domain_priors:requires_active_enforcement(lycurgan_laws__demographic_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__demographic_trap_reading, '3e0b1ca8-0924-47d6-9e93-50088721ee60').
narrative_ontology:cs_kernel_codification('3e0b1ca8-0924-47d6-9e93-50088721ee60', fixed_text).
narrative_ontology:cs_authority_grounding('3e0b1ca8-0924-47d6-9e93-50088721ee60', lineage).
narrative_ontology:cs_interpretation_layer_present('3e0b1ca8-0924-47d6-9e93-50088721ee60').
narrative_ontology:cs_reading_relation('3e0b1ca8-0924-47d6-9e93-50088721ee60', lycurgan_laws__adaptive_fiction_reading, coexists_with).
narrative_ontology:cs_reading_relation('3e0b1ca8-0924-47d6-9e93-50088721ee60', lycurgan_laws__sacral_fidelity_reading, forecloses).
narrative_ontology:cs_axiom('3e0b1ca8-0924-47d6-9e93-50088721ee60', foundational, lycurgan_laws_are_human_constructs).
narrative_ontology:cs_axiom_status(lycurgan_laws_are_human_constructs, holdable).
narrative_ontology:cs_axiom_grounding('3e0b1ca8-0924-47d6-9e93-50088721ee60', lycurgan_laws_are_human_constructs, empirically_contingent).
narrative_ontology:cs_axiom('3e0b1ca8-0924-47d6-9e93-50088721ee60', foundational, constitutional_immutability_causes_demographic_collapse).
narrative_ontology:cs_axiom_status(constitutional_immutability_causes_demographic_collapse, holdable).
narrative_ontology:cs_axiom_grounding('3e0b1ca8-0924-47d6-9e93-50088721ee60', constitutional_immutability_causes_demographic_collapse, empirically_contingent).
narrative_ontology:cs_reference_frame('3e0b1ca8-0924-47d6-9e93-50088721ee60', lycurgan_founding_order).
narrative_ontology:cs_drift_state('3e0b1ca8-0924-47d6-9e93-50088721ee60', classical_sparta_decline, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('3e0b1ca8-0924-47d6-9e93-50088721ee60', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__demographic_trap_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__demographic_trap_reading, spartan_oligarchic_elite).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, spartiates).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, helots).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(lycurgan_laws__demographic_trap_reading, kings).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, ephors).
narrative_ontology:constraint_vindicates(lycurgan_laws__demographic_trap_reading, constitutional_immutability_ensures_stability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the kleros land-allocation system, the ephorate, and the interpretation of the Great Rhetra; they concentrate land and political power while the citizen body shrinks. Their exit options include leveraging wealth abroad or marrying into other Greek elites.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, spartan_oligarchic_elite, agenda_setter,
    powerful, generational, arbitrage, regional).

% Full citizens bound by the agoge, syssitia, and inalienable land tenure; they cannot sell their kleros, cannot pursue trades, and lose citizenship if they fall below the property threshold. Their identity is fused to the Lycurgan order, making exit psychologically and socially unthinkable.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, spartiates, payer,
    moderate, biographical, identity_locked, regional).

% Enslaved population of Messenia and Laconia whose forced labor sustains the Spartiate leisure class; they have no political voice, no legal path to freedom, and are subject to annual ritual war (krypteia) that suppresses resistance.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, helots, excluded,
    powerless, generational, trapped, regional).

% Annually elected magistrates who supervise kings, enforce the Rhetra, and administer the krypteia; they are both the enforcement arm of the immutable constitution and personally trapped by the one-year term and the sacred aura of the office.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, ephors, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__demographic_trap_reading, ephors, payer).

% Dual hereditary monarchy with priestly, judicial, and military roles; they derive sacral legitimacy from the Lycurgan tradition but are checked by the ephors and the gerousia, and cannot alter the laws without inviting deposition.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, kings, beneficiary,
    powerful, generational, constrained, regional).

% Analyzes the Spartan constitutional order from outside; sees the structural trap where the very immutability that was meant to prevent stasis becomes the mechanism of demographic collapse.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, modern_scholar, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Lycurgan laws coordinated a militarized society, equal land distribution (kleros), collective defense, and a rigid citizenship criterion that tied political rights to landholding and completion of the agoge.
% TRANSFER_FUNCTION: Transfers political agency and adaptive capacity from the citizen body to the oligarchic elite; transfers economic surplus from helots to Spartiates, but the rigid land-tenure system concentrates land among a shrinking elite, while the citizen body bears the cost of demographic decline and military overexposure.
% ABSENT_VOICES: Helots, perioikoi (free non-citizens), and Spartan women (who held and inherited land but had no formal political voice) are structurally excluded from the constitutional conversation; they would object to the extraction and the closure of adaptation pathways.
% DISAPPEARANCE_RATIONALE: The entire Spartiate way of life — agoge, syssitia, inalienable kleros, citizenship criteria, and the helot system — depends on the legal framework; its removal would force a reorganization of land tenure, military obligation, and social hierarchy.
% FOUNDING_PROBLEM: To create a stable, militarily effective polis that avoids civil stasis (stasis) and tyranny by fixing the laws (nomoi) and distributing land equally among citizens, thereby aligning the interests of the warrior class with the survival of the state.
% FOUNDING_PROBLEM_CORROBORATION: Ancient sources (Plutarch, Aristotle, Xenophon) attest the founding problem; modern historians (e.g., Paul Cartledge, Stephen Hodkinson) argue the system became maladaptive after the Persian Wars and especially after the earthquake of 464 BCE. No living beneficiaries exist to corroborate a living founding problem.
narrative_ontology:disappearance_verdict(lycurgan_laws__demographic_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__demographic_trap_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__demographic_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(lycurgan_laws__demographic_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__demographic_trap_reading, 0.85, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__demographic_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lycurgan_laws__demographic_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lycurgan_laws__demographic_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.3 to 0.85 as the kleros system concentrates land (by 300, <1000 Spartiate families hold all citizen land). Suppression is high (0.9) because the laws forbid any revision (Plutarch, Lycurgus 13: 'Lycurgus made the Spartans swear not to alter his laws until he returned; he never returned'). Theater ratio grows as the rhetoric of 'Lycurgus' equality' masks oligarchic concentration. Accessibility collapse is near-total: no legal mechanism for reform, no exit for identity-locked Spartiates. Resistance is moderate: occasional reform movements (Agis IV, Cleomenes III) are crushed by the ephorate.
 *
 * PERSPECTIVAL GAP:
 *   From the oligarchic elite's seat, the constraint is a rope (coordination of a stable military aristocracy). From the Spartiate payer seat, it is a snare (extraction of adaptive capacity, demographic death spiral). From the helot seat, it is a snare with no coordination benefit. The engine computes this divergence from the declared roles, power, and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: spartan_oligarchic_elite (collects land rents, political control). Victims declared: spartiates (demographic collapse, loss of citizenship), helots (forced labor). The elite have arbitrage-grade exit (wealth, marriage alliances); Spartiates are identity-locked (citizenship = self-concept); helots are trapped (geographic, legal). Directionality derivation will assign low d to elite, high d to Spartiates and helots.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (stasis/tyranny avoidance via fixed laws) is dead — the external threat environment changed, the population collapsed, and the laws became a suicide pact. The arrangement persists only because the elite benefit from the concentration of land and the sacred aura prevents revision. This is pure mandatrophy: the mandate outlived its function and the constraint persists as extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_vs_human_origin,
    'Are the Lycurgan laws a genuine divine/natural ordinance (as the sacral_fidelity_reading claims) or a human construction that became sacralized?',
    'Historical-philological analysis of the Great Rhetra''s layers; archaeological evidence for the dating of the kleros system; comparative study of Dorian constitutional traditions.',
    'If divine origin is credible, the constraint may be a mountain from the sacral seat; if human construction, the snare classification holds across seats. This is the core ambiguity that generates the kernel''s contest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_vs_human_origin, conceptual, 'Whether the constraint''s authority is natural-law or constructed.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of adaptation structural (legal prohibition, ephorate enforcement) or internalized (Spartiate identity fused to the laws, belief that change is impiety)?',
    'Analyze the fate of reformers (Agis IV, Cleomenes III): were they stopped by legal machinery or by the citizen body''s own rejection? Examine the rhetoric of the ephorate vs. the gerousia.',
    'If internalized, the effective suppression is higher than the legal measure suggests — the constraint travels with the agent after exit. This would amplify extraction for the identity-locked Spartiate seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the Spartiate psyche.').

omega_variable(
    demographic_causality,
    'Did the constitutional immutability *cause* the demographic collapse, or did exogenous shocks (earthquake, wars, helot revolts) drive the collapse while the laws merely failed to adapt?',
    'Demographic modeling of Spartiate population under the kleros inheritance rules vs. historical battle losses and helot revolts; counterfactual simulation with adjustable inheritance laws.',
    'If the laws are the primary causal driver, the snare classification is strengthened; if exogenous shocks dominate, the constraint may be a piton (atrophied coordination) rather than an active snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_causality, empirical, 'Causal weight of constitutional rigidity vs. exogenous shocks in Spartiate demographic decline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__demographic_trap_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t0, lycurgan_laws__demographic_trap_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(lycu_tr_t60, lycurgan_laws__demographic_trap_reading, theater_ratio, 60, 0.15).
narrative_ontology:measurement(lycu_tr_t120, lycurgan_laws__demographic_trap_reading, theater_ratio, 120, 0.25).
narrative_ontology:measurement(lycu_tr_t180, lycurgan_laws__demographic_trap_reading, theater_ratio, 180, 0.35).
narrative_ontology:measurement(lycu_tr_t240, lycurgan_laws__demographic_trap_reading, theater_ratio, 240, 0.38).
narrative_ontology:measurement(lycu_tr_t300, lycurgan_laws__demographic_trap_reading, theater_ratio, 300, 0.4).

% Extraction over time
narrative_ontology:measurement(lycu_be_t0, lycurgan_laws__demographic_trap_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(lycu_be_t60, lycurgan_laws__demographic_trap_reading, base_extractiveness, 60, 0.45).
narrative_ontology:measurement(lycu_be_t120, lycurgan_laws__demographic_trap_reading, base_extractiveness, 120, 0.6).
narrative_ontology:measurement(lycu_be_t180, lycurgan_laws__demographic_trap_reading, base_extractiveness, 180, 0.72).
narrative_ontology:measurement(lycu_be_t240, lycurgan_laws__demographic_trap_reading, base_extractiveness, 240, 0.8).
narrative_ontology:measurement(lycu_be_t300, lycurgan_laws__demographic_trap_reading, base_extractiveness, 300, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(lycu_su_t0, lycurgan_laws__demographic_trap_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(lycu_su_t60, lycurgan_laws__demographic_trap_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(lycu_su_t120, lycurgan_laws__demographic_trap_reading, suppression_requirement, 120, 0.7).
narrative_ontology:measurement(lycu_su_t180, lycurgan_laws__demographic_trap_reading, suppression_requirement, 180, 0.8).
narrative_ontology:measurement(lycu_su_t240, lycurgan_laws__demographic_trap_reading, suppression_requirement, 240, 0.85).
narrative_ontology:measurement(lycu_su_t300, lycurgan_laws__demographic_trap_reading, suppression_requirement, 300, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__demographic_trap_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(lycurgan_laws__demographic_trap_reading, 0.08).
narrative_ontology:affects_constraint(lycurgan_laws__demographic_trap_reading, lycurgan_laws__adaptive_fiction_reading).
narrative_ontology:affects_constraint(lycurgan_laws__demographic_trap_reading, lycurgan_laws__sacral_fidelity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the lycurgan_laws kernel. The adaptive_fiction_reading claims the immutability is a noble lie; the sacral_fidelity_reading claims it is divine law. This reading claims it is a human-made snare that caused demographic collapse. The three stories form a constraint family linked by affects_constraints. The epsilon values differ: sacral_fidelity_reading will author near-zero extraction (mountain); adaptive_fiction_reading will author moderate extraction with low suppression (tangled_rope); this reading authors high extraction and high suppression (snare).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lycurgan_laws__demographic_trap_reading, institutional, 0.6).
constraint_indexing:directionality_override(lycurgan_laws__demographic_trap_reading, powerful, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
