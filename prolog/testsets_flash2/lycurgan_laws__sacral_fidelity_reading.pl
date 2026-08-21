% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__sacral_fidelity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__sacral_fidelity_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: lycurgan_laws__sacral_fidelity_reading
 *   human_readable: Lycurgan Laws: Sacral Fidelity Reading
 *   domain: political_philosophy/constitutional_theory/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents the Lycurgan laws as understood by the
 *   Spartans themselves, and by those who uphold the sacral fidelity reading:
 *   a divinely ordained, unchangeable constitution requiring absolute
 *   adherence. From this perspective, the laws are a 'mountain' – an
 *   irreducible, natural (or super-natural) limit on human action, whose
 *   persistence is independent of human enforcement. Spartan decline is
 *   attributed to external pressures or citizen failure to adhere, never to
 *   the laws' design. This is one reading of the 'lycurgan_laws' kernel,
 *   emphasizing its immutability and sacred status.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__sacral_fidelity_reading, 0.05).
domain_priors:suppression_score(lycurgan_laws__sacral_fidelity_reading, 0.95).
domain_priors:theater_ratio(lycurgan_laws__sacral_fidelity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__sacral_fidelity_reading, mountain).
narrative_ontology:human_readable(lycurgan_laws__sacral_fidelity_reading, "Lycurgan Laws: Sacral Fidelity Reading").
narrative_ontology:topic_domain(lycurgan_laws__sacral_fidelity_reading, "political_philosophy/constitutional_theory/commitment_systems").

domain_priors:emerges_naturally(lycurgan_laws__sacral_fidelity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__sacral_fidelity_reading, '4f2d843b-19e0-4204-a7c5-a7791aaff60c').
narrative_ontology:cs_kernel_codification('4f2d843b-19e0-4204-a7c5-a7791aaff60c', fixed_text).
narrative_ontology:cs_authority_grounding('4f2d843b-19e0-4204-a7c5-a7791aaff60c', lineage).
narrative_ontology:cs_interpretation_layer_present('4f2d843b-19e0-4204-a7c5-a7791aaff60c').
narrative_ontology:cs_reading_relation('4f2d843b-19e0-4204-a7c5-a7791aaff60c', lycurgan_laws__demographic_trap_reading, forecloses).
narrative_ontology:cs_reading_relation('4f2d843b-19e0-4204-a7c5-a7791aaff60c', lycurgan_laws__adaptive_fiction_reading, forecloses).
narrative_ontology:cs_axiom('4f2d843b-19e0-4204-a7c5-a7791aaff60c', foundational, laws_divinely_ordained).
narrative_ontology:cs_axiom_status(laws_divinely_ordained, holdable).
narrative_ontology:cs_axiom_grounding('4f2d843b-19e0-4204-a7c5-a7791aaff60c', laws_divinely_ordained, theological).
narrative_ontology:cs_axiom('4f2d843b-19e0-4204-a7c5-a7791aaff60c', foundational, immutability_ensures_virtue_and_stability).
narrative_ontology:cs_axiom_status(immutability_ensures_virtue_and_stability, holdable).
narrative_ontology:cs_axiom_grounding('4f2d843b-19e0-4204-a7c5-a7791aaff60c', immutability_ensures_virtue_and_stability, deontological).
narrative_ontology:cs_reference_frame('4f2d843b-19e0-4204-a7c5-a7791aaff60c', lycurgan_divine_order).
narrative_ontology:cs_drift_state('4f2d843b-19e0-4204-a7c5-a7791aaff60c', spartan_decline_era, gap(repudiation_pressure, minor, false)).
narrative_ontology:cs_created_at('4f2d843b-19e0-4204-a7c5-a7791aaff60c', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__sacral_fidelity_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, spartan_citizens).
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, gerousia).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, kings).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bound by the laws, they benefit from the stability and perceived divine favor, believing their adherence ensures the polis's strength and virtue. Their identity is fused with the Lycurgan system, making exit unthinkable.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, spartan_citizens, beneficiary,
    moderate, generational, identity_locked, local).

% The council of elders, responsible for interpreting and upholding the laws. They benefit from the authority derived from the laws' divine origin and immutability, reinforcing their own power by enforcing strict adherence.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, gerousia, agenda_setter,
    institutional, generational, constrained, local).

% The dual kings, whose authority is also grounded in the Lycurgan system. They benefit from the stability and legitimacy provided by the laws, even as they are bound by them. Their power is derived from, and constrained by, the sacral order.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, kings, beneficiary,
    powerful, biographical, constrained, local).

% The annually elected overseers, tasked with ensuring strict adherence to the laws, even by the kings. Their power is absolute in enforcing the divine ordinance, making them key beneficiaries of the sacral fidelity reading.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, ephors, agenda_setter,
    institutional, biographical, constrained, local).

% Historians and political theorists who analyze the Lycurgan system from a detached perspective, often contrasting the Spartan self-perception with empirical outcomes.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, external_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, divinely sanctioned social and political order, ensuring absolute unity and discipline among citizens, and providing a clear framework for governance and military life.
% TRANSFER_FUNCTION: Transfers absolute obedience and loyalty from citizens to the laws and the state, in exchange for social stability, military prowess, and a sense of collective identity and divine favor.
% ABSENT_VOICES: Any voices advocating for legal reform, adaptation to changing circumstances, or questioning the divine origin of the laws are suppressed by the system's foundational premise of immutability and sacrality. These voices would be seen as impious or seditious.
% DISAPPEARANCE_RATIONALE: If the belief in the sacral, unchangeable nature of the Lycurgan laws vanished, the entire Spartan social and political structure would collapse. The authority of the Gerousia and Ephors would evaporate, citizen identity would fragment, and the polis would be forced to invent new forms of governance or dissolve.
% FOUNDING_PROBLEM: To establish a perfectly ordered, militarily superior, and morally virtuous polis, free from internal strife and external corruption, by grounding its constitution in divine will.
% FOUNDING_PROBLEM_CORROBORATION: Spartan tradition and the pronouncements of the Gerousia and Ephors consistently affirmed the laws' divine origin and their ongoing necessity for the polis's survival. External historians like Plutarch, while not endorsing the divine origin, documented the Spartan belief in it and its foundational role.
narrative_ontology:disappearance_verdict(lycurgan_laws__sacral_fidelity_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__sacral_fidelity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__sacral_fidelity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(lycurgan_laws__sacral_fidelity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__sacral_fidelity_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__sacral_fidelity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, ExtMetricName, E),
    domain_priors:suppression_score(lycurgan_laws__sacral_fidelity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(lycurgan_laws__sacral_fidelity_reading),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(lycurgan_laws__sacral_fidelity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is near zero because, from this reading's perspective, the laws are not extracting from anyone but rather providing the necessary framework for a virtuous life. Suppression is near maximal (0.95) because any deviation or questioning of the laws is met with severe social and political penalties, reflecting their divine and unchangeable nature. Accessibility collapse is high (0.9) as no alternatives are considered legitimate. Resistance is minimal (0.05) because the system is deeply internalized and enforced. Theater ratio is low (0.1) as the laws are genuinely believed to be functional and sacred, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   From this reading, there is no significant perspectival gap among those who adhere to the sacral fidelity. All perceive the laws as a beneficial, unchangeable foundation. The gap emerges when this reading is contrasted with external or critical analyses (e.g., the 'demographic_trap_reading' or 'adaptive_fiction_reading'), which would classify the constraint very differently.
 *
 * DIRECTIONALITY LOGIC:
 *   Spartan citizens and the Gerousia are beneficiaries, as they believe the laws serve their collective good and provide a stable, virtuous society. Their identity is deeply intertwined with the Lycurgan system, making 'identity_locked' a key exit option. The laws are seen as subsidizing their way of life, not extracting from it. There are no 'victims' in this reading, as all are seen as willing participants in a divinely sanctioned order.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_origin_vs_human_construct,
    'Are the Lycurgan laws truly of divine origin, or are they a human construct presented as divine to enforce social cohesion and immutability?',
    'No empirical resolution possible for divine origin. Resolution depends on philosophical or theological commitments, or historical analysis of the political utility of such claims.',
    'If a human construct, the ''emerges_naturally'' claim would be false, reclassifying the constraint from a Mountain to a Snare or Tangled Rope, as its persistence would depend on active suppression and a coordination story masking extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_origin_vs_human_construct, conceptual, 'Ambiguity regarding the true origin of the laws and their naturalness.').

omega_variable(
    immutability_as_virtue_vs_brittleness,
    'Is the absolute immutability of the Lycurgan laws a virtue that ensures stability, or a structural brittleness that prevents necessary adaptation?',
    'Comparative historical analysis of other polities with rigid vs. adaptive constitutions, and counterfactual analysis of Spartan history under a revisable constitution.',
    'If immutability is a virtue, the Mountain classification holds. If it''s a brittleness, the constraint would be reclassified as a Snare or Piton, as its persistence would be due to inertia or active suppression despite its functional failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immutability_as_virtue_vs_brittleness, empirical, 'Whether the laws'' unchangeable nature is a strength or a weakness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__sacral_fidelity_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t0, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(lycu_tr_t50, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(lycu_tr_t100, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 100, 0.1).
narrative_ontology:measurement(lycu_tr_t150, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 150, 0.1).
narrative_ontology:measurement(lycu_tr_t200, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 200, 0.1).

% Extraction over time
narrative_ontology:measurement(lycu_be_t0, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(lycu_be_t50, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 50, 0.05).
narrative_ontology:measurement(lycu_be_t100, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 100, 0.05).
narrative_ontology:measurement(lycu_be_t150, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 150, 0.05).
narrative_ontology:measurement(lycu_be_t200, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 200, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(lycu_su_t0, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 0, 0.95).
narrative_ontology:measurement(lycu_su_t50, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 50, 0.95).
narrative_ontology:measurement(lycu_su_t100, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 100, 0.95).
narrative_ontology:measurement(lycu_su_t150, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 150, 0.95).
narrative_ontology:measurement(lycu_su_t200, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 200, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
