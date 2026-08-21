% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__humanitarian_ceiling_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__humanitarian_ceiling_reading, []).

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
 *   constraint_id: geneva_conventions_1949__humanitarian_ceiling_reading
 *   human_readable: Geneva Conventions (Humanitarian Ceiling Reading)
 *   domain: international_humanitarian_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'humanitarian ceiling' reading of the 1949
 *   Geneva Conventions, which posits that the conventions establish absolute,
 *   non-derogable humanitarian minimums for state conduct in armed conflict.
 *   These minimums apply regardless of adversary compliance, reciprocity, or
 *   perceived security threats. This reading emphasizes expansive protections
 *   for civilians and detainees, and places an asymmetric burden on state
 *   militaries to uphold these standards. It is a contested reading,
 *   particularly in asymmetric conflicts, but remains a foundational
 *   principle for many international legal scholars and humanitarian
 *   organizations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__humanitarian_ceiling_reading, 0.65).
domain_priors:suppression_score(geneva_conventions_1949__humanitarian_ceiling_reading, 0.78).
domain_priors:theater_ratio(geneva_conventions_1949__humanitarian_ceiling_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__humanitarian_ceiling_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__humanitarian_ceiling_reading, "Geneva Conventions (Humanitarian Ceiling Reading)").
narrative_ontology:topic_domain(geneva_conventions_1949__humanitarian_ceiling_reading, "international_humanitarian_law/political_philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__humanitarian_ceiling_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__humanitarian_ceiling_reading, 'e1f4ab50-845f-4e2a-a823-549e22d2ab1a').
narrative_ontology:cs_kernel_codification('e1f4ab50-845f-4e2a-a823-549e22d2ab1a', fixed_text).
narrative_ontology:cs_authority_grounding('e1f4ab50-845f-4e2a-a823-549e22d2ab1a', lineage).
narrative_ontology:cs_interpretation_layer_present('e1f4ab50-845f-4e2a-a823-549e22d2ab1a').
narrative_ontology:cs_reading_relation('e1f4ab50-845f-4e2a-a823-549e22d2ab1a', geneva_conventions_1949__conditional_reciprocity_reading, coexists_with).
narrative_ontology:cs_reading_relation('e1f4ab50-845f-4e2a-a823-549e22d2ab1a', geneva_conventions_1949__security_maximization_reading, coexists_with).
narrative_ontology:cs_axiom('e1f4ab50-845f-4e2a-a823-549e22d2ab1a', foundational, human_dignity_absolute_in_conflict).
narrative_ontology:cs_axiom_status(human_dignity_absolute_in_conflict, holdable).
narrative_ontology:cs_axiom_grounding('e1f4ab50-845f-4e2a-a823-549e22d2ab1a', human_dignity_absolute_in_conflict, deontological).
narrative_ontology:cs_axiom('e1f4ab50-845f-4e2a-a823-549e22d2ab1a', foundational, jus_in_bello_independent_of_jus_ad_bellum).
narrative_ontology:cs_axiom_status(jus_in_bello_independent_of_jus_ad_bellum, holdable).
narrative_ontology:cs_axiom_grounding('e1f4ab50-845f-4e2a-a823-549e22d2ab1a', jus_in_bello_independent_of_jus_ad_bellum, conventional).
narrative_ontology:cs_reference_frame('e1f4ab50-845f-4e2a-a823-549e22d2ab1a', post_wwii_humanitarian_consensus).
narrative_ontology:cs_drift_state('e1f4ab50-845f-4e2a-a823-549e22d2ab1a', contemporary_asymmetric_conflict_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('e1f4ab50-845f-4e2a-a823-549e22d2ab1a', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, civilian_populations).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, detained_combatants).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, medical_personnel).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, state_militaries).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, political_leaders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, irregular_combatants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the primary burden of compliance, often at perceived operational disadvantage. They must adhere to strict rules of engagement, protect civilians, and treat detainees humanely, even when adversaries do not reciprocate. Exit means violating international law and facing potential prosecution.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, state_militaries, payer,
    institutional, biographical, constrained, global).

% Are constrained in their strategic options by the absolute humanitarian minimums. They face domestic and international pressure to uphold the conventions, even when it complicates military objectives or prolongs conflicts. Exit means risking international condemnation and legal action.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, political_leaders, payer,
    institutional, generational, constrained, national).

% Receive protection from direct attack, indiscriminate violence, and disproportionate harm. Their lives are preserved and suffering mitigated by the conventions' strictures, regardless of the belligerents' conduct. They have no exit from conflict zones.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, civilian_populations, beneficiary,
    powerless, immediate, trapped, local).

% Are guaranteed humane treatment, access to medical care, and basic legal protections, even if they do not qualify for full Prisoner of War status. Their vulnerability is mitigated by the absolute nature of these protections. They are physically trapped.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, detained_combatants, beneficiary,
    powerless, immediate, trapped, local).

% Are protected in their duties to care for the wounded and sick, regardless of their affiliation. Their neutrality and ability to operate are safeguarded by the conventions, though this protection is often violated in practice. Exit means abandoning their humanitarian mission.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, medical_personnel, beneficiary,
    moderate, biographical, constrained, local).

% Interpret and enforce the conventions, holding individuals and states accountable for violations. They are the primary institutional mechanism for ensuring compliance with the humanitarian ceiling reading, even against powerful states.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, international_courts_and_tribunals, agenda_setter,
    institutional, generational, analytical, global).

% Benefit from basic humanitarian protections (e.g., prohibition of torture, summary execution) even if they do not meet the criteria for POW status, which would grant them more extensive rights. Their lack of formal status does not negate their fundamental human dignity under this reading.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, irregular_combatants, beneficiary,
    powerless, immediate, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal baseline for humane conduct in armed conflict, providing a common framework for all parties to minimize suffering and protect non-combatants, regardless of the nature of the conflict or the identity of the belligerents.
% TRANSFER_FUNCTION: Transfers the burden of restraint and compliance from vulnerable populations to state militaries and political leaders, who must absorb operational costs and strategic limitations to uphold humanitarian principles.
% ABSENT_VOICES: Those who advocate for 'total war' or 'unrestricted warfare' doctrines are excluded from the legitimate discourse, as their positions are fundamentally incompatible with the absolute humanitarian minimums established by this reading. Their arguments for unfettered military action are suppressed by the normative force of the conventions.
% DISAPPEARANCE_RATIONALE: If the humanitarian ceiling reading of the Geneva Conventions vanished, state militaries would face fewer constraints on their conduct, leading to a rapid escalation of civilian casualties, widespread abuse of detainees, and a collapse of protections for medical personnel. The nature of armed conflict would fundamentally shift towards greater brutality and disregard for human life.
% FOUNDING_PROBLEM: The horrors of World War II, particularly the widespread targeting of civilians and the inhumane treatment of prisoners, demonstrated the urgent need for universally binding rules to limit the barbarity of warfare.
% FOUNDING_PROBLEM_CORROBORATION: International humanitarian organizations, human rights advocates, and numerous academic scholars consistently attest that the founding problem of limiting suffering in war remains live, citing ongoing conflicts where violations occur but would be far worse without the conventions. Independent legal analyses from outside state military establishments corroborate the continued relevance of these protections.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__humanitarian_ceiling_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__humanitarian_ceiling_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(geneva_conventions_1949__humanitarian_ceiling_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__humanitarian_ceiling_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because this reading imposes significant operational and strategic costs on state militaries, requiring them to forgo certain tactics or accept higher risks to protect non-combatants. Suppression (0.78) is also high, as this reading actively suppresses security-maximization rationales and arguments for reciprocal non-compliance. The theater ratio (0.20) is relatively low, indicating that while there are performative aspects to compliance, the core function of establishing and enforcing humanitarian minimums is genuinely pursued by international bodies and many states. The claimed type is 'tangled_rope' because it genuinely coordinates humanitarian action while extracting significant costs from state actors, requiring active enforcement to maintain.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state militaries and political leaders (payers), this reading can feel like a snare, imposing unilateral costs and hindering effective security operations. From the perspective of civilian populations and humanitarian organizations (beneficiaries), it is a vital rope, providing essential protections. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Civilian populations, detained combatants, and medical personnel are clear beneficiaries, receiving protections regardless of their status or the conflict's nature. State militaries and political leaders are the primary payers, bearing the costs of compliance and strategic limitations. International courts and tribunals act as agenda-setters, interpreting and enforcing this reading. Irregular combatants are also beneficiaries of basic protections, even if they don't achieve full POW status, which is a key tenet of this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_cost_vs_humanitarian_gain,
    'What is the actual operational cost (in terms of military effectiveness or casualties) incurred by state militaries due to strict adherence to the humanitarian ceiling, versus the demonstrable humanitarian gains?',
    'Empirical studies comparing conflict outcomes and humanitarian impacts in contexts with varying degrees of adherence to this reading, controlling for other variables. This would require detailed, often classified, military data and independent humanitarian assessments.',
    'If costs are disproportionately high with minimal humanitarian gain, it could weaken the political will for this reading, pushing towards conditional reciprocity. If gains are substantial for moderate costs, it strengthens the reading''s normative force.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(operational_cost_vs_humanitarian_gain, empirical, 'Quantifying the trade-off between military effectiveness and humanitarian protection under this reading.').

omega_variable(
    enforcement_capacity_vs_state_power,
    'To what extent can international courts and tribunals (agenda-setters) genuinely enforce this reading against powerful states that perceive it as an impediment to their security interests?',
    'Analysis of historical cases where powerful states have been challenged or sanctioned for violations, assessing the effectiveness of enforcement mechanisms and the political costs incurred by non-compliant states.',
    'If enforcement capacity is weak against powerful states, the reading''s ''tangled rope'' nature might degrade towards a ''piton'' (theatrical compliance) or a ''snare'' (enforced only against weaker actors). Strong enforcement capacity reinforces its ''tangled rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_vs_state_power, empirical, 'The actual power of international enforcement mechanisms relative to state sovereignty.').

omega_variable(
    normative_force_vs_realpolitik,
    'Is the humanitarian ceiling reading primarily a normative ideal that guides some state behavior, or does it genuinely constrain state action even when it conflicts with perceived ''realpolitik'' security interests?',
    'Content analysis of state military doctrines, political speeches, and diplomatic communications, combined with observed behavior in conflict, to discern whether humanitarian considerations are integrated into decision-making or merely invoked rhetorically.',
    'If it''s primarily an ideal, its effective extractiveness and suppression might be lower than measured, as states find ways to circumvent it without explicit violation. If it genuinely constrains, the current metrics are accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normative_force_vs_realpolitik, conceptual, 'The extent to which the reading''s normative force translates into actual behavioral constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__humanitarian_ceiling_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1949, 0.1).
narrative_ontology:measurement(gene_tr_t1969, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1969, 0.12).
narrative_ontology:measurement(gene_tr_t1989, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1989, 0.15).
narrative_ontology:measurement(gene_tr_t2004, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2004, 0.18).
narrative_ontology:measurement(gene_tr_t2014, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2014, 0.19).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1949, 0.5).
narrative_ontology:measurement(gene_be_t1969, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1969, 0.55).
narrative_ontology:measurement(gene_be_t1989, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1989, 0.6).
narrative_ontology:measurement(gene_be_t2004, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2004, 0.63).
narrative_ontology:measurement(gene_be_t2014, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2014, 0.64).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1949, 0.6).
narrative_ontology:measurement(gene_su_t1969, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1969, 0.65).
narrative_ontology:measurement(gene_su_t1989, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1989, 0.7).
narrative_ontology:measurement(gene_su_t2004, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2004, 0.75).
narrative_ontology:measurement(gene_su_t2014, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2014, 0.77).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__humanitarian_ceiling_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
