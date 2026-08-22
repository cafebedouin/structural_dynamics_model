% ============================================================================
% CONSTRAINT STORY: second_amendment_text__collective_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__collective_security_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: second_amendment_text__collective_security_reading
 *   human_readable: Second Amendment Collective Security Reading
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the collective-security reading of the
 *   second_amendment_text kernel: the claim that the amendment conditions the
 *   right to keep and bear arms on organized militia service and authorizes
 *   extensive state regulation of personal firearms in service of collective
 *   security. Under this reading, the state regulatory apparatus is the
 *   structural beneficiary of expanded police power, while individual gun
 *   owners constitute the constrained class subject to licensing,
 *   prohibition, and criminal penalties. The story treats the reading as a
 *   commitment-system constraint grounded in a fixed constitutional text,
 *   mediated by a federal judiciary that defers to state judgments about
 *   civic defense. It is authored as one of three sibling constraints
 *   decomposed from the colloquial label 'the Second Amendment'; the other
 *   two (individual_right_reading, originalist_civic_virtue_reading)
 *   instantiate structurally distinct claims with different directionalities
 *   and Îµ profiles.
 *
 * KEY AGENTS:
 *   - state_regulatory_apparatus: Primary beneficiary/agenda-setter (institutional/analytical) â derives expanded police power and constitutional authority to license and prohibit arms from the collective-security reading.
 *   - individual_gun_owners: Primary target (organized/constrained) â bear regulatory burden, compliance costs, and criminal penalties of the militia-conditioned framework.
 *   - federal_judiciary: Analytical observer (institutional/analytical) â stabilizes the reading through interpretive deference to legislative judgments about organized civic defense.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__collective_security_reading, 0.48).
domain_priors:suppression_score(second_amendment_text__collective_security_reading, 0.72).
domain_priors:theater_ratio(second_amendment_text__collective_security_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__collective_security_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__collective_security_reading, "Second Amendment Collective Security Reading").
narrative_ontology:topic_domain(second_amendment_text__collective_security_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_text__collective_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__collective_security_reading, 'b70fa458-1696-4c85-9931-c901d327f755').
narrative_ontology:cs_kernel_codification('b70fa458-1696-4c85-9931-c901d327f755', fixed_text).
narrative_ontology:cs_authority_grounding('b70fa458-1696-4c85-9931-c901d327f755', lineage).
narrative_ontology:cs_interpretation_layer_present('b70fa458-1696-4c85-9931-c901d327f755').
narrative_ontology:cs_reading_relation('b70fa458-1696-4c85-9931-c901d327f755', second_amendment_text__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('b70fa458-1696-4c85-9931-c901d327f755', second_amendment_text__originalist_civic_virtue_reading, coexists_with).
narrative_ontology:cs_axiom('b70fa458-1696-4c85-9931-c901d327f755', foundational, right_conditioned_on_militia_service).
narrative_ontology:cs_axiom_status(right_conditioned_on_militia_service, holdable).
narrative_ontology:cs_axiom_grounding('b70fa458-1696-4c85-9931-c901d327f755', right_conditioned_on_militia_service, conventional).
narrative_ontology:cs_axiom('b70fa458-1696-4c85-9931-c901d327f755', foundational, state_discretion_over_personal_arms).
narrative_ontology:cs_axiom_status(state_discretion_over_personal_arms, holdable).
narrative_ontology:cs_axiom_grounding('b70fa458-1696-4c85-9931-c901d327f755', state_discretion_over_personal_arms, conventional).
narrative_ontology:cs_reference_frame('b70fa458-1696-4c85-9931-c901d327f755', organized_civic_militia_framework).
narrative_ontology:cs_drift_state('b70fa458-1696-4c85-9931-c901d327f755', post_originalist_challenge, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('b70fa458-1696-4c85-9931-c901d327f755', '').
narrative_ontology:cs_kernel_id(second_amendment_text__collective_security_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, state_regulatory_apparatus).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, individual_gun_owners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers firearms licensing, permitting, and prohibitory regimes under the collective-security reading of the Second Amendment. Derives constitutional authority to regulate, restrict, or disarm categories of civilians in service of organized civic defense and public safety. Sets the legal terms under which individual gun owners may possess arms.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, state_regulatory_apparatus, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_text__collective_security_reading, state_regulatory_apparatus, beneficiary).

% Bear the compliance costs of licensing, registration, waiting periods, categorical prohibitions, and criminal penalties justified by the militia clause. Politically organized through advocacy associations but legally constrained by a doctrinal framework that subordinates personal possession to state regulatory judgment about collective security.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, individual_gun_owners, payer,
    organized, biographical, constrained, national).

% Interprets the Second Amendment as conditioning the right to keep and bear arms on militia service and collective security. Under this reading, the judiciary defers to legislative and executive judgments about what serves organized civic defense, stabilizing the constraint through precedent and constitutional review.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, federal_judiciary, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_text__collective_security_reading, state_regulatory_apparatus).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes armed civic capacity under state supervision to provide for collective defense, insurrection suppression, and public order without relying exclusively on a large standing army or professional police force.
% TRANSFER_FUNCTION: Transfers discretionary power over the possession, carriage, and use of personal arms from individual citizens to state regulatory apparatuses, and transfers compliance costs â licensing fees, waiting periods, categorical prohibitions, and criminal penalties â from individual gun owners to the administrative state.
% ABSENT_VOICES: Individual self-defense advocates and proponents of an unconditioned personal right to arms are doctrinally marginalized under this reading; their constitutional theory is treated as historically anachronistic or textually unsupported within the framework, though they remain audible in dissent.
% DISAPPEARANCE_RATIONALE: If the collective-security reading vanished overnight, state licensing and prohibitory regimes would lose their primary constitutional footing and face immediate successful challenge under an individual-right framework; the legal architecture of firearms federalism would reorganize around personal-right protections and strict scrutiny of gun laws.
% FOUNDING_PROBLEM: How to maintain local and national security in a republic skeptical of standing armies while ensuring armed force remains subordinate to civil authority through organized militia institutions.
% FOUNDING_PROBLEM_CORROBORATION: Military historians and constitutional scholars outside the state-beneficiary camp attest that the militia-as-primary-defense model is obsolete; the contemporary security problem is solved by professional military and police forces, not citizen militias, and no credible security analyst argues for a return to militia-centric defense.
narrative_ontology:disappearance_verdict(second_amendment_text__collective_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__collective_security_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__collective_security_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_text__collective_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__collective_security_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__collective_security_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_text__collective_security_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_text__collective_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) is authored at the interval end (post-Bruen), reflecting the reading's diminished but persistent capacity to extract compliance from individual gun owners in jurisdictions that still enforce militia-conditioned regulations. Suppression (0.72) is high because the reading now requires active judicial and institutional defense against the ascendant individual-right framework. Theater_ratio (0.58) reflects the increasing share of performative militia rhetoric that persists after the doctrinal core has been hollowed out by Heller and Bruen. Accessibility_collapse (0.65) captures the partial collapse of alternatives: the individual-right reading has broken through but is not universally instantiated. Resistance (0.80) is very high due to sustained political and legal mobilization by gun-rights advocates. The temporal series show extraction peaking mid-century, then declining after 2008 while suppression and theater rise â a trajectory where the coordination function (civic defense) atrophies and the constraint persists as inertial regulatory power actively defended by its beneficiary.
 *
 * PERSPECTIVAL GAP:
 *   The state_regulatory_apparatus seat computes a low directionality (beneficiary of expanded authority), likely experiencing the constraint as coordination serving collective security. The individual_gun_owners seat computes a high directionality (target of regulation), experiencing the same structure as extractive constraint. The federal_judiciary seat, as analytical observer, sees the divergence directly. The engine captures this per-seat asymmetry from the structural data without requiring a single type verdict for all seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declaration (state_regulatory_apparatus) and victim declaration (individual_gun_owners) are the structural source. The state gains discretionary power to license, prohibit, and criminalize personal arm possession; the individual loses the legal option to possess arms outside state permission. The federal judiciary mediates this transfer by interpreting the fixed text to authorize state discretion. No directionality override is needed: the structural derivation from beneficiary/victim declarations plus exit options produces the correct d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â maintaining republican security without a standing army via organized militia â is dead (corroborated by military historians outside the beneficiary set). Yet the constraint persists because the state regulatory apparatus continues to benefit from the constitutional authority it provides. This mismatch (dead founding problem + world_rearranges disappearance verdict) flags the constraint as a mandatrophy candidate. However, the high theater ratio and active suppression metrics prevent misclassification as a piton: the constraint is not merely inertial performance but is actively enforced and defended by its beneficiary. The classification as tangled_rope captures that the coordination function (collective security) is genuine but atrophied, while the extraction (state regulatory power) remains active and asymmetric.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Is this constraint one reading of the Second Amendment kernel, and does its classification change if the individual-right reading is adopted instead?',
    'Compare with sibling constraint story for individual_right_reading; the structural delta is located in the beneficiary/victim inversion (individual gun owners become beneficiaries, state regulators become constrained).',
    'Under the individual-right reading, the directionality map inverts: individual_gun_owners shift from payer to beneficiary, and the constraint likely reclassifies as rope or scaffold rather than tangled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Structural dependence of classification on which kernel reading is adopted.').

omega_variable(
    militia_clause_operative_clause_relation,
    'Does the militia clause grammatically condition the operative clause, or is it a prefatory statement of purpose that does not limit the right?',
    'Linguistic and historical analysis of 18th-century usage of ''bearing arms'' and the syntactic function of absolute or conditional clauses in the founding era.',
    'If the militia clause is purely prefatory, the collective-security reading collapses; if conditional, the individual-right reading is foreclosed. This is the precise locus of the kernel contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_clause_operative_clause_relation, conceptual, 'The grammatical relationship between the militia clause and the right to keep and bear arms.').

omega_variable(
    collective_security_efficacy,
    'Does the state regulatory apparatus under the collective-security reading actually achieve superior collective security outcomes relative to less restrictive constitutional regimes?',
    'Comparative empirical study of violent crime, militia readiness, and mass-casualty events across jurisdictions with varying regulatory intensity under different constitutional readings.',
    'If regulation does not improve collective security, the coordination function is a cover story and the constraint reclassifies toward snare; if it does, the tangled-rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_security_efficacy, empirical, 'Whether the coordination function produces the security outcomes it claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__collective_security_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa_collective_sec_tr_t0, second_amendment_text__collective_security_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sa_collective_sec_tr_t20, second_amendment_text__collective_security_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(sa_collective_sec_tr_t40, second_amendment_text__collective_security_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(sa_collective_sec_tr_t60, second_amendment_text__collective_security_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement(sa_collective_sec_tr_t75, second_amendment_text__collective_security_reading, theater_ratio, 75, 0.5).
narrative_ontology:measurement(sa_collective_sec_tr_t90, second_amendment_text__collective_security_reading, theater_ratio, 90, 0.58).

% Extraction over time
narrative_ontology:measurement(sa_collective_sec_be_t0, second_amendment_text__collective_security_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(sa_collective_sec_be_t20, second_amendment_text__collective_security_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(sa_collective_sec_be_t40, second_amendment_text__collective_security_reading, base_extractiveness, 40, 0.74).
narrative_ontology:measurement(sa_collective_sec_be_t60, second_amendment_text__collective_security_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(sa_collective_sec_be_t75, second_amendment_text__collective_security_reading, base_extractiveness, 75, 0.55).
narrative_ontology:measurement(sa_collective_sec_be_t90, second_amendment_text__collective_security_reading, base_extractiveness, 90, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(sa_collective_sec_su_t0, second_amendment_text__collective_security_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(sa_collective_sec_su_t20, second_amendment_text__collective_security_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(sa_collective_sec_su_t40, second_amendment_text__collective_security_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(sa_collective_sec_su_t60, second_amendment_text__collective_security_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(sa_collective_sec_su_t75, second_amendment_text__collective_security_reading, suppression_requirement, 75, 0.7).
narrative_ontology:measurement(sa_collective_sec_su_t90, second_amendment_text__collective_security_reading, suppression_requirement, 90, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__collective_security_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, originalist_civic_virtue_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the second_amendment_text kernel family. It decomposes the colloquial label 'the Second Amendment' into a structurally distinct claim: that the right to keep and bear arms is conditioned on organized militia service and subject to state regulatory discretion. The individual_right_reading and originalist_civic_virtue_reading instantiate different structural claims from the same kernel text, with different Îµ profiles and directionalities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
