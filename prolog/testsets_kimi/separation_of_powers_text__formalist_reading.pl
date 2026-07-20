% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__formalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__formalist_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: separation_of_powers_text__formalist_reading
 *   human_readable: Formalist Non-Delegation Doctrine
 *   domain: constitutional law / political theory / administrative law
 *
 * SUMMARY:
 *   This constraint instantiates the formalist_reading of the
 *   separation_of_powers_text kernel. It treats the constitutional separation
 *   of powers as establishing strict, impermeable boundaries between
 *   legislative and executive authority, with the corollary that Congress
 *   cannot delegate legislative authority to administrative agencies. Under
 *   this reading, broad swathes of the modern administrative state are
 *   unconstitutional. The sibling readingsâfunctionalist_reading
 *   (permitting flexible delegation under intelligible principles) and
 *   unitary_executive_reading (consolidating executive power in the
 *   President)âare structurally distinct constraints linked through the
 *   same constitutional text. Administrative agencies enter the victim set
 *   because their delegated authority is declared void; regulated industries
 *   and the formalist judiciary occupy beneficiary positions.
 *
 * KEY AGENTS:
 *   - formalist_judiciary: Agenda-setter and doctrinal beneficiary (institutional/analytical) â sets the interpretive rule and collects authority from its enforcement.
 *   - administrative_agencies: Primary target and payer (institutional/trapped) â bear the loss of regulatory capacity and cannot exit the constraint.
 *   - regulated_industries: Material beneficiary (powerful/mobile) â accrue reduced compliance burdens and deregulatory gains.
 *   - functionalist_jurists: Excluded analytical voice (organized/analytical) â would advance an alternative reading but are structurally outside prevailing doctrine.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__formalist_reading, 0.85).
domain_priors:suppression_score(separation_of_powers_text__formalist_reading, 0.88).
domain_priors:theater_ratio(separation_of_powers_text__formalist_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__formalist_reading, tangled_rope).
narrative_ontology:human_readable(separation_of_powers_text__formalist_reading, "Formalist Non-Delegation Doctrine").
narrative_ontology:topic_domain(separation_of_powers_text__formalist_reading, "constitutional law / political theory / administrative law").

domain_priors:requires_active_enforcement(separation_of_powers_text__formalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__formalist_reading, '32d4cac3-e465-4c39-8bde-aefc9e1d2c56').
narrative_ontology:cs_kernel_codification('32d4cac3-e465-4c39-8bde-aefc9e1d2c56', fixed_text).
narrative_ontology:cs_authority_grounding('32d4cac3-e465-4c39-8bde-aefc9e1d2c56', lineage).
narrative_ontology:cs_interpretation_layer_present('32d4cac3-e465-4c39-8bde-aefc9e1d2c56').
narrative_ontology:cs_reading_relation('32d4cac3-e465-4c39-8bde-aefc9e1d2c56', separation_of_powers_text__functionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('32d4cac3-e465-4c39-8bde-aefc9e1d2c56', separation_of_powers_text__unitary_executive_reading, coexists_with).
narrative_ontology:cs_axiom('32d4cac3-e465-4c39-8bde-aefc9e1d2c56', foundational, legislative_power_nondelegable).
narrative_ontology:cs_axiom_status(legislative_power_nondelegable, holdable).
narrative_ontology:cs_axiom_grounding('32d4cac3-e465-4c39-8bde-aefc9e1d2c56', legislative_power_nondelegable, conventional).
narrative_ontology:cs_axiom('32d4cac3-e465-4c39-8bde-aefc9e1d2c56', foundational, impermeable_branch_boundaries).
narrative_ontology:cs_axiom_status(impermeable_branch_boundaries, holdable).
narrative_ontology:cs_axiom_grounding('32d4cac3-e465-4c39-8bde-aefc9e1d2c56', impermeable_branch_boundaries, conventional).
narrative_ontology:cs_reference_frame('32d4cac3-e465-4c39-8bde-aefc9e1d2c56', strict_branch_separation).
narrative_ontology:cs_drift_state('32d4cac3-e465-4c39-8bde-aefc9e1d2c56', modern_administrative_state, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('32d4cac3-e465-4c39-8bde-aefc9e1d2c56', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__formalist_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, formalist_judiciary).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, regulated_industries).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, administrative_agencies).
narrative_ontology:constraint_vindicates(separation_of_powers_text__formalist_reading, strict_separation_doctrine).
narrative_ontology:constraint_vindicates(separation_of_powers_text__formalist_reading, nondelegation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Constitution to prohibit congressional delegation of legislative authority to administrative agencies. Strikes down regulatory statutes and agency rules on non-delegation grounds. Derives institutional authority and prestige from being the final arbiter of constitutional meaning under this reading.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, formalist_judiciary, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__formalist_reading, formalist_judiciary, beneficiary).

% Depend on congressional delegation to issue rules and enforce regulations. Under the formalist reading, their authority is unconstitutional unless Congress specifies every detail. They cannot exit the constraint because their existence and capacity are predicated on the delegated authority now forbidden.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, administrative_agencies, payer,
    institutional, biographical, trapped, national).

% Face fewer binding regulations when agency rulemaking is struck down or paralyzed. Benefit from the dismantling of the administrative state's capacity to set and enforce compliance regimes. Have no structural reason to exit a constraint that reduces their regulatory burden.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, regulated_industries, beneficiary,
    powerful, biographical, mobile, national).

% Hold that flexible delegation under intelligible principles is constitutionally necessary and operationally inevitable for modern governance. Their reading is structurally excluded from prevailing doctrine under the formalist regime; they would object but lack adjudicative authority to change the constraint.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, functionalist_jurists, excluded,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(separation_of_powers_text__formalist_reading, regulated_industries).
narrative_ontology:fixing_cost_class(separation_of_powers_text__formalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserving democratic accountability by ensuring that legislative powerâthe power to make rules binding on private conductâis exercised only by the Congress elected by the people, preventing its migration to unelected administrative bodies.
% TRANSFER_FUNCTION: Transfers regulatory capacity and policy implementation authority away from administrative agencies and back to Congress (or into judicial invalidation), while transferring reduced regulatory obligation and compliance costs to regulated industries.
% ABSENT_VOICES: Functionalist jurists who view intelligible principle delegation as constitutionally necessary and operationally inevitable; agency experts with technical knowledge; legislators who depend on delegated authority to govern complex modern society.
% DISAPPEARANCE_RATIONALE: If the formalist reading vanished, agencies would resume broad delegated rulemaking authority, the regulatory state would reconstitute its existing architecture, and industries would face reinvigorated compliance obligationsâthe entire landscape of modern administrative governance would shift.
% FOUNDING_PROBLEM: The risk that legislative power would be exercised by unelected administrative officials, eroding democratic accountability and constitutional separation of powers.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional originalists and formalist jurists attest to the founding problem as an ongoing structural threat. Administrative law scholars and comparative governance experts dispute that strict non-delegation is required to solve it, arguing that modern democratic legitimacy can be preserved through oversight and procedural controls rather than absolute prohibition.
narrative_ontology:disappearance_verdict(separation_of_powers_text__formalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__formalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__formalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(separation_of_powers_text__formalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__formalist_reading, 0.85, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__formalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(separation_of_powers_text__formalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(separation_of_powers_text__formalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.85 because the formalist reading invalidates the statutory basis of vast administrative programs, extracting governance capacity from agencies. Suppression is 0.88 because the constraint persists only through active judicial enforcement striking down delegations and suppressing the functionalist alternative. Theater_ratio rises to 0.62 because an increasing share of formalist jurisprudence performs constitutional fidelity while functionally delivering deregulatory outcomes disconnected from the original coordination rationale. Accessibility_collapse is 0.78 because once the formalist reading is accepted, the legal alternative of intelligible principle delegation collapses as a live doctrinal option. Resistance is 0.72 because agencies, functionalist scholars, and legislators mount sustained substantive opposition. The claim/metric divergence is intentional: the constraint is claimed as tangled_rope (genuine coordination of democratic accountability coupled with asymmetric extraction) while the metrics describe heavily extractive, actively enforced operation.
 *
 * PERSPECTIVAL GAP:
 *   The formalist judiciary perceives the constraint as preserving constitutional order and democratic legitimacy; from this seat the metrics would read as low extraction and necessary enforcement. Administrative agencies perceive judicial tyranny and a crippling of statutory missions; from this seat the metrics read as near-total extraction. Regulated industries perceive a welcome return to legislative specificity; from this seat the constraint appears beneficial and legitimate. The engine computes this divergence from the structural data rather than the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The formalist_judiciary sits near the beneficiary end (d low) because it controls the interpretive apparatus and collects authority; its analytical exit means it could abandon the reading without personal cost. Administrative_agencies sit near the full-target end (d high) because they are identity-locked to delegated authority and structurally trappedâtheir institutional existence is the extraction target. Regulated_industries sit near the beneficiary end (d low) because they are mobile and receive the constraint's diffuse deregulatory gains.
 *
 * MANDATROPHY ANALYSIS:
 *   The formalist reading was built to solve the founding problem of unaccountable bureaucratic power. Its mandate is contested because the modern administrative state may require expert delegation to function. If the founding problem is dead or transformedâif democratic accountability is now preserved through oversight, notice-and-comment, and judicial review rather than absolute non-delegationâthen the constraint persists as mandatrophy. The rising theater_ratio and extractiveness over the interval suggest the reading has drifted from its coordination justification toward a performance of originalism that extracts regulatory capacity. The R5 mismatch (founding_problem_status contested, disappearance_verdict world_rearranges) flags this for downstream piton/snare analysis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formalist_workability_in_modern_state,
    'Can the formalist non-delegation doctrine be implemented without catastrophic collapse of existing regulatory capacity, or is its practical effect indistinguishable from a deregulatory snare?',
    'Historical or comparative analysis of governance systems with strict non-delegation; measurement of regulatory output before and after formalist judicial interventions.',
    'If strict non-delegation is unworkable in practice, the coordination claim is cover and the engine would compute a snare classification; if workable, the tangled rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formalist_workability_in_modern_state, empirical, 'Whether the formalist reading can coordinate modern governance or only extracts agency capacity.').

omega_variable(
    primary_beneficiary_ambiguity,
    'Does the formalist reading materially benefit regulated industries by paralyzing agencies, or does it benefit the citizenry by preserving legislative accountability?',
    'Trace regulatory burden and legislative output under formalist judicial regimes; compare agency rulemaking volume and industry compliance costs.',
    'If regulated industries capture the gains, gain_flow is concentrated and the constraint leans snare; if accountability is diffuse and citizens benefit, the extraction is less asymmetric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(primary_beneficiary_ambiguity, conceptual, 'Whether the reading''s extraction accrues to industries or to democratic legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__formalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sop_formalist_tr_t0, separation_of_powers_text__formalist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sop_formalist_tr_t5, separation_of_powers_text__formalist_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(sop_formalist_tr_t10, separation_of_powers_text__formalist_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(sop_formalist_tr_t15, separation_of_powers_text__formalist_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(sop_formalist_tr_t20, separation_of_powers_text__formalist_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(sop_formalist_tr_t25, separation_of_powers_text__formalist_reading, theater_ratio, 25, 0.5).
narrative_ontology:measurement(sop_formalist_tr_t30, separation_of_powers_text__formalist_reading, theater_ratio, 30, 0.55).
narrative_ontology:measurement(sop_formalist_tr_t35, separation_of_powers_text__formalist_reading, theater_ratio, 35, 0.6).
narrative_ontology:measurement(sop_formalist_tr_t40, separation_of_powers_text__formalist_reading, theater_ratio, 40, 0.62).

% Extraction over time
narrative_ontology:measurement(sop_formalist_be_t0, separation_of_powers_text__formalist_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(sop_formalist_be_t5, separation_of_powers_text__formalist_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(sop_formalist_be_t10, separation_of_powers_text__formalist_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(sop_formalist_be_t15, separation_of_powers_text__formalist_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(sop_formalist_be_t20, separation_of_powers_text__formalist_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(sop_formalist_be_t25, separation_of_powers_text__formalist_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(sop_formalist_be_t30, separation_of_powers_text__formalist_reading, base_extractiveness, 30, 0.75).
narrative_ontology:measurement(sop_formalist_be_t35, separation_of_powers_text__formalist_reading, base_extractiveness, 35, 0.82).
narrative_ontology:measurement(sop_formalist_be_t40, separation_of_powers_text__formalist_reading, base_extractiveness, 40, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(sop_formalist_su_t0, separation_of_powers_text__formalist_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(sop_formalist_su_t5, separation_of_powers_text__formalist_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(sop_formalist_su_t10, separation_of_powers_text__formalist_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(sop_formalist_su_t15, separation_of_powers_text__formalist_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(sop_formalist_su_t20, separation_of_powers_text__formalist_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(sop_formalist_su_t25, separation_of_powers_text__formalist_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(sop_formalist_su_t30, separation_of_powers_text__formalist_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(sop_formalist_su_t35, separation_of_powers_text__formalist_reading, suppression_requirement, 35, 0.84).
narrative_ontology:measurement(sop_formalist_su_t40, separation_of_powers_text__formalist_reading, suppression_requirement, 40, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__formalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, functionalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, unitary_executive_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the separation_of_powers_text kernel. It is decompositionally linked to functionalist_reading and unitary_executive_reading as alternative structurally distinct claims arising from the same constitutional text. Each reading carries a distinct epsilon, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
