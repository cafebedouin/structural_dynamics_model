% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__simulation_fidelity_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_proxy_sufficiency__simulation_fidelity_threshold, []).

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
 *   constraint_id: catastrophe_proxy_sufficiency__simulation_fidelity_threshold
 *   human_readable: Simulation Fidelity Threshold for Competence Retention
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   High-reliability organizations (nuclear, aviation, healthcare,
 *   petrochemical) require operators to maintain competence for catastrophic
 *   scenarios that occur once per generation or less. The simulation fidelity
 *   threshold reading asserts that competence retention depends on simulation
 *   crossing a technology-dependent fidelity threshold where
 *   stress/uncertainty matches real catastrophe — sufficiency is not
 *   categorical (simulation vs. no simulation) but graded by fidelity. This
 *   reading coordinates massive technology investment across the safety
 *   ecosystem while creating a binary regulatory gate: below threshold =
 *   insufficient, above threshold = sufficient. The engine computes per-seat
 *   types from the structural data; this reading claims rope (genuine
 *   coordination with minimal extraction) while metrics show moderate
 *   extraction (0.28) and rising theater (0.22) — the divergence is the
 *   measurement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.28).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.15).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, extractiveness, 0.28).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, "Simulation Fidelity Threshold for Competence Retention").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, "safety_engineering/organizational_learning/high_reliability_organizations").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, '81b018ff-dee0-43b9-9376-29d6a44f6343').
narrative_ontology:cs_kernel_codification('81b018ff-dee0-43b9-9376-29d6a44f6343', formalized).
narrative_ontology:cs_authority_grounding('81b018ff-dee0-43b9-9376-29d6a44f6343', lineage).
narrative_ontology:cs_interpretation_layer_present('81b018ff-dee0-43b9-9376-29d6a44f6343').
narrative_ontology:cs_reading_relation('81b018ff-dee0-43b9-9376-29d6a44f6343', catastrophe_proxy_sufficiency__catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('81b018ff-dee0-43b9-9376-29d6a44f6343', catastrophe_proxy_sufficiency__hybrid_degradation_reading, coexists_with).
narrative_ontology:cs_reading_relation('81b018ff-dee0-43b9-9376-29d6a44f6343', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, influences).
narrative_ontology:cs_axiom('81b018ff-dee0-43b9-9376-29d6a44f6343', foundational, fidelity_threshold_sufficiency).
narrative_ontology:cs_axiom_status(fidelity_threshold_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('81b018ff-dee0-43b9-9376-29d6a44f6343', fidelity_threshold_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('81b018ff-dee0-43b9-9376-29d6a44f6343', foundational, technology_dependent_competence_model).
narrative_ontology:cs_axiom_status(technology_dependent_competence_model, holdable).
narrative_ontology:cs_axiom_grounding('81b018ff-dee0-43b9-9376-29d6a44f6343', technology_dependent_competence_model, instrumental).
narrative_ontology:cs_reference_frame('81b018ff-dee0-43b9-9376-29d6a44f6343', post_tmi_regulatory_framework).
narrative_ontology:cs_drift_state('81b018ff-dee0-43b9-9376-29d6a44f6343', contemporary_fidelity_escalation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('81b018ff-dee0-43b9-9376-29d6a44f6343', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_technology_vendors).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, high_reliability_organizations).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, regulatory_bodies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, high_reliability_organizations).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, frontline_operators).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, technology_dependent_sufficiency).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, fidelity_threshold_competence_model).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and sell high-fidelity simulation platforms to nuclear, aviation, healthcare, and petrochemical operators. Revenue scales with fidelity requirements; the threshold narrative creates a moving target that justifies continuous procurement cycles. Can pivot to adjacent markets (training, digital twins) if regulation shifts.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_technology_vendors, beneficiary,
    organized, biographical, mobile, global).

% Operate nuclear plants, air traffic systems, surgical suites, chemical facilities. Mandate simulation-based recertification for operators. Bear procurement, maintenance, and opportunity costs of simulation programs. Cannot exit the requirement without losing license to operate; can choose vendors and fidelity tiers.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, high_reliability_organizations, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, high_reliability_organizations, payer).

% Control room operators, pilots, surgeons, process engineers. Spend career hours in simulators; recertification depends on passing threshold scenarios. Professional identity fused with competence demonstration in simulated catastrophe. Exit means leaving the profession; internalized belief that simulation fidelity equals real readiness.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, frontline_operators, payer,
    moderate, biographical, identity_locked, local).

% NRC, FAA, FDA, OSHA, IAEA. Set simulation fidelity standards for licensing and recertification. Gain regulatory legitimacy and inspection leverage from quantifiable thresholds. Capture risk: vendors and regulated entities co-shape standards through advisory committees.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, regulatory_bodies, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, regulatory_bodies, beneficiary).

% Academic and industry researchers studying transfer of training, stress inoculation, and fidelity-effectiveness curves. Publish meta-analyses showing diminishing returns above certain fidelity levels. No direct stake in procurement; career incentives align with nuance, not threshold binaries.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_fidelity_researchers, observer,
    analytical, generational, analytical, global).

% Communities downstream of nuclear/chemical facilities, airline passengers, surgical patients. Bear catastrophic risk if competence fails. No voice in fidelity threshold setting; told simulation sufficiency is proven. Exit impossible — cannot choose different safety regimes.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, public_stakeholders, excluded,
    powerless, generational, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates competence maintenance across distributed high-reliability organizations by establishing a technology-investment pathway: instead of waiting for rare catastrophes to test readiness, organizations invest in simulation platforms that promise measurable, repeatable stress exposure. Solves the coordination problem of 'how do we stay ready for events that happen once per generation?'
% TRANSFER_FUNCTION: Moves capital from high-reliability organizations (budget for simulation procurement, maintenance, operator time) to simulation technology vendors (revenue, R&D funding). Moves career progression and license-to-operate from frontline operators to regulatory bodies via threshold-gated recertification. Moves legitimacy from vague 'experience' claims to quantifiable fidelity metrics.
% ABSENT_VOICES: Frontline operators (identity-locked, cannot object without professional suicide) and public stakeholders (trapped, no access to standard-setting) are structurally excluded. Simulation fidelity researchers occasionally dissent in literature but their nuance is filtered out by regulatory threshold binaries. Would-be low-fidelity training providers are excluded by the threshold itself.
% DISAPPEARANCE_RATIONALE: If the fidelity threshold requirement vanished, HROs would revert to heterogeneous, experience-based competence models (apprenticeship, incident review, tabletop exercises). Vendors would lose the regulatory moat protecting high-end simulation markets. Regulators would lose quantifiable inspection criteria. Operators would face career uncertainty without standardized recertification. The safety ecosystem would reorganize around non-simulation coordination mechanisms.
% FOUNDING_PROBLEM: After Three Mile Island, Chernobyl, and early aviation hull losses, the industry recognized that operator competence degraded during long catastrophe-free intervals. Live catastrophic events were too rare and too costly to serve as training events. A substitute was needed that could deliver equivalent stress/uncertainty exposure on demand.
% FOUNDING_PROBLEM_CORROBORATION: Founding problem attested by Kemeny Commission (TMI), IAEA INSAG reports, and FAA Human Factors reviews — all outside simulation vendor interests. Simulation fidelity researchers (e.g., Salas et al. meta-analyses, Hays et al. transfer-of-training reviews) corroborate that the founding problem is real but contested whether threshold fidelity solves it or creates a proxy trap. Vendors and regulators cite the same founding documents to justify current thresholds.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency__simulation_fidelity_threshold_tests).
:- end_tests(catastrophe_proxy_sufficiency__simulation_fidelity_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness 0.28 reflects vendor revenue capture from threshold-driven procurement cycles, but the coordination function is real: without simulation thresholds, HROs would face uncoordinated, experience-only competence decay. Suppression 0.15 is low — alternatives (tabletop exercises, incident review, apprenticeship) exist but are not standardized or regulatorily recognized. Theater ratio 0.22 captures growing share of simulation activity that performs compliance (running threshold scenarios) rather than building genuine stress-response capacity. Accessibility collapse 0.35: alternatives persist but are marginalized by the threshold binary. Resistance 0.42: operators and researchers push back on fidelity sufficiency claims, but regulatory capture and vendor lobbying dampen dissent.
 *
 * PERSPECTIVAL GAP:
 *   From vendor/HRO/regulator seats, the constraint appears as rope: a coordination mechanism that solves the 'rare catastrophe' problem through technology investment. From frontline operator and public seats, the same structure operates as extraction: operators pay with career hours and identity lock-in for a proxy whose sufficiency is contested; public bears residual risk. The engine computes this divergence from the structural data — the claimed rope type is the vendor/HRO/regulator perspective, not the universal truth.
 *
 * DIRECTIONALITY LOGIC:
 *   Simulation vendors are structural beneficiaries (d near 0.0): they collect recurring revenue from threshold-mandated procurement, control the fidelity roadmap, and face mobile exit. HROs are agenda_setters who also pay (dual role, d ~0.4): they set requirements but bear costs and cannot exit the regulatory mandate. Frontline operators are payers with identity_locked exit (d ~0.85): professional identity fused to simulation performance, cannot leave without career death. Regulators are dual agenda_setter/beneficiary (d ~0.2): gain inspection leverage and legitimacy, but face analytical exit. Public stakeholders are excluded/trapped (d ~0.9): bear catastrophic risk, zero voice. Researchers are analytical observers (d ~0.5): symmetric costs/benefits.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (competence decay during catastrophe-free intervals) remains live — catastrophes are still rare. But the solution (fidelity threshold) may have outlived its justification if diminishing returns research is correct: above ~70% fidelity, additional investment yields negligible transfer gains. The mandate persists because the threshold creates a regulatable binary, not because the coordination problem requires it. This is mandatrophy in the narrow sense: the arrangement continues because it is administrable, not because it is optimal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fidelity_threshold_empirical_basis,
    'Does empirical evidence support a sharp fidelity threshold for competence transfer, or is the transfer curve continuous with diminishing returns?',
    'Meta-analysis of transfer-of-training studies across domains (nuclear, aviation, healthcare) with controlled fidelity manipulations, measuring stress-response and decision-quality in real vs. simulated events.',
    'If continuous with diminishing returns, the threshold binary is a regulatory artifact extracting vendor rents; if sharp threshold exists, the coordination function is genuinely served by the binary gate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fidelity_threshold_empirical_basis, empirical, 'Whether the threshold itself is empirically grounded or a regulatory convenience.').

omega_variable(
    stress_uncertainty_equivalence,
    'Can simulation ever generate stress/uncertainty equivalent to real catastrophe, or is the ontological gap (no actual consequences) irreducible?',
    'Longitudinal studies comparing physiological and cognitive stress markers in real emergencies vs. highest-fidelity simulations; neuroimaging of decision-making under genuine vs. simulated mortal threat.',
    'If irreducible gap exists, the threshold reading''s core premise (sufficiency via fidelity) is false; the constraint is extractive theater. If gap is bridgeable, the reading''s coordination claim holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stress_uncertainty_equivalence, conceptual, 'Ontological status of simulated stress/uncertainty vs. real catastrophe.').

omega_variable(
    kernel_reading_relations,
    'How do the four readings of catastrophe_proxy_sufficiency structurally relate: does simulation_fidelity_threshold foreclose, coexist with, or influence each sibling?',
    'Institutional mapping: which organizations hold which readings? Regulatory standards documents: which readings are codified? Funding flows: which readings attract procurement budget?',
    'If this reading forecloses catastrophe_necessity_reading, the kernel is polarized. If it coexists_with hybrid_degradation_reading, both can be institutionally live. If it influences simulation_as_proxy_catastrophe_reading by raising the fidelity bar, it creates upstream pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relations, conceptual, 'Structural relationships between this reading and its three siblings in the catastrophe_proxy_sufficiency kernel.').

omega_variable(
    vendor_capture_of_standard_setting,
    'To what extent do simulation vendors co-determine the fidelity thresholds that mandate their own products?',
    'Trace advisory committee memberships, standards body working groups, and revolving-door employment between vendors, regulators, and HROs over the interval.',
    'High capture would reclassify extraction upward and shift claimed type toward tangled_rope; low capture supports genuine coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vendor_capture_of_standard_setting, empirical, 'Degree of regulatory capture in simulation fidelity standard-setting.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 1979, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t1979, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 1979, 0.08).
narrative_ontology:measurement(cata_tr_t1986, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 1986, 0.12).
narrative_ontology:measurement(cata_tr_t1995, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 1995, 0.16).
narrative_ontology:measurement(cata_tr_t2005, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 2005, 0.19).
narrative_ontology:measurement(cata_tr_t2015, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 2015, 0.21).
narrative_ontology:measurement(cata_tr_t2025, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(cata_be_t1979, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 1979, 0.12).
narrative_ontology:measurement(cata_be_t1986, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 1986, 0.18).
narrative_ontology:measurement(cata_be_t1995, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 1995, 0.22).
narrative_ontology:measurement(cata_be_t2005, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 2005, 0.25).
narrative_ontology:measurement(cata_be_t2015, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 2015, 0.27).
narrative_ontology:measurement(cata_be_t2025, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 2025, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t1979, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 1979, 0.05).
narrative_ontology:measurement(cata_su_t1986, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 1986, 0.08).
narrative_ontology:measurement(cata_su_t1995, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 1995, 0.11).
narrative_ontology:measurement(cata_su_t2005, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 2005, 0.13).
narrative_ontology:measurement(cata_su_t2015, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 2015, 0.14).
narrative_ontology:measurement(cata_su_t2025, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 2025, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, resource_allocation).
narrative_ontology:boltzmann_floor_override(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.12).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, catastrophe_proxy_sufficiency__catastrophe_necessity_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, catastrophe_proxy_sufficiency__hybrid_degradation_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the catastrophe_proxy_sufficiency kernel. The kernel decomposes along two axes: (1) whether simulation can ever be sufficient (categorical vs. graded), and (2) whether sufficiency requires a fidelity threshold or is asymptotic. This reading = graded + threshold. catastrophe_necessity_reading = categorical no. hybrid_degradation_reading = graded no-threshold (asymptotic degradation). simulation_as_proxy_catastrophe_reading = categorical yes (asymptotic sufficiency). All four linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, moderate, 0.85).
constraint_indexing:directionality_override(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
