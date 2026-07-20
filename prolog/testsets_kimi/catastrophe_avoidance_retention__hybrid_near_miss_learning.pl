% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__hybrid_near_miss_learning
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_avoidance_retention__hybrid_near_miss_learning, []).

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
 *   constraint_id: catastrophe_avoidance_retention__hybrid_near_miss_learning
 *   human_readable: Distributed Near-Miss and Incident Learning for Catastrophe Avoidance
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This constraint instantiates the hybrid_near_miss_learning reading of the
 *   catastrophe_avoidance_retention kernel. The kernel asks how
 *   high-reliability fields retain competence to avoid catastrophes over
 *   time. This reading holds that competence is maintained through
 *   distributed cross-organizational learning from near-misses, foreign
 *   incidents, and high-realism drills, asserting that neither simulation
 *   alone nor actual catastrophe alone is sufficient. The constraint is an
 *   institutionalized arrangement of mandatory reporting, cross-border
 *   investigation, and safety-bulletin dissemination that binds operators
 *   into a learning network. Industries with strong cross-organizational
 *   learning (commercial aviation) exhibit sustained competence; industries
 *   without it (much of medicine) exhibit recurrent preventable failures.
 *
 * KEY AGENTS:
 *   - regulatory_safety_bodies: Primary agenda_setter (institutional/analytical) â sets reporting mandates and operates the network
 *   - incident_bearing_operators: Primary payer (powerful/constrained) â bears direct reporting costs, legal exposure, and reputational risk
 *   - frontline_practitioners: Secondary payer (moderate/identity_locked) â bears psychological and professional costs of self-reporting errors
 *   - learning_consuming_operators: Primary beneficiary (powerful/mobile) â absorbs shared safety intelligence with lower proportional cost
 *   - safety_dependent_public: Diffuse beneficiary (organized/constrained) â receives safety benefits without direct network visibility
 *   - non_participating_sectors: Excluded voice (moderate/trapped) â remains outside the norm-setting conversation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.48).
domain_priors:suppression_score(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.45).
domain_priors:theater_ratio(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, extractiveness, 0.48).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__hybrid_near_miss_learning, tangled_rope).
narrative_ontology:human_readable(catastrophe_avoidance_retention__hybrid_near_miss_learning, "Distributed Near-Miss and Incident Learning for Catastrophe Avoidance").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__hybrid_near_miss_learning, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__hybrid_near_miss_learning).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__hybrid_near_miss_learning, 'ecf3b307-795d-41df-a632-0321a7753523').
narrative_ontology:cs_kernel_codification('ecf3b307-795d-41df-a632-0321a7753523', formalized).
narrative_ontology:cs_authority_grounding('ecf3b307-795d-41df-a632-0321a7753523', expertise).
narrative_ontology:cs_interpretation_layer_present('ecf3b307-795d-41df-a632-0321a7753523').
narrative_ontology:cs_reading_relation('ecf3b307-795d-41df-a632-0321a7753523', catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, coexists_with).
narrative_ontology:cs_reading_relation('ecf3b307-795d-41df-a632-0321a7753523', catastrophe_avoidance_retention__catastrophe_as_necessary_selector, coexists_with).
narrative_ontology:cs_axiom('ecf3b307-795d-41df-a632-0321a7753523', foundational, distributed_incident_learning_necessary).
narrative_ontology:cs_axiom_status(distributed_incident_learning_necessary, holdable).
narrative_ontology:cs_axiom_grounding('ecf3b307-795d-41df-a632-0321a7753523', distributed_incident_learning_necessary, empirically_contingent).
narrative_ontology:cs_axiom('ecf3b307-795d-41df-a632-0321a7753523', foundational, just_culture_reporting_legitimacy).
narrative_ontology:cs_axiom_status(just_culture_reporting_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('ecf3b307-795d-41df-a632-0321a7753523', just_culture_reporting_legitimacy, conventional).
narrative_ontology:cs_reference_frame('ecf3b307-795d-41df-a632-0321a7753523', distributed_learning_competence_framework).
narrative_ontology:cs_drift_state('ecf3b307-795d-41df-a632-0321a7753523', contemporary_litigious_accountability_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ecf3b307-795d-41df-a632-0321a7753523', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, safety_dependent_public).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, learning_consuming_operators).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, incident_bearing_operators).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_practitioners).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__hybrid_near_miss_learning, high_reliability_organization_theory).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__hybrid_near_miss_learning, just_culture_reporting_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets mandatory incident reporting standards, operates cross-border investigation bureaus, enforces just-culture frameworks, and disseminates safety recommendations. Their authority derives from the statutory mandate to prevent catastrophes and from the empirical track record of networked safety improvement.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, regulatory_safety_bodies, agenda_setter,
    institutional, generational, analytical, global).

% Airlines, hospitals, or industrial plants that experience near-misses or incidents and must report them under regulatory mandate. They bear direct costs of internal investigation, legal exposure, reputational risk, and operational disruption while their data enters the shared learning pool.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, incident_bearing_operators, payer,
    powerful, biographical, constrained, global).

% Operators that disproportionately absorb safety bulletins, foreign incident reports, and drill curricula without proportionally contributing high-cost incident data. They improve their safety posture and reduce failure rates at lower direct cost than the reporting organizations.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, learning_consuming_operators, beneficiary,
    powerful, biographical, mobile, global).

% Pilots, surgeons, controllers, and technicians who must self-report errors and near-misses. They face psychological burden, potential disciplinary action when just-culture policies fail, and career jeopardy when disclosures are weaponized in litigation or internal review.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_practitioners, payer,
    moderate, immediate, identity_locked, national).

% Passengers, patients, and communities who rely on high-reliability systems. They benefit from reduced catastrophic failure rates but lack visibility into whether the learning network is functional or has become performative compliance.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, safety_dependent_public, beneficiary,
    organized, biographical, constrained, global).

% Organizations in sectors without strong cross-organizational reporting mandates, such as fragmented areas of general healthcare or private surgical centers. Their incidents remain local and unshared, and they are not present in the norm-setting conversations that establish reporting standards.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, non_participating_sectors, excluded,
    moderate, biographical, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pool dispersed, low-frequency near-miss and incident data across organizations and national boundaries so that rare failure modes are detected, analyzed, and communicated before they compound into catastrophe, supplementing organizational memory that single institutions cannot build alone.
% TRANSFER_FUNCTION: Moves detailed incident data and analytic capacity from the operators and practitioners who experience failures to regulators, peer organizations, and the broader field; moves compliance, legal, psychological, and reputational costs onto the reporting operators and frontline practitioners.
% ABSENT_VOICES: Organizations in litigation-heavy or fragmented sectors without reporting mandates, and frontline practitioners who fear retaliation despite just-culture policies, are structurally underrepresented. They would argue that reporting costs and liability exposure exceed the individual organization's benefit from participation, and that the current network privileges sectors with strong regulatory density.
% DISAPPEARANCE_RATIONALE: If the distributed learning network vanished, operators would revert to siloed local learning; recurring near-miss patterns would go undetected across organizations; catastrophe rates would rise as the same failures repeated without cross-institutional detection; regulatory oversight would lose its empirical foundation and shift toward reactive punishment; high-reliability fields would degrade toward the fragmented safety posture of less networked domains.
% FOUNDING_PROBLEM: Catastrophic failures in complex socio-technical systems are too rare for any single organization to learn from its own experience alone, while simulations cannot replicate the full contextual messiness of real near-misses; competence atrophy between catastrophes was causing predictable but unlearned-from failures.
% FOUNDING_PROBLEM_CORROBORATION: Safety scientists such as Weick, Sutcliffe, Dekker, and Reason corroborate the problem from outside the regulatory beneficiary set, based on ethnographic and empirical study of high-reliability organizations. However, operators in litigation-heavy jurisdictions and sectors without reporting mandates attest the problem is addressed differently in their contexts, or that the proposed cure creates worse secondary effects; no universal corroboration exists across all sectors.
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__hybrid_near_miss_learning, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__hybrid_near_miss_learning, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__hybrid_near_miss_learning, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_avoidance_retention__hybrid_near_miss_learning, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__hybrid_near_miss_learning_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_avoidance_retention__hybrid_near_miss_learning, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_avoidance_retention__hybrid_near_miss_learning_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) is moderate because the arrangement genuinely prevents catastrophes but imposes substantial asymmetric compliance and liability costs on reporting operators. Suppression (0.45) reflects the active enforcement needed to overcome organizational secrecy and liability fears. Theater ratio (0.22) is low-to-moderate: the core learning function is real, but bureaucratic compliance layers produce some performative reporting. Accessibility collapse (0.60) is moderate-high because once the networked learning framework is accepted, purely local or simulation-only alternatives lose legitimacy. Resistance (0.38) reflects persistent operator and practitioner reluctance to report fully under litigation pressure. The measurement series show slow drift upward in extraction and enforcement over the interval as regulatory frameworks matured and legal exposure expanded.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (incident-bearing operators, frontline practitioners) and the beneficiary seats (learning-consuming operators, safety-dependent public) should compute to different types. From the payer perspective, the constraint is an imposed burden with concentrated costs and legal hazard; from the beneficiary perspective, it is protective coordination. The regulatory seat experiences the structure as a functional governance mechanism. The engine computes this divergence from the structural role and exit declarations rather than from any authored type consensus.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory safety bodies sit near the symmetric-to-beneficiary end: they administer the constraint and gain authority and data, but do not capture a concentrated monetary rent. Incident-bearing operators and frontline practitioners sit near the target end because they bear direct extraction (compliance costs, legal risk, psychological burden) and have constrained or identity-locked exit. Learning-consuming operators and the safety-dependent public sit near the beneficiary end because they receive safety intelligence and risk reduction without proportional cost contribution. The excluded non-participating sectors are structurally outside the directionality derivation for this constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the Tangled Rope classification, this constraint could be misread as a Rope by emphasizing only the aviation safety record, or as a Snare by emphasizing only the compliance burden on hospitals or airlines. The Tangled Rope classification is warranted because the same structure that coordinates safety across organizations simultaneously extracts concentrated costs from the operators who must report incidents. The active enforcement requirementâregulatory mandates, just-culture policies, and cross-border agreementsâdistinguishes it from pure voluntary coordination and indicates that the coordination would collapse without sustained institutional pressure to overcome secrecy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sector_specificity_validity,
    'Is distributed near-miss learning structurally viable only in sectors with specific enabling conditions (high regulatory density, oligopolistic operators, strong unionization), or is it a general catastrophe-avoidance mechanism applicable across high-risk domains?',
    'Comparative cross-sector analysis measuring the relationship between reporting-network density and safety outcomes, controlling for regulatory environment and market structure.',
    'If viability is sector-specific, the constraint''s epsilon and coordination claims are locally valid but not generalizable, and the kernel decomposes into sector-specific sub-constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sector_specificity_validity, empirical, 'Whether the constraint generalizes across sectors or is aviation-specific').

omega_variable(
    reporting_cost_extraction_threshold,
    'Do the compliance, legal, and reputational costs imposed on incident-bearing operators exceed the value of the safety intelligence they contribute, creating net extraction rather than net coordination for the reporting party?',
    'Economic analysis of reporting costs (litigation, investigation, reputational loss) versus the actuarial value of prevented failures attributable to shared data from that party.',
    'If costs exceed contribution value, the constraint is more extractive than coordination for the payer seat, and the effective chi for reporters rises toward snare-like territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reporting_cost_extraction_threshold, empirical, 'Whether reporting costs exceed coordination benefits for payers').

omega_variable(
    simulation_substitution_boundary,
    'At what fidelity or scope threshold does high-realism simulation become a genuine functional substitute for distributed real-incident learning, and has that threshold been reached for any major failure-mode class?',
    'Empirical comparison of competence retention and error rates in organizations that rely primarily on simulation versus those in the distributed learning network, across matched failure domains.',
    'If simulation substitution is achieved, the hybrid reading''s necessity claim is weakened and the sibling simulation_as_proxy_catastrophe reading gains empirical support.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_substitution_boundary, empirical, 'Whether simulation can substitute for real incident learning').

omega_variable(
    reading_frame_contest,
    'This constraint is one reading of a contested kernel; does evidence from sectors with weak incident-sharing but strong safety records (if any exist) foreclose the hybrid_near_miss_learning reading in favor of simulation or catastrophe readings?',
    'Identification of high-reliability sectors that achieve low catastrophe rates without either strong distributed learning or frequent catastrophes; if found, they would challenge this reading''s necessity claim.',
    'If no such sectors exist, the hybrid reading is corroborated; if they exist, the kernel requires further decomposition by sector type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_frame_contest, conceptual, 'Whether the sibling readings structurally challenge this reading''s validity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t6, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 6, 0.12).
narrative_ontology:measurement(cata_tr_t12, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 12, 0.14).
narrative_ontology:measurement(cata_tr_t18, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 18, 0.17).
narrative_ontology:measurement(cata_tr_t24, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 24, 0.2).
narrative_ontology:measurement(cata_tr_t30, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 30, 0.22).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cata_be_t6, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 6, 0.35).
narrative_ontology:measurement(cata_be_t12, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(cata_be_t18, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 18, 0.42).
narrative_ontology:measurement(cata_be_t24, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 24, 0.45).
narrative_ontology:measurement(cata_be_t30, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 30, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(cata_su_t6, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 6, 0.35).
narrative_ontology:measurement(cata_su_t12, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 12, 0.4).
narrative_ontology:measurement(cata_su_t18, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 18, 0.45).
narrative_ontology:measurement(cata_su_t24, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 24, 0.48).
narrative_ontology:measurement(cata_su_t30, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__hybrid_near_miss_learning, enforcement_mechanism).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_avoidance_retention__catastrophe_as_necessary_selector).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the catastrophe_avoidance_retention kernel. The kernel decomposes the question of how high-reliability systems maintain catastrophe-avoidance competence into three mutually exclusive empirical-normative framings. The readings share a regulatory domain (safety-critical socio-technical systems) but differ in beneficiary structure, victim set, and epsilon profile. They form a constraint family linked by network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
