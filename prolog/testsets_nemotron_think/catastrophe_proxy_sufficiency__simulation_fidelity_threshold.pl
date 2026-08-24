% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__simulation_fidelity_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: catastrophe_proxy_sufficiency__simulation_fidelity_threshold
 *   human_readable: Catastrophe Proxy Sufficiency via Simulation Fidelity Threshold
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   High-reliability organizations (nuclear, aviation, chemical, healthcare)
 *   face a structural problem: catastrophic events are too rare, dangerous,
 *   or ethically impermissible to serve as routine competence-maintenance
 *   exercises. The simulation fidelity threshold reading asserts that
 *   technology-mediated simulation can substitute for real catastrophe
 *   exposure PROVIDED the simulation crosses a fidelity threshold where
 *   stress, uncertainty, and decision-density match real catastrophe
 *   conditions. This threshold is not categorical — it moves as simulation
 *   technology advances. The constraint coordinates competence maintenance
 *   across the field by establishing a technology-investment pathway:
 *   organizations invest in simulation infrastructure, vendors compete on
 *   fidelity, regulators certify thresholds, and the binary sufficiency
 *   condition (meets threshold / does not meet) replaces unbounded 'more
 *   simulation is better' with a clear coordination target. Beneficiaries are
 *   simulation technology vendors (who capture the investment flow) and the
 *   organizations/regulators/training institutions that gain a shared
 *   coordinate. No victim class is declared — participants are net
 *   beneficiaries of the coordination — but an omega flags potential
 *   exclusion of resource-constrained organizations.
 *
 * KEY AGENTS:
 *   - simulation_technology_vendors: Primary beneficiary (powerful/institutional) — sells fidelity-certified simulation platforms; revenue scales with threshold adoption
 *   - high_reliability_organizations: Beneficiary (institutional) — gains a bounded, certifiable competence-maintenance pathway; bears investment cost but avoids unbounded training expenditure
 *   - safety_regulators: Agenda setter (institutional) — defines and certifies fidelity thresholds; legitimizes the coordination standard
 *   - training_institutions: Beneficiary (organized) — integrates threshold-certified simulation into curricula; gains stable demand
 *   - frontline_operators: Beneficiary (organized, constrained exit) — receives structured, high-fidelity practice; career progression tied to threshold-certified credentials
 *   - resource_constrained_operators: Excluded (powerless, trapped) — cannot meet investment threshold; may face regulatory marginalization or competence decay
 *   - affected_publics: Excluded (powerless, trapped) — bears downstream risk if competence maintenance fails; no voice in threshold-setting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.18).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.12).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, extractiveness, 0.18).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, "Catastrophe Proxy Sufficiency via Simulation Fidelity Threshold").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, "safety_engineering/organizational_learning/high_reliability_organizations").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 'c0cf898f-ed05-4536-99c7-a7b3148cb3a3').
narrative_ontology:cs_kernel_codification('c0cf898f-ed05-4536-99c7-a7b3148cb3a3', formalized).
narrative_ontology:cs_authority_grounding('c0cf898f-ed05-4536-99c7-a7b3148cb3a3', expertise).
narrative_ontology:cs_interpretation_layer_present('c0cf898f-ed05-4536-99c7-a7b3148cb3a3').
narrative_ontology:cs_reading_relation('c0cf898f-ed05-4536-99c7-a7b3148cb3a3', catastrophe_proxy_sufficiency__catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('c0cf898f-ed05-4536-99c7-a7b3148cb3a3', catastrophe_proxy_sufficiency__hybrid_degradation_reading, coexists_with).
narrative_ontology:cs_reading_relation('c0cf898f-ed05-4536-99c7-a7b3148cb3a3', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, influences).
narrative_ontology:cs_axiom('c0cf898f-ed05-4536-99c7-a7b3148cb3a3', foundational, fidelity_threshold_is_sufficient_for_competence_retention).
narrative_ontology:cs_axiom_status(fidelity_threshold_is_sufficient_for_competence_retention, holdable).
narrative_ontology:cs_axiom_grounding('c0cf898f-ed05-4536-99c7-a7b3148cb3a3', fidelity_threshold_is_sufficient_for_competence_retention, empirically_contingent).
narrative_ontology:cs_axiom('c0cf898f-ed05-4536-99c7-a7b3148cb3a3', secondary, technology_investment_coordinates_competence_maintenance).
narrative_ontology:cs_axiom_status(technology_investment_coordinates_competence_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('c0cf898f-ed05-4536-99c7-a7b3148cb3a3', technology_investment_coordinates_competence_maintenance, conventional).
narrative_ontology:cs_reference_frame('c0cf898f-ed05-4536-99c7-a7b3148cb3a3', pre_threshold_simulation_practice).
narrative_ontology:cs_drift_state('c0cf898f-ed05-4536-99c7-a7b3148cb3a3', contemporary_fidelity_standards, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c0cf898f-ed05-4536-99c7-a7b3148cb3a3', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_technology_vendors).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, high_reliability_organizations).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, safety_regulators).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, training_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, frontline_operators).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, competence_retention_requires_fidelity_threshold).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, technology_investment_coordinates_safety_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and sell high-fidelity simulation platforms certified to meet the threshold. Revenue comes from organizational subscriptions, per-seat licenses, and certification fees. They compete on fidelity metrics and lobby standards bodies to shape threshold definitions. Exit is easy — they serve multiple industries and can pivot to adjacent simulation markets.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_technology_vendors, beneficiary,
    powerful, biographical, arbitrage, global).

% Nuclear plant operators, airlines, chemical manufacturers, major hospitals. They invest in simulation infrastructure to meet the certified threshold. The investment is substantial but bounded — it replaces open-ended training expenditures and reduces regulatory uncertainty. They cannot easily exit the need for competence maintenance, but they can choose vendors and influence threshold evolution through industry associations.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, high_reliability_organizations, beneficiary,
    institutional, generational, constrained, global).

% Nuclear regulatory commissions, aviation authorities, chemical safety boards. They define the fidelity threshold criteria, certify simulation platforms, and make threshold compliance a licensing condition. They do not directly pay or collect from the constraint but shape its operation. Their exit is analytical — they observe the system's effectiveness and can revise standards.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, safety_regulators, agenda_setter,
    institutional, generational, analytical, national).

% University nuclear engineering programs, flight academies, medical simulation centers. They integrate threshold-certified simulation into curricula and professional certification pathways. They gain stable enrollment and industry partnerships. Exit is constrained — their accreditation and relevance depend on teaching to the current threshold.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, training_institutions, beneficiary,
    organized, biographical, constrained, regional).

% Control room operators, pilots, process engineers, surgical teams. They receive high-fidelity simulation training that counts toward certification and career progression. The training is genuinely valuable for skill maintenance. Their exit is constrained — leaving the profession means losing the investment in threshold-specific credentials; staying means recurring re-certification on the platform.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, frontline_operators, beneficiary,
    organized, biographical, constrained, global).

% Small research reactors, regional airlines in developing economies, municipal water treatment plants, small chemical batch producers. They face the same competence-retention problem but cannot afford threshold-certified simulation infrastructure. They rely on lower-fidelity drills, tabletop exercises, and apprenticeship. They are structurally excluded from the certified pathway and may face regulatory pressure or insurance penalties for non-compliance.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, resource_constrained_operators, excluded,
    powerless, biographical, trapped, regional).

% Communities downstream of nuclear plants, chemical facilities, flight paths. They bear the consequence if competence maintenance fails. They have no voice in threshold-setting, no access to simulation vendors, and no exit from the risk exposure. Their situation is not improved or worsened directly by the threshold — they are excluded from the coordination entirely.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, affected_publics, excluded,
    powerless, generational, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates competence maintenance across high-reliability organizations by establishing a technology-investment pathway to a certified simulation fidelity threshold, replacing unbounded 'more simulation is better' with a binary sufficiency criterion that regulators, vendors, and operators can coordinate around.
% TRANSFER_FUNCTION: Moves capital investment from high-reliability organizations to simulation technology vendors in exchange for fidelity-certified training infrastructure; moves regulatory certainty from regulators to organizations via threshold compliance; moves career progression clarity from training institutions to frontline operators via certified credentials.
% ABSENT_VOICES: Resource-constrained high-reliability organizations (small nuclear operators, regional airlines in developing economies, municipal facilities) who cannot meet the investment threshold and face regulatory marginalization. Affected publics downstream of competence failures who bear risk but have no seat in threshold-setting. Frontline operators in excluded organizations who maintain competence through non-certified pathways.
% DISAPPEARANCE_RATIONALE: If the fidelity threshold and its certification infrastructure vanished overnight, organizations would lose the shared coordinate for 'sufficient' simulation investment. Regulators would revert to prescriptive training-hour requirements or subjective competence assessments. Vendors would compete on unbounded fidelity claims rather than threshold compliance. The field would reorganize around either under-investment (competence decay) or wasteful over-investment (unbounded fidelity arms race).
% FOUNDING_PROBLEM: How to maintain operational competence for catastrophic scenarios that are too rare, expensive, or ethically impossible to practice in reality, without imposing unbounded training costs on organizations or leaving competence to chance.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear industry (IAEA safety reports), aviation (ICAO training standards), chemical process safety (CCPS guidelines), and healthcare simulation literature independently document the competence-retention problem for rare catastrophic events. The problem is attested by regulatory bodies, professional societies, and accident investigation boards — not solely by simulation vendors.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low (0.18) because the constraint's primary function is coordination — it solves a genuine collective-action problem (how to maintain competence for rare catastrophes) with minimal coercive overhead. The binary threshold reduces wasteful over-investment and provides a clear sufficiency criterion. Suppression is low (0.12) — alternatives exist (drills, tabletop exercises, apprenticeship) but are less effective; the constraint persists because it works better, not because alternatives are crushed. Theater ratio is very low (0.08) — the simulation exercises have direct functional relevance to competence. Accessibility collapse is moderate (0.35) — the threshold creates a binary gate, but organizations below it can still operate (just without the certified pathway). Resistance is low (0.15) — the field adopted the threshold because it solves a real problem. The measurement series shows extractiveness and theater declining as the threshold matures and vendor competition commoditizes baseline fidelity.
 *
 * PERSPECTIVAL GAP:
 *   From the vendor seat, the threshold is a market-creating coordination standard that rewards fidelity investment. From the resource-constrained operator seat, the same threshold may appear as a capital barrier that privileges well-resourced organizations. From the regulator seat, it is a risk-reduction tool with clear certification logic. The engine computes these seat divergences from the structural data; the authored claim (rope) reflects the coordination function as structurally dominant.
 *
 * DIRECTIONALITY LOGIC:
 *   Vendors are structural beneficiaries (d near 0.0) — they collect the technology investment flow. High-reliability organizations are near-symmetric (d ~ 0.5) — they pay investment costs but receive a bounded, certifiable competence pathway that replaces unbounded alternatives. Regulators are agenda-setters with analytical exit (d ~ 0.3) — they shape the standard but don't directly pay or collect. Frontline operators are beneficiaries with constrained exit (d ~ 0.4) — they gain practice quality but career mobility depends on threshold-certified credentials. Resource-constrained operators are excluded (not in the coordination) — their structural position is not captured by directionality because they are outside the constraint's beneficiary/payer structure; the omega flags this gap.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (competence retention for rare catastrophes) remains live — catastrophic events have not become more frequent or ethically practicable for routine training. The arrangement has not outlived its function; rather, technology advances have improved the coordination's efficiency (declining extractiveness over time). No mandatrophy is present — the constraint's justification (the transition to technology-mediated competence maintenance) is still the steady state, not a completed transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Does this reading instantiate a distinct constraint from sibling readings of the catastrophe_proxy_sufficiency kernel, or does it describe the same structural phenomenon under a different label?',
    'Compare ε values and beneficiary/victim structures across readings: if this reading''s ε (0.18) and vendor-beneficiary structure differ materially from sibling readings, they are distinct constraints per ε-invariance.',
    'If readings are not distinct, the kernel decomposition is invalid and the family should collapse to one story; if distinct, each reading''s classification stands independently.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether simulation_fidelity_threshold is a structurally distinct constraint from catastrophe_necessity_reading, hybrid_degradation_reading, and simulation_as_proxy_catastrophe_reading').

omega_variable(
    fidelity_threshold_empirical_basis,
    'Is the fidelity threshold empirically grounded in competence-retention outcomes, or is it a vendor-driven specification that creates a binary sufficiency condition without validated correspondence to real catastrophe performance?',
    'Longitudinal studies comparing organizations that meet vs. fail the threshold on subsequent catastrophe-adjacent performance; regulatory review of threshold-setting processes for vendor capture.',
    'If thresholds are vendor-captured, the rope classification masks extractive coordination; if empirically grounded, the binary sufficiency condition is a genuine coordination achievement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fidelity_threshold_empirical_basis, empirical, 'Whether the fidelity threshold represents validated competence science or vendor-influenced standard-setting').

omega_variable(
    resource_constrained_organizations_exclusion,
    'Does the binary threshold exclude resource-constrained high-reliability organizations (small nuclear operators, regional airlines, developing-nation chemical plants) from recognized competence maintenance pathways?',
    'Survey of organizations below the investment threshold: do they maintain competence through alternative pathways, or do they face regulatory marginalization?',
    'If exclusion is systematic, the rope''s coordination function has a victim class not currently declared, potentially shifting classification toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_constrained_organizations_exclusion, empirical, 'Whether the threshold creates de facto exclusion of lower-resource operators from recognized competence pathways').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cps_sft_tr_t0, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cps_sft_tr_t4, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 4, 0.12).
narrative_ontology:measurement(cps_sft_tr_t8, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 8, 0.1).
narrative_ontology:measurement(cps_sft_tr_t12, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 12, 0.09).
narrative_ontology:measurement(cps_sft_tr_t16, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 16, 0.08).
narrative_ontology:measurement(cps_sft_tr_t20, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 20, 0.08).

% Extraction over time
narrative_ontology:measurement(cps_sft_be_t0, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(cps_sft_be_t4, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 4, 0.22).
narrative_ontology:measurement(cps_sft_be_t8, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 8, 0.2).
narrative_ontology:measurement(cps_sft_be_t12, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 12, 0.18).
narrative_ontology:measurement(cps_sft_be_t16, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 16, 0.18).
narrative_ontology:measurement(cps_sft_be_t20, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 20, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, resource_allocation).
narrative_ontology:boltzmann_floor_override(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.12).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_credentialing_standards).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, regulatory_competence_assurance).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, high_reliability_training_curricula).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_proxy_sufficiency kernel. The catastrophe_necessity_reading, hybrid_degradation_reading, and simulation_as_proxy_catastrophe_reading are sibling constraints with different ε values and beneficiary structures. This reading's ε (0.18) reflects coordination via technology investment; the catastrophe_necessity_reading would have higher ε (real catastrophe exposure is costly); the simulation_as_proxy_catastrophe_reading would have lower ε (no threshold gate, simulation is simply sufficient). They are linked as a constraint family via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
