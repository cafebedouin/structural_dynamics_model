% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__actuarial_risk_acceptance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rogers_commission_findings__actuarial_risk_acceptance, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: rogers_commission_findings__actuarial_risk_acceptance
 *   human_readable: Rogers Commission Findings: Actuarial Risk Acceptance
 *   domain: organizational_safety/technology_governance/regulatory_compliance
 *
 * SUMMARY:
 *   This constraint represents the 'actuarial risk acceptance' reading of the
 *   Rogers Commission findings, which mandated that flight operations are
 *   acceptable if failure probabilities are documented and accepted by
 *   informed decision-makers. This shifts the safety paradigm from absolute
 *   engineering thresholds to a quantified, managed risk approach. The
 *   constraint is claimed as a Tangled Rope because it provides a
 *   coordination function (allowing complex operations to proceed) but also
 *   involves asymmetric extraction, primarily from traditional engineering
 *   safety norms and teams, benefiting mission planners and program managers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__actuarial_risk_acceptance, 0.65).
domain_priors:suppression_score(rogers_commission_findings__actuarial_risk_acceptance, 0.7).
domain_priors:theater_ratio(rogers_commission_findings__actuarial_risk_acceptance, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, extractiveness, 0.65).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__actuarial_risk_acceptance, tangled_rope).
narrative_ontology:human_readable(rogers_commission_findings__actuarial_risk_acceptance, "Rogers Commission Findings: Actuarial Risk Acceptance").
narrative_ontology:topic_domain(rogers_commission_findings__actuarial_risk_acceptance, "organizational_safety/technology_governance/regulatory_compliance").

domain_priors:requires_active_enforcement(rogers_commission_findings__actuarial_risk_acceptance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__actuarial_risk_acceptance, '39e4e0b1-4dfa-495d-8f69-690d1440e0c5').
narrative_ontology:cs_kernel_codification('39e4e0b1-4dfa-495d-8f69-690d1440e0c5', formalized).
narrative_ontology:cs_authority_grounding('39e4e0b1-4dfa-495d-8f69-690d1440e0c5', lineage).
narrative_ontology:cs_interpretation_layer_present('39e4e0b1-4dfa-495d-8f69-690d1440e0c5').
narrative_ontology:cs_reading_relation('39e4e0b1-4dfa-495d-8f69-690d1440e0c5', rogers_commission_findings__engineering_absolute_threshold, influences).
narrative_ontology:cs_reading_relation('39e4e0b1-4dfa-495d-8f69-690d1440e0c5', rogers_commission_findings__management_compliance_narrative, coexists_with).
narrative_ontology:cs_axiom('39e4e0b1-4dfa-495d-8f69-690d1440e0c5', foundational, quantified_risk_is_manageable).
narrative_ontology:cs_axiom_status(quantified_risk_is_manageable, holdable).
narrative_ontology:cs_axiom_grounding('39e4e0b1-4dfa-495d-8f69-690d1440e0c5', quantified_risk_is_manageable, empirically_contingent).
narrative_ontology:cs_axiom('39e4e0b1-4dfa-495d-8f69-690d1440e0c5', foundational, informed_acceptance_legitimizes_residual_risk).
narrative_ontology:cs_axiom_status(informed_acceptance_legitimizes_residual_risk, holdable).
narrative_ontology:cs_axiom_grounding('39e4e0b1-4dfa-495d-8f69-690d1440e0c5', informed_acceptance_legitimizes_residual_risk, conventional).
narrative_ontology:cs_reference_frame('39e4e0b1-4dfa-495d-8f69-690d1440e0c5', post_challenger_risk_management_paradigm).
narrative_ontology:cs_drift_state('39e4e0b1-4dfa-495d-8f69-690d1440e0c5', contemporary, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('39e4e0b1-4dfa-495d-8f69-690d1440e0c5', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__actuarial_risk_acceptance, mission_planners).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__actuarial_risk_acceptance, program_managers).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, categorical_safety_norms).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, engineering_teams).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the ability to proceed with missions by quantifying and accepting risks, rather than being halted by absolute safety thresholds. This allows them to meet operational schedules and strategic objectives, but requires them to formally document and justify risk acceptance.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, mission_planners, beneficiary,
    powerful, immediate, constrained, national).

% Are responsible for implementing the risk quantification and acceptance process. They gain flexibility in decision-making but bear the burden of ensuring documentation and securing 'informed decision-maker' acceptance. They are incentivized to keep programs moving.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, program_managers, agenda_setter,
    institutional, biographical, constrained, national).

% Bear the cost of shifting from an absolute safety paradigm to one of quantified risk. They must now provide probabilistic assessments and justify 'acceptable' failure rates, which can conflict with their professional ethos of preventing all foreseeable failures. Their identity is tied to technical integrity.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, engineering_teams, payer,
    moderate, biographical, identity_locked, local).

% Represent the prior, more absolute safety standards that are now being 'paid down' or diluted by the actuarial approach. They are not an agent but are structurally impacted as their authority diminishes in favor of quantified risk acceptance.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, categorical_safety_norms, payer,
    powerless, generational, trapped, universal).
narrative_ontology:stakeholder_non_agent(rogers_commission_findings__actuarial_risk_acceptance, categorical_safety_norms).

% Are the individuals who must formally accept the documented failure probabilities. They hold significant power in authorizing missions but are constrained by the need to be 'informed' and to justify their acceptance, bearing the ultimate accountability for risk outcomes.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, informed_decision_makers, agenda_setter,
    institutional, immediate, constrained, national).

% Are largely excluded from the 'informed decision-maker' circle. They would likely object to the acceptance of any non-zero failure probability for human spaceflight, but their input is channeled through political processes rather than direct participation in risk acceptance.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, public_and_media, excluded,
    organized, immediate, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates complex, high-stakes technical programs by providing a framework for evaluating and accepting residual risks, allowing operations to proceed where absolute safety cannot be guaranteed.
% TRANSFER_FUNCTION: Transfers the burden of absolute safety guarantees from engineering design to a process of probabilistic quantification and formal acceptance by management, shifting accountability for residual risk.
% ABSENT_VOICES: The general public and media are largely absent from the 'informed decision-maker' process, and would likely advocate for more stringent, less probabilistic safety standards. Their exclusion allows for a more technocratic risk acceptance.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, mission planning would either halt due to inability to guarantee absolute safety, or proceed without formal risk acceptance, leading to a chaotic and unaccountable decision environment. The entire framework for high-stakes technical operations would need to be re-established.
% FOUNDING_PROBLEM: The Challenger disaster revealed a failure to adequately address known technical risks, leading to catastrophic outcomes despite prior warnings. The problem was how to manage and make decisions about complex systems with inherent, irreducible risks.
% FOUNDING_PROBLEM_CORROBORATION: Engineering professional bodies and independent safety review boards corroborate that managing irreducible risk in complex systems remains a live problem. While the specific O-ring issue was resolved, the broader challenge of risk acceptance persists, as evidenced by ongoing debates in aerospace and nuclear safety.
narrative_ontology:disappearance_verdict(rogers_commission_findings__actuarial_risk_acceptance, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__actuarial_risk_acceptance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__actuarial_risk_acceptance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(rogers_commission_findings__actuarial_risk_acceptance, 'none', 1).
narrative_ontology:epsilon_provenance(rogers_commission_findings__actuarial_risk_acceptance, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rogers_commission_findings__actuarial_risk_acceptance_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rogers_commission_findings__actuarial_risk_acceptance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rogers_commission_findings__actuarial_risk_acceptance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because the shift to actuarial risk allows for the 'acceptance' of non-zero failure probabilities, which can be seen as extracting from the prior, more stringent safety culture. Suppression (0.70) is significant as it requires active enforcement to ensure all parties adhere to the new risk quantification and acceptance protocols, suppressing alternative, more conservative safety approaches. Theater ratio (0.20) is relatively low, as the process is genuinely intended to manage risk, though there's a risk of it becoming performative over time. The metrics show a gradual increase in extractiveness and suppression as the actuarial approach becomes more entrenched.
 *
 * PERSPECTIVAL GAP:
 *   Mission planners and program managers experience this as a necessary coordination mechanism, enabling progress in high-risk endeavors. Engineering teams and those upholding categorical safety norms experience it as an extractive shift, forcing them to compromise on absolute safety in favor of quantified, accepted risk. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Mission planners and program managers are beneficiaries as they gain flexibility and the ability to proceed. Engineering teams and categorical safety norms are victims, as they bear the costs of this shift. Informed decision-makers are agenda-setters, holding the power to accept or reject risks, but also bearing accountability.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling coordination as pure extraction by acknowledging the genuine need to manage irreducible risks in complex systems. However, it also prevents mislabeling extraction as pure coordination by highlighting the shift in accountability and the potential for diluting safety standards under the guise of 'informed acceptance.' The 'contested' status of the founding problem indicates that while the original problem of managing risk is live, the solution's fairness and long-term efficacy are debated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    risk_quantification_accuracy,
    'How accurate and comprehensive are the probabilistic risk assessments, and do they truly capture all potential failure modes and their consequences?',
    'Independent, long-term validation studies comparing predicted failure rates with actual incident data, and expert review of assessment methodologies.',
    'If assessments are consistently inaccurate or incomplete, the ''informed decision-making'' becomes illusory, increasing the effective extractiveness and potentially reclassifying the constraint towards a Snare, as it would be extracting safety under false pretenses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(risk_quantification_accuracy, empirical, 'Uncertainty regarding the fidelity of risk quantification in practice.').

omega_variable(
    informed_decision_maker_independence,
    'To what extent are ''informed decision-makers'' truly independent and free from organizational or political pressure to accept risks, even when the probabilities are high?',
    'Analysis of decision-making records, interviews with decision-makers, and examination of organizational incentive structures and accountability mechanisms.',
    'If decision-makers are systematically pressured, the ''acceptance'' becomes performative, increasing the theater_ratio and effective suppression, pushing the constraint towards a Piton or Snare, as the coordination function would be undermined by coerced consent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informed_decision_maker_independence, empirical, 'Ambiguity regarding the genuine independence of risk acceptance.').

omega_variable(
    kernel_reading_divergence,
    'Is this ''actuarial risk acceptance'' reading genuinely compatible with the ''engineering absolute threshold'' and ''management compliance narrative'' readings of the Rogers Commission findings, or does it fundamentally undermine them?',
    'Longitudinal study of safety culture evolution in organizations adopting this reading, assessing whether absolute thresholds are still respected in practice and if compliance becomes a substitute for genuine risk reduction.',
    'If this reading systematically forecloses the ''absolute threshold'' by making it practically impossible to maintain, the constraint''s effective extractiveness from engineering teams is higher. If it merely coexists, the impact is less severe. This is a core conceptual divergence between the readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'The conceptual tension between different interpretations of the Rogers Commission findings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__actuarial_risk_acceptance, 1986, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roge_tr_t1986, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 1986, 0.1).
narrative_ontology:measurement(roge_tr_t1996, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 1996, 0.15).
narrative_ontology:measurement(roge_tr_t2006, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 2006, 0.18).
narrative_ontology:measurement(roge_tr_t2015, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 2015, 0.19).
narrative_ontology:measurement(roge_tr_t2024, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(roge_be_t1986, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 1986, 0.5).
narrative_ontology:measurement(roge_be_t1996, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 1996, 0.58).
narrative_ontology:measurement(roge_be_t2006, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 2006, 0.62).
narrative_ontology:measurement(roge_be_t2015, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 2015, 0.64).
narrative_ontology:measurement(roge_be_t2024, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(roge_su_t1986, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 1986, 0.6).
narrative_ontology:measurement(roge_su_t1996, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 1996, 0.65).
narrative_ontology:measurement(roge_su_t2006, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 2006, 0.68).
narrative_ontology:measurement(roge_su_t2015, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 2015, 0.69).
narrative_ontology:measurement(roge_su_t2024, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__actuarial_risk_acceptance, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rogers_commission_findings__actuarial_risk_acceptance, 0.1).
narrative_ontology:affects_constraint(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings__engineering_absolute_threshold).
narrative_ontology:affects_constraint(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings__management_compliance_narrative).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Rogers Commission findings, each with different structural properties and impacts. This reading focuses on actuarial risk acceptance, while siblings focus on absolute engineering thresholds and management compliance narratives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
