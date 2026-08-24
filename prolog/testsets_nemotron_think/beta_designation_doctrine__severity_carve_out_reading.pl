% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__severity_carve_out_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_beta_designation_doctrine__severity_carve_out_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: beta_designation_doctrine__severity_carve_out_reading
 *   human_readable: Beta Designation Categorically Unavailable for Critical Systems
 *   domain: technology_law/software_liability/consumer_protection
 *
 * SUMMARY:
 *   The severity_carve_out_reading of the beta_designation_doctrine kernel
 *   holds that beta designation — a software vendor's declaration that a
 *   product is in testing and therefore carries reduced liability — is
 *   categorically unavailable for life-safety, financial, or other critical
 *   systems regardless of how thoroughly tested or how transparently
 *   disclosed. This reading treats the constraint as a domain-specific
 *   physical limitation: when harm severity reaches a threshold (death,
 *   financial catastrophe, systemic collapse), the physical reality of
 *   consequences overrides any contractual liability allocation. The
 *   constraint is claimed as a mountain because it emerges from the nature of
 *   harm itself, not from legislative choice; the engine will evaluate
 *   whether the metric profile supports this or whether identifiable
 *   beneficiaries (end users, regulators) and extraction from vendors suggest
 *   a false summit.
 *
 * KEY AGENTS:
 *   - critical_system_end_users: Primary beneficiary (powerless/identity_locked) — protected from beta disclaimers in systems where failure causes death or catastrophe
 *   - patients_in_medical_devices: Primary beneficiary (powerless/trapped) — cannot exit medical device dependencies; beta unavailability is a survival constraint
 *   - financial_system_participants: Primary beneficiary (organized/constrained) — systemic risk protection overrides vendor liability limitation attempts
 *   - critical_system_software_vendors: Primary victim/payer (powerful/constrained) — bear full liability regardless of testing investment; cannot use beta to stage deployment
 *   - regulators_courts: Agenda_setter (institutional/analytical) — enforce the carve-out through rejection of beta defenses in critical-system litigation
 *   - safety_engineers: Observer (organized/analytical) — see the constraint as reflecting physical reality of harm severity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__severity_carve_out_reading, 0.15).
domain_priors:suppression_score(beta_designation_doctrine__severity_carve_out_reading, 0.25).
domain_priors:theater_ratio(beta_designation_doctrine__severity_carve_out_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__severity_carve_out_reading, mountain).
narrative_ontology:human_readable(beta_designation_doctrine__severity_carve_out_reading, "Beta Designation Categorically Unavailable for Critical Systems").
narrative_ontology:topic_domain(beta_designation_doctrine__severity_carve_out_reading, "technology_law/software_liability/consumer_protection").

domain_priors:emerges_naturally(beta_designation_doctrine__severity_carve_out_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__severity_carve_out_reading, '40524d32-a5e4-4802-b20f-677c805113e0').
narrative_ontology:cs_kernel_codification('40524d32-a5e4-4802-b20f-677c805113e0', formalized).
narrative_ontology:cs_authority_grounding('40524d32-a5e4-4802-b20f-677c805113e0', lineage).
narrative_ontology:cs_interpretation_layer_present('40524d32-a5e4-4802-b20f-677c805113e0').
narrative_ontology:cs_reading_relation('40524d32-a5e4-4802-b20f-677c805113e0', beta_designation_doctrine__expansive_shield_reading, forecloses).
narrative_ontology:cs_reading_relation('40524d32-a5e4-4802-b20f-677c805113e0', beta_designation_doctrine__narrow_warning_reading, coexists_with).
narrative_ontology:cs_axiom('40524d32-a5e4-4802-b20f-677c805113e0', foundational, safety_requirements_override_contractual_liability).
narrative_ontology:cs_axiom_status(safety_requirements_override_contractual_liability, holdable).
narrative_ontology:cs_axiom_grounding('40524d32-a5e4-4802-b20f-677c805113e0', safety_requirements_override_contractual_liability, deontological).
narrative_ontology:cs_axiom('40524d32-a5e4-4802-b20f-677c805113e0', secondary, harm_severity_establishes_liability_floor).
narrative_ontology:cs_axiom_status(harm_severity_establishes_liability_floor, holdable).
narrative_ontology:cs_axiom_grounding('40524d32-a5e4-4802-b20f-677c805113e0', harm_severity_establishes_liability_floor, deontological).
narrative_ontology:cs_reference_frame('40524d32-a5e4-4802-b20f-677c805113e0', contractual_liability_allocation).
narrative_ontology:cs_drift_state('40524d32-a5e4-4802-b20f-677c805113e0', contemporary_safety_regulation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('40524d32-a5e4-4802-b20f-677c805113e0', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, critical_system_end_users).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, patients_in_medical_devices).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, financial_system_participants).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, critical_system_software_vendors).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__severity_carve_out_reading, safety_requirements_override_contractual_liability).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__severity_carve_out_reading, harm_severity_establishes_liability_floor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Users of life-safety and financial-critical software (medical device patients, airline passengers, banking system participants) who cannot exit these systems and depend on the constraint preventing vendors from disclaiming liability via beta labels. Their self-concept and physical survival are fused to the constraint's operation.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, critical_system_end_users, beneficiary,
    powerless, biographical, identity_locked, global).

% Patients dependent on implanted or life-sustaining medical devices running software. They have zero exit options — device removal may mean death — and zero bargaining power. The beta prohibition is not a preference but a survival requirement.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, patients_in_medical_devices, beneficiary,
    powerless, biographical, trapped, global).

% Institutional and retail participants in financial markets who rely on trading, settlement, and clearing systems. They organize through regulatory advocacy but cannot individually exit the financial infrastructure. The constraint protects systemic stability that benefits all participants.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, financial_system_participants, beneficiary,
    organized, generational, constrained, global).

% Vendors developing software for medical devices, avionics, financial infrastructure, nuclear controls, etc. They bear full liability internalization costs — cannot use beta designation to stage deployment or limit exposure. Exit is constrained because critical-system markets are high-barrier, regulated, and often their core business. They lobby for beta pathways but face regulatory rejection.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, critical_system_software_vendors, payer,
    powerful, biographical, constrained, global).

% FDA, FAA, SEC, banking regulators, and courts that adjudicate liability in critical-system failures. They set and enforce the boundary by rejecting beta defenses, mandating rigorous certification (DO-178C, IEC 62304), and imposing liability regardless of testing disclosures. They neither extract nor pay but administer the constraint.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, regulators_courts, agenda_setter,
    institutional, generational, analytical, national).

% Engineers who design safety-critical systems and develop certification standards. They view the constraint as reflecting physical reality — no amount of testing can prove absence of catastrophic failure modes in complex systems — and treat beta prohibition as an engineering truth, not a legal choice.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, safety_engineers, observer,
    organized, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates liability allocation in critical systems by establishing a non-negotiable floor: when harm severity crosses a threshold (death, financial catastrophe), liability cannot be contracted away via beta designation. This prevents a race to the bottom where vendors compete by offering beta disclaimers instead of safety investment.
% TRANSFER_FUNCTION: Moves catastrophic risk from end users/patients/financial participants (who would bear it if beta disclaimers were valid) to software vendors (who must internalize it through insurance, redundancy, and safety engineering). The transfer is from powerless/trapped parties to powerful/constrained parties.
% ABSENT_VOICES: Startups and open-source developers who might enter critical-system markets if beta pathways existed for staged deployment; they are excluded because the constraint raises entry barriers. Future users of yet-undeployed critical systems (autonomous vehicles, AI diagnostics) whose risk profiles are not yet fully understood.
% DISAPPEARANCE_RATIONALE: If the beta prohibition vanished overnight, vendors would immediately deploy beta-labeled critical systems to accelerate time-to-market, shifting catastrophic risk to trapped end users. Medical device recalls would increase, financial system instability would rise, and the certification regimes (DO-178C, IEC 62304) that currently substitute for beta would lose their forcing function. The world would rearrange toward higher catastrophic failure rates.
% FOUNDING_PROBLEM: Early software in critical systems (1970s-80s medical devices, avionics) used beta or 'experimental' labels to limit liability while deploying in life-safety contexts. Catastrophic failures (Therac-25, early avionics incidents) demonstrated that contractual liability limitation is physically meaningless when software failure kills people — the harm occurs regardless of contract terms.
% FOUNDING_PROBLEM_CORROBORATION: FDA medical device guidance (not vendor-authored) explicitly rejects beta labeling for Class III devices; FAA DO-178C certification requires full liability internalization regardless of testing phase; financial regulators (SEC, OCC) treat beta as irrelevant to systemic risk liability. These corroborating sources are outside the beneficiary set (they are regulators, not end users).
narrative_ontology:disappearance_verdict(beta_designation_doctrine__severity_carve_out_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__severity_carve_out_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__severity_carve_out_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(beta_designation_doctrine__severity_carve_out_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__severity_carve_out_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beta_designation_doctrine__severity_carve_out_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, ExtMetricName, E),
    domain_priors:suppression_score(beta_designation_doctrine__severity_carve_out_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(beta_designation_doctrine__severity_carve_out_reading),
    narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(beta_designation_doctrine__severity_carve_out_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint shows mountain-like metrics: very low extractiveness (0.15) because it prevents vendors from externalizing catastrophic risk rather than extracting value; low suppression (0.25) because vendors comply primarily due to physical consequence awareness, not active enforcement; near-zero theater (0.1) because the constraint has no performative component — it is a hard boundary. Accessibility collapse is high (0.85) because once the harm-severity threshold is understood, no contractual workaround is treated as valid. Resistance is low (0.15) because the constraint aligns with physics of harm. The slight upward drift in extractiveness and suppression over time reflects vendor attempts to create 'beta-like' certified deployment pathways that test the boundary.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (end users, patients, financial participants) are structurally positioned at d ≈ 0.0-0.2: the constraint subsidizes them by preventing vendors from shifting catastrophic risk via beta labels. Victims (critical system vendors) sit at d ≈ 0.8-1.0: they bear full liability internalization costs with constrained exit (cannot avoid critical-system markets if that is their domain). Regulators/courts as agenda_setters sit near d ≈ 0.5 (symmetric): they administer the boundary but neither extract nor pay. The derivation follows from beneficiary/victim declarations plus exit options: beneficiaries are trapped/identity_locked in critical systems, vendors are constrained but powerful.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing vendors from using 'beta' to disclaim liability for systems where failure kills people or collapses financial infrastructure — remains live. The constraint is not vestigial; as software eats more critical infrastructure (autonomous vehicles, AI medical diagnosis, algorithmic trading), the carve-out's coordination function expands. No mandatrophy: the constraint's function has not atrophied, it has intensified.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine physical/natural constraint (safety requirements override contracts) or a constructed legal doctrine that benefits identifiable agents?',
    'Cross-jurisdictional comparison of whether beta designations are honored in critical systems regardless of testing; historical analysis of whether the carve-out emerged from judicial recognition of physical limits or legislative policy choice.',
    'If natural law, classification as mountain holds; if constructed doctrine with identifiable beneficiaries (end users, regulators), false_summit_mountain signature triggers reclassification to tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Natural-law vs. constructed-doctrine ambiguity for the severity carve-out reading').

omega_variable(
    critical_system_boundary,
    'Where exactly does the ''critical system'' boundary lie — does it include only immediate life-safety (medical devices, avionics) or extend to financial infrastructure, municipal systems, AI decision-making?',
    'Case law survey of beta designation rejection across domains; regulatory guidance on scope of ''critical'' in software liability frameworks.',
    'Boundary expansion increases beneficiary class and extraction from vendors; contraction reduces coordination coverage.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(critical_system_boundary, empirical, 'Scope ambiguity of ''critical systems'' in the severity carve-out').

omega_variable(
    testing_status_irrelevance,
    'Does the carve-out genuinely apply ''regardless of testing status or disclosure,'' or do extensive testing regimes (e.g., DO-178C Level A) create de facto pathways that functionally resemble beta?',
    'Analysis of certification regimes that achieve beta-like staged deployment under different labels; vendor surveys on whether ''beta'' label is avoided but equivalent practices persist.',
    'If testing regimes create functional equivalents, the carve-out''s extraction suppression is lower than claimed; the constraint becomes more performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(testing_status_irrelevance, empirical, 'Whether rigorous certification regimes functionally circumvent the beta prohibition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__severity_carve_out_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t1980, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(beta_tr_t1995, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 1995, 0.07).
narrative_ontology:measurement(beta_tr_t2010, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 2010, 0.08).
narrative_ontology:measurement(beta_tr_t2025, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(beta_be_t1980, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 1980, 0.1).
narrative_ontology:measurement(beta_be_t1995, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 1995, 0.12).
narrative_ontology:measurement(beta_be_t2010, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 2010, 0.14).
narrative_ontology:measurement(beta_be_t2025, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 2025, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t1980, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 1980, 0.2).
narrative_ontology:measurement(beta_su_t1995, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 1995, 0.22).
narrative_ontology:measurement(beta_su_t2010, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 2010, 0.24).
narrative_ontology:measurement(beta_su_t2025, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 2025, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__severity_carve_out_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine__expansive_shield_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine__narrow_warning_reading).

% DUAL FORMULATION NOTE:
% The beta_designation_doctrine kernel decomposes into three readings with different ε values and beneficiary/victim structures. This reading (severity_carve_out) has low ε (mountain profile) because it prevents risk externalization; expansive_shield_reading has high ε (snare profile) because it enables vendor risk-shifting across all domains; narrow_warning_reading has moderate ε (tangled_rope profile) because it coordinates testing disclosure while preserving liability. The readings are linked through the kernel's shared subject matter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(beta_designation_doctrine__severity_carve_out_reading, powerful, 0.85).
constraint_indexing:directionality_override(beta_designation_doctrine__severity_carve_out_reading, powerless, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
