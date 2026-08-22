% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__severity_carve_out_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Severity Carve-Out Reading of Beta Designation Doctrine
 *   domain: technology_law/software_liability/consumer_protection
 *
 * SUMMARY:
 *   This story instantiates the severity carve-out reading of the beta
 *   designation doctrine: regardless of how rigorous the testing was or how
 *   clear the disclosure, beta labeling is categorically unavailable as a
 *   liability-limiting mechanism once the software governs life-safety,
 *   financial, or other critical-harm domains. This is a domain-specific
 *   override rule layered on top of whatever general beta doctrine (expansive
 *   shield or narrow warning) would otherwise apply — it does not adjudicate
 *   what beta means generally, it simply removes the label's legal force
 *   entirely once harm severity crosses a threshold. The ε here (0.61 by
 *   interval end) reflects genuine coordination benefit (administrable
 *   bright-line, protects unbargained-for end users) layered with real
 *   extraction cost imposed on smaller vendors who lose a viable
 *   staged-testing pathway that larger incumbents no longer need. This is a
 *   distinct constraint from the expansive_shield_reading and
 *   narrow_warning_reading stories: those readings differ over what beta
 *   means as a general contractual mechanism, while this reading asserts that
 *   meaning is irrelevant once domain severity is high enough — the ε and
 *   beneficiary/victim sets do not transfer between the three stories.
 *
 * KEY AGENTS:
 *   - software_vendors_seeking_beta_shield_in_critical_domains: primary target — barred from using beta as liability shield
 *   - early_access_medical_device_developers: secondary target — loses staged testing pathway
 *   - fintech_startups_using_beta_labels_for_liability_avoidance: secondary target — loses cost-effective launch strategy
 *   - end_users_of_critical_systems: primary beneficiary — protected regardless of label
 *   - regulatory_agencies_overseeing_safety_domains: agenda-setter — administers the categorical rule
 *   - software_vendors_operating_in_carved_out_domains: incumbent beneficiary — competitive insulation from beta-using entrants
 *   - courts_adjudicating_beta_liability_disputes: analytical observer — draws and applies the domain boundary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__severity_carve_out_reading, 0.61).
domain_priors:suppression_score(beta_designation_doctrine__severity_carve_out_reading, 0.42).
domain_priors:theater_ratio(beta_designation_doctrine__severity_carve_out_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__severity_carve_out_reading, tangled_rope).
narrative_ontology:human_readable(beta_designation_doctrine__severity_carve_out_reading, "Severity Carve-Out Reading of Beta Designation Doctrine").
narrative_ontology:topic_domain(beta_designation_doctrine__severity_carve_out_reading, "technology_law/software_liability/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__severity_carve_out_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__severity_carve_out_reading, 'b03ba073-f59b-4aed-a69f-88abdd4dad06').
narrative_ontology:cs_kernel_codification('b03ba073-f59b-4aed-a69f-88abdd4dad06', distributed).
narrative_ontology:cs_authority_grounding('b03ba073-f59b-4aed-a69f-88abdd4dad06', distributed).
narrative_ontology:cs_reading_relation('b03ba073-f59b-4aed-a69f-88abdd4dad06', beta_designation_doctrine__expansive_shield_reading, forecloses).
narrative_ontology:cs_reading_relation('b03ba073-f59b-4aed-a69f-88abdd4dad06', beta_designation_doctrine__narrow_warning_reading, influences).
narrative_ontology:cs_axiom('b03ba073-f59b-4aed-a69f-88abdd4dad06', foundational, harm_severity_overrides_contractual_allocation).
narrative_ontology:cs_axiom_status(harm_severity_overrides_contractual_allocation, holdable).
narrative_ontology:cs_axiom_grounding('b03ba073-f59b-4aed-a69f-88abdd4dad06', harm_severity_overrides_contractual_allocation, deontological).
narrative_ontology:cs_axiom('b03ba073-f59b-4aed-a69f-88abdd4dad06', secondary, categorical_domain_exclusion_is_administrable).
narrative_ontology:cs_axiom_status(categorical_domain_exclusion_is_administrable, holdable).
narrative_ontology:cs_axiom_grounding('b03ba073-f59b-4aed-a69f-88abdd4dad06', categorical_domain_exclusion_is_administrable, instrumental).
narrative_ontology:cs_reference_frame('b03ba073-f59b-4aed-a69f-88abdd4dad06', product_liability_baseline_pre_beta_doctrine).
narrative_ontology:cs_drift_state('b03ba073-f59b-4aed-a69f-88abdd4dad06', contemporary_critical_software_expansion, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('b03ba073-f59b-4aed-a69f-88abdd4dad06', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, end_users_of_critical_systems).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, regulatory_agencies_overseeing_safety_domains).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, software_vendors_operating_in_carved_out_domains).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, software_vendors_seeking_beta_shield_in_critical_domains).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, early_access_medical_device_developers).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, fintech_startups_using_beta_labels_for_liability_avoidance).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, software_vendors_operating_in_carved_out_domains).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__severity_carve_out_reading, harm_severity_overrides_contractual_allocation).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__severity_carve_out_reading, categorical_domain_exclusion_is_administrable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Would prefer to ship life-safety, financial, or infrastructure-adjacent software under a 'beta' label to disclaim liability while iterating quickly and collecting real-world performance data. Under this reading, no amount of testing rigor or disclosure clarity lets them use the beta label to escape liability in these domains — they must either fully certify the product before release or accept full liability exposure. Their exit options are limited to withdrawing from the critical-systems market, delaying launch for certification, or absorbing liability risk directly.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, software_vendors_seeking_beta_shield_in_critical_domains, payer,
    organized, biographical, constrained, national).

% Smaller device and diagnostic-software developers who relied on beta-style staged rollouts to gather clinical feedback before full liability exposure. The carve-out forecloses that pathway entirely for anything touching patient safety, forcing them into costlier pre-market validation regimes or foreign markets with looser doctrine. They lack the capital reserves of larger incumbents to absorb this compliance cost.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, early_access_medical_device_developers, payer,
    moderate, biographical, constrained, national).

% Startups building trading, payment, or lending infrastructure who used beta labeling to launch fast and disclaim responsibility for calculation errors or fund-handling bugs. Under this reading their beta label is legally inert for financial-harm claims — courts and regulators treat the underlying product as fully warranted regardless of the label. They must now either delay launch, purchase liability insurance, or exit the financial-systems segment.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, fintech_startups_using_beta_labels_for_liability_avoidance, payer,
    moderate, biographical, constrained, national).

% Patients relying on medical software, depositors relying on banking infrastructure, and citizens relying on safety-critical systems who have no visibility into a vendor's internal testing status and no meaningful way to evaluate 'beta' risk before harm occurs. This reading protects them by making the label legally irrelevant in these domains — they benefit without having to do anything, and cannot be contracted out of the protection by a label they never negotiated.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, end_users_of_critical_systems, beneficiary,
    powerless, biographical, trapped, national).

% Agencies (health, financial, infrastructure regulators) that administer and enforce the categorical exclusion, drawing bright lines around which domains qualify as life-safety or financial-critical. They benefit from an administrable rule that removes the need for case-by-case adjudication of whether a given beta disclosure was 'genuine' or 'adequate' — the domain classification does the work instead.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, regulatory_agencies_overseeing_safety_domains, agenda_setter,
    institutional, generational, analytical, national).

% Large, well-capitalized incumbents already operating certified, fully-liable products in medical, financial, and infrastructure software. They benefit because the carve-out prevents smaller competitors from using beta-labeling as a cost advantage to undercut them on speed-to-market, effectively raising entry costs for rivals; they also bear ordinary product liability themselves but were largely already carrying it.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, software_vendors_operating_in_carved_out_domains, beneficiary,
    powerful, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__severity_carve_out_reading, software_vendors_operating_in_carved_out_domains, payer).

% Apply the categorical exclusion when a beta-labeled product causes harm in a covered domain, refusing to weigh testing status or disclosure quality once domain classification is established. They generate the case law that defines the boundary of 'critical system,' which is itself contested at the margins (e.g., wellness apps vs. diagnostic tools).
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, courts_adjudicating_beta_liability_disputes, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(beta_designation_doctrine__severity_carve_out_reading, diffuse).
narrative_ontology:fixing_cost_class(beta_designation_doctrine__severity_carve_out_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a bright-line rule that spares courts, regulators, and users from having to litigate whether a given beta disclosure was rigorous enough when the underlying harm is severe — domain classification substitutes for a fact-intensive adequacy inquiry.
% TRANSFER_FUNCTION: Moves the cost of liability-avoidance flexibility away from vendors seeking to use the beta label in life-safety, financial, or critical-infrastructure contexts, and back onto those vendors as ordinary product liability; correspondingly it moves risk protection to end users who cannot negotiate around a label, and moves a competitive advantage to already-certified incumbents.
% ABSENT_VOICES: Early-stage medical and fintech developers who could not appear before the doctrine-forming courts and legislatures with the same resources as incumbent vendors and insurers; their preferred middle-ground position (staged, disclosed testing with proportional liability limits, calibrated by domain) is not represented in the categorical rule.
% DISAPPEARANCE_RATIONALE: If the carve-out vanished, beta-labeling would re-enter life-safety, financial, and infrastructure software as a viable liability shield; smaller vendors would resume staged critical-system rollouts under looser disclosure regimes, incumbents would lose a source of competitive insulation, and end users would lose the categorical protection currently insulating them from labeling-based liability disclaimers in domains where harm severity is highest.
% FOUNDING_PROBLEM: Vendors were using ordinary beta/testing disclaimers to escape liability for life-safety and financial harms that traditional contract-based liability-shifting doctrine was never designed to reach — the general beta doctrine (time-bounded testing disclosure) did not anticipate harm severity as a limiting variable.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory agencies and courts outside any vendor's beneficiary interest attest the problem remains live, citing ongoing litigation over beta-labeled diagnostic and trading software; consumer protection organizations independent of both incumbent and startup vendors corroborate that harm-severity gaps in the general beta doctrine persist absent this carve-out.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__severity_carve_out_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__severity_carve_out_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__severity_carve_out_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(beta_designation_doctrine__severity_carve_out_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__severity_carve_out_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beta_designation_doctrine__severity_carve_out_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(beta_designation_doctrine__severity_carve_out_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(beta_designation_doctrine__severity_carve_out_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises over the interval as the carve-out is progressively applied to a widening set of domains (medical diagnostics, then adjacent wellness/health-adjacent software, then fintech infrastructure), each expansion imposing new compliance costs on smaller vendors while the coordination benefit (protecting unbargained-for end users) remains roughly constant. Suppression is moderate (0.42) — the rule is enforced through litigation and regulatory classification rather than direct prohibition, but vendors have no contractual workaround once a domain is classified as covered. Theater ratio is low-moderate and rising slowly (0.28), reflecting some drift toward domain-classification fights becoming their own compliance industry (expert witnesses on 'is this really a life-safety system') without displacing the rule's substantive bite.
 *
 * DIRECTIONALITY LOGIC:
 *   End users are the clearest beneficiaries: they never negotiated the beta label, cannot assess testing adequacy, and are structurally protected by a rule that makes the label legally inert in their domains — very low d. Vendors seeking to use beta as a shield in these domains are the clearest targets — high d, since the rule specifically exists to close off the option they most want. Incumbent vendors already carrying full liability are net beneficiaries of the competitive-insulation effect even though they are nominally within the same regulatory category — this is why software_vendors_operating_in_carved_out_domains carries a secondary payer role but skews toward beneficiary. Regulatory agencies are agenda-setters, not extraction targets or beneficiaries in the rent-collection sense, though the rule's administrability serves their institutional interest.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (harm-severity gaps in general beta liability doctrine) remains live per the founding_problem_status field, corroborated by regulators and independent consumer-protection observers rather than solely by the rule's beneficiaries — this distinguishes it from a piton or zombie mandate. The classification as tangled_rope rather than pure rope is deliberate: the coordination function (protecting non-bargaining end users) is real, but it rides alongside genuine asymmetric extraction from smaller vendors who lose a testing pathway that incumbents no longer need, and the rule requires active judicial/regulatory enforcement to hold the domain boundary — satisfying all three tangled_rope gates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    critical_domain_boundary_ambiguity,
    'Where exactly does ''life-safety, financial, or other critical system'' stop? Is a wellness app that tracks blood pressure a covered domain, or only FDA-regulated diagnostic software? Is a peer-to-peer payment app covered, or only regulated banking infrastructure?',
    'Accumulated case law and regulatory guidance defining the boundary; a body of contested edge-case rulings would reveal whether the categorical line is administrable or is itself becoming a site of extraction (litigation over classification substituting for litigation over adequacy).',
    'A narrow, stable boundary keeps this reading closer to a genuine coordination rule; a boundary that expands opportunistically to capture more domains over time would indicate the carve-out itself is being used as a vehicle for incumbent-protecting extraction beyond its founding justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(critical_domain_boundary_ambiguity, conceptual, 'Uncertainty over the scope of ''critical system'' domain classification.').

omega_variable(
    kernel_reading_coexistence,
    'Can the severity_carve_out_reading and the narrow_warning_reading coexist as the SAME jurisdiction''s operative doctrine, with the carve-out simply operating as a domain-specific exception layered on the general narrow reading — or does a jurisdiction have to choose one general framework (expansive or narrow) and then decide separately whether it also recognizes a severity carve-out?',
    'Comparative doctrinal analysis across jurisdictions: does any jurisdiction pair the expansive_shield_reading with this severity carve-out, or does the carve-out only ever appear alongside jurisdictions that have already adopted the narrow_warning_reading as their general rule?',
    'If the carve-out only ever appears alongside the narrow reading, the three-reading kernel may actually be a two-tier structure (general rule + severity override) rather than three fully independent competing readings, which would change how the reading_relations should be understood.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_coexistence, conceptual, 'Whether the severity carve-out is a standalone reading or a layered exception onto one of the other two general readings.').

omega_variable(
    incumbent_capture_of_carve_out,
    'Is the severity carve-out primarily protecting end users from unbargained-for risk, or is it being used by well-capitalized incumbents as a moat against faster-moving, beta-testing competitors in the critical-systems space?',
    'Track market concentration in medical-software and fintech-infrastructure segments before and after the carve-out''s adoption in a given jurisdiction; a widening incumbent market-share gap correlated with carve-out enforcement would support the capture reading.',
    'If capture dominates, this reading is more extractive (closer to a de facto tangled_rope tipping toward snare for smaller vendors) than the coordination-first framing suggests; if user protection dominates and incumbent benefit is incidental, the tangled_rope classification with moderate ε is well-calibrated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_capture_of_carve_out, empirical, 'Whether incumbent competitive benefit is incidental or the dominant driver of the carve-out''s persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__severity_carve_out_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t0, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(beta_tr_t4, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(beta_tr_t8, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(beta_tr_t12, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement(beta_tr_t16, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(beta_tr_t20, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(beta_tr_t24, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(beta_be_t0, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(beta_be_t4, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(beta_be_t8, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(beta_be_t12, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(beta_be_t16, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(beta_be_t20, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(beta_be_t24, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 24, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t0, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(beta_su_t4, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 4, 0.34).
narrative_ontology:measurement(beta_su_t8, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 8, 0.37).
narrative_ontology:measurement(beta_su_t12, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 12, 0.39).
narrative_ontology:measurement(beta_su_t16, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement(beta_su_t20, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(beta_su_t24, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__severity_carve_out_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(beta_designation_doctrine__severity_carve_out_reading, 0.12).
narrative_ontology:affects_constraint(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine__expansive_shield_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine__narrow_warning_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints decomposing the natural-language concept of 'beta designation doctrine' per the ε-invariance principle. expansive_shield_reading treats beta labeling as a comprehensive, indefinite liability waiver across all contexts (highest ε for affected end users, lowest for vendors). narrow_warning_reading treats it as a time-bounded testing disclosure with base liability preserved (moderate ε, coordination-forward). This severity_carve_out_reading asserts a domain-specific override: regardless of which general framework a jurisdiction follows, beta's liability-shielding force is categorically unavailable once life-safety, financial, or critical-infrastructure stakes are present. Each reading has a distinct ε, distinct beneficiary/victim structure, and distinct classification; they are linked here because they jointly exhaust the interpretive space physicists — er, lawyers and regulators — currently occupy when invoking 'the beta designation doctrine' as a single label.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
