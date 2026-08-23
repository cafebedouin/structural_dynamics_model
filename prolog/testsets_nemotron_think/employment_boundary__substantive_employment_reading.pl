% ============================================================================
% CONSTRAINT STORY: employment_boundary__substantive_employment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employment_boundary__substantive_employment_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: employment_boundary__substantive_employment_reading
 *   human_readable: Employment Boundary — Substantive Reading (Economic Dependence + Algorithmic Control)
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   The substantive employment reading holds that employment is defined by
 *   economic dependence and algorithmic control, not contract form. Under
 *   this reading, the current classification of platform workers as
 *   independent contractors is a constraint that excludes them from
 *   employment protections (social insurance, job security, collective
 *   bargaining rights). The constraint extracts value by transferring the
 *   cost of social protections from platforms to workers and the public,
 *   while platforms retain algorithmic control over work allocation, pricing,
 *   and discipline. The constraint requires active enforcement (contractual
 *   terms, algorithmic deactivation, lobbying against reclassification) and
 *   has identifiable beneficiaries (platform companies, investors) and
 *   victims (platform workers). The claimed type is tangled_rope: platforms
 *   provide genuine coordination (matching supply/demand, reducing search
 *   costs) but this coordination is entangled with asymmetric extraction
 *   (workers bear precarity, platforms capture rents).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__substantive_employment_reading, 0.52).
domain_priors:suppression_score(employment_boundary__substantive_employment_reading, 0.68).
domain_priors:theater_ratio(employment_boundary__substantive_employment_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__substantive_employment_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__substantive_employment_reading, "Employment Boundary — Substantive Reading (Economic Dependence + Algorithmic Control)").
narrative_ontology:topic_domain(employment_boundary__substantive_employment_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__substantive_employment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__substantive_employment_reading, '960e91bc-daf3-4d00-b2bd-22bed393048b').
narrative_ontology:cs_kernel_codification('960e91bc-daf3-4d00-b2bd-22bed393048b', distributed).
narrative_ontology:cs_authority_grounding('960e91bc-daf3-4d00-b2bd-22bed393048b', distributed).
narrative_ontology:cs_reading_relation('960e91bc-daf3-4d00-b2bd-22bed393048b', employment_boundary__formalist_employment_reading, coexists_with).
narrative_ontology:cs_reading_relation('960e91bc-daf3-4d00-b2bd-22bed393048b', employment_boundary__hybrid_security_reading, coexists_with).
narrative_ontology:cs_axiom('960e91bc-daf3-4d00-b2bd-22bed393048b', foundational, economic_dependence_defines_employment).
narrative_ontology:cs_axiom_status(economic_dependence_defines_employment, holdable).
narrative_ontology:cs_axiom_grounding('960e91bc-daf3-4d00-b2bd-22bed393048b', economic_dependence_defines_employment, deontological).
narrative_ontology:cs_axiom('960e91bc-daf3-4d00-b2bd-22bed393048b', foundational, algorithmic_control_equals_subordination).
narrative_ontology:cs_axiom_status(algorithmic_control_equals_subordination, holdable).
narrative_ontology:cs_axiom_grounding('960e91bc-daf3-4d00-b2bd-22bed393048b', algorithmic_control_equals_subordination, empirically_contingent).
narrative_ontology:cs_reference_frame('960e91bc-daf3-4d00-b2bd-22bed393048b', classical_employment_paradigm).
narrative_ontology:cs_drift_state('960e91bc-daf3-4d00-b2bd-22bed393048b', platform_economy_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('960e91bc-daf3-4d00-b2bd-22bed393048b', '').
narrative_ontology:cs_kernel_id(employment_boundary__substantive_employment_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, platform_companies).
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, venture_capital_backers).
narrative_ontology:constraint_victim(employment_boundary__substantive_employment_reading, platform_workers).
narrative_ontology:constraint_victim(employment_boundary__substantive_employment_reading, dependent_contractors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, dependent_contractors).
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, traditional_employers).
narrative_ontology:constraint_victim(employment_boundary__substantive_employment_reading, social_insurance_systems).
narrative_ontology:constraint_vindicates(employment_boundary__substantive_employment_reading, economic_dependence_defines_employment).
narrative_ontology:constraint_vindicates(employment_boundary__substantive_employment_reading, algorithmic_control_equals_subordination).
narrative_ontology:constraint_vindicates(employment_boundary__substantive_employment_reading, contract_form_is_irrelevant_to_employment_status).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Provide labor through platform algorithms that control work allocation, pricing, and discipline. Classified as independent contractors despite economic dependence on a single platform (or oligopoly of platforms). Bear full precarity: income volatility, no sick pay, no unemployment insurance, no pension contributions, algorithmic deactivation without recourse. Exit is constrained — leaving loses platform-specific capital (ratings, algorithmic visibility); alternative platforms impose similar terms; traditional employment is scarce in their regions/sectors.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, platform_workers, payer,
    moderate, biographical, constrained, global).

% Work across multiple platforms but remain algorithmically controlled by each. Have slightly more exit leverage (multi-homing) but still lack employment protections. Benefit marginally from schedule flexibility that the contractor form nominally permits, but this flexibility is largely illusory due to algorithmic incentives that penalize cherry-picking.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, dependent_contractors, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(employment_boundary__substantive_employment_reading, dependent_contractors, beneficiary).

% Design and own the algorithmic infrastructure that coordinates labor supply and demand. Set contract terms classifying workers as contractors. Capture the surplus from avoided employer-side social contributions (15-30% of labor cost depending on jurisdiction). Actively enforce the classification through contractual terms, algorithmic discipline (deactivation, downranking), and sustained lobbying/litigation against reclassification. Exit is arbitrage-grade: can restructure corporate form, relocate jurisdiction, or pivot business model.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, platform_companies, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(employment_boundary__substantive_employment_reading, platform_companies, beneficiary).

% Fund platform growth predicated on the contractor model's unit economics. The classification lowers labor costs and regulatory risk, increasing valuation multiples. Benefit from the extraction without operational involvement. Exit is arbitrage-grade: capital is mobile across jurisdictions and sectors.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, venture_capital_backers, beneficiary,
    powerful, biographical, arbitrage, global).

% Benefit indirectly from the platform model depressing labor standards and normalization of precarious work. The contractor classification creates a low-floor reference point that weakens bargaining power in adjacent sectors. Not directly involved in platform operations but structurally aligned with the classification's persistence.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, traditional_employers, beneficiary,
    organized, biographical, mobile, national).

% Attempt to organize platform workers but face structural barriers: algorithmic fragmentation of the workforce, contractual prohibitions on collective action, legal uncertainty about bargaining status. Would object to the classification if they had standing; their exclusion is maintained by the same legal/contractual architecture that defines workers as contractors.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, labor_unions_and_worker_organizations, excluded,
    organized, generational, constrained, national).

% Adjudicate the employment boundary through litigation (misclassification lawsuits), legislation (ABC tests, platform work directives), and regulatory guidance. Their rulings shift the constraint's effective extraction by mandating reclassification or creating hybrid categories. They see the full structure but are not themselves collectors or payers of the constraint's transfers.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, regulators_and_courts, observer,
    institutional, generational, analytical, national).

% Bear the fiscal externalities of the contractor classification: workers without unemployment insurance, pension gaps, uncompensated workplace injuries. Cannot exit the constraint — they must cover the shortfall when platform workers access safety nets without employer contributions. The constraint transfers costs from platforms to the public fisc.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, social_insurance_systems, payer,
    institutional, generational, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Platforms algorithmically match labor supply to demand in real time, solving search, pricing, routing, and trust problems that would be prohibitively costly for individual workers and customers to solve bilaterally.
% TRANSFER_FUNCTION: Transfers the cost of social protections (unemployment insurance, pensions, health/safety, paid leave) from platforms to workers and public systems, while transferring algorithmic control rights (allocation, discipline, data) from workers to platforms.
% ABSENT_VOICES: Platform workers in Global South jurisdictions without labor voice; migrant workers on platforms who face deportation risk if they challenge classification; workers who have been deactivated and cannot speak; future workers who will enter under whatever classification prevails.
% DISAPPEARANCE_RATIONALE: If the contractor classification vanished overnight, platforms would be legally required to provide employment protections (social contributions, paid leave, unfair dismissal protection, collective bargaining rights). This would increase platform labor costs by 15-30%, likely reducing platform margins or increasing consumer prices. Workers would gain statutory protections. The platform business model would reorganize around employment — some platforms might exit markets, others would automate more aggressively, some would adopt hybrid models. The labor market structure would fundamentally rearrange.
% FOUNDING_PROBLEM: Early platform markets needed to coordinate highly fragmented, variable labor supply with unpredictable demand in real time. The contractor classification was adopted (or emerged) as a way to avoid the rigidities of employment law (fixed shifts, scheduling constraints, high fixed costs per worker) while scaling rapidly across jurisdictions.
% FOUNDING_PROBLEM_CORROBORATION: Platform companies attest the problem is live: they argue algorithmic flexibility requires contractor status (corroborated by some labor economists who emphasize two-sided market efficiency). Workers, courts (Spain Supreme Court 2020, UK Supreme Court 2021 Uber ruling, Dutch courts), ILO (2021 platform work report), and OECD (2023 platform economy analysis) attest the founding problem is substantially solved — algorithmic coordination does not require misclassification; employment-compliant platforms exist (e.g., Just Eat in UK post-ruling, delivery platforms in Spain post-Rider Law). The corroboration from outside the beneficiary set (courts, international organizations, compliant platforms) supports the shifted-function reading.
narrative_ontology:disappearance_verdict(employment_boundary__substantive_employment_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__substantive_employment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__substantive_employment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(employment_boundary__substantive_employment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__substantive_employment_reading, 0.52, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employment_boundary__substantive_employment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(employment_boundary__substantive_employment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(employment_boundary__substantive_employment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52) reflects the gap between the value workers create and the protections they receive — platforms capture the surplus that would fund social insurance in a standard employment relationship. Suppression (0.68) is high because the constraint persists through active mechanisms: algorithmic deactivation threats, contractual prohibition on collective action, and sustained lobbying/legal campaigns against reclassification. Theater ratio (0.38) captures the 'partnership/entrepreneur' framing that masks the employment reality — real coordination exists but a growing share of enforcement defends the classification itself. Accessibility collapse (0.72) is high because workers lack viable exit: alternative platforms impose similar terms, traditional employment is scarce, and algorithmic blacklisting prevents mobility. Resistance (0.71) is high and rising: worker organizing, strategic litigation, and regulatory action across multiple jurisdictions.
 *
 * PERSPECTIVAL GAP:
 *   From the platform seat, the arrangement is a rope: genuine coordination solving search/matching/pricing problems with minimal coercion. From the worker seat, it is a snare: the coordination story is cover for extracting labor without protections. The engine computes this divergence from the structural data — the authored claim (tangled_rope) asserts both functions are real and entangled, which is exactly what the per-seat computation should reveal: coordination for the platform, extraction for the worker.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform companies are structural beneficiaries (d near 0.0): they collect the surplus from avoided social insurance contributions, control the algorithmic infrastructure, and set contract terms. Their exit is arbitrage-grade — they can relocate, restructure, or lobby. Platform workers are structural targets (d near 1.0): they bear the full cost of precarity (income volatility, no sick pay, no unemployment insurance, algorithmic discipline), and their exit options are constrained — leaving means losing platform-specific capital (ratings, algorithmic visibility) with no portable alternative. Dependent contractors (workers on multiple platforms but still algorithmically controlled) sit at d ~ 0.8. Regulators and courts are analytical observers (d = 0.5) but their rulings shift the constraint's effective extraction. Traditional employers are indirect beneficiaries (d ~ 0.2) — the contractor model depresses labor standards across sectors.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordinating flexible labor in digital markets) remains contested: platforms argue it is live and requires the contractor model; workers and courts increasingly find the problem solved by technology itself — algorithmic coordination does not require employment misclassification. The constraint persists because platforms capture the gains of coordination while externalizing its social costs. Mandatrophy is not resolved: the arrangement has not transitioned to a scaffold (no sunset clause, no transitional logic) and is not a piton (platforms actively defend it, extraction is not atrophied). It is an active tangled_rope where the coordination function is real but the extraction layer has thickened over time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint is one reading (substantive_employment_reading) of the contested kernel employment_boundary. What would the sibling readings (formalist_employment_reading, hybrid_security_reading) change structurally in the constraint''s beneficiary/victim sets and extraction profile?',
    'Comparative constraint story generation: author the formalist and hybrid readings as separate constraint stories with their own ε, beneficiaries, victims, and claimed_type; map structural deltas across the three.',
    'If the formalist reading produces a mountain or rope (low ε, no victims) while the substantive reading produces a tangled_rope (moderate ε, platform_workers as victims), the kernel''s classification is reading-dependent — the engine must track per-reading classifications, not a single kernel classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Committee-frame routing: this constraint is a reading of employment_boundary kernel; sibling readings instantiate different constraints with different structural profiles.').

omega_variable(
    coordination_extraction_boundary,
    'Does the platform''s algorithmic coordination function (matching, pricing, routing) genuinely require the contractor classification, or is the classification a separable extraction layer that could be removed while preserving coordination?',
    'Natural experiment from jurisdictions implementing employment reclassification (e.g., Spain''s Rider Law, California AB5/Prop22, EU Platform Work Directive): measure whether coordination efficiency (match speed, utilization, pricing) degrades when employment protections are mandated.',
    'If coordination survives reclassification, the contractor classification is pure extraction (snare); if coordination degrades substantially, the classification is a tangled_rope where extraction and coordination are structurally entangled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the constraint''s coordination and extraction components are structurally separable — the core empirical question for tangled_rope vs snare classification.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (algorithmic deactivation, contract terms, legal barriers to organizing) or internalized (workers accepting ''entrepreneur'' identity, fear of deactivation preventing resistance)?',
    'Post-reclassification suppression trajectory: if suppression persists after employment status is granted (e.g., algorithmic discipline continues under employment), reclassify as partially internalized; if suppression drops to legal-enforcement baseline, it was primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — workers carry the suppression with them after formal reclassification, requiring cultural/organizational remediation beyond legal change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs. internalized suppression mechanism in algorithmic management.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__substantive_employment_reading, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emp_boundary_substantive_tr_t0, employment_boundary__substantive_employment_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(emp_boundary_substantive_tr_t0, observed).
narrative_ontology:measurement(emp_boundary_substantive_tr_t4, employment_boundary__substantive_employment_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement_basis(emp_boundary_substantive_tr_t4, observed).
narrative_ontology:measurement(emp_boundary_substantive_tr_t8, employment_boundary__substantive_employment_reading, theater_ratio, 8, 0.33).
narrative_ontology:measurement_basis(emp_boundary_substantive_tr_t8, observed).
narrative_ontology:measurement(emp_boundary_substantive_tr_t12, employment_boundary__substantive_employment_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement_basis(emp_boundary_substantive_tr_t12, observed).
narrative_ontology:measurement(emp_boundary_substantive_tr_t14, employment_boundary__substantive_employment_reading, theater_ratio, 14, 0.38).
narrative_ontology:measurement_basis(emp_boundary_substantive_tr_t14, observed).

% Extraction over time
narrative_ontology:measurement(emp_boundary_substantive_be_t0, employment_boundary__substantive_employment_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(emp_boundary_substantive_be_t0, observed).
narrative_ontology:measurement(emp_boundary_substantive_be_t4, employment_boundary__substantive_employment_reading, base_extractiveness, 4, 0.41).
narrative_ontology:measurement_basis(emp_boundary_substantive_be_t4, observed).
narrative_ontology:measurement(emp_boundary_substantive_be_t8, employment_boundary__substantive_employment_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement_basis(emp_boundary_substantive_be_t8, observed).
narrative_ontology:measurement(emp_boundary_substantive_be_t12, employment_boundary__substantive_employment_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement_basis(emp_boundary_substantive_be_t12, observed).
narrative_ontology:measurement(emp_boundary_substantive_be_t14, employment_boundary__substantive_employment_reading, base_extractiveness, 14, 0.52).
narrative_ontology:measurement_basis(emp_boundary_substantive_be_t14, observed).

% Suppression requirement over time
narrative_ontology:measurement(emp_boundary_substantive_su_t0, employment_boundary__substantive_employment_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(emp_boundary_substantive_su_t0, observed).
narrative_ontology:measurement(emp_boundary_substantive_su_t4, employment_boundary__substantive_employment_reading, suppression_requirement, 4, 0.54).
narrative_ontology:measurement_basis(emp_boundary_substantive_su_t4, observed).
narrative_ontology:measurement(emp_boundary_substantive_su_t8, employment_boundary__substantive_employment_reading, suppression_requirement, 8, 0.61).
narrative_ontology:measurement_basis(emp_boundary_substantive_su_t8, observed).
narrative_ontology:measurement(emp_boundary_substantive_su_t12, employment_boundary__substantive_employment_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement_basis(emp_boundary_substantive_su_t12, observed).
narrative_ontology:measurement(emp_boundary_substantive_su_t14, employment_boundary__substantive_employment_reading, suppression_requirement, 14, 0.68).
narrative_ontology:measurement_basis(emp_boundary_substantive_su_t14, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__substantive_employment_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(employment_boundary__substantive_employment_reading, 0.15).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, platform_work_directive_implementation).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, algorithmic_management_transparency).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, portable_benefits_schemes).

% DUAL FORMULATION NOTE:
% Part of the employment_boundary constraint family. The substantive reading (this story) produces tangled_rope with platform_workers as victims. The formalist reading produces rope/mountain with no victims (coordination only). The hybrid reading produces scaffold (transitional third category with sunset). The three readings share the kernel but instantiate different constraints with different ε, beneficiary/victim sets, and claimed types. Linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(employment_boundary__substantive_employment_reading, institutional, 0.15).
constraint_indexing:directionality_override(employment_boundary__substantive_employment_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
