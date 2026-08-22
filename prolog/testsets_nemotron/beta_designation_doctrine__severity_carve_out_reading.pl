% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__severity_carve_out_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-23
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
    domain_priors:emerges_naturally/1,
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
 *   human_readable: Beta Designation Unavailable for Critical Systems
 *   domain: technology_law/software_liability/consumer_protection
 *
 * SUMMARY:
 *   This constraint embodies the doctrinal principle that beta designation —
 *   a software development label indicating incomplete testing — cannot
 *   function as a liability shield in domains where failure causes death,
 *   serious injury, or systemic financial collapse. The constraint is not a
 *   statute but a convergent regulatory and judicial doctrine: FDA, FAA,
 *   banking regulators, and courts consistently hold that safety requirements
 *   and harm severity override any contractual liability allocation attempted
 *   through beta labeling. The reading's ε is low (0.12) because the
 *   constraint extracts little from those it governs; it primarily prevents
 *   extraction (vendors extracting risk-transfer via beta labels).
 *   Suppression is low (0.18) because the constraint operates through legal
 *   validity (courts/regulators simply refuse to enforce beta waivers) rather
 *   than active coercion. Theater is minimal (0.08) — the constraint's
 *   function is genuine and its enforcement is the ordinary operation of
 *   liability law. Accessibility collapse is high (0.82) because once the
 *   principle is understood, alternatives (beta waivers in critical domains)
 *   are legally foreclosed. Resistance is low (0.22) because vendors in
 *   critical domains accept this as the cost of participation; the constraint
 *   is not controversial among regulated parties.
 *
 * KEY AGENTS:
 *   - critical_systems_users: Primary beneficiary (moderate/constrained) — protected from beta waivers in life-safety domains
 *   - software_vendors_critical_domains: Primary payer (powerful/constrained) — bears full liability regardless of development stage
 *   - regulatory_authorities: Agenda setter (institutional/analytical) — administers the categorical bar through enforcement
 *   - insurers_critical_systems: Dual payer/beneficiary (organized/mobile) — bears risk but gains pricing stability
 *   - legal_academics_liability: Observer (analytical/analytical) — analyzes doctrinal boundary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__severity_carve_out_reading, 0.12).
domain_priors:suppression_score(beta_designation_doctrine__severity_carve_out_reading, 0.18).
domain_priors:theater_ratio(beta_designation_doctrine__severity_carve_out_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__severity_carve_out_reading, mountain).
narrative_ontology:human_readable(beta_designation_doctrine__severity_carve_out_reading, "Beta Designation Unavailable for Critical Systems").
narrative_ontology:topic_domain(beta_designation_doctrine__severity_carve_out_reading, "technology_law/software_liability/consumer_protection").

domain_priors:emerges_naturally(beta_designation_doctrine__severity_carve_out_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__severity_carve_out_reading, '8243b262-d1d0-45d1-ba2f-6de8f8391b1a').
narrative_ontology:cs_kernel_codification('8243b262-d1d0-45d1-ba2f-6de8f8391b1a', distributed).
narrative_ontology:cs_authority_grounding('8243b262-d1d0-45d1-ba2f-6de8f8391b1a', distributed).
narrative_ontology:cs_reading_relation('8243b262-d1d0-45d1-ba2f-6de8f8391b1a', beta_designation_doctrine__expansive_shield_reading, forecloses).
narrative_ontology:cs_reading_relation('8243b262-d1d0-45d1-ba2f-6de8f8391b1a', beta_designation_doctrine__narrow_warning_reading, coexists_with).
narrative_ontology:cs_axiom('8243b262-d1d0-45d1-ba2f-6de8f8391b1a', foundational, harm_severity_creates_non_waivable_liability_floor).
narrative_ontology:cs_axiom_status(harm_severity_creates_non_waivable_liability_floor, holdable).
narrative_ontology:cs_axiom_grounding('8243b262-d1d0-45d1-ba2f-6de8f8391b1a', harm_severity_creates_non_waivable_liability_floor, deontological).
narrative_ontology:cs_axiom('8243b262-d1d0-45d1-ba2f-6de8f8391b1a', foundational, beta_designation_cannot_override_physical_consequences).
narrative_ontology:cs_axiom_status(beta_designation_cannot_override_physical_consequences, holdable).
narrative_ontology:cs_axiom_grounding('8243b262-d1d0-45d1-ba2f-6de8f8391b1a', beta_designation_cannot_override_physical_consequences, empirically_contingent).
narrative_ontology:cs_reference_frame('8243b262-d1d0-45d1-ba2f-6de8f8391b1a', liability_allocation_by_contractual_designation).
narrative_ontology:cs_drift_state('8243b262-d1d0-45d1-ba2f-6de8f8391b1a', contemporary_ai_critical_systems, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('8243b262-d1d0-45d1-ba2f-6de8f8391b1a', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, critical_systems_users).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, regulatory_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, insurers_critical_systems).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, software_vendors_critical_domains).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, insurers_critical_systems).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__severity_carve_out_reading, harm_severity_overrides_contractual_allocation).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__severity_carve_out_reading, safety_requirements_are_non_waivable_by_designation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Users of medical devices, financial infrastructure, aviation systems, and other life-safety domains who cannot opt out of reliance on these systems. They benefit from the categorical unavailability of beta designation because it prevents vendors from using beta labels to escape liability for failures in systems where failure causes death, injury, or catastrophic financial loss.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, critical_systems_users, beneficiary,
    moderate, biographical, constrained, national).

% Vendors developing software for life-safety, financial, or critical infrastructure domains. They bear the cost of full liability exposure regardless of development stage or disclosure. They cannot use beta designation to limit liability, must carry full insurance and compliance burden from first deployment, and face higher development costs due to the inability to iterate in production under reduced liability.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, software_vendors_critical_domains, payer,
    powerful, biographical, constrained, global).

% Regulators (FDA, FAA, SEC, banking regulators) who set safety and soundness standards for critical domains. They administer the constraint by refusing to recognize beta designation as a mitigating factor in enforcement actions. Their authority derives from statutory mandates to protect public safety and financial stability.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, regulatory_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Insurers who underwrite liability for critical-systems software vendors. They bear the risk of full liability exposure (payer role) but benefit from the predictability and non-waivability of the liability regime (beneficiary role), which makes risk modeling and pricing more stable than a regime where beta designation could arbitrarily reduce exposure.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, insurers_critical_systems, payer,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__severity_carve_out_reading, insurers_critical_systems, beneficiary).

% Scholars of products liability, software law, and regulatory theory who analyze the doctrinal boundary between contractual liability allocation and non-waivable safety requirements. They see the full structure but neither collect from nor pay into the constraint.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, legal_academics_liability, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the non-negotiable baseline of accountability for systems where failure causes irreversible harm. It solves the collective action problem of vendors racing to deploy under beta labels in high-stakes domains by making the liability floor structural rather than contractual.
% TRANSFER_FUNCTION: Moves the full cost of failure (injury, death, financial catastrophe) from victims to vendors and their insurers, without allowing contractual designation to shift it back. The transfer is from the party creating the risk to the party suffering the harm, with no beta-label escape valve.
% ABSENT_VOICES: Early-stage startups attempting to enter critical domains with limited capital — they would argue the categorical bar raises barriers to entry and prevents iterative safety improvement in production. They are structurally excluded because the constraint treats capital adequacy as a prerequisite for participation, not a problem to be solved by liability reduction.
% DISAPPEARANCE_RATIONALE: If the categorical bar vanished overnight, vendors would immediately deploy beta-labeled software in medical devices, payment systems, and aviation controls to accelerate time-to-market. Liability would shift to victims via click-wrap beta agreements. Regulatory enforcement would fragment as beta designation became a litigated defense. The critical-systems liability landscape would reorganize around contractual waiver rather than statutory floor.
% FOUNDING_PROBLEM: The historical problem was vendors using 'beta' or 'experimental' labels on safety-critical software (early medical device firmware, trading algorithms, flight control systems) to avoid liability for foreseeable catastrophic failures, while regulators lacked a clear doctrinal tool to reject the label categorically.
% FOUNDING_PROBLEM_CORROBORATION: FDA enforcement history (e.g., 2011 infusion pump guidance rejecting 'beta' as mitigation), FAA DO-178C certification requirements that make no provision for beta designation, SEC/FINRA regulatory framework for algorithmic trading systems. These regulatory bodies — outside the vendor beneficiary set — attest the problem persists: vendors still attempt beta-label defenses in critical domains, and regulators still reject them categorically.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__severity_carve_out_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__severity_carve_out_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__severity_carve_out_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(beta_designation_doctrine__severity_carve_out_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__severity_carve_out_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   The mountain classification rests on three pillars: (1) The constraint emerges from the physical reality of irreversible harm — no contractual label can undo death or systemic collapse. (2) It requires no active enforcement machinery beyond ordinary liability adjudication; courts and regulators simply decline to recognize the beta defense. (3) Beneficiaries (users, regulators) are identifiable but the constraint does not transfer value TO them — it prevents value extraction FROM them. The FSM omega documents the tension: the constraint appears natural (harm severity is physical) but its legal instantiation is constructed (courts could recognize beta waivers). The metrics are authored independently of the claim: ε=0.12 reflects minimal extraction; the mountain claim reflects structural invariance.
 *
 * PERSPECTIVAL GAP:
 *   From the vendor seat, the constraint appears as a cost-imposing requirement (full liability from day one). From the user/regulator seat, it appears as a protective floor (no beta escape hatch). The engine computes this divergence from the structural data: vendors are payers with constrained exit; users are beneficiaries with constrained exit; regulators are agenda-setters with analytical exit. The claimed type (mountain) is the analytical observer's classification; vendor seats may compute differently.
 *
 * DIRECTIONALITY LOGIC:
 *   Critical systems users are beneficiaries (d → 0.0) because the constraint prevents vendors from shifting catastrophic risk to them via beta labels. Software vendors in critical domains are payers (d → 1.0) because they bear full liability that could otherwise be contractually reduced. Regulatory authorities are agenda-setters (d → 0.5) — they administer but do not collect. Insurers are dual: payers for the risk they underwrite, beneficiaries of the regime's predictability. The directionality derivation from beneficiary/victim + exit produces the correct structural relationships; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows no mandatrophy: the founding problem (vendors using beta labels to escape liability in critical domains) remains live, as evidenced by ongoing regulatory guidance rejecting beta defenses. The arrangement has not outlived its function; if anything, the proliferation of software in critical domains (AI medical devices, algorithmic trading, autonomous vehicles) has expanded its scope. The low theater ratio and stable extractiveness over 35 years confirm functional persistence, not inertial maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_doctrine,
    'Is the categorical unavailability of beta designation in critical domains a genuine natural law (physical irreversibility of harm) or a constructed legal doctrine that could be reversed by legislative or judicial action?',
    'Legislative history review: did Congress/Parliament explicitly bar beta waivers in critical domains, or did courts/regulators derive the bar from general liability principles? Counterfactual: if a jurisdiction enacted a statute permitting beta waivers in medical devices with enhanced disclosure, would courts uphold it or strike it as contrary to public policy?',
    'If natural law, the constraint is a true mountain (ε ≈ 0, emerges_naturally = true). If constructed doctrine, it is a false summit mountain (FSM candidate) — appears natural but serves identifiable beneficiaries (users, regulators) and could be legislatively overridden. The FSM signature would trigger reclassification to tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_doctrine, conceptual, 'Whether the constraint''s mountain status reflects physical necessity or legal construction serving identifiable beneficiaries.').

omega_variable(
    critical_domain_boundary,
    'What counts as a ''critical system'' for purposes of the beta bar? The boundary determines the constraint''s scope and thus its effective extraction via the engine''s scope modifier.',
    'Survey of regulatory guidance (FDA, FAA, SEC, NERC) and case law to map the declared critical domains. Test edge cases: IoT medical devices, fintech payment apps, industrial control systems, autonomous vehicle software. Determine if the boundary is principle-based (severity of harm) or list-based (enumerated domains).',
    'A principle-based boundary (harm severity threshold) makes the constraint universal in scope (engine scope modifier = universal, higher χ). A list-based boundary makes it national/continental in scope (lower χ). The boundary also determines which vendors are payers and which users are beneficiaries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(critical_domain_boundary, empirical, 'Whether the critical-system boundary is principle-based (universal scope) or list-based (narrower scope).').

omega_variable(
    committer_frame_disagreement_location,
    'Where exactly do the three beta_designation_doctrine readings disagree structurally? Is it on the waivability of liability (expansive_shield vs. severity_carve_out), the temporal bound (narrow_warning vs. both), or the domain scope (severity_carve_out vs. expansive_shield)?',
    'Map each reading''s structural commitments: (1) Can liability be waived by designation? (2) Must the designation be time-bounded? (3) Does harm severity create a categorical exception? The disagreement location determines the reading_relations (forecloses vs. coexists_with vs. influences).',
    'If severity_carve_out forecloses expansive_shield (physical constraint vs. contractual freedom), the kernel has a genuine forecloses pair. If they merely coexist as competing policy positions, the relation is coexists_with. This affects the CS structure''s reading_relations and the engine''s cross-reading contamination analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_disagreement_location, conceptual, 'Structural disagreement location among the three kernel readings for CS commitment-system analysis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__severity_carve_out_reading, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t1990, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(beta_tr_t2000, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(beta_tr_t2010, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 2010, 0.08).
narrative_ontology:measurement(beta_tr_t2020, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 2020, 0.07).
narrative_ontology:measurement(beta_tr_t2025, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 2025, 0.08).

% Extraction over time
narrative_ontology:measurement(beta_be_t1990, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 1990, 0.15).
narrative_ontology:measurement(beta_be_t2000, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 2000, 0.14).
narrative_ontology:measurement(beta_be_t2010, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 2010, 0.12).
narrative_ontology:measurement(beta_be_t2020, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 2020, 0.11).
narrative_ontology:measurement(beta_be_t2025, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 2025, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t1990, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 1990, 0.25).
narrative_ontology:measurement(beta_su_t2000, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 2000, 0.2).
narrative_ontology:measurement(beta_su_t2010, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 2010, 0.18).
narrative_ontology:measurement(beta_su_t2020, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 2020, 0.15).
narrative_ontology:measurement(beta_su_t2025, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 2025, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__severity_carve_out_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(beta_designation_doctrine__severity_carve_out_reading, 0.1).
narrative_ontology:affects_constraint(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine__expansive_shield_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine__narrow_warning_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__severity_carve_out_reading, software_liability_regime).
narrative_ontology:affects_constraint(beta_designation_doctrine__severity_carve_out_reading, critical_infrastructure_regulation).

% DUAL FORMULATION NOTE:
% This reading and its two siblings form the beta_designation_doctrine constraint family. All three share the kernel (beta designation as liability allocation mechanism) but differ on waivability, temporal bounds, and domain scope. The severity_carve_out_reading is the most restrictive (categorical bar in critical domains), expansive_shield_reading the most permissive (comprehensive waiver), narrow_warning_reading intermediate (time-bounded disclosure). ε differs across readings: this reading ε≈0.12 (prevents extraction), expansive_shield ε≈0.7 (enables vendor extraction), narrow_warning ε≈0.3 (partial extraction with temporal limit).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
