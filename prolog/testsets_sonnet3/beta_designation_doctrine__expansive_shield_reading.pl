% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__expansive_shield_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_beta_designation_doctrine__expansive_shield_reading, []).

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
 *   constraint_id: beta_designation_doctrine__expansive_shield_reading
 *   human_readable: Beta Designation as Comprehensive, Indefinite, Universal Liability Waiver
 *   domain: technology_law/software_liability/consumer_protection
 *
 * SUMMARY:
 *   This story instantiates the expansive shield reading of the beta
 *   designation kernel: the claim that labeling a software feature or product
 *   'beta' constitutes a comprehensive liability waiver, that this waiver has
 *   no temporal limit (a feature can remain 'beta' for years while in full
 *   production use), and that it applies uniformly regardless of software
 *   context (from a note-taking app to a payment-processing API). Under this
 *   reading the label has drifted from a testing-phase disclosure into a
 *   standing risk-transfer instrument: vendors externalize essentially all
 *   defect costs onto users, downstream third parties, and integrators, while
 *   retaining unilateral control over when — if ever — the label is removed.
 *   This is a distinct constraint from the narrow_warning_reading (which
 *   time-bounds the waiver and preserves base liability) and from the
 *   severity_carve_out_reading (which forbids the designation for critical
 *   systems entirely) — the three readings have materially different ε,
 *   different victim sets, and different enforceability, and are linked here
 *   only through network.affects_constraints, not merged into one story.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__expansive_shield_reading, 0.81).
domain_priors:suppression_score(beta_designation_doctrine__expansive_shield_reading, 0.62).
domain_priors:theater_ratio(beta_designation_doctrine__expansive_shield_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__expansive_shield_reading, tangled_rope).
narrative_ontology:human_readable(beta_designation_doctrine__expansive_shield_reading, "Beta Designation as Comprehensive, Indefinite, Universal Liability Waiver").
narrative_ontology:topic_domain(beta_designation_doctrine__expansive_shield_reading, "technology_law/software_liability/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__expansive_shield_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__expansive_shield_reading, '0339b240-c8b9-4d55-bc2a-e10526004573').
narrative_ontology:cs_kernel_codification('0339b240-c8b9-4d55-bc2a-e10526004573', fixed_text).
narrative_ontology:cs_authority_grounding('0339b240-c8b9-4d55-bc2a-e10526004573', extraction).
narrative_ontology:cs_interpretation_layer_present('0339b240-c8b9-4d55-bc2a-e10526004573').
narrative_ontology:cs_reading_relation('0339b240-c8b9-4d55-bc2a-e10526004573', beta_designation_doctrine__narrow_warning_reading, forecloses).
narrative_ontology:cs_reading_relation('0339b240-c8b9-4d55-bc2a-e10526004573', beta_designation_doctrine__severity_carve_out_reading, forecloses).
narrative_ontology:cs_axiom('0339b240-c8b9-4d55-bc2a-e10526004573', foundational, label_alone_constitutes_full_waiver).
narrative_ontology:cs_axiom_status(label_alone_constitutes_full_waiver, holdable).
narrative_ontology:cs_axiom_grounding('0339b240-c8b9-4d55-bc2a-e10526004573', label_alone_constitutes_full_waiver, conventional).
narrative_ontology:cs_axiom('0339b240-c8b9-4d55-bc2a-e10526004573', foundational, waiver_duration_and_context_unbounded).
narrative_ontology:cs_axiom_status(waiver_duration_and_context_unbounded, holdable).
narrative_ontology:cs_axiom_grounding('0339b240-c8b9-4d55-bc2a-e10526004573', waiver_duration_and_context_unbounded, conventional).
narrative_ontology:cs_reference_frame('0339b240-c8b9-4d55-bc2a-e10526004573', genuine_testing_phase_disclosure).
narrative_ontology:cs_drift_state('0339b240-c8b9-4d55-bc2a-e10526004573', contemporary_saas_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('0339b240-c8b9-4d55-bc2a-e10526004573', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__expansive_shield_reading, software_vendors).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__expansive_shield_reading, vendor_legal_departments).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__expansive_shield_reading, venture_investors).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, beta_program_users).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, downstream_data_dependents).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, small_business_integrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft the terms-of-service clause that labels a release 'beta' and asserts that this label alone waives all liability for defects, for as long as the vendor chooses to keep the label attached, across every use case the software is put to — from casual note-taking apps to embedded control software. The vendor decides when (if ever) beta status ends and bears no obligation to disclose known defect classes beyond the label itself.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__expansive_shield_reading, software_vendors, beneficiary).

% Draft and litigate the beta clause as boilerplate across product lines; treat the designation as a durable liability firewall rather than a testing-phase disclosure, and defend it in court as applying regardless of how long the product has carried the label or what it is used for.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, vendor_legal_departments, beneficiary,
    institutional, generational, arbitrage, global).

% Benefit indirectly: portfolio companies can ship faster and monetize earlier because the beta label absorbs downstream liability risk that would otherwise require insurance reserves or slower QA cycles, improving reported margins and valuation multiples.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, venture_investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Install or are defaulted into 'beta' features embedded in products they already depend on. They bear data loss, financial loss, or functional failure caused by defects, with no recourse because the beta label — regardless of how long it has persisted or what the software actually does — is read by the vendor as a complete waiver. Exit means abandoning the underlying product entirely, which is often not a real option given switching costs and network effects.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, beta_program_users, payer,
    powerless, biographical, trapped, global).

% Never agreed to any terms; they are third parties whose records, transactions, or communications pass through a labeled-beta system chosen by someone else. When the beta software fails, the harm reaches them with no contractual relationship to invoke and no visibility into the label at all.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, downstream_data_dependents, payer,
    powerless, immediate, trapped, national).

% Build products or workflows on top of vendor APIs and SDKs that carry a permanent beta tag on core, load-bearing functionality. They cannot negotiate the clause, cannot get vendor indemnification, and often cannot afford to re-platform once integrated — the beta label functions as a standing risk transfer they absorbed unknowingly at integration time.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, small_business_integrators, payer,
    moderate, biographical, constrained, national).

% Rarely intervene because the beta label is treated by courts and by the regulators' own precedent as a disclosed-risk category rather than a defect concealment; they are structurally absent from the negotiation of what 'beta' actually means in each product's terms of service.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, consumer_protection_regulators, excluded,
    institutional, generational, analytical, national).

% Adjudicate individual disputes and, under this reading, tend to enforce the beta clause as written — a complete, indefinite, context-general waiver — absent an explicit statutory carve-out, effectively ratifying the vendor's framing case by case.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, product_liability_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(beta_designation_doctrine__expansive_shield_reading, software_vendors).
narrative_ontology:fixing_cost_class(beta_designation_doctrine__expansive_shield_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its narrowest true form, a beta label lets a vendor solicit real-world testing feedback while signaling to users that defects are more likely than in a finished release — a genuine, low-cost disclosure mechanism that helps both sides calibrate expectations during an actual testing phase.
% TRANSFER_FUNCTION: Under this reading, the label is stretched from a disclosure mechanism into a permanent risk-transfer instrument: it moves the cost of every defect, discovered at any time, in any context of use, from the vendor who controls code quality and release timing onto the user, integrator, or bystander who has no comparable control and often no comparable awareness.
% ABSENT_VOICES: Beta program users rarely read the terms of service closely enough to understand the label has no expiration; downstream data dependents never see the label at all; consumer protection regulators are excluded from the case-by-case adjudication that has let the expansive reading calcify into default practice.
% DISAPPEARANCE_RATIONALE: If the expansive shield reading vanished, vendors would need to either fix defects promptly, insure against them, time-bound the 'beta' status meaningfully, or price the residual risk into their product — all of which would shift substantial cost back onto the party that controls the code. Release cadences, QA investment, and insurance markets for software defects would all reorganize.
% FOUNDING_PROBLEM: Early beta programs solved a real problem: vendors needed a way to ship pre-release software to willing testers with clear, mutual understanding that defects were expected and testing was temporary, without full production-liability exposure for a genuinely experimental artifact.
% FOUNDING_PROBLEM_CORROBORATION: Vendors and their legal departments attest the founding problem persists in whatever form the label is applied. Independent voices outside the beneficiary set — consumer protection litigators, academic software-liability scholars, and several state attorneys general in unfair-practices complaints — attest that the founding problem (genuine testing-phase risk-sharing) has been decoupled from its original justification and that 'beta' now frequently labels stable, monetized, indefinitely-maintained production features.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__expansive_shield_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__expansive_shield_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__expansive_shield_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(beta_designation_doctrine__expansive_shield_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__expansive_shield_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beta_designation_doctrine__expansive_shield_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(beta_designation_doctrine__expansive_shield_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(beta_designation_doctrine__expansive_shield_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81 at interval end) because, under this reading, defect costs that would otherwise sit with the party controlling code quality are moved wholesale onto users and third parties, with no severity or duration limit narrowing the transfer. Suppression is moderate (0.62): there is no direct coercive enforcement comparable to a debt collector, but courts enforcing the clause as written, the absence of a practical alternative to accepting terms of service, and switching costs on integrated products all function as suppressive mechanisms. Theater ratio climbs across the interval (0.20 to 0.44) reflecting a growing gap between the label's original testing-disclosure function and its increasing use to badge stable, monetized, indefinitely-maintained features — the label increasingly performs risk-transfer while performing less and less actual testing-phase signaling.
 *
 * PERSPECTIVAL GAP:
 *   From the vendor's seat, the beta label is a legitimate, disclosed risk-sharing mechanism consistent with how software has always been tested — a rope. From the trapped user's or unwitting third party's seat, the same clause functions as an open-ended, unilaterally-controlled cost transfer with no exit — a tangled rope shading toward snare. The engine should register this divergence directly from the power/exit/beneficiary structure; the claimed_type of tangled_rope reflects that a genuine (if attenuated) coordination function — communicating that a feature is less mature — persists alongside the asymmetric extraction, which is what distinguishes this reading from a pure snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Software vendors and their legal departments are the clear structural beneficiaries: they draft the clause, control when beta status ends, and collect the benefit of externalized defect costs — d sits near the full-beneficiary end, especially given their arbitrage-grade exit (they can relabel, re-time, or restructure the clause at will). Venture investors benefit indirectly through faster monetization and lower reserve requirements. Beta program users, downstream data dependents, and small business integrators are targets: they bear costs they did not price in, cannot negotiate the clause, and in the case of downstream dependents never even see it — d sits near the full-target end, amplified by trapped or constrained exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (genuine testing-phase disclosure) is not dead in the abstract — some products really are in active testing — but under this reading the arrangement has been stretched to cover cases where the founding problem no longer applies: stable, revenue-generating, long-running features still carrying the label. This is precisely the mandatrophy pattern the classification should catch: a coordination mechanism (testing disclosure) is being used, by inertia or design, to cover an extraction mechanism (permanent liability transfer) that has outlived any genuine testing rationale. Tangled rope, rather than snare, is claimed because a residual coordination function is real for at least some labeled products, even as the label's expansive application substantially outruns it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    beta_kernel_reading_indeterminacy,
    'Which reading of the beta designation kernel actually governs a given dispute: the expansive shield reading (no temporal or severity limit), the narrow warning reading (time-bounded, base liability preserved), or the severity carve-out reading (categorically unavailable for critical systems)?',
    'Jurisdiction-by-jurisdiction case law tracking, statutory reform proposals, and terms-of-service audits comparing labeled duration against actual testing activity would show which reading courts and legislatures are converging toward.',
    'If courts converge on the narrow_warning_reading or the severity_carve_out_reading, this story''s extractiveness and victim set collapse substantially — the expansive shield reading would be a legally unsupported outlier rather than the operative doctrine this story assumes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beta_kernel_reading_indeterminacy, conceptual, 'Which sibling reading of the beta designation kernel actually controls in practice is unresolved and jurisdiction-dependent.').

omega_variable(
    duration_as_bad_faith_signal,
    'At what point does an indefinite beta label stop being a plausible testing-phase disclosure and become evidence of bad-faith mislabeling to avoid liability?',
    'Empirical study of time-to-production-revenue versus time-in-beta-label across a sample of vendor products; a large gap between monetization and label removal would be strong evidence against the expansive reading''s good-faith premise.',
    'If the gap is systematically large across the industry, it supports treating the expansive reading as a snare-like pretext rather than a genuine (if overextended) coordination mechanism, which would push the classification away from tangled_rope toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(duration_as_bad_faith_signal, empirical, 'Whether indefinite beta duration is itself diagnostic of extraction intent rather than genuine ongoing testing.').

omega_variable(
    downstream_third_party_standing,
    'Do downstream data dependents who never agreed to any terms of service have any legal standing to challenge the beta waiver at all, under this reading?',
    'Case law on third-party beneficiary and privity doctrine as applied to software terms of service; regulatory guidance on whether non-contracting parties harmed by labeled-beta failures have an independent cause of action.',
    'If third parties have no standing whatsoever, the victim set for this reading is even more powerless than the stakeholder situations describe, since they cannot even initiate the dispute that would test the clause''s enforceability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(downstream_third_party_standing, empirical, 'Whether non-contracting third parties harmed by beta-labeled failures can challenge the waiver at all.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__expansive_shield_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t0, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(beta_tr_t4, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(beta_tr_t8, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(beta_tr_t12, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(beta_tr_t16, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 16, 0.39).
narrative_ontology:measurement(beta_tr_t20, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(beta_tr_t24, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 24, 0.44).

% Extraction over time
narrative_ontology:measurement(beta_be_t0, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(beta_be_t4, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 4, 0.62).
narrative_ontology:measurement(beta_be_t8, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 8, 0.68).
narrative_ontology:measurement(beta_be_t12, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 12, 0.72).
narrative_ontology:measurement(beta_be_t16, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 16, 0.76).
narrative_ontology:measurement(beta_be_t20, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 20, 0.79).
narrative_ontology:measurement(beta_be_t24, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 24, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t0, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(beta_su_t4, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 4, 0.46).
narrative_ontology:measurement(beta_su_t8, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 8, 0.51).
narrative_ontology:measurement(beta_su_t12, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(beta_su_t16, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(beta_su_t20, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(beta_su_t24, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 24, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__expansive_shield_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(beta_designation_doctrine__expansive_shield_reading, 0.1).
narrative_ontology:affects_constraint(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine__narrow_warning_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine__severity_carve_out_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the beta_designation_doctrine kernel. narrow_warning_reading treats the label as time-bounded and preserving base liability once the testing phase ends; severity_carve_out_reading forbids the label outright for critical systems. All three share the same underlying kernel text (a vendor's 'beta' designation) but instantiate structurally distinct constraints with different ε, different beneficiary/victim structures, and different enforceability. This story (expansive_shield_reading) has the highest ε and the broadest victim set of the three because it authors no temporal or severity limit on the waiver.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
