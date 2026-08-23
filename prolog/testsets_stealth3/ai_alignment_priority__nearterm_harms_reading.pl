% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__nearterm_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_priority__nearterm_harms_reading, []).

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
 *   constraint_id: ai_alignment_priority__nearterm_harms_reading
 *   human_readable: Algorithmic Gatekeeping of Life Chances — Near-Term Harms Reading of AI Alignment
 *   domain: technological/economic/social
 *
 * SUMMARY:
 *   The standing arrangement under contest is the deployment of algorithmic
 *   decision systems into consequential life-chances domains — hiring, tenant
 *   screening, consumer credit, public benefits, gig-work management —
 *   together with the thin, reactive governance overlay that currently
 *   surrounds them. Systems are sold and configured by a small vendor set,
 *   operated by high-volume deployers who capture the efficiency surplus, and
 *   borne by applicants whose exits from the shared screening infrastructure
 *   are closed. Error costs distribute unevenly along age, race, and
 *   disability lines, and the machinery for contesting a score is slower than
 *   the machinery for generating one. KEY AGENTS (by structural
 *   relationship): see key_agents. This story instantiates ONE reading of the
 *   ai_alignment_priority kernel (see kernel_context); the epsilon referent
 *   is the standing arrangement described above, assessed by this reading's
 *   own lights — not the audit-and-redress program this reading advocates,
 *   which would drive epsilon toward zero by construction. The claimed_type
 *   and the metrics are authored independently: the type states what I
 *   believe is structurally true of the arrangement; the metrics state what I
 *   believe is descriptively true of its operation. Where the engine's
 *   computed per-seat types diverge from the claim, that divergence is the
 *   datum.
 *
 * KEY AGENTS:
 *   - high_volume_deployers: Primary beneficiary (powerful/mobile) — captures throughput and labor savings while error costs land elsewhere
 *   - automated_decision_system_vendors: Secondary beneficiary and de facto administrator (institutional/arbitrage) — sells and configures the gatekeeping stack, sets default thresholds
 *   - black_and_latino_credit_and_rental_applicants: Primary target (powerless/trapped) — bears disparate denial and mispricing with no provider to switch to
 *   - disabled_benefits_claimants: Primary target (powerless/trapped) — bears eligibility-automation error inside a monopoly state pipeline
 *   - older_job_applicants: Target (moderate/constrained) — bears ranking-model age penalties across every employer in the segment
 *   - gig_workers_under_algorithmic_management: Target (powerless/constrained) — bears dispatch, pricing, and deactivation control over income
 *   - sector_regulators: Agenda setter (institutional/constrained) — administers the thin binding oversight that exists, reactively
 *   - algorithmic_accountability_advocates: Excluded voice (organized/analytical) — documents harms from outside the deployment conversation
 *   - independent_ai_auditors: Excluded voice (moderate/analytical) — contractually barred from evaluating running systems
 *   - ai_governance_research_community: Analytical observer (moderate/analytical) — maps the alignment-priority dispute without enforcement role
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__nearterm_harms_reading, 0.72).
domain_priors:suppression_score(ai_alignment_priority__nearterm_harms_reading, 0.68).
domain_priors:theater_ratio(ai_alignment_priority__nearterm_harms_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__nearterm_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__nearterm_harms_reading, "Algorithmic Gatekeeping of Life Chances — Near-Term Harms Reading of AI Alignment").
narrative_ontology:topic_domain(ai_alignment_priority__nearterm_harms_reading, "technological/economic/social").

domain_priors:requires_active_enforcement(ai_alignment_priority__nearterm_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__nearterm_harms_reading, 'd2b34164-e8da-49aa-8ac4-453c53b64f7f').
narrative_ontology:cs_kernel_codification('d2b34164-e8da-49aa-8ac4-453c53b64f7f', distributed).
narrative_ontology:cs_authority_grounding('d2b34164-e8da-49aa-8ac4-453c53b64f7f', distributed).
narrative_ontology:cs_reading_relation('d2b34164-e8da-49aa-8ac4-453c53b64f7f', ai_alignment_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('d2b34164-e8da-49aa-8ac4-453c53b64f7f', ai_alignment_priority__integrated_reading, coexists_with).
narrative_ontology:cs_axiom('d2b34164-e8da-49aa-8ac4-453c53b64f7f', foundational, present_persons_justice_prior_to_speculative_risk).
narrative_ontology:cs_axiom_status(present_persons_justice_prior_to_speculative_risk, holdable).
narrative_ontology:cs_axiom_grounding('d2b34164-e8da-49aa-8ac4-453c53b64f7f', present_persons_justice_prior_to_speculative_risk, deontological).
narrative_ontology:cs_axiom('d2b34164-e8da-49aa-8ac4-453c53b64f7f', secondary, deployed_discrimination_counts_as_alignment_failure).
narrative_ontology:cs_axiom_status(deployed_discrimination_counts_as_alignment_failure, holdable).
narrative_ontology:cs_axiom_grounding('d2b34164-e8da-49aa-8ac4-453c53b64f7f', deployed_discrimination_counts_as_alignment_failure, conventional).
narrative_ontology:cs_reference_frame('d2b34164-e8da-49aa-8ac4-453c53b64f7f', justice_first_present_harm_framework).
narrative_ontology:cs_drift_state('d2b34164-e8da-49aa-8ac4-453c53b64f7f', contemporary_frontier_labs_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d2b34164-e8da-49aa-8ac4-453c53b64f7f', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, automated_decision_system_vendors).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, high_volume_deployers).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, black_and_latino_credit_and_rental_applicants).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, disabled_benefits_claimants).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, older_job_applicants).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, gig_workers_under_algorithmic_management).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sell scoring and decision products into hiring, tenant screening, consumer credit, and public benefits. Configure default thresholds and model architectures that determine who clears each gate, and shield model logic as trade secrets. Operate across jurisdictions, shifting product lines when any single regulator tightens its rules.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, automated_decision_system_vendors, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__nearterm_harms_reading, automated_decision_system_vendors, agenda_setter).

% Large employers, landlord chains, lenders, and agencies that integrate these systems to process application and claim volumes no human workforce could handle. Capture the throughput and labor-cost savings; the error costs of the systems land on the people scored, not on them. Can switch vendors, renegotiate contracts, or relocate operations when oversight tightens.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, high_volume_deployers, beneficiary,
    powerful, biographical, mobile, global).

% Face screening scores and pricing models trained on historically redlined data, producing higher decline and mispricing rates. Virtually every landlord and lender in their market draws on the same small set of screening products, so there is no provider to switch to; disputes route through opaque vendor processes with slow, uncertain correction.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, black_and_latino_credit_and_rental_applicants, payer,
    powerless, biographical, trapped, national).

% Subject to eligibility automation that flags them for overpayment recovery or denies claims on the basis of correlated proxies. The state pipeline is a monopoly — there is no alternative administrator to apply to — and reversing a wrongful flag or clawback takes years of correspondence most claimants cannot sustain.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, disabled_benefits_claimants, payer,
    powerless, biographical, trapped, national).

% Pass through resume-ranking and recorded-interview scoring that penalizes employment gaps and age-correlated signals. Some individual resources to contest decisions, but every major employer in their segment runs comparable stacks, so declining automated hiring means exiting the segment rather than choosing a fairer channel.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, older_job_applicants, payer,
    moderate, biographical, constrained, national).

% Work availability, routing, and pay are governed by dispatch and pricing algorithms; account deactivation ends income immediately. Collective responses — strikes, lawsuits, driver associations — exist in some markets but coverage is partial and reactivation terms remain unilateral.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, gig_workers_under_algorithmic_management, payer,
    powerless, biographical, constrained, global).

% Civil-rights and financial agencies holding formal authority over disparate impact and unfair practices. Budgets and in-house technical staff lag deployment velocity; enforcement arrives reactively, mostly through investigations triggered by journalists and litigants. They administer the thin layer of binding oversight that currently exists.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, sector_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Civil-rights organizations and investigative journalists documenting disparate outcomes and pressing for contestability rights and disclosure. They sit outside deployment decisions — consulted, if at all, after design freeze — and exercise leverage through publicity, comment periods, and litigation rather than a seat at the configuration table.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, algorithmic_accountability_advocates, excluded,
    organized, biographical, analytical, national).

% Researchers and audit practitioners seeking to evaluate deployed systems are contractually barred by platform terms and trade-secret claims. Access generally requires a deployer invitation, which selects for engagements likely to yield favorable findings; unsanctioned testing exposes the tester to legal risk.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, independent_ai_auditors, excluded,
    moderate, biographical, analytical, continental).

% Scholars mapping what the alignment agenda means and funds, publishing taxonomies of the dispute between present-harm, catastrophic-risk, and combined framings. They hold no enforcement role; their output shapes terminology and second-order attention rather than any seat's immediate position.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, ai_governance_research_community, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_priority__nearterm_harms_reading, high_volume_deployers).
narrative_ontology:fixing_cost_class(ai_alignment_priority__nearterm_harms_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves high-volume consequential decision-making: processing millions of job applications, rental and credit screenings, and benefits determinations at speed and procedural consistency no human workforce could match, with embedded fraud and anomaly detection.
% TRANSFER_FUNCTION: Moves decision outcomes and their error costs: throughput gains and avoided labor expense flow to deployers and vendors, while misclassification risks — denied housing, credit, employment, benefits, wrongful fraud flags — concentrate on the scored applicants least able to absorb or contest them.
% ABSENT_VOICES: Scored applicants and claimants appear only as training rows and outcome statistics; independent auditors are contractually barred from examining running systems; affected-community representatives hold no seat in threshold-setting or vendor procurement. Present, they would demand contestability rights, human fallback paths, and disparate-impact disclosure before deployment.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, application and claim volumes would crash back onto human review queues, vendor markets and procurement contracts would unwind, hiring and housing pipelines would reorganize around manual or differently-automated processes, and the error distribution would shift from algorithmic proxies to individual discretion — a wholesale rearrangement of how life chances are allocated at scale.
% FOUNDING_PROBLEM: Human decision-making at institutional scale was slow, inconsistent, expensive, and exposed to individual prejudice; automation promised uniformity and throughput for postwar-scale application volumes, later extended by machine learning to pattern detection beyond human capacity.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the beneficiary set: public-benefits agency caseload and backlog statistics, employer-side recruiting-throughput reporting, and applicant testimony gathered by civil-rights organizations — sources that acknowledge the volume and consistency problem is real even while disputing the current distribution of its error costs. No beneficiary-only attestation stands alone.
narrative_ontology:disappearance_verdict(ai_alignment_priority__nearterm_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__nearterm_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__nearterm_harms_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_alignment_priority__nearterm_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__nearterm_harms_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_priority__nearterm_harms_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_priority__nearterm_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_priority__nearterm_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.72) because the arrangement's gains and losses are decoupled: deployers book the savings, and the error burden — measured across documented disparate-impact findings in credit, screening, benefits, and gig management — concentrates on populations with the least capacity to absorb or appeal it. Suppression (0.68) is a raw structural property, unscaled by power or scope: exits are closed (no alternative screening provider, no human-fallback path in most pipelines), independent inspection is contractually barred, and model logic is trade-secret shielded. Theater (0.48) reflects a governance layer heavy on principles documents, voluntary pledges, and ethics boards without authority, relative to binding, retroactive-able remediation. Accessibility collapse (0.42) is moderate: individuals cannot practically exit the shared screening ecosystem, but human-review remnants, community know-how, and occasional litigation wins keep alternatives partly alive — far from a natural law's near-total collapse. Resistance (0.60) is real and organized: investigative journalism, civil-rights litigation, driver strikes, and a first generation of binding audit statutes, currently outmatched by deployment velocity. The temporal series run on one shared grid (T0..T12, anchored approximately 2014–2026) with all three metrics authored at every point; suppression_requirement is tracked because the story's central dynamic is enforcement-capacity hardening (lock-in, audit bans, lobbying), not merely extraction drift.
 *
 * PERSPECTIVAL GAP:
 *   From the deployer and vendor seats the arrangement computes as purchased coordination: a real throughput problem solved, fees paid willingly, harms unseen and statistically distant. From the trapped applicant seats the same structure computes as enforced gatekeeping: no exit, no contestation, error costs borne silently. The regulator seat sits between — formally empowered, practically reactive. The engine derives these divergent per-seat classifications from the declared power, exit, and role data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality for vendors and deployers (subsidized by the arrangement, with arbitrage-grade or mobile exit pushing them toward the beneficiary pole). Victim declarations map trapped and constrained payers toward the full-target pole — trapping amplifies effective extraction for the credit/rental and benefits seats especially, since their exit options are structurally nil rather than merely costly. Regulators derive near-symmetric directionality: they administer the arrangement without collecting from it or bearing its costs. No directionality_overrides are authored: the declared role-plus-exit combinations already differentiate every seat, and the override mechanism keys on power atoms, so any override would smear across distinct agents sharing an atom (e.g., the moderate-power auditor and applicant seats) and distort the derivation the structural data already produces correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — throughput and consistency at institutional scale — is live, so no mandatrophy resolution is declared and none is available. The tangled_rope claim earns its keep by blocking two symmetrical misreadings. Calling the arrangement a snare would erase the genuine coordination function: the volume problem is real, most processed decisions are adequate, and the reading itself does not propose abolishing automated decision-making — it proposes preventing and remediating its present harms, which presupposes the systems keep operating. Calling it a rope would erase the concentrated, identity-correlated error burden and the active machinery (lock-in, audit bans, trade-secret shielding) that holds the asymmetry in place. The hybrid classification keeps both faces visible and routes the dispute to the correct axis: not whether to coordinate, but who pays for the coordination's errors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is the nearterm_harms_reading of kernel ai_alignment_priority — what structural differences would instantiating existential_risk_reading or integrated_reading instead produce?',
    'Compile and classify the sibling stories (ai_alignment_priority__existential_risk_reading, ai_alignment_priority__integrated_reading) and compare victim sets, epsilon referents, and resource-flow structures across the kernel family.',
    'Under the existential reading the victim set shifts to future persons and the epsilon referent becomes control-preservation arrangements; under the integrated reading the victim set unions both populations and epsilon blends catastrophic and present-harm components. Seat-level classifications and network contamination paths change accordingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Kernel membership: this is one of three readings; siblings are separate constraints, not parts of this one.').

omega_variable(
    epsilon_referent_framing_choice,
    'Is the epsilon referent correctly fixed on the standing algorithmic-gatekeeping arrangement, or does a defensible alternative framing put the near-term-harms governance program itself (audit mandates, mitigation resource flows) under classification?',
    'Apply the epsilon-referent rule for kernel readings: a reading authors epsilon for the arrangement it contests, never for its endorsed alternative; verify the authored value tracks deployed-system harm incidence rather than program compliance rates.',
    'If the governance program were the referent, epsilon would fall toward coordination-cost levels and the type would drift rope/scaffold-ward; the high-extraction tangled_rope profile depends on the standing-arrangement framing adopted here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_referent_framing_choice, conceptual, 'Framing under-determination: standing deployment regime versus the reading''s own program as the classified object.').

omega_variable(
    fairness_metric_underdetermination,
    'The epsilon estimate aggregates disparate-impact evidence whose magnitude depends on the fairness criterion chosen (error-rate balance, calibration, demographic parity) — criteria that provably cannot all hold simultaneously when base rates differ.',
    'Report epsilon as a range across admissible criteria and weight criteria by the affected populations'' own redress interests rather than deployer-selected reporting metrics.',
    'A calibration-first accounting lowers measured extraction (equalized error where base rates differ); a parity-first accounting raises it. Type classification is stable but the value sits near the tangled_rope/snare boundary under criterion shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fairness_metric_underdetermination, empirical, 'Measured extraction depends on an underdetermined choice among incompatible fairness criteria.').

omega_variable(
    enforcement_trajectory_direction,
    'Will the arrangement''s enforcement machinery keep hardening (vendor lock-in, audit-access bans, trade-secret shielding, lobbying against mandates) or soften under binding regimes (EU AI Act high-risk conformity assessment, state audit statutes)?',
    'Extend the suppression_requirement series beyond T12 against enacted-mandate coverage; divergent regional trajectories indicate scope-split rather than global reversal.',
    'Continued hardening pushes payer seats toward full-target directionality and the arrangement toward snare; binding enforcement with functioning redress would pull it back toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_trajectory_direction, empirical, 'Direction of enforcement-capacity change is the pivotal open dynamic for lifecycle classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__nearterm_harms_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(ai_a_tr_t2, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 2, 0.26).
narrative_ontology:measurement(ai_a_tr_t4, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 4, 0.3).
narrative_ontology:measurement(ai_a_tr_t6, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 6, 0.35).
narrative_ontology:measurement(ai_a_tr_t8, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 8, 0.4).
narrative_ontology:measurement(ai_a_tr_t10, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 10, 0.44).
narrative_ontology:measurement(ai_a_tr_t12, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 12, 0.48).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(ai_a_be_t2, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 2, 0.61).
narrative_ontology:measurement(ai_a_be_t4, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 4, 0.64).
narrative_ontology:measurement(ai_a_be_t6, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 6, 0.66).
narrative_ontology:measurement(ai_a_be_t8, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 8, 0.69).
narrative_ontology:measurement(ai_a_be_t10, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 10, 0.71).
narrative_ontology:measurement(ai_a_be_t12, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 12, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(ai_a_su_t2, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 2, 0.49).
narrative_ontology:measurement(ai_a_su_t4, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 4, 0.53).
narrative_ontology:measurement(ai_a_su_t6, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 6, 0.57).
narrative_ontology:measurement(ai_a_su_t8, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 8, 0.61).
narrative_ontology:measurement(ai_a_su_t10, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(ai_a_su_t12, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 12, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__nearterm_harms_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority__integrated_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'AI alignment' decomposes into three structurally distinct readings of one kernel (ai_alignment_priority). This file authors only the nearterm_harms_reading: epsilon's referent is the standing algorithmic-gatekeeping arrangement assessed by present-harm lights (high, ~0.72), with victims drawn from present marginalized populations. The existential_risk_reading authors epsilon for control-preservation arrangements with a future-persons victim set; the integrated_reading unions both referents. The readings are linked as family members in mutual contest — the existential reading currently holds upstream legitimacy and funding weight that this reading contests, and the integrated reading mediates — not as causal dependencies. Each member keeps its own epsilon, beneficiaries, and claimed type per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
