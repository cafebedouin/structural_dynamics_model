% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__nearterm_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Near-Term Harms Reading of AI Alignment Priority
 *   domain: AI governance/technology ethics/risk assessment
 *
 * SUMMARY:
 *   This story instantiates the near-term-harms reading of the contested 'AI
 *   alignment priority' kernel: alignment is defined as the elimination of
 *   present, documentable discriminatory and extractive harms from deployed
 *   systems — hiring screens, credit models, predictive policing tools — with
 *   justice for marginalized populations (by age, race, and disability
 *   status) as the governing priority. Under this reading, resources,
 *   institutional legitimacy, and the definition of 'doing alignment work'
 *   flow to fairness-audit methodology, disparate-impact litigation, and
 *   bias-mitigation engineering. This is NOT the existential-risk reading
 *   (which defines alignment as preventing catastrophic loss of control over
 *   advanced systems) nor the integrated reading (which treats both
 *   priorities as complementary) — those are separate constraints, linked
 *   here only via the kernel network, each with its own ε, victim set, and
 *   beneficiary structure.
 *
 * KEY AGENTS:
 *   - fairness_audit_practitioners: agenda_setter (organized/mobile) — defines and administers the audit methodology
 *   - present_marginalized_users / algorithmically_screened_job_applicants / predictive_policing_targets / credit_scored_low_income_borrowers / disabled_applicants_of_hiring_ai: primary beneficiaries-in-name and simultaneous bearers of ongoing harm (powerless/trapped) — the named victim-beneficiary class this reading exists to serve
 *   - ai_ethics_compliance_teams: beneficiary/agenda_setter (organized/mobile) — professional and budgetary stake in this reading remaining the operative definition
 *   - ai_developers_and_deployers: payer/beneficiary (institutional/constrained) — bear audit and remediation cost, benefit from a bounded compliance target
 *   - existential_risk_researchers: excluded (organized/analytical) — structurally outside this reading's scope of what counts as alignment
 *   - policy_analytical_observer: observer (analytical) — traces the kernel contest across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__nearterm_harms_reading, 0.68).
domain_priors:suppression_score(ai_alignment_priority__nearterm_harms_reading, 0.52).
domain_priors:theater_ratio(ai_alignment_priority__nearterm_harms_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__nearterm_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__nearterm_harms_reading, "Near-Term Harms Reading of AI Alignment Priority").
narrative_ontology:topic_domain(ai_alignment_priority__nearterm_harms_reading, "AI governance/technology ethics/risk assessment").

domain_priors:requires_active_enforcement(ai_alignment_priority__nearterm_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__nearterm_harms_reading, '6a34c30a-4cb1-43f7-9016-1ba37aa8b6c7').
narrative_ontology:cs_kernel_codification('6a34c30a-4cb1-43f7-9016-1ba37aa8b6c7', distributed).
narrative_ontology:cs_authority_grounding('6a34c30a-4cb1-43f7-9016-1ba37aa8b6c7', distributed).
narrative_ontology:cs_reading_relation('6a34c30a-4cb1-43f7-9016-1ba37aa8b6c7', ai_alignment_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('6a34c30a-4cb1-43f7-9016-1ba37aa8b6c7', ai_alignment_priority__integrated_reading, influences).
narrative_ontology:cs_axiom('6a34c30a-4cb1-43f7-9016-1ba37aa8b6c7', foundational, present_identifiable_harm_has_moral_priority).
narrative_ontology:cs_axiom_status(present_identifiable_harm_has_moral_priority, holdable).
narrative_ontology:cs_axiom_grounding('6a34c30a-4cb1-43f7-9016-1ba37aa8b6c7', present_identifiable_harm_has_moral_priority, deontological).
narrative_ontology:cs_axiom('6a34c30a-4cb1-43f7-9016-1ba37aa8b6c7', secondary, disparate_impact_audit_is_primary_alignment_method).
narrative_ontology:cs_axiom_status(disparate_impact_audit_is_primary_alignment_method, holdable).
narrative_ontology:cs_axiom_grounding('6a34c30a-4cb1-43f7-9016-1ba37aa8b6c7', disparate_impact_audit_is_primary_alignment_method, instrumental).
narrative_ontology:cs_reference_frame('6a34c30a-4cb1-43f7-9016-1ba37aa8b6c7', civil_rights_disparate_impact_framework).
narrative_ontology:cs_drift_state('6a34c30a-4cb1-43f7-9016-1ba37aa8b6c7', post_generative_ai_deployment_surge, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6a34c30a-4cb1-43f7-9016-1ba37aa8b6c7', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, present_marginalized_users).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, fairness_audit_practitioners).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, ai_ethics_compliance_teams).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, algorithmically_screened_job_applicants).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, predictive_policing_targets).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, credit_scored_low_income_borrowers).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, disabled_applicants_of_hiring_ai).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, algorithmically_screened_job_applicants).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, ai_developers_and_deployers).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, ai_developers_and_deployers).
narrative_ontology:constraint_vindicates(ai_alignment_priority__nearterm_harms_reading, distributive_justice_priority_doctrine).
narrative_ontology:constraint_vindicates(ai_alignment_priority__nearterm_harms_reading, present_harm_primacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the methodology by which deployed systems are audited for disparate impact — defines the metrics (demographic parity, equalized odds), runs the audits, and certifies remediation. Controls what counts as 'aligned' under this reading and administers the enforcement mechanism (compliance requirements, disclosure mandates, litigation support).
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, fairness_audit_practitioners, agenda_setter,
    organized, biographical, mobile, national).

% Subject to deployed hiring, lending, and policing algorithms right now. Gains from this reading: bias audits, disparate-impact remediation, and legal recourse target harms they are actually experiencing today. They cannot opt out of the systems that score them and have no independent capacity to audit those systems themselves.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, present_marginalized_users, beneficiary,
    powerless, immediate, trapped, national).

% Screened out or down-ranked by hiring algorithms with documented racial and disability-status disparities. Under this reading they are named victims whose harm justifies the entire framework, but the audit-and-remediate cycle is slow and any given rejected applicant bears the harm before, or instead of, remediation reaching them. They pay the cost of the system operating while its fairness properties are contested and litigated.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, algorithmically_screened_job_applicants, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__nearterm_harms_reading, algorithmically_screened_job_applicants, beneficiary).

% Residents of neighborhoods flagged by predictive policing tools trained on historically biased arrest data. Cannot exit the jurisdiction's policing apparatus; bear surveillance and enforcement intensity the algorithm recommends, largely without visibility into the model's operation or standing to contest a given flag before harm occurs.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, predictive_policing_targets, payer,
    powerless, immediate, trapped, regional).

% Denied or priced out of credit by scoring models that encode proxies for race and income history. Can in principle seek alternative lenders, but alternatives are scarce, more expensive, or nonexistent for the same population the algorithm disadvantages — exit is nominal, not real.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, credit_scored_low_income_borrowers, payer,
    powerless, biographical, constrained, national).

% Video-interview and resume-screening AI systematically misreads speech patterns, facial affect, and employment-gap history correlated with disability. Named as a core victim group under this reading; largely unable to detect why they were screened out, let alone contest it.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, disabled_applicants_of_hiring_ai, payer,
    powerless, immediate, trapped, national).

% Internal corporate and consultancy teams whose professional mandate and budget derive from this reading's framework: they run the bias audits, write the fairness reports, and administer remediation. Career and institutional standing are built on near-term-harms alignment being the operative definition; a shift toward the existential-risk reading would displace their function.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, ai_ethics_compliance_teams, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__nearterm_harms_reading, ai_ethics_compliance_teams, agenda_setter).

% Firms building and deploying the screened systems bear compliance costs (audits, remediation, litigation exposure, delayed launches) under this reading, but also benefit from a bounded, checklist-style compliance target that is more tractable and PR-legible than open-ended catastrophic-risk mitigation. Can lobby for weaker audit standards or relocate deployment to lower-scrutiny jurisdictions.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, ai_developers_and_deployers, payer,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__nearterm_harms_reading, ai_developers_and_deployers, beneficiary).

% Argue that resources and institutional attention devoted to near-term fairness audits are diverted from catastrophic-risk research on frontier model capability control. Structurally outside this reading's conversation — this reading's framing treats their priority as a distraction from present, documentable harm to identifiable people.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, existential_risk_researchers, excluded,
    organized, civilizational, analytical, global).

% Studies how the alignment-priority kernel is read differently by different institutional actors and traces resource allocation, audit outcomes, and harm trajectories across the near-term and existential readings without being a party to either.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, policy_analytical_observer, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_priority__nearterm_harms_reading, fairness_audit_practitioners).
narrative_ontology:fixing_cost_class(ai_alignment_priority__nearterm_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates auditing, disclosure, and remediation practices across firms deploying algorithmic decision systems so that discriminatory impacts on protected and vulnerable groups are detected and corrected using a shared, comparable methodology rather than each firm inventing its own fairness standard ad hoc.
% TRANSFER_FUNCTION: Moves compliance cost, audit labor, and remediation investment from firms and their shareholders toward the marginalized populations whose treatment the audits target; simultaneously moves professional and budgetary standing toward audit practitioners and compliance teams, and moves institutional attention and funding away from catastrophic-risk research relative to the sibling reading.
% ABSENT_VOICES: Existential-risk researchers are structurally excluded from this reading's framing — they would argue the near-term focus under-resources catastrophic-risk work, but this reading's kernel definition treats their priority as outside its scope of what counts as 'alignment.' Individually harmed applicants and borrowers are also rarely present in the audit-design process itself, despite being the named beneficiary class.
% DISAPPEARANCE_RATIONALE: If this reading of alignment vanished, fairness-audit practitioners and compliance teams would lose their institutional mandate, and firms would face reduced near-term legal and reputational pressure over disparate impact — marginalized populations subject to hiring, lending, and policing algorithms would lose an active (if imperfect) remediation channel. Whether the 'world rearranges' or 'stays the same' is itself contested between this reading's proponents (who say real, present harm would go unaddressed) and existential-risk proponents (who say resources would simply flow to what they consider the more consequential problem).
% FOUNDING_PROBLEM: Deployed algorithmic systems (hiring screens, credit models, predictive policing tools) were documented causing disparate, discriminatory outcomes against protected groups, with no established audit or remediation mechanism holding deployers accountable in real time.
% FOUNDING_PROBLEM_CORROBORATION: Independent academic audits (e.g., peer-reviewed studies of hiring-algorithm and recidivism-model disparate impact), investigative journalism, and civil-rights litigation outcomes corroborate ongoing discriminatory impact from outside the compliance-team and audit-practitioner beneficiary set. Regulatory agencies (in jurisdictions with algorithmic accountability statutes) independently attest the problem remains active, not resolved.
narrative_ontology:disappearance_verdict(ai_alignment_priority__nearterm_harms_reading, contested).
narrative_ontology:founding_problem_status(ai_alignment_priority__nearterm_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__nearterm_harms_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_alignment_priority__nearterm_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__nearterm_harms_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.68) is authored high because deployed-system audits under this reading extract real, ongoing harm from specific, identifiable populations (job applicants, loan applicants, policed residents) whose exposure is continuous while the audit-and-remediation cycle lags behind deployment cycles — the harm is current-tense even when a remediation program exists. Suppression (0.52) reflects that most affected individuals cannot contest an individual algorithmic decision in real time or exit the systems that score them; it is moderate rather than severe because audit and disclosure regimes, litigation, and regulatory attention provide partial, if slow, recourse. Theater ratio (0.40) reflects a documented tendency for compliance programs to produce audit reports and fairness dashboards that satisfy disclosure requirements without necessarily changing deployed-model behavior in proportion to the harm measured — a genuine risk under any purely metrics-driven fairness regime. Accessibility collapse (0.35) is moderate-low: unlike a mountain, real alternative institutional arrangements exist (stronger liability regimes, pre-deployment certification, banning certain use-cases outright) and are actively debated, so alternatives have not collapsed. Resistance (0.62) is authored high because affected communities, civil-rights organizations, and litigation actively contest both the harms and the adequacy of the audit regime — this is a live, contested arrangement, not a settled one.
 *
 * DIRECTIONALITY LOGIC:
 *   Present marginalized users and the four named victim groups are declared as both the reading's justificatory beneficiary class (the entire framework exists to serve their interests) and structural payers (they are the ones actually screened, policed, and scored) — this dual role is intentional: this reading's moral claim is that their present suffering under deployed AI is the alignment problem, and its structural weakness is that remediation is slower than harm accrual, so the same population occupies both roles simultaneously. Fairness-audit practitioners and compliance teams are pure structural beneficiaries: their professional field and institutional mandate are constituted by this reading being the operative definition of alignment. Developers/deployers are payers of compliance cost but also beneficiaries of a bounded, legible compliance target relative to open-ended catastrophic-risk mitigation — this dual position reflects genuine industry preference for near-term reading's tractability.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (documented discriminatory algorithmic harm with no accountability mechanism) is authored as live, not dead — audits, litigation, and journalism continue to surface new instances. This blocks a mandatrophy verdict: the arrangement's justification has not evaporated even though its administering class (audit practitioners, compliance teams) has an obvious institutional interest in the problem being framed as perpetually live. The tangled-rope classification captures this precisely: there IS a genuine coordination function (shared audit methodology beats each firm inventing its own fairness standard) AND genuine asymmetric extraction (the harm continues to accrue to specific populations while remediation lags, and compliance-industry incentives may extend the audit-and-report cycle beyond what remediation strictly requires).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_zero_sum_ambiguity,
    'Is institutional/funding attention to near-term fairness harms genuinely in zero-sum competition with existential-risk research, or can both be pursued without meaningful tradeoff?',
    'Track whether organizations that increase near-term fairness audit investment show measurably reduced capability-control research output (or vice versa) across a multi-year panel of AI labs and policy bodies; absence of correlation would support the integrated reading''s premise.',
    'If genuinely zero-sum, this reading''s prioritization of near-term harms structurally extracts resources from existential-risk mitigation, strengthening the case that the readings are in real competition rather than complementary. If not zero-sum, the near-term reading''s implicit framing of existential-risk work as ''distraction'' is less defensible, and the integrated reading gains support.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(resource_zero_sum_ambiguity, empirical, 'Whether near-term and existential alignment priorities structurally compete for the same finite resources.').

omega_variable(
    audit_remediation_lag_severity,
    'How large is the gap between when an audit detects disparate impact and when deployed systems are actually remediated, and does that gap itself constitute a distinct extraction mechanism (harm continues to accrue during the lag) or an acceptable transitional cost of a genuinely functioning coordination mechanism?',
    'Longitudinal tracking of specific deployed systems from audit-flagged date to remediation-verified date, cross-referenced with harm incidence during the lag window.',
    'A long, harm-accruing lag would push this constraint''s classification toward snare (extraction with cosmetic coordination cover); a short, genuinely corrective lag supports the tangled_rope reading (real coordination function coexisting with real but bounded extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(audit_remediation_lag_severity, empirical, 'Whether the audit-to-remediation lag is itself an extraction mechanism.').

omega_variable(
    kernel_framing_choice_omega,
    'Is the choice to read ''alignment priority'' as fundamentally about present distributive justice (rather than catastrophic risk, or both) itself a defensible reading of the underlying commitment, or does it presuppose a contestable moral framework (near-term identifiable-victim harms take normative priority over diffuse future-probability harms)?',
    'Compare against how the sibling readings frame the same underlying kernel; document that both are internally coherent given different premises about moral priority under uncertainty (certain present harm to identifiable people vs. uncertain future catastrophic harm to unidentified people) — this is a genuine conceptual fork, not an empirical one.',
    'If one premise is judged philosophically dominant, that reading''s constraint would be authored as the ''true'' kernel resolution and the others as derivative or subordinate; under the ε-invariance principle we instead keep all three as separate, co-equal constraints linked by network edges rather than adjudicating between them here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice_omega, conceptual, 'Whether prioritizing present identifiable harm over diffuse future risk is itself a value choice this reading makes without fully surfacing it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__nearterm_harms_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_a_tr_t4, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(ai_a_tr_t8, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(ai_a_tr_t12, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(ai_a_tr_t16, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(ai_a_tr_t24, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ai_a_be_t4, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(ai_a_be_t8, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 8, 0.56).
narrative_ontology:measurement(ai_a_be_t12, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 12, 0.61).
narrative_ontology:measurement(ai_a_be_t16, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(ai_a_be_t24, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ai_a_su_t4, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 4, 0.36).
narrative_ontology:measurement(ai_a_su_t8, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 8, 0.41).
narrative_ontology:measurement(ai_a_su_t12, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 12, 0.45).
narrative_ontology:measurement(ai_a_su_t16, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(ai_a_su_t24, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__nearterm_harms_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_alignment_priority__nearterm_harms_reading, 0.1).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority__integrated_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language label 'AI alignment priority' per the ε-invariance principle: (1) nearterm_harms_reading (this file) — ε grounded in measured present disparate impact on named marginalized groups; (2) existential_risk_reading — ε grounded in speculative catastrophic loss-of-control scenarios, an entirely different evidentiary basis and victim set; (3) integrated_reading — treats both as complementary, with its own beneficiary/victim structure spanning both present and long-term populations. The three are linked via affects_constraints rather than merged, because averaging or parameterizing a single ε across them would violate DP-001 (ε-invariance) — each reading is measured by a genuinely different observable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_alignment_priority__nearterm_harms_reading, organized, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
