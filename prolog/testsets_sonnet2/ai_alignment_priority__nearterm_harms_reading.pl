% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__nearterm_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: ai_alignment_priority__nearterm_harms_reading
 *   human_readable: AI Alignment Priority — Near-term Discriminatory Harms Reading
 *   domain: AI Governance / Technology Ethics / Risk Assessment
 *
 * SUMMARY:
 *   This story instantiates the near-term-harms reading of the contested 'AI
 *   alignment priority' kernel: alignment work should be judged by whether it
 *   reduces present, measurable discriminatory and extractive harms from
 *   already-deployed systems — credit scoring, hiring screens, benefits
 *   eligibility, gig-platform management — with priority given to justice for
 *   marginalized populations (racial minorities, disabled users, elderly
 *   claimants, gig workers). This reading treats existing sociotechnical
 *   audit methodology as the legitimate evidentiary standard and directs
 *   resources toward bias mitigation rather than toward catastrophic-risk
 *   research. The sibling readings — existential-risk and integrated — are
 *   NOT described here except as named siblings in the kernel contest; they
 *   are separate constraint stories with their own ε, victim sets, and
 *   beneficiary structures.
 *
 * KEY AGENTS:
 *   - racially_marginalized_loan_and_hiring_applicants: primary target (powerless/trapped) — bears discriminatory scoring outcomes
 *   - disabled_users_of_automated_screening_systems: primary target (powerless/trapped) — bears calibration mismatch harms
 *   - elderly_users_of_automated_benefits_systems: primary target (powerless/constrained) — bears wrongful denial and appeal-burden harms
 *   - gig_workers_subject_to_algorithmic_management: secondary target (moderate/constrained) — bears opaque management harms
 *   - deployed_system_operators: agenda-setter and secondary beneficiary (institutional/arbitrage) — sets system configuration, benefits from compliance-audit legitimacy
 *   - alignment_research_institutions_focused_on_bias: primary beneficiary and agenda-setter (organized/mobile) — sets audit methodology, receives resource flow
 *   - present_vulnerable_populations: conditional beneficiary (powerless/trapped) — benefits only if enforcement reaches deployed systems
 *   - existential_risk_research_community: excluded (organized/mobile) — deprioritized under this reading's resource allocation
 *   - ai_policy_regulators: analytical observer (institutional/analytical) — adjudicates statutory priority between readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__nearterm_harms_reading, 0.68).
domain_priors:suppression_score(ai_alignment_priority__nearterm_harms_reading, 0.58).
domain_priors:theater_ratio(ai_alignment_priority__nearterm_harms_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__nearterm_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__nearterm_harms_reading, "AI Alignment Priority — Near-term Discriminatory Harms Reading").
narrative_ontology:topic_domain(ai_alignment_priority__nearterm_harms_reading, "AI Governance / Technology Ethics / Risk Assessment").

domain_priors:requires_active_enforcement(ai_alignment_priority__nearterm_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__nearterm_harms_reading, '46bb8e62-7f54-4fd0-ae29-57612ecd620b').
narrative_ontology:cs_kernel_codification('46bb8e62-7f54-4fd0-ae29-57612ecd620b', distributed).
narrative_ontology:cs_authority_grounding('46bb8e62-7f54-4fd0-ae29-57612ecd620b', distributed).
narrative_ontology:cs_reading_relation('46bb8e62-7f54-4fd0-ae29-57612ecd620b', ai_alignment_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('46bb8e62-7f54-4fd0-ae29-57612ecd620b', ai_alignment_priority__integrated_reading, influences).
narrative_ontology:cs_axiom('46bb8e62-7f54-4fd0-ae29-57612ecd620b', foundational, present_measurable_harm_has_lexical_priority_over_speculative_future_harm).
narrative_ontology:cs_axiom_status(present_measurable_harm_has_lexical_priority_over_speculative_future_harm, holdable).
narrative_ontology:cs_axiom_grounding('46bb8e62-7f54-4fd0-ae29-57612ecd620b', present_measurable_harm_has_lexical_priority_over_speculative_future_harm, deontological).
narrative_ontology:cs_axiom('46bb8e62-7f54-4fd0-ae29-57612ecd620b', foundational, justice_for_marginalized_populations_is_the_evaluative_criterion_for_alignment_success).
narrative_ontology:cs_axiom_status(justice_for_marginalized_populations_is_the_evaluative_criterion_for_alignment_success, holdable).
narrative_ontology:cs_axiom_grounding('46bb8e62-7f54-4fd0-ae29-57612ecd620b', justice_for_marginalized_populations_is_the_evaluative_criterion_for_alignment_success, deontological).
narrative_ontology:cs_axiom('46bb8e62-7f54-4fd0-ae29-57612ecd620b', secondary, sociotechnical_audit_methodology_is_the_legitimate_evidentiary_standard).
narrative_ontology:cs_axiom_status(sociotechnical_audit_methodology_is_the_legitimate_evidentiary_standard, holdable).
narrative_ontology:cs_axiom_grounding('46bb8e62-7f54-4fd0-ae29-57612ecd620b', sociotechnical_audit_methodology_is_the_legitimate_evidentiary_standard, conventional).
narrative_ontology:cs_reference_frame('46bb8e62-7f54-4fd0-ae29-57612ecd620b', civil_rights_and_algorithmic_accountability_tradition).
narrative_ontology:cs_drift_state('46bb8e62-7f54-4fd0-ae29-57612ecd620b', contemporary_ai_governance_debate, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('46bb8e62-7f54-4fd0-ae29-57612ecd620b', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, deployed_system_operators).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, alignment_research_institutions_focused_on_bias).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, present_vulnerable_populations).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, racially_marginalized_loan_and_hiring_applicants).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, disabled_users_of_automated_screening_systems).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, elderly_users_of_automated_benefits_systems).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, gig_workers_subject_to_algorithmic_management).
narrative_ontology:constraint_vindicates(ai_alignment_priority__nearterm_harms_reading, distributive_justice_as_primary_alignment_criterion).
narrative_ontology:constraint_vindicates(ai_alignment_priority__nearterm_harms_reading, sociotechnical_audit_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Subject to automated credit, hiring, and screening decisions built on historical data that encodes prior discrimination. They cannot opt out of the systems that decide whether they get loans or jobs, cannot see the model internals, and have limited recourse beyond formal complaint processes that rarely reverse decisions in time to matter.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, racially_marginalized_loan_and_hiring_applicants, payer,
    powerless, biographical, trapped, national).

% Encounter automated video interview scoring, resume filtering, and benefits eligibility systems calibrated on non-disabled populations, producing systematically lower scores for atypical speech, movement, or work history patterns. Appeals require documentation burdens that are themselves disabling.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, disabled_users_of_automated_screening_systems, payer,
    powerless, biographical, trapped, national).

% Depend on automated eligibility determination for pensions, healthcare, and social benefits; face wrongful denials from models that treat atypical usage patterns as fraud signals. Digital literacy and mobility barriers make appeal processes disproportionately costly for this group relative to younger claimants.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, elderly_users_of_automated_benefits_systems, payer,
    powerless, biographical, constrained, national).

% Assigned work, rated, and deactivated by opaque optimization systems that this reading treats as present, measurable, auditable harm. Some organize collectively (driver associations) but individual exit means loss of income with no equivalent platform to move to.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, gig_workers_subject_to_algorithmic_management, payer,
    moderate, biographical, constrained, global).

% Build and deploy the scoring, screening, and allocation systems. Under this reading they are the object of audit and remediation obligations; they benefit reputationally and legally from being able to point to fairness audits and bias-mitigation compliance, and set the actual configuration of the systems being audited.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, deployed_system_operators, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__nearterm_harms_reading, deployed_system_operators, beneficiary).

% Fairness/bias research groups, sociotechnical audit labs, and civil-society AI-justice organizations whose funding, staffing, and institutional standing depend on near-term-harms alignment being recognized as THE alignment priority. They set audit methodology and receive grant and consulting resources tied to this framing.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, alignment_research_institutions_focused_on_bias, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__nearterm_harms_reading, alignment_research_institutions_focused_on_bias, agenda_setter).

% The intended beneficiaries of bias mitigation work: the same marginalized groups named as victims of unmitigated systems become beneficiaries when audits succeed in changing model behavior or securing remediation. Their benefit is conditional on enforcement actually reaching deployed systems, not merely being published as research.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, present_vulnerable_populations, beneficiary,
    powerless, biographical, trapped, national).

% Researchers prioritizing catastrophic misalignment and loss-of-control risk are structurally deprioritized under this reading's resource allocation — funding, conference space, and policy attention directed toward near-term harms is, in their view, attention and capital not directed toward catastrophic scenarios. They are not absent from AI governance broadly, but are excluded from THIS constraint's priority-setting.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, existential_risk_research_community, excluded,
    organized, civilizational, mobile, global).

% Draft and enforce algorithmic accountability law, taking testimony from advocacy groups, operators, and researchers on both sides of the kernel contest, and deciding which harms get statutory priority and audit-mandate funding.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, ai_policy_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_priority__nearterm_harms_reading, alignment_research_institutions_focused_on_bias).
narrative_ontology:fixing_cost_class(ai_alignment_priority__nearterm_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a genuine, identifiable set of actors — auditors, civil-society organizations, and regulators — around detecting and remediating discriminatory or extractive outcomes in already-deployed systems, replacing ad hoc individual complaint with systematic sociotechnical audit methodology.
% TRANSFER_FUNCTION: Moves research funding, regulatory attention, compliance-audit revenue, and remediation obligations from general AI-safety budgets and system operators toward bias-mitigation research institutions and audit bodies; moves (when successful) material outcomes — loan approvals, job callbacks, benefits continuity — from denial back toward marginalized applicants.
% ABSENT_VOICES: The existential-risk research community is structurally absent from this constraint's priority-setting: they would argue that resources spent auditing present discriminatory outputs are resources not spent on catastrophic-risk research, and that framing near-term justice as THE alignment priority under-resources scenarios with irreversible stakes. They participate in AI governance generally but are excluded from this reading's own priority-setting apparatus.
% DISAPPEARANCE_RATIONALE: If this priority-reading vanished overnight — if no institutional actor treated near-term discriminatory harm as an alignment priority — sociotechnical audits would lose funding and legal mandate, marginalized populations subject to automated screening would lose their primary institutional advocates, and deployed-system operators would lose the compliance framework currently structuring (however imperfectly) their remediation obligations. The world of automated decision systems would reorganize around whatever priority reading filled the vacuum.
% FOUNDING_PROBLEM: Deployed automated systems (credit scoring, hiring screens, benefits eligibility, gig-platform management) were producing measurable discriminatory outcomes against protected and vulnerable populations well before any catastrophic-risk scenario materialized, and no existing regulatory or research apparatus was treating this as an 'alignment' problem rather than a narrower anti-discrimination or consumer-protection problem.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting bias-research institutions by independent investigative journalism documenting specific denial and scoring harms, by court findings in algorithmic discrimination litigation, and by regulator (ai_policy_regulators seat) testimony records — all attesting the underlying discriminatory outcomes are ongoing and empirically measurable, not merely a framing convenient to the research institutions that study them.
narrative_ontology:disappearance_verdict(ai_alignment_priority__nearterm_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__nearterm_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__nearterm_harms_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored at 0.68 because the standing arrangement under contest — deployed automated systems producing discriminatory outcomes against powerless populations with no meaningful individual exit — is substantially extractive by this reading's own lights; this is the referent, not the audit-remediation state this reading endorses as fix. Suppression (0.58) reflects real but partial structural barriers: appeal processes exist but are burdensome, not absent, and are compounded for disabled and elderly claimants by documentation and mobility costs. Theater ratio (0.42) reflects a real concern within this reading itself: audits and fairness reports are sometimes produced as compliance artifacts without corresponding changes to deployed model behavior, and this ratio is rising across the measured interval as audit-industry incentives partially decouple from remediation outcomes. Accessibility collapse is moderate (0.40) — some workable individual and collective remedies exist (litigation, organizing, regulatory complaint) even though systemic access to fair treatment inside automated systems has substantially narrowed. Resistance is high (0.72): affected populations, civil-society groups, and litigators actively contest these systems, unlike a mountain which would meet almost none.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (system operators), this looks like a rope: a coordination mechanism (fairness audits, compliance frameworks) that lets them demonstrate good-faith operation. From the payer seats (marginalized applicants, disabled users, elderly claimants, gig workers), the same structure computes as tangled — real coordination function (audits do sometimes change outcomes) layered with real extraction (the underlying discriminatory system persists, generates measurable harm, and requires ongoing enforcement pressure to produce any remediation at all). The engine's per-seat computation should surface this divergence structurally rather than the claim adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   The four named victim groups sit near the full-target end: powerless or moderate power, trapped or constrained exit, and no meaningful alternative to the automated systems that score them. Deployed system operators sit as agenda-setters who benefit secondarily through compliance legitimacy without bearing the harm themselves — arbitrage-grade exit (they can reconfigure or relocate systems more easily than affected populations can escape them). The bias-research institutions are the clearest structural beneficiary of THIS READING specifically: their funding, standing, and methodological authority are constituted by near-term-harms alignment being recognized as the priority, independent of whether remediation actually reaches deployed systems — this is why gain_flow names them rather than the vulnerable populations whose benefit is conditional and often unrealized.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (measurable discriminatory outcomes from deployed automated systems) remains live by corroboration from litigation records and independent journalism — this is not a dead mandate propped up by inertia. But the mandatrophy risk is real at the enforcement layer: if audit and compliance activity substitutes for actual remediation (rising theater_ratio), the constraint could drift from tangled_rope toward something closer to a snare wearing coordination language, or toward a piton if audit bodies persist performatively after operators route around them. Classifying this as tangled_rope rather than either pure rope or pure snare prevents mislabeling a genuine, contested coordination function (sociotechnical audit as evidentiary standard) as either wholly benign or wholly extractive when both elements are structurally present and verified by different seats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    priority_reading_resource_competition,
    'Does directing alignment-research funding and regulatory attention toward near-term discriminatory harms structurally reduce resources available to existential-risk research, or do the two draw from largely separate funding pools such that this reading''s resource claims do not actually compete with the sibling readings?',
    'Longitudinal tracking of AI-safety philanthropic and government funding allocations by stated research priority, cross-referenced against total AI-safety-adjacent funding growth to distinguish substitution from pool growth.',
    'If the pools are shared and roughly fixed, this reading''s rise increases the correctness of a resource-competition framing and increases ε on the tangled_rope''s enforcement/gatekeeping dimension against the excluded existential_risk_research_community seat. If the pools are largely separate and growing, the exclusion is less structurally consequential and the constraint looks closer to a clean rope for the research-funding dimension specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(priority_reading_resource_competition, empirical, 'Whether near-term and existential-risk alignment funding are substitutes or complements.').

omega_variable(
    audit_remediation_gap,
    'What fraction of sociotechnical bias audits produce actual changes to deployed model behavior versus functioning as compliance documentation with no behavioral consequence?',
    'Independent longitudinal study comparing audit findings against subsequent model version changes and outcome-distribution shifts for the audited populations.',
    'A high remediation-gap would support reclassifying enforcement activity as substantially theatrical, pushing the constraint toward piton at the audit-institution seat even while the underlying discriminatory harm remains snare-like toward victims. A low gap would support the tangled_rope classification as authored, with genuine coordination function intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(audit_remediation_gap, empirical, 'Whether audit activity translates into deployed-system behavior change.').

omega_variable(
    priority_framing_vs_kernel_contest_location,
    'Is the disagreement between this reading and the existential-risk reading located in a genuine empirical disagreement about which harms are more probable/severe, or in an irreducible values disagreement about how to weigh certain-but-bounded present harm against uncertain-but-catastrophic future harm?',
    'This is not resolvable by further data alone if the disagreement is values-located; it would require philosophical/decision-theoretic analysis of how the parties themselves justify their priority weighting, and whether any empirical update would in fact move them.',
    'If empirically located, the kernel contest could in principle be resolved by better risk estimates, making a future ''integrated_reading'' the natural convergence point. If values-located, the readings will coexist indefinitely regardless of evidence, supporting the coexists_with relation over any eventual foreclosure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(priority_framing_vs_kernel_contest_location, conceptual, 'Whether the kernel contest is empirical or normative in character.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__nearterm_harms_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(ai_a_tr_t0, observed).
narrative_ontology:measurement(ai_a_tr_t4, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 4, 0.3).
narrative_ontology:measurement_basis(ai_a_tr_t4, observed).
narrative_ontology:measurement(ai_a_tr_t8, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 8, 0.34).
narrative_ontology:measurement_basis(ai_a_tr_t8, observed).
narrative_ontology:measurement(ai_a_tr_t12, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 12, 0.37).
narrative_ontology:measurement_basis(ai_a_tr_t12, observed).
narrative_ontology:measurement(ai_a_tr_t16, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 16, 0.39).
narrative_ontology:measurement_basis(ai_a_tr_t16, observed).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(ai_a_tr_t20, projected).
narrative_ontology:measurement(ai_a_tr_t24, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement_basis(ai_a_tr_t24, projected).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(ai_a_be_t0, observed).
narrative_ontology:measurement(ai_a_be_t4, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 4, 0.55).
narrative_ontology:measurement_basis(ai_a_be_t4, observed).
narrative_ontology:measurement(ai_a_be_t8, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement_basis(ai_a_be_t8, observed).
narrative_ontology:measurement(ai_a_be_t12, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 12, 0.63).
narrative_ontology:measurement_basis(ai_a_be_t12, observed).
narrative_ontology:measurement(ai_a_be_t16, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement_basis(ai_a_be_t16, observed).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(ai_a_be_t20, projected).
narrative_ontology:measurement(ai_a_be_t24, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement_basis(ai_a_be_t24, projected).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(ai_a_su_t0, observed).
narrative_ontology:measurement(ai_a_su_t4, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 4, 0.49).
narrative_ontology:measurement_basis(ai_a_su_t4, observed).
narrative_ontology:measurement(ai_a_su_t8, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement_basis(ai_a_su_t8, observed).
narrative_ontology:measurement(ai_a_su_t12, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 12, 0.54).
narrative_ontology:measurement_basis(ai_a_su_t12, observed).
narrative_ontology:measurement(ai_a_su_t16, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 16, 0.56).
narrative_ontology:measurement_basis(ai_a_su_t16, observed).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 20, 0.57).
narrative_ontology:measurement_basis(ai_a_su_t20, projected).
narrative_ontology:measurement(ai_a_su_t24, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement_basis(ai_a_su_t24, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__nearterm_harms_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_alignment_priority__nearterm_harms_reading, 0.12).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, existential_risk_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, integrated_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the ai_alignment_priority kernel. existential_risk_reading authors high ε on catastrophic loss-of-control scenarios with a diffuse, civilizational victim set; nearterm_harms_reading (this story) authors high ε on deployed-system audits with a specific, named marginalized-population victim set; integrated_reading treats both as complementary and authors a blended coordination structure without reducing either priority to instrumental status for the other. The three share the kernel but are structurally distinct constraints with different ε, different beneficiaries, and different victims — they must not be merged or averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
