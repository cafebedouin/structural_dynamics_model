% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__nearterm_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Near-Term Harms Reading of AI Alignment Priority: Deployed-System Bias Audit and Mitigation Regime
 *   domain: technology_ethics/ai_governance
 *
 * SUMMARY:
 *   A governance arrangement has consolidated around one definition of AI
 *   alignment work: preventing present discriminatory and extractive harms
 *   from deployed systems, with justice for marginalized populations as the
 *   priority ordering. Its operational signature is the mandatory
 *   sociotechnical bias audit of deployed systems, mitigation requirements
 *   keyed to audit findings, documentation duties that create complaint-ready
 *   records, and resource flows channeled into bias measurement and
 *   remediation. The arrangement solves a real collective-action problem — no
 *   individual victim can detect population-level disparate error, and no
 *   individual deployer captures enough benefit from unilateral fairness
 *   investment — while simultaneously transferring audit fees, compliance
 *   burdens, and field-wide attention toward the audit-and-justice complex.
 *   KEY AGENTS (by structural relationship): - marginalized_user_populations:
 *   protected class (powerless/trapped) — bears the underlying discriminatory
 *   harms; receives the arrangement's audits, documentation, and redress
 *   channels - algorithmic_justice_research_community: collecting beneficiary
 *   (moderate/identity_locked) — audit mandates are its funding base and
 *   career ladder - civil_rights_advocacy_orgs: organizing beneficiary
 *   (organized/constrained) — gains statutory hooks and data access -
 *   commercial_ai_deployers: primary payer (powerful/mobile) — bears audit,
 *   redesign, delay, and liability costs - small_ai_vendors: secondary payer
 *   (moderate/constrained) — fixed compliance costs weigh disproportionately
 *   - regulator_coalition: agenda setter (institutional/analytical) — defines
 *   audit adequacy, scope, and penalties - informal_economy_workers: excluded
 *   voice (powerless/trapped) — governed by uncovered management algorithms -
 *   interdisciplinary_technology_assessors: analytical observer — evaluates
 *   whether audits reduce harm or move money
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__nearterm_harms_reading, 0.52).
domain_priors:suppression_score(ai_alignment_priority__nearterm_harms_reading, 0.38).
domain_priors:theater_ratio(ai_alignment_priority__nearterm_harms_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__nearterm_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__nearterm_harms_reading, "Near-Term Harms Reading of AI Alignment Priority: Deployed-System Bias Audit and Mitigation Regime").
narrative_ontology:topic_domain(ai_alignment_priority__nearterm_harms_reading, "technology_ethics/ai_governance").

domain_priors:requires_active_enforcement(ai_alignment_priority__nearterm_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__nearterm_harms_reading, '8eab16b6-94d7-4dd7-b05f-ec8b09032bbf').
narrative_ontology:cs_kernel_codification('8eab16b6-94d7-4dd7-b05f-ec8b09032bbf', distributed).
narrative_ontology:cs_authority_grounding('8eab16b6-94d7-4dd7-b05f-ec8b09032bbf', distributed).
narrative_ontology:cs_reading_relation('8eab16b6-94d7-4dd7-b05f-ec8b09032bbf', ai_alignment_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('8eab16b6-94d7-4dd7-b05f-ec8b09032bbf', ai_alignment_priority__integrated_reading, influences).
narrative_ontology:cs_axiom('8eab16b6-94d7-4dd7-b05f-ec8b09032bbf', foundational, present_harms_morally_prior).
narrative_ontology:cs_axiom_status(present_harms_morally_prior, holdable).
narrative_ontology:cs_axiom_grounding('8eab16b6-94d7-4dd7-b05f-ec8b09032bbf', present_harms_morally_prior, deontological).
narrative_ontology:cs_axiom('8eab16b6-94d7-4dd7-b05f-ec8b09032bbf', foundational, sociotechnical_audit_sufficiency).
narrative_ontology:cs_axiom_status(sociotechnical_audit_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('8eab16b6-94d7-4dd7-b05f-ec8b09032bbf', sociotechnical_audit_sufficiency, instrumental).
narrative_ontology:cs_reference_frame('8eab16b6-94d7-4dd7-b05f-ec8b09032bbf', present_harms_justice_priority).
narrative_ontology:cs_drift_state('8eab16b6-94d7-4dd7-b05f-ec8b09032bbf', contemporary_codification_wave, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('8eab16b6-94d7-4dd7-b05f-ec8b09032bbf', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, marginalized_user_populations).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, algorithmic_justice_research_community).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, civil_rights_advocacy_orgs).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, commercial_ai_deployers).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, small_ai_vendors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% People in groups with documented disparate error rates: rejected job applicants, denied credit applicants, misidentified individuals, benefits claimants whose payments are stopped. They cannot opt out of being subject to algorithmic decisions in employment, credit, housing, and services, and no individual can see the population-level patterns that disadvantage them. The arrangement gives them standardized audits, documentation usable in complaints, and redress channels that did not previously exist.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, marginalized_user_populations, beneficiary,
    powerless, biographical, trapped, global).

% Academics, auditors, and consultants who develop and run sociotechnical bias assessments. Mandated audits, mitigation grants, and disclosure duties are their funding base and career ladder, and their methods define what counts as evidence of harm. Their professional identities are built around the audit mission; adopting a different methodology would mean rebuilding standing from scratch.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, algorithmic_justice_research_community, beneficiary,
    moderate, biographical, identity_locked, global).

% Organizations that litigate, campaign, and testify on discriminatory system outcomes. Audit mandates and documentation duties supply the data access and statutory hooks they previously lacked against opaque systems; their agendas and fundraising are tied to the arrangement's continued expansion.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, civil_rights_advocacy_orgs, beneficiary,
    organized, generational, constrained, national).

% Enterprises running AI in hiring, lending, insurance, advertising, and customer-facing decisions. They pay audit fees, redesign costs, and delayed-launch expenses, and carry liability exposure from documented disparities. Large firms absorb these costs and convert compliance capacity into market advantage; they can also shift product lines or concentrate launches in lighter jurisdictions.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, commercial_ai_deployers, payer,
    powerful, biographical, mobile, global).

% Startups selling screening, scoring, and targeting tools. Fixed audit and documentation costs weigh disproportionately against their budgets; some withdraw from regulated segments or delay features rather than comply, narrowing their addressable market.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, small_ai_vendors, payer,
    moderate, immediate, constrained, regional).

% Legislatures and agencies that mandate bias audits, define conformity assessment, condition public procurement on documented fairness testing, and penalize noncompliance. They decide what counts as an adequate audit, who may perform one, and which systems fall in scope; their calendars and enforcement budgets pace the arrangement's expansion.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, regulator_coalition, agenda_setter,
    institutional, generational, analytical, continental).

% Workers managed by scheduling, task-allocation, and rating algorithms on platforms and in informal labor markets. Audit mandates reach enterprise procurement and regulated high-risk systems but rarely the management algorithms that set their pay and shifts; they would argue for coverage of algorithmic management but hold no seat in standards processes.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, informal_economy_workers, excluded,
    powerless, immediate, trapped, global).

% Researchers and standards scholars who track whether mandated audits reduce measured disparities or mainly redistribute spending. They publish evaluations of audit quality and regime effects; they neither collect from the arrangement nor bear its costs.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, interdisciplinary_technology_assessors, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_priority__nearterm_harms_reading, algorithmic_justice_research_community).
narrative_ontology:fixing_cost_class(ai_alignment_priority__nearterm_harms_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Detects and remediates discriminatory error patterns in deployed AI that no individual actor can address alone: affected individuals cannot observe population-level disparate error rates, and individual deployers cannot profitably fund fairness infrastructure unilaterally without handing competitors an unburdened market. Standardized sociotechnical audits pool detection, mitigation requirements compel internalization of harm costs, and documentation duties create the evidentiary record that complaint and litigation require.
% TRANSFER_FUNCTION: Moves audit fees, compliance spending, and research funding from AI deployers and general field budgets toward bias-measurement and mitigation activity; moves remediation obligations and liability exposure onto deployers; moves protection, evidentiary standing, and redress channels to marginalized user populations; moves professional status and career security to the audit and justice-research community.
% ABSENT_VOICES: Workers and users governed by algorithmic management systems outside enterprise procurement scopes (gig platforms, informal labor), who are rarely covered by audit mandates; deployers in jurisdictions without mandates facing patchwork obligations; and affected people unable to participate in standards consultations dominated by regulators, large vendors, and established advocacy organizations.
% DISAPPEARANCE_RATIONALE: Audit infrastructure, documentation duties, and redress channels would vanish overnight; deployers would revert to unaudited deployment where detection depends on chance journalistic or academic discovery; the audit profession and its funding streams would dissolve; advocacy organizations would lose statutory hooks and data-access rights; and populations currently protected would again bear undetected disparate error rates at scale.
% FOUNDING_PROBLEM: Deployed AI systems in hiring, lending, benefits eligibility, advertising delivery, policing, and healthcare produced documented disparate errors — qualified candidates filtered out, credit denied, faces misidentified, benefits clawed back — at population scale, invisible to individual victims, unpriced in deployer cost structures, and practically impossible to litigate case-by-case.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: government measurement programs (national-institute face-recognition demographic-differential testing), independent academic audit experiments, investigative newsrooms, and deployers' own incident disclosures and settlements attest both the original harm pattern and its continuation in newer system generations. No attestation depends solely on the audit-and-advocacy complex whose funding the arrangement sustains.
narrative_ontology:disappearance_verdict(ai_alignment_priority__nearterm_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__nearterm_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__nearterm_harms_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_alignment_priority__nearterm_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__nearterm_harms_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is moderate (0.52): the core cost imposition on deployers functions largely as internalization of harm they would otherwise externalize, but a growing share is fee transfer to the assessor complex and compliance expenditure that produces artifacts rather than remediation. Suppression is moderate-low (0.38) and is authored as a raw structural property — the arrangement enforces through mandates, procurement conditions, and reputational pressure, not through coercive control of individuals; the engine, not this scalar, scales extractiveness by directionality and scope. Theater ratio (0.42) reflects the coexistence of genuine remediation with checkbox auditing and ethics-statement performance; it is rising as compliance routinizes. Accessibility collapse is low-moderate (0.40): alternatives persist — self-audit formats, auditor choice, jurisdictional variation, human-review fallbacks — so the arrangement does not close the option space the way a natural limit would. Resistance (0.50) is sustained: industry pushback, standards fights, and litigation run alongside grassroots demand that strengthens the mandate. The claimed type (tangled_rope) is stated from structure — genuine coordination function plus asymmetric extraction plus active enforcement — independently of these metric values; where the engine's per-seat computation diverges from the claim, that divergence is the datum. All three tracked series share one time grid (2016–2026, biennial) so no metric row is sampled against another metric's end-state.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structure. From the deployer seat the arrangement is an imposed cost regime — audit fees, redesign cycles, launch delays, liability exposure — and computes as extraction it bears. From the marginalized-population seat the same arrangement is protection and standing it could not purchase individually — it computes as subsidy. From the assessor-community seat it is a mandate that constitutes livelihood and disciplinary authority. The regulator seat experiences it as instrument. None of these perceptions is authored as a classification; the engine derives each from power, exit, and directional position.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries map to the low-d pole: marginalized_user_populations (subsidized, immobile), the assessor community (collects fees and standing), and advocacy organizations (collect standing and mandate). Declared payers map toward the high-d pole: commercial deployers and small vendors bear the transfers. One override is authored: for the powerless seat, derivation from trapped exit could wrongly pull d toward the target end, because immobility ordinarily signals capture; here immobility reflects dependence on the protection itself, so d is pinned near the beneficiary pole (0.08). The same override nominally touches informal_economy_workers, who sit outside the arrangement's scope and neither pay nor collect materially — an imprecision tolerated because excluded seats are commentary-grade and never drive classification. Deployer mobility dampens its effective d relative to a trapped payer; the assessor community's identity lock deepens its beneficiary position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: new system generations reopen the harm pattern, so the mandate has not outlived its function and mandatrophy is not resolved. The classification prevents two opposite mislabelings. Reading the arrangement as pure coordination ignores the measurable fee transfers, the compliance burden falling hardest on small vendors, and the rising theater share — the tangled-rope structure keeps the extraction visible inside a functioning coordination frame. Reading it as pure extraction erases the collective-action problem that gave it standing: without pooled audit methodology and compelled internalization, detection reverts to chance and redress to the individually impossible. The trajectory to watch is piton drift: theater_ratio rising past 0.5 with a flat harm profile would indicate mandates persisting as ritual while the assessor complex administers them; the cost asymmetry test would then ask whether the regulator coalition could re-tier audits toward outcome verification more cheaply than the ritual costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint instantiates the nearterm_harms_reading of the ai_alignment_priority kernel; how would the sibling readings restructure it?',
    'Comparative authoring of the sibling stories: the existential_risk_reading relocates the audit object from deployed-system outcomes to loss-of-control scenarios and the beneficiary class from present marginalized populations to future populations; the integrated_reading refuses the priority ordering and treats both harm classes as complementary, changing the resource-flow split. Classification differences across the family locate the disagreement structurally.',
    'Sibling readings would change the victim set, the beneficiary class, the audit object, and the resource-flow target, producing different epsilon values and plausibly different types; this file''s classification is valid only for the near-term reading''s structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings are separate constraints, not alternatives inside this one.').

omega_variable(
    audit_theater_share,
    'What share of mandated audit activity reduces measured disparities versus transfers fees for compliance artifacts?',
    'Longitudinal linkage studies connecting audit findings to post-audit outcome changes in the audited systems, controlling for concurrent non-audit remediation.',
    'A high theater share pushes the arrangement toward inertial drift within its tangled structure and supplies the evidence base for outcome-based reform; a low share supports the coordination framing and argues against deregulatory remedies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(audit_theater_share, empirical, 'Functional versus performative composition of the audit mandate.').

omega_variable(
    fairness_metric_validity,
    'Do the distributive metrics the regime standardizes (demographic parity, equalized odds, calibration within groups) capture the harms the affected populations actually experience?',
    'Participatory evaluation comparing metric-flagged cases with complainants'' own accounts of harm, and tracking harms that surface through channels the metrics do not monitor.',
    'Divergence would mean the arrangement remediates the measurable rather than the harmful — misdirecting its own resource flows by its own lights — and would support metric pluralism or qualitative evidence requirements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fairness_metric_validity, conceptual, 'Validity gap between standardized fairness metrics and lived harm.').

omega_variable(
    compliance_cost_incidence,
    'Who ultimately bears the deployer compliance costs — margins, consumer prices, vendor wages, or slowed product availability?',
    'Pass-through econometrics on regulated segments comparing pre- and post-mandate pricing, employment, and product-release patterns.',
    'If costs pass through to consumers and users, the arrangement partially taxes its own beneficiary class and the payer seats'' effective directionality softens; if absorbed by margins, the extraction concentrates on deployer equity as designed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compliance_cost_incidence, empirical, 'Final incidence of the compliance burden behind the nominal payer seats.').

omega_variable(
    assessor_identity_lock,
    'Is the audit community''s attachment to its methodology epistemic or position-protective?',
    'Head-to-head trials of alternative interventions (design-stage constraints, deployment restrictions, liability shifting) against audit-and-remediate, observing whether the community updates on evidence or defends the mandate.',
    'If position-protective, the theater ratio understates capture, reform resistance is identity-driven rather than evidentiary, and the beneficiary seat''s directionality should be read closer to a capturing seat than its declarations suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(assessor_identity_lock, empirical, 'Epistemic versus rent-protective basis of the assessor complex''s methodological commitment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__nearterm_harms_reading, 2016, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t2016, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 2016, 0.18).
narrative_ontology:measurement(ai_a_tr_t2018, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 2018, 0.24).
narrative_ontology:measurement(ai_a_tr_t2020, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 2020, 0.3).
narrative_ontology:measurement(ai_a_tr_t2022, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 2022, 0.35).
narrative_ontology:measurement(ai_a_tr_t2024, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 2024, 0.39).
narrative_ontology:measurement(ai_a_tr_t2026, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t2016, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 2016, 0.34).
narrative_ontology:measurement(ai_a_be_t2018, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 2018, 0.39).
narrative_ontology:measurement(ai_a_be_t2020, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 2020, 0.44).
narrative_ontology:measurement(ai_a_be_t2022, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 2022, 0.47).
narrative_ontology:measurement(ai_a_be_t2024, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 2024, 0.5).
narrative_ontology:measurement(ai_a_be_t2026, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 2026, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t2016, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 2016, 0.14).
narrative_ontology:measurement(ai_a_su_t2018, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 2018, 0.2).
narrative_ontology:measurement(ai_a_su_t2020, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 2020, 0.27).
narrative_ontology:measurement(ai_a_su_t2022, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 2022, 0.32).
narrative_ontology:measurement(ai_a_su_t2024, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 2024, 0.36).
narrative_ontology:measurement(ai_a_su_t2026, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 2026, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__nearterm_harms_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority__integrated_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'AI alignment' decomposes into structurally distinct priority regimes under the epsilon-invariance principle: the family members differ in victim set, beneficiary class, audit object, and resource-flow target, so each warrants its own epsilon, stakeholders, and classification rather than one story with a measurement parameter. This file is the near-term-harms member; edges run to the existential-risk and integrated siblings, whose upstream claims about field priorities shape the legitimacy conditions this reading operates in and vice versa.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_alignment_priority__nearterm_harms_reading, powerless, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
