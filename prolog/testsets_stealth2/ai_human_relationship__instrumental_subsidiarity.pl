% ============================================================================
% CONSTRAINT STORY: ai_human_relationship__instrumental_subsidiarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_human_relationship__instrumental_subsidiarity, []).

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
 *   constraint_id: ai_human_relationship__instrumental_subsidiarity
 *   human_readable: Instrumental-Subsidiarity Governance of Artificial Intelligence
 *   domain: political_theology/technology_ethics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   ai_human_relationship: the instrumental-subsidiarity reading, in which AI
 *   is a morally neutral instrument, responsibility attaches to use-cases,
 *   subsidiarity serves as the procedural safeguard allocating decisions
 *   across governance levels, and human dignity is protected through legal
 *   frameworks, transparency requirements, and audit regimes. The constraint
 *   modeled is the standing governance arrangement itself — the framework of
 *   statutes, conformity assessments, ethics boards, and devolved review that
 *   operationalizes this reading. Its ε referent is that standing
 *   arrangement, assessed by the reading's own lights: the reading endorses
 *   the arrangement, and its own lights can still register the arrangement's
 *   actual operating costs, which is what the metrics below describe. Sibling
 *   readings (technocratic_optimization, incarnational_humanism) are separate
 *   constraints in separate files; per the epsilon-invariance principle this
 *   file neither hedges across them nor averages over them. The claim and the
 *   metrics are independent authored facts: the reading is CLAIMED as
 *   tangled_rope because the arrangement visibly pairs a genuine
 *   accountability-coordination function with an asymmetric burden structure,
 *   and the metrics are authored descriptively of how the arrangement
 *   actually operates. Interval mapping: T=0 approximates 2016 (first wave of
 *   AI ethics frameworks and algorithmic-accountability legislation); T=10
 *   approximates 2026.
 *
 * KEY AGENTS:
 *   - - ai_developing_corporations: Primary beneficiary (institutional/arbitrage) — collects the neutrality shield; design choices sit outside the regulatory perimeter
 *   - - national_ai_regulators: Agenda setter with secondary beneficiary position (institutional/constrained) — administers enforcement, collects authority and budget, depends on developer-supplied documentation
 *   - - algorithmic_decision_subjects: Primary target (powerless/trapped) — bears case-by-case remediation burden, cannot exit being decided about
 *   - - automation_displaced_workers: Secondary target (organized/constrained) — displacement processed as adjustment inside a fixed frame
 *   - - local_ai_deployers: Dual-positioned bearer (moderate/constrained) — receives discretion under subsidiarity, absorbs liability and compliance cost without commensurate capacity
 *   - - compliance_audit_industry: Secondary beneficiary (organized/mobile) — collects attestation fees the mandate creates
 *   - - affected_communities_without_standing: Excluded voice (powerless/trapped) — would contest deployment classes wholesale; enters only as individual complainants
 *   - - cst_political_theologians: Analytical observer (analytical/analytical) — sees the full structure, takes no rents, bears no burden
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_human_relationship__instrumental_subsidiarity, 0.58).
domain_priors:suppression_score(ai_human_relationship__instrumental_subsidiarity, 0.52).
domain_priors:theater_ratio(ai_human_relationship__instrumental_subsidiarity, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_human_relationship__instrumental_subsidiarity, tangled_rope).
narrative_ontology:human_readable(ai_human_relationship__instrumental_subsidiarity, "Instrumental-Subsidiarity Governance of Artificial Intelligence").
narrative_ontology:topic_domain(ai_human_relationship__instrumental_subsidiarity, "political_theology/technology_ethics").

domain_priors:requires_active_enforcement(ai_human_relationship__instrumental_subsidiarity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_human_relationship__instrumental_subsidiarity, 'c3a2b1b5-7c60-4a52-9c16-80a88dbd30ec').
narrative_ontology:cs_kernel_codification('c3a2b1b5-7c60-4a52-9c16-80a88dbd30ec', formalized).
narrative_ontology:cs_authority_grounding('c3a2b1b5-7c60-4a52-9c16-80a88dbd30ec', practice).
narrative_ontology:cs_interpretation_layer_present('c3a2b1b5-7c60-4a52-9c16-80a88dbd30ec').
narrative_ontology:cs_reading_relation('c3a2b1b5-7c60-4a52-9c16-80a88dbd30ec', ai_human_relationship__technocratic_optimization, coexists_with).
narrative_ontology:cs_reading_relation('c3a2b1b5-7c60-4a52-9c16-80a88dbd30ec', ai_human_relationship__incarnational_humanism, forecloses).
narrative_ontology:cs_axiom('c3a2b1b5-7c60-4a52-9c16-80a88dbd30ec', foundational, artifact_moral_neutrality).
narrative_ontology:cs_axiom_status(artifact_moral_neutrality, holdable).
narrative_ontology:cs_axiom_grounding('c3a2b1b5-7c60-4a52-9c16-80a88dbd30ec', artifact_moral_neutrality, conventional).
narrative_ontology:cs_axiom('c3a2b1b5-7c60-4a52-9c16-80a88dbd30ec', foundational, subsidiarity_orders_governance_levels).
narrative_ontology:cs_axiom_status(subsidiarity_orders_governance_levels, holdable).
narrative_ontology:cs_axiom_grounding('c3a2b1b5-7c60-4a52-9c16-80a88dbd30ec', subsidiarity_orders_governance_levels, conventional).
narrative_ontology:cs_axiom('c3a2b1b5-7c60-4a52-9c16-80a88dbd30ec', secondary, dignity_secured_by_juridical_transparency).
narrative_ontology:cs_axiom_status(dignity_secured_by_juridical_transparency, holdable).
narrative_ontology:cs_axiom_grounding('c3a2b1b5-7c60-4a52-9c16-80a88dbd30ec', dignity_secured_by_juridical_transparency, deontological).
narrative_ontology:cs_reference_frame('c3a2b1b5-7c60-4a52-9c16-80a88dbd30ec', tool_neutrality_subsidiary_governance).
narrative_ontology:cs_drift_state('c3a2b1b5-7c60-4a52-9c16-80a88dbd30ec', contemporary_ai_scaling_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c3a2b1b5-7c60-4a52-9c16-80a88dbd30ec', '').
narrative_ontology:cs_kernel_id(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, ai_developing_corporations).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, compliance_audit_industry).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, national_ai_regulators).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, algorithmic_decision_subjects).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, automation_displaced_workers).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, local_ai_deployers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, local_ai_deployers).
narrative_ontology:constraint_vindicates(ai_human_relationship__instrumental_subsidiarity, tool_neutrality_doctrine).
narrative_ontology:constraint_vindicates(ai_human_relationship__instrumental_subsidiarity, subsidiarity_principle).
narrative_ontology:constraint_vindicates(ai_human_relationship__instrumental_subsidiarity, juridical_dignity_protection).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and enforce AI statutes premised on tool-neutrality: define acceptable-use categories, run conformity-assessment and audit regimes, certify deployments, and impose transparency filings. Their administrative authority, staffing, and budget exist because the mandate exists. Verification depends heavily on documentation supplied by the developers being reviewed, and their scope of inquiry stops at the use-case boundary the premise draws.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, national_ai_regulators, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(ai_human_relationship__instrumental_subsidiarity, national_ai_regulators, beneficiary).

% Build, train, and deploy AI systems at scale. Because the governing premise locates moral and legal responsibility in use rather than in the artifact, scrutiny of design choices, training corpora, objective functions, and business models falls outside the regulatory perimeter; they face use-case compliance costs while retaining freedom over how systems are built. They supply the documentation regulators rely on, fund much of the surrounding compliance ecosystem, and can relocate operations or lobby across jurisdictions when rules bind.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, ai_developing_corporations, beneficiary,
    institutional, generational, arbitrage, global).

% Auditors, certifiers, and ethics consultants whose revenue exists because transparency and documentation are legally mandated. They collect fees for attestation and advisory work without setting the rules, deploying systems, or bearing downstream harm. Their book of business depends on the framework continuing in roughly its present form.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, compliance_audit_industry, beneficiary,
    organized, biographical, mobile, continental).

% People scored, ranked, matched, denied, or flagged by deployed systems in credit, welfare, hiring, insurance, and policing. Remedy runs case-by-case through complaint portals and litigation channels the framework provides, which requires records access, legal literacy, and time most do not have. They cannot exit being subjected to decisions made about them, and the governing premise frames their injury as misuse of a tool rather than a property of the deployed system.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, algorithmic_decision_subjects, payer,
    powerless, immediate, trapped, global).

% Workers whose roles are automated out from under them. Under the governing premise, displacement is processed as economic adjustment — retraining vouchers, transition payments, regional development funds — rather than as a question about how production is ordered. Unions negotiate the size of the mitigation package but the frame within which negotiation happens is fixed elsewhere.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, automation_displaced_workers, payer,
    organized, biographical, constrained, regional).

% Hospitals, schools, diocesan agencies, and municipal offices that adopt AI tools at the point of service. Subsidiarity assigns them front-line discretion over procurement and application, which they value as autonomy from distant standard-setters; the same principle assigns them fitness assessment, documentation, and liability for harms, usually without commensurate technical capacity to evaluate what they are deploying.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, local_ai_deployers, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(ai_human_relationship__instrumental_subsidiarity, local_ai_deployers, beneficiary).

% Communities under concentrated deployment — surveillance overlays, neighborhood-level scoring patterns, welfare-automation rollouts — who lack the legal literacy, records access, or consultation seats through which the framework routes objection. They would contest whole deployment classes rather than individual decisions, but they enter the process, when at all, only as isolated complainants after harm has occurred.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, affected_communities_without_standing, excluded,
    powerless, generational, trapped, regional).

% Scholars in Catholic social thought and political theology who analyze the arrangement's anthropology, its genealogy in the subsidiarity tradition, and what its premises assume about the human person. They collect no fees and bear no compliance burden; their critiques circulate in journals and synodal consultations adjacent to, but outside, the enforcement loop.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, cst_political_theologians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_human_relationship__instrumental_subsidiarity, ai_developing_corporations).
narrative_ontology:fixing_cost_class(ai_human_relationship__instrumental_subsidiarity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Assigns accountability for AI-mediated harm through a shared legal vocabulary: transparency obligations, conformity assessment, and liability rules let developers, deployers, regulators, and courts act without renegotiating responsibility case by case. Subsidiarity additionally allocates which governance level handles which decision — general standard-setting upward, application and review downward — solving the scale-mismatch between planetary technical systems and local moral agency.
% TRANSFER_FUNCTION: Moves compliance and documentation burden from the point of design toward deployers and public budgets; moves remediation labor onto harm subjects, who must assemble evidence and pursue redress case by case; moves discretionary authority downward to local institutions while moving definitional authority over acceptable use upward to legislatures and standards bodies; confers fee revenue on the attestation industry.
% ABSENT_VOICES: Communities bearing concentrated algorithmic harms sit outside the consultation rooms where use-categories are drawn; individuals subjected to automated decisions meet the framework only as complainants after injury; non-expert publics affected by deployment classes have no seat where acceptable use is defined. Their absence lets the use-category taxonomy present itself as consensual when it was never negotiated with those it sorts.
% DISAPPEARANCE_RATIONALE: Deployment would continue, but the accountability architecture would vanish overnight: audit mandates, transparency filings, liability channels, and local review procedures would dissolve, leaving remedy to ad hoc tort and market reputation. The attestation industry would lose its legal basis; local institutions would lose the discretion-and-liability settlement subsidiarity currently fixes; regulators would lose the mandate that constitutes them.
% FOUNDING_PROBLEM: How to govern powerful technical systems whose scale and opacity outrun the moral reach of individual users and local communities — preserving human responsibility and dignity without either capitulating to the machine's own logic or freezing technical development. The subsidiarity tradition posed this for twentieth-century industrial concentration; algorithmic deployment re-poses it at greater speed and scale.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: civil-society algorithmic-audit organizations documenting deployed-system harms, judicial findings in automated-decision cases, academic incident registries, and worker testimony on automation transitions all attest that the underlying problem persists. Several of these sources attest the problem while disputing this arrangement's neutrality premise — corroboration of the founding problem is not endorsement of the framework built on it.
narrative_ontology:disappearance_verdict(ai_human_relationship__instrumental_subsidiarity, world_rearranges).
narrative_ontology:founding_problem_status(ai_human_relationship__instrumental_subsidiarity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_human_relationship__instrumental_subsidiarity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_human_relationship__instrumental_subsidiarity, 'none', 1).
narrative_ontology:epsilon_provenance(ai_human_relationship__instrumental_subsidiarity, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_human_relationship__instrumental_subsidiarity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_human_relationship__instrumental_subsidiarity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_human_relationship__instrumental_subsidiarity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58: the arrangement builds real accountability machinery — transparency filings, conformity assessment, liability channels — yet the neutrality premise decouples responsibility from design, so the parties best positioned to prevent harm at the source face only use-case-level costs while harm subjects absorb remediation labor. Suppression 0.52: enforcement is real (certification regimes, audit mandates), and rival framings are not banned but are crowded out of the policy space where use-categories get defined; suppression here is structural-perimeter effect, not coercion of persons. Theater ratio 0.42: a growing share of activity is documentation produced to satisfy documentation requirements — ethics boards that advise without authority, impact assessments filed and archived — while the protective function thins at the margins. Accessibility collapse 0.35: alternatives remain genuinely available — ordering-critique framings stay live in theological and some legislative discourse, and several jurisdictions experiment with design-level duties — so understanding the arrangement does not close exits. Resistance 0.55: sustained critique from political theology, affected-community organizing, worker movements, and heterodox regulators meets the arrangement continuously. The three temporal series share one grid (T=0,2,4,6,8,10) so every metric is authored at every examined point; rising base_extractiveness tracks accumulation as deployment scales faster than verification capacity; rising theater_ratio tracks Goodhart drift of documentation displacing protection; rising suppression_requirement tracks enforcement machinery hardening around the neutrality premise rather than any change in the premise itself.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the regulator's chair the arrangement is a rule-of-law achievement: harms now have venues, deployments leave paper trails, discretion is bounded. From the developing corporation's chair it is a manageable compliance cost that purchases design freedom — the cheapest possible settlement. From the decision subject's chair it is a maze in which injury must be individually proven against systems whose workings are disclosed only in summary. From the local deployer's chair it is an unfunded mandate: discretion granted, liability assigned, capacity withheld. From the theologian's chair the entire edifice rests on an anthropology question it never adjudicates. The engine computes this divergence from the structural data; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations plus exit options drive the derivation, and no directionality overrides are needed: ai_developing_corporations (declared beneficiary, arbitrage-grade exit) derive near the beneficiary pole; compliance_audit_industry likewise; algorithmic_decision_subjects, automation_displaced_workers, and local_ai_deployers (declared victims, trapped or constrained exit) derive near the target pole, with deployers moderated by their secondary beneficiary declaration capturing the genuine discretion subsidy; affected_communities_without_standing sit at the extreme target end despite exclusion from the formal arrays, which is itself diagnostic. National_ai_regulators carry role agenda_setter with secondary_role beneficiary and appear in the beneficiaries array — the array drives their derived directionality toward the beneficiary side, which matches their structural position: they collect authority and budget from the mandate and enforce the boundary the premise draws. cst_political_theologians are analytical observers outside the chi computation. Scope amplification applies modestly: the arrangement operates nationally and continentally while its subjects are global, widening the verification gap the extractiveness figure already reflects.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live, so no mandatrophy is declared: the arrangement has not outlived its function, and the R5 mismatch consumer finds status=live crossed with verdict=world_rearranges — consistent, no zombie flag. The classification work the framework does here is preventing two opposite mislabels. Read as pure coordination, the arrangement's real achievements (accountability venues, devolved review, transparency infrastructure) would conceal that the neutrality premise functions as a shield whose rents accrue to a specific seat. Read as pure extraction, the analysis would erase the genuine collective-action problem the framework solves — responsibility assignment across actors who could not otherwise act at all — and would predict collapse that the observable record contradicts. The tangled_rope claim holds both halves. The forward risk the temporal series monitors is drift: if theater_ratio continues climbing while protective outcomes stagnate, the arrangement trends toward performance maintaining an atrophied function, and the founding-problem status would deserve re-examination in a later revision of this story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    neutrality_premise_kernel_contest,
    'This story instantiates the instrumental_subsidiarity reading of the ai_human_relationship kernel: is the tool-neutrality premise a stable load-bearing feature of the arrangement, or the wall whose removal collapses this reading into a sibling?',
    'Track whether regulatory reform proposals target use-case classification (premise intact) or impose design-level duties, documentation of training-data values, or artifact-level liability (premise abandoned); monitor whether major jurisdictions begin regulating model development rather than deployment.',
    'If the premise falls, the beneficiary structure inverts — design-level scrutiny converts the corporate shield into exposure, the attestation industry''s basis shifts, and this constraint converges structurally toward the incarnational_humanism sibling''s arrangement; if the premise holds, the authored structure stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(neutrality_premise_kernel_contest, conceptual, 'Committer structure: one reading of a contested kernel; the disagreement is located in the locus of moral evaluation (artifact vs. use).').

omega_variable(
    embedded_values_in_model_design,
    'Are AI systems morally neutral artifacts, or do training corpora, objective functions, alignment choices, and deployment defaults embed value commitments prior to any use-case?',
    'Technical audits comparing harm profiles across identical use-cases implemented on different design lineages; systematic documentation of value-laden choices surfaced in model cards, alignment reports, and red-team records.',
    'If design embeds values, use-case-only regulation systematically under-regulates and the arrangement''s effective extractiveness exceeds the authored estimate, with the shortfall borne by decision subjects; if artifact and use are genuinely separable, the current figures stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(embedded_values_in_model_design, empirical, 'Empirical status of the neutrality axiom on which the reading''s regulatory perimeter depends.').

omega_variable(
    subsidiarity_capacity_stranding,
    'Does subsidiarity''s devolution of AI decisions to the lowest competent level protect dignity through proximity and local knowledge, or strand harms at exactly the levels least equipped to detect them?',
    'Comparative outcome studies of institutions and jurisdictions with strong local technical-review capacity versus those without, holding deployment class constant; audit of whether local review bodies ever reject or modify certified systems.',
    'If stranding dominates, the subsidiarity component functions as burden-shifting onto local deployers and raises the arrangement''s extractiveness; if proximity protects, the component is genuine coordination and the extractiveness estimate falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidiarity_capacity_stranding, empirical, 'Whether the procedural safeguard coordinates or offloads.').

omega_variable(
    regulatory_documentation_dependence,
    'Is the framework''s reliance on developer-supplied documentation a transient capacity gap, or a structural capture channel that deepens as model complexity outpaces verifier capability?',
    'Longitudinal tracking of regulator verification capacity against model opacity; incidence of post-certification harm discoveries that contradict submitted documentation; staffing and expertise audits of conformity-assessment bodies.',
    'Structural dependence would date a drift in which the enforcing seat becomes progressively an extension of the beneficiary seat — consolidation of gains in one place even as formal safeguards multiply — warranting reclassification pressure in later revisions of this story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_documentation_dependence, empirical, 'Capture-channel risk inside the enforcement machinery.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_human_relationship__instrumental_subsidiarity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_h_tr_t0, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(ai_h_tr_t0, observed).
narrative_ontology:measurement(ai_h_tr_t2, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 2, 0.28).
narrative_ontology:measurement_basis(ai_h_tr_t2, observed).
narrative_ontology:measurement(ai_h_tr_t4, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 4, 0.32).
narrative_ontology:measurement_basis(ai_h_tr_t4, observed).
narrative_ontology:measurement(ai_h_tr_t6, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 6, 0.36).
narrative_ontology:measurement_basis(ai_h_tr_t6, observed).
narrative_ontology:measurement(ai_h_tr_t8, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 8, 0.39).
narrative_ontology:measurement_basis(ai_h_tr_t8, observed).
narrative_ontology:measurement(ai_h_tr_t10, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 10, 0.42).
narrative_ontology:measurement_basis(ai_h_tr_t10, observed).

% Extraction over time
narrative_ontology:measurement(ai_h_be_t0, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(ai_h_be_t0, observed).
narrative_ontology:measurement(ai_h_be_t2, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 2, 0.44).
narrative_ontology:measurement_basis(ai_h_be_t2, observed).
narrative_ontology:measurement(ai_h_be_t4, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 4, 0.49).
narrative_ontology:measurement_basis(ai_h_be_t4, observed).
narrative_ontology:measurement(ai_h_be_t6, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 6, 0.53).
narrative_ontology:measurement_basis(ai_h_be_t6, observed).
narrative_ontology:measurement(ai_h_be_t8, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 8, 0.56).
narrative_ontology:measurement_basis(ai_h_be_t8, observed).
narrative_ontology:measurement(ai_h_be_t10, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(ai_h_be_t10, observed).

% Suppression requirement over time
narrative_ontology:measurement(ai_h_su_t0, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(ai_h_su_t0, observed).
narrative_ontology:measurement(ai_h_su_t2, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 2, 0.41).
narrative_ontology:measurement_basis(ai_h_su_t2, observed).
narrative_ontology:measurement(ai_h_su_t4, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 4, 0.44).
narrative_ontology:measurement_basis(ai_h_su_t4, observed).
narrative_ontology:measurement(ai_h_su_t6, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 6, 0.47).
narrative_ontology:measurement_basis(ai_h_su_t6, observed).
narrative_ontology:measurement(ai_h_su_t8, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 8, 0.5).
narrative_ontology:measurement_basis(ai_h_su_t8, observed).
narrative_ontology:measurement(ai_h_su_t10, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 10, 0.52).
narrative_ontology:measurement_basis(ai_h_su_t10, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_human_relationship__instrumental_subsidiarity, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship__technocratic_optimization).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship__incarnational_humanism).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the AI-human relationship' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints — this instrumental_subsidiarity reading, the technocratic_optimization reading, and the incarnational_humanism reading. Each has its own epsilon, beneficiary/victim structure, and classification; forcing one story to cover all three would make epsilon observer-relative, which the framework forbids. Direction of influence: the incarnational_humanism reading is upstream doctrinal critique whose anthropological claims set the legitimacy conditions under which this reading's neutrality premise is defended; the technocratic_optimization reading is the practical rival whose deployment logic this reading's legal machinery partially disciplines and thereby shapes. Every family member links the others via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
