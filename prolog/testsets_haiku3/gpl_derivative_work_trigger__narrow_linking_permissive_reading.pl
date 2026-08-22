% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__narrow_linking_permissive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__narrow_linking_permissive_reading, []).

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
 *   constraint_id: gpl_derivative_work_trigger__narrow_linking_permissive_reading
 *   human_readable: GPL Derivative Work Trigger: Narrow Linking Permissive Reading
 *   domain: legal/software/copyright
 *
 * SUMMARY:
 *   The GPL's core copyleft mechanism depends on a clear definition of
 *   'derivative work'—the triggering event for source-disclosure obligation.
 *   This constraint instantiates ONE reading of that contested boundary: the
 *   narrow-linking reading asserts that dynamic linking is aggregation (not
 *   derivation), and only modifications to GPL code itself trigger copyleft
 *   obligations. Proprietary vendors benefit by receiving legal permission to
 *   distribute closed-source binaries linked against GPL libraries. GPL users
 *   lose the transparency guarantee copyleft was designed to provide. The FSF
 *   and copyleft advocates face a frustrated propagation goal. The reading is
 *   actively enforced through licensing interpretations, vendor compliance,
 *   and the de facto standard in commercial software integration. The
 *   claim/metric gap is intentional: the constraint is CLAIMED as tangled
 *   rope (it coordinates expectations around linking) while the metrics
 *   describe an extractive, enforcement-intensive arrangement. The narrow
 *   reading extracts closed-source privilege for vendors at the cost of
 *   transparency for users.
 *
 * KEY AGENTS:
 *   - proprietary_software_vendors — benefit from the permissive linking interpretation; have powerful exit options and capture the extraction
 *   - gpl_software_users — lose source transparency; trapped by software lock-in and identity fusion with vendor ecosystem
 *   - copyleft_advocates — their propagation goal is frustrated; organized resistance but constrained by copyright law interpretation
 *   - linking_boundary_interpreters — institutional agenda-setters who enforce the narrow reading via case law and licensing guidance
 *   - gpl_library_maintainers — dual position: benefit from adoption (permissive linking lowers friction) but lose downstream control
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.68).
domain_priors:suppression_score(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.52).
domain_priors:theater_ratio(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, tangled_rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "GPL Derivative Work Trigger: Narrow Linking Permissive Reading").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "legal/software/copyright").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__narrow_linking_permissive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__narrow_linking_permissive_reading, '86f4821d-3b25-4261-8ed4-53c7655c7434').
narrative_ontology:cs_kernel_codification('86f4821d-3b25-4261-8ed4-53c7655c7434', fixed_text).
narrative_ontology:cs_authority_grounding('86f4821d-3b25-4261-8ed4-53c7655c7434', extraction).
narrative_ontology:cs_interpretation_layer_present('86f4821d-3b25-4261-8ed4-53c7655c7434').
narrative_ontology:cs_reading_relation('86f4821d-3b25-4261-8ed4-53c7655c7434', gpl_derivative_work_trigger__broad_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('86f4821d-3b25-4261-8ed4-53c7655c7434', gpl_derivative_work_trigger__interface_boundary_reading, influences).
narrative_ontology:cs_axiom('86f4821d-3b25-4261-8ed4-53c7655c7434', foundational, linking_is_aggregation_not_derivation).
narrative_ontology:cs_axiom_status(linking_is_aggregation_not_derivation, holdable).
narrative_ontology:cs_axiom_grounding('86f4821d-3b25-4261-8ed4-53c7655c7434', linking_is_aggregation_not_derivation, conventional).
narrative_ontology:cs_axiom('86f4821d-3b25-4261-8ed4-53c7655c7434', foundational, code_modification_sole_copyleft_trigger).
narrative_ontology:cs_axiom_status(code_modification_sole_copyleft_trigger, holdable).
narrative_ontology:cs_axiom_grounding('86f4821d-3b25-4261-8ed4-53c7655c7434', code_modification_sole_copyleft_trigger, conventional).
narrative_ontology:cs_reference_frame('86f4821d-3b25-4261-8ed4-53c7655c7434', dynamic_linking_as_aggregation).
narrative_ontology:cs_drift_state('86f4821d-3b25-4261-8ed4-53c7655c7434', contemporary_gpl3_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('86f4821d-3b25-4261-8ed4-53c7655c7434', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, linking_permission_doctrine).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_software_users).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, copyleft_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_library_maintainers).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, software_users_with_alternatives).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_library_maintainers).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, legal_uncertainty_bearers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major software companies (Microsoft, Google, Apple, Oracle) and mid-market vendors that build proprietary applications and platforms on top of GPL-licensed libraries (OpenSSL, SQLite, glibc, Linux kernel interfaces). Under the narrow reading, they can distribute closed-source binaries linked against GPL code without licensing proprietary code under GPL or disclosing source. The reading allows them to capture the benefits of GPL infrastructure (stable, audited libraries) without reciprocal obligation. They actively promote the reading through licensing counsel, internal compliance policies, and industry collaboration (BSA, OSSA standards). Their exit option is arbitrage-grade: they can switch to permissive-licensed libraries, fork GPL code under permissive licenses, or re-license proprietary modules if the reading is displaced.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_vendors, beneficiary,
    powerful, generational, arbitrage, global).

% End users of proprietary software that incorporates GPL-licensed components dynamically (typical in cloud services, mobile apps, desktop applications). They receive a unified binary that blends proprietary and GPL code but cannot obtain the full source code of the combined system—only the GPL portions are available (often in source-code repositories the vendor maintains). They are locked into the proprietary vendor's ecosystem by software dependencies, file-format compatibility, and account lock-in (SaaS services, app stores, OS ecosystems). The narrow reading frustrates the transparency guarantee they would have received if the software were organized differently. Exit is identity-locked because switching off the vendor means forgoing the entire service stack, not just choosing an alternative for a single component.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_software_users, payer,
    powerless, biographical, identity_locked, global).

% The Free Software Foundation, Software Freedom Conservancy, GNU project maintainers, open-source advocates, and legal scholars who authored the GPL to ensure source code propagates through the entire software supply chain. They drafted the copyleft mechanism with the expectation that dynamic linking would create a derivative work, triggering source-disclosure obligation. The narrow reading frustrates that intent: vendors dynamically link GPL code without disclosing proprietary code or licensing it under GPL. Advocates resist through licensing guidance (FSF FAQ), case-law amicus briefs (Jacobsen v. Katzer), and policy advocacy, but they are constrained by copyright law mechanics—they cannot unilaterally redefine 'derivative work' outside the courts. They are partially excluded from the linking boundary's adjudication; courts and vendors interpret the boundary, not the original drafters.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, copyleft_advocates, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(gpl_derivative_work_trigger__narrow_linking_permissive_reading, copyleft_advocates, excluded).

% Courts, licensing committees (Software Freedom Conservancy, SFLC), law firms and licensing counsel (GPL Legal Network), and institutional interpreters of copyright law and open-source licensing. They adjudicate and enforce the narrow reading through case-law opinions, licensing opinions, and licensing guidance. The narrow reading is now the standard position cited in vendor compliance policies, licensing-risk assessments, and litigation settlements. Institutional interpreters set and maintain the boundary; their interpretation becomes the de facto rule vendors and projects follow.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, linking_boundary_interpreters, agenda_setter,
    institutional, generational, analytical, global).

% Authors and maintainers of widely-used GPL-licensed libraries (OpenSSL, SQLite, glibc, parts of the Linux kernel). They benefit from the narrow reading because it lowers the friction for proprietary vendors to adopt their code—vendors face no forced licensing choice or source-disclosure obligation, so adoption is easier. This increases library adoption, community contributions, and visibility. However, they lose downstream control: vendors make closed-source modifications to the library without sharing them back, and users of those modifications receive no source access. They are constrained in their exit options: forking the library under stricter terms (AGPL, Commons Clause) risks fragmenting the community; staying with GPL means accepting that the library will be used in closed-source stacks with no source propagation.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_library_maintainers, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_library_maintainers, payer).

% Sophisticated software users and organizations (enterprises, large open-source projects, research institutions) with sufficient technical literacy and resources to evaluate licensing implications and switch between GPL-based and permissive-licensed stacks. They benefit from the narrow reading because it lowers the licensing friction for adopting GPL libraries in proprietary systems without full source-disclosure obligations. They can invest in licensing counsel, conduct due diligence, and make informed choices about licensing tradeoffs. They have mobile exit options: they can choose between GPL and permissive libraries, contribute to either, and switch if licensing terms change.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, software_users_with_alternatives, beneficiary,
    moderate, biographical, mobile, global).

% Permissive open-source licenses (Apache 2.0, MIT, BSD, ISC) that allow proprietary derivative works without obligation. The narrow GPL reading functionally assimilates GPL-licensed code to permissive-license semantics when used via dynamic linking—the reading erodes the distinction between copyleft and permissive licensing for the linking use case. These licenses are structurally excluded from the GPL derivative-work mechanics; they benefit implicitly from the narrow reading but have no role in GPL licensing adjudication.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, competing_licenses, excluded,
    powerful, generational, trapped, global).

% Medium-sized software companies and open-source projects that must navigate GPL licensing without the legal resources of major vendors. They face compliance uncertainty: the narrow reading is the current de facto standard, but it has not been universally endorsed by courts; if courts adopt the broad-copyleft reading, their current proprietary distributions could be found infringing. They bear the ongoing cost of monitoring licensing interpretations, maintaining compliance risk assessments, and potentially re-architecting systems if the reading shifts. They are constrained in their exit options: they cannot easily switch to alternative licensing frameworks (which might be unfamiliar or unavailable) and cannot reliably predict whether their current compliance stance will be valid in five years.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, legal_uncertainty_bearers, payer,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_vendors).
narrative_ontology:fixing_cost_class(gpl_derivative_work_trigger__narrow_linking_permissive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the legal ambiguity of GPL's derivative-work boundary by establishing a bright-line rule: dynamic linking creates aggregation, not derivation; copyleft obligations apply only to code modifications, not to the act of linking. This rule coordinates expectations between GPL authors and proprietary vendors, reducing legal uncertainty about what constitutes GPL compliance. Vendors can rely on the narrow reading to make integration decisions; GPL projects can rely on it to advise license compatibility.
% TRANSFER_FUNCTION: Transfers the transparency guarantee from proprietary-system end users to proprietary vendors. Users lose the right to inspect and modify the full software stack they receive (proprietary module sources are hidden); vendors gain the legal permission to distribute closed-source binaries incorporating GPL libraries without disclosing proprietary code or licensing proprietary modules under GPL. The transfer is mediated through the copyright-law boundary: linking is classified as aggregation (not derivation), which severs the GPL chain.
% ABSENT_VOICES: Excluded: future GPL users not yet in the market, who would benefit from source transparency; authors of future GPL components, who would prefer broad-copyleft enforcement; competing-license maintainers (Apache, MIT, BSD) who benefit implicitly from the reading's erosion of copyleft distinction but are not parties to GPL licensing mechanics; sub-component developers (library maintainers like OpenSSL, SQLite authors) who authored in-tree GPL code expecting copyleft enforcement downstream but discover the linking boundary prevents it; software users in regulated industries (medical, aerospace) who might prefer GPL propagation for safety/auditability reasons but have no say in vendor licensing choices.
% DISAPPEARANCE_RATIONALE: If the narrow reading vanished (courts displaced it with the broad-copyleft reading), proprietary vendors would face a binding choice: license proprietary modules under GPL, halt dynamic linking, or settle licensing disputes with GPL maintainers. The software ecosystem would reorganize: (1) vendors would fork GPL libraries under permissive licenses to preserve closed-source privilege, (2) proprietary-GPL integration would sharply decline in new projects, (3) cloud services would face pressure to open-source components or relocate outside GPL-regulated jurisdictions. The commercial incentive structure around GPL-library adoption would shift from 'use freely with no obligation' to 'use with full copyleft obligation or fork with permissive license.' Current proprietary software stacks would face legal liability and architectural restructuring.
% FOUNDING_PROBLEM: Early GPL license language (1991–1999) specified 'derivative work' without precisely defining what constitutes one in the context of linking. The GPL's authors intended to ensure source propagation through modifications; the license text was ambiguous about whether dynamic linking (a runtime integration mechanism) created a derivative work or aggregation (distinct programs). Multiple interpretations coexisted: (1) the broad reading (linking = derivative), (2) the narrow reading (linking = aggregation), (3) the interface-boundary reading (API boundaries matter). Vendors and projects applied competing interpretations, creating legal uncertainty about compliance obligations. The founding problem was genuine ambiguity, not a settled principle vendors were evading.
% FOUNDING_PROBLEM_CORROBORATION: FSF's GPL FAQ (updated repeatedly 2000–present) acknowledges the linking ambiguity and attempts to clarify the narrow reading's interpretation. Licensing counsel opinions (FOSSOLOGY, Software Freedom Conservancy) cite the narrow reading as the current standard. Industry practice (cloud computing, mobile development, SaaS) has adopted the narrow reading as the default compliance baseline. However, NO corroboration exists from outside the vendor-favorable set: copyleft advocates dispute that the founding problem has been settled (FSF maintains alternative language in AGPL and argues the narrow reading subverts GPL intent); courts have not issued binding nationwide rulings establishing the narrow reading as the only legitimate interpretation (conflicting case-law opinions exist in different jurisdictions); no independent legal authority has conclusively determined whether the GPL's original drafters intended the narrow or broad reading. The founding problem was genuine ambiguity; the narrow reading resolved it for vendors, but not for the broader copyleft community.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__narrow_linking_permissive_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__narrow_linking_permissive_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__narrow_linking_permissive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_derivative_work_trigger__narrow_linking_permissive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_derivative_work_trigger__narrow_linking_permissive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.68 by interval end) because the narrow reading bestows a structural privilege: vendors can use GPL infrastructure without reciprocal obligation. The escalation from 0.48 to 0.68 reflects the reading's increasing entrenchment in practice (2010s–2020s: cloud computing and SaaS adoption created millions of GPL-linked binaries with zero source disclosure). Suppression is moderate (0.52) because the narrow reading is defended through licensing argumentation and institutional interpretation, not outright legal prohibition—vendors and interpreters cite licensing counsel and case law rather than brute force. Theater ratio rises from 0.20 to 0.41 over the interval, indicating that the enforcement machinery increasingly consists of rhetorical defense (licensing FAQs, policy documents, legal opinions) rather than enforcement of the original coordination function. The planar trajectory (plateau at t=25) suggests the reading reached stable institutional status once major vendors adopted it and courts began citing it; further extraction is capped by the boundary itself, not by enforcement intensity. Measurement series are aligned on one grid.
 *
 * PERSPECTIVAL GAP:
 *   Proprietary vendors (powerful, institutional) compute the constraint as a genuine coordination mechanism: the narrow reading provides a clear bright-line rule (modifications = obligation; linking ≠ obligation) that reduces friction for GPL adoption. GPL users (powerless, identity-locked) compute the same constraint as extraction: they lose the transparency guarantee their choice to adopt GPL was meant to secure. Copyleft advocates (organized, constrained) compute it as a frustration of their founding goal—a reading that subverts the GPL's intended propagation. The engine derives directionality from these asymmetries: vendors sit near the beneficiary end (d ≈ 0.2–0.3), users sit near the target end (d ≈ 0.8–0.9), advocates occupy a constrained middle ground (d ≈ 0.6–0.7).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: proprietary_software_vendors (permission to distribute closed-source binaries; powerful exit via license choice or alternative libraries), linking_permission_doctrine (the reading itself vindicates a legal claim; non-agent, feeds no directionality). Victims: gpl_software_users (trapped by software lock-in; identity-locked to vendor ecosystem; no exit without forgoing adoption), copyleft_advocates (frustrated goal; organized but constrained by copyright law mechanics). The narrow reading creates an asymmetry: vendors have arbitrage-grade exit (choose a permissive library or dual-license their own) while users have identity-locked exit (switching off GPL means losing access to the infrastructure they depend on). GPL library maintainers sit in a secondary-role position: they benefit from wider adoption (permissive linking lowers friction) but lose downstream control (modifications are hidden, not shared forward). The directionality override field is not needed here; the structural derivation from beneficiary/victim + exit_options correctly produces the observed asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The narrow-linking reading was born to solve a genuine coordination ambiguity: the GPL license text was ambiguous about whether dynamic linking creates a derivative work. The founding problem was legal uncertainty—multiple interpretations coexisted, creating compliance burden for vendors and projects. However, by the interval's end (t=35), the founding problem has largely dissolved: the narrow reading has become the de facto standard in industry practice and licensing guidance. Vendors confidently build on it; projects adopt it; courts cite it. Yet the constraint persists and even intensifies (extractiveness plateau at 0.68; theater ratio holds at 0.41). This is mandatrophy at work: the constraint's original function (coordinating expectations around an ambiguous license term) has been satisfied; the constraint now operates primarily as a protection of vendor privilege and suppression of the broad-copyleft alternative. The theater ratio indicates increasing performative maintenance (licensing documents and FAQ updates defending the narrow reading) relative to functional problem-solving (the ambiguity is resolved; vendors no longer face genuine uncertainty). A mandatrophy_resolved flag would be appropriate here: the founding problem is dead (the GPL license ambiguity has been settled by practice and interpretation), but the constraint persists because vendors benefit from its preservation and have the institutional power to maintain it through licensing interpretation and legal precedent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    linking_boundary_ontology,
    'Is dynamic linking a technical fact about the code''s runtime structure, or a legal categorization imposed by copyright law? Does the boundary between aggregation and derivation inhere in the code, or does it depend on the reading''s interpretive frame?',
    'A clear statutory or judicial definition of ''derivative work'' in copyright law (U.S. Copyright Office guidance, binding court ruling) would establish whether the boundary is a technical property or a legal convention. Absent that, the boundary remains interpretively constructed.',
    'If the boundary is technical/natural, the narrow reading is a factual claim about code structure, and disagreement would be empirical. If the boundary is legal/constructed, the narrow reading is a policy choice among valid alternatives, and its entrenchment is institutional, not inevitable. This classification consequence affects whether mandatrophy analysis diagnoses the constraint as a natural fact or a constructed privilege.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(linking_boundary_ontology, conceptual, 'Whether the aggregation/derivation boundary is a technical or interpretive fact.').

omega_variable(
    gpl_founder_intent_recovery,
    'What did the GPL''s drafters intend by ''derivative work''? Did they intend dynamic linking to trigger copyleft, or did they accept that linking is aggregation?',
    'Historical analysis of GPL-FAQ archives, FSF correspondence, and case law citing FSF amicus briefs (Jacobsen v. Katzer, Ntp v. Research in Motion, Oracle v. Google). A clear statement of intent from FSF leadership or a binding court decision adopting FSF interpretation would resolve this.',
    'If the original intent was to include dynamic linking in copyleft, the narrow reading is a deviation from intent, and the constraint''s mandatrophy is compounded (original function was to enforce broad copyleft; current function is to permit narrow loopholes). If the original intent was ambiguous, the narrow reading is a legitimate interpretation, and the constraint''s mandatrophy is less severe (the founding problem was true ambiguity, not a betrayed original function).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gpl_founder_intent_recovery, empirical, 'Whether the GPL''s founders intended linking to trigger copyleft.').

omega_variable(
    propagation_goal_frustration_mechanism,
    'Does the narrow reading frustrate the GPL''s propagation goal by design (vendors intentionally adopt it to block source propagation), or incidentally (the reading has side effects that weaken copyleft)?',
    'Licensing decision analysis: survey vendor justifications for adopting the narrow reading (intentional vs. incidental). Institutional analysis: did vendors and GPL maintainers negotiate the reading, or did vendors unilaterally claim it and FSF resist? Court record: do judges cite the narrow reading as a compromise solution, or as a straightforward interpretation?',
    'If intentional, the constraint is a snare masquerading as a rope (vendors deliberately use a legal reading to extract closed-source privilege). If incidental, the constraint is a rope with significant negative side effects (genuine coordination that happens to harm the propagation goal). This affects whether the constraint is classified as tangled_rope (the current claim) or snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(propagation_goal_frustration_mechanism, empirical, 'Whether the narrow reading''s frustration of copyleft propagation is intentional or incidental.').

omega_variable(
    suppression_internalization_vs_structural,
    'GPL users'' acceptance of the narrow reading: is this suppression structural (legal barriers and software lock-in prevent exit) or internalized (users have absorbed the permissive-linking narrative and believe the reading is legitimate)?',
    'Post-exit analysis: if a user community exits the proprietary-GPL stack and deploys permissive-license alternatives, do they retain suppression (internalized belief in the reading''s legitimacy) or do they abandon it (suppression was structural, tied to lock-in)? Surveying GPL users'' awareness and acceptance of the narrow reading''s implications for transparency.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure (users carry the reading''s legitimacy narrative with them, making re-adoption of GPL easier for vendors). If structural, suppression is tied to lock-in and weakens with exit (users who leave the ecosystem may adopt competing licenses and avoid GPL altogether). This affects whether the constraint''s persistence is maintained by inertia or by active institutional defense.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_vs_structural, empirical, 'Whether suppression of GPL propagation is structural or internalized.').

omega_variable(
    kernel_reading_forecast,
    'If courts adopt the broad-copyleft reading (linking = derivative work), would this reading''s classification flip from tangled_rope to snare, or would the narrow reading persist in a subset of domains (embedded systems, firmware, SaaS)?',
    'Scenario analysis: draft the broad-copyleft reading as a separate constraint story; model the scenario where courts bind vendors to it; trace which vendors/domains persist with the narrow reading and which adopt the broad reading; assess whether the narrow reading continues to extract privilege in residual domains.',
    'If the narrow reading would persist in some domains after a broad-copyleft shift, the narrow reading is a more robust extraction mechanism than the tangled_rope classification suggests (it would continue to protect proprietary vendors in domains where enforcement is weak, like embedded systems or legacy codebases). If the narrow reading would collapse entirely, the constraint''s mandatrophy is acute: it persists only because courts have not yet displaced it, not because it solves a genuine problem.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_forecast, empirical, 'Robustness of the narrow reading if courts adopt the broad-copyleft alternative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(gpl__tr_t5, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement(gpl__tr_t10, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 10, 0.29).
narrative_ontology:measurement(gpl__tr_t15, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(gpl__tr_t20, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(gpl__tr_t25, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(gpl__tr_t30, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(gpl__tr_t35, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 35, 0.41).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(gpl__be_t5, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(gpl__be_t10, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 10, 0.57).
narrative_ontology:measurement(gpl__be_t15, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(gpl__be_t20, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(gpl__be_t25, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(gpl__be_t30, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(gpl__be_t35, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 35, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(gpl__su_t5, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 5, 0.41).
narrative_ontology:measurement(gpl__su_t10, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(gpl__su_t15, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 15, 0.48).
narrative_ontology:measurement(gpl__su_t20, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(gpl__su_t25, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 25, 0.51).
narrative_ontology:measurement(gpl__su_t30, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(gpl__su_t35, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 35, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__narrow_linking_permissive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.12).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger__broad_copyleft_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger__interface_boundary_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_distribution_via_gpl_libraries).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, open_source_adoption_in_corporate_environments).

% DUAL FORMULATION NOTE:
% This story is one reading of a kernel contest about GPL derivative-work boundaries. The kernel 'gpl_derivative_work_trigger' has been decomposed into three constraint stories, each instantiating a competing reading: (1) narrow_linking_permissive_reading (this story) — linking is aggregation, only modifications trigger obligations; (2) broad_copyleft_reading — linking itself creates a derivative work; (3) interface_boundary_reading — API boundaries determine derivative status. The three readings coexist in case law and licensing practice; none has achieved binding precedence. The narrow reading is currently ascendant in industry practice (cloud computing, proprietary SaaS built on GPL components). Each reading has a distinct ε (beneficiary/victim asymmetry) and will compute different classifications per-seat. The network edges reflect downstream constraints affected by the GPL boundary dispute: proprietary vendors' ability to distribute binaries depends on which reading courts adopt.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_derivative_work_trigger__narrow_linking_permissive_reading, powerful, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
