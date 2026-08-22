% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__broad_copyleft_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__broad_copyleft_reading, []).

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
 *   constraint_id: gpl_derivative_work_trigger__broad_copyleft_reading
 *   human_readable: GPL Broad Copyleft Linking Trigger — Combined-Work Source Disclosure Obligation
 *   domain: legal/technological
 *
 * SUMMARY:
 *   Under the broad copyleft reading, any program that links to GPL-covered
 *   code — statically or dynamically — becomes a derivative work of the
 *   combined whole, and the GPL's source-disclosure terms attach to the
 *   entire combination. The reading is the Free Software Foundation's
 *   official interpretive position and the working assumption of most
 *   enforcement activity. Structurally it does two things at once through one
 *   instrument: it preserves the software commons (anyone may combine and
 *   redistribute, source returns on the same terms) and it imposes a
 *   concentrated, legally enforced cost on parties who mix proprietary and
 *   GPL code — publish source, pay for an exception, or engineer around the
 *   component. This file instantiates ONE reading of the contested kernel
 *   gpl_derivative_work_trigger; the narrow-permissive and interface-boundary
 *   siblings are separate constraints with their own epsilon values (far
 *   lower for the narrow reading; concentrated on opaque coupling for the
 *   interface-boundary reading), authored separately and linked through
 *   network.affects_constraints. Epsilon here is authored for this reading's
 *   own referent — the linking-trigger arrangement as this reading endorses
 *   it — and is not averaged across siblings. KEY AGENTS (by structural
 *   relationship): - fsf_license_stewards: Agenda-setting interpreter
 *   ([institutional]/[mobile]) — authors the license text and the linking FAQ
 *   position; collects interpretive authority -
 *   gpl_enforcement_organizations: Enforcement arm
 *   ([organized]/[constrained]) — negotiates and litigates compliance;
 *   depends on a supply of violations - gpl_contributor_community: Primary
 *   beneficiary ([organized]/[constrained]) — contributions protected from
 *   closed absorption - downstream_source_access_users: Secondary beneficiary
 *   ([moderate]/[mobile]) — receives source without bearing compliance cost -
 *   proprietary_software_vendors: Primary target ([powerful]/[constrained]) —
 *   bears the disclose-or-avoid-or-pay cost - embedded_device_manufacturers:
 *   Concentrated target ([organized]/[constrained]) — most-litigated segment,
 *   thinnest margins - dual_licensing_companies: Exception-market beneficiary
 *   ([organized]/[arbitrage]) — sells the exemptions the broad reading
 *   creates demand for - permissive_license_advocates: Excluded rival
 *   ([organized]/[mobile]) — holds the aggregation argument, no seat in
 *   interpretation - copyright_courts: Analytical observer
 *   ([institutional]/[analytical]) — adjudicates when litigation forces the
 *   question
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__broad_copyleft_reading, 0.51).
domain_priors:suppression_score(gpl_derivative_work_trigger__broad_copyleft_reading, 0.4).
domain_priors:theater_ratio(gpl_derivative_work_trigger__broad_copyleft_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, extractiveness, 0.51).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__broad_copyleft_reading, tangled_rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__broad_copyleft_reading, "GPL Broad Copyleft Linking Trigger — Combined-Work Source Disclosure Obligation").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__broad_copyleft_reading, "legal/technological").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__broad_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__broad_copyleft_reading, '7cdc2683-bd43-456d-9fb8-464f2c968375').
narrative_ontology:cs_kernel_codification('7cdc2683-bd43-456d-9fb8-464f2c968375', fixed_text).
narrative_ontology:cs_authority_grounding('7cdc2683-bd43-456d-9fb8-464f2c968375', lineage).
narrative_ontology:cs_interpretation_layer_present('7cdc2683-bd43-456d-9fb8-464f2c968375').
narrative_ontology:cs_reading_relation('7cdc2683-bd43-456d-9fb8-464f2c968375', gpl_derivative_work_trigger__narrow_linking_permissive_reading, coexists_with).
narrative_ontology:cs_reading_relation('7cdc2683-bd43-456d-9fb8-464f2c968375', gpl_derivative_work_trigger__interface_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('7cdc2683-bd43-456d-9fb8-464f2c968375', foundational, linkage_constitutes_combined_derivation).
narrative_ontology:cs_axiom_status(linkage_constitutes_combined_derivation, holdable).
narrative_ontology:cs_axiom_grounding('7cdc2683-bd43-456d-9fb8-464f2c968375', linkage_constitutes_combined_derivation, conventional).
narrative_ontology:cs_axiom('7cdc2683-bd43-456d-9fb8-464f2c968375', foundational, reciprocity_extends_to_whole_combined_work).
narrative_ontology:cs_axiom_status(reciprocity_extends_to_whole_combined_work, holdable).
narrative_ontology:cs_axiom_grounding('7cdc2683-bd43-456d-9fb8-464f2c968375', reciprocity_extends_to_whole_combined_work, instrumental).
narrative_ontology:cs_reference_frame('7cdc2683-bd43-456d-9fb8-464f2c968375', founders_intent_reciprocal_commons).
narrative_ontology:cs_drift_state('7cdc2683-bd43-456d-9fb8-464f2c968375', contemporary_post_oracle_cloud_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7cdc2683-bd43-456d-9fb8-464f2c968375', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_contributor_community).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, downstream_source_access_users).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, dual_licensing_companies).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, embedded_device_manufacturers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_enforcement_organizations).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__broad_copyleft_reading, copyleft_reciprocity_doctrine).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__broad_copyleft_reading, combined_work_unitarity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts and publishes the GPL, maintains the interpretive FAQ stating that linking produces a covered combined work, staffs a legal team defending that position, and revises the license text across versions. Its authority rests on authorship and continuity with the founding text; it can shape future versions but cannot bind unwilling parties to its reading of already-issued grants.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, fsf_license_stewards, agenda_setter,
    institutional, generational, mobile, global).

% Pursue compliance on behalf of copyright holders, typically negotiating first and litigating when talks fail; the BusyBox suits are the emblematic campaign. Their budgets and staffing depend on a continuing supply of violations, and their charters leave little room to pivot to unrelated work.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_enforcement_organizations, agenda_setter,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_enforcement_organizations, beneficiary).

% Thousands of developers maintain kernels, compilers, and libraries under the GPL. The reading assures them that products absorbing their code must return source on the same terms. Leaving for permissively licensed projects is possible, but reputational investment, accumulated expertise, and the codebases themselves anchor them where they are.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_contributor_community, beneficiary,
    organized, generational, constrained, global).

% Firms and individuals who acquire products built on GPL components and receive the corresponding source. They can audit, patch, and rebuild without having borne any drafting or compliance burden. Their interest is access; they lose it if the disclosure duty narrows.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, downstream_source_access_users, beneficiary,
    moderate, biographical, mobile, global).

% Build commercial products that statically or dynamically link GPL libraries — toolchains, compression and crypto libraries, UI frameworks. Each linkage forces a choice: publish source, drop the component, buy a commercial exception, or re-architect behind a purchased boundary. Corporate policy manuals treat the exposure as board-level legal risk.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_software_vendors, payer,
    powerful, biographical, constrained, global).

% Ship routers, televisions, and appliances running firmware assembled from Linux, glibc, and busybox-class components. Margins are thin, firmware embeds vendor secrets, and full source release is expensive; this segment drew the densest run of enforcement actions and complies unevenly.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, embedded_device_manufacturers, payer,
    organized, biographical, constrained, global).

% Hold copyright to widely deployed GPL components and sell commercial exceptions beside the free grant. The broader the set of combinations counted as covered, the larger the customer base for paid exceptions; their revenue tracks the reading's breadth even as they give the underlying code away.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, dual_licensing_companies, beneficiary,
    organized, biographical, arbitrage, global).

% Maintain BSD- and MIT-licensed alternatives and argue publicly that linking is aggregation rather than derivation. They hold no seat in GPL interpretation or enforcement negotiations, yet their ecosystems are the practical escape route for vendors and their argument the chief intellectual rival.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, permissive_license_advocates, excluded,
    organized, biographical, mobile, global).

% Hear derivative-work disputes when enforcement reaches litigation, applying the abstraction-filtration-comparison method. Their rulings — and their frequent refusal to reach the linking question — set the practical ceiling on the reading's force. Jurisdictionally national, effectually global.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, copyright_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_derivative_work_trigger__broad_copyleft_reading, diffuse).
narrative_ontology:fixing_cost_class(gpl_derivative_work_trigger__broad_copyleft_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps code released under the GPL inside the commons as it spreads into combined works: anyone may combine, study, and redistribute, and the combination's source must flow back on the same terms. Solves the free-rider problem that would otherwise let closed products absorb commons infrastructure — kernels, compilers, libraries — without returning anything.
% TRANSFER_FUNCTION: Moves source code and modification rights from parties who combine proprietary code with GPL components to the public commons; moves the right to use maintained commons infrastructure to those same parties as the consideration for accepting the terms.
% ABSENT_VOICES: Permissive-license advocates hold the aggregation argument but no seat in GPL interpretation or enforcement; end users of noncompliant embedded products rarely know the code inside their devices; foreign courts and legislatures shape the trigger's real-world force without participating in its formulation.
% DISAPPEARANCE_RATIONALE: If the broad linking trigger vanished overnight, proprietary products would absorb GPL components without source flowing back, dual-licensing exception revenue would collapse, contributor assurance against closed absorption would evaporate, and combination practice would reorganize around permissive-style norms — the software commons economy would rearrange around the new default.
% FOUNDING_PROBLEM: In the early free-software movement, code that had been shared was being absorbed into proprietary combined works and closed again; the arrangement was built to prevent appropriation of commons code as it propagated into larger products.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration from outside the beneficiary set: vendor compliance policies and documented avoidance behavior attest the trigger's perceived legal force; the US appellate abstraction-filtration-comparison lineage supplies the doctrinal frame courts actually apply; academic copyright scholarship treats combination scope as an open, consequential question. No disinterested source certifies the founding problem's current salience — all corroboration is indirect, which is itself signal about how contested the kernel remains.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__broad_copyleft_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__broad_copyleft_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__broad_copyleft_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_derivative_work_trigger__broad_copyleft_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__broad_copyleft_reading, 0.51, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__broad_copyleft_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_derivative_work_trigger__broad_copyleft_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_derivative_work_trigger__broad_copyleft_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.51: the obligation is real, wide, and legally enforced, but it is entered by accepting a license grant whose condition is stated up front, and the authoring seat endorses the exchange — moderate, not predatory. Suppression 0.40: copyright-backed enforcement is genuine coercion, but exits are priced rather than eliminated (permissive substitutes, dual-license purchase, re-architecting), so the raw structural suppression sits mid-range; per the framework's division of labor, this scalar is unscaled — the engine amplifies or damps only extractiveness via directionality and scope. Theater ratio 0.18: compliance overwhelmingly produces real source releases; a minority of token disclosures and open-washing accounts for the performative share. Accessibility collapse 0.35: once the trigger is understood, alternatives remain visibly open — this is the opposite profile of a natural law. Resistance 0.62: the kernel contest itself is the resistance — a decades-long doctrinal and industrial campaign against exactly this reading. The measurement series run on one shared eight-point grid (every tracked metric authored at every point, t=35 marked projected as a current-year estimate); the suppression_requirement series is authored because this story specifically traces enforcement-capacity change — buildup through the 2004–2016 enforcement era, then partial normalization after community enforcement statements discouraged routine litigation. Trajectories are monotonic-drifting rather than cyclical; no intermittent-reinforcement mechanism is posited.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the steward seats compute differently from the same structural facts. From the fsf_license_stewards and gpl_contributor_community positions the arrangement is a reciprocity mechanism they built and defend — the disclosure duty is the point, not a defect. From the proprietary_software_vendors and embedded_device_manufacturers positions the same instrument operates as an enforced cost levied on their business models, with the added grievance that the derivativeness determination itself is unsettled doctrine. Same-level lateral differentiation matters here: proprietary vendors (powerful) and embedded manufacturers (organized) hold the identical payer role under identical nominal obligations, but exit quality differs — vendors can re-architect, buy exceptions, or fund permissive replacements, while thin-margin device makers face entangled firmware and comply unevenly, which is why enforcement historically concentrated on them. The copyright_courts seat sees neither coordination nor extraction but an unresolved doctrinal question they frequently decline to reach.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: gpl_contributor_community and downstream_source_access_users sit near the subsidized pole (the arrangement delivers them commons integrity and source access at no compliance cost), and dual_licensing_companies sit near it with an arbitrage twist — the broader the reading, the larger their exception market. Victim declarations drive high directionality: proprietary_software_vendors and embedded_device_manufacturers bear the transfer directly with constrained exits, placing them near the full-target pole. The fsf_license_stewards collect interpretive authority rather than fees, keeping them near the beneficiary end as administrators; gpl_enforcement_organizations are dual-positioned (administer and benefit from violation supply). No directionality overrides were needed: the beneficiary/victim declarations plus exit options reproduce the true relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — appropriation of shared code as it propagates into combined works — remains live, so no mandatrophy resolution is declared and none should be inferred from the modest post-2016 enforcement decline. The classification guards against both symmetrical mislabelings. Read from the vendor seats alone, the trigger looks like pure imposed extraction; declaring the beneficiary structure prevents that error by recording the genuine coordination function the same instrument performs. Read from the steward seats alone, it looks like pure commons coordination; declaring the victims and the enforcement requirement prevents that error by recording the concentrated, coerced cost asymmetry riding on the same structure. The temporal series watches the failure mode specific to this arrangement: if enforcement capacity decays further while the nominal obligation persists, the structure drifts toward inertial maintenance — obligations honored mainly by the already-compliant, enforced against no one — which the theater_ratio and suppression_requirement trajectories are positioned to detect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint instantiates the broad_copyleft_reading of kernel gpl_derivative_work_trigger; how would the sibling readings (narrow_linking_permissive_reading, interface_boundary_reading) change the structural facts if adopted?',
    'Authoring the sibling stories and comparing computed classifications; definitive appellate doctrine on whether linking produces a derivative work.',
    'Under the narrow reading the victim set shrinks to verbatim modifiers, epsilon drops toward coordination-only levels, and the type pulls toward rope; under the interface-boundary reading extraction concentrates on opaque coupling and clean-API users exit the obligation entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer-frame omega: this file is one reading of a contested kernel; sibling readings instantiate different constraints with different epsilon and victim sets.').

omega_variable(
    dynamic_linking_derivativeness_uncertainty,
    'Does runtime dynamic linking, absent literal code copying, produce a derivative work under copyright''s abstraction-filtration-comparison test?',
    'Appellate application of the abstraction-filtration-comparison method to representative linking scenarios, or legislative clarification of combination scope.',
    'Resolves the largest block of vendor-side legal uncertainty; a negative answer collapses most of the measured burden for dynamically linked products and shrinks the effective governed set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dynamic_linking_derivativeness_uncertainty, empirical, 'Whether the technical act of dynamic linking meets the legal test for derivation.').

omega_variable(
    disclosure_benefit_incidence,
    'Who actually captures the value of mandated source disclosure — downstream users exercising modification rights, or competitors of the disclosing vendor?',
    'Empirical study of reuse of disclosed source in competitor products versus end-user modification and rebuild rates.',
    'If competitors are the primary recipients, the beneficiary declaration overstates user-facing benefit and the coordination story weakens toward rent-shifting between firms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disclosure_benefit_incidence, empirical, 'Incidence of the disclosure mandate''s benefits across declared beneficiary groups.').

omega_variable(
    enforcement_capacity_trajectory,
    'Will enforcement capacity continue its post-2016 normalization, or resurge through strategic litigation against high-value targets?',
    'Track enforcement filing rates, enforcement-organization funding, and settlement volumes over the coming decade.',
    'Rising enforcement raises effective suppressive force and pushes holdout-segment operation toward harder-edged extraction; continued decay leaves the trigger as normative background with the temporal signature of inertial maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_trajectory, empirical, 'Future path of the enforcement machinery that holds the obligation in place.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__broad_copyleft_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 0, 0.07).
narrative_ontology:measurement_basis(gpl__tr_t0, observed).
narrative_ontology:measurement(gpl__tr_t5, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement_basis(gpl__tr_t5, observed).
narrative_ontology:measurement(gpl__tr_t10, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement_basis(gpl__tr_t10, observed).
narrative_ontology:measurement(gpl__tr_t15, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 15, 0.14).
narrative_ontology:measurement_basis(gpl__tr_t15, observed).
narrative_ontology:measurement(gpl__tr_t20, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement_basis(gpl__tr_t20, observed).
narrative_ontology:measurement(gpl__tr_t25, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 25, 0.15).
narrative_ontology:measurement_basis(gpl__tr_t25, observed).
narrative_ontology:measurement(gpl__tr_t30, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 30, 0.17).
narrative_ontology:measurement_basis(gpl__tr_t30, observed).
narrative_ontology:measurement(gpl__tr_t35, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 35, 0.18).
narrative_ontology:measurement_basis(gpl__tr_t35, projected).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(gpl__be_t0, observed).
narrative_ontology:measurement(gpl__be_t5, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 5, 0.41).
narrative_ontology:measurement_basis(gpl__be_t5, observed).
narrative_ontology:measurement(gpl__be_t10, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement_basis(gpl__be_t10, observed).
narrative_ontology:measurement(gpl__be_t15, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement_basis(gpl__be_t15, observed).
narrative_ontology:measurement(gpl__be_t20, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement_basis(gpl__be_t20, observed).
narrative_ontology:measurement(gpl__be_t25, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 25, 0.55).
narrative_ontology:measurement_basis(gpl__be_t25, observed).
narrative_ontology:measurement(gpl__be_t30, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 30, 0.53).
narrative_ontology:measurement_basis(gpl__be_t30, observed).
narrative_ontology:measurement(gpl__be_t35, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 35, 0.51).
narrative_ontology:measurement_basis(gpl__be_t35, projected).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement_basis(gpl__su_t0, observed).
narrative_ontology:measurement(gpl__su_t5, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 5, 0.08).
narrative_ontology:measurement_basis(gpl__su_t5, observed).
narrative_ontology:measurement(gpl__su_t10, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 10, 0.15).
narrative_ontology:measurement_basis(gpl__su_t10, observed).
narrative_ontology:measurement(gpl__su_t15, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 15, 0.28).
narrative_ontology:measurement_basis(gpl__su_t15, observed).
narrative_ontology:measurement(gpl__su_t20, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement_basis(gpl__su_t20, observed).
narrative_ontology:measurement(gpl__su_t25, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 25, 0.46).
narrative_ontology:measurement_basis(gpl__su_t25, observed).
narrative_ontology:measurement(gpl__su_t30, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 30, 0.43).
narrative_ontology:measurement_basis(gpl__su_t30, observed).
narrative_ontology:measurement(gpl__su_t35, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 35, 0.4).
narrative_ontology:measurement_basis(gpl__su_t35, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__broad_copyleft_reading, resource_allocation).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, narrow_linking_permissive_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, interface_boundary_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the GPL linking trigger' decomposes into three structurally distinct readings of one kernel (gpl_derivative_work_trigger): broad_copyleft_reading (this file — any linking derives; epsilon approximately 0.51, wide victim set spanning all linkers), narrow_linking_permissive_reading (only modifications to GPL code itself derive; epsilon far lower, victims shrink to verbatim modifiers), and interface_boundary_reading (clean API boundaries bar derivation even under tight coupling; epsilon concentrates on opaque-coupling cases). Each is a separate file with its own epsilon, beneficiaries, and victims, per the epsilon-invariance principle. They are linked here because the broad reading is the interpretive baseline against which the other two define themselves, and broad-reading enforcement activity shaped the litigation environment in which the interface-boundary argument matured.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
