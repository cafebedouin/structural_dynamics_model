% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_commons_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_commons_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_commons_reading
 *   human_readable: GPL Mandatory Reciprocity Obligation (Commons-Defense Reading)
 *   domain: economic/legal/technological
 *
 * SUMMARY:
 *   Under the commons reading, the GPL's reciprocity condition is an
 *   institutional technology for defending a shared code pool against
 *   enclosure: anyone who distributes a derivative must offer it under the
 *   same terms, so improvements cannot be privately captured. The standing
 *   arrangement under assessment is the reciprocity obligation itself as it
 *   operates today — binding at redistribution, silent on internal use,
 *   enforced by copyright holders and specialized compliance organizations,
 *   and increasingly stressed by delivery modes (hosted services, model
 *   training) that never 'distribute' in the license's sense. KEY AGENTS (by
 *   structural relationship): - software_commons_institution: institutional
 *   beneficiary (not an actor; the pool and its normative order) -
 *   gpl_contributors: primary beneficiary (moderate/mobile) — contribute
 *   under capture protection - downstream_users_derivative_projects:
 *   beneficiary (moderate/mobile) — free use, reciprocity only at
 *   redistribution - proprietary_integration_seekers: primary target
 *   (powerful/constrained) — bear the foreclosed-option cost -
 *   relicensing_exit_maximizers: target (moderate/trapped) — cannot reclaim
 *   pooled value for private sale - enforcement_organizations: agenda setter
 *   (organized/generational) — set and police the boundary -
 *   dual_licensing_copyright_holders: secondary beneficiary with a local
 *   capture channel (powerful/arbitrage) - unexercised_rights_device_owners:
 *   excluded voice (powerless/trapped) - ip_courts_legal_scholars: analytical
 *   observer (institutional).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.55).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.55).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.14).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 0.14).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_commons_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_commons_reading, "GPL Mandatory Reciprocity Obligation (Commons-Defense Reading)").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_commons_reading, "economic/legal/technological").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_commons_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_commons_reading, '1c2ba61e-b4bd-4f80-8d1e-2412f1189ba3').
narrative_ontology:cs_kernel_codification('1c2ba61e-b4bd-4f80-8d1e-2412f1189ba3', fixed_text).
narrative_ontology:cs_authority_grounding('1c2ba61e-b4bd-4f80-8d1e-2412f1189ba3', lineage).
narrative_ontology:cs_interpretation_layer_present('1c2ba61e-b4bd-4f80-8d1e-2412f1189ba3').
narrative_ontology:cs_reading_relation('1c2ba61e-b4bd-4f80-8d1e-2412f1189ba3', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('1c2ba61e-b4bd-4f80-8d1e-2412f1189ba3', gpl_reciprocity_obligation__copyleft_as_restriction_reading, influences).
narrative_ontology:cs_axiom('1c2ba61e-b4bd-4f80-8d1e-2412f1189ba3', foundational, mandatory_reciprocity_sustains_commons_production).
narrative_ontology:cs_axiom_status(mandatory_reciprocity_sustains_commons_production, holdable).
narrative_ontology:cs_axiom_grounding('1c2ba61e-b4bd-4f80-8d1e-2412f1189ba3', mandatory_reciprocity_sustains_commons_production, empirically_contingent).
narrative_ontology:cs_axiom('1c2ba61e-b4bd-4f80-8d1e-2412f1189ba3', secondary, improvement_return_is_owed_not_gratuitous).
narrative_ontology:cs_axiom_status(improvement_return_is_owed_not_gratuitous, holdable).
narrative_ontology:cs_axiom_grounding('1c2ba61e-b4bd-4f80-8d1e-2412f1189ba3', improvement_return_is_owed_not_gratuitous, deontological).
narrative_ontology:cs_reference_frame('1c2ba61e-b4bd-4f80-8d1e-2412f1189ba3', defended_commons_boundary).
narrative_ontology:cs_drift_state('1c2ba61e-b4bd-4f80-8d1e-2412f1189ba3', cloud_and_model_training_delivery_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1c2ba61e-b4bd-4f80-8d1e-2412f1189ba3', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, software_commons_institution).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_contributors).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, downstream_users_derivative_projects).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_integration_seekers).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, relicensing_exit_maximizers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, dual_licensing_copyright_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The accumulated body of GPL-licensed code together with the normative order that governs contributions to it. Every compliant distribution returns improvements to this pool, and its boundary is maintained by the license terms themselves. It is not an actor: it cannot move, renegotiate, or decline what flows into it, and it persists only insofar as contributors and distributors keep acting within its terms.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, software_commons_institution, beneficiary,
    institutional, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(gpl_reciprocity_obligation__copyleft_as_commons_reading, software_commons_institution).

% Developers who publish work under GPL terms. Their contributions cannot be turned into closed products by anyone downstream, and they receive the whole pool's improvements back free of charge. They may stop contributing at any time without penalty; their past work remains in the pool, which costs them nothing they did not already give.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_contributors, beneficiary,
    moderate, biographical, mobile, global).

% Projects and end users who take GPL code, modify it, and run it internally or redistribute it under the same terms. They obtain a production-grade shared base at zero license cost and inherit every upstream fix. The only obligation attaches at redistribution: publish corresponding source. Internal use carries no obligations at all.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, downstream_users_derivative_projects, beneficiary,
    moderate, biographical, mobile, global).

% Firms that want to ship closed-source products incorporating GPL components, typically embedded devices, appliances, or enterprise stacks built on GPL infrastructure such as the Linux kernel. Their options are publishing their derivative source, engineering the GPL code out at significant cost, purchasing a commercial exception where one exists, or distributing non-compliantly and carrying litigation risk. The value they want sits inside the pool, so walking away means rebuilding substitutes of uncertain quality.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_integration_seekers, payer,
    powerful, biographical, constrained, global).

% Developers or founders who contributed code to a GPL project and later want to commercialize it under proprietary terms. Their work is now entangled with other people's contributions and derivative improvements, so the asset they want to sell cannot be cleanly separated from the pool. They can abandon the code, but they cannot reclaim the accumulated value around it.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, relicensing_exit_maximizers, payer,
    moderate, biographical, trapped, global).

% The Free Software Foundation, Software Freedom Conservancy, and comparable bodies that hold copyright on portions of the pool, publish license versions and interpretive guidance, and pursue compliance through correspondence and, occasionally, litigation. They set the boundary's practical meaning and decide which violations to pursue. They could reduce or halt enforcement activity at will; doing so would erode the boundary rather than free them from any burden.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, enforcement_organizations, agenda_setter,
    organized, generational, mobile, global).

% Copyright holders of projects offered under both GPL and a paid commercial license. The GPL side builds the contributor base and forbids closed redistribution, which channels commercial integrators toward purchasing the paid exception. They control both sides of the offer for their own projects and can adjust commercial terms as market conditions change.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, dual_licensing_copyright_holders, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_commons_reading, dual_licensing_copyright_holders, agenda_setter).

% Consumers who bought devices running GPL firmware without receiving the offer of corresponding source. The license grants them rights they overwhelmingly do not know they hold and lack the technical or legal capacity to exercise. They are absent from license-policy debates, which occur among foundations, corporate counsel, and large adopters.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, unexercised_rights_device_owners, excluded,
    powerless, immediate, trapped, global).

% Courts and academic commentators who adjudicate whether the license is contract, license, or conditional gift, whether its terms are enforceable, and what counts as distribution or derivative work. German decisions have upheld the license's enforceability; US litigation has mostly settled before ruling. They take no side in the policy dispute but determine the boundary's legal reality.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, ip_courts_legal_scholars, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_reciprocity_obligation__copyleft_as_commons_reading, diffuse).
narrative_ontology:fixing_cost_class(gpl_reciprocity_obligation__copyleft_as_commons_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the enclosure problem in collaborative code production: by conditioning redistribution on identical terms, it guarantees that improvements to the shared pool return to the pool, which keeps contribution rational for anyone who fears their work will be captured and closed.
% TRANSFER_FUNCTION: Moves option value rather than money in the general case: distributors surrender the possibility of exclusive proprietary exploitation of pooled code, and the surrendered option is converted into guaranteed source access and improvement-return for everyone in the pool. In dual-licensed projects, part of that surrendered option converts into license-fee revenue for the copyright holder.
% ABSENT_VOICES: Device owners holding unexercised GPL rights, developers deterred from evaluating GPL code by compliance anxiety and never surveyed, and firms locked out of integration who have no seat in license-policy forums dominated by foundations and large-adopter counsel. Their objections exist but are not voiced where license versions and enforcement priorities are set.
% DISAPPEARANCE_RATIONALE: If the reciprocity condition vanished overnight, proprietary forks of every major GPL project would appear within months, contribution incentives would restructure around whatever new defense emerged or none, and the current landscape of shared infrastructure would fragment into a permissive-licensed patchwork with capture by whoever moved fastest. Arrangements across the entire free-software economy depend on the boundary holding.
% FOUNDING_PROBLEM: In the early 1980s, software that had circulated openly was being closed by employers and vendors, leaving developers unable to cooperate or repair their tools. The GNU project needed a legal instrument that would let code accumulate publicly without any single distributor converting the accumulation into a closed product.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: the documented pre-GPL record of commons collapse (proprietary Unix fragmentation, the BSD litigation era), the litigation record of attempted capture (BusyBox enforcement actions, Hellwig v. VMware, SFC v. Vizio), and academic commons-governance literature treating defended boundaries as a precondition of sustained shared production. Courts enforcing the terms attest the obligation's operational reality independently of any advocacy organization's self-description.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_commons_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_commons_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_commons_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_commons_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_commons_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_commons_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_reciprocity_obligation__copyleft_as_commons_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claim is tangled_rope because the structure shows all three canonical elements: a genuine coordination function (enclosure prevention keeps contribution rational), asymmetric position (distributors seeking closed derivatives bear a cost no one else bears), and active enforcement (copyright litigation and organized compliance campaigns). Extractiveness ends at 0.55: the burden on exit-maximizers is real and grows with the pool's value, but it is declared in advance, attaches only at voluntary incorporation, and leaves permissive-licensed and original-code alternatives fully available. Suppression (0.55) reflects the enforcement machinery the boundary requires — the license does not self-execute, and every violation must be threatened or sued — while being narrowly targeted at one prohibited act rather than at participation broadly; the suppression_requirement series traces the build-out of that machinery from informal norms (T0) through professionalized enforcement (T20 onward). Theater is low throughout (0.10-0.17): compliance checking and source publication are functional acts, with only ritual residue in unread license headers and fundraising rhetoric. Accessibility_collapse (0.58) is moderate: within a chosen GPL codebase the non-reciprocal path collapses almost entirely, but the meta-alternatives (other licenses, dual-license purchase, engineering around) remain open. Resistance (0.45) is persistent but bounded: corporate preference for permissive licenses, periodic validity challenges, and ecosystem gravity toward MIT/Apache defaults, against mass voluntary adoption. All three series share one eight-point grid so no metric is sampled against another's end-state.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute divergent types from identical structural data. From proprietary_integration_seekers the arrangement operates as a wall around assets they helped make valuable, with exit priced at rebuild-or-publish; from gpl_contributors and downstream users it operates as ordinary infrastructure whose only rule is fairness at the gate. The enforcement seat experiences neither extraction nor subsidy but boundary maintenance as a vocation. The engine computes these per-seat classifications from power, exit, and declared position; this story does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries sit near the subsidized end: contributors and downstream users receive the pool's value without bearing the reciprocity burden (their exit is mobile — they can leave anytime at no cost), and the commons-as-institution is marked agent=false precisely so a non-actor does not feed the directionality arithmetic. Declared victims sit near the full-target end: integration seekers are constrained (the value they want is inside the pool) and relicensing exit-maximizers are trapped (their desired asset is inseparable from others' contributions), which amplifies their effective extraction beyond what nominal power alone would predict. Enforcement organizations derive their position from the agenda_setter role rather than from either declaration. Dual-licensing holders are the one seat where the derivation needs qualification: they are declared beneficiaries, but their arbitrage exit and their sale of the very option the license forecloses makes them partially captured by the arrangement — see the dual_licensing_capture_exception omega rather than a hard-coded override, since the capture is project-local, not ecosystem-wide.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing enclosure of collectively built software — is live, corroborated by ongoing capture attempts and by the delivery-mode leakage described in the saas_enclosure_frontier omega; mandatrophy is not resolved and no sunset applies. The classification matters here because the arrangement's extraction is its mechanism, not its corruption: reading the burden on exit-maximizers as pure extraction (snare) would erase the coordination function that justifies it, and reading the coordination story at face value (rope) would erase the identifiable parties who pay for a benefit they declined. The live drift risk runs the other way: if hosted-service and training-time capture proceed unchecked because the distribution trigger never fires, the boundary's enforcement becomes performance aimed at yesterday's enclosure vector — theater_ratio climbing while the real action migrates outside the license's reach — which is the recognizable onset of piton dynamics. The temporal series is designed to make that transition detectable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Does the commons-reading''s beneficiary/victim structure (commons as institution benefits; individual exit-maximizers pay) correctly locate the obligation''s structural positions, or do the sibling readings instantiate genuinely different constraints over the same license text?',
    'Cross-reading comparison of computed per-seat classifications: generate copyleft_as_freedom_reading and copyleft_as_restriction_reading as separate stories and compare seat-level types and epsilon against this file.',
    'If the freedom-reading computes materially different victim sets (end users as beneficiaries of enforced freedoms rather than the institution), the kernel''s readings are indexically distinct constraints and corpus-level aggregation over ''GPL'' is invalid; if they converge, the readings are rhetorical variants of one structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'This story is one reading of kernel gpl_reciprocity_obligation; sibling readings may instantiate different constraints.').

omega_variable(
    dual_licensing_capture_exception,
    'Is the diffuse-receipt verdict stable, given that dual-licensed projects convert the foreclosed proprietary option into fee revenue captured by a single copyright holder?',
    'Measure the share of the GPL ecosystem by value under dual-licensing monetization versus pure reciprocity; if monetized capture approaches majority share, re-run the receipt analysis per project class.',
    'If dual licensing dominates, gain_flow shifts to dual_licensing_copyright_holders and the arrangement drifts toward price discrimination wearing a commons costume; if it remains marginal, the diffuse verdict stands for the general obligation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_licensing_capture_exception, empirical, 'Whether localized capture channels overturn the no-single-capturer finding.').

omega_variable(
    entry_voluntariness_vs_ecosystem_trap,
    'Is the burden on exit-maximizers consensual-at-entry (they chose to incorporate GPL code knowing the terms) or structurally coercive (for critical infrastructure like the kernel, no realistic substitute exists, so incorporation is not meaningfully optional)?',
    'Counterfactual substitutability analysis for essential GPL components: cost and feasibility of equivalent permissive-licensed or original replacements at current quality thresholds.',
    'If substitutes are unrealizable for core infrastructure, effective coercion rises above what entry-voluntariness suggests and the payer seats'' computed extraction amplifies; if substitutes are viable, the burden prices as an accepted license term.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entry_voluntariness_vs_ecosystem_trap, empirical, 'Whether voluntariness at entry neutralizes the trapped-exit position of integration seekers.').

omega_variable(
    commons_vs_contributor_beneficiary_primacy,
    'Do realized gains accrue to flesh-and-blood contributors and users, or does the institution capture symbolic value while maintenance labor goes unpaid and downstream firms extract uncompensated commercial value?',
    'Maintainer-compensation surveys cross-referenced against measured commercial value of GPL-dependent products; compare labor supplied to value retained at each seat.',
    'If unpaid labor materially subsidizes commercial users, the beneficiary structure tilts toward the firms and the declared polarity understates extraction from the contributor class; the agent=false marking of the commons would then be masking the real capture question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_vs_contributor_beneficiary_primacy, empirical, 'Whether the institutional beneficiary designation conceals an uncompensated-labor transfer.').

omega_variable(
    saas_enclosure_frontier,
    'Does the reciprocity boundary hold as software delivery shifts to hosted services and model training, where the distribution trigger never fires?',
    'Track AGPL adoption rates, enclosure incidents in GPL-only projects exposed to service delivery, and enforcement outcomes against service-mode use.',
    'If service-mode enclosure proceeds unchecked, the constraint''s coordination function decays for the growing share of the economy delivered as a service, driving theater_ratio up and initiating piton drift for classic GPL; if AGPL-style patches contain the leak, the boundary remains functional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(saas_enclosure_frontier, empirical, 'Whether the distribution-triggered boundary survives delivery-mode migration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_commons_tr_t0, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gpl_commons_tr_t5, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 5, 0.11).
narrative_ontology:measurement(gpl_commons_tr_t10, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement(gpl_commons_tr_t15, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 15, 0.17).
narrative_ontology:measurement(gpl_commons_tr_t20, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(gpl_commons_tr_t25, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 25, 0.15).
narrative_ontology:measurement(gpl_commons_tr_t30, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 30, 0.13).
narrative_ontology:measurement(gpl_commons_tr_t35, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 35, 0.14).

% Extraction over time
narrative_ontology:measurement(gpl_commons_be_t0, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(gpl_commons_be_t5, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 5, 0.43).
narrative_ontology:measurement(gpl_commons_be_t10, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 10, 0.49).
narrative_ontology:measurement(gpl_commons_be_t15, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(gpl_commons_be_t20, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(gpl_commons_be_t25, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 25, 0.55).
narrative_ontology:measurement(gpl_commons_be_t30, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 30, 0.53).
narrative_ontology:measurement(gpl_commons_be_t35, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 35, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(gpl_commons_su_t0, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(gpl_commons_su_t5, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 5, 0.16).
narrative_ontology:measurement(gpl_commons_su_t10, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 10, 0.24).
narrative_ontology:measurement(gpl_commons_su_t15, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(gpl_commons_su_t20, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(gpl_commons_su_t25, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 25, 0.56).
narrative_ontology:measurement(gpl_commons_su_t30, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(gpl_commons_su_t35, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 35, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_commons_reading, resource_allocation).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, copyleft_as_freedom_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, copyleft_as_restriction_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'GPL reciprocity' decomposes into three structurally distinct readings of one kernel: this commons-reading (beneficiary: the commons as institution; targets: exit-maximizers; medium epsilon), the freedom-reading (beneficiary: end-user freedoms; targets: proprietary licensors), and the restriction-reading (the same terms experienced purely as business-model prohibition). Each reading assigns a different beneficiary/victim polarity and therefore a different epsilon over the same license text; they are separate constraint stories linked here, not one story with a measurement parameter. This file instantiates only the commons-reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
