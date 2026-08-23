% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__interface_boundary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__interface_boundary_reading, []).

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
 *   constraint_id: gpl_derivative_work_trigger__interface_boundary_reading
 *   human_readable: Interface-Boundary Aggregation Convention (GPL Derivative-Work Trigger)
 *   domain: legal/software-licensing/open-source-governance
 *
 * SUMMARY:
 *   A doctrinal convention in open-source governance holds that proprietary
 *   code interacting with GPL-licensed libraries across clean, documented API
 *   boundaries constitutes aggregation rather than derivation — even when the
 *   coupling is technically tight (dynamic linking, shared address spaces,
 *   embedded interpreters) — so that copyleft source obligations stop at the
 *   interface and do not reach the caller. This file instantiates ONE reading
 *   of the contested kernel gpl_derivative_work_trigger; the broad copyleft
 *   reading and the narrow permissive reading are separate constraints with
 *   their own epsilon values and target sets, linked through the network
 *   section. Per the epsilon-referent rule, extractiveness here is authored
 *   for the standing arrangement under contest — the
 *   interface-boundary-governed mixed-licensing ecosystem — as THIS reading
 *   assesses it, not as the broad reading would assess the same arrangement.
 *   The generation brief hypothesized a scaffold; analysis refined the claim
 *   to tangled_rope (reasoning in mandatrophy_analysis and the
 *   scaffold_vs_steady_state_question omega): the convention carries no
 *   declared sunset anywhere in its instruments, its adherents defend it as
 *   permanent doctrine, and the transitional character the scaffold
 *   hypothesis detects is real but instantiation-specific — routed to the
 *   omega and the enforcement trajectory rather than forced into the claim.
 *   KEY AGENTS (by structural relationship): see key_agents; interval maps
 *   0=1991 (GPLv2/LGPLv1, mere-aggregation language) to 35=2026
 *   (post-Google-v.-Oracle, AI-training era).
 *
 * KEY AGENTS:
 *   - gpl_copyright_holders_stewards: Agenda setter (institutional/constrained) — administers the boundary, issues exceptions and interpretive statements, chooses whether to assert broader ownership claims
 *   - proprietary_ecosystem_integrators: Primary beneficiary (powerful/constrained) — captures infrastructure value behind closed application layers
 *   - gpl_contributor_community: Primary payer (organized/constrained) — supplies the commons; absorbs the reciprocity shortfall
 *   - combined_stack_end_users: Secondary payer (moderate/constrained) — surrenders visibility and control of the closed layers atop open infrastructure
 *   - corporate_open_source_counsel: Secondary beneficiary (institutional/mobile) — operationalizes the convention as compliance practice
 *   - free_software_legal_advocacy_orgs: Contesting observer (organized/analytical) — supplies the external pressure the convention's enforcement must continuously answer
 *   - judicial_authorities: Decisive observer (institutional/analytical) — their silence is load-bearing
 *   - saas_operators_on_gpl_infrastructure: Excluded party (powerful/arbitrage) — network use escapes every reading; absent from the conversation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__interface_boundary_reading, 0.38).
domain_priors:suppression_score(gpl_derivative_work_trigger__interface_boundary_reading, 0.42).
domain_priors:theater_ratio(gpl_derivative_work_trigger__interface_boundary_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__interface_boundary_reading, tangled_rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__interface_boundary_reading, "Interface-Boundary Aggregation Convention (GPL Derivative-Work Trigger)").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__interface_boundary_reading, "legal/software-licensing/open-source-governance").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__interface_boundary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__interface_boundary_reading, '7e72afc9-3fc1-4d4c-9c07-d7020ca0da61').
narrative_ontology:cs_kernel_codification('7e72afc9-3fc1-4d4c-9c07-d7020ca0da61', fixed_text).
narrative_ontology:cs_authority_grounding('7e72afc9-3fc1-4d4c-9c07-d7020ca0da61', practice).
narrative_ontology:cs_interpretation_layer_present('7e72afc9-3fc1-4d4c-9c07-d7020ca0da61').
narrative_ontology:cs_reading_relation('7e72afc9-3fc1-4d4c-9c07-d7020ca0da61', gpl_derivative_work_trigger__broad_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('7e72afc9-3fc1-4d4c-9c07-d7020ca0da61', gpl_derivative_work_trigger__narrow_linking_permissive_reading, coexists_with).
narrative_ontology:cs_axiom('7e72afc9-3fc1-4d4c-9c07-d7020ca0da61', foundational, invocation_across_documented_interface_is_not_transformation).
narrative_ontology:cs_axiom_status(invocation_across_documented_interface_is_not_transformation, holdable).
narrative_ontology:cs_axiom_grounding('7e72afc9-3fc1-4d4c-9c07-d7020ca0da61', invocation_across_documented_interface_is_not_transformation, empirically_contingent).
narrative_ontology:cs_axiom('7e72afc9-3fc1-4d4c-9c07-d7020ca0da61', secondary, boundary_discipline_sustains_aggregation_treatment).
narrative_ontology:cs_axiom_status(boundary_discipline_sustains_aggregation_treatment, holdable).
narrative_ontology:cs_axiom_grounding('7e72afc9-3fc1-4d4c-9c07-d7020ca0da61', boundary_discipline_sustains_aggregation_treatment, instrumental).
narrative_ontology:cs_reference_frame('7e72afc9-3fc1-4d4c-9c07-d7020ca0da61', interface_boundary_aggregation_norm).
narrative_ontology:cs_drift_state('7e72afc9-3fc1-4d4c-9c07-d7020ca0da61', post_google_v_oracle_ai_training_era, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('7e72afc9-3fc1-4d4c-9c07-d7020ca0da61', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__interface_boundary_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__interface_boundary_reading, proprietary_ecosystem_integrators).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__interface_boundary_reading, corporate_open_source_counsel).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__interface_boundary_reading, gpl_contributor_community).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__interface_boundary_reading, combined_stack_end_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__interface_boundary_reading, gpl_contributor_community).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__interface_boundary_reading, combined_stack_end_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publish and maintain GPL-licensed libraries and kernels; decide which interfaces receive explicit use exceptions, publish interpretive statements about where license obligations begin and end, and choose whether to assert broader ownership claims against integrators. They can tighten or relax the boundary at any time, but doing so risks splitting their contributor base or collapsing the adoption that funds the project's relevance. Relicensing wholesale requires aggregating copyrights from many contributors, which bounds their practical freedom of movement.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, gpl_copyright_holders_stewards, agenda_setter,
    institutional, generational, constrained, global).

% Build commercial products that call GPL libraries and kernels through their published interfaces; their application-layer source stays closed while the underlying infrastructure stays open. They staff compliance teams to keep product code on documented interfaces and away from internal symbols, and they budget for legal risk that a court could someday disagree with the boundary convention. Moving to permissively licensed infrastructure would mean re-engineering products around different kernels and libraries, so they adapt to boundary changes rather than leave.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, proprietary_ecosystem_integrators, beneficiary,
    powerful, biographical, constrained, global).

% Write and maintain the GPL code that integrators build on. They receive adoption, bug reports, compatibility testing, and occasional upstreamed fixes from commercial users, and their work runs widely deployed systems. When integrators capture the value of that infrastructure in closed products without contributing comparable source back, the shortfall lands on them as unpaid common maintenance and on the license's reciprocity promise as a partial hollowing-out. Withholding their labor or forking is possible but means abandoning the project they built.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, gpl_contributor_community, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(gpl_derivative_work_trigger__interface_boundary_reading, gpl_contributor_community, beneficiary).

% Run phones, appliances, servers, and services assembled from open GPL infrastructure underneath closed proprietary components. They can inspect and modify the open layers, obtain security patches for them, and gain from the competition and capability the mixed model produces. They cannot study, modify, or repair the closed layers, cannot verify what those layers do with their data, and depend on vendors for updates to the whole stack. Switching to fully open stacks exists as an option but at real cost in capability and convenience.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, combined_stack_end_users, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(gpl_derivative_work_trigger__interface_boundary_reading, combined_stack_end_users, beneficiary).

% Publish legal analyses arguing that linking produces derivative works and that the boundary convention understates license obligations; fund enforcement litigation against integrators they judge non-compliant; press stewards to tighten boundaries. They hold no administrative authority over the convention, but their campaigns are the main external pressure the convention must continuously answer, and their litigation wins and losses move the risk calculus every integrator budgets for.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, free_software_legal_advocacy_orgs, observer,
    organized, generational, analytical, global).

% In-house and firm lawyers who translate the boundary convention into compliance policy: interface audits, contribution policies, exception drafting, acquisition due diligence. Their practice exists because the convention holds; a definitive contrary court ruling would collapse much of its value overnight, while a definitive confirming ruling would commoditize it. They move between employers and firms and advise whichever position the legal environment rewards.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, corporate_open_source_counsel, beneficiary,
    institutional, biographical, mobile, global).

% Courts and copyright offices that have so far declined to rule decisively on whether interacting across an interface creates a derivative work, deciding adjacent questions (copyrightability of API declaring code, fair use in reimplementation) whose implications bleed into the question without settling it. Their silence is load-bearing: the convention persists in the space their indecision leaves.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, judicial_authorities, observer,
    institutional, civilizational, analytical, national).

% Run GPL-licensed software as network services without distributing binaries, offering the functionality through interfaces of their own. Because no distribution occurs, the license's source-sharing trigger never fires under any reading, and they sit outside the boundary debate entirely. They would object to any tightening that reached network use, but no seat in the interpretive conversation represents them, and their business model quietly defines the escape hatch the convention's critics point to.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, saas_operators_on_gpl_infrastructure, excluded,
    powerful, biographical, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_derivative_work_trigger__interface_boundary_reading, proprietary_ecosystem_integrators).
narrative_ontology:fixing_cost_class(gpl_derivative_work_trigger__interface_boundary_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Lets two licensing regimes compose into single product stacks: a stable, documented interface contract divides labor so that open infrastructure is maintained once, publicly, and proprietary application investment builds on it without either side absorbing the other's obligations. Without it, every mixed stack faces an all-or-nothing choice between full disclosure and abandoning shared infrastructure.
% TRANSFER_FUNCTION: Moves infrastructure value — maintenance, reliability, security patching of GPL components — into closed proprietary products without reciprocal source return, and moves adoption, compatibility investment, bug reports, and occasional upstreamed fixes back to the commons. Net direction over the interval runs from the contributor community and from combined-stack users (who surrender visibility and control of the closed layers) toward proprietary integrators.
% ABSENT_VOICES: Combined-stack end users appear in license debates only through proxy organizations; non-commercial redistributors without counsel face the same boundary questions with none of the interpretive resources; and SaaS operators — whose network delivery escapes every reading's trigger — are entirely outside the conversation while quietly defining its largest loophole. Courts, the only seat able to settle the question, have declined to occupy it.
% DISAPPEARANCE_RATIONALE: If the convention vanished overnight — courts adopting the broad reading — thousands of shipping products would become non-compliant simultaneously: integrators would face a litigation wave or scramble to open application layers, relicense stacks, or re-engineer onto permissive infrastructure; GPL projects would lose the adoption that sustains them; device and service prices and architectures would reorganize around whichever escape routes remained.
% FOUNDING_PROBLEM: Early copyleft adoption stalled commercially: businesses would not ship products on GPL libraries if doing so obligated disclosing their entire application source, and permissive-licensed competitors were absorbing the infrastructure niche. The interface-boundary convention (with the LGPL and explicit exceptions as its institutional forms) was articulated to let mixed stacks ship — keeping copyleft infrastructure economically viable while containing disclosure obligations to the licensed components themselves.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the beneficiary set: the Free Software Foundation's own historical rationales for the LGPL and library exceptions (authored by the convention's chief critics, who concede the composition problem is real while disputing this reading's answer to it); academic copyright scholarship on derivative works and program interaction; and integrator-side testimony in the Google v. Oracle proceedings documenting the commercial stakes. No corroborating source claims the problem is solved.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__interface_boundary_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__interface_boundary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__interface_boundary_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_derivative_work_trigger__interface_boundary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__interface_boundary_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__interface_boundary_reading_tests).
:- end_tests(gpl_derivative_work_trigger__interface_boundary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.38): the convention coordinates genuinely — mixed stacks ship that otherwise would not — while a reciprocity shortfall accumulates on the contributor side and a visibility/control shortfall accumulates on the user side; the reading's own lights count much of the flow as consented license terms, which caps epsilon below pure-extraction territory. Suppression (0.42) reflects the convention's dependence on active maintenance — steward statements, compliance auditing, maintainer gatekeeping against internal-interface exposure — rather than participant preference; alternatives (permissive stacks, dual licensing, AGPL) remain available, so coercion is bounded. Theater (0.35): a growing share of activity is compliance documentation and interpretive statement-making that performs certainty the underlying legal question does not possess. Accessibility collapse is low (0.30): exits and substitutes survive contact with the convention. Resistance is substantial (0.60): advocacy litigation campaigns arrive in waves (BusyBox, VMware/Hellwig, Vizio) superimposed on a secular rise, which the coarse shared grid samples as monotonic growth. The three metric series share one eight-point grid (t=0..35) so every metric is authored at every examined time point. Suppression_requirement is tracked because this story specifically traces enforcement-capacity maturation — informal goodwill norms professionalizing into a compliance industry and litigation-defense posture — not merely extraction drift.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same structure. From the integrator seat the convention is enabling infrastructure — the thing that makes their product category possible — and its costs (compliance staffing, residual litigation risk) read as ordinary overhead. From the contributor seat the same structure is a reciprocity leak: their commons supports closed products whose winners rarely return comparable source. From the end-user seat it is a visibility ceiling: open foundations under closed houses. The steward seat straddles: adoption justifies tolerance, mission resents it. Counsel experiences the convention as fragile rent — valuable exactly as long as courts stay silent. The engine derives these divergences from the declared roles, power, and exit atoms; nothing in the claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Integrators and counsel declare as beneficiaries (derived d near the beneficiary end): the convention subsidizes both — product viability for the former, practice existence for the latter. Contributors and combined-stack end users declare as targets (derived d near the target end): both bear costs the convention's operation generates — unreciprocated appropriation for the former, surrendered stack visibility for the latter — though each carries a secondary beneficiary position (adoption and capability respectively), which tempers but does not invert their directionality. Stewards hold the agenda-setter seat with near-symmetric directionality: they collect adoption and relevance and pay enforcement labor and mission erosion. Advocacy organizations and courts are observers with analytical exit; SaaS operators are excluded with arbitrage-grade exit — the convention touches them least, which is precisely why they need not enter the conversation. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms produce the correct qualitative ordering without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — copyleft/proprietary composition blocking mixed stacks — remains live and is corroborated from outside the beneficiary set, so the convention is not a zombie mandate; mandatrophy resolution turns instead on whether its JUSTIFICATION has migrated from solving composition to defending interpretive turf. Two signals cut differently: enforcement labor has professionalized and grown (rising suppression_requirement series), which is what mandate-persistence looks like; but the convention's core service — making mixed stacks shippable — is still consumed daily by every integrator, which is what live function looks like. The scaffold hypothesis from the generation brief fails the structural test: no declared sunset exists in the convention's instruments (the Classpath and GCC runtime exceptions are perpetual; kernel no-guarantee postures are deterrents, not expiry clauses), and adherents defend the reading as steady-state doctrine. The transitional character is real but local — instantiated in specific postures, not in the convention — and is routed to the scaffold_vs_steady_state_question omega rather than forced into the claim. The classification prevents mislabeling in both directions: reading the convention as pure coordination ignores the measurable reciprocity leak; reading it as pure extraction ignores that the leak is priced into voluntary license choices and bounded by available exits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection,
    'This story instantiates the interface_boundary_reading of the gpl_derivative_work_trigger kernel; which reading actually governs license obligations — broad copyleft (any linking, even dynamic, creates a derivative work), interface-boundary (clean APIs aggregate even under tight coupling), or narrow permissive (only modifications trigger)?',
    'Decisive judicial treatment of inter-program derivation across invocation boundaries, or a uniform steward-enforced interpretive standard adopted across major GPL projects.',
    'Under the broad reading the target set expands to every proprietary integrator and epsilon rises sharply; under the narrow reading targets nearly vanish and the arrangement approaches pure coordination. This story''s classification holds only within the interface reading; the siblings are separate constraints with their own epsilon values.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Committer-frame omega: this constraint is one of three live readings of the derivative-work-trigger kernel; sibling readings are separate files.').

omega_variable(
    scaffold_vs_steady_state_question,
    'Is the interface-boundary convention a transitional holding pattern whose justification expires once courts resolve the derivation question, or steady-state doctrine its adherents defend indefinitely?',
    'Track steward behavior over time: conversion of informal tolerance into permanent formal exceptions and stability guarantees indicates steady state; lapse of no-guarantee postures or expiry-bounded exceptions indicates a transitional arrangement with functional sunset.',
    'A transitional resolution would attach sunset semantics and shift classification toward scaffold; a steady-state resolution supports the tangled_rope claim authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_vs_steady_state_question, conceptual, 'Generation brief hypothesized scaffold; authored claim refines to tangled_rope pending resolution of this question.').

omega_variable(
    contributor_consent_pricing,
    'Do GPL contributors experience integrator appropriation as uncompensated loss, or is it fully priced into their voluntary license choice?',
    'Contributor revealed-preference studies: contribution rates under counterfactual reciprocity mandates; surveys of stated willingness to contribute if integrator source-return were compulsory.',
    'Full pricing pushes effective extraction toward the coordination-cost floor; unpriced network-effect pressure raises it and strengthens the extraction leg of the hybrid classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contributor_consent_pricing, empirical, 'Whether the reciprocity shortfall is consented exchange or unpriced appropriation.').

omega_variable(
    clean_boundary_definability,
    'Does a ''clean API boundary'' remain a workable criterion as toolchains deepen coupling — static linking, header macro expansion, template instantiation, link-time optimization, and generated bindings blur the line between interacting and incorporating?',
    'Comparative compliance jurisprudence: which integrator practices counsel treats as safe across toolchain generations; litigation outcomes on borderline static-linking cases.',
    'If clean boundaries cease to be identifiable, the reading''s protective scope contracts toward the narrow reading and measured extraction rises; if toolchains stabilize around inspectable interfaces, the convention consolidates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clean_boundary_definability, empirical, 'Load-bearing vagueness in the reading''s boundary criterion under modern toolchains.').

omega_variable(
    network_use_escape_hatch,
    'How much of the arrangement''s apparent stability depends on the SaaS escape hatch — network delivery evading every reading''s trigger — rather than on the boundary convention itself?',
    'Counterfactual adoption analysis if AGPL-style network triggers were universalized: would integrators migrate server-side and hollow out the convention''s relevance?',
    'High dependence means the convention coordinates less than it appears and its cost profile shifts toward one-way appropriation with declining reciprocal return; low dependence confirms genuine compositional coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(network_use_escape_hatch, empirical, 'Whether the convention''s coordination function is real or shadowed by distribution-avoidance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__interface_boundary_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_interface_boundary_tr_t0, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(gpl_interface_boundary_tr_t5, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement(gpl_interface_boundary_tr_t10, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(gpl_interface_boundary_tr_t15, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(gpl_interface_boundary_tr_t20, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(gpl_interface_boundary_tr_t25, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 25, 0.31).
narrative_ontology:measurement(gpl_interface_boundary_tr_t30, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 30, 0.33).
narrative_ontology:measurement(gpl_interface_boundary_tr_t35, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 35, 0.35).

% Extraction over time
narrative_ontology:measurement(gpl_interface_boundary_be_t0, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(gpl_interface_boundary_be_t5, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 5, 0.24).
narrative_ontology:measurement(gpl_interface_boundary_be_t10, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(gpl_interface_boundary_be_t15, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 15, 0.31).
narrative_ontology:measurement(gpl_interface_boundary_be_t20, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 20, 0.33).
narrative_ontology:measurement(gpl_interface_boundary_be_t25, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 25, 0.35).
narrative_ontology:measurement(gpl_interface_boundary_be_t30, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 30, 0.37).
narrative_ontology:measurement(gpl_interface_boundary_be_t35, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 35, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(gpl_interface_boundary_su_t0, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(gpl_interface_boundary_su_t5, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 5, 0.23).
narrative_ontology:measurement(gpl_interface_boundary_su_t10, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(gpl_interface_boundary_su_t15, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 15, 0.32).
narrative_ontology:measurement(gpl_interface_boundary_su_t20, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(gpl_interface_boundary_su_t25, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 25, 0.38).
narrative_ontology:measurement(gpl_interface_boundary_su_t30, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 30, 0.4).
narrative_ontology:measurement(gpl_interface_boundary_su_t35, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 35, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__interface_boundary_reading, identity_coordination).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__interface_boundary_reading, gpl_derivative_work_trigger__broad_copyleft_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__interface_boundary_reading, gpl_derivative_work_trigger__narrow_linking_permissive_reading).

% DUAL FORMULATION NOTE:
% One colloquial label — 'does linking trigger GPL source obligations?' — decomposes into three structurally distinct constraints (broad, interface-boundary, and narrow-permissive readings of the gpl_derivative_work_trigger kernel). Each has its own epsilon, beneficiary/victim structure, and classification; this file instantiates the interface-boundary reading only. The broad reading is the doctrinal pole the advocacy community cites as baseline; the narrow reading is the deregulatory pole; the interface reading mediates and is the operative industry convention. Family members link via network.affects_constraints; the upstream broad reading supplies the doctrinal arguments the interface reading's enforcement must answer.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
