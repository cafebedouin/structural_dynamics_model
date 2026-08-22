% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__strong_copyleft_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_copyleft_scope__strong_copyleft_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: gpl_copyleft_scope__strong_copyleft_reading
 *   human_readable: GPL Section 2(b) — Strong Copyleft (Expansive Derivative-Work Boundary) Reading
 *   domain: software licensing / intellectual property / open source governance
 *
 * SUMMARY:
 *   GPL Section 2(b)'s text requires that 'any work that you distribute or
 *   publish, that in whole or in part contains or is derived from the
 *   Program' be licensed as a whole under GPL terms. This story authors the
 *   STRONG reading: that the derivative-work boundary extends to any form of
 *   code coupling — dynamic linking, plugin architectures, shared address
 *   space — such that a proprietary component linking against a GPL library
 *   becomes a combined work subject to full disclosure. This reading is
 *   FSF-promulgated and treated as authoritative guidance by many compliance
 *   programs, but it is one of three live readings of the same license text
 *   (see kernel_context) and its ε, beneficiaries, and victims are authored
 *   independently of the narrow-scope and enforcement-vacuum siblings.
 *
 * KEY AGENTS:
 *   - free_software_foundation: agenda-setter, authors and promotes this reading
 *   - proprietary_software_vendors: primary target, structurally excluded from integration without disclosure
 *   - gpl_component_maintainers: primary beneficiary, commons preserved against silent privatization
 *   - commercial_plugin_developers and startups_using_dynamic_linking: powerless/moderate payers with high sunk-cost exposure
 *   - courts_and_legal_scholars: analytical observers, doctrine largely unsettled
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__strong_copyleft_reading, 0.71).
domain_priors:suppression_score(gpl_copyleft_scope__strong_copyleft_reading, 0.68).
domain_priors:theater_ratio(gpl_copyleft_scope__strong_copyleft_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__strong_copyleft_reading, snare).
narrative_ontology:human_readable(gpl_copyleft_scope__strong_copyleft_reading, "GPL Section 2(b) — Strong Copyleft (Expansive Derivative-Work Boundary) Reading").
narrative_ontology:topic_domain(gpl_copyleft_scope__strong_copyleft_reading, "software licensing / intellectual property / open source governance").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__strong_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__strong_copyleft_reading, 'e4115b47-b9e6-4ddb-9e8c-a28bbc78428b').
narrative_ontology:cs_kernel_codification('e4115b47-b9e6-4ddb-9e8c-a28bbc78428b', fixed_text).
narrative_ontology:cs_authority_grounding('e4115b47-b9e6-4ddb-9e8c-a28bbc78428b', lineage).
narrative_ontology:cs_interpretation_layer_present('e4115b47-b9e6-4ddb-9e8c-a28bbc78428b').
narrative_ontology:cs_reading_relation('e4115b47-b9e6-4ddb-9e8c-a28bbc78428b', gpl_copyleft_scope__narrow_scope_reading, coexists_with).
narrative_ontology:cs_reading_relation('e4115b47-b9e6-4ddb-9e8c-a28bbc78428b', gpl_copyleft_scope__enforcement_vacuum_reading, influences).
narrative_ontology:cs_axiom('e4115b47-b9e6-4ddb-9e8c-a28bbc78428b', foundational, functional_coupling_constitutes_derivative_work).
narrative_ontology:cs_axiom_status(functional_coupling_constitutes_derivative_work, holdable).
narrative_ontology:cs_axiom_grounding('e4115b47-b9e6-4ddb-9e8c-a28bbc78428b', functional_coupling_constitutes_derivative_work, conventional).
narrative_ontology:cs_axiom('e4115b47-b9e6-4ddb-9e8c-a28bbc78428b', secondary, software_freedom_transitivity_requires_maximal_boundary).
narrative_ontology:cs_axiom_status(software_freedom_transitivity_requires_maximal_boundary, holdable).
narrative_ontology:cs_axiom_grounding('e4115b47-b9e6-4ddb-9e8c-a28bbc78428b', software_freedom_transitivity_requires_maximal_boundary, instrumental).
narrative_ontology:cs_reference_frame('e4115b47-b9e6-4ddb-9e8c-a28bbc78428b', fsf_stewardship_interpretive_primacy).
narrative_ontology:cs_drift_state('e4115b47-b9e6-4ddb-9e8c-a28bbc78428b', post_busybox_settlement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e4115b47-b9e6-4ddb-9e8c-a28bbc78428b', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, free_software_foundation).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, gpl_component_maintainers).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, copyleft_aligned_developer_communities).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, commercial_plugin_developers).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, startups_using_dynamic_linking).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, embedded_systems_integrators).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__strong_copyleft_reading, software_freedom_transitivity_doctrine).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__strong_copyleft_reading, copyleft_viral_propagation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authored the GPL text and this expansive reading of Section 2(b); issues compliance guidance, funds enforcement litigation and cease-and-desist campaigns, and treats the maximal derivative-work boundary as the correct reading of its own license. Sets the interpretive agenda that downstream compliance programs follow.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, free_software_foundation, agenda_setter,
    institutional, civilizational, analytical, global).

% Release code under GPL expecting that anything dynamically linking against it must also release source. Under this reading their code cannot be quietly absorbed into proprietary products without reciprocal disclosure; they benefit from the structural guarantee that downstream commercial users either comply or avoid their code entirely, preserving the commons.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, gpl_component_maintainers, beneficiary,
    organized, generational, mobile, global).

% Build ecosystems where the expansive reading guarantees any derivative distributed publicly stays open. They gain assurance that competitors cannot fork-and-close their contributions via a loophole reading of linking boundaries.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, copyleft_aligned_developer_communities, beneficiary,
    organized, generational, mobile, global).

% Cannot dynamically link against GPL components without triggering full-source disclosure obligations for the combined work under this reading. Their options are: avoid the GPL component entirely (re-engineering cost), negotiate a commercial dual-license (if the maintainer offers one), or accept forced disclosure of proprietary code — all costly, none of which existed as a live option once the component was already integrated into a product line.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, proprietary_software_vendors, payer,
    powerful, biographical, constrained, global).

% Build plugins or extensions that dynamically link into GPL-licensed host applications for commercial sale. Under this reading their plugin is a combined work requiring GPL licensing, which conflicts with a closed-source commercial model; many are small firms without the legal capacity to litigate the boundary question and simply exit the ecosystem or comply at a loss.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, commercial_plugin_developers, payer,
    moderate, biographical, trapped, global).

% Adopted a GPL library early for speed-to-market without legal review, only to discover under this reading that shipping a proprietary product dynamically linked to it exposes the entire codebase to disclosure demands. Rewriting the dependency late in development is often financially fatal; they have no meaningful exit once the architecture is committed.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, startups_using_dynamic_linking, payer,
    powerless, immediate, trapped, national).

% Ship firmware combining GPL kernel or driver components with proprietary control logic on physical devices. Under the expansive reading, tight coupling in embedded contexts (shared address space, static or dynamic linking against kernel modules) is treated as combined-work territory, forcing disclosure of proprietary firmware or costly architectural isolation (e.g., user-space daemons, IPC boundaries) to preserve separation.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, embedded_systems_integrators, payer,
    moderate, biographical, constrained, global).

% Would prefer a predictable, narrow derivative-work boundary that tracks conventional copyright doctrine so commercial integration is calculable in advance. They are not party to FSF's interpretive guidance process and have no forum to contest the expansive reading except after-the-fact litigation, which most firms avoid.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, industry_standards_bodies, excluded,
    organized, generational, constrained, global).

% Adjudicate the rare cases that reach litigation and analyze the doctrinal soundness of extending the derivative-work concept to dynamic linking. Their rulings, when they occur, could validate or narrow this reading, but the paucity of decided cases leaves the boundary largely unresolved in binding precedent.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, courts_and_legal_scholars, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_copyleft_scope__strong_copyleft_reading, gpl_component_maintainers).
narrative_ontology:fixing_cost_class(gpl_copyleft_scope__strong_copyleft_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Guarantees that anyone who builds on GPL-licensed code and distributes the result must release the combined work's source, preserving a durable commons where contributions cannot be privatized downstream — this solves the real problem of copyleft code being absorbed into closed products without reciprocity.
% TRANSFER_FUNCTION: Moves the option value of proprietary commercialization from vendors, plugin developers, and integrators who build on GPL components to the maintainer community and the broader commons, in the form of forced source disclosure or foreclosed product strategies.
% ABSENT_VOICES: Industry standards bodies and commercial integrators who would prefer a narrow, copyright-doctrine-consistent boundary have no seat in FSF's interpretive guidance process; they only surface once litigation or a cease-and-desist letter forces the question, by which point architectural commitments are sunk.
% DISAPPEARANCE_RATIONALE: If the expansive reading were abandoned overnight in favor of a narrow derivative-work boundary, proprietary vendors and plugin developers would freely dynamically link against GPL components without disclosure obligations, commercial dual-licensing revenue for maintainers would collapse, and the copyleft commons would lose its structural guarantee against silent privatization — both the extraction and the coordination function it rides on would disappear together.
% FOUNDING_PROBLEM: Early free-software authors needed a mechanism to prevent their contributions from being taken proprietary by commercial actors who would improve the code, ship closed binaries, and give nothing back — undermining the incentive to contribute to a shared commons at all.
% FOUNDING_PROBLEM_CORROBORATION: Independent legal scholars studying open-source licensing (e.g., in software law casebooks and law review analyses of GPL enforcement) corroborate that free-riding on shared code without reciprocal disclosure remains a real economic dynamic in commercial software; however, the SAME scholars are split on whether the EXPANSIVE dynamic-linking boundary specifically (as opposed to a narrower derivative-work reading) is necessary to solve that problem, or whether it over-solves it at the cost of legitimate interoperability — corroboration exists for the founding problem, not for this reading's particular scope choice.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__strong_copyleft_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__strong_copyleft_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__strong_copyleft_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_copyleft_scope__strong_copyleft_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__strong_copyleft_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_copyleft_scope__strong_copyleft_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_copyleft_scope__strong_copyleft_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_copyleft_scope__strong_copyleft_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.71 by interval end) because, under this reading, the practical effect on a vendor who has architecturally committed to dynamic linking against a GPL component is total: comply (full disclosure), abandon the product line, or face litigation exposure. Suppression is authored substantial (0.68) because enforcement relies on cease-and-desist campaigns and the in terrorem effect of unresolved doctrine — few vendors litigate to test the boundary, so the expansive reading holds by threat more than by settled precedent. Theater ratio is kept low (0.22) because the coordination function (preventing free-riding on shared code) is real, not merely performative; what varies is the SCOPE of the boundary claimed, not whether a real function exists at all. The suppression_requirement series shows enforcement posture hardening over the interval as FSF and downstream compliance vendors institutionalized the reading into standard legal advice, independent of any single court ruling settling it.
 *
 * PERSPECTIVAL GAP:
 *   From the FSF/maintainer seat, Section 2(b) under this reading is coordination: it is the mechanism that keeps the commons a commons. From the trapped startup or plugin-developer seat, the identical clause is an extraction mechanism that converts an early, uninformed dependency choice into a disclose-or-abandon ultimatum with no negotiated exit. The engine should compute these as structurally different experiences of the same clause because the beneficiary/victim declarations and exit-option data differ sharply by seat, not because the clause's text differs.
 *
 * DIRECTIONALITY LOGIC:
 *   gpl_component_maintainers and copyleft_aligned_developer_communities are structural beneficiaries: the expansive boundary is what makes their contribution model viable against free-riding, so their directionality sits near the beneficiary end. Proprietary_software_vendors, commercial_plugin_developers, startups_using_dynamic_linking, and embedded_systems_integrators are targets: the same clause that protects the commons imposes an all-or-nothing choice on them, and their exit options range from constrained (established firms with negotiating leverage) to trapped (startups with sunk architectural commitments) — the trapped end pushes their effective directionality toward the full-target extreme even though the nominal clause is symmetric in text.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing free-riding on shared contributions) remains live and is corroborated outside the beneficiary set by independent legal scholarship — this blocks a pure mandatrophy verdict for the copyleft mechanism as a whole. But the SPECIFIC scope choice this reading makes (extending to all forms of dynamic coupling, including architectures that arguably resemble mere aggregation) is contested precisely on whether it still serves the founding problem or has drifted into over-broad claim-staking that a narrower doctrine-consistent reading would not make. That drift question is exactly what the enforcement_vacuum_reading and narrow_scope_reading siblings exist to test — this story does not resolve it, per Rule 1.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dynamic_linking_derivative_work_status,
    'Does dynamic linking against a GPL library create a legally cognizable ''combined work'' under copyright''s derivative-work doctrine, or does the FSF''s expansive reading exceed what copyright law itself supports?',
    'A definitive appellate ruling squarely addressing dynamic linking (rather than settled-before-trial cases like the BusyBox litigation) would resolve whether courts adopt the expansive functional-coupling test or a narrower code-copying test.',
    'If courts adopt the expansive reading, this constraint''s classification as snare (from the vendor seat) is validated as legally enforceable extraction rather than merely threatened extraction. If courts adopt the narrow reading, this story''s authored ε overstates the constraint''s true legal force and the enforcement_vacuum_reading better describes present reality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dynamic_linking_derivative_work_status, empirical, 'Whether the expansive derivative-work boundary is legally cognizable or merely FSF-asserted.').

omega_variable(
    kernel_reading_selection_basis,
    'Is the strong_copyleft_reading the correct reading of the kernel to author as ''the'' operative constraint, or does the enforcement_vacuum_reading better describe what actually governs vendor behavior in practice (i.e., is the real constraint the THREAT of the expansive reading rather than its settled legal status)?',
    'Survey of actual vendor compliance decisions: do firms comply because they believe the expansive reading is legally correct, or because litigation risk and reputational cost make compliance cheaper than testing the boundary regardless of the doctrine''s merits?',
    'If vendor behavior is driven by risk-aversion under genuine doctrinal uncertainty rather than belief in the expansive reading''s correctness, the enforcement_vacuum_reading may be the more accurate description of the operative constraint, with this story describing the reading that FSF promotes rather than the reading that actually binds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, conceptual, 'Whether this reading describes actual operative constraint or promoted doctrine.').

omega_variable(
    commons_preservation_vs_overreach,
    'Does the expansive coupling boundary preserve the commons against genuine free-riding, or does it also capture architectures (loose plugin interfaces, IPC-separated processes) that pose no real free-riding risk and where the disclosure demand is pure overreach?',
    'Case-by-case technical analysis of specific coupling architectures against the economic logic of the founding problem (does the specific coupling pattern actually enable free-riding on the maintainer''s investment, or is it functionally equivalent to mere aggregation with a technical linking step).',
    'If a substantial share of captured architectures pose no genuine free-riding risk, the beneficiary/victim structure includes false-positive victims — the coordination function does not actually require the full scope this reading claims, supporting the narrow_scope_reading as the better-fitted boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_preservation_vs_overreach, conceptual, 'Whether the expansive boundary''s scope matches the founding problem''s actual reach.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__strong_copyleft_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gpl__tr_t5, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(gpl__tr_t10, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(gpl__tr_t15, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(gpl__tr_t20, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(gpl__tr_t25, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 25, 0.22).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(gpl__be_t5, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(gpl__be_t10, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(gpl__be_t15, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(gpl__be_t20, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(gpl__be_t25, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 25, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(gpl__su_t5, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(gpl__su_t10, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(gpl__su_t15, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(gpl__su_t20, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(gpl__su_t25, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 25, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__strong_copyleft_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, narrow_scope_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, enforcement_vacuum_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the gpl_copyleft_scope kernel (GPL Section 2(b)'s derivative-work boundary). strong_copyleft_reading authors high ε (0.71) reflecting the expansive coupling boundary as structurally exclusionary of proprietary integration. narrow_scope_reading authors the same license text with a copyright-doctrine-consistent narrow boundary, producing much lower ε and a different (or absent) victim set. enforcement_vacuum_reading treats the operative constraint as contingent on which interpretive community holds enforcement capacity, producing a distinct, context-dependent ε. All three share the same kernel text but diverge in beneficiary/victim structure, classification, and ε — per the ε-invariance principle, they are authored as separate files linked here rather than as one story with a hedged value.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
