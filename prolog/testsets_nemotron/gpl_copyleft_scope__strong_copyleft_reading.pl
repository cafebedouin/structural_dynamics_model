% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__strong_copyleft_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: GPL Section 2(b) Strong Copyleft Scope — Viral Derivative Work Boundary
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   GPL Section 2(b) requires that any work combined with or dynamically
 *   linked to GPL-licensed code must itself be licensed under the GPL. The
 *   strong_copyleft_reading interprets 'derivative work' to encompass all
 *   forms of code coupling — dynamic linking, plugin architectures, RPC/IPC
 *   boundaries where the FSF argues the combined work forms a single program.
 *   This reading structurally excludes proprietary software vendors from
 *   integrating GPL components without full source release. Free software
 *   communities gain a structural guarantee that code touching GPL components
 *   remains free. Enforcement threats (FSF compliance actions, corporate
 *   legal risk) are credible against dynamic linking patterns. The constraint
 *   operates as a high-extraction snare: proprietary vendors are the primary
 *   victims (blocked from integration or forced to open source), while free
 *   software communities are beneficiaries (guaranteed code availability).
 *   The viral boundary has expanded over time through FSF guidance (GPLv3,
 *   FAQ interpretations) and compliance practice, not through judicial
 *   clarification — the derivative work boundary remains legally untested in
 *   major jurisdictions.
 *
 * KEY AGENTS:
 *   - proprietary_software_vendors: Primary target (powerful/constrained) — structurally excluded from GPL component integration without full source release
 *   - free_software_communities: Primary beneficiary (organized/biographical) — gains structural guarantee of code availability
 *   - commercial_integrators: Secondary victim (moderate/constrained) — blocked from mixed-source products
 *   - mixed_source_product_teams: Secondary victim (moderate/trapped) — cannot combine GPL and proprietary components
 *   - fsf_compliance_lab: Agenda setter (institutional/generational) — administers enforcement, issues guidance expanding scope
 *   - corporate_legal_departments: Observer (institutional/analytical) — assesses risk, drives avoidance strategies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__strong_copyleft_reading, 0.82).
domain_priors:suppression_score(gpl_copyleft_scope__strong_copyleft_reading, 0.78).
domain_priors:theater_ratio(gpl_copyleft_scope__strong_copyleft_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__strong_copyleft_reading, snare).
narrative_ontology:human_readable(gpl_copyleft_scope__strong_copyleft_reading, "GPL Section 2(b) Strong Copyleft Scope — Viral Derivative Work Boundary").
narrative_ontology:topic_domain(gpl_copyleft_scope__strong_copyleft_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__strong_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__strong_copyleft_reading, '863059e9-c82c-4221-9112-bac67b8a739d').
narrative_ontology:cs_kernel_codification('863059e9-c82c-4221-9112-bac67b8a739d', formalized).
narrative_ontology:cs_authority_grounding('863059e9-c82c-4221-9112-bac67b8a739d', lineage).
narrative_ontology:cs_interpretation_layer_present('863059e9-c82c-4221-9112-bac67b8a739d').
narrative_ontology:cs_reading_relation('863059e9-c82c-4221-9112-bac67b8a739d', gpl_copyleft_scope__narrow_scope_reading, coexists_with).
narrative_ontology:cs_reading_relation('863059e9-c82c-4221-9112-bac67b8a739d', gpl_copyleft_scope__enforcement_vacuum_reading, influences).
narrative_ontology:cs_axiom('863059e9-c82c-4221-9112-bac67b8a739d', foundational, derivative_work_includes_dynamic_linking).
narrative_ontology:cs_axiom_status(derivative_work_includes_dynamic_linking, holdable).
narrative_ontology:cs_axiom_grounding('863059e9-c82c-4221-9112-bac67b8a739d', derivative_work_includes_dynamic_linking, deontological).
narrative_ontology:cs_axiom('863059e9-c82c-4221-9112-bac67b8a739d', foundational, software_freedom_requires_viral_copyleft).
narrative_ontology:cs_axiom_status(software_freedom_requires_viral_copyleft, holdable).
narrative_ontology:cs_axiom_grounding('863059e9-c82c-4221-9112-bac67b8a739d', software_freedom_requires_viral_copyleft, deontological).
narrative_ontology:cs_reference_frame('863059e9-c82c-4221-9112-bac67b8a739d', gplv2_textual_copyleft).
narrative_ontology:cs_drift_state('863059e9-c82c-4221-9112-bac67b8a739d', contemporary_cloud_mobile_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('863059e9-c82c-4221-9112-bac67b8a739d', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, free_software_communities).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, copyleft_advocacy_organizations).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, commercial_integrators).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, mixed_source_product_teams).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__strong_copyleft_reading, software_freedom_requires_viral_copyleft).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__strong_copyleft_reading, dynamic_linking_creates_derivative_work).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cannot integrate GPL-licensed components into proprietary products without triggering viral copyleft. The cost of compliance is full source release of the combined work — existential for proprietary business models. Their exit options are: avoid GPL components entirely (costly rewrites), maintain GPL-free alternative stacks, or litigate boundary. They are structurally excluded from the GPL ecosystem unless they surrender proprietary status.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, proprietary_software_vendors, payer,
    powerful, biographical, constrained, global).

% Gain structural guarantee that code touching GPL components remains free and available. The viral boundary prevents proprietary capture of community contributions. They can exit to permissive-licensed ecosystems but the coordination value of the shared GPL commons is high. They do not administer enforcement but benefit from FSF compliance actions.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, free_software_communities, beneficiary,
    organized, generational, mobile, global).

% Build products combining GPL and proprietary components. Blocked by the viral boundary from distributing mixed-source products without full source release. Must either open-source proprietary components (business model loss), rewrite GPL dependencies (engineering cost), or accept legal risk. Less leverage than large proprietary vendors to negotiate or litigate.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, commercial_integrators, payer,
    moderate, biographical, constrained, global).

% Teams within companies that have both GPL and proprietary codebases. Cannot dynamically link or combine without triggering copyleft. Trapped by organizational commitment to both codebases — rewriting is prohibitively expensive, open-sourcing is organizationally forbidden. They bear the compliance cost directly in engineering constraints and legal review overhead.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, mixed_source_product_teams, payer,
    moderate, immediate, trapped, global).

% Administers GPL compliance program, issues guidance (FAQ, GPLv3) that expands the viral boundary, initiates enforcement actions against violations. Collects no direct revenue but derives institutional legitimacy and donation base from enforcement. Can shift interpretation unilaterally through guidance documents. Exit is arbitrary — they define the boundary.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, fsf_compliance_lab, agenda_setter,
    institutional, generational, arbitrage, global).

% Assess GPL compliance risk for their organizations. Drive avoidance strategies (GPL bans, license scanning, clean-room policies). Neither collect nor pay the extraction directly but shape organizational exposure. Their analysis treats the viral boundary as a risk variable, not a coordination benefit.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, corporate_legal_departments, observer,
    institutional, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that free software remains free by making the freedom viral — any code combined with GPL code must itself be free. Solves the collective action problem where individual actors would otherwise privatize improvements to shared code.
% TRANSFER_FUNCTION: Moves the right to distribute combined works from proprietary vendors (who would keep combined works proprietary) to free software communities (who receive source code availability). The transfer is the viral copyleft obligation: combine with GPL code, release your code under GPL.
% ABSENT_VOICES: End users who would benefit from mixed-source products (GPL components in proprietary applications) are not represented in the licensing discourse. Small proprietary vendors without legal resources to assess boundary risk are excluded from the compliance conversation. Jurisdictions without software patent/copyright clarity on dynamic linking have no voice in the FSF's interpretive expansion.
% DISAPPEARANCE_RATIONALE: If the strong copyleft boundary vanished overnight, proprietary vendors would immediately integrate GPL components into proprietary products without source release. The GPL commons would face massive proprietary capture. Free software communities would lose the structural guarantee that their code stays free. The entire GPL ecosystem would reorganize — likely toward permissive licenses or contractual copyleft alternatives.
% FOUNDING_PROBLEM: Preventing proprietary capture of free software: without viral copyleft, companies could take community code, improve it privately, and distribute the improved version as proprietary software — extracting value from the commons without contributing back.
% FOUNDING_PROBLEM_CORROBORATION: The FSF and free software communities attest the problem is live — proprietary capture remains the default without viral copyleft. Proprietary vendors and open source industry groups (OSI, corporate foundations) attest the problem is substantially solved by community norms and that the viral boundary now primarily extracts from legitimate integrators. Independent legal scholars are divided: some (Nimmer, Goldstein) treat viral scope as unsupported by copyright doctrine; others (Moglen, Stallman) treat it as necessary for software freedom. No consensus outside the benefiting parties.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__strong_copyleft_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__strong_copyleft_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__strong_copyleft_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(gpl_copyleft_scope__strong_copyleft_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__strong_copyleft_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.82) is high because the constraint structurally excludes an entire class of actors (proprietary vendors) from using GPL components on their terms — the cost of compliance is full source release of the combined work, which for proprietary vendors is existential. Suppression (0.78) is high because the constraint's persistence depends on active enforcement (compliance actions, legal threats, audit demands) and on suppressing the exit of 'rewrite in proprietary code' through the viral boundary's reach — the FSF actively pursues violations and publishes compliance guides that expand the boundary. Theater ratio (0.15) is low because the enforcement machinery is functional and the coordination function (guaranteed code availability) is real, not performative. Accessibility collapse (0.65) is moderate-high: alternatives exist (permissive licenses, clean-room reimplementation) but are costly. Resistance (0.72) is high: proprietary vendors invest heavily in avoidance (GPL-free stacks, license compliance tooling, legal opinions narrowing scope). The measurement series shows extraction and suppression rising over 34 years as the viral boundary expanded through FSF guidance and compliance practice, while judicial clarification never arrived.
 *
 * PERSPECTIVAL GAP:
 *   From the free_software_communities seat, the constraint is genuine coordination — it solves the collective action problem of keeping code free by making freedom viral. From the proprietary_software_vendors seat, the same structure is pure extraction — it blocks them from an entire ecosystem of components unless they surrender their business model. The engine computes this divergence from the structural data: beneficiaries (free_software_communities) get low directionality (d ~ 0.15), victims (proprietary_software_vendors) get high directionality (d ~ 0.85). The agenda_setter (fsf_compliance_lab) sits near the beneficiary end but with institutional power to enforce.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary vendors are full targets: they bear the extraction (forced source release or exclusion), have constrained exit (rewrite or avoid GPL components), and the constraint's enforcement machinery targets them directly. Free software communities are beneficiaries: they collect the guarantee of code availability without running the enforcement machinery. Commercial integrators and mixed-source teams are secondary victims — they pay compliance costs or face exclusion. FSF compliance lab is the agenda_setter: it administers the constraint, issues guidance that expands scope, and initiates enforcement. Corporate legal departments are observers: they analyze risk but neither collect nor pay directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing proprietary capture of free code) is contested as live vs. dead. The strong_copyleft_reading holds it as live (software_freedom_requires_viral_copyleft axiom). The narrow_scope_reading holds it as dead (traditional copyright doctrine sufficient). The enforcement_vacuum_reading holds it as contested (depends on enforcement capacity). This reading classifies as snare because the viral boundary's primary operational effect is structural exclusion of proprietary vendors — the coordination function for free software communities is real but the extraction from excluded parties is the dominant structural pattern. The constraint would be tangled_rope if the coordination function were the primary operational reality; it is snare because the exclusion is structural and the enforcement is active.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'How does this strong copyleft reading relate structurally to the gpl_copyleft_scope kernel and its sibling readings?',
    'Committer-frame analysis: this reading instantiates one specific constraint from the kernel; sibling readings (narrow_scope_reading, enforcement_vacuum_reading) are separate constraints with their own ε and structural data. The disagreement is located in the derivative_work_boundary axiom — strong_copyleft declares all code coupling as derivative; narrow_scope restricts to traditional copyright derivative works; enforcement_vacuum treats the boundary as practically indeterminate without judicial precedent.',
    'If the kernel framing is rejected — if there is no single persistent commitment that these readings interpret differently — then the three constraints are independent, not sibling readings. This would collapse the reading_relations structure and make the CS drift analysis inapplicable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Whether gpl_copyleft_scope is a genuine kernel with multiple readings or three independent constraint claims').

omega_variable(
    derivative_work_boundary_ambiguity,
    'Does dynamic linking, plugin architecture, or IPC coupling legally constitute a derivative work under copyright law, or is this an interpretive expansion by the FSF?',
    'Definitive judicial precedent in a major jurisdiction (US CAFC, EU CJEU) directly addressing GPL Section 2(b) scope over dynamic linking and plugin boundaries.',
    'If dynamic linking is held not to create derivative works, the strong_copyleft_reading''s extraction claim collapses — the constraint becomes an unenforceable assertion. If held to create derivative works, narrow_scope_reading''s coordination claim collapses — the constraint becomes enforceable viral copyleft. The enforcement_vacuum_reading would be resolved either way.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(derivative_work_boundary_ambiguity, empirical, 'Legal status of the viral boundary — the core empirical uncertainty driving the kernel contest').

omega_variable(
    extraction_vs_coordination_balance,
    'Is the strong copyleft boundary primarily extraction (excluding proprietary vendors from GPL ecosystems) or coordination (ensuring code availability for free software communities)?',
    'Counterfactual observation: if proprietary vendors could integrate GPL components without source release, would free software communities lose substantial code availability? Or would the coordination function (shared codebase) persist via voluntary contribution?',
    'If coordination persists without enforcement, the high ε is extractive overhead. If coordination collapses without enforcement, the high ε is the price of the coordination function — a tangled_rope rather than a pure snare. This reading authors snare based on structural exclusion of proprietary vendors as the dominant operational reality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_balance, preference, 'Whether the constraint''s viral boundary serves coordination or primarily extracts from excluded parties').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__strong_copyleft_reading, 1991, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t1991, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 1991, 0.05).
narrative_ontology:measurement(gpl__tr_t1999, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 1999, 0.08).
narrative_ontology:measurement(gpl__tr_t2007, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 2007, 0.1).
narrative_ontology:measurement(gpl__tr_t2015, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 2015, 0.12).
narrative_ontology:measurement(gpl__tr_t2022, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 2022, 0.14).
narrative_ontology:measurement(gpl__tr_t2025, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(gpl__be_t1991, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 1991, 0.55).
narrative_ontology:measurement(gpl__be_t1999, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 1999, 0.62).
narrative_ontology:measurement(gpl__be_t2007, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 2007, 0.71).
narrative_ontology:measurement(gpl__be_t2015, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 2015, 0.76).
narrative_ontology:measurement(gpl__be_t2022, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 2022, 0.8).
narrative_ontology:measurement(gpl__be_t2025, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 2025, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1991, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 1991, 0.45).
narrative_ontology:measurement(gpl__su_t1999, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 1999, 0.55).
narrative_ontology:measurement(gpl__su_t2007, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 2007, 0.65).
narrative_ontology:measurement(gpl__su_t2015, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 2015, 0.72).
narrative_ontology:measurement(gpl__su_t2022, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 2022, 0.76).
narrative_ontology:measurement(gpl__su_t2025, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__strong_copyleft_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gpl_copyleft_scope__strong_copyleft_reading, 0.12).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope__narrow_scope_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope__enforcement_vacuum_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, gplv3_anti_tivoization).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, lgpl_exception_boundary).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, agpl_network_copyleft).

% DUAL FORMULATION NOTE:
% Part of the gpl_copyleft_scope constraint family (kernel decomposition per ε-invariance principle). This reading instantiates the viral boundary as enforceable derivative work extension. narrow_scope_reading instantiates the restrictive boundary. enforcement_vacuum_reading instantiates the indeterminate boundary. All three share the kernel text but author different ε, different beneficiary/victim structures, and different claimed types. This reading claims snare with high ε; narrow_scope likely claims rope or tangled_rope with lower ε; enforcement_vacuum likely claims piton or scaffold.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_copyleft_scope__strong_copyleft_reading, institutional, 0.15).
constraint_indexing:directionality_override(gpl_copyleft_scope__strong_copyleft_reading, powerful, 0.85).
constraint_indexing:directionality_override(gpl_copyleft_scope__strong_copyleft_reading, moderate, 0.75).
constraint_indexing:directionality_override(gpl_copyleft_scope__strong_copyleft_reading, organized, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
