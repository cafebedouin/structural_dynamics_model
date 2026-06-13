% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__strong_copyleft_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gpl_copyleft_scope__strong_copyleft_reading
 *   human_readable: GPL Section 2(b) Strong Copyleft Scope — Proprietary Integration Barrier
 *   domain: intellectual_property/software_licensing/open_source_governance
 *
 * SUMMARY:
 *   GPL Section 2(b) is a contested provision. The strong copyleft reading
 *   interprets 'combined or dynamically linked work' expansively to include
 *   plugin architectures, service-oriented interactions, and loader-based
 *   code coupling, requiring any integrator to GPL-license the entire
 *   combined work. This reading benefits free-software communities by
 *   preventing proprietary capture of GPL-licensed components, but it
 *   extracts a structural cost from proprietary vendors by foreclosing them
 *   from integrating GPL code without source-code release. The constraint is
 *   claimed as a snare: proprietary vendors are victims of an asymmetric
 *   extraction enforced through legal threat and compliance pressure, while
 *   free-software communities are the beneficiaries. The strong reading is
 *   NOT the only reading of Section 2(b); it coexists with a narrow-scope
 *   reading (copyleft applies only to direct derivative works) and an
 *   enforcement-vacuum reading (absence of judicial precedent allows
 *   institutional assertion to dominate). This story instantiates the strong
 *   reading only — the other readings are separate constraint stories in the
 *   family, linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - free_software_communities (beneficiary, organized, mobile) — gain structural guarantee of source availability through expanded copyleft scope
 *   - proprietary_software_vendors (payer, institutional, constrained) — structurally excluded from integrating GPL code without full source release
 *   - legal_interpreters_fsf_aligned (agenda_setter, institutional, arbitrage) — author and promulgate the strong reading; enforce through licensing guidance and litigation threat
 *   - legal_interpreters_narrow_reading (payer, institutional, constrained) — argue GPL Section 2(b) applies narrowly; suppressed by absence of binding precedent
 *   - hybrid_licensing_models (payer, powerful, constrained) — dual-licensed vendors whose business model depends on proprietary variants; excluded from integrating GPL code freely
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__strong_copyleft_reading, 0.76).
domain_priors:suppression_score(gpl_copyleft_scope__strong_copyleft_reading, 0.71).
domain_priors:theater_ratio(gpl_copyleft_scope__strong_copyleft_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__strong_copyleft_reading, snare).
narrative_ontology:human_readable(gpl_copyleft_scope__strong_copyleft_reading, "GPL Section 2(b) Strong Copyleft Scope — Proprietary Integration Barrier").
narrative_ontology:topic_domain(gpl_copyleft_scope__strong_copyleft_reading, "intellectual_property/software_licensing/open_source_governance").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__strong_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__strong_copyleft_reading, '105f6c75-db03-462e-a778-3ee1a0d04bbd').
narrative_ontology:cs_kernel_codification('105f6c75-db03-462e-a778-3ee1a0d04bbd', fixed_text).
narrative_ontology:cs_authority_grounding('105f6c75-db03-462e-a778-3ee1a0d04bbd', extraction).
narrative_ontology:cs_interpretation_layer_present('105f6c75-db03-462e-a778-3ee1a0d04bbd').
narrative_ontology:cs_reading_relation('105f6c75-db03-462e-a778-3ee1a0d04bbd', gpl_copyleft_scope__narrow_scope_reading, coexists_with).
narrative_ontology:cs_reading_relation('105f6c75-db03-462e-a778-3ee1a0d04bbd', gpl_copyleft_scope__enforcement_vacuum_reading, coexists_with).
narrative_ontology:cs_axiom('105f6c75-db03-462e-a778-3ee1a0d04bbd', foundational, copyleft_propagation_to_combined_forms).
narrative_ontology:cs_axiom_status(copyleft_propagation_to_combined_forms, holdable).
narrative_ontology:cs_axiom_grounding('105f6c75-db03-462e-a778-3ee1a0d04bbd', copyleft_propagation_to_combined_forms, instrumental).
narrative_ontology:cs_axiom('105f6c75-db03-462e-a778-3ee1a0d04bbd', foundational, derivative_boundary_extends_beyond_copyright_doctrine).
narrative_ontology:cs_axiom_status(derivative_boundary_extends_beyond_copyright_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('105f6c75-db03-462e-a778-3ee1a0d04bbd', derivative_boundary_extends_beyond_copyright_doctrine, deontological).
narrative_ontology:cs_reference_frame('105f6c75-db03-462e-a778-3ee1a0d04bbd', maximalist_copyleft_scope_prevents_proprietary_capture).
narrative_ontology:cs_drift_state('105f6c75-db03-462e-a778-3ee1a0d04bbd', contemporary_microservices_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('105f6c75-db03-462e-a778-3ee1a0d04bbd', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, free_software_communities).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, gpl_licensed_projects).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, hybrid_licensing_models).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, open_source_maintainers).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, hybrid_licensing_vendors).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, legal_interpreters_narrow_scope).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit structurally from GPL Section 2(b)'s scope: any vendor or developer integrating GPL-licensed components must release derived works under GPL, guaranteeing source code availability and preventing proprietary capture of community efforts. The scope extends to dynamic linking, plugin architectures, and combined works, creating a credible enforcement perimeter around the copyleft promise. Communities can contribute code without fear that vendors will capture and proprietary-fork their work.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, free_software_communities, beneficiary,
    organized, generational, mobile, global).

% Structurally excluded from integrating GPL components into proprietary codebases without releasing the entire product under GPL — an unacceptable cost given business model depends on source code secrecy. Can use GPL code only by open-sourcing their own work or avoiding GPL dependencies entirely, both of which reduce technical optionality. Must monitor license compliance for GPL scope ambiguities and operate under legal-uncertainty risk given absence of binding precedent on dynamic linking.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, proprietary_software_vendors, payer,
    institutional, generational, constrained, global).

% Developers offering dual-licensed software (proprietary + GPL) or using GPL code under commercial exceptions face the constraint's full force: Section 2(b)'s broad scope on dynamic linking and combined works means they cannot freely distribute proprietary code that uses GPL libraries, even through plugin or service architectures. Licensing revenue model depends on selling proprietary variants; the constraint narrows the use cases where dual licensing works.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, hybrid_licensing_vendors, payer,
    powerful, biographical, constrained, global).

% Gain defensive power from Section 2(b)'s broad interpretation: incorporating their code into any combined or dynamically linked work triggers GPL propagation obligations on the integrator, preventing proprietary forks or proprietary overlay architectures. The strong reading of 'combined work' and 'dynamic linking' maximizes the scope of this defensive mechanism and reduces proprietary vendors' architectural options.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, gpl_licensed_projects, beneficiary,
    organized, generational, mobile, global).

% Individual and small-team maintainers of GPL projects benefit from the broad scope: they lack the resources to police proprietary uses of their code directly, but Section 2(b)'s interpretation means proprietary vendors cannot easily create closed derivatives or proprietary integrations. The threat of copyleft obligation is the primary enforcement mechanism they can rely on without litigation cost.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, open_source_maintainers, beneficiary,
    moderate, biographical, mobile, global).

% The Free Software Foundation and allied lawyers author and promulgate the strong reading of Section 2(b), interpreting 'combined work' and 'dynamic linking' expansively to include plugin architectures, service-oriented interactions, and loader-based coupling. Enforce the interpretation through public licensing guidance, compliance letters, and litigation threat. Control the institutional narrative around GPL scope; benefit from scope expansion that maximizes leverage against proprietary vendors.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, legal_interpreters_fsf_aligned, agenda_setter,
    institutional, generational, arbitrage, global).

% Copyright scholars, industry counsel, and some judicial voices argue GPL Section 2(b) applies only to direct derivative works following traditional copyright doctrine, not to aggregations, plugin systems, or certain dynamic linking scenarios. Structurally suppressed by absence of controlling judicial precedent: no court has definitively settled the boundary, so the strong reading persists through institutional assertion and threat rather than clear legal authority. Their interpretation is a live alternative but lacks the institutional voice of FSF.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, legal_interpreters_narrow_scope, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__strong_copyleft_reading, legal_interpreters_narrow_scope, excluded).

% Software architectures built on plugin systems (e.g., game modding frameworks, browser extension platforms, IDE plugin ecosystems) face structural threat from the strong copyleft reading: if the base application incorporates GPL code and uses a dynamic loader for plugins, all plugins become subject to copyleft obligation under the 'combined work' interpretation. Largely excluded from free-software participation and would object to the scope but have no seat in GPL governance conversation.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, proprietary_plugin_ecosystems, excluded,
    powerful, biographical, trapped, global).

% Companies evaluating whether to embed GPL libraries (e.g., FFmpeg, OpenSSL, zlib) in their products observe the constraint and assess legal and business risk. Monitor FSF enforcement actions, court decisions, and licensing guidance to calibrate copyleft exposure. Purchasing and architectural decisions shaped by scope of Section 2(b) as interpreted. Risk-aversion often leads to vendor products (commercial variants of GPL libraries) or non-GPL alternatives.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, commercial_embedding_users, observer,
    institutional, generational, analytical, global).

% Courts have not authoritatively resolved the scope of GPL Section 2(b) in binding precedent. The strong reading persists through institutional assertion (FSF guidance, enforcement letters, compliance-tool design) and threat of litigation, but no controlling court decision has validated or narrowed the scope. This ambiguity is the structural condition enabling competing readings to coexist and allowing FSF institutional authority to stand in for legal clarity.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, judicial_system, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_copyleft_scope__strong_copyleft_reading, legal_interpreters_fsf_aligned).
narrative_ontology:fixing_cost_class(gpl_copyleft_scope__strong_copyleft_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures source code availability across a supply chain of software components: when copyleft obligation extends to all combined and dynamically linked works, any vendor integrating GPL code is incentivized to participate in (or at least respect) the free-software development model rather than capture derivatives. The coordination problem is preventing proprietary forking of community infrastructure.
% TRANSFER_FUNCTION: Transfers the structural right to modify, redistribute, and proprietary-fork GPL-licensed code away from proprietary vendors and toward GPL-licensed community projects. Under the strong reading, vendors who integrate GPL components must either accept GPL propagation (releasing their entire product as GPL) or accept the architectural constraint of avoiding GPL dependencies — a de facto transfer of control to free-software maintainers.
% ABSENT_VOICES: Proprietary plugin ecosystem developers, closed-source commercial frameworks that might benefit from GPL components, and vendors pursuing hybrid licensing models all would contest the scope of Section 2(b) if they had a seat in GPL governance — but they are excluded by the nature of the constraint itself. Additionally, copyright scholars advocating for narrow scope and plugin-architecture protection are suppressed by the absence of controlling judicial precedent and the institutional authority of FSF guidance.
% DISAPPEARANCE_RATIONALE: If the strong copyleft scope of Section 2(b) disappeared — if vendors could integrate GPL code into proprietary software without GPL propagation — the free-software ecosystem would restructure immediately: GPL maintainers would lose the primary mechanism preventing proprietary capture, would face dramatic increases in proprietary forks, and the copyleft guarantee would collapse. Proprietary vendors would readily incorporate GPL components, and the incentive structure that sustains free-software commons contribution would degrade substantially.
% FOUNDING_PROBLEM: Early GPL drafting (1989) aimed to prevent proprietary vendors from capturing community-developed software by creating derived proprietary versions — a bait-and-switch against contributor community. Section 2(b) was designed to extend copyleft beyond direct source-code copying to combined and derivative forms, to close loopholes in traditional copyright doctrine that proprietary vendors could exploit.
% FOUNDING_PROBLEM_CORROBORATION: The FSF attests the founding problem is live: proprietary vendors continue to seek ways to integrate GPL components while avoiding copyleft obligations, creating pressure to expand the scope of Section 2(b). Proprietary vendors attest the problem is over-solved: they argue modern modular architectures (plugins, microservices, dynamic linking) were not contemplated in 1989 and should not trigger copyleft obligation. Copyright scholars and industry counsel document that no controlling court decision has validated the broad scope, meaning the constraint persists through institutional assertion and threat rather than established law.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__strong_copyleft_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__strong_copyleft_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__strong_copyleft_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(gpl_copyleft_scope__strong_copyleft_reading, 'none', 1).

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
 *   Extractiveness is high (0.76 at interval end) because the strong copyleft scope structurally forecloses proprietary vendors from one of their primary architectural options: integrating battle-tested GPL libraries into closed-source products. The constraint is extractive relative to the payer seats because it transfers control of copyleft scope interpretation from vendors (who would prefer narrow scope) to free-software institutions (who enforce broad scope). Suppression is substantial (0.71) because vendors comply primarily through legal threat and compliance risk, not through shared belief in copyleft norms — the narrow-reading advocates are suppressed by lack of judicial authority, not by fair-minded debate. Theater is moderate (0.28): the coordination function (preventing proprietary capture) is real and was the founding intent, but increasingly the constraint functions to extract control over software architecture from proprietary vendors. The extractiveness series shows gradual increase (0.62 to 0.76) over the interval, indicating accumulating pressure as software architecture evolves toward more dynamic and modular forms (microservices, plugin systems, containers) that the broad copyleft scope catches more expansively. Suppression also increases (0.58 to 0.71) as FSF and free-software institutions have built more aggressive enforcement and compliance-detection infrastructure (license scanning, GitHub compliance automation). Theater ratio increases slightly (0.18 to 0.28), indicating that as the constraint's extractive function becomes clearer, more institutional energy goes into legitimation theater (copyleft-as-commons-protection narrative) rather than enforcement mechanics.
 *
 * PERSPECTIVAL GAP:
 *   From the free-software community's seat, the strong copyleft scope is a legitimate coordination mechanism that defends collective contribution against proprietary capture. From the proprietary vendor's seat, the same scope is arbitrary exclusion from useful components, enforced through legal threat and institutional power rather than judicial clarity. The agenda-setter seat (FSF-aligned interpreters) benefits from the scope ambiguity: they can issue guidance as if the scope were settled law while vendors bear the compliance risk of the interpretation. The narrow-reading interpreters are suppressed not by logical refutation but by institutional asymmetry: they lack the authority of the Free Software Foundation and depend on legal precedent that does not yet exist. The engine computes these seat divergences from the structural data: beneficiary/victim declarations feed directionality (free-software communities d ≈ 0.1, proprietary vendors d ≈ 0.85), exit options (proprietary vendors: constrained; free-software: mobile) feed effective extraction, and power atoms (proprietary: institutional; free-software: organized) modulate the asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Free-software communities are beneficiaries (d near 0.1): they gain control over their code's use and prevent proprietary capture without bearing the cost of enforcement — FSF institutions bear that cost. Proprietary vendors are victims (d near 0.85): they face legal and compliance cost to integrate GPL code, have constrained exit (they can avoid GPL dependencies, but at substantial architectural cost), and gain no benefit from the constraint. The narrow-reading interpreters are structurally payers (d ≈ 0.6–0.7): their interpretation is suppressed by institutional asymmetry and legal-precedent absence, not because their reading is weaker on intellectual merit. Hybrid-licensing vendors occupy a particularly extractive position (d ≈ 0.8): their business model depends on proprietary variants, and the broad scope directly forecloses them from integrating GPL code. The directionality here tracks the extraction asymmetry: the constraint's persistence requires continued institutional assertion by FSF-aligned actors and legal threat against vendors; if vendors achieved parity of institutional voice and judicial clarity favored narrow scope, the constraint would collapse.
 *
 * MANDATROPHY ANALYSIS:
 *   The strong copyleft scope has a live founding problem (preventing proprietary capture of community code) and a contested current status (is the founding problem still the primary function, or has scope-expansion become an institutional rent-seek?). Proprietaries argue the founding problem was solved decades ago (proprietary vendors now respect copyleft boundaries); free-software advocates argue it is live (proprietary vendors still seek workarounds). The constraint shows signs of institutional drift toward rent-extraction: the theater ratio is rising, indicating more legitimation theater; the suppresssion requirement is increasing, indicating vendors are pushing back harder and FSF is escalating compliance enforcement. The absence of controlling judicial precedent is a structural red flag — the constraint persists through institutional assertion and threat, not through settled law, which means it is vulnerable to precedent-setting litigation. If a court were to adopt the narrow-scope reading, the constraint would collapse overnight; if a court were to validate the strong reading, it would stabilize and shed the theater ratio (litigation risk would drop, compliance would be more transparent). The mandate (prevent proprietary capture) and the mechanism (copyleft scope expansion) are increasingly separable — the mandate could be satisfied with narrower scope and clearer judicial settlement, but institutional interests in FSF-aligned communities depend on the broad scope for maximum leverage.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dynamic_linking_scope_ambiguity,
    'Does ''dynamically linked work'' under GPL Section 2(b) include plugin architectures, service-oriented interactions, and loader-based coupling, or only traditional runtime linking (shared libraries, DLLs)?',
    'Binding judicial precedent. A U.S. court ruling on whether a plugin architecture or microservice-based system that loads GPL code triggers copyleft obligation on the loading container. Current state: no controlling precedent; FSF asserts broad scope; proprietary vendors argue narrow scope; industry counsel operates under litigation risk without clarity.',
    'A narrow-scope ruling would reclassify the constraint from snare to rope (coordination function re-emphasized, extraction diminished); broad-scope judicial validation would stabilize the constraint and reduce theater_ratio as compliance becomes transparent legal obligation rather than institutional threat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dynamic_linking_scope_ambiguity, empirical, 'The boundary of ''dynamically linked work'' that triggers copyleft propagation remains judicially unsettled.').

omega_variable(
    combined_work_derivative_boundary,
    'Is a combined work that merely aggregates GPL and proprietary components (without source-code modification) itself a ''derivative work'' under copyright law, or is aggregation structurally distinct from derivation?',
    'Copyright doctrine analysis and judicial clarification. A binding ruling on whether aggregation alone (without modification) triggers derivative-work status and thus copyleft obligation.',
    'If aggregation alone creates derivative status, extractiveness increases (more systems fall under copyleft scope). If derivative status requires modification, extractiveness decreases and the constraint narrows toward traditional copyright norms. This would distinguish the strong reading from the narrow reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(combined_work_derivative_boundary, empirical, 'Whether mere aggregation of GPL and proprietary code creates a derivative work under copyright law.').

omega_variable(
    institutional_authority_vs_precedent_asymmetry,
    'Does the absence of controlling judicial precedent mean the FSF''s institutional interpretation of Section 2(b) is legally binding, or is it merely one plausible reading competing with narrow-scope interpretations on equal footing?',
    'Judicial settlement or consensus-building among copyright scholars and industry counsel. Current state: FSF asserts its interpretation as normative through public guidance and enforcement letters; proprietary vendors treat the interpretation as one contested reading among others, reducing compliance pressure from legal uncertainty to business-model evaluation.',
    'If FSF interpretation achieves binding status (through precedent or regulatory acceptance), suppression decreases and the constraint stabilizes as transparent copyleft obligation; if narrow-scope interpretation gains parity, suppression persists (vendors continue to operate under legal-uncertainty risk) or extractiveness decreases (constraint scope narrows).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_authority_vs_precedent_asymmetry, conceptual, 'Whether institutional assertion of copyleft scope carries the weight of binding legal authority or remains one contested reading.').

omega_variable(
    modern_architecture_scope_drift,
    'As software architecture evolves (microservices, containers, serverless computing, modular AI systems), does the scope of ''combined work'' under the strong copyleft reading expand to catch more architectural patterns, or do new architectural paradigms create genuine separation that the broad reading never intended to capture?',
    'Technological and legal analysis of emerging architectures. As codebases become more modular and loosely coupled, does the constraint''s extractiveness increase (broad scope catches more systems) or decrease (new architectures create boundaries the strong reading did not contemplate)? Measurement of FSF enforcement activity against new patterns and vendor responses.',
    'Increasing extractiveness would suggest the constraint is drifting toward institutional rent-seek (scope-expansion following technological change). Stable extractiveness would suggest the scope is stable relative to architecture. Decreasing extractiveness would suggest new architectures escape the constraint or force scope recalibration.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(modern_architecture_scope_drift, empirical, 'Whether the strong copyleft scope expands, stabilizes, or recedes relative to evolving software architecture.').

omega_variable(
    suppression_mechanism_institutional_vs_internalized,
    'Is the measured suppression (0.71) a structural property of legal-uncertainty risk (vendors must operate under compliance uncertainty with no clear precedent), or have proprietary developers internalized copyleft norms and adjusted their architectures willingly?',
    'Post-precedent analysis: if a binding judicial ruling establishes the scope clearly (either broad or narrow), does vendor compliance change because the legal uncertainty is resolved, or because they accept the normative claim of copyleft? Empirical tracking: vendor behavior post-ruling, voluntary vs. grudging compliance, architectural innovation around the constraint.',
    'If suppression is structural (legal-uncertainty risk), a binding ruling would reduce suppression regardless of scope. If suppression is internalized (normative acceptance), suppression would persist even post-precedent in narrow-scope outcome. This distinction affects whether the constraint''s extractiveness is genuinely snare-like (coercive) or has shifted toward cultural norm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_institutional_vs_internalized, empirical, 'Whether the measured suppression is structural legal-uncertainty risk or internalized normative acceptance of copyleft principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__strong_copyleft_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(gpl__tr_t0, observed).
narrative_ontology:measurement(gpl__tr_t5, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 5, 0.19).
narrative_ontology:measurement_basis(gpl__tr_t5, observed).
narrative_ontology:measurement(gpl__tr_t10, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(gpl__tr_t10, observed).
narrative_ontology:measurement(gpl__tr_t15, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement_basis(gpl__tr_t15, observed).
narrative_ontology:measurement(gpl__tr_t20, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement_basis(gpl__tr_t20, observed).
narrative_ontology:measurement(gpl__tr_t25, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement_basis(gpl__tr_t25, observed).
narrative_ontology:measurement(gpl__tr_t30, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(gpl__tr_t30, projected).
narrative_ontology:measurement(gpl__tr_t35, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 35, 0.28).
narrative_ontology:measurement_basis(gpl__tr_t35, projected).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(gpl__be_t0, observed).
narrative_ontology:measurement(gpl__be_t5, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 5, 0.65).
narrative_ontology:measurement_basis(gpl__be_t5, observed).
narrative_ontology:measurement(gpl__be_t10, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 10, 0.69).
narrative_ontology:measurement_basis(gpl__be_t10, observed).
narrative_ontology:measurement(gpl__be_t15, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 15, 0.72).
narrative_ontology:measurement_basis(gpl__be_t15, observed).
narrative_ontology:measurement(gpl__be_t20, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 20, 0.74).
narrative_ontology:measurement_basis(gpl__be_t20, observed).
narrative_ontology:measurement(gpl__be_t25, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 25, 0.75).
narrative_ontology:measurement_basis(gpl__be_t25, observed).
narrative_ontology:measurement(gpl__be_t30, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 30, 0.76).
narrative_ontology:measurement_basis(gpl__be_t30, projected).
narrative_ontology:measurement(gpl__be_t35, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 35, 0.76).
narrative_ontology:measurement_basis(gpl__be_t35, projected).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(gpl__su_t0, observed).
narrative_ontology:measurement(gpl__su_t5, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement_basis(gpl__su_t5, observed).
narrative_ontology:measurement(gpl__su_t10, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(gpl__su_t10, observed).
narrative_ontology:measurement(gpl__su_t15, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement_basis(gpl__su_t15, observed).
narrative_ontology:measurement(gpl__su_t20, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(gpl__su_t20, observed).
narrative_ontology:measurement(gpl__su_t25, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(gpl__su_t25, observed).
narrative_ontology:measurement(gpl__su_t30, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(gpl__su_t30, projected).
narrative_ontology:measurement(gpl__su_t35, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(gpl__su_t35, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__strong_copyleft_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gpl_copyleft_scope__strong_copyleft_reading, 0.12).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope__narrow_scope_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope__enforcement_vacuum_reading).

% DUAL FORMULATION NOTE:
% GPL Section 2(b) kernel decomposes into three constraint stories corresponding to three readings: strong_copyleft_reading (this story, high-epsilon snare structure) interprets derivative work broadly and extends copyleft to dynamic linking and combined works; narrow_scope_reading interprets copyleft narrowly to traditional copyright derivative-work doctrine; enforcement_vacuum_reading notes absence of binding precedent allows institutional assertion to dominate and readings to coexist. Each reading instantiates a different epsilon (strong reading: 0.76 extractive; narrow reading: lower extractiveness, less institutional asymmetry; vacuum reading: extractiveness depends on which institutional voice dominates in a given context). The three readings coexist because no controlling court has unified the scope under one interpretation. See commentary.kernel_context and cs_structure.reading_relations for the contested-kernel structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_copyleft_scope__strong_copyleft_reading, institutional, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
