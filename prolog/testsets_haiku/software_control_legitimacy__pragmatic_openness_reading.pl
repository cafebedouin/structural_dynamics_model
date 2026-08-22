% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__pragmatic_openness_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__pragmatic_openness_reading, []).

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
 *   constraint_id: software_control_legitimacy__pragmatic_openness_reading
 *   human_readable: Software Control Pragmatic Openness Model
 *   domain: technology/software engineering/political economy
 *
 * SUMMARY:
 *   This constraint instantiates the pragmatic-openness reading of the
 *   software-control kernel. Under this reading, software control is a
 *   development methodology choice: open source produces genuine quality
 *   benefits through distributed peer review and collaboration, AND
 *   proprietary models are legitimate alternatives for software creators who
 *   choose restricted licensing. The reading does not declare proprietary
 *   software unethical or exploitative; it asserts methodological
 *   coexistence. The founding problem it solves is how to enable large-scale
 *   distributed software collaboration without proprietary gatekeeping, while
 *   leaving the choice of licensing model to developers. No victim set is
 *   authored because both poles of the duality are treated as legitimate; the
 *   beneficiaries are those who gain from open coordination (developers,
 *   users, enterprises adopting open code). Extractiveness is low (0.28)
 *   because the model does not coercively extract from its participants — it
 *   coordinates them through shared code visibility and reciprocal
 *   contribution. Suppression is minimal (0.15) because no enforcement
 *   machinery is needed to keep the model intact; only technical
 *   infrastructure (version control, code hosting, package managers) is
 *   required, and these are largely voluntary.
 *
 * KEY AGENTS:
 *   - open_source_developers: gain visibility, credentials, and collaborative problem-solving; can exit to proprietary work or other projects freely
 *   - software_users: gain code transparency, customization rights, and security vetting through public review; can fork or switch implementations if projects stagnate
 *   - enterprise_adopters: gain cost reduction, customization, and distributed maintenance burden; can modify or replace open components
 *   - software_engineers_choosing_model: set the constraint by deciding whether to use open or proprietary licensing; the choice itself is the coordinative act
 *   - proprietary_software_vendors: coexist under a different but coexisting model; this reading does not position them as exploited or illegitimate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__pragmatic_openness_reading, 0.28).
domain_priors:suppression_score(software_control_legitimacy__pragmatic_openness_reading, 0.15).
domain_priors:theater_ratio(software_control_legitimacy__pragmatic_openness_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__pragmatic_openness_reading, rope).
narrative_ontology:human_readable(software_control_legitimacy__pragmatic_openness_reading, "Software Control Pragmatic Openness Model").
narrative_ontology:topic_domain(software_control_legitimacy__pragmatic_openness_reading, "technology/software engineering/political economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__pragmatic_openness_reading, '37f7e417-ded0-4fec-aa63-5b666fae8d92').
narrative_ontology:cs_kernel_codification('37f7e417-ded0-4fec-aa63-5b666fae8d92', distributed).
narrative_ontology:cs_authority_grounding('37f7e417-ded0-4fec-aa63-5b666fae8d92', practice).
narrative_ontology:cs_interpretation_layer_present('37f7e417-ded0-4fec-aa63-5b666fae8d92').
narrative_ontology:cs_reading_relation('37f7e417-ded0-4fec-aa63-5b666fae8d92', software_control_legitimacy__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('37f7e417-ded0-4fec-aa63-5b666fae8d92', software_control_legitimacy__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('37f7e417-ded0-4fec-aa63-5b666fae8d92', software_control_legitimacy__commons_reading, coexists_with).
narrative_ontology:cs_axiom('37f7e417-ded0-4fec-aa63-5b666fae8d92', foundational, methodology_choice_legitimate).
narrative_ontology:cs_axiom_status(methodology_choice_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('37f7e417-ded0-4fec-aa63-5b666fae8d92', methodology_choice_legitimate, instrumental).
narrative_ontology:cs_axiom('37f7e417-ded0-4fec-aa63-5b666fae8d92', foundational, both_models_produce_quality).
narrative_ontology:cs_axiom_status(both_models_produce_quality, holdable).
narrative_ontology:cs_axiom_grounding('37f7e417-ded0-4fec-aa63-5b666fae8d92', both_models_produce_quality, empirically_contingent).
narrative_ontology:cs_reference_frame('37f7e417-ded0-4fec-aa63-5b666fae8d92', developer_methodology_autonomy).
narrative_ontology:cs_drift_state('37f7e417-ded0-4fec-aa63-5b666fae8d92', contemporary_consolidation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('37f7e417-ded0-4fec-aa63-5b666fae8d92', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, open_source_developers).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, software_users).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, enterprise_adopters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in peer review, collaborative debugging, and reputation-building through open source contribution. They gain visibility, portfolio credentials, and access to a global pool of collaborative problem-solvers. They can choose which projects to join, fork projects if governance fails, or start new ones. The open model coordinates their efforts without requiring proprietary licensing agreements.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, open_source_developers, beneficiary,
    organized, generational, mobile, global).

% Gain access to software whose quality has been vetted through public code review, transparent bug tracking, and community testing. They can inspect the code to understand what it does, modify it for their own needs, and share improvements. They are not locked into vendor decision-making for security updates or feature direction. They can switch to alternative open source implementations if a project stagnates.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, software_users, beneficiary,
    organized, biographical, mobile, global).

% Deploy open source software at scale without per-seat licensing costs, customize it for internal needs, and contribute improvements back to reduce maintenance burden. They benefit from the distributed testing and security review provided by the global user base. They maintain the optionality to fork or replace components if vendor practices become unfavorable.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, enterprise_adopters, beneficiary,
    powerful, biographical, mobile, global).

% Operate under a different but coexisting model where software is distributed under restrictive licenses with per-user fees or seat-based pricing. This reading does not claim proprietary models are illegitimate or harmful; it asserts that both models produce legitimate software and compete on merit. Vendors are observers because this reading does not position them as coordinated parties within the open model, nor as exploited targets.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, proprietary_software_vendors, observer,
    institutional, generational, mobile, global).

% Make the structural choice: whether to distribute their software under open licenses (permitting inspection, modification, redistribution) or proprietary licenses (restricting use, modification, distribution). This reading treats the choice as legitimate for either direction, driven by development goals, revenue models, and community engagement preferences. The choice itself is the coordinative act.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, software_engineers_choosing_model, agenda_setter,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__pragmatic_openness_reading, diffuse).
narrative_ontology:fixing_cost_class(software_control_legitimacy__pragmatic_openness_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of quality assurance and distributed problem-solving by making source code visible to a global community of reviewers, testers, and contributors. Peer review happens in the open; security vulnerabilities are found faster because more eyes inspect the code; feature requests and bug fixes are crowdsourced. This coordination function does not require proprietary licensing; it requires transparent code.
% TRANSFER_FUNCTION: Transfers the cost of quality assurance, security review, and maintenance from individual vendors to a distributed network of contributors and users who benefit directly from the software's functionality. Individual developers gain reputation and portfolio value; enterprises gain cost reductions through shared maintenance; users gain security and transparency. The transfer is reciprocal, not extractive: contributors gain what they invest for.
% ABSENT_VOICES: Proprietary software vendors are not in the conversation under this reading because the reading does not claim they are exploited or illegitimate; it simply asserts that open source is a viable alternative model. Users locked into proprietary systems by switching costs or vendor lock-in are absent in the sense that they cannot easily exit to open alternatives, but this reading does not position them as victims — it frames their constraints as a choice by software engineers to use proprietary models, which is legitimate.
% DISAPPEARANCE_RATIONALE: If the legitimacy of the open source model disappeared (if it were declared invalid or withdrawn), software development would reorganize around proprietary licensing and per-seat fees, reducing the pool of contributors who can legally inspect and modify code. Quality assurance would depend on vendor investment alone rather than distributed peer review. Users would lose transparency and customization rights. The constraint's disappearance would eliminate one pole of a coexisting duality; the world would not remain unchanged — it would compress into a proprietary-only model.
% FOUNDING_PROBLEM: Early software development was dominated by proprietary implementations under tight vendor control, with limited transparency about code quality, security practices, or developer contributions. The founding problem was: how can distributed developers collaborate on large software systems without proprietary licensing overhead, and how can users verify software quality without vendor gatekeeping?
% FOUNDING_PROBLEM_CORROBORATION: Open source projects demonstrate continuous solving of the founding problem: Linux kernel development coordinates thousands of distributed contributors without proprietary licensing; security researchers use open source inspection to find vulnerabilities faster than proprietary vendors; enterprises adopt open source because it solves real coordination and cost problems. Attestation comes from independent sources: academic literature on software quality (open source code reviews catch defects faster), enterprise adoption patterns (Fortune 500 companies run on open source infrastructure), security researchers (who prefer open code to audit), and community growth metrics.
narrative_ontology:disappearance_verdict(software_control_legitimacy__pragmatic_openness_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__pragmatic_openness_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__pragmatic_openness_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_control_legitimacy__pragmatic_openness_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__pragmatic_openness_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__pragmatic_openness_reading_tests).
:- end_tests(software_control_legitimacy__pragmatic_openness_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low and stable over the interval (0.22 → 0.28) because the model produces genuine coordination benefits that outweigh any individual cost. The cost to participating developers is genuine (time contributed to open source projects), but they receive direct benefits (reputation, learning, portfolio value, access to distributed problem-solving). Users receive transparency and customization without paying per-seat fees. Enterprises gain cost reductions. The model does not hide its operation or coerce participation; it attracts participants because they benefit. Suppression is minimal (0.15 at end, rising slightly from 0.08 at start) because the only enforcement needed is technical (access control to repositories, code review gates, license compliance checking). The rising trajectory reflects growing pressure to enforce license compliance as open source adoption scales, and increasing need for governance structures as projects grow large — but this is infrastructure, not coercion. Theater ratio remains very low (0.05 → 0.08) because the model's functional goal (peer review, distributed collaboration) is its primary output; there is little performative activity masking a different function. The measurements grid is aligned: every metric is authored at every time point.
 *
 * PERSPECTIVAL GAP:
 *   The key structural asymmetry is between an open source developer's perspective and a proprietary vendor's perspective. From the developer's seat, open source licensing coordinates large-scale collaboration and produces better software through peer review; the choice to open-source code is liberation from vendor gatekeeping. From the proprietary vendor's seat, restrictive licensing protects investment and enables commercial sustainability; the choice to keep code proprietary is legitimate business strategy. This reading does not adjudicate which perspective is correct — it asserts both are valid. The engine, computing per-seat directionality from power, exit options, and beneficiary/victim status, should produce a low-extraction profile from the open-source-developer seat and a symmetric or slightly beneficial profile from the enterprise-using-open-code seat, reflecting the reading's structure. The reading does NOT produce a high-extraction profile from any seat, because no seat is positioned as coercively extracted from.
 *
 * DIRECTIONALITY LOGIC:
 *   Open source developers sit near the beneficiary end of the directionality spectrum (d near 0.2): they participate voluntarily, they exit easily (switching to other projects or proprietary work), they receive direct benefits (visibility, credentials, problem-solving access). Software users sit slightly further along (d near 0.3): they gain transparency and customization, but they carry some cost (lack of vendor support guarantees, responsibility for their own security patching). Enterprise adopters sit near symmetric (d near 0.4–0.5): they gain cost reduction and customization, but they bear maintenance responsibility. The software engineer making the model choice is the agenda-setter (setting the licensing terms), but under this reading the choice itself is legitimate — it is not an extraction point. Proprietary vendors are observers because the reading does not claim they are targeted or harmed by open source coexistence; they operate in parallel.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (enabling large-scale distributed software collaboration without vendor gatekeeping) is live and actively solved by Linux, Apache, Kubernetes, and millions of other projects. This analysis prevents misclassification as piton or snare: the constraint is not a degraded function persisting through inertia, nor is it a cover story for pure extraction. The constraint solves a real coordination problem that proprietary vendors do not address (transparent peer review at scale). The measurement series showing stable extractiveness and low theater ratio confirms the function is not atrophying and the coordination is not theatrical cover for something else. The constraint is a live rope producing genuine benefits without coercion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quality_causation_vs_openness,
    'Does open source produce better software because the code is open (enabling peer review), or because open source projects have stronger contributor motivation and culture?',
    'Comparative empirical study of software quality metrics (defect density, security vulnerability rates, maintenance responsiveness) controlled for project maturity, contributor base size, and use-case overlap. Natural experiments where projects transition from proprietary to open licensing, or vice versa.',
    'If openness itself drives quality, the reading''s causal claim is strong. If quality results from contributor motivation independent of licensing, the reading is partially supported but the licensing choice becomes less mechanistically important than community engagement. The reading remains valid under either outcome, but the strength of the endorsement for open licensing shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quality_causation_vs_openness, empirical, 'Whether open source quality benefits flow from transparency or from other factors.').

omega_variable(
    legitimacy_vs_pragmatism,
    'Does this reading treat proprietary licensing as legitimately equivalent to open licensing, or as a pragmatic alternative accepted for specific business contexts but less ideal?',
    'Examination of whether the reading''s proponents make symmetrical claims about both models, or whether open source is framed as superior with proprietary licensing tolerated only for market reasons.',
    'If the reading genuinely treats both as equivalent legitimate choices, it is a true coexistence framing. If it treats proprietary as necessary-evil pragmatism while positioning open as the ideal, it shades toward a freedom_imperative framing and may be misclassified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_vs_pragmatism, conceptual, 'The precise normative standing of proprietary models under this reading.').

omega_variable(
    suppression_mechanism_over_time,
    'As open source adoption scales and projects grow large, does the minimal enforcement infrastructure (code review gates, license compliance, access control) remain technical coordination, or does it shift toward institutional suppression of alternative governance models?',
    'Observation of large open source projects (Linux, Apache) over 10+ years: do governance structures become more permissive or more restrictive? Do projects with more bureaucratic code-review processes lose contributors? Do license enforcement actions increase relative to contributor growth?',
    'If suppression mechanisms strengthen significantly, the suppression metric would rise and the constraint might reclassify toward tangled_rope (coordination + enforcement overhead). If suppression remains minimal and technical, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_over_time, empirical, 'Long-term trajectory of enforcement infrastructure as open source projects mature.').

omega_variable(
    kernel_reading_ambiguity,
    'Is the pragmatic_openness_reading structurally distinct from the property_rights_reading, or is it a variation of property_rights applied to open licenses (treating open licenses as a legitimate property choice)?',
    'Examination of whether the reading grounds legitimacy in developer property rights (the right to choose licensing terms, including open source), or in the quality/coordination benefits of openness itself. If grounded in property rights, the reading may shade toward the property_rights sibling; if grounded in methodology and coordination benefits, it is distinctly pragmatic_openness.',
    'If the two readings are structurally identical (both asserting developer property rights), they should be merged or the distinction should be documented as an omega in the property_rights_reading. If they differ in grounding (methodology vs. property rights), they remain distinct readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether pragmatic_openness is a distinct reading or a variant of property_rights_reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__pragmatic_openness_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(soft_tr_t5, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 5, 0.06).
narrative_ontology:measurement(soft_tr_t10, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement(soft_tr_t15, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement(soft_tr_t20, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(soft_tr_t25, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 25, 0.08).
narrative_ontology:measurement(soft_tr_t30, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 30, 0.08).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(soft_be_t5, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 5, 0.24).
narrative_ontology:measurement(soft_be_t10, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 10, 0.26).
narrative_ontology:measurement(soft_be_t15, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 15, 0.27).
narrative_ontology:measurement(soft_be_t20, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(soft_be_t25, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 25, 0.28).
narrative_ontology:measurement(soft_be_t30, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 30, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(soft_su_t5, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 5, 0.1).
narrative_ontology:measurement(soft_su_t10, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 10, 0.12).
narrative_ontology:measurement(soft_su_t15, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 15, 0.13).
narrative_ontology:measurement(soft_su_t20, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 20, 0.14).
narrative_ontology:measurement(soft_su_t25, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 25, 0.15).
narrative_ontology:measurement(soft_su_t30, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 30, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__pragmatic_openness_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_control_legitimacy__pragmatic_openness_reading, 0.12).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__property_rights_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__commons_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the software_control_legitimacy kernel. The kernel is the contested claim about legitimate software control. The pragmatic_openness_reading treats open source and proprietary models as coexisting legitimate alternatives, both producing valid software through different methodologies and governance structures. Sibling readings emphasize different aspects: freedom_imperative_reading emphasizes user freedom as an ethical requirement; property_rights_reading emphasizes creator ownership and commercial sustainability; commons_reading emphasizes negotiated collective management rather than absolute positions. All four readings share the same referent (software development and distribution practices) but author different ε values and different beneficiary/victim structures from different normative premises. Decomposition reasoning: a single 'software control' story would require averaging over incompatible readings, violating ε-invariance; separating into four stories preserves each reading's structural logic.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
