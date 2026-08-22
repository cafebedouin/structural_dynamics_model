% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__strong_copyleft_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: gpl_copyleft_scope__strong_copyleft_reading
 *   human_readable: GPL Section 2(b) Strong Copyleft Derivative Work Scope
 *   domain: intellectual_property/software_licensing
 *
 * SUMMARY:
 *   GPL Section 2(b) and its interpretation under the strong copyleft reading
 *   establish that any work 'combined' or 'dynamically linked' with GPL code
 *   must itself be released under GPL or compatible terms. This reading
 *   extends the derivative work boundary far beyond traditional copyright
 *   doctrine—capturing plugin architectures, dynamic linking, and runtime
 *   coupling as triggering copyleft obligation. The strong reading benefits
 *   free-software communities (ensuring code remains in commons) and
 *   enforcing entities (FSF, Conservancy) by structurally precluding
 *   proprietary enclosure. It harms proprietary vendors and commercial
 *   integrators (constrained to open-source or avoidance). The claim/metric
 *   divergence is deliberately authored: the constraint is CLAIMED as snare
 *   (the strong reading instantiates extraction from proprietary vendors)
 *   while the metrics describe an extractive, actively enforced mechanism
 *   with rising enforcement infrastructure and scope expansion over three
 *   decades. This divergence is the signal the engine measures; do not
 *   reconcile the claim to the metrics.
 *
 * KEY AGENTS:
 *   - free_software_communities: primary beneficiary (structural guarantee of code commons); organized power, generational horizon, arbitrage exit (chose GPL voluntarily)
 *   - copyleft_enforcing_entities (FSF, Conservancy): agenda-setter (controls interpretation, enforces via litigation); institutional power, generational horizon, arbitrage exit (hold copyright, control enforcement)
 *   - proprietary_vendors (Microsoft, Apple, commercial database vendors): primary victim (structurally excluded from GPL integration without source release); powerful locally but constrained by GPL scope; biographical horizon, constrained exit (duplication costs, GPL library value)
 *   - commercial_integrators (mid-market software, IoT, SaaS): secondary victim (high compliance costs, rearchitecture pressure); moderate power, biographical horizon, constrained exit (GPL component value vs. compliance burden)
 *   - judicial_system: observer seat (interprets GPL, occasionally narrows scope); institutional power, analytical exit (produces binding verdicts but does not choose constraint existence)
 *   - academic_researchers: excluded (not in GPL governance, trapped by unknowing incorporation); moderate power, biographical horizon, trapped exit (disclosure late in development)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__strong_copyleft_reading, 0.78).
domain_priors:suppression_score(gpl_copyleft_scope__strong_copyleft_reading, 0.71).
domain_priors:theater_ratio(gpl_copyleft_scope__strong_copyleft_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__strong_copyleft_reading, snare).
narrative_ontology:human_readable(gpl_copyleft_scope__strong_copyleft_reading, "GPL Section 2(b) Strong Copyleft Derivative Work Scope").
narrative_ontology:topic_domain(gpl_copyleft_scope__strong_copyleft_reading, "intellectual_property/software_licensing").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__strong_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__strong_copyleft_reading, 'aa0222f4-39f2-4e19-913c-3708760ab187').
narrative_ontology:cs_kernel_codification('aa0222f4-39f2-4e19-913c-3708760ab187', fixed_text).
narrative_ontology:cs_authority_grounding('aa0222f4-39f2-4e19-913c-3708760ab187', extraction).
narrative_ontology:cs_interpretation_layer_present('aa0222f4-39f2-4e19-913c-3708760ab187').
narrative_ontology:cs_reading_relation('aa0222f4-39f2-4e19-913c-3708760ab187', gpl_copyleft_scope__narrow_scope_reading, coexists_with).
narrative_ontology:cs_reading_relation('aa0222f4-39f2-4e19-913c-3708760ab187', gpl_copyleft_scope__enforcement_vacuum_reading, coexists_with).
narrative_ontology:cs_axiom('aa0222f4-39f2-4e19-913c-3708760ab187', foundational, derivative_work_includes_dynamic_linking).
narrative_ontology:cs_axiom_status(derivative_work_includes_dynamic_linking, holdable).
narrative_ontology:cs_axiom_grounding('aa0222f4-39f2-4e19-913c-3708760ab187', derivative_work_includes_dynamic_linking, conventional).
narrative_ontology:cs_axiom('aa0222f4-39f2-4e19-913c-3708760ab187', foundational, copyleft_reciprocity_binding_on_all_coupling).
narrative_ontology:cs_axiom_status(copyleft_reciprocity_binding_on_all_coupling, holdable).
narrative_ontology:cs_axiom_grounding('aa0222f4-39f2-4e19-913c-3708760ab187', copyleft_reciprocity_binding_on_all_coupling, deontological).
narrative_ontology:cs_reference_frame('aa0222f4-39f2-4e19-913c-3708760ab187', broad_derivative_work_copyleft).
narrative_ontology:cs_drift_state('aa0222f4-39f2-4e19-913c-3708760ab187', contemporary_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aa0222f4-39f2-4e19-913c-3708760ab187', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, free_software_communities).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, copyleft_enforcing_entities).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, proprietary_vendors).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, commercial_integrators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, dual_licensing_vendors).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, software_license_compliance_industry).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, dual_licensing_vendors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% GPL communities (Linux kernel, GNU toolchain, Apache Foundation members) benefit from the strong copyleft reading because it structurally guarantees that any work incorporating their code must release source. They gain visibility into derivative work, prevent proprietary enclosure of their contributions, and maintain a commons of GPL-licensed code. Their exit is voluntary: they choose GPL precisely for this copying enforcement; they can relicense to permissive licenses but this would betray the community's foundational commitment.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, free_software_communities, beneficiary,
    organized, generational, arbitrage, global).

% The Free Software Foundation, Software Freedom Conservancy, and GPL enforcement campaigns set the interpretation and enforce it through litigation and cease-and-desist letters. They adjudicate what counts as derivative work, threaten proprietary vendors who integrate GPL code into closed products, and maintain the GPL's authority as a binding contract. Their power derives from controlling the interpretive tradition and holding standing to litigate copyright infringement on behalf of GPL projects.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, copyleft_enforcing_entities, agenda_setter,
    institutional, generational, arbitrage, global).

% Commercial software vendors (Microsoft, Apple, proprietary database vendors) that want to incorporate GPL-licensed libraries face a structural choice: release their entire product source under GPL (destroying their proprietary business model), or avoid the GPL component entirely. Their options are constrained because the strong copyleft reading treats many forms of integration (dynamic linking, plugin architectures, runtime coupling) as triggering copyleft obligation. The enforcement threat is credible and carries legal/financial risk; choosing avoidance means losing the functionality the GPL component provides or duplicating its work at high cost.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, proprietary_vendors, payer,
    powerful, biographical, constrained, global).

% Mid-market and independent software vendors that build on GPL-licensed infrastructure (embedded systems, SaaS platforms, IoT firmware) bear high compliance costs under the strong reading. They must either open-source their entire application stack or negotiate dual-licensing arrangements (often at prohibitive cost) or rearchitect to avoid GPL components. Their constrained exit derives from GPL component integration being economically attractive but legally risky under the strong reading.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, commercial_integrators, payer,
    moderate, biographical, constrained, global).

% Companies that hold copyright on GPL software and offer it under both GPL and proprietary commercial licenses (MySQL, Qt, MongoDB) occupy a middle position. They benefit from the strong copyleft reading because it pressures users toward purchasing commercial licenses to avoid open-source obligations. They also pay because their own products must comply with GPL if they distribute GPL-licensed dependencies under the GPL path. Their exit is relatively mobile: they control the copyright and can adjust licensing terms, renegotiate with acquisitors, or shift business models.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, dual_licensing_vendors, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__strong_copyleft_reading, dual_licensing_vendors, payer).

% National courts (particularly U.S. federal courts, EU courts) interpret the GPL and copyright law, deciding whether specific integration patterns trigger copyleft obligations. Judicial interpretation constrains the FSF's authority: courts have occasionally narrowed the derivative work scope (e.g., GCC v. GPLV3 disputes, Java linking controversies). Their power derives from the authority to issue binding precedent; their exit is analytical—they produce verdicts but cannot choose the constraint's existence.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, judicial_system, observer,
    institutional, generational, analytical, national).

% Researchers building new algorithms or systems that might integrate GPL code face uncertainty: the strong copyleft reading could force their work into open-source, disrupting funding models and institutional expectations. They are largely excluded from the GPL governance conversation; academic licensing conventions (BSD, MIT, Apache) are their default to avoid copyleft entanglement. The strong reading traps them: if they unknowingly incorporate GPL code, they discover the obligation late in development.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, academic_researchers, excluded,
    moderate, biographical, trapped, global).

% Companies providing license compliance scanning, SBOM generation, and open-source risk management benefit from the strong copyleft reading because it creates demand for compliance tools. The stronger and broader the GPL scope, the more commercial value accrues to vendors who help proprietary software companies navigate the risk. Their exit is mobile: if copyleft scope narrows, their market shrinks and they pivot or diversify.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, software_license_compliance_industry, beneficiary,
    moderate, biographical, mobile, global).

% Organizations like IETF, W3C, and OASIS observe GPL licensing because it affects which implementations can be incorporated into standards reference code and which can implement standards specifications. The strong copyleft reading influences interoperability outcomes: if standards require GPL-compatible implementations, proprietary vendors face barriers to participation. Their analytical position means they document impacts but do not directly control the GPL scope.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, open_standards_bodies, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_copyleft_scope__strong_copyleft_reading, copyleft_enforcing_entities).
narrative_ontology:fixing_cost_class(gpl_copyleft_scope__strong_copyleft_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a reciprocal commitment: if you build on GPL code, your modifications and derivative works must be released under compatible terms. This solves a coordination problem for contributors: without copyleft, each contributor fears their work will be enclosed in proprietary systems, making contributions inefficient. Copyleft makes the reciprocal obligation credible and binding, enabling large-scale collaborative development without enclosure risk.
% TRANSFER_FUNCTION: Transfers the legal obligation to release source code from the GPL project (who authored the original) to anyone who combines or dynamically links GPL code into their own work. The strong reading makes this transfer scope broad: it captures not only direct modifications but also dynamic linking, plugin integration, and certain aggregation patterns. Proprietary vendors must either release their source (transfer of intellectual property control) or avoid the GPL component entirely (transfer of opportunity cost).
% ABSENT_VOICES: Proprietary vendors are largely silenced in GPL governance: FSF and free-software communities set interpretation, academic researchers are absent from licensing discussions, and developing-world developers with limited compliance resources are excluded. Judicial actors (courts) are the main counterweight, but they speak only when litigation materializes. Technology policy makers and standards bodies observe but do not control the scope.
% DISAPPEARANCE_RATIONALE: If GPL Section 2(b) and its strong copyleft reading disappeared overnight, proprietary software vendors would immediately integrate GPL libraries into closed systems without source release, GPL communities would lose the structural guarantee that enables their governance model, and the incentive to contribute to GPL projects would collapse (contributions would be enclosed rather than retained in commons). The software licensing landscape would shift dramatically toward permissive licenses and proprietary dominance; GPL's power would evaporate.
% FOUNDING_PROBLEM: Early free software faced enclosure risk: contributors to GNU and Linux systems faced the possibility that their work would be integrated into proprietary systems without credit, modification visibility, or commons preservation. GPL Section 2(b) was designed to make enclosure impossible: any derivative work must carry copyleft forward, making the commons irreversible.
% FOUNDING_PROBLEM_CORROBORATION: FSF and copyleft communities attest the problem is live and growing: proprietary integration threats continue, GPLv3 evolution (2007) addressed new enclosure vectors (firmware tivoization, cloud computing). Independent analysis from GPL enforcement surveys (Conservancy, Software Law) documents continuing integration attempts and enforcement necessity. However, judicial verdicts and licensing scholars argue the enclosure risk has been substantially mitigated by alternative legal instruments (contract, trademark) and open-source success; this corroboration comes from outside the GPL-benefiting communities and challenges the problem's present severity.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__strong_copyleft_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__strong_copyleft_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__strong_copyleft_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_copyleft_scope__strong_copyleft_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__strong_copyleft_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.78 at interval end) because the constraint imposes a binary choice on proprietary vendors: open-source the entire product stack or avoid the GPL component. There is no middle ground under the strong reading; the choice is enforced by copyright law and litigation threat. Suppression is elevated (0.71) because the enforcement mechanism—FSF/Conservancy cease-and-desist letters, litigation, trademark claims—actively maintains the scope boundary. The measurement series show monotonic increase from 1991 (GPL adoption) through 2026: extractiveness rises as GPL adoption widens (Linux dominance, critical infrastructure dependence) and scope expansions (GPLv3 addressing firmware, cloud computing) extend copyleft reach. Suppression rises as enforcement infrastructure matures (SFLC founding 2005, enforcement campaigns 2000s–2010s, commercial risk escalates). Theater ratio is moderate (0.28) and growing slowly: the security/commons-preservation narrative is genuine, but as GPL scope expands into dynamic linking and plugin architectures, a growing share of enforcement effort defends scope boundaries rather than the original problem (enclosure of modification history). The grid shows organizational-level suppression surpassing structural-level resistance by 2026, indicating that vendor compliance machinery now manages the constraint more than vendor resistance does—organizational level faces highest stakes inflation and highest suppression, reflecting where the negotiation and compliance burden concentrates.
 *
 * PERSPECTIVAL GAP:
 *   The free-software-communities and copyleft-enforcing-entities seats should compute the strong copyleft reading as rope or tangled-rope (coordination with legitimate enforcement). The proprietary-vendors and commercial-integrators seats should compute it as snare (pure extraction enforced by copyright + threat). The judicial-observer seat should compute it as contested or ambiguous, because courts have occasionally narrowed the derivative-work scope (suggesting the constraint's type is not settled). This multi-seat divergence is the constraint's structural signature: what appears as legitimate reciprocal coordination from one seat is extraction machinery from another. The engine's per-seat classification captures this divergence; the commentary explains why it exists.
 *
 * DIRECTIONALITY LOGIC:
 *   The strong copyleft reading generates asymmetric directionality across stakeholder seats. Free-software communities have directionality near 0.0 (full beneficiary): they gain structural guarantee of code availability without bearing enforcement costs (the FSF bears litigation risk, vendors bear compliance cost). Copyleft enforcing entities sit near 0.15–0.25 (slight beneficiary): they bear litigation and reputation risk but gain authority, standing, and legitimacy from the interpretation. Proprietary vendors have directionality near 0.85–0.95 (near-target): they bear the compliance burden, must rearchitect, or lose GPL components; the constraint extracts from their freedom to build closed products. Commercial integrators sit near 0.80 (strong target): they face the binary choice without the vendor's negotiating power. Dual-licensing vendors sit near 0.55 (symmetric): they collect license fees from the constraint but must also comply if they distribute GPL-dependent code; the constraint is simultaneously their business model and their operational burden. The engine should compute these divergences from the beneficiary/victim declarations and power atoms; the commentary explains the structural asymmetry that generates them.
 *
 * MANDATROPHY ANALYSIS:
 *   The strong copyleft reading's founding problem (preventing enclosure of GPL contributions) is descriptively LIVE: proprietary vendors continue to seek integration paths, GPLv3 (2007) evolved to address new enclosure vectors (firmware, cloud), and enforcement campaigns document ongoing integration attempts. However, the founding problem's structural necessity has been substantially questioned: alternative legal instruments (trademark, contract, open-source market success) may now provide sufficient enclosure prevention; the GPL scope may have expanded beyond the original problem into a general-purpose extraction mechanism for vendors seeking to avoid contributor-reciprocity norms. The mandatrophy question is contestable: has the constraint's scope expansion moved past solving the founding problem into rent-seeking by GPL communities, or is the expanded scope legitimate prevention of evolved enclosure vectors? The strong reading instantiates snare classification because it imposes a binary structural choice (open or avoid) with no middle ground, enforcement is active and credible, and beneficiaries (GPL communities, FSF) collect the constraint's operation without bearing its compliance costs. The constraint persists because enforcement entities hold credible legal standing (copyright ownership by project contributors, litigation capability) and the founding problem remains contestable—enough ambiguity for the snare to persist without requiring proof that the problem is false.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    derivative_work_boundary_contention,
    'What constitutes a ''derivative work'' or ''combined work'' under GPL Section 2(b)? Does dynamic linking, plugin integration, or runtime IPC coupling trigger copyleft obligation, or only direct modification?',
    'Judicial precedent from U.S. federal courts (particularly cases involving GPL enforcement: VMware, Artifex, others) and international courts (EU copyright directive interpretation). Precedent establishing bright-line tests for derivative-work determination would resolve the ambiguity.',
    'If courts adopt narrow-scope reading (traditional derivative-work doctrine): proprietary vendors gain significant integration freedom, GPL scope collapses substantially, extractiveness drops to 0.35–0.45, snare reclassifies toward rope or contested. If courts adopt strong-reading: extractiveness remains elevated (0.75+), snare holds, FSF authority is institutionalized.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(derivative_work_boundary_contention, empirical, 'Judicial interpretation of GPL derivative-work boundary under copyright law.').

omega_variable(
    enclosure_risk_persistence,
    'Is the founding problem (proprietary enclosure of GPL contributions) still structurally live, or have alternative legal instruments (trademark, contract, market mechanisms) sufficiently mitigated the risk?',
    'Empirical survey of proprietary integration attempts, re-analysis of enforcement litigation scope and frequency (comparing 1995–2005 vs. 2015–2025), and comparison of GPL-preserved commons vs. permissive-licensed-but-enclosed projects (e.g., Apache license projects acquired and closed).',
    'If enclosure risk is substantially mitigated: the founding problem is dead, mandatrophy threshold is crossed, snare reclassifies as piton (theatrical enforcement of a solved problem). If enclosure risk persists or evolved: snare classification holds, strong copyleft reading remains functionally live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enclosure_risk_persistence, empirical, 'Whether the founding problem (preventing proprietary enclosure) remains structurally unsolved.').

omega_variable(
    scope_expansion_vs_problem_evolution,
    'Do GPL scope expansions (GPLv3 addressing firmware, cloud computing, dynamic linking over networks) reflect legitimate evolution to address evolved enclosure vectors, or scope creep driven by free-software advocacy communities seeking to maximize adoption of open-source models?',
    'Historical analysis of GPL scope changes (v1→v2→v3) and the stated justification for each; comparison of scope expansions to documented enclosure threats contemporaneous to each version; independent analysis from licensing scholars outside free-software advocacy (e.g., law-and-economics research, empirical studies of licensing trends).',
    'If expansions track evolved threats: strong reading''s scope is justified, extractiveness reflects legitimate boundary maintenance, snare classification holds with lower theater_ratio. If expansions exceed threat evolution: strong reading is extractive scope-seeking, theater_ratio should be higher (0.40+), snare edges toward piton.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scope_expansion_vs_problem_evolution, conceptual, 'Whether GPL scope expansion reflects problem evolution or advocacy mission creep.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.71) structurally enforced (credible litigation threat, copyright enforcement machinery) or partially internalized (proprietary vendors have accepted copyleft norms, self-suppressing their integration desires)?',
    'Post-suppression scenario: if GPL enforcement enforcement capacity were removed (hypothetically), would proprietary vendors continue to avoid GPL integration due to internalized norms, or would integration attempts spike immediately?',
    'If suppression is mostly structural: effective suppression is approximately 0.71, snare classification is stable. If suppression is substantially internalized: vendors carry the constraint with them beyond enforcement reach, effective suppression is higher (0.80+), constraint is more durable than structural metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of proprietary GPL integration is structural or internalized.').

omega_variable(
    reading_identity_kernel_contention,
    'Is GPL Section 2(b) a KERNEL (a persisting commitment read differently by different parties), or is the strong reading THE correct interpretation and narrow/vacuum readings are simply false?',
    'Documentary evidence from GPL history: did FSF''s original 1991 Section 2(b) intend broad scope (strong reading vindicated from inception), or did scope expand gradually (kernel present throughout, readings diverged over time)? Comparison to legal doctrine of originalism vs. living-document interpretation.',
    'If strong reading is original and correct: this is not a kernel story—the story is a straightforward snare, and sibling readings are simply errors or bad-faith positions. If scope ambiguity existed in 1991 and grew over time: the kernel is genuine, strong reading is one valid instantiation, and story classification as snare reflects one legitimate reading (not THE truth).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_identity_kernel_contention, conceptual, 'Whether GPL 2(b) is intrinsically ambiguous (kernel) or the strong reading is the definitive correct interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__strong_copyleft_reading, 1991, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t1991, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 1991, 0.05).
narrative_ontology:measurement(gpl__tr_t2001, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 2001, 0.12).
narrative_ontology:measurement(gpl__tr_t2007, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 2007, 0.18).
narrative_ontology:measurement(gpl__tr_t2012, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 2012, 0.22).
narrative_ontology:measurement(gpl__tr_t2018, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 2018, 0.26).
narrative_ontology:measurement(gpl__tr_t2026, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(gpl__be_t1991, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 1991, 0.35).
narrative_ontology:measurement(gpl__be_t2001, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 2001, 0.52).
narrative_ontology:measurement(gpl__be_t2007, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 2007, 0.61).
narrative_ontology:measurement(gpl__be_t2012, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 2012, 0.68).
narrative_ontology:measurement(gpl__be_t2018, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 2018, 0.74).
narrative_ontology:measurement(gpl__be_t2026, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 2026, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1991, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 1991, 0.25).
narrative_ontology:measurement(gpl__su_t2001, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 2001, 0.48).
narrative_ontology:measurement(gpl__su_t2007, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 2007, 0.58).
narrative_ontology:measurement(gpl__su_t2012, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 2012, 0.65).
narrative_ontology:measurement(gpl__su_t2018, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 2018, 0.68).
narrative_ontology:measurement(gpl__su_t2026, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 2026, 0.71).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1991, tn=2026
narrative_ontology:measurement(gpl__grid_01, gpl_copyleft_scope__strong_copyleft_reading, accessibility_collapse(class), 1991, 0.2).
narrative_ontology:measurement(gpl__grid_02, gpl_copyleft_scope__strong_copyleft_reading, accessibility_collapse(class), 2026, 0.58).
narrative_ontology:measurement(gpl__grid_03, gpl_copyleft_scope__strong_copyleft_reading, accessibility_collapse(individual), 1991, 0.15).
narrative_ontology:measurement(gpl__grid_04, gpl_copyleft_scope__strong_copyleft_reading, accessibility_collapse(individual), 2026, 0.48).
narrative_ontology:measurement(gpl__grid_05, gpl_copyleft_scope__strong_copyleft_reading, accessibility_collapse(organizational), 1991, 0.25).
narrative_ontology:measurement(gpl__grid_06, gpl_copyleft_scope__strong_copyleft_reading, accessibility_collapse(organizational), 2026, 0.68).
narrative_ontology:measurement(gpl__grid_07, gpl_copyleft_scope__strong_copyleft_reading, accessibility_collapse(structural), 1991, 0.3).
narrative_ontology:measurement(gpl__grid_08, gpl_copyleft_scope__strong_copyleft_reading, accessibility_collapse(structural), 2026, 0.62).
narrative_ontology:measurement(gpl__grid_09, gpl_copyleft_scope__strong_copyleft_reading, resistance(class), 1991, 0.65).
narrative_ontology:measurement(gpl__grid_10, gpl_copyleft_scope__strong_copyleft_reading, resistance(class), 2026, 0.58).
narrative_ontology:measurement(gpl__grid_11, gpl_copyleft_scope__strong_copyleft_reading, resistance(individual), 1991, 0.55).
narrative_ontology:measurement(gpl__grid_12, gpl_copyleft_scope__strong_copyleft_reading, resistance(individual), 2026, 0.48).
narrative_ontology:measurement(gpl__grid_13, gpl_copyleft_scope__strong_copyleft_reading, resistance(organizational), 1991, 0.8).
narrative_ontology:measurement(gpl__grid_14, gpl_copyleft_scope__strong_copyleft_reading, resistance(organizational), 2026, 0.68).
narrative_ontology:measurement(gpl__grid_15, gpl_copyleft_scope__strong_copyleft_reading, resistance(structural), 1991, 0.75).
narrative_ontology:measurement(gpl__grid_16, gpl_copyleft_scope__strong_copyleft_reading, resistance(structural), 2026, 0.62).
narrative_ontology:measurement(gpl__grid_17, gpl_copyleft_scope__strong_copyleft_reading, stakes_inflation(class), 1991, 0.2).
narrative_ontology:measurement(gpl__grid_18, gpl_copyleft_scope__strong_copyleft_reading, stakes_inflation(class), 2026, 0.55).
narrative_ontology:measurement(gpl__grid_19, gpl_copyleft_scope__strong_copyleft_reading, stakes_inflation(individual), 1991, 0.15).
narrative_ontology:measurement(gpl__grid_20, gpl_copyleft_scope__strong_copyleft_reading, stakes_inflation(individual), 2026, 0.45).
narrative_ontology:measurement(gpl__grid_21, gpl_copyleft_scope__strong_copyleft_reading, stakes_inflation(organizational), 1991, 0.4).
narrative_ontology:measurement(gpl__grid_22, gpl_copyleft_scope__strong_copyleft_reading, stakes_inflation(organizational), 2026, 0.82).
narrative_ontology:measurement(gpl__grid_23, gpl_copyleft_scope__strong_copyleft_reading, stakes_inflation(structural), 1991, 0.35).
narrative_ontology:measurement(gpl__grid_24, gpl_copyleft_scope__strong_copyleft_reading, stakes_inflation(structural), 2026, 0.78).
narrative_ontology:measurement(gpl__grid_25, gpl_copyleft_scope__strong_copyleft_reading, suppression(class), 1991, 0.08).
narrative_ontology:measurement(gpl__grid_26, gpl_copyleft_scope__strong_copyleft_reading, suppression(class), 2026, 0.62).
narrative_ontology:measurement(gpl__grid_27, gpl_copyleft_scope__strong_copyleft_reading, suppression(individual), 1991, 0.05).
narrative_ontology:measurement(gpl__grid_28, gpl_copyleft_scope__strong_copyleft_reading, suppression(individual), 2026, 0.52).
narrative_ontology:measurement(gpl__grid_29, gpl_copyleft_scope__strong_copyleft_reading, suppression(organizational), 1991, 0.12).
narrative_ontology:measurement(gpl__grid_30, gpl_copyleft_scope__strong_copyleft_reading, suppression(organizational), 2026, 0.74).
narrative_ontology:measurement(gpl__grid_31, gpl_copyleft_scope__strong_copyleft_reading, suppression(structural), 1991, 0.15).
narrative_ontology:measurement(gpl__grid_32, gpl_copyleft_scope__strong_copyleft_reading, suppression(structural), 2026, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__strong_copyleft_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gpl_copyleft_scope__strong_copyleft_reading, 0.12).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope__narrow_scope_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope__enforcement_vacuum_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, open_source_market_adoption).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, proprietary_software_licensing_constraints).

% DUAL FORMULATION NOTE:
% The GPL copyleft scope kernel (gpl_copyleft_scope) decomposes into three constraint stories instantiating three distinct readings: (1) strong_copyleft_reading (this file): GPL 2(b) extends to all code coupling; high extraction, snare classification; free-software beneficiary, proprietary-vendor target. (2) narrow_scope_reading: GPL 2(b) follows traditional copyright derivative-work doctrine; lower extraction, rope or contested classification; less asymmetric victim set. (3) enforcement_vacuum_reading: absence of judicial precedent allows readings to coexist as interpretive plurality; constraint type depends on which enforcing community (FSF-aligned or industry) controls specific contexts; extraction rates and suppression differ by ecosystem. These are NOT different observable perspectives on one constraint (ε-invariance principle)—they are structurally distinct constraints sharing a common kernel (the text of GPL 2(b)) but instantiating different beneficiary/victim structures and extractiveness. The ε values differ materially (0.78 for strong, ~0.40 for narrow, ~0.55 for vacuum). They are linked via network.affects_constraints because the interpretation landscape is competitive: strong reading enforces (FSF litigation, copyleft campaigns) and influences narrow reading's defensibility; narrow reading would weaken strong reading if adopted (would shift ecosystem baseline); enforcement_vacuum reading is the empirical state where both coexist in fragmented jurisdiction-dependent ecosystems.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_copyleft_scope__strong_copyleft_reading, powerful, 0.88).
constraint_indexing:directionality_override(gpl_copyleft_scope__strong_copyleft_reading, moderate, 0.8).
constraint_indexing:directionality_override(gpl_copyleft_scope__strong_copyleft_reading, organized, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
