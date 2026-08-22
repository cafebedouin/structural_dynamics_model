% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__pragmatic_openness_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: software_control_legitimacy__pragmatic_openness_reading
 *   human_readable: Pragmatic Openness: Software Methodology Choice with Coexisting Models
 *   domain: software_engineering/political_economy/intellectual_property
 *
 * SUMMARY:
 *   The pragmatic openness reading treats software control as a development
 *   methodology choice rather than a moral or property rights issue. It
 *   asserts that open source development — characterized by public peer
 *   review, distributed collaboration, and transparent iteration — tends to
 *   produce higher-quality software, especially for infrastructure,
 *   platforms, and developer tools. Crucially, it grants legitimacy to
 *   proprietary models as valid alternatives for contexts where closed
 *   development better serves commercial sustainability, user experience
 *   integration, or specialized domain requirements. This reading became
 *   institutionally dominant in the late 1990s through the 'open source'
 *   rebranding (OSI, 1998), which explicitly distanced itself from the
 *   freedom-imperative framing of the Free Software Foundation. The
 *   constraint it describes is the emerging professional consensus that
 *   methodological pluralism is healthy: engineers and firms choose open or
 *   closed based on project goals, and the ecosystem benefits from both.
 *
 * KEY AGENTS:
 *   - software_developers: Primary beneficiaries (powerful/mobile) — gain methodological choice, peer learning, career capital from open contribution
 *   - software_users: Beneficiaries (organized/constrained) — receive more reliable, auditable, interoperable software from open development
 *   - open_source_communities: Beneficiaries/agenda_setters (organized/constrained) — coordinate production, set norms, capture reputation value
 *   - commercial_software_vendors: Beneficiaries (institutional/arbitrage) — adopt open source strategically, contribute selectively, maintain proprietary products where advantageous
 *   - analytical_observer: Observer (analytical/analytical) — evaluates quality claims, tracks ecosystem health
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__pragmatic_openness_reading, 0.12).
domain_priors:suppression_score(software_control_legitimacy__pragmatic_openness_reading, 0.15).
domain_priors:theater_ratio(software_control_legitimacy__pragmatic_openness_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__pragmatic_openness_reading, rope).
narrative_ontology:human_readable(software_control_legitimacy__pragmatic_openness_reading, "Pragmatic Openness: Software Methodology Choice with Coexisting Models").
narrative_ontology:topic_domain(software_control_legitimacy__pragmatic_openness_reading, "software_engineering/political_economy/intellectual_property").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__pragmatic_openness_reading, '2227ddbb-a24c-4108-99e8-b5022b9d9e20').
narrative_ontology:cs_kernel_codification('2227ddbb-a24c-4108-99e8-b5022b9d9e20', distributed).
narrative_ontology:cs_authority_grounding('2227ddbb-a24c-4108-99e8-b5022b9d9e20', practice).
narrative_ontology:cs_interpretation_layer_present('2227ddbb-a24c-4108-99e8-b5022b9d9e20').
narrative_ontology:cs_reading_relation('2227ddbb-a24c-4108-99e8-b5022b9d9e20', software_control_legitimacy__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('2227ddbb-a24c-4108-99e8-b5022b9d9e20', software_control_legitimacy__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('2227ddbb-a24c-4108-99e8-b5022b9d9e20', software_control_legitimacy__commons_reading, influences).
narrative_ontology:cs_axiom('2227ddbb-a24c-4108-99e8-b5022b9d9e20', foundational, methodological_pluralism_optimizes_quality).
narrative_ontology:cs_axiom_status(methodological_pluralism_optimizes_quality, holdable).
narrative_ontology:cs_axiom_grounding('2227ddbb-a24c-4108-99e8-b5022b9d9e20', methodological_pluralism_optimizes_quality, empirically_contingent).
narrative_ontology:cs_axiom('2227ddbb-a24c-4108-99e8-b5022b9d9e20', foundational, proprietary_models_are_legitimate_alternatives).
narrative_ontology:cs_axiom_status(proprietary_models_are_legitimate_alternatives, holdable).
narrative_ontology:cs_axiom_grounding('2227ddbb-a24c-4108-99e8-b5022b9d9e20', proprietary_models_are_legitimate_alternatives, conventional).
narrative_ontology:cs_reference_frame('2227ddbb-a24c-4108-99e8-b5022b9d9e20', pre_open_source_coordination_failure).
narrative_ontology:cs_drift_state('2227ddbb-a24c-4108-99e8-b5022b9d9e20', contemporary_cloud_ai_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2227ddbb-a24c-4108-99e8-b5022b9d9e20', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, software_developers).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, software_users).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, open_source_communities).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, commercial_software_vendors).
narrative_ontology:constraint_vindicates(software_control_legitimacy__pragmatic_openness_reading, peer_review_improves_software_quality).
narrative_ontology:constraint_vindicates(software_control_legitimacy__pragmatic_openness_reading, collaborative_development_accelerates_innovation).
narrative_ontology:constraint_vindicates(software_control_legitimacy__pragmatic_openness_reading, methodological_pluralism_benefits_ecosystem).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Choose open or proprietary development per project. Gain from peer review, visible prior art, reusable components, and career capital from open contributions. Exit is easy: switch projects, change employers, fork code. No structural lock-in to either model.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, software_developers, beneficiary,
    powerful, biographical, mobile, global).

% Benefit from more reliable, interoperable, auditable software produced via open development. Also use proprietary software where it offers better UX or specialized features. Exit constrained by ecosystem lock-in (file formats, platform APIs) but not by the methodological choice itself — they can often choose open alternatives.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, software_users, beneficiary,
    organized, biographical, constrained, global).

% Coordinate production norms, maintain shared infrastructure, govern contribution processes. Capture reputation and influence. Constrained exit because community capital is project-specific, but can fork or migrate to adjacent communities. Not trapped — identity is portable across open source projects.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, open_source_communities, agenda_setter,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__pragmatic_openness_reading, open_source_communities, beneficiary).

% Strategically adopt open source for commoditized layers (OS, databases, ML frameworks) while maintaining proprietary products for differentiation. Contribute patches upstream when it reduces maintenance burden. Arbitrage-grade exit: can shift strategy per product line, acquire open source companies, or lobby for policy changes.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, commercial_software_vendors, beneficiary,
    institutional, generational, arbitrage, global).

% Evaluates quality claims, tracks ecosystem health, measures value flows between open and proprietary sectors. No stake in outcomes; exit is analytical frame-switching.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of reliable, scalable software production by enabling distributed peer review and collaborative iteration across organizational boundaries. Open source creates a shared quality-assurance layer that no single firm could afford alone.
% TRANSFER_FUNCTION: Moves engineering effort and bug fixes from contributors (individual and corporate) into shared codebases; moves finished software from those codebases to users and commercial products. No mandatory transfer — participation is voluntary on all sides. Commercial vendors capture value by building proprietary layers atop open foundations.
% ABSENT_VOICES: End-users in regulated or specialized verticals (medical devices, avionics, industrial control) where certification requirements make open collaboration difficult — they would object to the claim that open source is universally 'better' for their domain. Also absent: developers in highly proprietary ecosystems (game consoles, specialized hardware) where open source is structurally excluded by platform gates.
% DISAPPEARANCE_RATIONALE: If the pragmatic openness consensus vanished, the software ecosystem would lose its coordinating methodology framework: firms would revert to fully proprietary stacks (losing shared infrastructure benefits) or adopt freedom-imperative mandates (losing commercial flexibility). The current equilibrium of methodological choice per project would collapse into polarized camps.
% FOUNDING_PROBLEM: Pre-1998 software development suffered from fragmented practices, unreliable proprietary APIs, vendor lock-in with no source access, and duplicated effort on infrastructure. The 'open source' framing (1998) proposed methodological openness as a pragmatic solution to these coordination failures.
% FOUNDING_PROBLEM_CORROBORATION: Independent software engineering research (e.g., studies on Linux kernel defect density, GitHub ecosystem analyses) corroborates that open collaboration solves infrastructure coordination problems. Commercial vendors (Google, Microsoft, Amazon) corroborate by investing billions in open source infrastructure while maintaining proprietary products — revealed preference for pluralism. The freedom_imperative and property_rights readings contest whether the problem is *fully* solved or *correctly* framed, but do not dispute that the coordination failure was real.
narrative_ontology:disappearance_verdict(software_control_legitimacy__pragmatic_openness_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__pragmatic_openness_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__pragmatic_openness_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(software_control_legitimacy__pragmatic_openness_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__pragmatic_openness_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low (0.12) because the reading explicitly rejects coercive enforcement of either model — participation in open source is voluntary, proprietary licensing is a market choice, and no structural transfer is mandated. The slight nonzero ε reflects the soft pressure on firms to 'open source' for talent recruitment and ecosystem legitimacy, which some experience as extractive. Suppression is minimal (0.15) — no alternatives are structurally blocked; proprietary software thrives alongside open source. Theater ratio is very low (0.08) because the coordination function (peer review, collaboration) is genuinely performed and visibly effective in major projects (Linux, Git, Kubernetes, etc.). Accessibility collapse is low (0.25) — developers can and do switch between open and proprietary modes per project. Resistance is moderate (0.35) — the freedom_imperative reading contests the legitimacy grant to proprietary models, and the property_rights reading contests the quality superiority claim for open source.
 *
 * PERSPECTIVAL GAP:
 *   From the software_developer seat, the constraint is a genuine rope: voluntary coordination that improves their craft and career. From the commercial_vendor seat, it is also a rope: they gain a talent pipeline and commoditized infrastructure layer while retaining proprietary differentiation. From the freedom_imperative observer seat, the same arrangement looks like a snare — the legitimacy grant to proprietary software enables continued user subjugation. The engine computes these divergences from the declared roles and exit options; the reading itself does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   All declared beneficiaries (developers, users, open_source_communities, commercial_vendors) are net recipients of the constraint's coordination function — they gain methodological choice, quality improvements, and ecosystem interoperability without being forced to contribute. No victim set is declared because the reading structurally denies coercion: proprietary developers choose their model; open source contributors choose their projects; users choose their software. The slight directionality toward developers and users as primary beneficiaries reflects the reading's quality-optimization telos. Commercial vendors sit near symmetric (d ~ 0.5): they extract value from open source but also contribute strategically, and their exit options (arbitrage) keep effective extraction low.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fragmented, unreliable software development practices) remains live but transformed: modern software complexity makes coordination *more* valuable, not less. The reading has not atrophied — its coordination function has expanded with the software ecosystem. No mandatrophy resolution is declared because the constraint's justification (better software through methodological choice) is actively confirmed by the growth of open source in critical infrastructure. The property_rights and freedom_imperative readings persist as live contestation, not as evidence of this reading's obsolescence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disambiguation,
    'Is this constraint a genuine methodological stance (coexisting legitimate models) or a pragmatic compromise that covertly legitimizes proprietary dominance?',
    'Trace institutional funding and governance: if major commercial vendors fund ''pragmatic openness'' advocacy while maintaining proprietary cores, the reading may function as extraction cover. Empirical audit of sponsor networks and code ownership patterns.',
    'If cover story, reclassifies toward tangled_rope with commercial vendors as beneficiaries and open_source_contributors as victims. If genuine, remains rope with plural beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, conceptual, 'Whether pragmatic openness is a stable methodological position or a contingent compromise masking power asymmetry').

omega_variable(
    extraction_boundary_of_coexistence,
    'Does the coexistence of proprietary and open models extract value from open source labor without reciprocity?',
    'Measure value flows: corporate consumption of open source (dependency chains, cloud hosting) vs. corporate contribution (patches, funding, maintenance). Net negative flow indicates extraction.',
    'If extraction exists, the ''legitimate alternatives'' framing obscures a transfer function from commons to proprietary capture, shifting ε upward.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_boundary_of_coexistence, empirical, 'Whether methodological pluralism masks asymmetric value capture from open source ecosystems').

omega_variable(
    quality_claim_empirical_basis,
    'Is the claim ''open source produces better software'' empirically robust across domains, or does it hold only in specific niches (infrastructure, tooling) while failing in others (end-user applications, specialized verticals)?',
    'Longitudinal defect density, security vulnerability, and maintainability studies across open vs. closed codebases in matched domains. Control for team size, funding, and domain maturity.',
    'If the quality advantage is domain-contingent, the reading''s coordination function is narrower than claimed — it coordinates well for some software types but not others, making ''better software'' a partial truth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quality_claim_empirical_basis, empirical, 'Domain generality of the open source quality advantage claim').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__pragmatic_openness_reading, 1998, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1998, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 1998, 0.05).
narrative_ontology:measurement(soft_tr_t2004, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 2004, 0.06).
narrative_ontology:measurement(soft_tr_t2010, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 2010, 0.07).
narrative_ontology:measurement(soft_tr_t2016, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 2016, 0.08).
narrative_ontology:measurement(soft_tr_t2020, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 2020, 0.08).
narrative_ontology:measurement(soft_tr_t2024, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 2024, 0.08).

% Extraction over time
narrative_ontology:measurement(soft_be_t1998, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 1998, 0.08).
narrative_ontology:measurement(soft_be_t2004, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2004, 0.1).
narrative_ontology:measurement(soft_be_t2010, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2010, 0.11).
narrative_ontology:measurement(soft_be_t2016, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2016, 0.12).
narrative_ontology:measurement(soft_be_t2020, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2020, 0.12).
narrative_ontology:measurement(soft_be_t2024, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2024, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1998, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 1998, 0.1).
narrative_ontology:measurement(soft_su_t2004, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 2004, 0.12).
narrative_ontology:measurement(soft_su_t2010, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 2010, 0.14).
narrative_ontology:measurement(soft_su_t2016, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 2016, 0.15).
narrative_ontology:measurement(soft_su_t2020, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 2020, 0.15).
narrative_ontology:measurement(soft_su_t2024, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__pragmatic_openness_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_control_legitimacy__pragmatic_openness_reading, 0.1).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__property_rights_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__commons_reading).

% DUAL FORMULATION NOTE:
% This reading decomposes the kernel's 'software control' label into a quality-optimization coordination claim (open source peer review) + a pluralism grant (proprietary legitimacy). The freedom_imperative reading extracts a moral constraint from the same kernel; the property_rights reading extracts an exclusionary property constraint; the commons_reading extracts a governance constraint. Each has distinct ε, beneficiaries, and victims. The network edges reflect institutional citation: pragmatic openness is cited by commercial actors to legitimize selective openness; freedom_imperative cites it as compromise; property_rights cites it as evidence that openness is optional.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(software_control_legitimacy__pragmatic_openness_reading, institutional, 0.45).
constraint_indexing:directionality_override(software_control_legitimacy__pragmatic_openness_reading, powerful, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
