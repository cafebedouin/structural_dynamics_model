% ============================================================================
% CONSTRAINT STORY: software_source_status__pragmatic_development_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__pragmatic_development_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: software_source_status__pragmatic_development_reading
 *   human_readable: Open Source as Superior Development Methodology (Pragmatic Reading)
 *   domain: software_engineering/political_economy_of_technology
 *
 * SUMMARY:
 *   The pragmatic reading instantiates one interpretation of the contested
 *   software-source-status kernel: open source is valued as a superior
 *   development methodology because transparency, peer review, and
 *   collaborative iteration produce higher-quality, more-secure,
 *   more-innovative software faster than proprietary closed development. This
 *   reading does NOT claim software freedom is an ethical imperative; it
 *   treats openness as instrumentally justified by quality outcomes.
 *   Proprietary software is not inherently illegitimate under this reading —
 *   it is methodologically disadvantaged and therefore increasingly displaced
 *   in domains where quality and security are competitive factors. The
 *   reading has become institutionalized in cloud infrastructure, systems
 *   software, security tooling, and academic computing, and it shapes
 *   investment and hiring narratives in technology organizations globally.
 *
 * KEY AGENTS:
 *   - open_source_contributors: gain reputation, code quality, collaborative problem-solving; stake in the reading's empirical claim that open development produces superior results
 *   - software_users: gain auditability, customization, security transparency; stake in reading validating their access to source code as a quality assurance mechanism
 *   - innovation_ecosystem: gains reusable foundations, interoperability, faster derivative development; stake in network effects that open-source architecture enables
 *   - proprietary_software_developers: face narrative pressure that their methodology is inferior; bear cost in recruitment, legitimacy, and market positioning when the reading holds sway
 *   - proprietary_licensing_advocates: bear cost to core legitimacy claim that intellectual property restriction is justified; must defend proprietary development on non-quality grounds
 *   - freedom_imperative_advocates: EXCLUDED from coordination story; their normative claim is sidelined by instrumentalist framing
 *   - regulators_and_policy_makers: use the reading to justify digital sovereignty, government adoption of open source, and security-audit requirements without endorsing the freedom-as-right claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__pragmatic_development_reading, 0.31).
domain_priors:suppression_score(software_source_status__pragmatic_development_reading, 0.18).
domain_priors:theater_ratio(software_source_status__pragmatic_development_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__pragmatic_development_reading, rope).
narrative_ontology:human_readable(software_source_status__pragmatic_development_reading, "Open Source as Superior Development Methodology (Pragmatic Reading)").
narrative_ontology:topic_domain(software_source_status__pragmatic_development_reading, "software_engineering/political_economy_of_technology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__pragmatic_development_reading, '0c0eca39-7021-4fb2-ab63-9be9642d1e9a').
narrative_ontology:cs_kernel_codification('0c0eca39-7021-4fb2-ab63-9be9642d1e9a', distributed).
narrative_ontology:cs_authority_grounding('0c0eca39-7021-4fb2-ab63-9be9642d1e9a', distributed).
narrative_ontology:cs_reading_relation('0c0eca39-7021-4fb2-ab63-9be9642d1e9a', software_source_status__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('0c0eca39-7021-4fb2-ab63-9be9642d1e9a', software_source_status__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('0c0eca39-7021-4fb2-ab63-9be9642d1e9a', software_source_status__utilitarian_hybrid_reading, influences).
narrative_ontology:cs_axiom('0c0eca39-7021-4fb2-ab63-9be9642d1e9a', foundational, transparency_improves_quality).
narrative_ontology:cs_axiom_status(transparency_improves_quality, holdable).
narrative_ontology:cs_axiom_grounding('0c0eca39-7021-4fb2-ab63-9be9642d1e9a', transparency_improves_quality, empirically_contingent).
narrative_ontology:cs_axiom('0c0eca39-7021-4fb2-ab63-9be9642d1e9a', foundational, openness_instrumentally_justified).
narrative_ontology:cs_axiom_status(openness_instrumentally_justified, holdable).
narrative_ontology:cs_axiom_grounding('0c0eca39-7021-4fb2-ab63-9be9642d1e9a', openness_instrumentally_justified, instrumental).
narrative_ontology:cs_reference_frame('0c0eca39-7021-4fb2-ab63-9be9642d1e9a', peer_review_quality_assurance).
narrative_ontology:cs_drift_state('0c0eca39-7021-4fb2-ab63-9be9642d1e9a', contemporary_corporate_open_source_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0c0eca39-7021-4fb2-ab63-9be9642d1e9a', '').
narrative_ontology:cs_kernel_id(software_source_status__pragmatic_development_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, open_source_contributors).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, software_users_accessing_quality).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, innovation_ecosystem_participants).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__pragmatic_development_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(software_source_status__pragmatic_development_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__pragmatic_development_reading_tests).
:- end_tests(software_source_status__pragmatic_development_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is LOW (0.31 at interval end) because the constraint's operation depends on voluntary contribution and genuine coordination benefit (quality), not coercion. Suppression is similarly LOW (0.18) because exit from open-source participation is available — contributors can choose proprietary work, different projects, or cessation. Theater is MODERATE-LOW (0.22) because the coordination function (peer review improving quality) is real and ongoing; the theater component grows as corporate open-source marketing and 'ethics washing' increase (performing commitment to the reading without substantive investment). MEASUREMENT TRAJECTORY: All three metrics show slow RISE from 1990 to 2026. Extractiveness rises because open source becomes institutionalized and mandatory in career paths, and because corporate control of major projects (GitHub, npm, container registries) introduces extractive layers (vendor lock-in on platforms, surveillance of development patterns, forced adoption of corporate governance). Suppression rises as developers face implicit pressure to open-source work for career advancement and reputation, and as proprietary development becomes stigmatized. Theater rises as corporate entities adopt open-source framing for legitimacy while centralizing control of popular projects. The constraint is EARLY in its extraction ratchet — the founding problem (quality through transparency) is still substantially real, so extractiveness and suppression remain low relative to snares or tangled ropes. The measurement grid is shared across all three metrics: every point appears in all three series.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (open-source contributors, ecosystem builders), the reading is a coordination achievement — the constraint captures genuine quality benefits and aligns incentives around transparency. From the payer seats (proprietary developers, proprietary advocates), the reading operates as delegitimization — their methodologies and business models are narratively displaced regardless of whether their quality or security outcomes are actually inferior. From the excluded seats (freedom advocates), the reading commits a deeper wrong: it instrumentalizes freedom as a means to quality, sidelining the ethical claim to freedom as an end. The engine should compute distinctly different classifications at each seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Open-source contributors sit near the beneficiary end (d ~0.15): they gain reputation, code quality, collaborative benefits; their exit is mobile (they can work on other projects or proprietary software). Users and ecosystem actors sit near symmetric (d ~0.4–0.5): genuine coordination benefit (quality, innovation acceleration) balanced against the cost of voluntary dependency on commons maintenance. Proprietary developers are payers (d ~0.65): they bear narrative cost and market pressure as the reading delegitimizes their methodology, though their exit is constrained by market structure rather than absolute. Freedom advocates are STRUCTURALLY OUTSIDE the directionality calculation: they are excluded from the coordination story, so their d cannot be computed from the beneficiary/victim data that grounds this reading's structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The pragmatic reading avoids the mandatrophy trap that threatens the freedom_imperative_reading. Freedom-as-right claims become outdated if social conditions change (e.g., software becomes so complex that proprietary closed review becomes empirically superior, or if freedom advocacy is captured by corporate interests). The pragmatic reading's mandate — transparency improves quality — remains live as long as code review and distributed auditing continue to catch bugs and vulnerabilities faster than proprietary review pipelines. The constraint's persistence is justified by ongoing empirical vindication of peer review, not by foundational rights claims that could atrophy. However, the reading is vulnerable to a different mandatrophy: if proprietary development produces equal or superior quality outcomes but remains closed due to vendor lock-in and market power (not methodology), the reading's justification persists as theater while the coordination function fades. The measurement trajectory hints at this: extractiveness and theater both rise while the founding problem remains live — a sign that institutional capture of open-source tooling may be introducing new extraction mechanisms that degrade the pure coordination story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quality_empirics_contestation,
    'Is the claimed quality superiority of open-source development empirically robust, or does it reflect selection bias (open source dominates quality-critical domains like security and infrastructure while proprietary software dominates user-facing domains where other factors matter more)?',
    'Large-scale empirical studies comparing defect density, security vulnerability counts, and mean-time-to-fix across comparable open and proprietary codebases, controlling for domain, scale, and maintenance investment. Requires reproducible methodology and independent corroboration.',
    'If the quality advantage is selection bias rather than inherent methodology: the reading''s entire justification collapses, and the constraint becomes pure extraction masked by legitimacy narrative (reclassifies toward snare). If the quality advantage is real but domain-specific: the reading remains valid for quality-critical domains but loses universality, reclassifying toward rope-with-limited-scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quality_empirics_contestation, empirical, 'Whether open development''s apparent quality superiority is methodological or driven by domain selection bias.').

omega_variable(
    freedom_vs_pragmatism_distinction,
    'Does instrumentalizing freedom (treating it as means to quality) represent a legitimate reading of the kernel, or does it commit a category error by subsetting a broader normative claim?',
    'Genealogical and philosophical analysis of how freedom-as-right and freedom-as-method relate. Does the pragmatic reading leave room for freedom advocates'' core claim, or does it foreclose it by reframing freedom as contingent on outcomes?',
    'If the readings are logically independent: the pragmatic reading coexists legitimately with the freedom reading. If pragmatism forecloses freedom-as-right by making freedom contingent on quality: the relationship is foreclosure rather than coexistence, and the kernel contest is unresolvable within a single framework (true kernel contest, not just perspective difference).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(freedom_vs_pragmatism_distinction, conceptual, 'Whether pragmatic and freedom-imperative readings coexist or one forecloses the other.').

omega_variable(
    institutional_capture_of_open_source,
    'As corporate entities (GitHub, npm, etc.) centralize control of major open-source projects and platforms, does open development remain a genuine coordination solution or become a captured extraction mechanism dressed in coordination language?',
    'Monitor the extractiveness and theater_ratio trajectories: rising extractiveness with rising theater suggests capture (extraction increasing while coordination function degrades). Test by examining whether developers retain exit capability (can they fork projects, run alternative registries, build alternative platforms?) or whether platform switching costs are now prohibitive.',
    'Evidence of capture would suggest the reading''s coordinate function is atrophying while extraction mechanisms layer on top. The constraint would reclassify toward tangled_rope or snare as the founding-problem justification decays and mandatory open-source participation becomes enforced by market power rather than chosen for quality benefits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_of_open_source, empirical, 'Whether corporate platform centralization is capturing open source as an extraction mechanism.').

omega_variable(
    kernel_reading_coexistence,
    'Can the pragmatic development reading coexist with the freedom imperative reading in a single normative framework, or do they logically foreclose each other?',
    'Attempt to construct a coherent normative position that holds BOTH ''open development is methodologically superior'' (pragmatic) AND ''software freedom is an ethical imperative independent of outcomes'' (freedom). If the construction succeeds without contradiction, readings coexist. If it fails (one must be rejected for logical consistency), determine which reading forecloses which.',
    'If they foreclose each other: the kernel contest is deep and unresolvable (true antagonism of first principles). If they coexist: different parties can hold both, and the contested kernel remains live across multiple readings. This affects how regulatory and policy resolution can proceed — foreclosure suggests winner-take-all dynamics; coexistence allows pluralist accommodation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_coexistence, conceptual, 'Logical relationship between pragmatic and freedom-imperative readings of the software source kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__pragmatic_development_reading, 1990, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1990, software_source_status__pragmatic_development_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(soft_tr_t2000, software_source_status__pragmatic_development_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(soft_tr_t2010, software_source_status__pragmatic_development_reading, theater_ratio, 2010, 0.16).
narrative_ontology:measurement(soft_tr_t2016, software_source_status__pragmatic_development_reading, theater_ratio, 2016, 0.19).
narrative_ontology:measurement(soft_tr_t2021, software_source_status__pragmatic_development_reading, theater_ratio, 2021, 0.21).
narrative_ontology:measurement(soft_tr_t2026, software_source_status__pragmatic_development_reading, theater_ratio, 2026, 0.22).

% Extraction over time
narrative_ontology:measurement(soft_be_t1990, software_source_status__pragmatic_development_reading, base_extractiveness, 1990, 0.05).
narrative_ontology:measurement(soft_be_t2000, software_source_status__pragmatic_development_reading, base_extractiveness, 2000, 0.12).
narrative_ontology:measurement(soft_be_t2010, software_source_status__pragmatic_development_reading, base_extractiveness, 2010, 0.22).
narrative_ontology:measurement(soft_be_t2016, software_source_status__pragmatic_development_reading, base_extractiveness, 2016, 0.28).
narrative_ontology:measurement(soft_be_t2021, software_source_status__pragmatic_development_reading, base_extractiveness, 2021, 0.3).
narrative_ontology:measurement(soft_be_t2026, software_source_status__pragmatic_development_reading, base_extractiveness, 2026, 0.31).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1990, software_source_status__pragmatic_development_reading, suppression_requirement, 1990, 0.06).
narrative_ontology:measurement(soft_su_t2000, software_source_status__pragmatic_development_reading, suppression_requirement, 2000, 0.1).
narrative_ontology:measurement(soft_su_t2010, software_source_status__pragmatic_development_reading, suppression_requirement, 2010, 0.14).
narrative_ontology:measurement(soft_su_t2016, software_source_status__pragmatic_development_reading, suppression_requirement, 2016, 0.16).
narrative_ontology:measurement(soft_su_t2021, software_source_status__pragmatic_development_reading, suppression_requirement, 2021, 0.17).
narrative_ontology:measurement(soft_su_t2026, software_source_status__pragmatic_development_reading, suppression_requirement, 2026, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__pragmatic_development_reading, information_standard).
narrative_ontology:boltzmann_floor_override(software_source_status__pragmatic_development_reading, 0.05).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__property_rights_reading).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% The software_source_status kernel decomposes into four constraint stories, each representing a distinct reading of the contested question 'what status should source code have?' This reading (pragmatic_development) claims open development is superior because transparency and peer review produce higher-quality, more-secure, more-innovative software. Sibling readings treat source code as an ethical imperative (freedom), as legitimate intellectual property (property_rights), or as contextually variable (utilitarian_hybrid). The four stories are NOT alternative measurements of one constraint; they are structurally distinct constraints with different ε values, different beneficiary/victim structures, and different persistence mechanisms. Links via affects_constraints capture the kernel's unity while acknowledging reading-level independence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
