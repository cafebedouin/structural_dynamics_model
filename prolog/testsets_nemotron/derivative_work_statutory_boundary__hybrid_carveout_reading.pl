% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__hybrid_carveout_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__hybrid_carveout_reading, []).

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
 *   constraint_id: derivative_work_statutory_boundary__hybrid_carveout_reading
 *   human_readable: Derivative Work Boundary with Commercial/Non-Commercial Carveout
 *   domain: intellectual_property/technology_governance/information_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the hybrid carveout reading of the
 *   derivative work statutory boundary kernel: the law permits non-commercial
 *   transformative uses without authorization while requiring commercial
 *   users to license. The constraint operates as a categorical split —
 *   non-commercial creators (fan fiction writers, remix artists, academic
 *   researchers, educators) face no extraction; commercial developers and
 *   platforms face licensing costs and clearance burdens. The claimed type is
 *   tangled_rope because the constraint simultaneously coordinates (provides
 *   a clear rule for non-commercial transformative use, reducing uncertainty
 *   for that class) and extracts (commercial actors pay rents to rightsholder
 *   licensing entities). The kernel is contested: the enclosure reading would
 *   treat all uses as derivative works requiring authorization (higher ε,
 *   snare); the coordination reading would treat only fixed recastings as
 *   derivative (lower ε, rope). This reading sits between them with a
 *   categorical commercial/non-commercial split.
 *
 * KEY AGENTS:
 *   - rightsholder_licensing_entities: Primary beneficiary (institutional/moderate) — collects licensing revenue from commercial users
 *   - non_commercial_creators: Primary beneficiary (organized/mobile) — exempted from licensing, permitted transformative use
 *   - commercial_developers: Primary victim (powerful/constrained) — bears licensing costs and clearance friction
 *   - commercial_transformative_users: Primary victim (moderate/constrained) — bears costs despite transformative purpose
 *   - courts_interpretive_bodies: Agenda setter (institutional/analytical) — adjudicates boundary cases, defines transformative/commercial
 *   - platform_intermediaries: Secondary actor (institutional/constrained) — enforces at scale via Content ID, takedown systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.42).
domain_priors:suppression_score(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.55).
domain_priors:theater_ratio(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__hybrid_carveout_reading, tangled_rope).
narrative_ontology:human_readable(derivative_work_statutory_boundary__hybrid_carveout_reading, "Derivative Work Boundary with Commercial/Non-Commercial Carveout").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__hybrid_carveout_reading, "intellectual_property/technology_governance/information_economics").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__hybrid_carveout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__hybrid_carveout_reading, '2c962e25-8d4d-4abf-807e-ea1f6f2e2ee3').
narrative_ontology:cs_kernel_codification('2c962e25-8d4d-4abf-807e-ea1f6f2e2ee3', formalized).
narrative_ontology:cs_authority_grounding('2c962e25-8d4d-4abf-807e-ea1f6f2e2ee3', lineage).
narrative_ontology:cs_interpretation_layer_present('2c962e25-8d4d-4abf-807e-ea1f6f2e2ee3').
narrative_ontology:cs_reading_relation('2c962e25-8d4d-4abf-807e-ea1f6f2e2ee3', derivative_work_statutory_boundary__enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('2c962e25-8d4d-4abf-807e-ea1f6f2e2ee3', derivative_work_statutory_boundary__coordination_reading, coexists_with).
narrative_ontology:cs_axiom('2c962e25-8d4d-4abf-807e-ea1f6f2e2ee3', foundational, commercial_exploitation_triggers_derivative_right).
narrative_ontology:cs_axiom_status(commercial_exploitation_triggers_derivative_right, holdable).
narrative_ontology:cs_axiom_grounding('2c962e25-8d4d-4abf-807e-ea1f6f2e2ee3', commercial_exploitation_triggers_derivative_right, conventional).
narrative_ontology:cs_axiom('2c962e25-8d4d-4abf-807e-ea1f6f2e2ee3', foundational, non_commercial_transformative_use_exempt).
narrative_ontology:cs_axiom_status(non_commercial_transformative_use_exempt, holdable).
narrative_ontology:cs_axiom_grounding('2c962e25-8d4d-4abf-807e-ea1f6f2e2ee3', non_commercial_transformative_use_exempt, conventional).
narrative_ontology:cs_reference_frame('2c962e25-8d4d-4abf-807e-ea1f6f2e2ee3', statutory_derivative_work_right_with_fair_use_codification).
narrative_ontology:cs_drift_state('2c962e25-8d4d-4abf-807e-ea1f6f2e2ee3', platform_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2c962e25-8d4d-4abf-807e-ea1f6f2e2ee3', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, rightsholder_licensing_entities).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, non_commercial_creators).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_developers).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_transformative_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, academic_researchers).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, platform_intermediaries).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_exploitation_distinction_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collect licensing revenue from commercial users via collective management organizations and direct licensing. Set terms through market power and statutory defaults. Face near-zero exit costs — they administer the constraint and can lobby to expand its scope. Their revenue depends on the commercial carveout being enforced.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, rightsholder_licensing_entities, beneficiary,
    institutional, generational, arbitrage, global).

% Create transformative works (fan fiction, remixes, critiques, educational materials) without seeking permission or paying royalties. The categorical exemption is their primary benefit. Exit is mobile — they can remain non-commercial or shift to commercial if they choose to license. Organized via communities (AO3, GitHub, creative commons) but individually mobile.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, non_commercial_creators, beneficiary,
    organized, biographical, mobile, global).

% Build commercial products incorporating copyrighted expression (game mods, sampling-based music, adaptation-based media). Must clear rights or risk infringement. Licensing costs are significant and clearance is uncertain. Exit is constrained — audiences are on platforms that enforce via Content ID; leaving means abandoning distribution channels. Powerful individually (large studios) but constrained by the enforcement infrastructure.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_developers, payer,
    powerful, biographical, constrained, global).

% Create transformative works for commercial purposes (parody merchandise, commercial critique channels, transformative apps). Despite transformative purpose, the commercial trigger requires authorization. Bear same licensing costs as non-transformative commercial users. Exit is constrained — platform monetization requires compliance; alternative distribution lacks audience reach.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_transformative_users, payer,
    moderate, biographical, constrained, global).

% Adjudicate boundary cases: what counts as transformative, what counts as commercial, where the line falls. Their rulings define the operational boundary for all other seats. Neither collect licensing revenue nor pay it — they interpret. Exit is analytical — they observe the system from outside its extraction flows.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, courts_interpretive_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Operate the algorithmic enforcement layer (Content ID, copyright match systems, takedown workflows). Bear compliance costs and safe harbor obligations. Also benefit from the constraint — it structures their relationship with rightsholders and creators. Exit is constrained — they cannot operate user-generated content platforms without some enforcement regime. Their enforcement choices effectively set the practical boundary stricter than the legal one.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, platform_intermediaries, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__hybrid_carveout_reading, platform_intermediaries, payer).

% Create works using only public domain or CC0 sources to avoid the constraint entirely. They would object to the constraint's scope if present but have exited via source selection. Their absence from the licensing market is a structural response to the constraint's cost.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, public_domain_creators, excluded,
    moderate, biographical, mobile, global).

% Rely on non-commercial transformative exemption for text mining, computational analysis, and scholarly critique. The carveout enables research that would be impossible under licensing. Mobile exit — they can frame work as non-commercial. Institutional policies often reinforce non-commercial framing.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, academic_researchers, beneficiary,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__hybrid_carveout_reading, rightsholder_licensing_entities).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__hybrid_carveout_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear categorical rule that exempts non-commercial transformative use from licensing, reducing transaction costs and legal uncertainty for a large class of creators (educators, fan communities, researchers, hobbyists) who would otherwise face prohibitive clearance burdens.
% TRANSFER_FUNCTION: Moves licensing revenue from commercial developers and commercial transformative users to rightsholder licensing entities (collective management organizations, publishers, studios) as the price of legally incorporating copyrighted expression into commercial products.
% ABSENT_VOICES: Commercial transformative users who would argue that transformative purpose should matter more than commercial status — they are partially represented by commercial_developers but their distinct transformative claim is not separately voiced. Public domain advocates who would argue the constraint's scope chills even non-infringing uses — they are excluded by the constraint's boundary definition.
% DISAPPEARANCE_RATIONALE: If the hybrid carveout vanished overnight, non-commercial creators would face immediate infringement risk for transformative works (world rearranges for them — loss of safe harbor). Commercial licensing markets would collapse or reorganize around new defaults (world rearranges for rightsholders and commercial developers). Platform enforcement systems would lose their legal basis for categorical non-commercial exemptions.
% FOUNDING_PROBLEM: Preventing unauthorized commercial substitution for original works while avoiding the prohibitive transaction costs of requiring licenses for all transformative uses, especially non-commercial ones where licensing markets fail.
% FOUNDING_PROBLEM_CORROBORATION: Rightsholder entities attest the problem is live — unauthorized commercial substitution persists and licensing markets are essential. Non-commercial creator communities and digital rights organizations attest the problem is substantially solved for non-commercial uses and the carveout is justified. Courts and legislatures are the contested arena — case law oscillates between expanding and contracting the transformative/commercial boundary.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__hybrid_carveout_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__hybrid_carveout_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__hybrid_carveout_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(derivative_work_statutory_boundary__hybrid_carveout_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__hybrid_carveout_reading_tests).
:- end_tests(derivative_work_statutory_boundary__hybrid_carveout_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42) is moderate — commercial actors face real licensing costs but non-commercial actors face zero. Suppression (0.55) is elevated because the commercial boundary is actively enforced via platform systems (Content ID, automated takedown) and litigation threat, not merely by rightsholder preference. Theater ratio (0.28) reflects that the transformative use standard has genuine doctrinal content but is increasingly performed via algorithmic proxies that over-suppress. Accessibility collapse (0.35) is low because alternatives exist (public domain, CC-licensed works, non-transformative creation) and the boundary is porous. Resistance (0.48) is moderate — commercial actors lobby for broader fair use; rightsholders lobby for narrower transformative standards.
 *
 * DIRECTIONALITY LOGIC:
 *   Rightsholder licensing entities are structural beneficiaries (d ~ 0.15): they collect licensing revenue, set terms via collective management organizations, and face near-zero exit costs from the constraint. Non-commercial creators are structural beneficiaries (d ~ 0.10): they receive a categorical exemption, have mobile exit (can stay non-commercial), and the constraint subsidizes their activity. Commercial developers are structural targets (d ~ 0.85): they bear licensing costs, face constrained exit (cannot reach audiences without licensing or risk infringement), and have powerful but not arbitrage-level power. Commercial transformative users are structural targets (d ~ 0.80): same cost burden despite transformative purpose, constrained exit. Courts are agenda setters (d ~ 0.50 symmetric): they interpret but do not systematically collect or pay. Platform intermediaries are near-symmetric (d ~ 0.55): they bear compliance costs but also benefit from safe harbor.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing unauthorized commercial substitution for original works) remains live but has been partially solved by licensing markets. The constraint persists because the commercial/non-commercial split creates a stable political economy: rightsholders get revenue, non-commercial creators get freedom, and the commercial middle bears the cost. This is not pure mandatrophy — the coordination function for non-commercial creators is genuine — but the extraction from commercial transformative users exceeds what the coordination function justifies, making it a tangled rope rather than a clean rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the hybrid carveout reading a distinct constraint from the enclosure and coordination readings of the derivative work statutory boundary kernel?',
    'Compare ε values and structural beneficiary/victim sets across readings. If ε differs substantially or beneficiary/victim sets are disjoint, they are separate constraints.',
    'If confirmed as distinct, each reading gets its own classification and the kernel decomposes into a constraint family linked by network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the three readings instantiate three separate constraints with invariant ε values.').

omega_variable(
    commercial_boundary_ambiguity,
    'Where exactly does the commercial/non-commercial boundary fall for edge cases (e.g., ad-supported platforms, donation-funded creators, nonprofit entities selling merchandise)?',
    'Case law tracking and regulatory guidance on commercial use definitions in copyright contexts.',
    'Boundary ambiguity inflates suppression (chilling effect) and creates a de facto expansion of the extraction zone beyond clear commercial actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commercial_boundary_ambiguity, empirical, 'Structural ambiguity in the commercial/non-commercial distinction that affects who counts as a victim.').

omega_variable(
    transformative_standard_stability,
    'Is the ''transformative use'' standard stable enough to serve as a coordination mechanism, or does its case-by-case unpredictability make it a cover for discretionary enforcement?',
    'Empirical analysis of transformative use rulings across jurisdictions and time; variance in outcomes for similar fact patterns.',
    'If the standard is predictably applied, the non-commercial carveout functions as genuine coordination; if unpredictable, it functions as a discretionary grant that rightsholders can override — shifting the constraint toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transformative_standard_stability, conceptual, 'Whether the transformative use standard provides genuine predictability or operates as discretionary cover.').

omega_variable(
    cs_framing_underdetermination,
    'Does the commitment system structure of this constraint frame the kernel as the statutory text itself (formalized), or as the judicial interpretive tradition that has built up around fair use and derivative works (distributed/practice)?',
    'Trace how courts and litigants actually invoke authority: do they cite the statutory text as controlling, or the interpretive tradition as the operative norm?',
    'If the text is the declared kernel but the interpretive tradition is the operative authority, the CS classification shifts from formalized+lineage to distributed/practice, changing which drift modes are detectable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Alternative CS framings of the same kernel that yield different kernel_codification/authority_grounding values.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__hybrid_carveout_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dwhc_tr_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(dwhc_tr_t0, observed).
narrative_ontology:measurement(dwhc_tr_t10, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(dwhc_tr_t10, observed).
narrative_ontology:measurement(dwhc_tr_t20, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(dwhc_tr_t20, observed).
narrative_ontology:measurement(dwhc_tr_t30, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(dwhc_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(dwhc_be_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(dwhc_be_t0, observed).
narrative_ontology:measurement(dwhc_be_t10, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement_basis(dwhc_be_t10, observed).
narrative_ontology:measurement(dwhc_be_t20, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 20, 0.36).
narrative_ontology:measurement_basis(dwhc_be_t20, observed).
narrative_ontology:measurement(dwhc_be_t30, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement_basis(dwhc_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(dwhc_su_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(dwhc_su_t0, observed).
narrative_ontology:measurement(dwhc_su_t10, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement_basis(dwhc_su_t10, observed).
narrative_ontology:measurement(dwhc_su_t20, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement_basis(dwhc_su_t20, observed).
narrative_ontology:measurement(dwhc_su_t30, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement_basis(dwhc_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__hybrid_carveout_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.15).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary__enclosure_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary__coordination_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, platform_content_id_enforcement).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, collective_management_licensing).

% DUAL FORMULATION NOTE:
% The derivative_work_statutory_boundary kernel decomposes into three constraint stories with distinct ε values and beneficiary/victim structures. This reading (hybrid_carveout) has moderate ε (0.42) with a categorical beneficiary split. The enclosure_reading has high ε (expected >0.7) with universal victimhood. The coordination_reading has low ε (expected <0.2) with minimal victims. All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(derivative_work_statutory_boundary__hybrid_carveout_reading, powerful, 0.85).
constraint_indexing:directionality_override(derivative_work_statutory_boundary__hybrid_carveout_reading, moderate, 0.8).
constraint_indexing:directionality_override(derivative_work_statutory_boundary__hybrid_carveout_reading, institutional, 0.15).
constraint_indexing:directionality_override(derivative_work_statutory_boundary__hybrid_carveout_reading, organized, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
