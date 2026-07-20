% ============================================================================
% CONSTRAINT STORY: software_source_status__pragmatic_development_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: software_source_status__pragmatic_development_reading
 *   human_readable: Pragmatic Open Source Development Superiority Claim
 *   domain: software_engineering/political_economy/intellectual_property
 *
 * SUMMARY:
 *   This constraint instantiates the pragmatic_development_reading of the
 *   software_source_status kernel. The colloquial label 'open source'
 *   conflates multiple structurally distinct claims: the
 *   freedom_imperative_reading (ethical deontology), this
 *   pragmatic_development_reading (instrumental quality), the
 *   property_rights_reading (creator exclusion rights), and the
 *   utilitarian_hybrid_reading (context-dependent welfare maximization). This
 *   file addresses ONLY the pragmatic reading: open development as a superior
 *   methodology justified by empirical quality outcomes, with proprietary
 *   software treated as legitimate but inferior. The arrangement coordinates
 *   genuine distributed development while asymmetrically extracting unpaid
 *   labor from individual maintainers who are identity-locked into the
 *   ecosystem.
 *
 * KEY AGENTS:
 *   - Corporate OSS consumers (institutional/arbitrage): Primary beneficiaries and agenda-setters who capture value from freely available infrastructure.
 *   - Unpaid maintainers (moderate/identity_locked): Primary targets whose labor is extracted and whose professional identity fuses with the constraint.
 *   - OSS foundations (institutional/constrained): Agenda-setters that administer legitimacy definitions and depend on corporate sponsorship.
 *   - Proprietary software vendors (powerful/constrained): Targets experiencing market suppression from zero-cost alternatives.
 *   - Academic researchers (analytical/analytical): Observer seat providing empirical evaluation of the quality claims.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__pragmatic_development_reading, 0.63).
domain_priors:suppression_score(software_source_status__pragmatic_development_reading, 0.48).
domain_priors:theater_ratio(software_source_status__pragmatic_development_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, extractiveness, 0.63).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__pragmatic_development_reading, tangled_rope).
narrative_ontology:human_readable(software_source_status__pragmatic_development_reading, "Pragmatic Open Source Development Superiority Claim").
narrative_ontology:topic_domain(software_source_status__pragmatic_development_reading, "software_engineering/political_economy/intellectual_property").

domain_priors:requires_active_enforcement(software_source_status__pragmatic_development_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__pragmatic_development_reading, 'd60acb15-db2c-48df-b9d4-da28fde9acd6').
narrative_ontology:cs_kernel_codification('d60acb15-db2c-48df-b9d4-da28fde9acd6', distributed).
narrative_ontology:cs_authority_grounding('d60acb15-db2c-48df-b9d4-da28fde9acd6', practice).
narrative_ontology:cs_interpretation_layer_present('d60acb15-db2c-48df-b9d4-da28fde9acd6').
narrative_ontology:cs_reading_relation('d60acb15-db2c-48df-b9d4-da28fde9acd6', software_source_status__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('d60acb15-db2c-48df-b9d4-da28fde9acd6', software_source_status__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('d60acb15-db2c-48df-b9d4-da28fde9acd6', software_source_status__utilitarian_hybrid_reading, influences).
narrative_ontology:cs_axiom('d60acb15-db2c-48df-b9d4-da28fde9acd6', foundational, open_development_superior_methodology).
narrative_ontology:cs_axiom_status(open_development_superior_methodology, holdable).
narrative_ontology:cs_axiom_grounding('d60acb15-db2c-48df-b9d4-da28fde9acd6', open_development_superior_methodology, empirically_contingent).
narrative_ontology:cs_axiom('d60acb15-db2c-48df-b9d4-da28fde9acd6', foundational, proprietary_software_legitimate).
narrative_ontology:cs_axiom_status(proprietary_software_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('d60acb15-db2c-48df-b9d4-da28fde9acd6', proprietary_software_legitimate, conventional).
narrative_ontology:cs_reference_frame('d60acb15-db2c-48df-b9d4-da28fde9acd6', empirical_development_practice).
narrative_ontology:cs_drift_state('d60acb15-db2c-48df-b9d4-da28fde9acd6', contemporary_platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d60acb15-db2c-48df-b9d4-da28fde9acd6', '').
narrative_ontology:cs_kernel_id(software_source_status__pragmatic_development_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, corporate_oss_consumers).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, enterprise_users).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, oss_foundations).
narrative_ontology:constraint_victim(software_source_status__pragmatic_development_reading, unpaid_maintainers).
narrative_ontology:constraint_victim(software_source_status__pragmatic_development_reading, proprietary_software_vendors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Consume open-source infrastructure at massive scale, fund selected foundations and high-profile projects, hire maintainers selectively, and shape governance through sponsorship and employment. Capture enormous value from freely available code while bearing only selective maintenance costs. Can fork, abandon, or replace projects if the constraint dissolves.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, corporate_oss_consumers, agenda_setter,
    institutional, generational, arbitrage, global).

% Administer license definitions (OSD), convene conferences, certify projects, and enforce community norms. Collect corporate sponsorships and depend on the ecosystem's continued legitimacy for institutional survival. Their authority rests on interpreting what counts as legitimate open development.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, oss_foundations, agenda_setter,
    institutional, generational, constrained, global).

% Deploy open-source software to reduce licensing and infrastructure costs. Benefit from network effects and shared maintenance. Have no direct obligation to contribute back and can switch to proprietary alternatives if the constraint weakens, though migration costs create moderate stickiness.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, enterprise_users, beneficiary,
    organized, biographical, mobile, global).

% Perform the bulk of code review, bug triage, security patching, and community management without compensation. Their professional identity and social status are fused with open-source contribution, making exit psychologically costly even when economically rational. Bear the direct labor cost that corporate consumers capture.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, unpaid_maintainers, payer,
    moderate, biographical, identity_locked, global).

% Lose market share, pricing power, and engineering talent to zero-cost open-source alternatives. Face social stigma in developer communities and licensing incompatibility that blocks integration. Network effects in the open-source ecosystem constrain their ability to compete on technical merit alone.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, proprietary_software_vendors, payer,
    powerful, biographical, constrained, global).

% Study software engineering outcomes, empirical productivity, and labor economics. Provide evidence that either supports or undermines the pragmatic claim that open development produces superior quality. Neither capture gains nor bear costs from the constraint's operation.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, academic_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__pragmatic_development_reading, corporate_oss_consumers).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables distributed software development across organizational boundaries by providing shared codebases, transparent peer review, and reusable infrastructure without centralized contracting or proprietary negotiation.
% TRANSFER_FUNCTION: Moves unpaid development labor, security review effort, and infrastructure maintenance costs from individual maintainers and small contributors to corporate consumers and enterprise users, justified by promises of quality outcomes and career advancement.
% ABSENT_VOICES: Burned-out maintainers who have exited silently, end-users who lack technical capacity to evaluate quality claims, and proprietary vendors from adjacent markets are excluded from governance and licensing debates. Their absence creates a false consensus that the arrangement is unanimously beneficial.
% DISAPPEARANCE_RATIONALE: If the pragmatic reading vanished overnight, the ideological justification for unpaid labor and permissive corporate consumption would weaken. Corporate OSS strategies would require renegotiation, development practices would shift toward explicit compensation or proprietary models, and the current software supply chain's cost structure would reorganize.
% FOUNDING_PROBLEM: Software development in the 1990s was dominated by proprietary silos with slow release cycles, opaque quality assurance, and high barriers to collaborative improvement.
% FOUNDING_PROBLEM_CORROBORATION: Early open-source advocates (Raymond, Torvalds) attest the problem from inside the movement. Critics from software engineering economics and labor studies attest that the founding problem has been superseded by a new problem â systematic extraction of unpaid labor â and that the arrangement persists because it benefits corporate consumers, not because it continues to solve the original quality problem.
narrative_ontology:disappearance_verdict(software_source_status__pragmatic_development_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__pragmatic_development_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__pragmatic_development_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_source_status__pragmatic_development_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__pragmatic_development_reading, 0.63, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__pragmatic_development_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_source_status__pragmatic_development_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_source_status__pragmatic_development_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.63) is moderate-high because the pragmatic reading justifies a massive transfer of unpaid labor to corporate beneficiaries under quality rhetoric. Suppression (0.48) is moderate: social norms, licensing incompatibilities, and platform network effects suppress proprietary alternatives and wage demands without reaching the coercive intensity of the freedom-imperative reading. Theater ratio (0.42) reflects partial performativity: 'many eyes' and 'peer review' claims are often invoked to justify extraction even when security outcomes and maintenance loads contradict the rhetoric. Accessibility collapse (0.55) captures that proprietary alternatives persist but are increasingly marginalized in infrastructure markets. Resistance (0.40) reflects maintainer burnout as passive resistance and proprietary vendor lobbying, though open dissent is muted by ideological dominance.
 *
 * PERSPECTIVAL GAP:
 *   The corporate consumer seat experiences this constraint as rope-like coordination: efficient resource sharing that reduces infrastructure costs. The unpaid maintainer seat experiences it as tangled-rope or snare-like extraction: uncompensated labor sustained by identity lock-in and career dependency. The proprietary vendor seat experiences it as active suppression of their business model. The academic observer seat sees the structural mismatch between the quality justification and the labor extraction outcome. The engine computes this divergence from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Corporate consumers and enterprise users are structural beneficiaries (low d) because the constraint subsidizes their infrastructure costs. OSS foundations are agenda-setters with intermediate d because they coordinate the mechanism but also capture institutional rents. Unpaid maintainers are structural targets with high d: their labor is extracted, and their identity_locked exit amplifies effective extraction beyond the base metric. Proprietary vendors are targets (high d) because the constraint structurally suppresses their market participation. Academic observers sit at analytical scope with neutral d.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resists pure-rope classification because the coordination function (shared development, peer review) is accompanied by asymmetric extraction (unpaid labor â corporate balance sheets) that requires active enforcement through licensing and social norms. It resists pure-snare classification because the coordination function is genuine and not merely cover: shared codebases do produce real efficiencies. The founding problem (proprietary silos hindering collaborative quality) is contested as still live; critics argue the problem has mutated into systematic labor extraction. This contested status is exactly what prevents mandatrophy mislabeling â the arrangement cannot be called pure coordination, nor pure extraction, because both functions are structurally present and entangled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    oss_quality_superiority,
    'Does open development methodology demonstrably produce higher-quality software than proprietary development, controlling for resource levels and project scale?',
    'Large-n comparative studies of defect rates, security incident response times, and long-term maintainability across matched proprietary and open-source projects.',
    'If the empirical claim fails, the pragmatic reading loses its foundational justification and collapses toward either the utilitarian hybrid or property rights readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oss_quality_superiority, empirical, 'Whether the instrumental quality claim grounding pragmatic open source is empirically sustained.').

omega_variable(
    corporate_capture_vs_community,
    'Is the contemporary open-source ecosystem primarily a community-driven coordination mechanism or a corporate labor-extraction mechanism?',
    'Quantitative analysis of contribution patterns (paid vs unpaid), governance control (corporate vs community), and value capture flows.',
    'If extraction dominates, the constraint''s classification shifts toward snare-like characteristics; if community coordination dominates, it remains rope-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corporate_capture_vs_community, empirical, 'Whether corporate capture has converted open coordination into extractive enclosure.').

omega_variable(
    practice_vs_expertise_authority,
    'Does the pragmatic reading''s authority derive from practitioner experience or from empirical software engineering research?',
    'Tracing the epistemic warrants used in major pragmatic open-source advocacy against peer-reviewed software engineering literature.',
    'If practice-based, the reading resists empirical refutation and behaves more like identity coordination; if research-based, it is vulnerable to contradictory studies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(practice_vs_expertise_authority, conceptual, 'Alternative authority grounding for the pragmatic reading''s empirical claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__pragmatic_development_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__pragmatic_development_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(soft_tr_t6, software_source_status__pragmatic_development_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement(soft_tr_t12, software_source_status__pragmatic_development_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(soft_tr_t18, software_source_status__pragmatic_development_reading, theater_ratio, 18, 0.36).
narrative_ontology:measurement(soft_tr_t24, software_source_status__pragmatic_development_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(soft_tr_t30, software_source_status__pragmatic_development_reading, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__pragmatic_development_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(soft_be_t6, software_source_status__pragmatic_development_reading, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(soft_be_t12, software_source_status__pragmatic_development_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(soft_be_t18, software_source_status__pragmatic_development_reading, base_extractiveness, 18, 0.56).
narrative_ontology:measurement(soft_be_t24, software_source_status__pragmatic_development_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(soft_be_t30, software_source_status__pragmatic_development_reading, base_extractiveness, 30, 0.63).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_source_status__pragmatic_development_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(soft_su_t6, software_source_status__pragmatic_development_reading, suppression_requirement, 6, 0.34).
narrative_ontology:measurement(soft_su_t12, software_source_status__pragmatic_development_reading, suppression_requirement, 12, 0.38).
narrative_ontology:measurement(soft_su_t18, software_source_status__pragmatic_development_reading, suppression_requirement, 18, 0.42).
narrative_ontology:measurement(soft_su_t24, software_source_status__pragmatic_development_reading, suppression_requirement, 24, 0.45).
narrative_ontology:measurement(soft_su_t30, software_source_status__pragmatic_development_reading, suppression_requirement, 30, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__pragmatic_development_reading, resource_allocation).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__property_rights_reading).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the software_source_status kernel, decomposed from the colloquial label 'open source' which conflates ethical, pragmatic, property-rights, and utilitarian claims. Each reading carries a distinct epsilon and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
