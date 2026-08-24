% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__coordination_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: derivative_work_statutory_boundary__coordination_reading
 *   human_readable: Derivative Work Boundary - Coordination Reading
 *   domain: intellectual_property/technology_governance/information_economics
 *
 * SUMMARY:
 *   This constraint story captures the coordination reading of the statutory
 *   derivative work boundary (17 U.S.C. § 101): only fixed recastings
 *   substantially incorporating original expression constitute derivative
 *   works; transformative uses (parody, commentary, adaptation adding new
 *   expression/meaning) and intermediate uses (copying for non-expressive
 *   purposes like ML training, reverse engineering, indexing) fall outside
 *   the derivative work right. This reading operates as a low-extraction rope
 *   — it coordinates by providing a stable, predictable safe harbor that
 *   enables follow-on creativity and technological innovation without
 *   permission-seeking overhead. The claimed type (rope) and metrics (low ε,
 *   low suppression, low theater) are authored independently; the engine
 *   computes per-seat classifications from the structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__coordination_reading, 0.18).
domain_priors:suppression_score(derivative_work_statutory_boundary__coordination_reading, 0.12).
domain_priors:theater_ratio(derivative_work_statutory_boundary__coordination_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__coordination_reading, rope).
narrative_ontology:human_readable(derivative_work_statutory_boundary__coordination_reading, "Derivative Work Boundary - Coordination Reading").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__coordination_reading, "intellectual_property/technology_governance/information_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__coordination_reading, 'e0cd9fa3-a7a2-45f7-9959-62ac5af4dcc8').
narrative_ontology:cs_kernel_codification('e0cd9fa3-a7a2-45f7-9959-62ac5af4dcc8', fixed_text).
narrative_ontology:cs_authority_grounding('e0cd9fa3-a7a2-45f7-9959-62ac5af4dcc8', lineage).
narrative_ontology:cs_interpretation_layer_present('e0cd9fa3-a7a2-45f7-9959-62ac5af4dcc8').
narrative_ontology:cs_reading_relation('e0cd9fa3-a7a2-45f7-9959-62ac5af4dcc8', derivative_work_statutory_boundary__enclosure_reading, forecloses).
narrative_ontology:cs_reading_relation('e0cd9fa3-a7a2-45f7-9959-62ac5af4dcc8', derivative_work_statutory_boundary__hybrid_carveout_reading, coexists_with).
narrative_ontology:cs_axiom('e0cd9fa3-a7a2-45f7-9959-62ac5af4dcc8', foundational, transformative_use_excluded_from_derivative_work).
narrative_ontology:cs_axiom_status(transformative_use_excluded_from_derivative_work, holdable).
narrative_ontology:cs_axiom_grounding('e0cd9fa3-a7a2-45f7-9959-62ac5af4dcc8', transformative_use_excluded_from_derivative_work, conventional).
narrative_ontology:cs_axiom('e0cd9fa3-a7a2-45f7-9959-62ac5af4dcc8', foundational, intermediate_copying_permissible_for_non_expressive_purposes).
narrative_ontology:cs_axiom_status(intermediate_copying_permissible_for_non_expressive_purposes, holdable).
narrative_ontology:cs_axiom_grounding('e0cd9fa3-a7a2-45f7-9959-62ac5af4dcc8', intermediate_copying_permissible_for_non_expressive_purposes, conventional).
narrative_ontology:cs_reference_frame('e0cd9fa3-a7a2-45f7-9959-62ac5af4dcc8', statutory_text_17usc101).
narrative_ontology:cs_drift_state('e0cd9fa3-a7a2-45f7-9959-62ac5af4dcc8', generative_ai_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e0cd9fa3-a7a2-45f7-9959-62ac5af4dcc8', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, transformative_creators).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, ml_researchers).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, generative_ai_developers).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, public_domain).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__coordination_reading, transformative_use_doctrine).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__coordination_reading, fair_use_doctrine).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__coordination_reading, idea_expression_dichotomy).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__coordination_reading, intermediate_copying_permissible).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Artists, writers, and creators who build new works by transforming existing expression — parody, commentary, remix, adaptation. They rely on the statutory boundary to create without seeking permission or paying royalties for the transformative use itself. Their exit is mobile: they can choose transformative projects or work in jurisdictions with similar safe harbors.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, transformative_creators, beneficiary,
    organized, biographical, mobile, national).

% Academic and industry researchers training machine learning models on copyrighted corpora. They treat the ingestion of works for statistical pattern extraction as non-expressive, intermediate use outside derivative work scope. Their exit is mobile: research can shift to open corpora or jurisdictions with explicit text-and-data-mining exceptions.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, ml_researchers, beneficiary,
    organized, biographical, mobile, global).

% Companies building and deploying large-scale generative models (LLMs, image generators, code models). They structure their training pipelines around the premise that model training is non-infringing intermediate use. They also shape the regulatory conversation through lobbying and standard-setting. Their exit is arbitrage-grade: they can relocate training compute, restructure data pipelines, or jurisdictionalize entities to favorable regimes.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, generative_ai_developers, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__coordination_reading, generative_ai_developers, agenda_setter).

% Individual authors, publishers, studios, and collective management organizations holding copyright in underlying works. They set licensing terms for expressive uses but are constrained by this reading from claiming derivative work rights over transformative or intermediate uses. Their exit is constrained: they cannot opt out of the statutory boundary without legislative change, though they can pursue contractual workarounds (terms of service, technical protection measures).
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, copyright_holders, agenda_setter,
    institutional, generational, constrained, national).

% Federal courts interpreting the statutory definition of 'derivative work' (17 U.S.C. § 101) and applying the transformative use doctrine (Campbell v. Acuff-Rose, Google v. Oracle, Warhol Foundation v. Goldsmith). They do not collect rents from the boundary but their interpretations determine its operational contour. Their seat is analytical: they observe the constraint's operation and authoritatively resolve boundary disputes.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, courts, observer,
    institutional, generational, analytical, national).

% Collective management organizations (ASCAP, BMI, Copyright Clearance Center), stock photo agencies, and rights clearance services whose business models depend on licensing derivative work permissions. They are structurally excluded from the transformative/non-transformative boundary — their revenue depends on the boundary being narrower (enclosure reading) or commercially carved out (hybrid reading). Their exit is trapped: the coordination reading directly undermines their core licensing proposition.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, licensing_intermediaries, excluded,
    organized, biographical, trapped, national).

% The collective cultural commons enriched when transformative works enter public discourse without permission friction. This is a non-agent abstract beneficiary — the coordination reading expands the effective public domain by limiting derivative work capture of transformative reuse.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, public_domain, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(derivative_work_statutory_boundary__coordination_reading, public_domain).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__coordination_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear statutory boundary distinguishing transformative and intermediate uses (which are non-infringing) from derivative works (which require authorization), enabling generative technologies, creative reuse, and research without ex-ante licensing transaction costs.
% TRANSFER_FUNCTION: Moves the permission to use copyrighted expression in transformative and intermediate ways from copyright holders to transformative users, ML researchers, and AI developers — a transfer of legal privilege without monetary consideration, reducing transaction costs for follow-on innovation.
% ABSENT_VOICES: Licensing intermediaries and rights management organizations (collective management societies, stock agencies, clearance services) who would argue for a broader derivative work right encompassing transformative and intermediate uses. They are structurally excluded because their business model requires the boundary to be drawn at 'any use' (enclosure reading) or at 'commercial use' (hybrid reading).
% DISAPPEARANCE_RATIONALE: If the coordination reading vanished overnight, transformative creators would face licensing demands for parody and commentary; ML training on copyrighted corpora would become presumptively infringing; generative AI development would require negotiating millions of licenses or restricting training to public domain data — the entire generative technology ecosystem would reorganize around permission-based access.
% FOUNDING_PROBLEM: Legal uncertainty over the derivative work boundary was chilling transformative creativity (parody, criticism, adaptation) and technological innovation (reverse engineering, text-and-data mining, model training) by exposing follow-on creators to infringement liability for non-expressive uses of protected expression.
% FOUNDING_PROBLEM_CORROBORATION: Courts (Campbell v. Acuff-Rose, Google v. Oracle), technology policy scholars (Samuelson, Litman, Sag), and legislative history of the 1976 Copyright Act's fair use codification corroborate that the derivative work right was never intended to reach transformative or intermediate uses. The copyright holder lobby disputes this status, arguing the founding problem is solved by voluntary licensing markets.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(derivative_work_statutory_boundary__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__coordination_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__coordination_reading_tests).
:- end_tests(derivative_work_statutory_boundary__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the constraint primarily *removes* a potential extraction vector (derivative work licensing fees for transformative uses) rather than imposing one. Suppression is low (0.12) because the constraint's persistence does not depend on coercing transformative users — it depends on courts refusing to expand the derivative work right. Theater ratio is minimal (0.08) because the transformative use doctrine performs genuine coordination: it resolves genuine uncertainty about what uses require permission. Accessibility collapse is moderate (0.35) because while the boundary enables transformative uses, alternative coordination mechanisms (voluntary licensing, collective licensing) remain partially available. Resistance is low (0.22) because the primary resistance comes from copyright holders litigating boundary cases, not from transformative users resisting the constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the transformative_creator seat, the constraint is pure coordination — a reliable safe harbor. From the copyright_holder seat, the same constraint looks like an uncompensated taking of derivative work control. From the licensing_intermediary seat, it looks like regulatory capture by tech platforms. The engine computes these divergences from the declared roles, power, and exit options; the commentary does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Transformative creators, ML researchers, and generative AI developers are structural beneficiaries (d near 0.0): the constraint subsidizes their activity by removing a permission requirement. Copyright holders are agenda_setters with constrained exit (d ~0.6): they administer the underlying rights but cannot control transformative uses under this reading. Licensing intermediaries are excluded (d not computed — they are not governed by the constraint but displaced by it). Courts are analytical observers (d = 0.5 by definition). The public_domain non-agent beneficiary receives diffuse subsidy. The engine derives directionality from these structural positions plus exit options: generative_ai_developers' arbitrage-grade exit pulls their d toward beneficiary; licensing_intermediaries' trapped exit would pull toward target if they were governed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (uncertainty chilling transformative use) remains live — generative AI has expanded the frontier of 'intermediate use' beyond what Campbell or Google v. Oracle contemplated. The constraint has not atrophied; its coordination function has expanded. However, if courts adopt the enclosure reading (any use = derivative work), the mandate would outlive its function: the statutory text would become a snare extracting licensing fees for non-expressive uses. The coordination reading prevents this mandatrophy by anchoring the boundary in the statutory text's 'recasting, transforming, adapting' language.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the coordination_reading a faithful instantiation of the statutory kernel, or does it import policy preferences not in the text?',
    'Textualist vs. purposive interpretation of ''recasting, transforming, adapting'' in 17 U.S.C. § 101; legislative history of the 1976 Act''s derivative work definition.',
    'If the coordination reading imports policy, its ε may be higher than authored (it would be a constructed safe harbor, not a textual boundary). If textual, ε remains low and the reading is a genuine rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this reading''s low extraction is intrinsic to the statutory text or a judicial construction.').

omega_variable(
    sibling_foreclosure_delta,
    'How would adopting the enclosure_reading structurally transform the beneficiary/victim architecture?',
    'Counterfactual stakeholder mapping: under enclosure_reading, transformative_creators and ml_researchers become payers; copyright_holders and licensing_intermediaries become concentrated beneficiaries; generative_ai_developers face prohibitive licensing costs.',
    'Enclosure reading would reclassify the constraint from rope to snare (high extraction, active enforcement, identifiable victims). The foreclosure relation means no single legal framework can hold both readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_foreclosure_delta, conceptual, 'Structural delta between coordination and enclosure readings — the foreclosure mechanism.').

omega_variable(
    hybrid_carveout_boundary,
    'Where does the commercial/non-commercial line in the hybrid_carveout_reading fall for ML training and generative AI deployment?',
    'Case law on commercial fair use (Warhol Foundation v. Goldsmith); legislative proposals for AI training licensing regimes; industry practice on open vs. commercial model weights.',
    'If hybrid reading treats commercial ML training as requiring authorization but non-commercial research as free, generative_ai_developers become partial payers; the constraint becomes tangled_rope. If all AI deployment is commercial, hybrid converges toward enclosure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hybrid_carveout_boundary, empirical, 'Whether the hybrid reading''s commercial carveout captures generative AI value extraction.').

omega_variable(
    transformative_use_scope_creep,
    'Does the transformative use doctrine''s expansion to cover ML training represent doctrinal drift or faithful application?',
    'Comparative analysis: Google v. Oracle (API copying for interoperability = transformative) vs. Warhol (commercial licensing market substitution = not transformative); application to model training where output may substitute for training data.',
    'If drift, the coordination reading''s low ε may not hold for commercial generative AI — the constraint would show extraction accumulation (T17 trigger). If faithful, the rope classification is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_use_scope_creep, conceptual, 'Whether the coordination reading''s boundary is stable or drifting under generative AI pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__coordination_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dwsc_coordination_tr_t0, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(dwsc_coordination_tr_t0, observed).
narrative_ontology:measurement(dwsc_coordination_tr_t10, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement_basis(dwsc_coordination_tr_t10, observed).
narrative_ontology:measurement(dwsc_coordination_tr_t20, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement_basis(dwsc_coordination_tr_t20, observed).
narrative_ontology:measurement(dwsc_coordination_tr_t30, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 30, 0.08).
narrative_ontology:measurement_basis(dwsc_coordination_tr_t30, observed).
narrative_ontology:measurement(dwsc_coordination_tr_t40, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 40, 0.08).
narrative_ontology:measurement_basis(dwsc_coordination_tr_t40, observed).
narrative_ontology:measurement(dwsc_coordination_tr_t50, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 50, 0.08).
narrative_ontology:measurement_basis(dwsc_coordination_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(dwsc_coordination_be_t0, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(dwsc_coordination_be_t0, observed).
narrative_ontology:measurement(dwsc_coordination_be_t10, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement_basis(dwsc_coordination_be_t10, observed).
narrative_ontology:measurement(dwsc_coordination_be_t20, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement_basis(dwsc_coordination_be_t20, observed).
narrative_ontology:measurement(dwsc_coordination_be_t30, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 30, 0.19).
narrative_ontology:measurement_basis(dwsc_coordination_be_t30, observed).
narrative_ontology:measurement(dwsc_coordination_be_t40, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 40, 0.17).
narrative_ontology:measurement_basis(dwsc_coordination_be_t40, observed).
narrative_ontology:measurement(dwsc_coordination_be_t50, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 50, 0.18).
narrative_ontology:measurement_basis(dwsc_coordination_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(dwsc_coordination_su_t0, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(dwsc_coordination_su_t0, observed).
narrative_ontology:measurement(dwsc_coordination_su_t10, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 10, 0.15).
narrative_ontology:measurement_basis(dwsc_coordination_su_t10, observed).
narrative_ontology:measurement(dwsc_coordination_su_t20, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 20, 0.12).
narrative_ontology:measurement_basis(dwsc_coordination_su_t20, observed).
narrative_ontology:measurement(dwsc_coordination_su_t30, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 30, 0.11).
narrative_ontology:measurement_basis(dwsc_coordination_su_t30, observed).
narrative_ontology:measurement(dwsc_coordination_su_t40, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 40, 0.12).
narrative_ontology:measurement_basis(dwsc_coordination_su_t40, observed).
narrative_ontology:measurement(dwsc_coordination_su_t50, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 50, 0.12).
narrative_ontology:measurement_basis(dwsc_coordination_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__coordination_reading, information_standard).
narrative_ontology:boltzmann_floor_override(derivative_work_statutory_boundary__coordination_reading, 0.02).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, fair_use_doctrine).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, idea_expression_dichotomy).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, ai_training_copyright_exception).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, text_and_data_mining_exception).

% DUAL FORMULATION NOTE:
% The derivative_work_statutory_boundary kernel decomposes into three constraint stories: coordination_reading (this file, rope), enclosure_reading (snare), and hybrid_carveout_reading (tangled_rope). All three share the statutory text as kernel but instantiate different structural constraints with different ε values and beneficiary/victim architectures. This coordination_reading is the upstream constraint — its transformative use doctrine is cited as precedent in fair_use_doctrine and ai_training_copyright_exception.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
