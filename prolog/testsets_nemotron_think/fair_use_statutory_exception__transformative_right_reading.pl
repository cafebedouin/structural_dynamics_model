% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__transformative_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_statutory_exception__transformative_right_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: fair_use_statutory_exception__transformative_right_reading
 *   human_readable: Fair Use as Transformative Right
 *   domain: intellectual_property/legal_interpretation
 *
 * SUMMARY:
 *   This constraint story models the fair use statutory exception (17 USC
 *   107) as read by the transformative_right_reading: fair use is a positive
 *   right enabling transformative reuse and cultural production, not merely
 *   an affirmative defense. Courts have a duty to facilitate innovation by
 *   recognizing transformative uses as fair even when licensing markets
 *   exist. The reading claims low extraction for transformative uses (the
 *   constraint enables them) and high extraction for substitutive uses (the
 *   constraint does not protect them), with licensing markets not dispositive
 *   and burden of proof shared. This is one reading of the contested kernel
 *   'fair_use_statutory_exception'; sibling readings are
 *   'market_licensing_reading' and 'narrow_defense_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__transformative_right_reading, 0.42).
domain_priors:suppression_score(fair_use_statutory_exception__transformative_right_reading, 0.22).
domain_priors:theater_ratio(fair_use_statutory_exception__transformative_right_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__transformative_right_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_statutory_exception__transformative_right_reading, "Fair Use as Transformative Right").
narrative_ontology:topic_domain(fair_use_statutory_exception__transformative_right_reading, "intellectual_property/legal_interpretation").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__transformative_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__transformative_right_reading, 'aa71c5b8-83bb-4310-a5c6-216a6c9b9d87').
narrative_ontology:cs_kernel_codification('aa71c5b8-83bb-4310-a5c6-216a6c9b9d87', formalized).
narrative_ontology:cs_authority_grounding('aa71c5b8-83bb-4310-a5c6-216a6c9b9d87', lineage).
narrative_ontology:cs_interpretation_layer_present('aa71c5b8-83bb-4310-a5c6-216a6c9b9d87').
narrative_ontology:cs_reading_relation('aa71c5b8-83bb-4310-a5c6-216a6c9b9d87', fair_use_statutory_exception__market_licensing_reading, coexists_with).
narrative_ontology:cs_reading_relation('aa71c5b8-83bb-4310-a5c6-216a6c9b9d87', fair_use_statutory_exception__narrow_defense_reading, coexists_with).
narrative_ontology:cs_axiom('aa71c5b8-83bb-4310-a5c6-216a6c9b9d87', foundational, transformative_use_constitutes_fair_use_right).
narrative_ontology:cs_axiom_status(transformative_use_constitutes_fair_use_right, holdable).
narrative_ontology:cs_axiom_grounding('aa71c5b8-83bb-4310-a5c6-216a6c9b9d87', transformative_use_constitutes_fair_use_right, conventional).
narrative_ontology:cs_axiom('aa71c5b8-83bb-4310-a5c6-216a6c9b9d87', foundational, licensing_markets_not_dispositive_for_transformative_use).
narrative_ontology:cs_axiom_status(licensing_markets_not_dispositive_for_transformative_use, holdable).
narrative_ontology:cs_axiom_grounding('aa71c5b8-83bb-4310-a5c6-216a6c9b9d87', licensing_markets_not_dispositive_for_transformative_use, conventional).
narrative_ontology:cs_axiom('aa71c5b8-83bb-4310-a5c6-216a6c9b9d87', secondary, burden_of_proof_shared_in_transformative_use_analysis).
narrative_ontology:cs_axiom_status(burden_of_proof_shared_in_transformative_use_analysis, holdable).
narrative_ontology:cs_axiom_grounding('aa71c5b8-83bb-4310-a5c6-216a6c9b9d87', burden_of_proof_shared_in_transformative_use_analysis, instrumental).
narrative_ontology:cs_reference_frame('aa71c5b8-83bb-4310-a5c6-216a6c9b9d87', statutory_fair_use_as_transformative_right).
narrative_ontology:cs_drift_state('aa71c5b8-83bb-4310-a5c6-216a6c9b9d87', post_google_oracle_2021, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aa71c5b8-83bb-4310-a5c6-216a6c9b9d87', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, transformative_creators).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, cultural_producers).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, public_domain).
narrative_ontology:constraint_victim(fair_use_statutory_exception__transformative_right_reading, copyright_holders).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__transformative_right_reading, transformative_use_promotes_progress).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__transformative_right_reading, cultural_production_requires_reuse).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__transformative_right_reading, permission_culture_chills_innovation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Artists, writers, musicians, filmmakers, and developers who build new works by transforming existing copyrighted material (parody, criticism, remix, sampling, computational analysis). They rely on fair use to create without clearing rights for every reference. Their exit option is self-censorship or licensing — both costly and chilling.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, transformative_creators, beneficiary,
    moderate, biographical, constrained, national).

% Institutions (museums, libraries, archives, educational institutions, documentary filmmakers, news organizations) that curate, preserve, and recontextualize cultural artifacts. They depend on fair use for exhibitions, digitization, teaching, and reporting. Exit means abandoning projects or paying prohibitive licensing fees.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, cultural_producers, beneficiary,
    organized, generational, constrained, national).

% The collective cultural commons that grows when transformative works enter circulation. Not an agent but a structural beneficiary: each transformative work that fair use permits enriches the raw material for future creators. Has no exit — it is the substrate that the constraint sustains or depletes.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, public_domain, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(fair_use_statutory_exception__transformative_right_reading, public_domain).

% Rights owners (individual authors, publishers, studios, record labels, software companies) who lose licensing revenue and control when courts classify a use as fair. They can lobby for legislative narrowing, pursue strategic litigation, or build walled gardens (DRM, platform terms) to circumvent fair use. Their exit is political and technological, not market-based.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, copyright_holders, payer,
    powerful, generational, arbitrage, global).

% Federal courts (especially the Supreme Court and Courts of Appeals) that articulate and apply the four-factor test. They set the operational boundary of the transformative right through precedent. Their decisions determine whether the constraint functions as a right or a narrow defense. They cannot exit the role; they can only drift in interpretation.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% Collective management organizations, stock agencies, clearance houses, and platform content-ID systems that monetize permissions for reuse. They are structurally excluded from the fair use determination — their business model depends on fair use being narrow. They would argue that every transformative use should be a licensed use.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, licensing_intermediaries, excluded,
    organized, biographical, trapped, global).

% Academics who theorize fair use as a right, a defense, or a market-failure mechanism. They supply the intellectual frameworks courts adopt or reject. They neither collect nor pay; they map the constraint's structure from outside.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables transformative reuse and cultural production by providing a legal permission to reuse copyrighted material for new expression, commentary, criticism, parody, and computational analysis — solving the coordination problem of how later creators access the cultural raw material that earlier creators have proprietary control over.
% TRANSFER_FUNCTION: Moves control over transformative uses from copyright holders (who would charge licensing fees or deny permission) to transformative creators (who incorporate existing works into new expression). Transfers cultural value from proprietary control to the public domain. Transfers risk of litigation from creators to courts (who must articulate the boundary).
% ABSENT_VOICES: Small creators without legal resources who self-censor rather than risk litigation; future creators whose works don't yet exist and thus cannot object; licensing intermediaries who would profit from controlling every transformative use but are not parties to fair use cases; the public domain itself, which has no standing to sue.
% DISAPPEARANCE_RATIONALE: If fair use as a transformative right vanished overnight, transformative creation would shift to a permission-only regime: parody, criticism, remix, sampling, text-and-data mining, and AI training would require licenses that copyright holders can deny or price prohibitively. Cultural production would reorganize around clearance culture — only the well-resourced or non-controversial would create transformatively.
% FOUNDING_PROBLEM: The Copyright Act of 1976 codified fair use (17 USC 107) but left its scope ambiguous. The founding problem was balancing author incentive (copyright's monopoly) with public access to culture for further creation — how to let later creators build on earlier works without destroying the market for the originals.
% FOUNDING_PROBLEM_CORROBORATION: House Report 94-1476 (1976) states fair use permits 'use of copyrighted material for purposes such as criticism, comment, news reporting, teaching, scholarship, or research' — legislative history supports transformative purpose. Supreme Court in Campbell v. Acuff-Rose (1994) unanimously affirmed transformative use as the 'heart of the fair use inquiry.' Google v. Oracle (2021) extended it to functional software interfaces. Copyright industry testimony (RIAA, MPAA, AAP) consistently argues the founding problem is piracy and that fair use should be a narrow defense — their corroboration is the opposing position.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__transformative_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__transformative_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__transformative_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fair_use_statutory_exception__transformative_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__transformative_right_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__transformative_right_reading_tests).
:- end_tests(fair_use_statutory_exception__transformative_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.42) reflects the reading's assessment that the constraint, properly applied, extracts moderately from copyright holders (who lose control over transformative uses) while enabling transformative creators. The 1976-1994 period shows higher extraction (0.65) because courts had not yet crystallized transformative use as the core factor (pre-Campbell). Post-Campbell (1994), extraction drops as the transformative right becomes operational. Theater ratio is low (0.18) — the four-factor test has real analytical bite, though recent drift toward market-licensing framing raises it slightly. Suppression is low (0.22) because fair use is a permission, not a prohibition; the constraint's persistence does not depend on coercing transformative creators. Resistance is high (0.68) because copyright holders actively litigate and lobby to narrow the transformative right. Accessibility collapse is moderate (0.58) — licensing alternatives exist but are costly and incomplete.
 *
 * PERSPECTIVAL GAP:
 *   From the transformative creator's seat, the constraint is a ROPE (genuine coordination enabling cultural production). From the copyright holder's seat, it is a SNARE (extraction of their property right without compensation). From the court's seat, it is a TANGLED ROPE (coordination function + asymmetric extraction requiring active judicial line-drawing). The engine computes this per-seat divergence from the structural data; the claimed_type 'tangled_rope' reflects the reading's own structural assessment that BOTH coordination and extraction are present.
 *
 * DIRECTIONALITY LOGIC:
 *   Transformative creators and cultural producers are beneficiaries (d near 0.0): the constraint subsidizes their creative activity by removing the need to clear rights for transformative uses. Copyright holders are payers (d near 1.0): they bear the cost of lost licensing revenue and control. Courts as agenda-setters sit near symmetric (d ~0.5): they administer the constraint but do not personally collect or pay. The public domain is a non-agent beneficiary — it accumulates value but has no directionality. Licensing intermediaries are excluded: their exclusion is structural (they profit from the constraint's narrowing). The engine derives d from these structural declarations plus exit options: copyright holders have arbitrage exit (political/technological), transformative creators have constrained exit (self-censor or license).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (balancing author incentive with public access for further creation) remains LIVE and CONTESTED. Copyright holders argue the problem is now piracy and that fair use has metastasized beyond its founding scope. Transformative advocates argue the problem is permission culture and that fair use must expand to match digital reality. The constraint has not atrophied — its function is actively contested. Mandatrophy is not resolved; the arrangement persists because the founding problem persists, but the reading's claim that courts MUST facilitate innovation is a normative demand, not a description of current practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the transformative_right_reading a distinct constraint from the market_licensing_reading and narrow_defense_reading, or are they observable-dependent measurements of a single fair use constraint?',
    'Apply the ε-invariance test: if measuring fair use by ''transformative uses'' yields low ε but measuring by ''substitutive uses'' yields high ε, and the structural data (beneficiaries, victims, enforcement) differ, then they are distinct constraints linked by network.affects_constraints.',
    'If distinct, each reading gets its own constraint story with its own ε, stakeholders, and classification. If one constraint, the ε variance signals internal heterogeneity requiring decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s readings instantiate separate constraints per ε-invariance.').

omega_variable(
    transformative_substitutive_boundary,
    'Where is the structural boundary between transformative use (low ε) and substitutive use (high ε) in this reading''s operationalization?',
    'Analyze court opinions post-Google v. Oracle: do they articulate a workable boundary, or does the transformative/substitutive distinction collapse into a market-harm analysis (converging with market_licensing_reading)?',
    'If the boundary is unstable, the reading''s claimed low ε for transformative uses is aspirational, not descriptive — actual extraction may be higher. This would shift measured extractiveness upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_substitutive_boundary, empirical, 'Operational stability of the transformative/substitutive distinction in case law.').

omega_variable(
    burden_of_proof_allocation,
    'Does the transformative_right_reading''s claim of ''shared burden of proof'' match actual litigation practice, or does the burden remain disproportionately on the defendant?',
    'Empirical study of fair use litigation outcomes: win rates for transformative use defendants vs. procedural posture (summary judgment vs. trial), and whether courts shift burden to plaintiff after prima facie transformative showing.',
    'If burden remains on defendant, the constraint''s effective extraction for transformative creators is higher than claimed — litigation cost becomes a suppression mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burden_of_proof_allocation, empirical, 'Whether procedural burden allocation matches the reading''s structural claim.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by transformative creators structural (litigation cost, chilling effect) or internalized (self-censorship from perceived risk)?',
    'Post-litigation surveys of creators: do they resume transformative work after winning, or does the experience permanently alter their creative calculus?',
    'If internalized, the constraint''s effective suppression is higher than the structural measure — the target carries the suppression after exit from any single case.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression for transformative creators.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__transformative_right_reading, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_use_transformative_right_tr_t0, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(fair_use_transformative_right_tr_t18, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 18, 0.18).
narrative_ontology:measurement(fair_use_transformative_right_tr_t30, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(fair_use_transformative_right_tr_t48, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 48, 0.18).

% Extraction over time
narrative_ontology:measurement(fair_use_transformative_right_be_t0, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(fair_use_transformative_right_be_t18, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 18, 0.48).
narrative_ontology:measurement(fair_use_transformative_right_be_t30, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(fair_use_transformative_right_be_t48, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 48, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(fair_use_transformative_right_su_t0, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(fair_use_transformative_right_su_t18, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 18, 0.22).
narrative_ontology:measurement(fair_use_transformative_right_su_t30, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 30, 0.2).
narrative_ontology:measurement(fair_use_transformative_right_su_t48, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 48, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__transformative_right_reading, information_standard).
narrative_ontology:boltzmann_floor_override(fair_use_statutory_exception__transformative_right_reading, 0.02).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception__market_licensing_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception__narrow_defense_reading).

% DUAL FORMULATION NOTE:
% This reading and its siblings decompose the colloquial label 'fair use' into structurally distinct constraints. The transformative_right_reading claims low ε for transformative uses; market_licensing_reading claims high ε where markets exist; narrow_defense_reading claims high ε universally. They share the statutory kernel (17 USC 107) but instantiate different constraints with different beneficiary/victim structures. Linked via affects_constraints for contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_statutory_exception__transformative_right_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
