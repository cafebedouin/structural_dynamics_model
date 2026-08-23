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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: fair_use_statutory_exception__transformative_right_reading
 *   human_readable: Fair Use Statutory Exception â Transformative Right Reading
 *   domain: legal/intellectual_property
 *
 * SUMMARY:
 *   Fair use under 17 U.S.C. Â§ 107 is a contested kernel. This constraint
 *   story instantiates the transformative_right_reading: the view that fair
 *   use exists to enable transformative reuse and cultural production, and
 *   that courts must facilitate innovation. The reading treats licensing
 *   markets as non-dispositive and shares the burden of proof between
 *   claimants. Structurally, the constraint coordinates downstream creativity
 *   while extracting licensing value from upstream copyright holders,
 *   requiring active judicial enforcement. The claim/metric independence is
 *   maintained: the reading claims a coordination function while the metrics
 *   describe the hybrid extraction-coordination structure.
 *
 * KEY AGENTS:
 *   - transformative_creators: Primary beneficiary (moderate/constrained) â gains freedom to reuse without licensing
 *   - commercial_copyright_holders: Primary target (powerful/constrained) â bears lost licensing revenue and exclusivity
 *   - federal_judiciary: Agenda setter (institutional/analytical) â administers the doctrinal test and boundary-setting
 *   - licensing_intermediaries: Secondary payer (organized/constrained) â loses transactional revenue and institutional role
 *   - legal_academics: Analytical observer â sees the full structural tension between property and innovation frames
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__transformative_right_reading, 0.48).
domain_priors:suppression_score(fair_use_statutory_exception__transformative_right_reading, 0.58).
domain_priors:theater_ratio(fair_use_statutory_exception__transformative_right_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__transformative_right_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_statutory_exception__transformative_right_reading, "Fair Use Statutory Exception â Transformative Right Reading").
narrative_ontology:topic_domain(fair_use_statutory_exception__transformative_right_reading, "legal/intellectual_property").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__transformative_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__transformative_right_reading, '11c6d2a6-33f8-41c2-999f-3e24f095f328').
narrative_ontology:cs_kernel_codification('11c6d2a6-33f8-41c2-999f-3e24f095f328', fixed_text).
narrative_ontology:cs_authority_grounding('11c6d2a6-33f8-41c2-999f-3e24f095f328', lineage).
narrative_ontology:cs_interpretation_layer_present('11c6d2a6-33f8-41c2-999f-3e24f095f328').
narrative_ontology:cs_reading_relation('11c6d2a6-33f8-41c2-999f-3e24f095f328', fair_use_statutory_exception__narrow_defense_reading, coexists_with).
narrative_ontology:cs_reading_relation('11c6d2a6-33f8-41c2-999f-3e24f095f328', fair_use_statutory_exception__market_licensing_reading, coexists_with).
narrative_ontology:cs_axiom('11c6d2a6-33f8-41c2-999f-3e24f095f328', foundational, transformative_reuse_as_cultural_imperative).
narrative_ontology:cs_axiom_status(transformative_reuse_as_cultural_imperative, holdable).
narrative_ontology:cs_axiom_grounding('11c6d2a6-33f8-41c2-999f-3e24f095f328', transformative_reuse_as_cultural_imperative, instrumental).
narrative_ontology:cs_axiom('11c6d2a6-33f8-41c2-999f-3e24f095f328', foundational, licensing_markets_non_dispositive).
narrative_ontology:cs_axiom_status(licensing_markets_non_dispositive, holdable).
narrative_ontology:cs_axiom_grounding('11c6d2a6-33f8-41c2-999f-3e24f095f328', licensing_markets_non_dispositive, conventional).
narrative_ontology:cs_reference_frame('11c6d2a6-33f8-41c2-999f-3e24f095f328', transformative_reuse_mandate).
narrative_ontology:cs_drift_state('11c6d2a6-33f8-41c2-999f-3e24f095f328', contemporary_digital_reproduction_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('11c6d2a6-33f8-41c2-999f-3e24f095f328', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, transformative_creators).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, informational_public).
narrative_ontology:constraint_victim(fair_use_statutory_exception__transformative_right_reading, commercial_copyright_holders).
narrative_ontology:constraint_victim(fair_use_statutory_exception__transformative_right_reading, licensing_intermediaries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Remix artists, documentarians, appropriation artists, and follow-on innovators who rely on unlicensed access to pre-existing works to create new cultural products. Without the transformative use doctrine, their practice would require prohibitively expensive clearances or face injunctive relief.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, transformative_creators, beneficiary,
    moderate, biographical, constrained, national).

% The diffuse public that benefits from access to transformative works, criticism, parody, and educational materials that would not be produced under a pure licensing regime. They do not directly create but consume and depend on the cultural ecosystem the doctrine enables.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, informational_public, beneficiary,
    powerless, generational, constrained, national).

% Major studios, publishers, and record labels that hold broad copyright portfolios. The transformative reading denies them licensing revenue for uses that courts classify as transformative, limiting their ability to monetize every downstream use of their works.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, commercial_copyright_holders, payer,
    powerful, biographical, constrained, global).

% Collective rights organizations and licensing agencies that facilitate transactional permission systems. The transformative reading bypasses their infrastructure for entire categories of use, reducing their transaction volume and justifying role.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, licensing_intermediaries, payer,
    organized, biographical, constrained, national).

% Federal courts that interpret and apply the four-factor fair use test. Under the transformative reading, they must weight the first factor heavily and facilitate innovation, effectively setting the boundary between permissible transformative reuse and infringing substitution.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Copyright scholars and law professors who analyze the doctrinal evolution, provide amicus briefs, and train judges and practitioners. They observe the structural tension between property-based and innovation-based copyright frameworks without direct financial stake in the outcome.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, legal_academics, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables downstream creativity and cumulative innovation by reducing transaction costs and permission-seeking barriers for creators who build on existing works.
% TRANSFER_FUNCTION: Transfers the right to control and monetize transformative uses from commercial copyright holders to subsequent creators and the reading public; moves value from licensed transactional channels to unlicensed creative production.
% ABSENT_VOICES: Individual authors who might prefer selective licensing; foreign rights holders operating under moral rights regimes that reject broad fair use; small copyright holders whose works are appropriated by well-funded transformative entities without compensation.
% DISAPPEARANCE_RATIONALE: If the transformative fair use reading vanished, courts would revert to market-substitution analysis; documentary film, remix culture, appropriation art, and algorithmic training would face comprehensive licensing requirements; the information economy would reorganize around permission-based clearance cultures.
% FOUNDING_PROBLEM: Absolute copyright control would create a permission culture where every follow-on use requires a license, stifling criticism, parody, and cumulative innovation.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and innovation economists outside the direct beneficiary set attest that copyright expansion created the need for a robust exception; copyright industry associations contest this, arguing that voluntary licensing markets could accommodate transformative reuse without doctrinal exemption.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__transformative_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__transformative_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__transformative_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fair_use_statutory_exception__transformative_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__transformative_right_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__transformative_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_statutory_exception__transformative_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_statutory_exception__transformative_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48) because the constraint transfers significant licensing value from copyright holders to transformative users, but only in the domain of transformative uses; it does not nullify copyright wholesale. Suppression is substantial (0.58) because the constraint actively suppresses the alternative of a pure licensing market through judicial dismissal of claims that fall on the transformative side of the line. Theater ratio is moderate (0.35) because fair use analysis has developed elaborate four-factor tests that are partly substantive and partly performative. Accessibility collapse is 0.60 because once the transformative reading is accepted, the licensing alternative collapses for transformative uses. Resistance is high (0.68) because copyright industries vigorously resist this reading through litigation, lobbying, and international pressure.
 *
 * PERSPECTIVAL GAP:
 *   The copyright holder seat experiences the constraint as extraction of their statutory entitlement to control derivative markets; the transformative creator seat experiences it as a necessary coordination mechanism enabling their practice. The judiciary seat experiences it as an interpretive mandate without direct financial stake. The engine should compute these differently: copyright holders as high-d targets, creators as low-d beneficiaries, courts near symmetric.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright holders and licensing intermediaries are targets because the constraint directly limits their exclusivity and revenue streams. Transformative creators and the public are beneficiaries because the constraint subsidizes their creative activity by removing licensing friction. The federal judiciary sits near symmetric because it administers the constraint without direct financial stake, though its institutional authority is bound up in the reading's legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by distinguishing the coordination function (enabling follow-on innovation) from the extraction function (denying licensing revenue to holders). Without this distinction, a pure property-rights reading would label all fair use as pure extraction from copyright holders; a pure public-domain reading would label it as costless coordination. The transformative reading correctly identifies the hybrid structure: coordinative for downstream creativity and extractive for upstream control. The founding problem (permission culture stifling innovation) is contested but partially corroborated, suggesting the constraint is evolving rather than atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the fair use exception a narrow defensive carve-out from property rights, a market-failure corrective, or an affirmative right enabling transformative cultural production?',
    'Supreme Court precedent consolidation or legislative revision clarifying the statutory purpose and factor-one weighting.',
    'If resolved as narrow defense, effective extraction shifts toward creators and users; if resolved as transformative right, the current reading stabilizes and the hybrid coordination-extraction structure is entrenched.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Contested kernel ambiguity over which reading governs fair use.').

omega_variable(
    market_harm_vs_transformative_purpose,
    'Does the fourth factor (market harm) or the first factor (transformative purpose) properly dominate when they conflict in fair use analysis?',
    'Empirical study of innovation and licensing market efficacy in transformative use categories; or definitive Supreme Court hierarchy-of-factors ruling.',
    'If market harm dominates, the constraint shifts toward market_licensing_reading with higher extraction from transformative creators; if transformative purpose dominates, the current reading stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_harm_vs_transformative_purpose, empirical, 'Hierarchy of fair use factors underdetermines classification.').

omega_variable(
    fair_use_nature_as_limitation,
    'Is fair use an inherent structural limit on copyright''s scope, or a legislatively granted exception that could be revoked?',
    'Constitutional challenge or legislative history analysis establishing whether the First Amendment or copyright clause requires fair use.',
    'If inherent limitation, classification tends toward rope-like immunity from revision; if revocable exception, classification remains tangled_rope with contingent enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fair_use_nature_as_limitation, conceptual, 'Whether fair use is structurally internal or external to copyright.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__transformative_right_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_use_transformative_tr_t0, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fair_use_transformative_tr_t5, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(fair_use_transformative_tr_t10, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(fair_use_transformative_tr_t15, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement(fair_use_transformative_tr_t20, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(fair_use_transformative_tr_t25, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 25, 0.32).
narrative_ontology:measurement(fair_use_transformative_tr_t30, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 30, 0.35).

% Extraction over time
narrative_ontology:measurement(fair_use_transformative_be_t0, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(fair_use_transformative_be_t5, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(fair_use_transformative_be_t10, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(fair_use_transformative_be_t15, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 15, 0.38).
narrative_ontology:measurement(fair_use_transformative_be_t20, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(fair_use_transformative_be_t25, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 25, 0.45).
narrative_ontology:measurement(fair_use_transformative_be_t30, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 30, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(fair_use_transformative_su_t0, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(fair_use_transformative_su_t5, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(fair_use_transformative_su_t10, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(fair_use_transformative_su_t15, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 15, 0.48).
narrative_ontology:measurement(fair_use_transformative_su_t20, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 20, 0.53).
narrative_ontology:measurement(fair_use_transformative_su_t25, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement(fair_use_transformative_su_t30, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__transformative_right_reading, resource_allocation).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception__narrow_defense_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception__market_licensing_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel fair_use_statutory_exception. The kernel decomposes into three structurally distinct constraints because the epsilon values and stakeholder directionalities differ across readings. This reading treats fair use as an innovation-facilitating coordination mechanism with moderate extraction from copyright holders; the narrow defense reading treats it as a minimal carve-out with negligible extraction from holders but high extraction from users; the market licensing reading treats it as a market-failure device with extraction contingent on licensing feasibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
