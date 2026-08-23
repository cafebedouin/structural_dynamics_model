% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__creator_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_four_factor_test__creator_centric_reading, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: fair_use_four_factor_test__creator_centric_reading
 *   human_readable: Fair Use Four-Factor Test (Creator-Centric Reading)
 *   domain: legal/intellectual_property
 *
 * SUMMARY:
 *   This constraint story captures the creator-centric reading of the U.S.
 *   fair use four-factor test (17 U.S.C. Â§ 107). Under this reading, fair
 *   use is a narrow exception to a default property rule, with the four
 *   factors weighed primarily to preserve creator incentives by minimizing
 *   uncompensated uses. Rights holders capture the gains through expanded
 *   licensing control, while transformative users and public-domain-dependent
 *   creators bear the costs of litigation uncertainty, chilling effects, and
 *   licensing friction. The constraint is structurally a tangled rope: it
 *   coordinates cultural production through the incentive mechanism, but
 *   asymmetrically extracts from transformative and access-dependent users.
 *   The engine will compute per-seat classifications; the rights-holder seat
 *   may compute toward rope/coordination, while the transformative-user seat
 *   computes toward snare/extraction.
 *
 * KEY AGENTS:
 *   - rights_holders: Primary beneficiary (powerful/global) â captures licensing surplus and blocking power over derivative markets.
 *   - federal_judiciary: Agenda setter (institutional/national) â interprets and enforces the four-factor test with market harm as the doctrinal north star.
 *   - transformative_users: Primary target (moderate/national) â bears litigation risk, statutory damages exposure, and chilling effect on derivative works.
 *   - public_domain_dependents: Secondary target (powerless/national) â loses access to raw material for new creation as the public domain shrinks and licensing walls rise.
 *   - copyright_scholars: Analytical observer (analytical/national) â documents empirical and doctrinal gaps between incentive theory and observed creative output.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__creator_centric_reading, 0.72).
domain_priors:suppression_score(fair_use_four_factor_test__creator_centric_reading, 0.68).
domain_priors:theater_ratio(fair_use_four_factor_test__creator_centric_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__creator_centric_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__creator_centric_reading, "Fair Use Four-Factor Test (Creator-Centric Reading)").
narrative_ontology:topic_domain(fair_use_four_factor_test__creator_centric_reading, "legal/intellectual_property").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__creator_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__creator_centric_reading, 'b58482e3-6bf7-4146-bea8-c793bef86ece').
narrative_ontology:cs_kernel_codification('b58482e3-6bf7-4146-bea8-c793bef86ece', formalized).
narrative_ontology:cs_authority_grounding('b58482e3-6bf7-4146-bea8-c793bef86ece', lineage).
narrative_ontology:cs_interpretation_layer_present('b58482e3-6bf7-4146-bea8-c793bef86ece').
narrative_ontology:cs_reading_relation('b58482e3-6bf7-4146-bea8-c793bef86ece', fair_use_four_factor_test__user_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('b58482e3-6bf7-4146-bea8-c793bef86ece', fair_use_four_factor_test__transformative_use_reading, influences).
narrative_ontology:cs_axiom('b58482e3-6bf7-4146-bea8-c793bef86ece', foundational, copyright_as_property_default).
narrative_ontology:cs_axiom_status(copyright_as_property_default, holdable).
narrative_ontology:cs_axiom_grounding('b58482e3-6bf7-4146-bea8-c793bef86ece', copyright_as_property_default, instrumental).
narrative_ontology:cs_axiom('b58482e3-6bf7-4146-bea8-c793bef86ece', secondary, market_harm_as_north_star).
narrative_ontology:cs_axiom_status(market_harm_as_north_star, holdable).
narrative_ontology:cs_axiom_grounding('b58482e3-6bf7-4146-bea8-c793bef86ece', market_harm_as_north_star, conventional).
narrative_ontology:cs_reference_frame('b58482e3-6bf7-4146-bea8-c793bef86ece', classical_copyright_incentives).
narrative_ontology:cs_drift_state('b58482e3-6bf7-4146-bea8-c793bef86ece', digital_reproduction_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b58482e3-6bf7-4146-bea8-c793bef86ece', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, rights_holders).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, transformative_users).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, public_domain_dependents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold copyrights in expressive works and benefit from licensing revenue and the ability to block unauthorized derivative uses. They enforce rights through litigation and DMCA takedowns, and the narrow fair use reading maximizes their licensing leverage and control over secondary markets.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, rights_holders, beneficiary,
    powerful, generational, arbitrage, global).

% Federal courts applying the four-factor test under 17 U.S.C. Â§ 107. They set interpretive boundaries by weighing the statutory factors with market harm as a central concern, often requiring licenses for commercial or derivative uses, thereby enforcing a property-default framework.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Artists, remixers, documentarians, and commentators who build on existing works without licensing. They face uncertainty, litigation risk, and statutory damages if a court finds their use unfair, forcing them to seek permission, pay licenses, or abandon projects.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, transformative_users, payer,
    moderate, biographical, constrained, national).

% Educators, archivists, and future creators who rely on works entering the public domain or broad fair use access to build new culture. Term extension and narrow fair use shrink the public domain and raise licensing costs, extracting raw material from them.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, public_domain_dependents, payer,
    powerless, biographical, constrained, national).

% Academics studying intellectual property law who observe the divergence between the incentive theory and empirical creative output, documenting chilling effects, rent concentration in large holders, and the doctrinal drift of the four-factor test.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, copyright_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_four_factor_test__creator_centric_reading, rights_holders).
narrative_ontology:fixing_cost_class(fair_use_four_factor_test__creator_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a predictable legal environment where creators can monetize expressive works by restricting unauthorized copying and derivative uses, preserving economic incentives for cultural production through a default property rule with a narrow safety valve.
% TRANSFER_FUNCTION: Moves economic surplus and expressive capacity from unauthorized and transformative users to rights holders by requiring licenses for uses that fall outside a narrow exception, and moves access to cultural raw materials from the public to rights holders through expanded control.
% ABSENT_VOICES: Transformative users and public access advocates appear in litigation but are systematically disadvantaged by litigation cost asymmetry and the burden of proof; future creators who would build on locked-up works and the dead whose estates control public domain entry have no seat at the table.
% DISAPPEARANCE_RATIONALE: If the narrow fair use constraint disappeared, transformative users would operate without licensing friction, derivative and remix works would proliferate, rights holders would lose licensing revenue from borderline uses, and the ecosystem of cultural production would reorganize around broader access and lower transaction costs.
% FOUNDING_PROBLEM: The public goods problem of cultural creation: without exclusive rights and limited exceptions, copyists could free-ride on creators' effort and investment, potentially undermining the incentive to produce expressive works in the first place.
% FOUNDING_PROBLEM_CORROBORATION: Rights holders and the Copyright Office attest the problem remains live. Empirical legal scholars and economists outside the beneficiary set contest the magnitude of the incentive effect, citing robust creative output in low-enforcement environments, the chilling effect of overbroad rights, and the concentration of benefits in large media conglomerates rather than marginal creators.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__creator_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__creator_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__creator_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fair_use_four_factor_test__creator_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__creator_centric_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__creator_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_four_factor_test__creator_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_four_factor_test__creator_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the narrow reading forces transformative users into licensing markets or silence, transferring surplus to rights holders. Suppression (0.68) is high because statutory damages, automatic injunctions, and DMCA takedowns actively suppress unauthorized uses. Theater_ratio (0.25) is modest: the four-factor test is a real interpretive framework, not pure theater, but its application in this reading follows predictable pro-rights-holder patterns. Accessibility_collapse (0.60) reflects that alternatives (relying on fair use without counsel) collapse once users understand litigation risk. Resistance (0.55) captures sustained pushback from documentary filmmakers, remix artists, and public-interest litigants. The measurement series run on a shared time grid aligned to the interval.
 *
 * PERSPECTIVAL GAP:
 *   The rights-holder seat experiences the constraint as a necessary coordination device that prevents free-riding and sustains investment in cultural goods; the engine may compute this seat as closer to rope. The transformative-user seat experiences the same legal test as an extractive snare that forces them to license uses that should be free; the engine will compute high effective extraction for this seat due to high base extractiveness, constrained exit, and moderate power. The divergence is structural and intended.
 *
 * DIRECTIONALITY LOGIC:
 *   Rights holders are declared beneficiaries: they collect economic rents and blocking power from the narrow interpretation, placing them near the beneficiary end of the directionality spectrum. The federal judiciary is the agenda setter; its authority is constituted by the interpretation layer rather than rent collection, sitting near symmetric but with institutional insulation. Transformative_users and public_domain_dependents are declared victims (role: payer): they bear costs through licensing friction, litigation risk, and forgone expression, placing them near the full-target end. Their exit options are constrained because statutory damages and the subjective four-factor test make unauthorized use prohibitively risky.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy mislabeling by acknowledging the genuine coordination functionâthe public-goods problem in cultural production is realâwhile insisting that the current arrangement asymmetrically extracts from the payer seats. If the founding problem (incentive shortfall) were dead, the constraint would be a piton or snare; because the problem remains contested and the coordination function is partially operative, tangled_rope is the structurally accurate claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incentive_effect_empirical_basis,
    'Does strong copyright with narrow fair use actually increase the quantity and quality of cultural production, or does it primarily transfer rents without incentivizing marginal creation?',
    'Cross-national econometric studies comparing creative output in jurisdictions with varying fair use breadth; natural experiments from copyright term changes and doctrinal shifts.',
    'If the incentive effect is weak or negative, the coordination story collapses toward pure extraction, reclassifying the rights-holder seat and potentially shifting the whole constraint toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incentive_effect_empirical_basis, empirical, 'Empirical basis for the incentive theory underlying the creator-centric reading').

omega_variable(
    market_harm_vs_transformation_boundary,
    'Where is the line between a transformative use that creates a new market and one that merely supersedes the original in a licensing market?',
    'Detailed economic substitution analysis in high-profile fair use litigation; amicus briefs from economists addressing market definition.',
    'If courts systematically classify new markets as potential licensing markets for the original, the constraint''s extraction is higher than its coordination; if transformativeness is allowed to define its own market, the creator-centric reading overreaches.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_harm_vs_transformation_boundary, conceptual, 'Conceptual ambiguity in market harm analysis for transformative works').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__creator_centric_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t0, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(fair_tr_t9, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 9, 0.15).
narrative_ontology:measurement(fair_tr_t18, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 18, 0.18).
narrative_ontology:measurement(fair_tr_t27, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 27, 0.22).
narrative_ontology:measurement(fair_tr_t36, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 36, 0.24).
narrative_ontology:measurement(fair_tr_t45, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 45, 0.25).

% Extraction over time
narrative_ontology:measurement(fair_be_t0, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fair_be_t9, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 9, 0.5).
narrative_ontology:measurement(fair_be_t18, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 18, 0.58).
narrative_ontology:measurement(fair_be_t27, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 27, 0.65).
narrative_ontology:measurement(fair_be_t36, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 36, 0.7).
narrative_ontology:measurement(fair_be_t45, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 45, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t0, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(fair_su_t9, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 9, 0.55).
narrative_ontology:measurement(fair_su_t18, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 18, 0.62).
narrative_ontology:measurement(fair_su_t27, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 27, 0.68).
narrative_ontology:measurement(fair_su_t36, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 36, 0.69).
narrative_ontology:measurement(fair_su_t45, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 45, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__creator_centric_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
