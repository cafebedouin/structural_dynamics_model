% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__creator_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: fair_use_four_factor_test__creator_centric_reading
 *   human_readable: Fair Use Four-Factor Test (Creator-Centric Reading)
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   This constraint represents the 'creator-centric' reading of the fair use
 *   four-factor test, where fair use is interpreted as a narrow exception to
 *   copyright's exclusive rights, primarily aimed at preserving creator
 *   incentives. This reading emphasizes the potential market harm to the
 *   original work and the commercial nature of the use, often leading to
 *   outcomes that favor copyright holders. This is one reading of the
 *   'fair_use_four_factor_test' kernel, alongside 'user_centric_reading' and
 *   'transformative_use_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__creator_centric_reading, 0.7).
domain_priors:suppression_score(fair_use_four_factor_test__creator_centric_reading, 0.65).
domain_priors:theater_ratio(fair_use_four_factor_test__creator_centric_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__creator_centric_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__creator_centric_reading, "Fair Use Four-Factor Test (Creator-Centric Reading)").
narrative_ontology:topic_domain(fair_use_four_factor_test__creator_centric_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__creator_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__creator_centric_reading, '73faac8b-7146-4347-99fb-62bc4b2a2017').
narrative_ontology:cs_kernel_codification('73faac8b-7146-4347-99fb-62bc4b2a2017', fixed_text).
narrative_ontology:cs_authority_grounding('73faac8b-7146-4347-99fb-62bc4b2a2017', lineage).
narrative_ontology:cs_interpretation_layer_present('73faac8b-7146-4347-99fb-62bc4b2a2017').
narrative_ontology:cs_reading_relation('73faac8b-7146-4347-99fb-62bc4b2a2017', fair_use_four_factor_test__user_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('73faac8b-7146-4347-99fb-62bc4b2a2017', fair_use_four_factor_test__transformative_use_reading, coexists_with).
narrative_ontology:cs_axiom('73faac8b-7146-4347-99fb-62bc4b2a2017', foundational, copyright_as_fundamental_property_right).
narrative_ontology:cs_axiom_status(copyright_as_fundamental_property_right, holdable).
narrative_ontology:cs_axiom_grounding('73faac8b-7146-4347-99fb-62bc4b2a2017', copyright_as_fundamental_property_right, deontological).
narrative_ontology:cs_axiom('73faac8b-7146-4347-99fb-62bc4b2a2017', foundational, market_harm_as_primary_fair_use_factor).
narrative_ontology:cs_axiom_status(market_harm_as_primary_fair_use_factor, holdable).
narrative_ontology:cs_axiom_grounding('73faac8b-7146-4347-99fb-62bc4b2a2017', market_harm_as_primary_fair_use_factor, conventional).
narrative_ontology:cs_reference_frame('73faac8b-7146-4347-99fb-62bc4b2a2017', original_copyright_incentive_framework).
narrative_ontology:cs_drift_state('73faac8b-7146-4347-99fb-62bc4b2a2017', contemporary_digital_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('73faac8b-7146-4347-99fb-62bc4b2a2017', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, copyright_holders).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, content_licensors).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, transformative_users).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, derivative_creators).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, public_domain_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Primary beneficiaries of strong copyright protection; they assert their exclusive rights and seek to maximize licensing revenue. This reading of fair use aligns with their interests by limiting exceptions.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, copyright_holders, agenda_setter,
    institutional, generational, mobile, global).

% Benefit from the ability to license copyrighted works for various uses, with fair use acting as a narrow carve-out that preserves their market. They support interpretations that prioritize market harm.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, content_licensors, beneficiary,
    organized, biographical, mobile, global).

% Individuals or entities who wish to use copyrighted material in new contexts (parody, commentary, education) but face legal risk and potential litigation under this narrow interpretation of fair use. Their creative output is chilled.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, transformative_users, payer,
    moderate, biographical, constrained, national).

% Artists, writers, and other creators who build upon existing works. This reading makes it harder and riskier for them to create new works without explicit permission, increasing their costs or forcing them to abandon projects.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, derivative_creators, payer,
    moderate, biographical, constrained, national).

% Groups arguing for a robust public domain and broader user rights. This reading of fair use actively works against their goals by strengthening private property rights over cultural commons. They are often excluded from the direct negotiation of fair use application.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, public_domain_advocates, excluded,
    organized, generational, trapped, global).

% Interpret and apply the four-factor test in specific cases. This reading reflects a judicial tendency to prioritize creator incentives and market protection, often leading to outcomes favorable to copyright holders.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Balances the exclusive rights of creators with the public's interest in using copyrighted works, providing a framework for courts to adjudicate disputes and guide creative reuse.
% TRANSFER_FUNCTION: Transfers potential revenue and control over derivative works from transformative users and derivative creators to copyright holders, by narrowly defining permissible unlicensed use.
% ABSENT_VOICES: The voices of the public domain and user rights advocates are often marginalized in the legal discourse that shapes this reading, which tends to prioritize commercial interests and established property rights.
% DISAPPEARANCE_RATIONALE: If the fair use test vanished, copyright holders would assert absolute control, leading to a dramatic chilling effect on all forms of creative reuse, commentary, and education. The cultural landscape would become far more restrictive, and the market for derivative works would collapse or become entirely permission-based.
% FOUNDING_PROBLEM: To provide a limited, flexible exception to copyright's exclusive rights, preventing copyright from stifling creativity and public discourse, while still incentivizing creators.
% FOUNDING_PROBLEM_CORROBORATION: Copyright holders and their legal representatives argue the problem of incentivizing creation remains live and requires strong protection. Transformative users and public domain advocates argue the problem has shifted to one of over-protection, with fair use failing to adequately serve its balancing function; legal scholars and cultural critics outside the benefiting parties corroborate this shift.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__creator_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__creator_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__creator_centric_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(fair_use_four_factor_test__creator_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__creator_centric_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.7) because this reading consistently favors copyright holders, effectively extracting value from potential transformative uses. Suppression (0.65) is also significant, as the threat of litigation and high legal costs actively discourages many derivative creators and transformative users. Theater ratio is low (0.2) because the legal process is genuinely engaged in adjudication, even if the interpretive bias is clear. The trend shows increasing extractiveness and suppression over time as this reading gained prominence, reflecting a hardening of copyright enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of copyright holders, this reading is a necessary 'rope' to protect their property and incentivize creation. From the perspective of transformative users, it operates as a 'snare' that stifles creativity and extracts value through legal threat. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright holders and content licensors are clear beneficiaries, as this reading maximizes their control and revenue. Transformative users and derivative creators are victims, bearing the costs of restricted use and legal risk. Public domain advocates are excluded, as their perspective is largely marginalized by this interpretation. Courts act as agenda-setters, applying this interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_interpretive_drift,
    'To what extent has judicial interpretation of the four factors drifted towards prioritizing market harm and commerciality over other factors, independent of legislative intent?',
    'Empirical analysis of court decisions over time, quantifying the weight given to each of the four factors in fair use rulings, particularly in cases involving transformative use.',
    'If significant drift is found, it would suggest that the ''creator-centric'' reading is a product of judicial activism rather than a faithful application of the statute, potentially strengthening arguments for legislative reform or alternative judicial interpretations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_interpretive_drift, empirical, 'Assessing the degree of judicial bias in fair use rulings.').

omega_variable(
    chilling_effect_quantification,
    'What is the quantifiable chilling effect of this creator-centric reading on the production of new transformative and derivative works?',
    'Surveys of creators, analysis of abandoned projects due to fair use concerns, and comparison of creative output in jurisdictions with different fair use interpretations.',
    'A high quantifiable chilling effect would undermine the ''incentive'' justification for this reading, suggesting it actively hinders cultural production rather than fostering it, potentially reclassifying the constraint as more extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(chilling_effect_quantification, empirical, 'Measuring the impact of fair use interpretation on creative output.').

omega_variable(
    conceptual_framing_of_fair_use,
    'Is fair use fundamentally a defense against infringement (creator-centric) or an affirmative right of users (user-centric)?',
    'Conceptual analysis of legal philosophy and historical legislative intent, combined with a comparative study of international copyright regimes that adopt different fundamental framings.',
    'If fair use is framed as an affirmative right, the ''creator-centric'' reading would be seen as structurally misaligned with the underlying purpose, leading to a re-evaluation of its legitimacy and classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conceptual_framing_of_fair_use, conceptual, 'The fundamental conceptual framing of fair use as a defense vs. a right.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__creator_centric_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1976, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 1976, 0.1).
narrative_ontology:measurement(fair_tr_t1990, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(fair_tr_t2000, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(fair_tr_t2010, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(fair_tr_t2024, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(fair_be_t1976, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 1976, 0.5).
narrative_ontology:measurement(fair_be_t1990, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(fair_be_t2000, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(fair_be_t2010, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(fair_be_t2024, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1976, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 1976, 0.5).
narrative_ontology:measurement(fair_su_t1990, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 1990, 0.58).
narrative_ontology:measurement(fair_su_t2000, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement(fair_su_t2010, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 2010, 0.64).
narrative_ontology:measurement(fair_su_t2024, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__creator_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, copyright_term_extension).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, digital_millennium_copyright_act).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'fair_use_four_factor_test' kernel. The 'creator_centric_reading' emphasizes property rights and creator incentives, while 'user_centric_reading' emphasizes public access, and 'transformative_use_reading' prioritizes new meaning. Each reading constitutes a distinct constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
