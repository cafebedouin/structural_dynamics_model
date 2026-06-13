% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__transformative_use_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_four_factor_test__transformative_use_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fair_use_four_factor_test__transformative_use_reading
 *   human_readable: Fair Use Four-Factor Test: Transformative Use Dominance Reading
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   This constraint represents the 'transformative use' reading of the fair
 *   use doctrine in US copyright law, where the addition of new meaning or
 *   message to a copyrighted work is given significant weight in the
 *   four-factor balancing test, often subordinating market harm to the
 *   original work. This reading has been influential in shaping digital
 *   cultural production and the rise of remix culture, but it also creates
 *   tension with original content creators who see their control and
 *   potential revenue diminished.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__transformative_use_reading, 0.45).
domain_priors:suppression_score(fair_use_four_factor_test__transformative_use_reading, 0.3).
domain_priors:theater_ratio(fair_use_four_factor_test__transformative_use_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__transformative_use_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__transformative_use_reading, "Fair Use Four-Factor Test: Transformative Use Dominance Reading").
narrative_ontology:topic_domain(fair_use_four_factor_test__transformative_use_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__transformative_use_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__transformative_use_reading, '835d38e2-4b21-453b-9bc1-ade9e66fe204').
narrative_ontology:cs_kernel_codification('835d38e2-4b21-453b-9bc1-ade9e66fe204', formalized).
narrative_ontology:cs_authority_grounding('835d38e2-4b21-453b-9bc1-ade9e66fe204', lineage).
narrative_ontology:cs_interpretation_layer_present('835d38e2-4b21-453b-9bc1-ade9e66fe204').
narrative_ontology:cs_reading_relation('835d38e2-4b21-453b-9bc1-ade9e66fe204', fair_use_four_factor_test__creator_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('835d38e2-4b21-453b-9bc1-ade9e66fe204', fair_use_four_factor_test__user_centric_reading, coexists_with).
narrative_ontology:cs_axiom('835d38e2-4b21-453b-9bc1-ade9e66fe204', foundational, new_meaning_trumps_market_harm).
narrative_ontology:cs_axiom_status(new_meaning_trumps_market_harm, holdable).
narrative_ontology:cs_axiom_grounding('835d38e2-4b21-453b-9bc1-ade9e66fe204', new_meaning_trumps_market_harm, conventional).
narrative_ontology:cs_axiom('835d38e2-4b21-453b-9bc1-ade9e66fe204', secondary, public_benefit_from_transformation).
narrative_ontology:cs_axiom_status(public_benefit_from_transformation, holdable).
narrative_ontology:cs_axiom_grounding('835d38e2-4b21-453b-9bc1-ade9e66fe204', public_benefit_from_transformation, instrumental).
narrative_ontology:cs_reference_frame('835d38e2-4b21-453b-9bc1-ade9e66fe204', flexible_balancing_test).
narrative_ontology:cs_drift_state('835d38e2-4b21-453b-9bc1-ade9e66fe204', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('835d38e2-4b21-453b-9bc1-ade9e66fe204', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, remix_artists_and_creators).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, user_generated_content_platforms).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, original_content_creators).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, traditional_publishers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the broad interpretation of fair use, allowing them to create new works by building upon existing copyrighted material without needing explicit permission. Their ability to operate depends heavily on this reading.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, remix_artists_and_creators, beneficiary,
    moderate, biographical, constrained, global).

% Benefit from reduced liability and increased content volume, as users are emboldened to upload transformative works. This reading supports their business model by lowering legal risk associated with hosting user-generated content.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, user_generated_content_platforms, beneficiary,
    institutional, generational, arbitrage, global).

% Bear the cost of diminished control over their copyrighted works and potential market displacement, as transformative uses may compete with their original or licensed derivative markets. They often litigate to defend their rights.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, original_content_creators, payer,
    powerful, biographical, constrained, global).

% Experience erosion of their licensing revenue streams and control over derivative markets. They must adapt their business models or engage in costly litigation to challenge uses they deem non-fair.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, traditional_publishers, payer,
    institutional, generational, constrained, global).

% Interpret and apply the fair use doctrine, with a strong emphasis on transformativeness. Their rulings shape the boundaries of what constitutes fair use, effectively setting the agenda for cultural production and intellectual property rights.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, courts_and_judges, agenda_setter,
    institutional, generational, analytical, national).

% Analyze and debate the implications of the transformative use doctrine, influencing legal discourse and potential legislative changes. They provide the intellectual framework for challenging or defending this reading.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, legal_scholars_and_advocates, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Balances the rights of copyright holders with the public interest in promoting creativity and access to knowledge, by providing a legal safe harbor for certain unauthorized uses of copyrighted material.
% TRANSFER_FUNCTION: Transfers a degree of control and potential economic value from original content creators to those who create new, transformative works, and to platforms that host such works.
% ABSENT_VOICES: Small, independent original creators who lack the resources to litigate against transformative uses that they believe infringe on their rights, or who are not adequately compensated for the use of their work. Their voices are often drowned out by larger platforms and well-funded remix artists.
% DISAPPEARANCE_RATIONALE: If the transformative use reading of fair use vanished, remix culture would largely cease to exist in its current form, UGC platforms would face immense legal liability, and the balance of power in cultural production would shift dramatically back towards original copyright holders, stifling many forms of creative expression.
% FOUNDING_PROBLEM: The original copyright framework, designed for a scarcity economy of physical copies, struggled to accommodate new forms of creative expression (e.g., parody, commentary, education) that built upon existing works, leading to potential stifling of creativity and innovation.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, cultural critics, and many artists attest that the problem of balancing creator rights with new forms of creativity remains live, especially with the rise of digital technologies and AI-generated content. Courts continue to grapple with these issues, indicating an ongoing need for a flexible doctrine like fair use.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__transformative_use_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__transformative_use_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__transformative_use_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(fair_use_four_factor_test__transformative_use_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__transformative_use_reading_tests).
:- end_tests(fair_use_four_factor_test__transformative_use_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because while it enables new creation, it also shifts value away from original creators. Suppression (0.3) is relatively low, as the constraint aims to enable, rather than restrict, certain uses, but it still requires active enforcement through litigation to define its boundaries. The claimed type is 'tangled_rope' because it genuinely coordinates cultural production while simultaneously extracting value from original creators for the benefit of transformative users and platforms.
 *
 * PERSPECTIVAL GAP:
 *   Original content creators perceive this reading as highly extractive, eroding their property rights. Transformative artists and UGC platforms, however, view it as a necessary coordination mechanism that fosters innovation and free expression. Courts, as agenda-setters, navigate these competing perspectives, with recent trends favoring transformativeness.
 *
 * DIRECTIONALITY LOGIC:
 *   Remix artists and UGC platforms are clear beneficiaries, as this reading directly enables their activities and reduces their legal exposure. Original content creators and traditional publishers are victims, as their control and potential revenue are diminished. Courts act as agenda-setters, interpreting and enforcing the doctrine. The directionality for beneficiaries is low (subsidized), and for victims, it is high (extracted from).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to foster creativity remains live, but its application has shifted significantly. The 'transformative use' reading prevents mislabeling genuine creative coordination as pure extraction by acknowledging the public benefit, while still recognizing the asymmetric extraction from original creators. The ongoing contestation over its application suggests it is not a 'piton' but an actively negotiated 'tangled_rope'.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformative_threshold_ambiguity,
    'What is the precise threshold for ''transformativeness'' that justifies subordinating market harm, and is this threshold consistently applied across different media and contexts?',
    'A series of landmark court cases that establish clearer, more objective criteria for transformativeness, or legislative action to codify specific examples and exclusions.',
    'A clearer threshold would reduce uncertainty for both creators and users, potentially lowering litigation costs and making the constraint''s application more predictable. If the threshold is found to be inconsistently applied, it would highlight a lack of clear coordination and increase perceived extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_threshold_ambiguity, conceptual, 'Ambiguity in defining and applying the ''transformativeness'' criterion.').

omega_variable(
    market_harm_subordination_justification,
    'Is the subordination of market harm in transformative use cases genuinely promoting public benefit, or is it primarily subsidizing new forms of commercial exploitation?',
    'Empirical studies on the economic impact of transformative uses on original markets versus the economic value generated by the new works, and analysis of who captures that value.',
    'If primarily subsidizing commercial exploitation, the constraint''s extractive nature would be amplified, potentially reclassifying it closer to a ''snare'' for original creators. If public benefit is clearly demonstrated, its ''tangled_rope'' classification would be reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(market_harm_subordination_justification, empirical, 'Justification for subordinating market harm in fair use analysis.').

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a genuine reading of the ''fair_use_four_factor_test'' kernel, or has the emphasis on transformativeness fundamentally altered the kernel''s original intent, effectively creating a new constraint?',
    'Historical legal analysis tracing the evolution of fair use jurisprudence, comparing the ''transformative use'' reading against the legislative history and early judicial interpretations of the four factors.',
    'If it''s a fundamental alteration, it suggests a ''practice_drift'' in the kernel''s application, potentially leading to a re-evaluation of the ''fair_use_four_factor_test'' as a whole. If it''s a consistent evolution, it reinforces the idea of the kernel as a flexible framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether transformative use is a reading or a re-interpretation of the fair use kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__transformative_use_reading, 1994, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(fair_be_t1994, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 1994, 0.2).
narrative_ontology:measurement(fair_be_t2004, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2004, 0.3).
narrative_ontology:measurement(fair_be_t2014, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2014, 0.4).
narrative_ontology:measurement(fair_be_t2024, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1994, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 1994, 0.15).
narrative_ontology:measurement(fair_su_t2004, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2004, 0.2).
narrative_ontology:measurement(fair_su_t2014, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2014, 0.25).
narrative_ontology:measurement(fair_su_t2024, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__transformative_use_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'fair_use_four_factor_test' kernel. The other readings are 'creator_centric_reading' and 'user_centric_reading', each with different ε values and stakeholder impacts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
