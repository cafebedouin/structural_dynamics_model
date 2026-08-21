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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: fair_use_four_factor_test__transformative_use_reading
 *   human_readable: Fair Use Four-Factor Test: Transformativeness Dominance Reading
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   This constraint represents a specific reading of the U.S. copyright law's
 *   fair use doctrine, where the 'transformativeness' of a new work (i.e.,
 *   whether it adds new meaning or purpose to the original) is the dominant
 *   factor in the four-factor balancing test. Under this reading, potential
 *   market harm to the original work is subordinated if the new use is
 *   sufficiently transformative. This interpretation has gained significant
 *   traction in judicial precedent, particularly since the 1994 Campbell v.
 *   Acuff-Rose Music, Inc. Supreme Court decision, enabling remix culture and
 *   platforms hosting user-generated content, while simultaneously imposing
 *   costs on original copyright holders.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__transformative_use_reading, 0.65).
domain_priors:suppression_score(fair_use_four_factor_test__transformative_use_reading, 0.3).
domain_priors:theater_ratio(fair_use_four_factor_test__transformative_use_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__transformative_use_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__transformative_use_reading, "Fair Use Four-Factor Test: Transformativeness Dominance Reading").
narrative_ontology:topic_domain(fair_use_four_factor_test__transformative_use_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__transformative_use_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__transformative_use_reading, 'c9aa7a6e-cd1d-4aee-b9ea-c6bb5d89a121').
narrative_ontology:cs_kernel_codification('c9aa7a6e-cd1d-4aee-b9ea-c6bb5d89a121', formalized).
narrative_ontology:cs_authority_grounding('c9aa7a6e-cd1d-4aee-b9ea-c6bb5d89a121', lineage).
narrative_ontology:cs_interpretation_layer_present('c9aa7a6e-cd1d-4aee-b9ea-c6bb5d89a121').
narrative_ontology:cs_reading_relation('c9aa7a6e-cd1d-4aee-b9ea-c6bb5d89a121', fair_use_four_factor_test__creator_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('c9aa7a6e-cd1d-4aee-b9ea-c6bb5d89a121', fair_use_four_factor_test__user_centric_reading, influences).
narrative_ontology:cs_axiom('c9aa7a6e-cd1d-4aee-b9ea-c6bb5d89a121', foundational, new_meaning_priority).
narrative_ontology:cs_axiom_status(new_meaning_priority, holdable).
narrative_ontology:cs_axiom_grounding('c9aa7a6e-cd1d-4aee-b9ea-c6bb5d89a121', new_meaning_priority, conventional).
narrative_ontology:cs_axiom('c9aa7a6e-cd1d-4aee-b9ea-c6bb5d89a121', secondary, market_harm_secondary).
narrative_ontology:cs_axiom_status(market_harm_secondary, holdable).
narrative_ontology:cs_axiom_grounding('c9aa7a6e-cd1d-4aee-b9ea-c6bb5d89a121', market_harm_secondary, conventional).
narrative_ontology:cs_reference_frame('c9aa7a6e-cd1d-4aee-b9ea-c6bb5d89a121', promote_public_interest_in_expression_through_transformation).
narrative_ontology:cs_drift_state('c9aa7a6e-cd1d-4aee-b9ea-c6bb5d89a121', contemporary_judicial_practice, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c9aa7a6e-cd1d-4aee-b9ea-c6bb5d89a121', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, remix_artists_and_creators).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, tech_platforms_enabling_ugc).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, original_copyright_holders).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, traditional_media_companies).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__transformative_use_reading, free_speech_principles).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__transformative_use_reading, cultural_progress_through_remix).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the legal space to create new works by transforming existing copyrighted material without needing prior permission or licensing. Their ability to operate depends heavily on this interpretation of fair use.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, remix_artists_and_creators, beneficiary,
    moderate, biographical, mobile, global).

% Benefit from a legal framework that protects user-generated content (UGC) and enables their business models, which often rely on the free flow of transformative works. They face less liability for hosting such content under this reading.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, tech_platforms_enabling_ugc, beneficiary,
    institutional, generational, arbitrage, global).

% Bear the cost of diminished control over their copyrighted works and potential loss of licensing revenue when those works are used transformatively. They must litigate to defend their rights against claims of fair use.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, original_copyright_holders, payer,
    organized, biographical, constrained, global).

% Interpret and apply the fair use doctrine, with this reading emphasizing transformativeness as the primary factor. Their rulings shape the boundaries of permissible use and influence cultural production.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, courts_and_judges, agenda_setter,
    institutional, generational, analytical, national).

% Observe and advocate for interpretations of copyright law that prioritize public access and the ability to build upon existing culture. This reading generally aligns with their goals, though they might push for even broader user rights.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, public_domain_advocates, observer,
    organized, generational, analytical, global).

% Similar to original copyright holders, they experience reduced ability to control and monetize their content when transformative uses are broadly permitted. They often lobby for stricter copyright enforcement.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, traditional_media_companies, payer,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the balance between copyright protection for original creators and the public interest in fostering new creative expression and cultural commentary, by prioritizing uses that add new meaning or purpose.
% TRANSFER_FUNCTION: Transfers a degree of control and potential market revenue from original copyright holders to transformative users and the platforms that host their content, in exchange for promoting new cultural production.
% ABSENT_VOICES: Very small independent creators who lack the resources to either defend their original works against transformative uses or to participate in the complex legal and lobbying efforts that shape fair use policy. Their interests are often aggregated into larger 'copyright holder' or 'creator' groups.
% DISAPPEARANCE_RATIONALE: If this reading of fair use vanished overnight, the legal landscape for cultural production would be drastically altered. Remix culture would face immediate legal challenges, tech platforms would be inundated with infringement claims, and the balance of power in intellectual property would shift heavily towards original copyright holders, stifling new forms of expression.
% FOUNDING_PROBLEM: The original problem was how to balance the constitutional mandate to promote the progress of science and useful arts by securing exclusive rights for creators, with the need for subsequent creators to build upon existing works and for the public to access and engage with culture.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, cultural historians, and public interest groups widely attest that the tension between creator rights and public access remains a live and evolving problem in the digital age. Judicial opinions, legislative debates, and academic literature from outside the direct beneficiaries corroborate this ongoing challenge.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__transformative_use_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__transformative_use_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__transformative_use_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(fair_use_four_factor_test__transformative_use_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__transformative_use_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__transformative_use_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_four_factor_test__transformative_use_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_four_factor_test__transformative_use_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` is moderate-high (0.65) because this reading allows for the use of copyrighted material without compensation, effectively extracting value from the original work for the benefit of the transformative user. `suppression` is low (0.3) because the constraint's primary function is to enable, rather than suppress, new creative uses, though it does suppress some copyright infringement claims. `theater_ratio` is low (0.1) as the fair use test, while complex, is a functional legal mechanism. `resistance` is high (0.7) due to ongoing litigation and lobbying from copyright holders who view this interpretation as eroding their rights. The temporal measurements reflect the increasing judicial emphasis on transformativeness over the last three decades, leading to a gradual increase in extraction from original creators and a decrease in the suppressive force of copyright claims against transformative uses.
 *
 * PERSPECTIVAL GAP:
 *   Original copyright holders perceive this reading as an erosion of their property rights and a form of uncompensated extraction, while remix artists and tech platforms view it as a necessary enabler of creativity and innovation. Courts, as agenda-setters, navigate these competing interests, with this reading representing a particular judicial philosophy that prioritizes public benefit through new expression.
 *
 * DIRECTIONALITY LOGIC:
 *   Remix artists and tech platforms are clear beneficiaries, gaining legal protection and market access. Original copyright holders and traditional media companies are payers, losing some control and potential revenue. Device users are indirect beneficiaries of a richer cultural landscape. Courts are agenda-setters, interpreting and enforcing the rules. Rival payment networks are not directly relevant to this specific constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to balance creator rights with public interest remains live. However, the 'subordination of market harm' aspect is often contested, with critics arguing that the original problem of promoting progress has been reinterpreted to allow for extraction from creators, rather than solely fostering new creation. The engine's classification as a Tangled Rope reflects this dual function of coordination (enabling new art) and extraction (from original creators).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformative_threshold_ambiguity,
    'What is the precise threshold for ''transformativeness'' that justifies subordinating market harm, and is this threshold consistently applied across different judicial circuits and media types?',
    'Empirical analysis of judicial decisions over time, identifying patterns in how ''new meaning or purpose'' is defined and applied in practice, potentially leading to clearer legislative guidelines.',
    'If the threshold is inconsistent or too low, the constraint''s extractiveness from original creators is higher and less predictable than intended, potentially chilling original creation. If too high, it stifles transformative works.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_threshold_ambiguity, empirical, 'Ambiguity in the definition and application of ''transformativeness'' in fair use cases.').

omega_variable(
    market_harm_subordination_justification,
    'Is the subordination of market harm always justified by the public benefit of transformative use, or does it disproportionately impact certain classes of creators (e.g., independent artists vs. large corporations)?',
    'Economic impact studies on different creator demographics, and philosophical analysis of the normative trade-offs between property rights and public access in specific contexts.',
    'If disproportionate harm is found, the constraint''s effective extraction is higher for vulnerable creators, suggesting a need for policy adjustments or alternative compensation mechanisms. If justified, the current balance is affirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_harm_subordination_justification, conceptual, 'Whether the subordination of market harm is equitably justified across all creators.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__transformative_use_reading, 1994, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1994, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 1994, 0.1).
narrative_ontology:measurement(fair_tr_t2004, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2004, 0.1).
narrative_ontology:measurement(fair_tr_t2014, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2014, 0.1).
narrative_ontology:measurement(fair_tr_t2024, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(fair_be_t1994, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 1994, 0.5).
narrative_ontology:measurement(fair_be_t2004, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2004, 0.55).
narrative_ontology:measurement(fair_be_t2014, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2014, 0.6).
narrative_ontology:measurement(fair_be_t2024, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1994, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 1994, 0.4).
narrative_ontology:measurement(fair_su_t2004, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2004, 0.37).
narrative_ontology:measurement(fair_su_t2014, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2014, 0.33).
narrative_ontology:measurement(fair_su_t2024, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__transformative_use_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, copyright_term_extension).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, dmca_safe_harbor_provisions).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, cultural_production_funding_models).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'fair_use_four_factor_test' kernel. It focuses on the dominance of transformativeness, while 'creator_centric_reading' emphasizes property rights and 'user_centric_reading' emphasizes broad public access. All three are distinct constraints linked by their shared kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
