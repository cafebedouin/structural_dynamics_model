% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__user_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_four_factor_test__user_centric_reading, []).

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
 *   constraint_id: fair_use_four_factor_test__user_centric_reading
 *   human_readable: Fair Use Four-Factor Test (User-Centric Reading)
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   This constraint represents a 'user-centric' reading of the fair use
 *   doctrine, emphasizing its role as an affirmative right for users to
 *   access and transform copyrighted works for public benefit. The four
 *   factors (purpose and character of the use, nature of the copyrighted
 *   work, amount and substantiality of the portion used, and effect of the
 *   use upon the potential market for or value of the copyrighted work) are
 *   weighed with a strong bias towards promoting public access and cultural
 *   production, even at the expense of some creator compensation. This
 *   reading views fair use as a necessary counterweight to the expansive
 *   nature of copyright, ensuring a robust public domain and freedom of
 *   expression. This is one reading of the 'fair_use_four_factor_test'
 *   kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__user_centric_reading, 0.25).
domain_priors:suppression_score(fair_use_four_factor_test__user_centric_reading, 0.3).
domain_priors:theater_ratio(fair_use_four_factor_test__user_centric_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__user_centric_reading, rope).
narrative_ontology:human_readable(fair_use_four_factor_test__user_centric_reading, "Fair Use Four-Factor Test (User-Centric Reading)").
narrative_ontology:topic_domain(fair_use_four_factor_test__user_centric_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__user_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__user_centric_reading, 'e49044fa-5f30-4a0f-85a8-fd0a4b81086c').
narrative_ontology:cs_kernel_codification('e49044fa-5f30-4a0f-85a8-fd0a4b81086c', fixed_text).
narrative_ontology:cs_authority_grounding('e49044fa-5f30-4a0f-85a8-fd0a4b81086c', lineage).
narrative_ontology:cs_interpretation_layer_present('e49044fa-5f30-4a0f-85a8-fd0a4b81086c').
narrative_ontology:cs_reading_relation('e49044fa-5f30-4a0f-85a8-fd0a4b81086c', fair_use_four_factor_test__creator_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('e49044fa-5f30-4a0f-85a8-fd0a4b81086c', fair_use_four_factor_test__transformative_use_reading, coexists_with).
narrative_ontology:cs_axiom('e49044fa-5f30-4a0f-85a8-fd0a4b81086c', foundational, fair_use_as_affirmative_right).
narrative_ontology:cs_axiom_status(fair_use_as_affirmative_right, holdable).
narrative_ontology:cs_axiom_grounding('e49044fa-5f30-4a0f-85a8-fd0a4b81086c', fair_use_as_affirmative_right, deontological).
narrative_ontology:cs_axiom('e49044fa-5f30-4a0f-85a8-fd0a4b81086c', foundational, public_access_priority).
narrative_ontology:cs_axiom_status(public_access_priority, holdable).
narrative_ontology:cs_axiom_grounding('e49044fa-5f30-4a0f-85a8-fd0a4b81086c', public_access_priority, instrumental).
narrative_ontology:cs_reference_frame('e49044fa-5f30-4a0f-85a8-fd0a4b81086c', public_interest_balancing).
narrative_ontology:cs_drift_state('e49044fa-5f30-4a0f-85a8-fd0a4b81086c', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e49044fa-5f30-4a0f-85a8-fd0a4b81086c', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, public_users).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, educational_institutions).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, cultural_producers).
narrative_ontology:constraint_victim(fair_use_four_factor_test__user_centric_reading, copyright_holders).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__user_centric_reading, public_domain_enrichment).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__user_centric_reading, free_speech_principles).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from broad access to copyrighted works for commentary, criticism, news reporting, teaching, scholarship, or research without needing permission or paying royalties. Their ability to engage in cultural production and discourse is enhanced.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, public_users, beneficiary,
    organized, generational, constrained, global).

% Relies on fair use to incorporate copyrighted materials into curricula, research, and teaching without prohibitive licensing costs. This enables broader dissemination of knowledge and supports academic freedom.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, educational_institutions, beneficiary,
    institutional, generational, constrained, national).

% Leverages existing copyrighted works to create new art, music, literature, and other cultural products, fostering a vibrant and derivative cultural landscape. Fair use reduces barriers to entry for new creators.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, cultural_producers, beneficiary,
    moderate, biographical, mobile, global).

% Experiences reduced control over their copyrighted works and potentially diminished revenue from uses deemed 'fair.' They bear the cost of uncompensated use, which they argue can disincentivize creation.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, copyright_holders, payer,
    powerful, biographical, constrained, global).

% Interpret and apply the four-factor test in specific cases, shaping the boundaries of fair use. Their rulings define the practical scope of the right and influence subsequent cultural and legal practices.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% Monitor the application of fair use and consider potential amendments to copyright law. They respond to lobbying from both user and creator groups, balancing competing interests in intellectual property.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, legislators, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Balances the exclusive rights of copyright holders with the public's interest in accessing and using copyrighted works for socially beneficial purposes, fostering cultural production and public discourse.
% TRANSFER_FUNCTION: Transfers the right to use copyrighted material without permission or payment from copyright holders to users (public, educational, cultural producers) under specific circumstances, facilitating public access and new creation.
% ABSENT_VOICES: Future generations of creators and users, who would advocate for a robust public domain and flexible copyright exceptions to ensure continued cultural flourishing, are not directly represented in current legislative or judicial debates.
% DISAPPEARANCE_RATIONALE: If fair use vanished, public access to copyrighted works for criticism, education, and new creation would be severely curtailed. Every use would require permission, leading to a chilling effect on cultural production, increased litigation, and a less vibrant public sphere. The entire ecosystem of derivative works and educational materials would need to be renegotiated or cease to exist.
% FOUNDING_PROBLEM: Copyright law, by granting exclusive rights, risked stifling creativity and public access to knowledge, creating a need for a mechanism to permit socially beneficial uses without permission.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, public interest groups, and educators consistently attest that the tension between exclusive rights and public access remains a live and evolving problem, requiring a flexible doctrine like fair use. Independent analyses of cultural production trends support the ongoing need for such a balance.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__user_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__user_centric_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__user_centric_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(fair_use_four_factor_test__user_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__user_centric_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__user_centric_reading_tests).
:- end_tests(fair_use_four_factor_test__user_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because this reading prioritizes public benefit over private gain, minimizing the 'cost' to users. Suppression is also low (0.3) as it aims to reduce barriers to use, though some legal uncertainty and enforcement costs for users remain. Theater ratio is low (0.1) because the doctrine, in this reading, genuinely serves its stated purpose of balancing rights. Accessibility collapse is moderate (0.4) as it opens up many uses, but not all, and resistance is low (0.2) from users, who largely benefit, but higher from rights holders (captured in the 'payer' role).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public users and educational institutions, this reading of fair use is a vital 'rope' that enables their activities. From the perspective of copyright holders, it might feel more like a 'tangled rope' or even a 'snare,' as it extracts value from their intellectual property without direct compensation. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Public users, educational institutions, and cultural producers are the primary beneficiaries, experiencing low directionality as the constraint subsidizes their activities. Copyright holders are the victims/payers, facing higher directionality due to reduced control and potential revenue. Courts act as agenda-setters, interpreting and enforcing the doctrine, while legislators observe and may adjust the legal framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This user-centric reading actively resists mandatrophy by continuously re-asserting the public benefit and cultural production mandate of fair use, preventing it from atrophying into a mere formality or being captured by exclusive rights holders. The ongoing contestation with creator-centric readings ensures its function remains debated and, in this reading, defended.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_public_benefit,
    'How broadly should ''public access and cultural production'' be interpreted in the context of fair use balancing?',
    'Judicial precedent that explicitly defines the scope of public benefit, or legislative clarification of the policy goals of fair use.',
    'A broader interpretation would further reduce extractiveness for users and increase it for rights holders; a narrower interpretation would shift the balance towards creator control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_public_benefit, conceptual, 'Ambiguity in defining the ''public benefit'' criterion in fair use.').

omega_variable(
    market_harm_assessment,
    'To what extent should potential market harm to the copyright holder be subordinated to the public benefit in the four-factor analysis?',
    'Empirical studies on the actual economic impact of various fair uses on different markets, combined with judicial guidance on the weight of the fourth factor.',
    'If market harm is consistently subordinated, the constraint''s extractiveness from rights holders remains low; if it gains more weight, extractiveness increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_harm_assessment, empirical, 'The relative weight of market harm versus public benefit in fair use analysis.').

omega_variable(
    reading_legitimacy,
    'Is this user-centric reading a legitimate interpretation of fair use, or does it overstep the statutory intent?',
    'Supreme Court rulings that explicitly endorse or reject a user-centric framework, or a legislative re-codification of fair use that clarifies its primary purpose.',
    'If deemed illegitimate, the constraint would shift towards a more creator-centric or transformative-use classification, increasing extractiveness for users. If affirmed, its current classification as a ''rope'' for users would be reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_legitimacy, conceptual, 'The fundamental legitimacy of the user-centric interpretation of fair use.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__user_centric_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1976, fair_use_four_factor_test__user_centric_reading, theater_ratio, 1976, 0.05).
narrative_ontology:measurement(fair_tr_t1990, fair_use_four_factor_test__user_centric_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(fair_tr_t2000, fair_use_four_factor_test__user_centric_reading, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(fair_tr_t2010, fair_use_four_factor_test__user_centric_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(fair_tr_t2024, fair_use_four_factor_test__user_centric_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(fair_be_t1976, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 1976, 0.2).
narrative_ontology:measurement(fair_be_t1990, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 1990, 0.22).
narrative_ontology:measurement(fair_be_t2000, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 2000, 0.23).
narrative_ontology:measurement(fair_be_t2010, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 2010, 0.24).
narrative_ontology:measurement(fair_be_t2024, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1976, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 1976, 0.25).
narrative_ontology:measurement(fair_su_t1990, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 1990, 0.28).
narrative_ontology:measurement(fair_su_t2000, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 2000, 0.29).
narrative_ontology:measurement(fair_su_t2010, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 2010, 0.29).
narrative_ontology:measurement(fair_su_t2024, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__user_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, creator_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, transformative_use_reading).

% DUAL FORMULATION NOTE:
% This is one of three distinct readings of the 'fair_use_four_factor_test' kernel. Each reading instantiates a different constraint with its own structural properties and classification. This user-centric reading emphasizes public access and cultural production, contrasting with creator-centric and transformative-use perspectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
