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
 *   This constraint represents the 'creator-centric' reading of the
 *   four-factor fair use test in US copyright law. Under this interpretation,
 *   fair use is a narrow exception to the copyright holder's exclusive
 *   rights, with a strong emphasis on preserving creator incentives and
 *   minimizing market harm. The four factors (purpose and character of the
 *   use, nature of the copyrighted work, amount and substantiality of the
 *   portion used, and effect of the use upon the potential market for or
 *   value of the copyrighted work) are weighed with a bias towards protecting
 *   the rights holder. This reading often leads to a chilling effect on
 *   transformative and derivative works.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__creator_centric_reading, 0.7).
domain_priors:suppression_score(fair_use_four_factor_test__creator_centric_reading, 0.6).
domain_priors:theater_ratio(fair_use_four_factor_test__creator_centric_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__creator_centric_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__creator_centric_reading, "Fair Use Four-Factor Test (Creator-Centric Reading)").
narrative_ontology:topic_domain(fair_use_four_factor_test__creator_centric_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__creator_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__creator_centric_reading, '50beeed4-378c-400b-a758-8d3a4bb0e0df').
narrative_ontology:cs_kernel_codification('50beeed4-378c-400b-a758-8d3a4bb0e0df', fixed_text).
narrative_ontology:cs_authority_grounding('50beeed4-378c-400b-a758-8d3a4bb0e0df', lineage).
narrative_ontology:cs_interpretation_layer_present('50beeed4-378c-400b-a758-8d3a4bb0e0df').
narrative_ontology:cs_reading_relation('50beeed4-378c-400b-a758-8d3a4bb0e0df', fair_use_four_factor_test__user_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('50beeed4-378c-400b-a758-8d3a4bb0e0df', fair_use_four_factor_test__transformative_use_reading, coexists_with).
narrative_ontology:cs_axiom('50beeed4-378c-400b-a758-8d3a4bb0e0df', foundational, copyright_as_property_right).
narrative_ontology:cs_axiom_status(copyright_as_property_right, holdable).
narrative_ontology:cs_axiom_grounding('50beeed4-378c-400b-a758-8d3a4bb0e0df', copyright_as_property_right, deontological).
narrative_ontology:cs_axiom('50beeed4-378c-400b-a758-8d3a4bb0e0df', foundational, creator_incentive_primary).
narrative_ontology:cs_axiom_status(creator_incentive_primary, holdable).
narrative_ontology:cs_axiom_grounding('50beeed4-378c-400b-a758-8d3a4bb0e0df', creator_incentive_primary, instrumental).
narrative_ontology:cs_reference_frame('50beeed4-378c-400b-a758-8d3a4bb0e0df', traditional_copyright_protection).
narrative_ontology:cs_drift_state('50beeed4-378c-400b-a758-8d3a4bb0e0df', contemporary_digital_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('50beeed4-378c-400b-a758-8d3a4bb0e0df', '').
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

% Primary beneficiaries of copyright protection, they assert broad control over their works and view fair use as a narrow, exceptional defense. They actively enforce their rights through litigation and licensing, shaping the interpretation of the four factors to favor their interests.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, copyright_holders, agenda_setter,
    institutional, generational, mobile, global).

% Entities that profit from licensing copyrighted material. This reading of fair use strengthens their position by making unauthorized use riskier, thus driving demand for licenses and increasing their revenue streams.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, content_licensors, beneficiary,
    organized, biographical, mobile, global).

% Artists, educators, critics, and remix creators who wish to use copyrighted material in new, expressive ways. Under this reading, they face significant legal risk and potential liability, leading to self-censorship or costly licensing negotiations.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, transformative_users, payer,
    moderate, biographical, constrained, global).

% Creators whose work builds upon existing copyrighted material. This reading makes it harder for them to claim fair use, increasing their costs and limiting their creative freedom, often leading to a 'chilling effect' on new works.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, derivative_creators, payer,
    moderate, biographical, constrained, global).

% Groups arguing for broader public access to cultural works and a robust public domain. This reading of fair use limits the scope of what can be freely used, hindering the growth of the public domain and cultural commons.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, public_domain_advocates, excluded,
    organized, generational, trapped, national).

% The ultimate arbiters of fair use disputes, they interpret and apply the four factors. Under this reading, courts tend to prioritize the rights holder's market interests and the potential for market harm, even for highly transformative uses.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate the balance between copyright holders' exclusive rights and the public's interest in using copyrighted works, by providing a flexible, case-by-case defense against infringement claims.
% TRANSFER_FUNCTION: Transfers control and potential revenue from unauthorized users (even transformative ones) back to copyright holders, by making fair use a narrow, difficult-to-prove exception.
% ABSENT_VOICES: The broader public, who would benefit from a more expansive fair use doctrine that fosters cultural production and access, is largely absent from the direct legal disputes, experiencing the effects indirectly through reduced availability of derivative works and higher licensing costs.
% DISAPPEARANCE_RATIONALE: If the creator-centric reading of fair use vanished, copyright holders would face a flood of unauthorized uses, potentially undermining their economic models. Conversely, transformative creators would experience a surge in creative freedom, leading to a rapid reorganization of cultural production and licensing practices.
% FOUNDING_PROBLEM: To provide a limited, equitable defense against copyright infringement, acknowledging that some unauthorized uses are socially beneficial and do not unduly harm creators, while primarily protecting the economic incentives of authors.
% FOUNDING_PROBLEM_CORROBORATION: Copyright industry groups and many legal scholars attest that the problem of balancing creator incentives with public access remains live, and that a strong, creator-centric fair use is essential for maintaining a vibrant creative economy. Independent economists and cultural theorists, however, often contest the degree to which current fair use interpretations actually achieve this balance, suggesting it over-protects incumbents.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__creator_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__creator_centric_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__creator_centric_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(fair_use_four_factor_test__creator_centric_reading, 'none', 1).

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
 *   The extractiveness (0.7) is high because this reading significantly limits the scope of free use, forcing many creators to seek licenses or risk litigation, thus transferring value to rights holders. Suppression (0.6) is moderate, as the legal framework actively discourages unauthorized use through the threat of enforcement, but some avenues for fair use claims still exist. Theater ratio (0.2) is low, as the enforcement mechanism is genuinely aimed at protecting economic rights, not merely performing a function. Accessibility collapse (0.4) is moderate, as alternatives (licensing, creating entirely original works) exist but are often costly or creatively restrictive. Resistance (0.3) is also moderate, with ongoing legal challenges and advocacy for broader fair use, but not widespread non-compliance.
 *
 * PERSPECTIVAL GAP:
 *   Copyright holders and content licensors experience this as a necessary protection for their creative and economic output, ensuring a return on investment. Transformative users and derivative creators, however, experience it as a significant barrier to entry and a chilling effect on new cultural production, perceiving it as an extractive mechanism that prioritizes incumbent rights over innovation and public access.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright holders and content licensors are clear beneficiaries, as this reading strengthens their control and revenue streams. Transformative users and derivative creators are victims, bearing the costs of licensing or litigation risk. Courts act as agenda-setters, interpreting the law in a way that reinforces this creator-centric bias. Public domain advocates are excluded, as their arguments for broader access are marginalized by this interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by clearly identifying the beneficiaries and victims. While the original mandate of copyright includes incentivizing creation, this specific reading of fair use has arguably drifted towards over-protection, potentially stifling the very creativity it aims to foster. The high extractiveness and suppression, coupled with the contested status of the founding problem, suggest a potential for mandatrophy where the mechanism designed to incentivize creation now primarily serves to extract rents from subsequent creators.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    creator_incentive_vs_public_benefit,
    'Does this creator-centric reading of fair use optimally balance creator incentives with public benefit and the promotion of new cultural works, or does it over-prioritize the former at the expense of the latter?',
    'Longitudinal empirical studies on the impact of fair use rulings on both creator output and derivative creation, as well as economic analysis of market concentration in creative industries.',
    'If it''s found to over-prioritize creator incentives, it would suggest the constraint is more extractive than coordinative, potentially reclassifying it closer to a Snare from the public''s perspective. If the balance is optimal, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creator_incentive_vs_public_benefit, empirical, 'The actual impact of this reading on the overall creative ecosystem.').

omega_variable(
    fair_use_reading_framing_ambiguity,
    'Is this constraint a genuine interpretation of the fair use statute''s intent, or a constructed legal framework that primarily benefits identifiable agents (copyright holders) under the guise of statutory fidelity?',
    'Historical legal analysis of legislative intent, comparative analysis with international copyright regimes, and critical legal studies examining power dynamics in IP law.',
    'If primarily a constructed benefit, the constraint''s effective extractiveness is higher, and its ''coordination'' function is more theatrical, pushing it closer to a Snare. If a genuine interpretation, the Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fair_use_reading_framing_ambiguity, conceptual, 'Ambiguity between statutory interpretation and interest-driven construction.').

omega_variable(
    kernel_reading_identification,
    'This constraint is one reading of the ''fair_use_four_factor_test'' kernel. What structural elements would change if the ''transformative_use_reading'' or ''user_centric_reading'' were adopted?',
    'Analysis of hypothetical court rulings under alternative readings, and legislative changes reflecting those interpretations.',
    'The ''transformative_use_reading'' would reduce extractiveness on derivative creators by prioritizing new meaning over market harm. The ''user_centric_reading'' would reduce extractiveness on all users by framing fair use as an affirmative right, shifting the burden of proof and expanding the scope of permissible uses. Both would significantly alter the beneficiary/victim structure and likely lower the overall extractiveness and suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Impact of alternative fair use interpretations on constraint structure.').


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
narrative_ontology:measurement(fair_su_t1976, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 1976, 0.4).
narrative_ontology:measurement(fair_su_t1990, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(fair_su_t2000, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(fair_su_t2010, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(fair_su_t2024, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__creator_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, copyright_term_extension).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, digital_millennium_copyright_act).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'fair_use_four_factor_test' kernel. The other readings are 'transformative_use_reading' and 'user_centric_reading', which offer different interpretations of the four factors and their balancing, leading to different structural outcomes for creators and users.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
