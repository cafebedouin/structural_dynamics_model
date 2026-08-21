% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__transformative_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   human_readable: Fair Use as Transformative Right (Legal Reading)
 *   domain: intellectual_property_law/legal_interpretation/information_economics
 *
 * SUMMARY:
 *   This constraint story models the 'transformative right' reading of the
 *   fair use statutory exception in intellectual property law. Under this
 *   reading, fair use is interpreted broadly to enable and encourage new
 *   cultural production and innovation through the reuse and adaptation of
 *   existing works. Courts are expected to actively facilitate this
 *   innovation, with less emphasis on the potential market harm to the
 *   original copyright holder, especially when the new use is highly
 *   transformative. This reading views fair use as a fundamental right or
 *   privilege, not merely a narrow defense.
 *
 * KEY AGENTS:
 *   - transformative_creators: Primary beneficiary (moderate/constrained) — benefits from broad fair use.
 *   - original_copyright_holders_in_transformative_contexts: Primary payer (powerful/constrained) — bears the cost of limited exclusive rights.
 *   - courts_and_judges: Agenda setter (institutional/analytical) — interpret and shape the doctrine.
 *   - licensing_market_operators: Excluded (organized/trapped) — their market is diminished by this reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__transformative_right_reading, 0.3).
domain_priors:suppression_score(fair_use_statutory_exception__transformative_right_reading, 0.4).
domain_priors:theater_ratio(fair_use_statutory_exception__transformative_right_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__transformative_right_reading, rope).
narrative_ontology:human_readable(fair_use_statutory_exception__transformative_right_reading, "Fair Use as Transformative Right (Legal Reading)").
narrative_ontology:topic_domain(fair_use_statutory_exception__transformative_right_reading, "intellectual_property_law/legal_interpretation/information_economics").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__transformative_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__transformative_right_reading, 'cb037bea-7cb0-4de7-b393-78ee099c7289').
narrative_ontology:cs_kernel_codification('cb037bea-7cb0-4de7-b393-78ee099c7289', fixed_text).
narrative_ontology:cs_authority_grounding('cb037bea-7cb0-4de7-b393-78ee099c7289', lineage).
narrative_ontology:cs_interpretation_layer_present('cb037bea-7cb0-4de7-b393-78ee099c7289').
narrative_ontology:cs_reading_relation('cb037bea-7cb0-4de7-b393-78ee099c7289', fair_use_statutory_exception__narrow_defense_reading, coexists_with).
narrative_ontology:cs_reading_relation('cb037bea-7cb0-4de7-b393-78ee099c7289', fair_use_statutory_exception__market_licensing_reading, coexists_with).
narrative_ontology:cs_axiom('cb037bea-7cb0-4de7-b393-78ee099c7289', foundational, transformative_use_is_public_good).
narrative_ontology:cs_axiom_status(transformative_use_is_public_good, holdable).
narrative_ontology:cs_axiom_grounding('cb037bea-7cb0-4de7-b393-78ee099c7289', transformative_use_is_public_good, deontological).
narrative_ontology:cs_axiom('cb037bea-7cb0-4de7-b393-78ee099c7289', foundational, innovation_requires_building_on_prior_works).
narrative_ontology:cs_axiom_status(innovation_requires_building_on_prior_works, holdable).
narrative_ontology:cs_axiom_grounding('cb037bea-7cb0-4de7-b393-78ee099c7289', innovation_requires_building_on_prior_works, empirically_contingent).
narrative_ontology:cs_reference_frame('cb037bea-7cb0-4de7-b393-78ee099c7289', public_benefit_oriented_copyright).
narrative_ontology:cs_drift_state('cb037bea-7cb0-4de7-b393-78ee099c7289', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('cb037bea-7cb0-4de7-b393-78ee099c7289', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, transformative_creators).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, public_domain_advocates).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, innovation_economy).
narrative_ontology:constraint_victim(fair_use_statutory_exception__transformative_right_reading, original_copyright_holders_in_transformative_contexts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These creators rely on the ability to reuse and adapt existing works to create new, transformative content without needing prior permission or paying licensing fees. Their ability to innovate is directly tied to a broad interpretation of fair use.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, transformative_creators, beneficiary,
    moderate, biographical, constrained, global).

% Advocate for a robust public domain and the free flow of information and culture. They view fair use as a critical mechanism to prevent copyright from becoming an absolute monopoly, ensuring future creativity and access to knowledge.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, public_domain_advocates, beneficiary,
    organized, generational, analytical, global).

% Benefits from a legal framework that encourages new forms of expression, technology, and business models that build upon existing cultural works. A broad fair use doctrine reduces legal friction for startups and creative ventures.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, innovation_economy, beneficiary,
    institutional, generational, mobile, global).

% These rights holders find their exclusive rights limited by transformative uses, potentially reducing their ability to control or monetize derivative works. They bear the cost of not being able to license every use of their work, even if transformative.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, original_copyright_holders_in_transformative_contexts, payer,
    powerful, biographical, constrained, global).

% Interpret and apply the fair use doctrine, shaping its scope through case law. This reading directs them to prioritize the public benefit of transformative works and innovation over the copyright holder's exclusive control.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, courts_and_judges, agenda_setter,
    institutional, generational, analytical, national).

% These entities facilitate the licensing of copyrighted works. Under this reading, their potential market for licensing transformative uses is diminished, as such uses are often deemed fair use and thus do not require a license.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, licensing_market_operators, excluded,
    organized, biographical, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Balances the exclusive rights of copyright holders with the public interest in promoting creativity, innovation, and the free exchange of ideas, by providing a legal safe harbor for transformative uses.
% TRANSFER_FUNCTION: Transfers the right to reuse and adapt copyrighted material for transformative purposes from the original copyright holder to the transformative creator, without monetary compensation or permission.
% ABSENT_VOICES: Those who advocate for an absolute property right in copyright, where any use requires permission or payment, are often marginalized in this reading. They would argue that this interpretation undermines the economic incentive for creation.
% DISAPPEARANCE_RATIONALE: If fair use as a transformative right vanished, the landscape of cultural production and innovation would fundamentally change. Transformative creators would face prohibitive legal barriers, leading to a chilling effect on new works, increased litigation, and a significant slowdown in cultural and technological development that relies on building upon existing works.
% FOUNDING_PROBLEM: The problem of how to balance the economic incentives for creators (via copyright) with the public's need for access to and reuse of creative works to foster new creativity and knowledge.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, economists, and technology policy experts (outside the direct beneficiaries of this reading) widely attest that balancing copyright with innovation remains a live and critical problem, especially in the digital age. Judicial opinions also frequently acknowledge this ongoing tension.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__transformative_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__transformative_right_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__transformative_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(fair_use_statutory_exception__transformative_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__transformative_right_reading, 0.3, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.3) is relatively low because this reading aims to minimize the burden on transformative creators, effectively transferring the right to use without payment. Suppression (0.4) is moderate, reflecting the ongoing legal battles and the need for courts to actively enforce this interpretation against more restrictive views. Theater ratio (0.1) is low as the function is genuinely about enabling reuse, not merely performing a legal ritual. Accessibility collapse (0.4) is moderate, as alternatives to fair use (e.g., licensing) still exist but are often less desirable or feasible for transformative works. Resistance (0.5) is moderate, reflecting the continuous pushback from copyright holders and those advocating for a narrower interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of transformative creators, this reading is a vital 'rope' that enables their work. From the perspective of original copyright holders, it can feel like a 'snare' that diminishes their property rights. The courts, as agenda setters, navigate this tension, with this reading guiding them towards prioritizing the public benefit of transformation.
 *
 * DIRECTIONALITY LOGIC:
 *   Transformative creators, public domain advocates, and the innovation economy are beneficiaries (low d) as the constraint directly enables their activities. Original copyright holders are payers (high d) as their exclusive rights are curtailed. Courts are agenda setters, actively shaping the interpretation. Licensing market operators are excluded, as this reading reduces the scope for their services.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents the mislabeling of genuine cultural production as mere infringement. By emphasizing transformation and innovation, it ensures that the fair use doctrine remains a dynamic tool for balancing rights, rather than atrophying into a purely defensive mechanism that stifles creativity. It actively resists the 'snare' classification that a narrow, market-focused reading might produce, by asserting a public-interest coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformative_threshold_ambiguity,
    'What degree of ''transformative'' is sufficient to qualify for fair use under this reading, and how consistently is it applied across different courts and technologies?',
    'Empirical analysis of judicial decisions over time, identifying consistent patterns or divergences in the application of the ''transformative use'' factor across various media and technologies.',
    'If the threshold is inconsistently applied or too high, the effective extractiveness for transformative creators increases, potentially shifting the classification towards a ''tangled_rope'' due to uncertainty and litigation costs. If consistently broad, it reinforces the ''rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_threshold_ambiguity, empirical, 'Uncertainty in the judicial interpretation and application of ''transformative use''.').

omega_variable(
    market_harm_dispositivity_ambiguity,
    'To what extent should the ''effect upon the potential market'' factor be considered dispositive, even for highly transformative uses, under this reading?',
    'Conceptual clarification through leading judicial opinions or legislative guidance that explicitly defines the weight of market harm in transformative contexts, particularly when a new market for transformative licenses might emerge.',
    'If market harm is given significant weight even for transformative uses, the constraint''s extractiveness increases, and its classification might drift towards a ''tangled_rope'' or even ''snare'' for creators. If market harm is largely discounted for transformative uses, it reinforces the ''rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_harm_dispositivity_ambiguity, conceptual, 'Ambiguity regarding the weight of market harm in fair use analysis for transformative works.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__transformative_right_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(fair_be_t1976, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 1976, 0.2).
narrative_ontology:measurement(fair_be_t1990, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 1990, 0.25).
narrative_ontology:measurement(fair_be_t2000, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 2000, 0.28).
narrative_ontology:measurement(fair_be_t2010, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 2010, 0.3).
narrative_ontology:measurement(fair_be_t2020, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 2020, 0.3).
narrative_ontology:measurement(fair_be_t2024, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1976, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 1976, 0.3).
narrative_ontology:measurement(fair_su_t1990, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(fair_su_t2000, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 2000, 0.38).
narrative_ontology:measurement(fair_su_t2010, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement(fair_su_t2020, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 2020, 0.4).
narrative_ontology:measurement(fair_su_t2024, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__transformative_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception__narrow_defense_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception__market_licensing_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, copyright_term_extension_laws).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'fair_use_statutory_exception' kernel. It is linked to sibling readings that offer alternative interpretations of fair use, and to broader copyright laws it influences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
