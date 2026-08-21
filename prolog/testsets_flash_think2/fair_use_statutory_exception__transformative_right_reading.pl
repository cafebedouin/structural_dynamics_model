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
 *   constraint_id: fair_use_statutory_exception__transformative_right_reading
 *   human_readable: Fair Use as Transformative Right (Courts Facilitate Innovation)
 *   domain: intellectual_property_law/legal_interpretation/information_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'transformative right' reading of
 *   the fair use statutory exception. In this reading, fair use is understood
 *   as a fundamental right designed to enable transformative reuse and
 *   cultural production, with courts actively interpreting the doctrine to
 *   facilitate innovation. It stands in contrast to readings that view fair
 *   use as a narrow defense or primarily through the lens of market
 *   licensing. The metrics reflect the intent and effect of this specific
 *   interpretation, aiming for low extraction on transformative uses.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__transformative_right_reading, 0.25).
domain_priors:suppression_score(fair_use_statutory_exception__transformative_right_reading, 0.3).
domain_priors:theater_ratio(fair_use_statutory_exception__transformative_right_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__transformative_right_reading, rope).
narrative_ontology:human_readable(fair_use_statutory_exception__transformative_right_reading, "Fair Use as Transformative Right (Courts Facilitate Innovation)").
narrative_ontology:topic_domain(fair_use_statutory_exception__transformative_right_reading, "intellectual_property_law/legal_interpretation/information_economics").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__transformative_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__transformative_right_reading, 'dfe14dfc-80ea-4f16-ac6c-8ab8a807db94').
narrative_ontology:cs_kernel_codification('dfe14dfc-80ea-4f16-ac6c-8ab8a807db94', fixed_text).
narrative_ontology:cs_authority_grounding('dfe14dfc-80ea-4f16-ac6c-8ab8a807db94', lineage).
narrative_ontology:cs_interpretation_layer_present('dfe14dfc-80ea-4f16-ac6c-8ab8a807db94').
narrative_ontology:cs_reading_relation('dfe14dfc-80ea-4f16-ac6c-8ab8a807db94', fair_use_statutory_exception__narrow_defense_reading, coexists_with).
narrative_ontology:cs_reading_relation('dfe14dfc-80ea-4f16-ac6c-8ab8a807db94', fair_use_statutory_exception__market_licensing_reading, coexists_with).
narrative_ontology:cs_axiom('dfe14dfc-80ea-4f16-ac6c-8ab8a807db94', foundational, transformative_use_is_public_good).
narrative_ontology:cs_axiom_status(transformative_use_is_public_good, holdable).
narrative_ontology:cs_axiom_grounding('dfe14dfc-80ea-4f16-ac6c-8ab8a807db94', transformative_use_is_public_good, deontological).
narrative_ontology:cs_axiom('dfe14dfc-80ea-4f16-ac6c-8ab8a807db94', foundational, innovation_requires_building_on_prior_works).
narrative_ontology:cs_axiom_status(innovation_requires_building_on_prior_works, holdable).
narrative_ontology:cs_axiom_grounding('dfe14dfc-80ea-4f16-ac6c-8ab8a807db94', innovation_requires_building_on_prior_works, instrumental).
narrative_ontology:cs_reference_frame('dfe14dfc-80ea-4f16-ac6c-8ab8a807db94', public_interest_balancing_framework).
narrative_ontology:cs_drift_state('dfe14dfc-80ea-4f16-ac6c-8ab8a807db94', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('dfe14dfc-80ea-4f16-ac6c-8ab8a807db94', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, transformative_creators).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, public_domain_advocates).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, general_public).
narrative_ontology:constraint_victim(fair_use_statutory_exception__transformative_right_reading, copyright_holders).
narrative_ontology:constraint_victim(fair_use_statutory_exception__transformative_right_reading, licensing_market_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Artists, writers, musicians, and developers who build new works by transforming existing copyrighted material. This reading of fair use enables their creative and commercial activities without requiring prior permission or licensing fees, fostering innovation.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, transformative_creators, beneficiary,
    moderate, biographical, mobile, global).

% Authors, publishers, and corporations who own copyrights. This reading limits their exclusive rights, allowing others to use their works transformatively without compensation, which they perceive as an erosion of their property rights and market control.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, copyright_holders, payer,
    powerful, generational, constrained, global).

% Judicial bodies responsible for interpreting and applying copyright law, including the fair use doctrine. Under this reading, they actively seek to facilitate innovation and cultural production by broadly construing 'transformative use'.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% Organizations and individuals who champion the public's right to access and build upon cultural works. This reading aligns with their goals of expanding the public domain and promoting free cultural exchange.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, public_domain_advocates, beneficiary,
    organized, generational, analytical, global).

% Entities that facilitate the licensing of copyrighted works. This reading reduces the scope for mandatory licensing, potentially diminishing their market and revenue streams by allowing uses that would otherwise require a license.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, licensing_market_operators, payer,
    organized, biographical, constrained, global).

% Consumers and participants in cultural discourse who benefit from a richer, more diverse cultural landscape enabled by transformative works. They indirectly pay for copyright protection through higher prices for original works but gain from increased access to derivative content.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, general_public, beneficiary,
    powerless, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_statutory_exception__transformative_right_reading, transformative_creators).
narrative_ontology:fixing_cost_class(fair_use_statutory_exception__transformative_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Balances the exclusive rights of copyright holders with the public interest in promoting cultural progress and innovation, by establishing a legal framework for certain unauthorized uses of copyrighted material.
% TRANSFER_FUNCTION: Transfers the right to reuse and transform copyrighted material from copyright holders to creators, enabling new cultural production without requiring permission or payment for qualifying uses.
% ABSENT_VOICES: Strict property rights advocates and potential licensors are structurally marginalized; they would argue for a narrower interpretation of fair use to maximize copyright holders' control and licensing revenue, but their arguments are de-emphasized by this reading.
% DISAPPEARANCE_RATIONALE: If fair use vanished overnight, cultural production would be severely stifled. Every derivative work, parody, commentary, or educational use would require explicit permission, leading to a less vibrant public domain, reduced innovation, and significant legal overhead for creators. The digital economy, heavily reliant on reuse, would fundamentally reorganize.
% FOUNDING_PROBLEM: The absolute enforcement of copyright's exclusive rights would paradoxically hinder the very progress of science and useful arts it aims to promote, by preventing new creators from building upon existing works.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, cultural critics, economists, and technology policy experts (outside of copyright holder associations) corroborate the ongoing necessity of fair use to prevent copyright from becoming an impediment to innovation and free expression. Legislative hearings and academic studies frequently attest to this problem.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__transformative_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__transformative_right_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__transformative_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(fair_use_statutory_exception__transformative_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__transformative_right_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

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
 *   The `extractiveness` (0.25) and `suppression` (0.30) are low because this reading actively seeks to minimize the burden on transformative creators, treating fair use as an enabling right rather than a mere exception. The `accessibility_collapse` is low (0.20) as this interpretation expands, rather than collapses, alternatives for creative reuse. `Resistance` is high (0.75) due to ongoing challenges from copyright holders and licensing entities who advocate for stricter enforcement of exclusive rights. `Theater_ratio` is low (0.15) as the judicial function is genuinely about balancing interests, not performative maintenance of an atrophied function. The slight increase in extractiveness and suppression towards the end of the interval reflects the challenges posed by new technologies (e.g., generative AI) which introduce new complexities in defining 'transformative' and lead to increased litigation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of transformative creators, this reading of fair use is a vital rope, enabling their work and fostering cultural growth. From the perspective of copyright holders, it can feel like a snare, eroding their property rights and forcing them to bear costs without compensation. The courts, as agenda-setters, navigate this tension, but this reading prioritizes the public interest in innovation.
 *
 * DIRECTIONALITY LOGIC:
 *   Transformative creators and the general public are clear beneficiaries, gaining the ability to create and consume new works without undue restriction. Public domain advocates also benefit from a more robust public domain. Copyright holders and licensing market operators are the primary payers, as their exclusive rights and potential revenue streams are curtailed by this expansive interpretation of fair use. Courts act as agenda-setters, actively shaping the legal landscape to align with this reading's goals.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformative_definition_ambiguity,
    'How does the evolving definition of ''transformative use'' (especially with generative AI) impact the practical application and stability of this reading?',
    'Landmark court decisions establishing clear precedents for AI-generated content, or legislative updates to the fair use statute that explicitly address new technologies.',
    'If ''transformative'' is narrowly construed for new technologies, the effective extractiveness on creators could rise, pushing the constraint towards a Tangled Rope or Snare. If broadly construed, it reinforces the Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_definition_ambiguity, empirical, 'Ambiguity in defining ''transformative'' in the face of new technologies.').

omega_variable(
    burden_of_proof_chilling_effect,
    'Does the practical burden of proving fair use in litigation still create a chilling effect on smaller creators, despite this reading''s intent to facilitate innovation?',
    'Empirical studies on creator behavior and legal costs for fair use defenses, or the establishment of clear, low-cost advisory mechanisms for fair use claims.',
    'If a significant chilling effect persists, the effective suppression on smaller creators is higher than measured, suggesting a more extractive classification for that seat, potentially pushing the overall constraint towards a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burden_of_proof_chilling_effect, empirical, 'Whether the legal process itself suppresses innovation for smaller creators.').

omega_variable(
    market_harm_vs_innovation_priority,
    'To what extent should potential market harm to the original work be weighed against the public benefit of transformative innovation, when no direct market for the transformative use exists?',
    'Further judicial clarification on the ''fourth factor'' of fair use (effect upon the potential market for or value of the copyrighted work), or legislative guidance on the hierarchy of fair use factors.',
    'If market harm is consistently prioritized even in the absence of a direct market, the constraint''s extractiveness would rise, and its classification would shift towards a Tangled Rope, aligning more with the ''market_licensing_reading''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(market_harm_vs_innovation_priority, conceptual, 'Conceptual tension between market harm and innovation in fair use analysis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__transformative_right_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1976, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 1976, 0.1).
narrative_ontology:measurement(fair_tr_t1986, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 1986, 0.12).
narrative_ontology:measurement(fair_tr_t1996, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 1996, 0.13).
narrative_ontology:measurement(fair_tr_t2006, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 2006, 0.14).
narrative_ontology:measurement(fair_tr_t2016, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 2016, 0.15).
narrative_ontology:measurement(fair_tr_t2024, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(fair_be_t1976, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 1976, 0.35).
narrative_ontology:measurement(fair_be_t1986, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 1986, 0.3).
narrative_ontology:measurement(fair_be_t1996, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 1996, 0.28).
narrative_ontology:measurement(fair_be_t2006, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 2006, 0.26).
narrative_ontology:measurement(fair_be_t2016, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 2016, 0.24).
narrative_ontology:measurement(fair_be_t2024, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1976, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 1976, 0.4).
narrative_ontology:measurement(fair_su_t1986, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 1986, 0.35).
narrative_ontology:measurement(fair_su_t1996, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 1996, 0.32).
narrative_ontology:measurement(fair_su_t2006, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 2006, 0.28).
narrative_ontology:measurement(fair_su_t2016, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 2016, 0.25).
narrative_ontology:measurement(fair_su_t2024, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__transformative_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, copyright_licensing_markets).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, digital_content_distribution).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, intellectual_property_rights_enforcement).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'fair_use_statutory_exception' kernel, focusing on fair use as an enabling right for transformative creation. It is linked to 'narrow_defense_reading' and 'market_licensing_reading' as sibling interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
