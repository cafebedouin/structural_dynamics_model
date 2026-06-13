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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   This constraint represents the 'transformative right' reading of the fair
 *   use statutory exception in US copyright law. Under this reading, fair use
 *   is primarily understood as a right for creators to make new works that
 *   transform existing copyrighted material, with courts actively
 *   facilitating innovation. It prioritizes the public benefit of new
 *   cultural production over the copyright holder's exclusive control,
 *   especially when the new use does not directly substitute for the original
 *   in the market. This is one reading of the 'fair_use_statutory_exception'
 *   kernel, which also includes 'narrow_defense_reading' and
 *   'market_licensing_reading'.
 *
 * KEY AGENTS:
 *   - transformative_creators: Primary beneficiary (moderate/constrained) — gains legal space for new works.
 *   - courts_and_judges: Agenda setter (institutional/analytical) — interprets and shapes the doctrine.
 *   - copyright_holders: Payer (powerful/constrained) — bears cost of reduced control over works.
 *   - public_domain: Beneficiary (analytical/analytical) — expanded by more accessible material.
 *   - licensing_market_operators: Excluded (organized/constrained) — diminished market for licenses.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__transformative_right_reading, 0.25).
domain_priors:suppression_score(fair_use_statutory_exception__transformative_right_reading, 0.3).
domain_priors:theater_ratio(fair_use_statutory_exception__transformative_right_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 0.3).
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
narrative_ontology:cs_story_uid(fair_use_statutory_exception__transformative_right_reading, '59b21e29-2e74-476b-8a8f-894dbaebcf95').
narrative_ontology:cs_kernel_codification('59b21e29-2e74-476b-8a8f-894dbaebcf95', fixed_text).
narrative_ontology:cs_authority_grounding('59b21e29-2e74-476b-8a8f-894dbaebcf95', lineage).
narrative_ontology:cs_interpretation_layer_present('59b21e29-2e74-476b-8a8f-894dbaebcf95').
narrative_ontology:cs_reading_relation('59b21e29-2e74-476b-8a8f-894dbaebcf95', fair_use_statutory_exception__narrow_defense_reading, coexists_with).
narrative_ontology:cs_reading_relation('59b21e29-2e74-476b-8a8f-894dbaebcf95', fair_use_statutory_exception__market_licensing_reading, coexists_with).
narrative_ontology:cs_axiom('59b21e29-2e74-476b-8a8f-894dbaebcf95', foundational, transformative_use_is_public_good).
narrative_ontology:cs_axiom_status(transformative_use_is_public_good, holdable).
narrative_ontology:cs_axiom_grounding('59b21e29-2e74-476b-8a8f-894dbaebcf95', transformative_use_is_public_good, deontological).
narrative_ontology:cs_axiom('59b21e29-2e74-476b-8a8f-894dbaebcf95', foundational, innovation_requires_building_on_prior_works).
narrative_ontology:cs_axiom_status(innovation_requires_building_on_prior_works, holdable).
narrative_ontology:cs_axiom_grounding('59b21e29-2e74-476b-8a8f-894dbaebcf95', innovation_requires_building_on_prior_works, empirically_contingent).
narrative_ontology:cs_reference_frame('59b21e29-2e74-476b-8a8f-894dbaebcf95', constitutional_balance_of_incentives).
narrative_ontology:cs_drift_state('59b21e29-2e74-476b-8a8f-894dbaebcf95', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('59b21e29-2e74-476b-8a8f-894dbaebcf95', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, transformative_creators).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, public_domain).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, innovation_economy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fair_use_statutory_exception__transformative_right_reading, copyright_holders).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__transformative_right_reading, free_speech_doctrine).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__transformative_right_reading, innovation_incentive_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Artists, writers, coders, and educators who build new works by commenting on, parodying, or otherwise transforming existing copyrighted material. They benefit from the legal space to create without needing prior permission or paying licensing fees, but still face litigation risk.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, transformative_creators, beneficiary,
    moderate, biographical, constrained, global).

% Interpret and apply the fair use doctrine, shaping its scope through case law. This reading emphasizes their role in fostering innovation and cultural production by broadly construing transformative use.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, courts_and_judges, agenda_setter,
    institutional, generational, analytical, national).

% Owners of original copyrighted works who see their exclusive rights limited by fair use. Under this reading, they bear the cost of reduced control over their works when those works are used transformatively, but retain rights against substitutive uses.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, copyright_holders, payer,
    powerful, generational, constrained, global).

% The body of creative works and knowledge that is available for free use by everyone. This reading expands the public domain by allowing more works to be built upon without permission, enriching the commons.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, public_domain, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(fair_use_statutory_exception__transformative_right_reading, public_domain).

% Entities that facilitate the licensing of copyrighted works. This reading diminishes their potential market by reducing the need for licenses for transformative uses, effectively excluding them from a segment of potential transactions.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, licensing_market_operators, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Balances the incentive for creators to produce original works with the public's interest in free expression and the creation of new, transformative works, preventing copyright from becoming a barrier to cultural progress.
% TRANSFER_FUNCTION: Transfers the right to reuse copyrighted material for transformative purposes from the copyright holder to the transformative creator, without monetary compensation, in exchange for the public benefit of new cultural production.
% ABSENT_VOICES: Advocates for a pure property rights view of copyright, who would argue that any use without permission is an infringement and that fair use should be abolished or severely curtailed. They are often excluded from the interpretive process that prioritizes transformative use.
% DISAPPEARANCE_RATIONALE: If this reading of fair use vanished, transformative creation would largely cease due to prohibitive licensing costs and litigation risk, stifling innovation and cultural commentary. Copyright holders would gain absolute control, but the public would lose access to a vibrant ecosystem of derivative works.
% FOUNDING_PROBLEM: Copyright law, while incentivizing creation, could stifle subsequent creativity and public discourse if not balanced by exceptions for socially beneficial uses like criticism, commentary, news reporting, teaching, scholarship, or research.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, artists' rights organizations, and technology innovators consistently attest that the tension between copyright protection and new creation remains a live and evolving problem, requiring a robust fair use doctrine. This is corroborated by ongoing litigation and legislative debates from outside the immediate beneficiaries of copyright.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__transformative_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__transformative_right_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__transformative_right_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(fair_use_statutory_exception__transformative_right_reading, 'none', 1).

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
 *   The extractiveness (0.25) is relatively low because this reading aims to minimize the burden on transformative creators, treating the 'cost' to copyright holders as a necessary trade-off for public benefit. Suppression (0.3) is also low, as the doctrine is meant to enable, not restrict, new uses, though litigation risk remains a form of soft suppression. Theater ratio (0.1) is low, reflecting that the doctrine is actively applied and serves its stated function, with little performative maintenance. The slight increase in extractiveness and suppression towards the end of the interval reflects ongoing legal challenges and the increasing commercial value of transformative works, leading to more contested boundaries.
 *
 * PERSPECTIVAL GAP:
 *   Transformative creators experience this as a genuine 'rope' or even a 'mountain' (a natural right to build on culture), while copyright holders may experience it as a 'snare' or 'tangled rope' that extracts value from their property. Courts, as agenda setters, aim for a 'rope' classification, balancing interests. The engine's classification will reflect these divergences based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Transformative creators and the public domain are clear beneficiaries (low d) as the constraint enables their activities. Copyright holders are targets (high d) as their exclusive rights are curtailed. Courts are agenda setters, mediating the balance. Licensing market operators are excluded, as this reading reduces the scope for their services.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading actively resists mandatrophy by continuously re-evaluating the balance between copyright protection and innovation. Its persistence is tied to the ongoing need to facilitate new cultural production in the face of evolving technologies and creative practices. The 'contested' status of the founding problem reflects this ongoing dynamic, where the constraint's function is actively debated but remains relevant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformative_vs_substitutive_ambiguity,
    'How clearly can courts distinguish between genuinely transformative uses and merely substitutive uses that harm the market for the original work?',
    'Empirical studies on market impact of various ''transformative'' uses, or clearer legislative guidance on the definition of ''transformative''.',
    'If the distinction is consistently clear, the constraint functions as intended (low extraction for beneficiaries). If ambiguous, it introduces uncertainty and litigation risk, increasing effective extraction for creators and making it more ''tangled_rope''-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_vs_substitutive_ambiguity, empirical, 'Ambiguity in applying the ''transformative use'' criterion.').

omega_variable(
    market_licensing_dispositivity,
    'To what extent should the existence of a licensing market for a particular use preclude a fair use defense?',
    'Supreme Court rulings clarifying the weight of market harm in fair use analysis, or legislative amendments to the Copyright Act.',
    'If market availability becomes highly dispositive (as in the ''market_licensing_reading''), this reading''s extractiveness would increase significantly for creators, shifting it towards a ''snare''. If market availability is deemed less relevant, it remains a ''rope''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(market_licensing_dispositivity, conceptual, 'The role of licensing markets in fair use determination.').

omega_variable(
    burden_of_proof_allocation,
    'Who bears the primary burden of proof in fair use cases: the plaintiff (copyright holder) to show infringement, or the defendant (transformative creator) to show fair use?',
    'Clearer judicial precedent or legislative clarification on evidentiary standards in fair use litigation.',
    'If the burden shifts heavily to the defendant, it increases the cost and risk of transformative creation, effectively raising suppression and extractiveness, pushing the constraint towards a ''tangled_rope''. If the burden is shared or on the plaintiff, it supports the ''rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burden_of_proof_allocation, preference, 'Allocation of litigation burden in fair use cases.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__transformative_right_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1976, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 1976, 0.1).
narrative_ontology:measurement(fair_tr_t1990, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(fair_tr_t2000, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(fair_tr_t2010, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(fair_tr_t2024, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(fair_be_t1976, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 1976, 0.3).
narrative_ontology:measurement(fair_be_t1990, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 1990, 0.28).
narrative_ontology:measurement(fair_be_t2000, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 2000, 0.25).
narrative_ontology:measurement(fair_be_t2010, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 2010, 0.23).
narrative_ontology:measurement(fair_be_t2024, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1976, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 1976, 0.4).
narrative_ontology:measurement(fair_su_t1990, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(fair_su_t2000, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 2000, 0.3).
narrative_ontology:measurement(fair_su_t2010, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 2010, 0.28).
narrative_ontology:measurement(fair_su_t2024, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__transformative_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, copyright_term_extension).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, digital_millennium_copyright_act).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'fair_use_statutory_exception' kernel, alongside 'narrow_defense_reading' and 'market_licensing_reading'. Each reading represents a distinct structural constraint with different extractiveness and beneficiary/victim profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
