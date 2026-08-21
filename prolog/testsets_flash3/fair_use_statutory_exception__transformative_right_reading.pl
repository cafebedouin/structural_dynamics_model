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
 *   This constraint represents a specific legal reading of the fair use
 *   doctrine (17 U.S.C. § 107) that emphasizes its role in enabling
 *   transformative reuse and cultural production. It views fair use not
 *   merely as a defense against infringement, but as an affirmative right
 *   that courts must actively facilitate to promote innovation. This reading
 *   prioritizes the 'transformative' nature of the new work and downplays the
 *   significance of potential licensing markets in fair use analysis. It is
 *   one reading of the 'fair_use_statutory_exception' kernel, distinct from
 *   more restrictive interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__transformative_right_reading, 0.25).
domain_priors:suppression_score(fair_use_statutory_exception__transformative_right_reading, 0.35).
domain_priors:theater_ratio(fair_use_statutory_exception__transformative_right_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 0.35).
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
narrative_ontology:cs_story_uid(fair_use_statutory_exception__transformative_right_reading, 'de3f93a7-2ae2-4308-9d49-2688ad48991b').
narrative_ontology:cs_kernel_codification('de3f93a7-2ae2-4308-9d49-2688ad48991b', fixed_text).
narrative_ontology:cs_authority_grounding('de3f93a7-2ae2-4308-9d49-2688ad48991b', lineage).
narrative_ontology:cs_interpretation_layer_present('de3f93a7-2ae2-4308-9d49-2688ad48991b').
narrative_ontology:cs_reading_relation('de3f93a7-2ae2-4308-9d49-2688ad48991b', fair_use_statutory_exception__narrow_defense_reading, coexists_with).
narrative_ontology:cs_reading_relation('de3f93a7-2ae2-4308-9d49-2688ad48991b', fair_use_statutory_exception__market_licensing_reading, coexists_with).
narrative_ontology:cs_axiom('de3f93a7-2ae2-4308-9d49-2688ad48991b', foundational, transformative_use_is_public_good).
narrative_ontology:cs_axiom_status(transformative_use_is_public_good, holdable).
narrative_ontology:cs_axiom_grounding('de3f93a7-2ae2-4308-9d49-2688ad48991b', transformative_use_is_public_good, deontological).
narrative_ontology:cs_axiom('de3f93a7-2ae2-4308-9d49-2688ad48991b', foundational, innovation_trumps_potential_market).
narrative_ontology:cs_axiom_status(innovation_trumps_potential_market, holdable).
narrative_ontology:cs_axiom_grounding('de3f93a7-2ae2-4308-9d49-2688ad48991b', innovation_trumps_potential_market, instrumental).
narrative_ontology:cs_reference_frame('de3f93a7-2ae2-4308-9d49-2688ad48991b', constitutional_progress_clause).
narrative_ontology:cs_drift_state('de3f93a7-2ae2-4308-9d49-2688ad48991b', contemporary_digital_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('de3f93a7-2ae2-4308-9d49-2688ad48991b', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, transformative_creators).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, public_domain_advocates).
narrative_ontology:constraint_victim(fair_use_statutory_exception__transformative_right_reading, copyright_holders_of_original_works).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Artists, educators, critics, and innovators who reuse copyrighted material in new contexts, creating works that comment on, critique, or build upon the original. They benefit from the legal space to create without needing prior permission or paying licensing fees, fostering cultural production and innovation.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, transformative_creators, beneficiary,
    moderate, biographical, constrained, global).

% Organizations and individuals who champion the expansion and accessibility of the public domain, viewing fair use as a crucial mechanism for balancing private rights with public access to knowledge and culture. They benefit from interpretations that prioritize transformative use over strict property rights.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, public_domain_advocates, beneficiary,
    organized, generational, analytical, global).

% Authors, artists, publishers, and corporations who own original copyrighted works. They bear the cost of this reading by having their exclusive rights limited, potentially losing licensing revenue for uses deemed transformative, and facing a higher burden to prove market harm.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, copyright_holders_of_original_works, payer,
    powerful, generational, constrained, global).

% The judicial system responsible for interpreting and applying copyright law, including the fair use doctrine. This reading positions them as facilitators of innovation and cultural production, requiring them to weigh the public interest in transformative use against private property rights.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, courts_and_judges, agenda_setter,
    institutional, civilizational, analytical, national).

% Entities that facilitate the licensing of copyrighted works. This reading diminishes their role by making the existence of a potential licensing market less determinative in fair use analysis, thereby reducing their potential revenue and influence.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, licensing_market_operators, excluded,
    organized, biographical, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the balance between copyright holders' exclusive rights and the public's interest in free expression and cultural progress, enabling new creative works to emerge from existing ones without stifling innovation.
% TRANSFER_FUNCTION: Transfers the right to reuse copyrighted material for transformative purposes from the copyright holder to the transformative creator, without requiring monetary compensation or prior permission.
% ABSENT_VOICES: Strict property rights advocates and licensing market operators, who would argue for a narrower interpretation of fair use that prioritizes the copyright holder's control and potential market for their work. Their arguments are often marginalized in this reading.
% DISAPPEARANCE_RATIONALE: If this reading of fair use vanished, transformative creators would face increased legal risk and licensing costs, stifling innovation and cultural production. Copyright holders would gain greater control, but the public domain would shrink, fundamentally altering the landscape of creative industries and public access to culture.
% FOUNDING_PROBLEM: To balance the exclusive rights of copyright holders with the public interest in promoting the progress of science and useful arts, preventing copyright from becoming a barrier to new creation and knowledge dissemination.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, cultural critics, and technology innovators consistently corroborate the ongoing need for this balance, citing the rapid pace of digital creation and the importance of remix culture. This corroboration comes from outside the direct beneficiaries of this specific reading.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__transformative_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__transformative_right_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__transformative_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   Extractiveness is low (0.25) because this reading aims to minimize the burden on transformative creators, allowing them to use copyrighted material without significant cost. Suppression is moderate (0.35) as it still requires creators to navigate legal uncertainty and potential litigation, but the legal framework is designed to be more permissive. Theater ratio is low (0.1) because the doctrine's function is genuinely to balance rights, not to mask extraction. The metrics reflect a legal framework that, by its own lights, is intended to be a coordination mechanism for cultural production.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of transformative creators, this reading is a vital rope, enabling their work. From the perspective of copyright holders, it can feel like a snare, eroding their property rights. The courts, as agenda-setters, aim for a balanced rope, but their interpretation directly impacts the perceived type for other stakeholders. The engine will compute these divergent classifications based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Transformative creators and public domain advocates are clear beneficiaries, as this reading grants them greater freedom and reduces their costs. Copyright holders of original works are payers, as their exclusive rights are curtailed, potentially reducing their control and revenue. Courts act as agenda-setters, actively shaping the interpretation. Licensing market operators are excluded, as their business model is challenged by a broader fair use right.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading actively resists mandatrophy by re-centering fair use on its original purpose of promoting progress, rather than allowing it to atrophy into a mere technicality or be subsumed by market logic. It prevents mislabeling genuine cultural coordination as pure extraction by ensuring that the 'public benefit' aspect of copyright is actively enforced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformative_vs_substitutive_ambiguity,
    'How consistently can courts distinguish ''transformative'' uses (which this reading protects) from ''substitutive'' uses (which it does not)?',
    'Empirical analysis of judicial decisions over time, assessing the consistency and predictability of ''transformative'' rulings across different judges and circuits.',
    'If the distinction is consistently applied, the constraint functions as intended. If it''s highly ambiguous, the legal uncertainty increases suppression for creators and extractiveness for copyright holders (due to litigation costs), potentially shifting the constraint towards a tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_vs_substitutive_ambiguity, empirical, 'Ambiguity in applying the core ''transformative'' criterion.').

omega_variable(
    market_harm_relevance_ambiguity,
    'To what extent should the existence of a potential licensing market for a particular use influence a fair use determination, even for transformative uses?',
    'Legislative clarification or Supreme Court precedent explicitly defining the weight of market harm in transformative use cases.',
    'If market harm is deemed highly relevant, this reading''s low extractiveness for creators would increase, shifting it towards the ''market_licensing_reading'' and potentially a tangled_rope. If market harm is consistently de-emphasized, this reading''s character as a rope is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(market_harm_relevance_ambiguity, conceptual, 'The conceptual weight of market harm in fair use analysis.').

omega_variable(
    kernel_reading_divergence,
    'Is this ''transformative_right_reading'' genuinely distinct from other fair use interpretations, or does it merely represent a different emphasis within a broader, unified doctrine?',
    'Comparative legal analysis of judicial opinions, scholarly commentary, and legislative history across jurisdictions, focusing on whether the core axioms of this reading are truly irreconcilable with those of sibling readings.',
    'If the readings are fundamentally distinct, the kernel framework is validated. If they are merely stylistic variations, the ''fair_use_statutory_exception'' might be better modeled as a single constraint with internal tensions rather than a kernel with multiple readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Whether this reading constitutes a structurally distinct constraint or a variant of a single, broader constraint.').


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
narrative_ontology:measurement(fair_su_t1990, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 1990, 0.38).
narrative_ontology:measurement(fair_su_t2000, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement(fair_su_t2010, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 2010, 0.33).
narrative_ontology:measurement(fair_su_t2024, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__transformative_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, copyright_term_extension).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, digital_millennium_copyright_act).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'fair_use_statutory_exception' kernel. It is linked to 'narrow_defense_reading' and 'market_licensing_reading' as sibling interpretations of the same statutory text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
