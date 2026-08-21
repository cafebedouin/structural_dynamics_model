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
 *   human_readable: Fair Use as Transformative Right (Legal Reading)
 *   domain: intellectual_property_law/legal_interpretation/information_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'transformative_right_reading' of
 *   the 'fair_use_statutory_exception' kernel. This reading interprets fair
 *   use not merely as a defense against infringement, but as an affirmative
 *   right designed to enable transformative reuse and cultural production,
 *   with courts having a duty to facilitate innovation. It emphasizes the
 *   public benefit of new creative works over the exclusive control of
 *   copyright holders, particularly when the new work adds significant value
 *   or meaning.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__transformative_right_reading, 0.45).
domain_priors:suppression_score(fair_use_statutory_exception__transformative_right_reading, 0.55).
domain_priors:theater_ratio(fair_use_statutory_exception__transformative_right_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__transformative_right_reading, rope).
narrative_ontology:human_readable(fair_use_statutory_exception__transformative_right_reading, "Fair Use as Transformative Right (Legal Reading)").
narrative_ontology:topic_domain(fair_use_statutory_exception__transformative_right_reading, "intellectual_property_law/legal_interpretation/information_economics").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__transformative_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__transformative_right_reading, 'f1fe7661-c2ba-4379-9453-9abe27d45e4e').
narrative_ontology:cs_kernel_codification('f1fe7661-c2ba-4379-9453-9abe27d45e4e', fixed_text).
narrative_ontology:cs_authority_grounding('f1fe7661-c2ba-4379-9453-9abe27d45e4e', lineage).
narrative_ontology:cs_interpretation_layer_present('f1fe7661-c2ba-4379-9453-9abe27d45e4e').
narrative_ontology:cs_reading_relation('f1fe7661-c2ba-4379-9453-9abe27d45e4e', fair_use_statutory_exception__narrow_defense_reading, coexists_with).
narrative_ontology:cs_reading_relation('f1fe7661-c2ba-4379-9453-9abe27d45e4e', fair_use_statutory_exception__market_licensing_reading, forecloses).
narrative_ontology:cs_axiom('f1fe7661-c2ba-4379-9453-9abe27d45e4e', foundational, transformative_use_is_public_good).
narrative_ontology:cs_axiom_status(transformative_use_is_public_good, holdable).
narrative_ontology:cs_axiom_grounding('f1fe7661-c2ba-4379-9453-9abe27d45e4e', transformative_use_is_public_good, deontological).
narrative_ontology:cs_axiom('f1fe7661-c2ba-4379-9453-9abe27d45e4e', foundational, innovation_requires_unfettered_expression).
narrative_ontology:cs_axiom_status(innovation_requires_unfettered_expression, holdable).
narrative_ontology:cs_axiom_grounding('f1fe7661-c2ba-4379-9453-9abe27d45e4e', innovation_requires_unfettered_expression, instrumental).
narrative_ontology:cs_reference_frame('f1fe7661-c2ba-4379-9453-9abe27d45e4e', constitutional_balance_of_rights).
narrative_ontology:cs_drift_state('f1fe7661-c2ba-4379-9453-9abe27d45e4e', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f1fe7661-c2ba-4379-9453-9abe27d45e4e', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, transformative_creators).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, public_domain_advocates).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, innovation_economy).
narrative_ontology:constraint_victim(fair_use_statutory_exception__transformative_right_reading, copyright_holders).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__transformative_right_reading, innovation_incentive_theory).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__transformative_right_reading, cultural_commons_principle).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__transformative_right_reading, free_speech_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Artists, writers, musicians, and developers who reuse copyrighted material in new, transformative ways. They benefit from the legal protection fair use offers but face litigation risk and legal costs.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, transformative_creators, beneficiary,
    moderate, biographical, constrained, global).

% Individuals and corporations holding exclusive rights to copyrighted works. They bear the cost of diminished control over their works and potential loss of licensing revenue when fair use is successfully asserted against them. They actively litigate to narrow fair use.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, copyright_holders, payer,
    powerful, biographical, mobile, global).

% Judicial bodies responsible for interpreting and applying fair use law. This reading expects them to actively facilitate innovation by broadly construing transformative use and balancing interests.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% Organizations and individuals who champion the public's right to access and build upon existing culture. They benefit from a robust fair use doctrine that expands the cultural commons.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, public_domain_advocates, beneficiary,
    organized, generational, analytical, global).

% Entities that manage and collect royalties for copyrighted works. This reading's emphasis on transformative use and its rejection of market licensing as dispositive directly challenges their business model and scope of operation.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, licensing_agencies, excluded,
    powerful, biographical, constrained, global).

% Academics and legal experts who analyze, critique, and propose interpretations of fair use law. They provide the intellectual framework for this reading and document its practical effects.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, legal_scholars, observer,
    analytical, generational, analytical, universal).

% The broader ecosystem of technology companies, startups, and creative industries that rely on the ability to build new products and services by reusing existing information and cultural assets. A strong fair use doctrine reduces their legal risk and transaction costs.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, innovation_economy, beneficiary,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_statutory_exception__transformative_right_reading, diffuse).
narrative_ontology:fixing_cost_class(fair_use_statutory_exception__transformative_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the tension between copyright holders' exclusive rights and the public interest in free expression, innovation, and cultural progress, by providing a legal safety valve for transformative reuse.
% TRANSFER_FUNCTION: It transfers the right to use copyrighted material for transformative purposes from the copyright holder to the transformative creator, without requiring permission or payment, thereby limiting the scope of the copyright monopoly.
% ABSENT_VOICES: Those who believe that all uses of copyrighted material should require permission and payment, and that fair use is an unwarranted infringement on property rights. They are often represented by copyright holder lobbies but are structurally excluded from the *framing* of fair use as a right.
% DISAPPEARANCE_RATIONALE: If fair use vanished, the legal landscape for creative and technological innovation would fundamentally shift. Transformative creators would face prohibitive licensing costs or litigation risks, stifling new works. The public domain would shrink, and copyright holders would gain unprecedented control, leading to a less vibrant and dynamic cultural and information economy.
% FOUNDING_PROBLEM: To prevent copyright's exclusive rights from stifling subsequent creativity, scholarship, and public discourse, ensuring that the ultimate purpose of copyright—to promote the progress of science and useful arts—is served.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, technology policy experts, and advocates for open culture consistently corroborate that the tension between copyright and innovation remains a live and critical problem, especially in the digital age. Independent economic analyses also support the role of fair use in fostering economic growth and cultural production.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__transformative_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__transformative_right_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__transformative_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(fair_use_statutory_exception__transformative_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__transformative_right_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

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
 *   The base extractiveness (0.45) is moderate, reflecting the inherent friction and legal costs involved in asserting fair use, even when the doctrine is interpreted broadly. It's not zero because copyright holders still exert pressure. Suppression (0.55) is also moderate; while the legal right exists, the threat of litigation and the ambiguity of 'transformative' can still deter creators. Resistance (0.70) is high due to persistent lobbying and litigation by copyright holders seeking to narrow fair use. Theater ratio (0.15) is low, as the legal process is generally functional, though some arguments may be performative. The slight dip and then rise in extractiveness and suppression in measurements reflects periods of judicial expansion of fair use followed by counter-pressures from copyright holders, leading to a dynamic equilibrium.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of transformative creators, this reading of fair use is a vital rope, enabling their work. From the perspective of copyright holders, it can feel like a snare, eroding their property rights. The courts, as agenda-setters, navigate this tension, with this reading urging them to lean towards innovation. The engine will compute these divergent classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Transformative creators and public domain advocates are clear beneficiaries, as the constraint aims to enable their activities. The innovation economy also benefits from reduced friction. Copyright holders are the primary targets/payers, as their exclusive rights are limited by this interpretation. Courts act as agenda-setters, shaping the doctrine through their rulings. Licensing agencies are excluded, as this reading explicitly de-emphasizes the role of licensing markets.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading actively combats mandatrophy by re-centering fair use on its original purpose of promoting progress, rather than allowing it to atrophy into a mere technical defense. By emphasizing innovation, it seeks to keep the constraint's function live and relevant to contemporary cultural production, preventing it from becoming a piton of outdated legal formalism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the fair use kernel, or merely a policy preference within a single, unified fair use doctrine?',
    'Analysis of judicial opinions and scholarly discourse: if distinct interpretive methodologies and foundational axioms are consistently applied, it supports distinct readings.',
    'If a distinct reading, it justifies separate constraint stories and allows for tracking of inter-reading relations. If a policy preference, the kernel should be modeled as a single constraint with internal contestation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Distinguishing a kernel reading from a policy preference.').

omega_variable(
    transformative_vs_substitutive_ambiguity,
    'How consistently and predictably do courts distinguish between ''transformative'' and ''substitutive'' uses, especially in novel technological contexts?',
    'Empirical study of judicial outcomes across different circuits and technologies, assessing the variance in application of the ''transformative'' factor.',
    'If the distinction is highly ambiguous or inconsistently applied, the effective suppression for transformative creators is higher than measured, as legal uncertainty acts as a deterrent. If clear, the constraint functions more as intended.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_vs_substitutive_ambiguity, empirical, 'Clarity and consistency of ''transformative'' use interpretation.').

omega_variable(
    burden_of_proof_allocation,
    'Is the burden of proof for fair use truly shared, or does the practical reality of litigation disproportionately fall on transformative creators?',
    'Analysis of litigation costs, settlement patterns, and success rates for fair use defenses, particularly for independent creators versus large institutions.',
    'If the burden is effectively shifted to creators, the ''suppression'' metric is understated, as the cost of asserting the right acts as a barrier. This would push the constraint closer to a snare for smaller creators.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burden_of_proof_allocation, empirical, 'Practical allocation of fair use burden of proof.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__transformative_right_reading, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1990, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(fair_tr_t1995, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 1995, 0.12).
narrative_ontology:measurement(fair_tr_t2000, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 2000, 0.13).
narrative_ontology:measurement(fair_tr_t2005, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 2005, 0.14).
narrative_ontology:measurement(fair_tr_t2010, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(fair_tr_t2015, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 2015, 0.16).
narrative_ontology:measurement(fair_tr_t2020, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 2020, 0.15).

% Extraction over time
narrative_ontology:measurement(fair_be_t1990, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(fair_be_t1995, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 1995, 0.48).
narrative_ontology:measurement(fair_be_t2000, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 2000, 0.46).
narrative_ontology:measurement(fair_be_t2005, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 2005, 0.45).
narrative_ontology:measurement(fair_be_t2010, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 2010, 0.44).
narrative_ontology:measurement(fair_be_t2015, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 2015, 0.43).
narrative_ontology:measurement(fair_be_t2020, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 2020, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1990, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(fair_su_t1995, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 1995, 0.58).
narrative_ontology:measurement(fair_su_t2000, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 2000, 0.56).
narrative_ontology:measurement(fair_su_t2005, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 2005, 0.55).
narrative_ontology:measurement(fair_su_t2010, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 2010, 0.54).
narrative_ontology:measurement(fair_su_t2015, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 2015, 0.53).
narrative_ontology:measurement(fair_su_t2020, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 2020, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__transformative_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, copyright_term_extension).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, digital_millennium_copyright_act_enforcement).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, narrow_defense_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, market_licensing_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'fair_use_statutory_exception' kernel. This 'transformative_right_reading' emphasizes innovation and public benefit, contrasting with the 'narrow_defense_reading' and 'market_licensing_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
