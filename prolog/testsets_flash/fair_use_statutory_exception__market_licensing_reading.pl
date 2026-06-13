% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__market_licensing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_statutory_exception__market_licensing_reading, []).

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
 *   constraint_id: fair_use_statutory_exception__market_licensing_reading
 *   human_readable: Fair Use Statutory Exception (Market Licensing Reading)
 *   domain: intellectual_property_law/legal_interpretation/information_economics
 *
 * SUMMARY:
 *   This constraint represents the 'market licensing' reading of the fair use
 *   statutory exception in intellectual property law. Under this
 *   interpretation, any potential use of copyrighted material that could
 *   conceivably be licensed is deemed to harm the market for licensed uses,
 *   thereby precluding a fair use defense. Fair use is thus restricted to
 *   situations where no market, actual or potential, exists for the use in
 *   question. This reading effectively transforms fair use from a balancing
 *   test into a market-centric gate, leading to extremely high extraction and
 *   suppression for creators and educators.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__market_licensing_reading, 0.85).
domain_priors:suppression_score(fair_use_statutory_exception__market_licensing_reading, 0.75).
domain_priors:theater_ratio(fair_use_statutory_exception__market_licensing_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__market_licensing_reading, snare).
narrative_ontology:human_readable(fair_use_statutory_exception__market_licensing_reading, "Fair Use Statutory Exception (Market Licensing Reading)").
narrative_ontology:topic_domain(fair_use_statutory_exception__market_licensing_reading, "intellectual_property_law/legal_interpretation/information_economics").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__market_licensing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__market_licensing_reading, 'f7975f87-0589-4c4f-8826-1e2ec8a69fdc').
narrative_ontology:cs_kernel_codification('f7975f87-0589-4c4f-8826-1e2ec8a69fdc', fixed_text).
narrative_ontology:cs_authority_grounding('f7975f87-0589-4c4f-8826-1e2ec8a69fdc', lineage).
narrative_ontology:cs_interpretation_layer_present('f7975f87-0589-4c4f-8826-1e2ec8a69fdc').
narrative_ontology:cs_reading_relation('f7975f87-0589-4c4f-8826-1e2ec8a69fdc', fair_use_statutory_exception__transformative_right_reading, forecloses).
narrative_ontology:cs_reading_relation('f7975f87-0589-4c4f-8826-1e2ec8a69fdc', fair_use_statutory_exception__narrow_defense_reading, influences).
narrative_ontology:cs_axiom('f7975f87-0589-4c4f-8826-1e2ec8a69fdc', foundational, any_potential_market_harm_precludes_fair_use).
narrative_ontology:cs_axiom_status(any_potential_market_harm_precludes_fair_use, holdable).
narrative_ontology:cs_axiom_grounding('f7975f87-0589-4c4f-8826-1e2ec8a69fdc', any_potential_market_harm_precludes_fair_use, conventional).
narrative_ontology:cs_axiom('f7975f87-0589-4c4f-8826-1e2ec8a69fdc', secondary, copyright_is_absolute_property_right).
narrative_ontology:cs_axiom_status(copyright_is_absolute_property_right, holdable).
narrative_ontology:cs_axiom_grounding('f7975f87-0589-4c4f-8826-1e2ec8a69fdc', copyright_is_absolute_property_right, deontological).
narrative_ontology:cs_reference_frame('f7975f87-0589-4c4f-8826-1e2ec8a69fdc', market_maximization_copyright_framework).
narrative_ontology:cs_drift_state('f7975f87-0589-4c4f-8826-1e2ec8a69fdc', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f7975f87-0589-4c4f-8826-1e2ec8a69fdc', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__market_licensing_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, copyright_holders).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, licensing_agencies).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, transformative_creators).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, educational_institutions).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, public_domain_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and benefit from the broadest possible interpretation of copyright protection, viewing any potential unlicensed use as a market harm. They actively enforce their rights through litigation and licensing, capturing revenue from a wide range of uses.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, copyright_holders, agenda_setter,
    institutional, generational, arbitrage, global).

% Profit from facilitating licensing transactions. This reading expands the scope of licensable uses, thereby increasing their potential market and revenue. They support interpretations that favor market-based solutions over exceptions.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, licensing_agencies, beneficiary,
    organized, biographical, mobile, global).

% Seek to reuse copyrighted material in new, transformative ways. Under this reading, they face significant legal risk and licensing costs, as almost any use could be deemed to harm a potential market. Their creative output is stifled or made economically unviable.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, transformative_creators, payer,
    moderate, biographical, constrained, global).

% Rely on fair use for teaching, research, and scholarship. This reading forces them to seek licenses for many activities previously considered fair use, increasing operational costs and limiting access to materials for students and faculty.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, educational_institutions, payer,
    organized, generational, constrained, national).

% Argue for a robust public domain and broad exceptions to copyright to foster creativity and access to knowledge. This reading effectively shrinks the public domain and fair use, undermining their core mission and making their advocacy largely ineffective within this legal framework.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, public_domain_advocates, excluded,
    powerless, civilizational, trapped, global).

% Are tasked with interpreting copyright law, including fair use. Under this reading, their role shifts towards identifying potential markets and enforcing licensing, rather than balancing competing interests or fostering transformative uses. They become enforcers of market logic.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, if restrictive, framework for copyright holders to monetize their works by establishing a presumption of market harm for any unlicensed use, thereby reducing ambiguity in licensing negotiations.
% TRANSFER_FUNCTION: Transfers potential revenue from creators and users of copyrighted works to copyright holders and licensing agencies, by expanding the scope of licensable uses and narrowing the fair use exception.
% ABSENT_VOICES: The voices of cultural commons advocates, open access proponents, and those who believe in a robust public domain are effectively absent from the legal discourse shaped by this reading, as their arguments for non-market values are systematically de-prioritized.
% DISAPPEARANCE_RATIONALE: If this reading of fair use vanished, the landscape of intellectual property would immediately shift. Copyright holders would lose a powerful tool for asserting market harm, leading to more expansive fair use claims, reduced licensing revenue, and a significant increase in transformative and educational uses without prior permission. The entire information economy would need to re-evaluate its monetization strategies.
% FOUNDING_PROBLEM: The original fair use doctrine was intended to balance copyright protection with the public interest in promoting scholarship and creativity, preventing copyright from stifling innovation.
% FOUNDING_PROBLEM_CORROBORATION: Copyright holders and licensing agencies argue the problem is live, asserting that without this reading, their ability to monetize and protect their works would be severely undermined. However, transformative creators, legal scholars, and public interest groups (outside the benefiting parties) widely corroborate that the original problem of balancing interests has been superseded by a focus on market maximization, rendering the founding problem 'dead' in its original spirit.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__market_licensing_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__market_licensing_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__market_licensing_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(fair_use_statutory_exception__market_licensing_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__market_licensing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_statutory_exception__market_licensing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_statutory_exception__market_licensing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because this reading maximizes the scope of licensable activity, channeling revenue to copyright holders and licensing agencies. Suppression (0.75) is high due to the chilling effect on transformative and educational uses, as the risk of litigation for 'market harm' becomes pervasive. The theater ratio (0.1) is low because the enforcement of this reading is direct and effective, with little performative overhead; it genuinely functions to extract. Accessibility collapse is high (0.8) because alternatives to licensing (i.e., fair use) are severely curtailed. Resistance is moderate (0.6) from creators and educators, but often insufficient to overturn this dominant legal interpretation.
 *
 * PERSPECTIVAL GAP:
 *   Copyright holders and licensing agencies perceive this reading as a legitimate and necessary enforcement of property rights, ensuring creators are compensated. Transformative creators and educational institutions experience it as a snare, stifling innovation and access to knowledge by imposing prohibitive costs and legal risks. Courts, when adopting this reading, act as enforcers of market logic, which is a different role than balancing public interest.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright holders and licensing agencies are clear beneficiaries, as this reading expands their revenue streams and control (low d). Transformative creators and educational institutions are clear victims, bearing the costs of licensing or litigation (high d). Public domain advocates are excluded, their arguments for non-market values systematically marginalized. Courts, when adopting this reading, become agenda-setters for the market-centric view.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading exemplifies a form of mandatrophy where the original mandate of fair use (balancing copyright with public interest) has atrophied, replaced by a de facto mandate of market maximization. The constraint is not a Piton because it is actively and effectively enforced, generating substantial revenue for beneficiaries. It is a Snare because the coordination story (clarity for market transactions) is cover for asymmetric extraction, suppressing alternatives and creating identifiable victims. The classification prevents mislabeling it as a Rope (genuine coordination) or a Mountain (natural market law) by highlighting its coercive, extractive nature and the active suppression required for its persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    potential_market_definition,
    'How broadly should ''potential market'' be defined? Does it include markets that would only exist if fair use were eliminated?',
    'Judicial clarification or legislative amendment explicitly defining the scope of ''potential market'' and whether it can be created by the very act of restricting fair use.',
    'A narrow definition would allow more uses to qualify as fair use, reducing extraction; a broad definition entrenches this reading, increasing extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(potential_market_definition, conceptual, 'Ambiguity in defining ''potential market'' as a basis for market harm.').

omega_variable(
    market_vs_transformative_value,
    'Is the value of transformative reuse adequately captured by market mechanisms, or does it represent a distinct public good that requires non-market protection?',
    'Empirical studies on the economic and cultural impact of transformative works, and policy debates on the role of non-market cultural production.',
    'If transformative value is distinct and significant, this reading''s high extraction would be seen as a net social loss, potentially leading to reclassification towards a Tangled Rope or even a Piton if the ''market harm'' becomes purely theatrical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_vs_transformative_value, empirical, 'Whether market logic fully accounts for the value of fair use.').

omega_variable(
    reading_vs_kernel_ambiguity,
    'Is this ''market_licensing_reading'' a genuine interpretation of the fair use statute, or has it become a de facto amendment that overrides the statute''s original intent?',
    'Legislative review and potential amendment of the fair use statute, or a Supreme Court ruling that explicitly rejects this reading''s foundational premises.',
    'If deemed an override, the constraint''s legitimacy would collapse, and it would be reclassified as a Snare, with the ''interpretation'' serving as pure cover for extraction. If upheld as a valid interpretation, its status as a Snare would be reinforced by legal precedent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_kernel_ambiguity, conceptual, 'Whether the reading is an interpretation or an effective amendment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__market_licensing_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1980, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(fair_tr_t1990, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(fair_tr_t2000, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(fair_tr_t2010, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(fair_tr_t2024, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(fair_be_t1980, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 1980, 0.4).
narrative_ontology:measurement(fair_be_t1990, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(fair_be_t2000, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 2000, 0.7).
narrative_ontology:measurement(fair_be_t2010, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 2010, 0.8).
narrative_ontology:measurement(fair_be_t2024, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1980, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 1980, 0.3).
narrative_ontology:measurement(fair_su_t1990, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(fair_su_t2000, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(fair_su_t2010, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(fair_su_t2024, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__market_licensing_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, copyright_term_extension).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, digital_rights_management_enforcement).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'fair_use_statutory_exception' kernel. Its high extractiveness and suppression are in stark contrast to the 'transformative_right_reading' (a Rope or Scaffold) and the 'narrow_defense_reading' (a Tangled Rope), which are separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
