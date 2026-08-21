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
 *   constraint_id: fair_use_statutory_exception__market_licensing_reading
 *   human_readable: Fair Use as Market Harm Doctrine
 *   domain: intellectual_property_law/legal_interpretation/information_economics
 *
 * SUMMARY:
 *   This constraint represents a specific legal interpretation of the fair
 *   use doctrine in intellectual property law, where any potential market for
 *   a copyrighted work (including derivative or transformative uses) is
 *   considered to be harmed by unlicensed use. Consequently, fair use is
 *   severely restricted, existing only where no market for licensing could
 *   conceivably be established. This reading prioritizes the economic
 *   interests of copyright holders, often at the expense of transformative
 *   creativity and public access. The high extractiveness and suppression
 *   reflect the active enforcement of this market-centric view.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__market_licensing_reading, 0.85).
domain_priors:suppression_score(fair_use_statutory_exception__market_licensing_reading, 0.9).
domain_priors:theater_ratio(fair_use_statutory_exception__market_licensing_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__market_licensing_reading, snare).
narrative_ontology:human_readable(fair_use_statutory_exception__market_licensing_reading, "Fair Use as Market Harm Doctrine").
narrative_ontology:topic_domain(fair_use_statutory_exception__market_licensing_reading, "intellectual_property_law/legal_interpretation/information_economics").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__market_licensing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__market_licensing_reading, '99e2b60b-e595-46c8-8e51-0e366da39910').
narrative_ontology:cs_kernel_codification('99e2b60b-e595-46c8-8e51-0e366da39910', fixed_text).
narrative_ontology:cs_authority_grounding('99e2b60b-e595-46c8-8e51-0e366da39910', lineage).
narrative_ontology:cs_interpretation_layer_present('99e2b60b-e595-46c8-8e51-0e366da39910').
narrative_ontology:cs_reading_relation('99e2b60b-e595-46c8-8e51-0e366da39910', fair_use_statutory_exception__transformative_right_reading, forecloses).
narrative_ontology:cs_reading_relation('99e2b60b-e595-46c8-8e51-0e366da39910', fair_use_statutory_exception__narrow_defense_reading, influences).
narrative_ontology:cs_axiom('99e2b60b-e595-46c8-8e51-0e366da39910', foundational, potential_market_is_actual_market_harm).
narrative_ontology:cs_axiom_status(potential_market_is_actual_market_harm, holdable).
narrative_ontology:cs_axiom_grounding('99e2b60b-e595-46c8-8e51-0e366da39910', potential_market_is_actual_market_harm, empirically_contingent).
narrative_ontology:cs_axiom('99e2b60b-e595-46c8-8e51-0e366da39910', foundational, copyright_holder_absolute_control_over_derivative_markets).
narrative_ontology:cs_axiom_status(copyright_holder_absolute_control_over_derivative_markets, holdable).
narrative_ontology:cs_axiom_grounding('99e2b60b-e595-46c8-8e51-0e366da39910', copyright_holder_absolute_control_over_derivative_markets, conventional).
narrative_ontology:cs_reference_frame('99e2b60b-e595-46c8-8e51-0e366da39910', copyright_as_absolute_property).
narrative_ontology:cs_drift_state('99e2b60b-e595-46c8-8e51-0e366da39910', contemporary_digital_licensing_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('99e2b60b-e595-46c8-8e51-0e366da39910', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__market_licensing_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, copyright_holders).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, licensing_agencies).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, transformative_creators).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, public_domain_advocates).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, educational_institutions).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, digital_platforms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and benefit from this interpretation, which maximizes their control over all potential markets for their copyrighted works, including derivative and transformative uses. They actively enforce their rights through litigation and licensing.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, copyright_holders, agenda_setter,
    institutional, generational, arbitrage, global).

% Profit directly from the expansion of licensing requirements. This reading creates a market for every potential use, increasing their revenue streams and justifying their existence as intermediaries.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, licensing_agencies, beneficiary,
    organized, biographical, arbitrage, global).

% Bear the costs of this interpretation, facing increased legal risk and licensing fees for uses that might otherwise be considered fair. Their ability to innovate and create new works based on existing culture is severely hampered.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, transformative_creators, payer,
    moderate, biographical, constrained, global).

% Argue against this interpretation, seeing it as an erosion of the public domain and a barrier to cultural progress. Their arguments for balancing copyright with public interest are often marginalized in legal discourse favoring market protection.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, public_domain_advocates, excluded,
    organized, generational, analytical, global).

% Face significant challenges in using copyrighted materials for teaching and research without incurring licensing costs or legal risks. This interpretation restricts their ability to disseminate knowledge freely.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, educational_institutions, payer,
    organized, generational, constrained, national).

% Are pressured to implement robust content filtering and licensing mechanisms to avoid liability for user-generated content that might infringe on potential markets. This increases their operational costs and limits user expression.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, digital_platforms, payer,
    powerful, biographical, constrained, global).

% Are the primary interpreters and enforcers of this doctrine. Their rulings shape the scope of fair use, and this reading reflects a judicial tendency to prioritize copyright holders' economic interests.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, albeit restrictive, framework for copyright holders to monetize their works and control all potential derivative uses, ensuring a predictable revenue stream for creators and industries.
% TRANSFER_FUNCTION: Transfers potential revenue from any unlicensed use (including transformative or educational uses) to copyright holders and licensing agencies, by eliminating fair use as a viable alternative to licensing.
% ABSENT_VOICES: Transformative creators, educators, and public interest groups who advocate for a broader interpretation of fair use are structurally marginalized by this reading. Their arguments for cultural production and public access are often subordinated to market protection.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished overnight, fair use would significantly expand, leading to a surge in transformative and educational uses without licensing. Licensing markets would contract, and the digital cultural landscape would reorganize around greater freedom of expression and innovation.
% FOUNDING_PROBLEM: To protect the economic incentives of creators by ensuring they can control and profit from all potential uses of their work, thereby fostering creativity and investment in cultural production.
% FOUNDING_PROBLEM_CORROBORATION: Copyright industry groups and some legal scholars attest that the problem of incentivizing creators remains live and requires strong market protection. Public interest groups, educators, and other scholars dispute this, arguing that this interpretation stifles creativity and public access, indicating the founding problem is either solved or the solution has become extractive.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__market_licensing_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__market_licensing_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__market_licensing_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(fair_use_statutory_exception__market_licensing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__market_licensing_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is high because this interpretation effectively converts nearly all potential uses into licensable events, maximizing revenue for copyright holders. Suppression is also high due to the active legal and technological enforcement mechanisms (e.g., DMCA takedowns, content ID systems) that prevent unlicensed uses. The theater ratio is low because this interpretation is actively and functionally enforced, not merely maintained for show. Accessibility collapse is high as alternatives to licensing are systematically eliminated. Resistance is significant from creators and public interest groups, but often unsuccessful against the entrenched legal framework.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of copyright holders, this interpretation is a necessary 'rope' that coordinates incentives and protects investment. From the perspective of transformative creators and educators, it operates as a 'snare,' extracting value and suppressing innovation under the guise of market protection. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright holders and licensing agencies are clear beneficiaries, as this reading expands their revenue and control. Transformative creators, public domain advocates, educational institutions, and digital platforms are victims, bearing the costs of licensing or legal risk, and facing restricted access to cultural materials. Courts act as agenda-setters by interpreting and enforcing this doctrine.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    potential_vs_actual_market_harm,
    'Does the mere existence of a ''potential'' licensing market equate to ''actual'' market harm from unlicensed use, or does actual harm require a demonstrated impact on an existing market?',
    'Empirical economic studies that differentiate between hypothetical market opportunities and measurable losses in established markets, or judicial rulings that require evidence of actual market displacement.',
    'If actual harm is required, the extractiveness of this constraint would decrease, and its classification might shift towards a ''tangled_rope'' or ''rope'' as fair use expands. If potential harm is sufficient, the current ''snare'' classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(potential_vs_actual_market_harm, empirical, 'Ambiguity in defining ''market harm'' for fair use analysis.').

omega_variable(
    fair_use_purpose_ambiguity,
    'Is the primary purpose of fair use to correct market failures and promote cultural production, or to serve as a narrow exception to property rights that primarily protects the copyright holder''s market?',
    'Legislative clarification of the intent behind fair use, or a shift in judicial philosophy towards prioritizing public interest and transformative creativity over market control.',
    'If fair use is primarily for cultural production, this constraint''s high extractiveness and suppression would be seen as illegitimate, pushing for reclassification. If it''s a narrow property exception, the current ''snare'' classification is consistent with its function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fair_use_purpose_ambiguity, conceptual, 'Conceptual disagreement over the fundamental purpose of fair use.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (legal barriers, technological enforcement) or internalized (creators self-censoring due to fear of litigation)?',
    'Surveys of creators'' behavior and legal advice, or analysis of the chilling effect of litigation on creative output. If suppression persists after legal barriers are reduced, it suggests internalization.',
    'If internalized suppression is a significant factor, the constraint''s effective suppression is higher than the structural measure suggests, as creators carry the suppression with them even in less restrictive legal environments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in intellectual property.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__market_licensing_reading, 2000, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t2000, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(fair_tr_t2005, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 2005, 0.11).
narrative_ontology:measurement(fair_tr_t2010, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(fair_tr_t2015, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(fair_tr_t2020, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(fair_tr_t2025, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(fair_be_t2000, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 2000, 0.7).
narrative_ontology:measurement(fair_be_t2005, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 2005, 0.75).
narrative_ontology:measurement(fair_be_t2010, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 2010, 0.8).
narrative_ontology:measurement(fair_be_t2015, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 2015, 0.82).
narrative_ontology:measurement(fair_be_t2020, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 2020, 0.84).
narrative_ontology:measurement(fair_be_t2025, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 2025, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t2000, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(fair_su_t2005, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 2005, 0.8).
narrative_ontology:measurement(fair_su_t2010, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 2010, 0.85).
narrative_ontology:measurement(fair_su_t2015, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 2015, 0.87).
narrative_ontology:measurement(fair_su_t2020, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 2020, 0.89).
narrative_ontology:measurement(fair_su_t2025, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 2025, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
