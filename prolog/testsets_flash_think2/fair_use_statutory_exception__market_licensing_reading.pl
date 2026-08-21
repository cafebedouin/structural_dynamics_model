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
 *   domain: Intellectual Property Law / Legal Interpretation / Information Economics
 *
 * SUMMARY:
 *   This constraint represents a specific, highly restrictive interpretation
 *   of the fair use doctrine in intellectual property law, asserting that any
 *   use that *could* be licensed (i.e., for which a market *could* exist)
 *   inherently harms the market for licensed uses, thereby negating a fair
 *   use defense. This reading effectively collapses fair use to only de
 *   minimis or unmonetizable uses, prioritizing copyright holders' market
 *   control over other public interests. It is one reading of the broader
 *   'fair_use_statutory_exception' kernel.
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
narrative_ontology:constraint_claim(fair_use_statutory_exception__market_licensing_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_statutory_exception__market_licensing_reading, "Fair Use as Market Harm Doctrine").
narrative_ontology:topic_domain(fair_use_statutory_exception__market_licensing_reading, "Intellectual Property Law / Legal Interpretation / Information Economics").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__market_licensing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__market_licensing_reading, 'b40b04f2-76e7-48b9-b894-4b5fcd31b9e9').
narrative_ontology:cs_kernel_codification('b40b04f2-76e7-48b9-b894-4b5fcd31b9e9', fixed_text).
narrative_ontology:cs_authority_grounding('b40b04f2-76e7-48b9-b894-4b5fcd31b9e9', lineage).
narrative_ontology:cs_interpretation_layer_present('b40b04f2-76e7-48b9-b894-4b5fcd31b9e9').
narrative_ontology:cs_reading_relation('b40b04f2-76e7-48b9-b894-4b5fcd31b9e9', fair_use_statutory_exception__transformative_right_reading, forecloses).
narrative_ontology:cs_reading_relation('b40b04f2-76e7-48b9-b894-4b5fcd31b9e9', fair_use_statutory_exception__narrow_defense_reading, coexists_with).
narrative_ontology:cs_axiom('b40b04f2-76e7-48b9-b894-4b5fcd31b9e9', foundational, potential_market_harm_is_actual_harm).
narrative_ontology:cs_axiom_status(potential_market_harm_is_actual_harm, holdable).
narrative_ontology:cs_axiom_grounding('b40b04f2-76e7-48b9-b894-4b5fcd31b9e9', potential_market_harm_is_actual_harm, empirically_contingent).
narrative_ontology:cs_axiom('b40b04f2-76e7-48b9-b894-4b5fcd31b9e9', foundational, copyright_is_absolute_property).
narrative_ontology:cs_axiom_status(copyright_is_absolute_property, holdable).
narrative_ontology:cs_axiom_grounding('b40b04f2-76e7-48b9-b894-4b5fcd31b9e9', copyright_is_absolute_property, deontological).
narrative_ontology:cs_reference_frame('b40b04f2-76e7-48b9-b894-4b5fcd31b9e9', copyright_as_property_maximization).
narrative_ontology:cs_drift_state('b40b04f2-76e7-48b9-b894-4b5fcd31b9e9', contemporary_digital_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b40b04f2-76e7-48b9-b894-4b5fcd31b9e9', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__market_licensing_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, copyright_holders).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, licensing_agencies).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, transformative_creators).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, public_domain_advocates).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, educational_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and benefit from this interpretation, which maximizes their control over all potential markets for their works. They initiate litigation to enforce this view and influence legislative efforts.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, copyright_holders, agenda_setter,
    institutional, generational, arbitrage, global).

% Profit from the expansion of licensing requirements, as this reading creates a market for nearly every use, increasing their revenue streams and justifying their existence.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, licensing_agencies, beneficiary,
    organized, biographical, mobile, global).

% Bear the costs of this interpretation, facing increased legal risk and licensing fees for uses they believe should fall under fair use. Their ability to create new works by building on existing ones is severely hampered.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, transformative_creators, payer,
    moderate, biographical, constrained, global).

% Argue against this interpretation, seeing it as an erosion of the public domain and a barrier to cultural progress. They are often marginalized in legal and policy discussions dominated by copyright holder interests.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, public_domain_advocates, excluded,
    organized, generational, analytical, global).

% Face significant challenges in using copyrighted materials for teaching and research, as this reading forces them to seek licenses for many uses previously considered fair. This increases costs and administrative burden.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, educational_institutions, payer,
    organized, biographical, constrained, national).

% Are the primary interpreters and enforcers of this doctrine. While some judges may resist, this reading provides a clear, market-centric framework that can be appealing for its apparent simplicity in adjudication.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate the market for copyrighted works by establishing that all potential uses are subject to licensing, thereby providing a clear framework for transactions and compensation to creators.
% TRANSFER_FUNCTION: Transfers potential economic value from any unlicensed use (regardless of its transformative nature or public benefit) to copyright holders, by asserting that such use inherently harms a potential market.
% ABSENT_VOICES: Transformative creators, public domain advocates, and open access movements are largely excluded from the legal and policy discourse that entrenches this reading. They would argue for a robust fair use doctrine that prioritizes cultural production and public access over market control.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, the landscape of intellectual property would fundamentally shift. Fair use would expand significantly, reducing licensing requirements for many uses, particularly transformative ones. This would lead to a surge in creative reuse, lower costs for education and research, and a re-evaluation of copyright holders' revenue models, reorganizing the information economy.
% FOUNDING_PROBLEM: To protect the economic incentive of creators by ensuring they control all potential markets for their work, preventing free riders from undermining the value of copyrighted content.
% FOUNDING_PROBLEM_CORROBORATION: Copyright holder associations and their legal representatives consistently attest that the threat of market harm from unlicensed uses is live and growing, especially in the digital age. However, independent legal scholars and economists often contest the empirical basis for 'potential market harm' in many contexts, arguing that some fair uses can actually expand overall market value or create new markets.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__market_licensing_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__market_licensing_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__market_licensing_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   The extractiveness is very high (0.85) because this reading expands the scope of what constitutes 'harm' to a potential market, forcing licensing for a vast array of uses. Suppression is also very high (0.90) as it actively discourages and legally challenges any unlicensed use that could conceivably be monetized, effectively collapsing alternatives for creators and educators. The theater ratio is low (0.10) because this interpretation is actively and effectively enforced through litigation and licensing demands, serving its beneficiaries directly. Resistance is high (0.70) due to ongoing legal challenges and advocacy from groups promoting broader fair use.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of copyright holders, this reading is a necessary coordination mechanism to ensure market stability and creator compensation. From the perspective of transformative creators and educators, it is a snare that stifles innovation and access to knowledge. The engine's classification will highlight this divergence by computing different effective extraction values for each seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright holders and licensing agencies are clear beneficiaries, as this reading maximizes their revenue and control. Transformative creators and educational institutions are primary targets, facing increased costs and legal risks. Public domain advocates are excluded, as their arguments for broader public access are fundamentally at odds with this market-centric view. Courts act as agenda setters, interpreting and enforcing the doctrine, often swayed by the clear economic arguments presented by beneficiaries.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_harm_empirical_basis,
    'Does every potential licensed use truly harm the market for licensed uses, or do some fair uses (e.g., transformative works) actually expand or create new markets?',
    'Longitudinal empirical studies on the economic impact of various fair use applications, distinguishing between direct substitution and market expansion/creation.',
    'If empirical evidence shows that certain fair uses do not harm or even benefit markets, this reading''s justification would be undermined, potentially leading to a reclassification towards a less extractive type. If the ''harm'' is consistently demonstrated, it would reinforce this reading''s claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_harm_empirical_basis, empirical, 'Empirical validity of the ''potential market harm'' premise.').

omega_variable(
    fair_use_conceptual_framing,
    'Is fair use primarily a market protection mechanism for copyright holders, or is it a user right designed to facilitate cultural production and public access?',
    'Legal and philosophical analysis of the legislative history and constitutional underpinnings of copyright, weighing economic incentives against public benefit and free speech principles.',
    'If fair use is framed primarily as a user right, this reading''s high extraction and suppression would be seen as fundamentally illegitimate, pushing classification towards a Snare. If framed as market protection, the current classification as a Tangled Rope (coordination + extraction) would be more robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fair_use_conceptual_framing, conceptual, 'Core conceptual framing of fair use doctrine.').

omega_variable(
    kernel_reading_divergence,
    'How would the classification of fair use change if the ''transformative_right_reading'' or ''narrow_defense_reading'' of the fair_use_statutory_exception kernel were adopted?',
    'Comparative analysis of legal outcomes and economic impacts under different dominant interpretations of fair use.',
    'The ''transformative_right_reading'' would likely result in significantly lower extractiveness and suppression, potentially classifying fair use as a Rope or even a Scaffold (if seen as temporary support for innovation). The ''narrow_defense_reading'' would likely yield similar, though slightly less extreme, extractiveness and suppression to this reading, but with a clearer focus on direct market substitution rather than potential market harm.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Impact of alternative kernel readings on fair use classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__market_licensing_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1980, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(fair_tr_t1990, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(fair_tr_t2000, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(fair_tr_t2010, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(fair_tr_t2024, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(fair_be_t1980, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(fair_be_t1990, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 1990, 0.7).
narrative_ontology:measurement(fair_be_t2000, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement(fair_be_t2010, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 2010, 0.82).
narrative_ontology:measurement(fair_be_t2024, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1980, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(fair_su_t1990, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 1990, 0.75).
narrative_ontology:measurement(fair_su_t2000, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 2000, 0.83).
narrative_ontology:measurement(fair_su_t2010, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 2010, 0.87).
narrative_ontology:measurement(fair_su_t2024, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__market_licensing_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, copyright_term_extension).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, digital_millennium_copyright_act_enforcement).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'fair_use_statutory_exception' kernel. Its high extractiveness and suppression are in direct contrast to the 'transformative_right_reading' of the same kernel, which would exhibit significantly lower extraction. It is a more extreme version of the 'narrow_defense_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
