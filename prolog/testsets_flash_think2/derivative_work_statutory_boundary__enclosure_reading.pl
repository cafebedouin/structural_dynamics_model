% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__enclosure_reading, []).

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
 *   constraint_id: derivative_work_statutory_boundary__enclosure_reading
 *   human_readable: Derivative Work Statutory Boundary (Enclosure Reading)
 *   domain: intellectual_property_law/technology_governance/information_economics
 *
 * SUMMARY:
 *   This constraint is the 'enclosure reading' of the
 *   'derivative_work_statutory_boundary' kernel, which broadly interprets any
 *   use of copyrighted expression in creating new work as constituting the
 *   preparation of a derivative work. This interpretation leads to high
 *   extraction, as licensing requirements are enforced pre-creation,
 *   downstream innovation is bottlenecked, and extraction flows primarily to
 *   incumbent copyright holders. Sibling readings include
 *   'coordination_reading' (a narrower interpretation favoring transformative
 *   use) and 'hybrid_carveout_reading' (a context-dependent interpretation
 *   with carveouts for non-commercial transformative use).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__enclosure_reading, 0.85).
domain_priors:suppression_score(derivative_work_statutory_boundary__enclosure_reading, 0.75).
domain_priors:theater_ratio(derivative_work_statutory_boundary__enclosure_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__enclosure_reading, snare).
narrative_ontology:human_readable(derivative_work_statutory_boundary__enclosure_reading, "Derivative Work Statutory Boundary (Enclosure Reading)").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__enclosure_reading, "intellectual_property_law/technology_governance/information_economics").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__enclosure_reading, '87593726-cf69-4178-b743-6a798dfc1993').
narrative_ontology:cs_kernel_codification('87593726-cf69-4178-b743-6a798dfc1993', formalized).
narrative_ontology:cs_authority_grounding('87593726-cf69-4178-b743-6a798dfc1993', lineage).
narrative_ontology:cs_interpretation_layer_present('87593726-cf69-4178-b743-6a798dfc1993').
narrative_ontology:cs_reading_relation('87593726-cf69-4178-b743-6a798dfc1993', derivative_work_statutory_boundary__coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('87593726-cf69-4178-b743-6a798dfc1993', derivative_work_statutory_boundary__hybrid_carveout_reading, forecloses).
narrative_ontology:cs_axiom('87593726-cf69-4178-b743-6a798dfc1993', foundational, maximal_control_over_expression).
narrative_ontology:cs_axiom_status(maximal_control_over_expression, holdable).
narrative_ontology:cs_axiom_grounding('87593726-cf69-4178-b743-6a798dfc1993', maximal_control_over_expression, conventional).
narrative_ontology:cs_axiom('87593726-cf69-4178-b743-6a798dfc1993', secondary, incentive_through_exclusive_rights).
narrative_ontology:cs_axiom_status(incentive_through_exclusive_rights, holdable).
narrative_ontology:cs_axiom_grounding('87593726-cf69-4178-b743-6a798dfc1993', incentive_through_exclusive_rights, instrumental).
narrative_ontology:cs_reference_frame('87593726-cf69-4178-b743-6a798dfc1993', maximal_control_over_expression).
narrative_ontology:cs_drift_state('87593726-cf69-4178-b743-6a798dfc1993', contemporary_digital_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('87593726-cf69-4178-b743-6a798dfc1993', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, incumbent_copyright_holders).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, new_creators).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, ai_developers).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, public_domain_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, digital_platforms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These entities hold extensive portfolios of copyrighted works and actively enforce a broad interpretation of derivative work, requiring licenses for any new creation that draws upon their existing expression. They benefit from expanded control and revenue streams.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, incumbent_copyright_holders, agenda_setter,
    institutional, generational, arbitrage, global).

% Artists, writers, musicians, and other creators who wish to build upon or transform existing works. Under this reading, they face significant legal hurdles, licensing costs, or outright prohibitions, bottlenecking their creative output and limiting their market access.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, new_creators, payer,
    moderate, biographical, constrained, global).

% Companies and researchers developing artificial intelligence models that learn from vast datasets, often including copyrighted material. This broad interpretation of derivative work creates immense legal and financial risk, requiring extensive licensing for training data and potentially for AI-generated outputs.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, ai_developers, payer,
    powerful, biographical, constrained, global).

% Legal scholars, activists, and organizations who argue for a narrower interpretation of copyright, emphasizing the importance of a rich public domain and fair use for cultural progress. Their arguments are often marginalized in legislative and judicial processes dominated by incumbent interests.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, public_domain_advocates, excluded,
    organized, generational, analytical, global).

% These bodies are responsible for interpreting and enacting intellectual property law. While theoretically neutral, they are often influenced by lobbying from incumbent copyright holders, leading to interpretations that favor broad control.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, courts_and_legislatures, agenda_setter,
    institutional, generational, analytical, national).

% Companies that host and distribute digital content. While they face some liability, a clear (even if broad) legal framework allows them to implement automated content identification and licensing systems, mediating transactions and often taking a cut. They benefit from the stability of the legal regime.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, digital_platforms, beneficiary,
    powerful, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__enclosure_reading, incumbent_copyright_holders).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__enclosure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, albeit expansive, legal definition for what constitutes a 'derivative work,' aiming to coordinate the rights of original creators with those who wish to build upon existing expression, thereby incentivizing original creation.
% TRANSFER_FUNCTION: Transfers control and potential revenue streams from new creators and AI developers to incumbent copyright holders, requiring licensing fees or permission for any new work deemed to incorporate or be based on existing copyrighted expression.
% ABSENT_VOICES: Open-source communities, remix artists, and advocates for transformative use and a robust public domain are largely excluded from the legal and policy-making processes that shape this broad interpretation. Their arguments for a more permissive environment for new creation are often overridden by incumbent interests.
% DISAPPEARANCE_RATIONALE: If this broad interpretation of derivative work vanished overnight, the entire creative and digital economy would undergo a profound reorganization. Licensing requirements for new works would collapse, leading to an explosion of derivative and transformative content, while incumbent copyright holders would lose significant revenue and control, forcing a re-evaluation of their business models.
% FOUNDING_PROBLEM: The original problem was to ensure that creators could profit not only from their initial work but also from subsequent adaptations and transformations, thereby providing a strong incentive for creative output and investment in new works.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent copyright holders and their industry associations assert that the founding problem of incentivizing creation remains live and requires strong, broad protections. However, new creators, AI developers, and independent legal scholars (outside the benefiting parties) argue that the current interpretation overshoots this original intent, stifles innovation, and creates new problems of enclosure in the digital age, suggesting the problem's status is contested.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__enclosure_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(derivative_work_statutory_boundary__enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__enclosure_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(derivative_work_statutory_boundary__enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(derivative_work_statutory_boundary__enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.85) because this broad interpretation grants extensive control to copyright holders, allowing them to demand licenses for a wide range of new creative activities, often at rates decoupled from the marginal cost of the original work. Suppression is also high (0.75) due to the legal and technical enforcement mechanisms (e.g., DMCA takedowns, content ID systems) that actively prevent or penalize unauthorized derivative creation. The theater ratio is low (0.15) because the enforcement directly serves the extractive function, with little performative maintenance. Accessibility collapse is high (0.80) as the legal framework severely limits alternatives to seeking permission. Resistance is moderate (0.55) reflecting ongoing legal challenges and public debate, but direct non-compliance is risky.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of incumbent copyright holders, this constraint is a necessary 'rope' that incentivizes creation and protects investment. From the perspective of new creators and AI developers, it operates as a 'snare,' stifling innovation and extracting rents. The engine's classification will reflect the latter due to the high extraction and suppression metrics, despite the claimed coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent copyright holders are the primary beneficiaries and agenda-setters, collecting revenue and shaping legal interpretations. New creators and AI developers are the primary targets/payers, bearing the costs of licensing and facing constrained exit options due to the ubiquity of copyrighted material. Digital platforms benefit from a clear framework that allows them to mediate licensing. Public domain advocates are structurally excluded from the decision-making processes.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    derivative_work_definition_ambiguity,
    'Is the statutory definition of ''derivative work'' inherently broad, or is its current expansive interpretation a result of judicial and legislative capture by incumbent interests?',
    'Comparative legal analysis of international copyright regimes, historical analysis of legislative intent, and empirical study of judicial decision-making patterns over time.',
    'If the definition is found to be inherently ambiguous and its breadth a result of interpretation, it strengthens arguments for legislative reform or judicial re-interpretation towards a narrower scope, potentially reclassifying the constraint towards a ''tangled_rope'' or ''rope'' for new creators. If it''s found to be inherently broad, the ''snare'' classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(derivative_work_definition_ambiguity, conceptual, 'Ambiguity in the legal definition of ''derivative work'' and its interpretation.').

omega_variable(
    innovation_incentive_vs_bottleneck,
    'Does the broad interpretation of derivative work genuinely incentivize new creation, or does it primarily bottleneck downstream innovation and creativity?',
    'Empirical studies comparing innovation rates and creative output in jurisdictions with different derivative work standards, and economic analysis of the impact of licensing costs on new ventures.',
    'If the broad interpretation is shown to stifle innovation, it undermines the primary justification for the constraint, reinforcing its ''snare'' classification and supporting calls for reform. If it demonstrably boosts overall creation, it would lend credence to a ''tangled_rope'' or ''rope'' framing, though the extractive component would remain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_incentive_vs_bottleneck, empirical, 'The actual impact of broad derivative work rights on overall creative output and innovation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__enclosure_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(deri_tr_t8, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(deri_tr_t16, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 16, 0.13).
narrative_ontology:measurement(deri_tr_t24, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 24, 0.14).
narrative_ontology:measurement(deri_tr_t32, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 32, 0.15).
narrative_ontology:measurement(deri_tr_t40, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(deri_be_t8, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 8, 0.75).
narrative_ontology:measurement(deri_be_t16, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 16, 0.8).
narrative_ontology:measurement(deri_be_t24, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 24, 0.83).
narrative_ontology:measurement(deri_be_t32, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 32, 0.84).
narrative_ontology:measurement(deri_be_t40, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 40, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t0, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(deri_su_t8, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 8, 0.65).
narrative_ontology:measurement(deri_su_t16, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(deri_su_t24, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 24, 0.73).
narrative_ontology:measurement(deri_su_t32, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 32, 0.74).
narrative_ontology:measurement(deri_su_t40, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 40, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__enclosure_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
