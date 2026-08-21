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
 *   constraint_id: derivative_work_statutory_boundary__enclosure_reading
 *   human_readable: Derivative Work Statutory Boundary (Enclosure Reading)
 *   domain: intellectual_property_law/technology_governance/information_economics
 *
 * SUMMARY:
 *   This constraint represents the 'enclosure reading' of the derivative work
 *   statutory boundary, which interprets 'derivative work' broadly to include
 *   almost any use of copyrighted expression in creating new work. This
 *   reading is characterized by high extraction, as it mandates licensing
 *   requirements pre-creation, bottlenecks downstream innovation, and
 *   channels economic value to incumbent copyright holders. It is a specific
 *   interpretation of a contested legal kernel, distinct from more permissive
 *   readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__enclosure_reading, 0.85).
domain_priors:suppression_score(derivative_work_statutory_boundary__enclosure_reading, 0.9).
domain_priors:theater_ratio(derivative_work_statutory_boundary__enclosure_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__enclosure_reading, snare).
narrative_ontology:human_readable(derivative_work_statutory_boundary__enclosure_reading, "Derivative Work Statutory Boundary (Enclosure Reading)").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__enclosure_reading, "intellectual_property_law/technology_governance/information_economics").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__enclosure_reading, '97719934-1874-42af-ad24-9ab456cc4f30').
narrative_ontology:cs_kernel_codification('97719934-1874-42af-ad24-9ab456cc4f30', fixed_text).
narrative_ontology:cs_authority_grounding('97719934-1874-42af-ad24-9ab456cc4f30', extraction).
narrative_ontology:cs_interpretation_layer_present('97719934-1874-42af-ad24-9ab456cc4f30').
narrative_ontology:cs_reading_relation('97719934-1874-42af-ad24-9ab456cc4f30', derivative_work_statutory_boundary__coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('97719934-1874-42af-ad24-9ab456cc4f30', derivative_work_statutory_boundary__hybrid_carveout_reading, forecloses).
narrative_ontology:cs_axiom('97719934-1874-42af-ad24-9ab456cc4f30', foundational, author_absolute_control_over_transformation).
narrative_ontology:cs_axiom_status(author_absolute_control_over_transformation, holdable).
narrative_ontology:cs_axiom_grounding('97719934-1874-42af-ad24-9ab456cc4f30', author_absolute_control_over_transformation, conventional).
narrative_ontology:cs_reference_frame('97719934-1874-42af-ad24-9ab456cc4f30', maximal_author_control_framework).
narrative_ontology:cs_drift_state('97719934-1874-42af-ad24-9ab456cc4f30', contemporary_digital_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('97719934-1874-42af-ad24-9ab456cc4f30', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, incumbent_copyright_holders).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, licensing_agencies).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, ai_developers).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, transformative_artists).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, open_source_communities).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, independent_creators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit significantly from a broad interpretation of derivative work, allowing them to control and monetize nearly any subsequent use of their copyrighted expression in new creations. They actively enforce these rights through litigation and lobbying.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, incumbent_copyright_holders, agenda_setter,
    institutional, generational, arbitrage, global).

% Act as intermediaries, facilitating the licensing of copyrighted works. They profit from the necessity of obtaining licenses under this broad interpretation, collecting fees for their services.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, licensing_agencies, beneficiary,
    organized, biographical, mobile, global).

% Require access to vast datasets of copyrighted material for training AI models. Under this reading, they face prohibitive licensing costs or significant legal risks, bottlenecking innovation in AI.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, ai_developers, payer,
    organized, immediate, constrained, global).

% Create new artistic works that build upon, reinterpret, or sample existing copyrighted expressions. This interpretation forces them to seek expensive licenses or risk infringement lawsuits, stifling their creative output.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, transformative_artists, payer,
    moderate, biographical, constrained, global).

% Advocate for and rely on permissive licensing and free reuse of creative works. This broad derivative work interpretation directly conflicts with their ethos and operational models, creating legal uncertainty and barriers to collaborative creation.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, open_source_communities, payer,
    organized, generational, constrained, global).

% Lack the financial and legal resources to navigate complex licensing requirements or defend against infringement claims. This interpretation effectively bars them from creating works that draw inspiration from or incorporate elements of existing copyrighted material.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, independent_creators, payer,
    powerless, immediate, trapped, global).

% Analyze the legal, economic, and social implications of derivative work interpretations. They often highlight the tension between authorial control and the public interest in fostering new creation.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, legal_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to provide a clear legal framework for authors to control the transformation and adaptation of their original works, theoretically incentivizing creation by securing exclusive rights.
% TRANSFER_FUNCTION: Transfers control and economic value from those who wish to build upon existing copyrighted works to the original copyright holders, primarily through licensing fees and legal settlements.
% ABSENT_VOICES: Future innovators and creators who are deterred from creating new works due to the high legal risk and cost of licensing; their potential contributions are never realized under this restrictive interpretation.
% DISAPPEARANCE_RATIONALE: If this broad interpretation vanished, there would be a massive surge in new creative and technological works building on existing content, a collapse in licensing revenue for incumbents, and a rapid reorganization of creative industries around more permissive reuse models.
% FOUNDING_PROBLEM: To incentivize authors to create original works by granting them exclusive rights to control and profit from adaptations and transformations of their creations.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent copyright holders and their legal representatives assert the problem is live and this broad interpretation is essential for continued creation. Technology companies, AI developers, and legal scholars argue that the original problem is over-solved, and this interpretation now stifles innovation, citing economic studies and historical precedents of creative reuse.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__enclosure_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.85) because this interpretation grants broad control to original copyright holders, enabling them to demand significant licensing fees for a wide range of transformative uses. Suppression is also high (0.90) due to the legal threats, litigation costs, and chilling effect on creators who cannot afford licenses or legal defense. The theater ratio is low (0.10) as enforcement is active and directly serves the economic interests of beneficiaries, rather than being performative. The increasing extractiveness and suppression over time reflect the expanding scope of digital creation and the intensified enforcement efforts by incumbents.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of incumbent copyright holders, this interpretation is a necessary protection of their intellectual property and an incentive for creation. From the perspective of creators and innovators, it is an extractive barrier that stifles new works and concentrates control in the hands of a few.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent copyright holders and licensing agencies are clear beneficiaries, collecting rents and controlling access. AI developers, transformative artists, open-source communities, and independent creators are the primary targets, bearing the costs of licensing or legal risk. Legal scholars act as observers, analyzing the system's effects.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    innovation_incentive_vs_stifling,
    'Is this broad interpretation of derivative work a necessary incentive for original creation, or does it primarily function to stifle new works and concentrate control?',
    'Empirical studies on innovation rates, creative output, and market concentration under different derivative work regimes, comparing jurisdictions with varying interpretations.',
    'If found to stifle innovation, it would strongly support reclassification towards a Snare and advocate for policy changes favoring narrower interpretations. If found to be a necessary incentive, it would reinforce the current classification''s justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_incentive_vs_stifling, empirical, 'Ambiguity regarding the actual economic effect of broad derivative work rights on innovation.').

omega_variable(
    statutory_interpretation_coherence,
    'Is the ''enclosure reading'' the only coherent interpretation of the Copyright Act''s derivative work provisions, or are alternative readings (e.g., coordination_reading, hybrid_carveout_reading) equally defensible within the statutory text and legislative intent?',
    'In-depth legal textual analysis, examination of legislative history, and comparative analysis of judicial precedents from different legal traditions or historical periods.',
    'If alternative readings are found to be equally defensible, it highlights the conceptual contestability of the kernel and the policy choice embedded in this reading, suggesting that the current interpretation is not a ''natural law'' but a constructed legal outcome. If not, it strengthens this reading''s claim to textual fidelity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(statutory_interpretation_coherence, conceptual, 'Ambiguity regarding the inherent ''naturalness'' or inevitability of this specific legal interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__enclosure_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(deri_tr_t6, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 6, 0.1).
narrative_ontology:measurement(deri_tr_t12, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(deri_tr_t18, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 18, 0.1).
narrative_ontology:measurement(deri_tr_t24, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 24, 0.1).
narrative_ontology:measurement(deri_tr_t30, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(deri_be_t6, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 6, 0.75).
narrative_ontology:measurement(deri_be_t12, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 12, 0.8).
narrative_ontology:measurement(deri_be_t18, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 18, 0.82).
narrative_ontology:measurement(deri_be_t24, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 24, 0.84).
narrative_ontology:measurement(deri_be_t30, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 30, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t0, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(deri_su_t6, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 6, 0.8).
narrative_ontology:measurement(deri_su_t12, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 12, 0.85).
narrative_ontology:measurement(deri_su_t18, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 18, 0.87).
narrative_ontology:measurement(deri_su_t24, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 24, 0.89).
narrative_ontology:measurement(deri_su_t30, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 30, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__enclosure_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, fair_use_doctrine__restrictive_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, copyright_term_extension__incumbent_benefit_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
