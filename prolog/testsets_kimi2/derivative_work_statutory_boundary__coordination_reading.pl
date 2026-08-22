% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__coordination_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: derivative_work_statutory_boundary__coordination_reading
 *   human_readable: Derivative Work Boundary â Coordination Reading
 *   domain: intellectual_property_law/technology_governance/information_economics
 *
 * SUMMARY:
 *   This constraint instantiates the coordination_reading of the
 *   derivative_work_statutory_boundary kernel. The kernel is the statutory
 *   definition of 'derivative work' in 17 U.S.C. Â§ 101 and the associated
 *   case law interpreting it. This reading holds that the kernel's legitimate
 *   authority extends only to fixed recastings substantially incorporating
 *   original expression, leaving transformative and intermediate
 *   usesâincluding machine learning trainingânon-infringing. Sibling
 *   readings include the enclosure_reading (any use of copyrighted expression
 *   in creating new work constitutes preparation of a derivative work) and
 *   the hybrid_carveout_reading (the boundary varies by commercial
 *   exploitation, requiring authorization for commercial transformative use).
 *
 * KEY AGENTS:
 *   - Federal judiciary (institutional/analytical): agenda-setter interpreting the statutory boundary through case law.
 *   - Transformative creators (moderate/constrained): beneficiaries relying on narrow boundary for remix and commentary.
 *   - ML training operators (powerful/arbitrage): beneficiaries depending on non-expressive use theory for model training.
 *   - Generative tech sector (organized/constrained): beneficiaries deploying generative systems under transformative use shelter.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__coordination_reading, 0.22).
domain_priors:suppression_score(derivative_work_statutory_boundary__coordination_reading, 0.28).
domain_priors:theater_ratio(derivative_work_statutory_boundary__coordination_reading, 0.16).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, theater_ratio, 0.16).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__coordination_reading, rope).
narrative_ontology:human_readable(derivative_work_statutory_boundary__coordination_reading, "Derivative Work Boundary â Coordination Reading").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__coordination_reading, "intellectual_property_law/technology_governance/information_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__coordination_reading, '9c1d9781-8768-4c42-8c2d-10be21dead96').
narrative_ontology:cs_kernel_codification('9c1d9781-8768-4c42-8c2d-10be21dead96', fixed_text).
narrative_ontology:cs_authority_grounding('9c1d9781-8768-4c42-8c2d-10be21dead96', lineage).
narrative_ontology:cs_interpretation_layer_present('9c1d9781-8768-4c42-8c2d-10be21dead96').
narrative_ontology:cs_reading_relation('9c1d9781-8768-4c42-8c2d-10be21dead96', derivative_work_statutory_boundary__enclosure_reading, forecloses).
narrative_ontology:cs_reading_relation('9c1d9781-8768-4c42-8c2d-10be21dead96', derivative_work_statutory_boundary__hybrid_carveout_reading, forecloses).
narrative_ontology:cs_axiom('9c1d9781-8768-4c42-8c2d-10be21dead96', foundational, statutory_boundary_tracks_fixation).
narrative_ontology:cs_axiom_status(statutory_boundary_tracks_fixation, holdable).
narrative_ontology:cs_axiom_grounding('9c1d9781-8768-4c42-8c2d-10be21dead96', statutory_boundary_tracks_fixation, conventional).
narrative_ontology:cs_axiom('9c1d9781-8768-4c42-8c2d-10be21dead96', foundational, transformative_use_categorically_permissible).
narrative_ontology:cs_axiom_status(transformative_use_categorically_permissible, holdable).
narrative_ontology:cs_axiom_grounding('9c1d9781-8768-4c42-8c2d-10be21dead96', transformative_use_categorically_permissible, conventional).
narrative_ontology:cs_reference_frame('9c1d9781-8768-4c42-8c2d-10be21dead96', expression_based_derivative_boundary).
narrative_ontology:cs_drift_state('9c1d9781-8768-4c42-8c2d-10be21dead96', generative_ai_emergence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9c1d9781-8768-4c42-8c2d-10be21dead96', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, transformative_creators).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, ml_training_operators).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, generative_tech_sector).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the statutory derivative work definition and fair use doctrine through case law, setting the effective boundary between licensable fixed recastings and permissible transformative or intermediate uses. Authority derives from continuity with the Copyright Act text and precedent.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Create remixes, appropriation art, commentary, and parody relying on the narrow statutory boundary to avoid ex-ante licensing for non-substitutive transformations; their creative practice depends on legal clearance.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, transformative_creators, beneficiary,
    moderate, biographical, constrained, national).

% Build large machine learning models by training on copyrighted corpora under the shelter of the non-expressive use theory; a broader derivative boundary would impose prohibitive mass-licensing costs and fragment training data access.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, ml_training_operators, beneficiary,
    powerful, biographical, arbitrage, global).

% Deploy generative systems whose outputs may incidentally resemble training data; the coordination reading provides a legal framework in which non-substantial, transformative generation is treated as non-infringing.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, generative_tech_sector, beneficiary,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Delineates a predictable legal boundary that allows follow-on creators and technology developers to know in advance which uses require permission and which do not, solving the coordination problem of sequential innovation under copyright without case-by-case bargaining over every recombinant or intermediate use.
% TRANSFER_FUNCTION: Moves legal clearance to engage in transformative recombination, intermediate copying, and non-expressive machine learning from the domain of licensable property claims to the domain of unencumbered use, transferring creative and computational freedom downstream.
% ABSENT_VOICES: Original rightsholder collectives and their representatives object that the narrow boundary devalues their property; they are vocal in litigation and legislative hearings but structurally positioned as opposing the coordination frame rather than as seated payers within it.
% DISAPPEARANCE_RATIONALE: If the coordination reading vanished and were replaced by the enclosure reading, mass machine learning would require fragmented licensing, generative technologies would face prohibitive clearance costs, and transformative remix practices would retreat to jurisdictions with clearer safe harbors; the information-economy would reorganize around ex-ante permission.
% FOUNDING_PROBLEM: Early copyright doctrine risked overbroad property rights that chilled follow-on speech, sequential innovation, and unforeseen recombinant technologies by requiring licenses for every use incorporating prior expression.
% FOUNDING_PROBLEM_CORROBORATION: Independent legal scholars and technology policy researchers outside the beneficiary set attest that overbroad derivative rights chill innovation and speech; rightsholder collectives attest the problem is insufficient control. No uncontested neutral corroboration exists.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(derivative_work_statutory_boundary__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__coordination_reading, 0.22, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__coordination_reading_tests).
:- end_tests(derivative_work_statutory_boundary__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.22) because the coordination reading exempts the vast majority of recombinant and intermediate uses from licensing. Suppression is low (0.28) because enforcement targets only fixed recastings, leaving transformative and intermediate uses unsuppressed. Theater ratio is low (0.16) because judicial opinions in this line are largely functional interpretive work rather than performative maintenance. Accessibility collapse is moderate (0.50): once the boundary is understood, creators can navigate it, but legal uncertainty around generative AI leaves some alternatives unmapped. Resistance is moderate (0.42) because rightsholder collectives actively litigate for broader boundaries, though the reading assesses this as advocacy for extraction rather than legitimate opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the federal judiciary's seat, the constraint is a neutral statutory interpretation framework. From the beneficiary seats, it is a necessary legal clearance enabling entire creative and technological sectors. From the opposing enclosure-advocate perspective, the same framework appears as an unjust confiscation of property rights. The engine computes this divergence from structural data; the authored claim (rope) reflects the assessment that the coordination function dominates and extractive overhead is minimal.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal judiciary sits near symmetric as agenda-setter (interpreting the statute, d â 0.5). Downstream beneficiariesâtransformative creators, ML operators, and the generative tech sectorâsit near the beneficiary end (d â 0.2) because the constraint subsidizes their activity by removing licensing friction. No full targets are structurally declared because the reading frames foregone licensing revenue on transformative uses as illegitimate overreach rather than as extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by distinguishing the legitimate coordination functionâpreventing market substitution via direct recastingâfrom extractionâdemanding licenses for non-substitutive transformative or intermediate use. The coordination reading's founding problem, chilling effects from overbroad rights, remains live in contemporary technological context, preventing piton classification. The absence of a sunset clause and the ongoing judicial maintenance prevent scaffold classification despite the 'coordination scaffold' language sometimes used descriptively for generative technologies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    generative_ai_training_status,
    'Does machine learning training on copyrighted works constitute a fixed recasting substantially incorporating original expression, or an intermediate non-expressive use falling outside the derivative work boundary?',
    'Judicial resolution in pending and future litigation (e.g., Andersen v. Stability AI, Getty Images v. Stability AI) or legislative clarification of the statutory definition in the context of generative AI.',
    'If ML training is classified as fixed recasting, extractiveness would rise sharply and the coordination reading''s applicability to generative technologies would collapse; if classified as non-expressive intermediate use, the reading''s low-Îµ rope structure is preserved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generative_ai_training_status, conceptual, 'Uncertainty about whether ML training is a derivative work under the coordination reading').

omega_variable(
    transformativeness_standard_stability,
    'Will federal courts maintain the categorical transformativeness standard of Campbell v. Acuff-Rose, or revert toward market-substitution analysis that narrows the coordination reading?',
    'Tracking of appellate and Supreme Court copyright jurisprudence over the next decade, particularly in cases involving appropriation art and commercial generative outputs.',
    'A reversion to market-substitution analysis would increase effective extraction by requiring licenses for commercially valuable transformative uses, shifting the constraint toward tangled_rope or snare territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformativeness_standard_stability, empirical, 'Stability of the transformativeness standard against market-substitution pressures').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__coordination_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(deri_tr_t10, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(deri_tr_t20, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(deri_tr_t30, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement(deri_tr_t40, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 40, 0.14).
narrative_ontology:measurement(deri_tr_t50, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 50, 0.16).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(deri_be_t10, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 10, 0.11).
narrative_ontology:measurement(deri_be_t20, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 20, 0.13).
narrative_ontology:measurement(deri_be_t30, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 30, 0.15).
narrative_ontology:measurement(deri_be_t40, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 40, 0.18).
narrative_ontology:measurement(deri_be_t50, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 50, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(derivative_work_statutory_boundary__coordination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__coordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary__enclosure_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary__hybrid_carveout_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the derivative_work_statutory_boundary kernel. The coordination reading (low Îµ, rope) decomposes from the enclosure reading (high Îµ, claimed as snare) and hybrid carveout reading (medium Îµ, claimed as tangled_rope) by fixing the referent to the statutory text's fixation-based definition and transformative use doctrine.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
