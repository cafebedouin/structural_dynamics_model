% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__pragmatic_openness_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__pragmatic_openness_reading, []).

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
 *   constraint_id: software_control_legitimacy__pragmatic_openness_reading
 *   human_readable: Pragmatic Openness Reading of Software Control Legitimacy
 *   domain: software_engineering/political_economy/intellectual_property
 *
 * SUMMARY:
 *   This constraint story instantiates the pragmatic_openness_reading of the
 *   software_control_legitimacy kernel. Under this reading, software control
 *   is a development methodology choice: open source coordinates distributed
 *   peer review and collaboration to produce higher-quality software, while
 *   proprietary development remains a legitimate alternative. The kernel is
 *   contested among four readings: pragmatic openness (this file),
 *   freedom_imperative (proprietary is ethically illegitimate),
 *   property_rights (control is creator property), and commons (collective
 *   governance). This reading authors a low-extraction coordination structure
 *   with no victim set.
 *
 * KEY AGENTS:
 *   - open_source_contributors: Primary beneficiaries (moderate/mobile) â contribute to and draw on peer-reviewed code.
 *   - software_users: Primary beneficiaries (organized/mobile) â receive quality-optimized software.
 *   - proprietary_developers: Non-victim observers (moderate/mobile) â operate under a model treated as legitimate alternative.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__pragmatic_openness_reading, 0.18).
domain_priors:suppression_score(software_control_legitimacy__pragmatic_openness_reading, 0.12).
domain_priors:theater_ratio(software_control_legitimacy__pragmatic_openness_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__pragmatic_openness_reading, rope).
narrative_ontology:human_readable(software_control_legitimacy__pragmatic_openness_reading, "Pragmatic Openness Reading of Software Control Legitimacy").
narrative_ontology:topic_domain(software_control_legitimacy__pragmatic_openness_reading, "software_engineering/political_economy/intellectual_property").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__pragmatic_openness_reading, 'f75efe9c-83ab-46cf-b9dd-594e97968174').
narrative_ontology:cs_kernel_codification('f75efe9c-83ab-46cf-b9dd-594e97968174', distributed).
narrative_ontology:cs_authority_grounding('f75efe9c-83ab-46cf-b9dd-594e97968174', distributed).
narrative_ontology:cs_reading_relation('f75efe9c-83ab-46cf-b9dd-594e97968174', software_control_legitimacy__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('f75efe9c-83ab-46cf-b9dd-594e97968174', software_control_legitimacy__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('f75efe9c-83ab-46cf-b9dd-594e97968174', software_control_legitimacy__commons_reading, coexists_with).
narrative_ontology:cs_axiom('f75efe9c-83ab-46cf-b9dd-594e97968174', foundational, pragmatic_quality_superiority).
narrative_ontology:cs_axiom_status(pragmatic_quality_superiority, holdable).
narrative_ontology:cs_axiom_grounding('f75efe9c-83ab-46cf-b9dd-594e97968174', pragmatic_quality_superiority, empirically_contingent).
narrative_ontology:cs_axiom('f75efe9c-83ab-46cf-b9dd-594e97968174', foundational, methodological_pluralism).
narrative_ontology:cs_axiom_status(methodological_pluralism, holdable).
narrative_ontology:cs_axiom_grounding('f75efe9c-83ab-46cf-b9dd-594e97968174', methodological_pluralism, instrumental).
narrative_ontology:cs_reference_frame('f75efe9c-83ab-46cf-b9dd-594e97968174', pragmatic_methodology_optimization).
narrative_ontology:cs_drift_state('f75efe9c-83ab-46cf-b9dd-594e97968174', contemporary_software_ecosystem, gap(stable, minor, false)).
narrative_ontology:cs_created_at('f75efe9c-83ab-46cf-b9dd-594e97968174', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, open_source_contributors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, software_users).
narrative_ontology:constraint_vindicates(software_control_legitimacy__pragmatic_openness_reading, open_source_quality_hypothesis).
narrative_ontology:constraint_vindicates(software_control_legitimacy__pragmatic_openness_reading, methodological_diversity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Volunteer or paid developers who contribute code to open projects, participate in peer review, and benefit from shared debugging and reusable components. They choose projects freely and can exit to proprietary employment or other projects without penalty.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, open_source_contributors, beneficiary,
    moderate, biographical, mobile, global).

% Individuals and organizations that consume software. They benefit from higher-quality, more secure, and transparently auditable code produced by open collaborative processes. They retain choice between open and proprietary products.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, software_users, beneficiary,
    organized, biographical, mobile, global).

% Developers and firms that create proprietary software. Under this reading they are treated as legitimate alternatives rather than targets; they operate under a different methodology but are not suppressed or excluded from the market.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, proprietary_developers, observer,
    moderate, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates distributed software development by enabling asynchronous peer review, collaborative debugging, transparent source modification, and shared component reuse across organizational boundaries, solving quality-assurance and reliability problems in complex systems.
% TRANSFER_FUNCTION: Moves labor, technical attention, and code review effort from distributed contributors into publicly inspectable codebases; moves higher-reliability software artifacts and security transparency to users.
% ABSENT_VOICES: Freedom-imperative advocates who view proprietary software as ethically illegitimate, and strict property-rights maximalists who assert absolute creator control, are backgrounded in this framing; their positions are seated in sibling readings but not here.
% DISAPPEARANCE_RATIONALE: If the pragmatic openness framework vanished, distributed peer-review coordination would weaken, reusable open infrastructure would contract, and software development would shift toward more closed silos with less public scrutiny and slower defect discovery.
% FOUNDING_PROBLEM: How to ensure software quality, reliability, and security when development is distributed across many independent actors without centralized quality control or hierarchical management.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by peer-reviewed software engineering research and independent industry reliability studies from outside the open source advocacy community; some funding sources carry mixed interests, but the coordination problem itself is widely attested.
narrative_ontology:disappearance_verdict(software_control_legitimacy__pragmatic_openness_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__pragmatic_openness_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__pragmatic_openness_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_control_legitimacy__pragmatic_openness_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__pragmatic_openness_reading, 0.18, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__pragmatic_openness_reading_tests).
:- end_tests(software_control_legitimacy__pragmatic_openness_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the arrangement coordinates voluntary contribution and transparent review without coercively extracting from any party. Suppression is minimal (0.12) because proprietary alternatives are explicitly legitimate and not suppressed. Theater ratio is low (0.10) as the coordination function is genuine and not performative. Accessibility collapse is moderate-low (0.30) because understanding the methodology does not close off alternatives. Resistance is negligible (0.10) because the constraint operates through voluntary participation. The measurement series shows gradual slight increase as open source becomes institutionally entrenched, but the constraint remains in the coordination regime.
 *
 * PERSPECTIVAL GAP:
 *   All seated agents experience low directionality. Contributors and users are net beneficiaries. Proprietary developers are not targeted; they experience the constraint as ambient background rather than extraction. There is no significant perspectival divergence because the constraint lacks an enforcement mechanism that would create asymmetric costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (open_source_contributors, software_users) derive genuine coordination surplus from peer review and shared infrastructure, placing them near the beneficiary end of the directionality spectrum (low d). No victims are declared, so no high-d seats are generated. Proprietary developers, while not beneficiaries of this specific coordination mechanism, are explicitly treated as legitimate alternatives and thus experience neutral directionality rather than target status.
 *
 * MANDATROPHY ANALYSIS:
 *   The rope classification prevents mislabeling this coordination mechanism as extraction: there is no active enforcement, no suppression of alternatives, and no identifiable victim set. If extraction metrics were high or victims were present, the engine would flag a tangled rope or snare, signaling that the methodology-choice framing serves as cover for coercion. Here, the structural data (low epsilon, low suppression, no victims, no enforcement) align with the claimed type, indicating genuine coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    open_source_quality_empirical_status,
    'Does open source development consistently produce higher software quality than proprietary alternatives, or is the superiority claim domain-specific and contingent on project type and governance model?',
    'Systematic meta-analysis of defect rates, security patch velocity, and maintainability across matched open and proprietary codebases, controlling for size, domain, and funding.',
    'If the empirical superiority claim fails, the foundational axiom of this reading weakens, potentially collapsing the constraint toward property_rights or piton status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_source_quality_empirical_status, empirical, 'Empirical status of open source quality superiority claim').

omega_variable(
    pragmatic_reading_structural_independence,
    'Is the pragmatic openness reading a stable independent constraint, or does it function as a compromise formation that collapses into the freedom_imperative reading when proprietary software causes harms, or into the property_rights reading when commercial interests dominate?',
    'Discourse analysis of pragmatic openness advocates'' argumentative patterns under empirical or ethical challenge; tracking whether they revert to deontological or property-based claims.',
    'If the reading collapses under pressure, it lacks independent structural integrity and should be reclassified as derivative or piton rather than a genuine rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pragmatic_reading_structural_independence, conceptual, 'Structural independence of pragmatic openness as kernel reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__pragmatic_openness_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sw_ctrl_prag_open_tr_t0, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(sw_ctrl_prag_open_tr_t6, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 6, 0.06).
narrative_ontology:measurement(sw_ctrl_prag_open_tr_t12, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 12, 0.07).
narrative_ontology:measurement(sw_ctrl_prag_open_tr_t18, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 18, 0.08).
narrative_ontology:measurement(sw_ctrl_prag_open_tr_t24, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 24, 0.09).
narrative_ontology:measurement(sw_ctrl_prag_open_tr_t30, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(sw_ctrl_prag_open_be_t0, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(sw_ctrl_prag_open_be_t6, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 6, 0.13).
narrative_ontology:measurement(sw_ctrl_prag_open_be_t12, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 12, 0.14).
narrative_ontology:measurement(sw_ctrl_prag_open_be_t18, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 18, 0.15).
narrative_ontology:measurement(sw_ctrl_prag_open_be_t24, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 24, 0.16).
narrative_ontology:measurement(sw_ctrl_prag_open_be_t30, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 30, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(software_control_legitimacy__pragmatic_openness_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__pragmatic_openness_reading, resource_allocation).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, property_rights_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, commons_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the software_control_legitimacy kernel, decomposed per the epsilon-invariance principle. The pragmatic openness reading instantiates a low-extraction coordination framing where software control is treated as methodology choice. Sibling readings instantiate structurally distinct constraints with different epsilon values and victim/beneficiary structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
