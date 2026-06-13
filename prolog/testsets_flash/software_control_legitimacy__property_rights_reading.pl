% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__property_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__property_rights_reading, []).

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
 *   constraint_id: software_control_legitimacy__property_rights_reading
 *   human_readable: Software Control as Property Right
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   This constraint models the view that software control is a legitimate
 *   property right, granting creators authority to restrict use,
 *   modification, and distribution. This perspective emphasizes protecting
 *   investment and enabling commercial sustainability. It is one reading of
 *   the broader 'software_control_legitimacy' kernel, which is contested by
 *   alternative views emphasizing user freedom, pragmatic openness, or a
 *   commons approach. This reading positions software vendors and investors
 *   as beneficiaries, while users, independent developers, and FOSS advocates
 *   bear the costs of restricted access and modification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__property_rights_reading, 0.45).
domain_priors:suppression_score(software_control_legitimacy__property_rights_reading, 0.6).
domain_priors:theater_ratio(software_control_legitimacy__property_rights_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__property_rights_reading, tangled_rope).
narrative_ontology:human_readable(software_control_legitimacy__property_rights_reading, "Software Control as Property Right").
narrative_ontology:topic_domain(software_control_legitimacy__property_rights_reading, "software_engineering/political_economy_of_technology/intellectual_property").

domain_priors:requires_active_enforcement(software_control_legitimacy__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__property_rights_reading, 'f68be570-a177-452b-8222-0e89bc85f37d').
narrative_ontology:cs_kernel_codification('f68be570-a177-452b-8222-0e89bc85f37d', formalized).
narrative_ontology:cs_authority_grounding('f68be570-a177-452b-8222-0e89bc85f37d', lineage).
narrative_ontology:cs_interpretation_layer_present('f68be570-a177-452b-8222-0e89bc85f37d').
narrative_ontology:cs_reading_relation('f68be570-a177-452b-8222-0e89bc85f37d', software_control_legitimacy__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('f68be570-a177-452b-8222-0e89bc85f37d', software_control_legitimacy__pragmatic_openness_reading, coexists_with).
narrative_ontology:cs_reading_relation('f68be570-a177-452b-8222-0e89bc85f37d', software_control_legitimacy__commons_reading, influences).
narrative_ontology:cs_axiom('f68be570-a177-452b-8222-0e89bc85f37d', foundational, software_is_intellectual_property).
narrative_ontology:cs_axiom_status(software_is_intellectual_property, holdable).
narrative_ontology:cs_axiom_grounding('f68be570-a177-452b-8222-0e89bc85f37d', software_is_intellectual_property, conventional).
narrative_ontology:cs_axiom('f68be570-a177-452b-8222-0e89bc85f37d', foundational, exclusive_rights_incentivize_innovation).
narrative_ontology:cs_axiom_status(exclusive_rights_incentivize_innovation, holdable).
narrative_ontology:cs_axiom_grounding('f68be570-a177-452b-8222-0e89bc85f37d', exclusive_rights_incentivize_innovation, empirically_contingent).
narrative_ontology:cs_reference_frame('f68be570-a177-452b-8222-0e89bc85f37d', classical_intellectual_property_regime).
narrative_ontology:cs_drift_state('f68be570-a177-452b-8222-0e89bc85f37d', contemporary_digital_commons_discourse, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f68be570-a177-452b-8222-0e89bc85f37d', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__property_rights_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, software_vendors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, investors).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, users).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, independent_developers).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, foss_advocates).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__property_rights_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(software_control_legitimacy__property_rights_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__property_rights_reading_tests).
:- end_tests(software_control_legitimacy__property_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it provides a genuine coordination function (enabling commercial software development and investment) but also involves asymmetric extraction. Software vendors benefit from protected revenue streams, while users and developers face restrictions on use and modification. The extractiveness (0.45) reflects the cost of licensing and restricted freedoms. Suppression (0.6) is moderate, as legal frameworks and technical measures actively enforce these rights, limiting alternatives like unauthorized copying or modification. Theater ratio is low (0.1) as the enforcement is genuinely functional in protecting commercial interests.
 *
 * PERSPECTIVAL GAP:
 *   Software vendors and investors (agenda_setters/beneficiaries) experience this as a necessary framework for innovation and commercial viability. Users and independent developers (payers/victims) experience it as a restriction on their ability to use, modify, and share software, leading to higher costs and reduced flexibility. FOSS advocates view it as fundamentally unjust, denying the collaborative nature of software development.
 *
 * DIRECTIONALITY LOGIC:
 *   Software vendors and investors are clear beneficiaries (low d) as the constraint directly protects their commercial models. Users and independent developers are targets (high d) as they bear the costs of licensing and restrictions. FOSS advocates are also targets, as their core principles are directly opposed by this property-rights framework. The constraint subsidizes commercial development by extracting from users' and developers' freedoms.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (protecting investment, enabling commercial sustainability) is still live, but its status is contested. Critics argue that the problem of 'unfettered copying' has been over-solved, and the current regime primarily serves rent extraction rather than genuine innovation protection. The classification as Tangled Rope acknowledges both the coordination function and the extractive asymmetry, preventing mislabeling it as a pure Rope (ignoring extraction) or a pure Snare (ignoring coordination).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine property right, or a constructed mechanism for rent extraction?',
    'Analysis of economic returns relative to investment and risk; comparison with other intellectual property regimes.',
    'If primarily rent extraction, the classification shifts towards Snare; if a genuine property right, it remains Tangled Rope with a stronger coordination justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''property_rights_reading'' of the ''software_control_legitimacy'' kernel.').

omega_variable(
    freedom_imperative_conflict,
    'How would the ''freedom_imperative_reading'' alter the structural properties of this constraint?',
    'If the ''freedom_imperative_reading'' were adopted, it would fundamentally challenge the legitimacy of proprietary software, likely leading to a reclassification of this constraint as a Snare due to its denial of user control.',
    'A shift to the ''freedom_imperative_reading'' would increase perceived extractiveness and suppression from the user''s perspective, potentially reclassifying this constraint as a Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(freedom_imperative_conflict, conceptual, 'Conflict with the ''freedom_imperative_reading'' of the software_control_legitimacy kernel.').

omega_variable(
    commons_reading_implications,
    'What would be the structural implications if the ''commons_reading'' of software control were adopted?',
    'The ''commons_reading'' would likely lead to a re-evaluation of default licensing, favoring more permissive models and potentially reducing the scope and enforcement of proprietary restrictions, shifting this constraint towards a Rope or Scaffold.',
    'Adoption of the ''commons_reading'' would reduce extractiveness and suppression, potentially reclassifying this constraint as a Rope or Scaffold, emphasizing collective management over individual property rights.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_reading_implications, conceptual, 'Implications of the ''commons_reading'' of the software_control_legitimacy kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__property_rights_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__property_rights_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(soft_tr_t10, software_control_legitimacy__property_rights_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(soft_tr_t20, software_control_legitimacy__property_rights_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__property_rights_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(soft_be_t10, software_control_legitimacy__property_rights_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(soft_be_t20, software_control_legitimacy__property_rights_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_control_legitimacy__property_rights_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(soft_su_t10, software_control_legitimacy__property_rights_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(soft_su_t20, software_control_legitimacy__property_rights_reading, suppression_requirement, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__property_rights_reading, resource_allocation).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, software_control_legitimacy__pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, software_control_legitimacy__commons_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'software_control_legitimacy' kernel, each representing a distinct structural claim about software control. They are linked to capture their interdependencies and contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
