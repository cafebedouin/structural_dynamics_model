% ============================================================================
% CONSTRAINT STORY: informed_consent_impossibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_informed_consent_impossibility, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: informed_consent_impossibility
 *   human_readable: Structural Impossibility of Consent from Future Persons in Germline Genetic Modification
 *   domain: bioethics/reproductive_medicine/genetic_engineering
 *
 * SUMMARY:
 *   The structural impossibility of obtaining consent from a future person
 *   whose genome is modified represents a temporal-logical constraint
 *   distinct from parental proxy consent for existing children. When parents
 *   consent to medical treatment for a living child, the child exists and has
 *   interests that can be represented. When parents consent to germline
 *   genetic modification, the future person does not yet exist, and the
 *   modification is identity-constituting — the person who comes into
 *   existence would not be the same person without that modification. This is
 *   the non-identity problem: there is no 'same person' who could have
 *   existed with or without the modification to consent or refuse. The
 *   constraint appears as a mountain from all perspectives because it is a
 *   temporal-logical impossibility, not a procedural gap or regulatory
 *   choice. However, the constraint has identifiable beneficiaries:
 *   deontological bioethics frameworks gain legitimacy from the impossibility
 *   (it vindicates their precautionary stance), and regulatory precautionary
 *   regimes gain justification (the impossibility provides a stable
 *   foundation for prohibition). This makes the constraint a false summit
 *   candidate — the temporal-logical impossibility is genuine, but its
 *   interpretation as an absolute barrier to germline modification (rather
 *   than as a structural feature requiring alternative ethical frameworks)
 *   benefits specific normative positions. The theater_ratio is low (0.15)
 *   because the constraint is not performative — the impossibility is real
 *   and widely recognized across regulatory regimes, philosophical
 *   traditions, and clinical practice. The modest increase over the interval
 *   reflects growing procedural complexity in consent documentation as
 *   germline technologies advance, but the core constraint remains
 *   non-theatrical. Extractiveness is very low (0.08) because the constraint
 *   does not extract rents in the traditional sense — it is a logical limit,
 *   not an institutional arrangement. The modest extractiveness reflects that
 *   deontological frameworks and precautionary regimes do benefit from the
 *   constraint's existence, but this benefit is indirect (legitimacy,
 *   justification) rather than direct (resource capture, career advantage).
 *
 * KEY AGENTS:
 *   - Future Person: Primary agent (powerless/trapped) — cannot consent because does not exist at time of modification; experiences constraint as absolute temporal-logical barrier
 *   - Prospective Parents: Decision-makers (moderate/constrained) — face irreducible structural constraint in proxy consent; cannot obtain future person's consent for identity-constituting modifications
 *   - Regulatory Bodies: Institutional actors (institutional/arbitrage) — recognize constraint as logical limit across all major frameworks (FDA, EMA, national bioethics councils); cannot solve structural impossibility through policy
 *   - Deontological Bioethics Frameworks: Beneficiaries (institutional/arbitrage) — gain legitimacy from constraint's existence; the impossibility vindicates precautionary stance and provides foundation for prohibition
 *   - Regulatory Precautionary Regimes: Beneficiaries (institutional/arbitrage) — gain justification from constraint; the impossibility provides stable basis for restrictive policies
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes constraint as genuine temporal-logical limit but notes that its framing as absolute barrier benefits specific normative positions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(informed_consent_impossibility, 0.08).
domain_priors:suppression_score(informed_consent_impossibility, 0.12).
domain_priors:theater_ratio(informed_consent_impossibility, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(informed_consent_impossibility, extractiveness, 0.08).
narrative_ontology:constraint_metric(informed_consent_impossibility, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(informed_consent_impossibility, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(informed_consent_impossibility, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(informed_consent_impossibility, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(informed_consent_impossibility, mountain).
narrative_ontology:human_readable(informed_consent_impossibility, "Structural Impossibility of Consent from Future Persons in Germline Genetic Modification").
narrative_ontology:topic_domain(informed_consent_impossibility, "bioethics/reproductive_medicine/genetic_engineering").

domain_priors:emerges_naturally(informed_consent_impossibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(informed_consent_impossibility, deontological_bioethics_frameworks).
narrative_ontology:constraint_beneficiary(informed_consent_impossibility, regulatory_precautionary_regimes).
narrative_ontology:constraint_vindicates(informed_consent_impossibility, temporal_asymmetry_of_consent).
narrative_ontology:constraint_vindicates(informed_consent_impossibility, non_identity_problem_constraint).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE FUTURE PERSON (MOUNTAIN) — The agent whose genome is modified cannot consent because they do not yet exist at the time of modification. This is a temporal-logical impossibility, not a procedural gap. No amount of institutional reform, technological advancement, or ethical framework revision can enable a non-existent agent to consent to their own creation. The constraint is experienced as absolute — there is no exit from non-existence into retroactive consent.
constraint_indexing:constraint_classification(informed_consent_impossibility, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: PROSPECTIVE PARENTS (MOUNTAIN) — Parents face an irreducible structural constraint: they can consent on behalf of a future child for therapeutic interventions that prevent suffering, but they cannot obtain the future person's consent for identity-constituting modifications. The non-identity problem is a logical barrier, not a regulatory one. Even parents with full autonomy and resources cannot solve the temporal asymmetry — the person whose genome they modify would not exist without that modification, so there is no 'same person' to consent or refuse.
constraint_indexing:constraint_classification(informed_consent_impossibility, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGULATORY BODIES (MOUNTAIN) — Regulatory frameworks can require parental consent, ethics review, and safety thresholds, but they cannot solve the structural impossibility of obtaining consent from the future person. The constraint is recognized across all major regulatory regimes (FDA, EMA, national bioethics councils) as a logical limit, not a policy choice. Regulators with full institutional power and global coordination cannot make a non-existent person consent. The constraint persists regardless of who enforces it or whether anyone enforces it.
constraint_indexing:constraint_classification(informed_consent_impossibility, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DEONTOLOGICAL BIOETHICS FRAMEWORKS (MOUNTAIN) — Kantian and rights-based frameworks treat the consent impossibility as a categorical constraint: persons have a right to autonomy over identity-constituting choices, and germline modification violates this right by definition because the future person cannot consent. However, these frameworks BENEFIT from the constraint's existence — the impossibility vindicates their precautionary stance and provides a stable foundation for regulatory prohibition. This is a false summit candidate: the constraint is presented as a natural law (temporal-logical impossibility) but identifiable beneficiaries exist (deontological frameworks gain legitimacy; precautionary regulatory regimes gain justification).
constraint_indexing:constraint_classification(informed_consent_impossibility, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal perspective, the consent impossibility is a temporal-logical constraint that would persist in any possible world where time is asymmetric and identity is constituted through genetic endowment. No technological advancement (time travel, retroactive consent mechanisms, identity-preserving modification) can solve the non-identity problem without dissolving the concept of personal identity itself. The constraint is a genuine mountain — but the analytical observer must note that its framing as an absolute barrier to germline modification (rather than as a structural feature requiring alternative ethical frameworks) benefits specific normative positions.
constraint_indexing:constraint_classification(informed_consent_impossibility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(informed_consent_impossibility_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(informed_consent_impossibility, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(informed_consent_impossibility, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(informed_consent_impossibility, ExtMetricName, E),
    domain_priors:suppression_score(informed_consent_impossibility, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(informed_consent_impossibility),
    narrative_ontology:constraint_metric(informed_consent_impossibility, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(informed_consent_impossibility, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(informed_consent_impossibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The constraint is a temporal-logical impossibility, not an institutional arrangement that extracts rents. The modest extractiveness reflects that deontological bioethics frameworks and precautionary regulatory regimes do benefit from the constraint's existence — it vindicates their positions and provides justification for restrictive policies — but this benefit is indirect (legitimacy, justification) rather than direct (resource capture, career advantage). The constraint does not create a verification bottleneck, a priority race, or a gatekeeping mechanism. It is a logical limit that happens to align with certain normative positions. Suppression (0.12): Very low. The constraint does not suppress alternatives through coercion or enforcement. It is a logical barrier that persists regardless of institutional arrangements. The modest suppression reflects that regulatory frameworks do prohibit germline modification partly on the basis of the consent impossibility, but the prohibition is grounded in the logical constraint rather than imposed arbitrarily. Theater ratio (0.15): Very low. The constraint is not performative. The impossibility of obtaining consent from a non-existent person is a genuine temporal-logical barrier recognized across regulatory regimes, philosophical traditions, and clinical practice. The modest theater reflects growing procedural complexity in consent documentation as germline technologies advance (ethics review boards, multi-stage consent processes, long-term follow-up protocols), but the core constraint is not theatrical. Accessibility collapse (0.92): Very high. Once the non-identity problem is understood, alternative consent frameworks collapse almost completely. Hypothetical consent, species-level consent, and harm-threshold consent are all proposed as workarounds, but each faces the same irreducible problem: the person whose genome is modified would not exist without that modification, so there is no 'same person' to consent. Resistance (0.08): Very low. The constraint meets almost no real resistance. Regulatory frameworks, bioethics committees, and clinical practitioners across all major jurisdictions recognize the consent impossibility as a genuine structural barrier. The modest resistance comes from utilitarian and consequentialist frameworks that argue the constraint should not be treated as absolute, but even these frameworks acknowledge the logical problem — they dispute its ethical implications, not its existence.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates mountain classification from all perspectives because the temporal-logical impossibility is genuine and irreducible. The future person cannot consent because they do not exist. Parents cannot obtain the future person's consent because the modification is identity-constituting. Regulatory bodies cannot solve the structural impossibility through policy. Deontological frameworks recognize the constraint as categorical. The analytical observer confirms the constraint as a temporal-logical limit. However, the constraint is a false summit candidate because it has identifiable beneficiaries: deontological bioethics frameworks gain legitimacy from the impossibility, and precautionary regulatory regimes gain justification. The mountain classification is structurally correct — the constraint is a genuine logical limit — but its interpretation as an absolute barrier to germline modification (rather than as a structural feature requiring alternative ethical frameworks) benefits specific normative positions. The perspectival gap is not between different constraint types (all perspectives see mountain) but between the constraint's logical status (genuine impossibility) and its ethical implications (contestable barrier vs structural feature).
 *
 * DIRECTIONALITY LOGIC:
 *   The future person is the primary agent whose interests are at stake, but they cannot be a beneficiary or victim in the traditional sense because they do not exist at the time of modification. The constraint does not extract from them — it prevents their existence in one form while enabling their existence in another. Directionality for the future person is undefined (the agent is not yet present to experience extraction). Prospective parents are neither clear beneficiaries nor victims — they face a genuine structural constraint in proxy consent, but the constraint does not extract from them in the way a snare extracts from trapped agents. Directionality for parents is near-neutral (d ≈ 0.5). Regulatory bodies are not victims — they recognize the constraint as a logical limit and use it to ground policy, but they do not bear costs from its existence. Directionality for regulators is low (d ≈ 0.2). Deontological bioethics frameworks and precautionary regulatory regimes are beneficiaries — they gain legitimacy and justification from the constraint's existence. The impossibility vindicates their precautionary stance and provides a stable foundation for prohibition. Directionality for these beneficiaries is very low (d ≈ 0.1), producing low or negative effective extraction (they benefit from the constraint). The analytical observer has no directionality (d is undefined for analytical contexts) — the observer recognizes the constraint as a genuine temporal-logical limit but notes that its framing as an absolute barrier benefits specific normative positions.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that a genuine mountain can have identifiable beneficiaries without ceasing to be a mountain. The temporal-logical impossibility of obtaining consent from a non-existent person is a real structural constraint — it would persist in any possible world where time is asymmetric and identity is constituted through genetic endowment. No amount of institutional reform, technological advancement, or ethical framework revision can enable a non-existent agent to consent to their own creation. The constraint is a mountain. But the constraint also benefits deontological bioethics frameworks and precautionary regulatory regimes — it vindicates their positions and provides justification for restrictive policies. This makes the constraint a false summit candidate. The false summit detector will flag the constraint because it is claimed as mountain, has very low extractiveness and suppression, emerges naturally, and has declared beneficiaries. The engine will evaluate whether the beneficiary structure is sufficient to reclassify the constraint as tangled_rope or whether the mountain classification stands despite the beneficiaries. The key question is whether the beneficiaries' gain is a side effect of a genuine natural law or whether the 'natural law' framing is itself a construction that serves the beneficiaries. The omega variables document this irreducible uncertainty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mountain_vs_constructed_barrier,
    'Is the consent impossibility a genuine temporal-logical constraint (mountain) or a constructed ethical barrier that benefits precautionary regulatory regimes (false summit)?',
    'Philosophical analysis of the non-identity problem; comparison of regulatory frameworks that treat the constraint as absolute vs those that treat it as one factor among many; examination of whether alternative consent frameworks (hypothetical consent, harm-threshold consent, species-level consent) are rejected on logical grounds or on normative grounds.',
    'If genuine mountain: germline modification is categorically impermissible under any consent-based ethics. If false summit: the constraint is real but its interpretation as an absolute barrier is a normative choice that benefits deontological frameworks and precautionary regimes. The structural impossibility remains, but its ethical implications are contestable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mountain_vs_constructed_barrier, conceptual, 'Whether consent impossibility is temporal-logical constraint or constructed ethical barrier').

omega_variable(
    non_identity_problem_resolution,
    'Does the non-identity problem dissolve under alternative conceptions of personal identity (narrative identity, psychological continuity, species-level identity)?',
    'Philosophical analysis of identity theories; examination of whether any coherent identity theory allows ''the same person'' to exist with and without a given genetic modification; assessment of whether species-level or collective consent frameworks can substitute for individual consent.',
    'If non-identity problem is irreducible: the constraint is a genuine mountain across all identity theories. If it dissolves under alternative theories: the constraint is relative to a specific (Lockean/psychological-continuity) conception of identity, and the mountain classification depends on which identity theory is adopted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_identity_problem_resolution, conceptual, 'Whether non-identity problem is irreducible across identity theories').

omega_variable(
    therapeutic_vs_enhancement_boundary,
    'Does the consent impossibility apply equally to therapeutic interventions (preventing genetic disease) and enhancement modifications (increasing cognitive capacity), or is the boundary between therapy and enhancement itself a constructed distinction that determines where the constraint binds?',
    'Analysis of regulatory frameworks that permit therapeutic germline modification but prohibit enhancement; examination of whether the therapy/enhancement distinction tracks a genuine structural difference (preventing harm vs creating benefit) or a normative preference; assessment of whether parents'' proxy consent is treated as legitimate for therapy but illegitimate for enhancement.',
    'If the constraint applies equally: all germline modification is impermissible regardless of therapeutic intent. If the constraint is therapy-relative: the impossibility of consent is not the true barrier — the barrier is a normative judgment about what parents may choose for future children, and the consent impossibility is invoked selectively to block enhancements while permitting therapies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(therapeutic_vs_enhancement_boundary, conceptual, 'Whether consent impossibility is therapy-relative or applies universally').

omega_variable(
    counterfactual_consent_validity,
    'Can hypothetical or counterfactual consent frameworks (what a rational person would consent to if they could) substitute for actual consent in germline modification decisions?',
    'Philosophical analysis of hypothetical consent theories (Rawlsian veil of ignorance, rational contractor models); examination of whether regulatory frameworks accept hypothetical consent for other irreversible decisions affecting future persons (vaccination, education, citizenship); assessment of whether the rejection of hypothetical consent for germline modification is principled or selective.',
    'If hypothetical consent is valid: the constraint is not absolute — parents can act on reasonable projections of what the future person would consent to. If hypothetical consent is invalid: the constraint is absolute, but this raises the question of why hypothetical consent is accepted for other irreversible parental decisions (medical treatment, educational choices, religious upbringing).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_consent_validity, conceptual, 'Whether hypothetical consent can substitute for actual consent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(informed_consent_impossibility, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(consent_imp_theater_1990, informed_consent_impossibility, theater_ratio, 0, 0.1).
narrative_ontology:measurement(consent_imp_theater_2000, informed_consent_impossibility, theater_ratio, 10, 0.12).
narrative_ontology:measurement(consent_imp_theater_2010, informed_consent_impossibility, theater_ratio, 20, 0.15).
narrative_ontology:measurement(consent_imp_theater_2020, informed_consent_impossibility, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(consent_imp_extract_1990, informed_consent_impossibility, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(consent_imp_extract_2000, informed_consent_impossibility, base_extractiveness, 10, 0.06).
narrative_ontology:measurement(consent_imp_extract_2010, informed_consent_impossibility, base_extractiveness, 20, 0.07).
narrative_ontology:measurement(consent_imp_extract_2020, informed_consent_impossibility, base_extractiveness, 30, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(informed_consent_impossibility, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is downstream of safety_risk_structure (the physical risks of germline modification) but represents a distinct ethical constraint. The upstream constraint has its own extractiveness reflecting the empirical uncertainty and potential harms; this constraint has its own extractiveness reflecting the temporal-logical impossibility of consent and the beneficiary structure of deontological frameworks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
