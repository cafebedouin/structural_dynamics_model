% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__graduated_access_filter
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_licensing_statute_mandate__graduated_access_filter, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: licensing_statute_mandate__graduated_access_filter
 *   human_readable: Licensing Statute as Graduated Access Filter by Class
 *   domain: labor/regulatory
 *
 * SUMMARY:
 *   Statutory credential requirements appear neutral: meet the standard,
 *   enter the profession. In practice, they sort workers by prior class,
 *   resource access, and family capital. This is ONE reading of the contested
 *   licensing-statute kernel — specifically, the reading that frames
 *   licensing statutes as graduated-access filters that extract by class
 *   exclusion. Two sibling readings exist: public_safety_coordination
 *   (licenses prevent consumer harm through minimum standards) and
 *   rent_seeking_suppression (licenses restrict labor supply for incumbent
 *   extraction). This story instantiates the graduated-access reading: the
 *   statute IS extractive, the extraction IS asymmetric by class, and the
 *   mechanism IS structural exclusion masquerading as neutral standards.
 *
 * KEY AGENTS:
 *   - credentialed_incumbent_practitioners: beneficiary, set standards, capture regulatory boards
 *   - marginalized_workers_without_acquisition_resources: victim, structurally trapped, face class-sorted barriers
 *   - immigrant_populations_with_foreign_credentials: victim, face redundant credentialing and deskilling
 *   - economically_precarious_license_seekers: victim, identity-locked, cannot afford credential stacking
 *   - consumer_safety_constituency: passive beneficiary of public-safety framing, pays via elevated prices
 *   - licensing_authority_administrators: agenda-setter, derive budget and authority from the constraint
 *   - legislative_overseers: analytical observers, periodically review but face regulatory capture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__graduated_access_filter, 0.68).
domain_priors:suppression_score(licensing_statute_mandate__graduated_access_filter, 0.71).
domain_priors:theater_ratio(licensing_statute_mandate__graduated_access_filter, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, extractiveness, 0.68).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__graduated_access_filter, snare).
narrative_ontology:human_readable(licensing_statute_mandate__graduated_access_filter, "Licensing Statute as Graduated Access Filter by Class").
narrative_ontology:topic_domain(licensing_statute_mandate__graduated_access_filter, "labor/regulatory").

domain_priors:requires_active_enforcement(licensing_statute_mandate__graduated_access_filter).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__graduated_access_filter, 'e798e375-eed0-49a2-98fe-952b66437239').
narrative_ontology:cs_kernel_codification('e798e375-eed0-49a2-98fe-952b66437239', formalized).
narrative_ontology:cs_authority_grounding('e798e375-eed0-49a2-98fe-952b66437239', extraction).
narrative_ontology:cs_interpretation_layer_present('e798e375-eed0-49a2-98fe-952b66437239').
narrative_ontology:cs_reading_relation('e798e375-eed0-49a2-98fe-952b66437239', licensing_statute_mandate__public_safety_coordination, coexists_with).
narrative_ontology:cs_reading_relation('e798e375-eed0-49a2-98fe-952b66437239', licensing_statute_mandate__rent_seeking_suppression, coexists_with).
narrative_ontology:cs_axiom('e798e375-eed0-49a2-98fe-952b66437239', foundational, credential_barriers_sort_by_prior_class).
narrative_ontology:cs_axiom_status(credential_barriers_sort_by_prior_class, holdable).
narrative_ontology:cs_axiom_grounding('e798e375-eed0-49a2-98fe-952b66437239', credential_barriers_sort_by_prior_class, empirically_contingent).
narrative_ontology:cs_axiom('e798e375-eed0-49a2-98fe-952b66437239', foundational, statutory_licensing_forecloses_alternative_pathways).
narrative_ontology:cs_axiom_status(statutory_licensing_forecloses_alternative_pathways, holdable).
narrative_ontology:cs_axiom_grounding('e798e375-eed0-49a2-98fe-952b66437239', statutory_licensing_forecloses_alternative_pathways, deontological).
narrative_ontology:cs_reference_frame('e798e375-eed0-49a2-98fe-952b66437239', meritocratic_credential_sorting).
narrative_ontology:cs_drift_state('e798e375-eed0-49a2-98fe-952b66437239', contemporary_inequality_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e798e375-eed0-49a2-98fe-952b66437239', '').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, credentialed_incumbent_practitioners).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, licensing_authority_administrators).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, marginalized_workers_without_acquisition_resources).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, immigrant_populations_with_foreign_credentials).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, economically_precarious_license_seekers).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__graduated_access_filter, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(licensing_statute_mandate__graduated_access_filter, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(licensing_statute_mandate__graduated_access_filter_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(licensing_statute_mandate__graduated_access_filter, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(licensing_statute_mandate__graduated_access_filter_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 to 0.68 over the interval (t0 to t25), then plateaus. The rise reflects credential stacking: exam requirements, continuing education, application fees, and background checks accumulate as licensing boards (lobbied by incumbents) expand standards beyond minimum competence into competitive moats. The plateau at t25 reflects statutory stabilization — the constraint reaches its equilibrium extraction level once credential barriers are maximized within political tolerance. Suppression follows the same trajectory (0.55 to 0.71) because extraction of this type REQUIRES active enforcement: barriers must be legally maintained against lower-credential practitioners and alternative pathways must be statutorily foreclosed. Theater rises from 0.28 to 0.42: the constraint's public-safety justification is real but declining as a proportion of enforcement effort — more enforcement focuses on preventing alternative certification (guilds, apprenticeships, reputation networks) and protecting incumbent market share. Accessibility collapse (0.62) reflects the fact that once the statute exists, alternatives are technically unavailable to new entrants (legal barriers are high) but some remnants persist (informal work, unregulated sectors, out-of-state licensing reciprocity). Resistance is moderate (0.58) because marginalized workers lack organized voice, but incumbent practitioners and professional associations actively defend the constraint.
 *
 * PERSPECTIVAL GAP:
 *   The incumbent practitioners and licensing boards perceive this as rope (genuine coordination) or even mountain (natural competence floors). Marginalized workers and immigrant populations perceive it as pure snare (barriers designed to exclude them and protect incumbents). Legislative overseers are analytical observers caught between two framings. The engine computes per-seat classification from the structural data: from the target seats (marginalized workers, immigrants), the constraint should compute as snare or high-extraction tangled_rope; from the beneficiary seats (incumbents), it should compute as rope with coordination function. The divergence across seats is the central signal this story carries.
 *
 * DIRECTIONALITY LOGIC:
 *   Credentialed incumbents sit at d ≈ 0.1 (full beneficiary): they benefit from restricted labor supply, capture regulatory boards, and have arbitrage options if ever forced to exit. Licensing administrators sit at d ≈ 0.15 (beneficiary): they gain budget, authority, and institutional importance from administering the constraint. Marginalized workers without resources sit at d ≈ 0.95 (near-total target): they pay time, money, and opportunity without meaningful benefit and are trapped by the statute — exit to informal work is precarious and low-wage. Immigrant populations with foreign credentials sit at d ≈ 0.85 (target): they are doubly burdened (foreign credential unrecognized + cost of re-certification) and cannot arbitrage their existing credentials. Economically precarious license-seekers sit at d ≈ 0.90 (target): they are identity-locked to the licensed profession and pay the full extraction burden (fees, study time, foregone income) with near-zero exit to dignity. Consumer safety constituency sits at d ≈ 0.35 (slightly toward beneficiary): they receive genuine coordination (safety standard) but pay via elevated prices and restricted choice. The claim and metrics are independent by design: the claim is 'snare,' and the metrics (high extractiveness, high suppression, class-sorted victims) support that claim from the graduated-access reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (consumer harm from unqualified practitioners) is contested — incumbents say it is still live, labor researchers say it is solved. The constraint persists at high extraction levels (0.68) not because the founding problem is acute, but because incumbents and licensing bureaucracies benefit from maintaining the status quo. The measurement series shows extractiveness and suppression rising then stabilizing, not declining, which would be consistent with mandatrophy (a decaying constraint sustained by inertia). Instead, the constraint shows ACTIVE MAINTENANCE at high extraction — incumbents actively lobby to expand credential requirements, licensing boards defend existing barriers, and alternative pathways are statutorily foreclosed. This is not mandatrophy; it is an actively defended snare. If anything, the constraint exhibits institutional capture and rent-seeking, not theater-based persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    class_sorting_vs_competence_selection,
    'Do credential barriers sort primarily by competence (filtering unqualified practitioners) or primarily by prior class (filtering economically precarious populations)?',
    'Correlation analysis comparing exam pass rates by socioeconomic background; comparison of credential-holder competence (outcomes, harm rates, complaints) vs. statistical difference in outcomes between credentialed and alternative practitioners; natural experiments from jurisdictions with lower barriers.',
    'If primarily competence: the constraint is closer to rope (coordination with side effect of class sorting). If primarily class: the constraint is snare with public-safety justification as cover story. The credentialed-incumbent reading of this oracle would likely dispute the segregation and argue competence dominates; marginalized workers and labor economists would argue class dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(class_sorting_vs_competence_selection, empirical, 'Whether barriers sort by competence or by prior class and resource access.').

omega_variable(
    alternative_certification_viability,
    'If statutory licensing were removed, would reputation mechanisms, sectoral certification, apprenticeship systems, or liability law adequately maintain consumer safety?',
    'Natural experiments from unregulated professions (consulting, training, many service trades) that maintain safety through reputation and liability; historical evidence from professions before licensing mandates; cross-jurisdictional comparison of licensing vs. alternative regimes.',
    'If alternatives can maintain safety: the public-safety justification is cover story for a snare; the constraint extracts without necessary function. If alternatives cannot maintain safety, the public-safety justification is correct and extraction is the price of genuine coordination. This directly determines whether the constraint is snare or tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_certification_viability, empirical, 'Whether consumer safety depends on statutory licensing or can be maintained by alternative mechanisms.').

omega_variable(
    regulatory_capture_dynamics,
    'To what extent do incumbent practitioners and licensing boards informally coordinate to maintain and expand credential barriers?',
    'Examination of licensing board membership, professional association participation by board members, regulatory influence patterns, and legislative history of credential-expansion votes.',
    'High capture indicates snare with active maintenance by beneficiaries; low capture would suggest the barriers persist for public-safety reasons. The measurement series shows stable suppression (0.71 at t25+), suggesting active maintenance rather than decay — consistent with high capture.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capture_dynamics, empirical, 'The degree to which incumbent practitioners shape licensing standards to restrict competition.').

omega_variable(
    identity_lock_mechanism_in_precarious_workers,
    'Why do economically precarious workers remain identity-locked to the licensed profession rather than shifting to unregulated alternatives or informal economy work?',
    'Ethnographic or interview research with aspiring practitioners who have failed credential requirements; historical analysis of upward-mobility narratives; comparison of exit rates among trapped vs. mobile worker populations.',
    'If identity lock is internalized (credentialing = dignity, professionalism = identity), the constraint''s suppression is higher than structural barriers alone; targets carry the barrier with them even after exit. If identity lock is structural only (limited alternatives for decent wages), exit is possible but costly. This affects the suppression reading and the terminal type — deep identity lock amplifies the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_precarious_workers, empirical, 'Whether precarious workers'' lock to licensing is internalized (identity-fused) or structural (limited economic alternatives).').

omega_variable(
    sibling_reading_contest_location,
    'On what structural facts do the three readings of the licensing-statute kernel diverge?',
    'The three readings (graduated_access_filter, public_safety_coordination, rent_seeking_suppression) share the same statute and text but instantiate different constraints with different ε values and beneficiary/victim structures. This omega documents the under-determination: the statutory text does NOT adjudicate which reading is correct; structural facts about class sorting, competence effects, and capture dynamics must be measured independently.',
    'This is a conceptual omega about the contest itself. Resolving it requires data on class sorting (gradient_item_1), consumer-safety adequacy of alternatives (gradient_item_2), and capture dynamics (gradient_item_3). Each reading claims a different diagnosis; the three cannot coexist in a single framework — they are readings, not perspectives on the same constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_contest_location, conceptual, 'The structural facts and measurements that distinguish the three sibling readings of the licensing-statute kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__graduated_access_filter, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lice_tr_t0, licensing_statute_mandate__graduated_access_filter, theater_ratio, 0, 0.28).
narrative_ontology:measurement(lice_tr_t5, licensing_statute_mandate__graduated_access_filter, theater_ratio, 5, 0.32).
narrative_ontology:measurement(lice_tr_t10, licensing_statute_mandate__graduated_access_filter, theater_ratio, 10, 0.35).
narrative_ontology:measurement(lice_tr_t15, licensing_statute_mandate__graduated_access_filter, theater_ratio, 15, 0.39).
narrative_ontology:measurement(lice_tr_t20, licensing_statute_mandate__graduated_access_filter, theater_ratio, 20, 0.41).
narrative_ontology:measurement(lice_tr_t25, licensing_statute_mandate__graduated_access_filter, theater_ratio, 25, 0.42).
narrative_ontology:measurement(lice_tr_t30, licensing_statute_mandate__graduated_access_filter, theater_ratio, 30, 0.42).
narrative_ontology:measurement(lice_tr_t40, licensing_statute_mandate__graduated_access_filter, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(lice_be_t0, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(lice_be_t5, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(lice_be_t10, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(lice_be_t15, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 15, 0.61).
narrative_ontology:measurement(lice_be_t20, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(lice_be_t25, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(lice_be_t30, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(lice_be_t40, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(lice_su_t0, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(lice_su_t5, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(lice_su_t10, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(lice_su_t15, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 15, 0.66).
narrative_ontology:measurement(lice_su_t20, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(lice_su_t25, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(lice_su_t30, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(lice_su_t40, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__graduated_access_filter, identity_coordination).
narrative_ontology:boltzmann_floor_override(licensing_statute_mandate__graduated_access_filter, 0.1).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate__public_safety_coordination).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate__rent_seeking_suppression).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, occupational_mobility_barrier_class_stratification).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, informal_economy_legalization_pressure).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the licensing_statute_mandate kernel. The public_safety_coordination reading emphasizes the coordination function and instantiates the statute as rope/mountain; rent_seeking_suppression emphasizes direct rent extraction and instantiates the statute as snare. This reading (graduated_access_filter) emphasizes the CLASS-SORTING mechanism and the structural EXCLUSION of marginalized workers, instantiating the statute as snare with a coordination justification that serves as cover story. All three readings share the same statutory text and formal rules; they differ in what problem is solved, who benefits, and what the primary effect is. Network links indicate causal influence: the founding problem (consumer-harm prevention) influences the public-safety reading; incumbent beneficiaries influence the rent-seeking reading; structural class sorting influences the graduated-access reading. This story does NOT decompose the statute itself (it is one constraint), but rather one reading of the statute's structural effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(licensing_statute_mandate__graduated_access_filter, powerless, 0.92).
constraint_indexing:directionality_override(licensing_statute_mandate__graduated_access_filter, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
