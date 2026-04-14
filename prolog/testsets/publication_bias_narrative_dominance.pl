% ============================================================================
% CONSTRAINT STORY: publication_bias_narrative_dominance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_publication_bias_narrative_dominance, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: publication_bias_narrative_dominance
 *   human_readable: Publication Bias and Narrative Dominance in Knowledge Production
 *   domain: epistemology/institutional_science
 *
 * SUMMARY:
 *   Publication bias and narrative dominance in knowledge production
 *   represent a structural constraint that operates through journal
 *   gatekeeping, metrics-driven career incentives, and the concentration of
 *   prestige in high-rejection-rate venues. The constraint creates asymmetric
 *   extraction: researchers with null or unexpected findings bear career
 *   costs while high-status institutions benefit from preferential
 *   publication and citation amplification. The mechanism combines genuine
 *   coordination (journals do organize knowledge dissemination) with
 *   substantial extraction (the bias direction systematically favors certain
 *   narratives over truthful ones). The constraint exhibits properties of
 *   Tangled Rope from moderate institutional perspectives, Snare from
 *   powerless negative-result researchers, and appears as an immutable
 *   natural law (Mountain) only when viewed from an analytical context that
 *   naturalizes institutional selection as information scarcity. The theater
 *   ratio has increased over the measurement interval (0.48 to 0.64) as
 *   impact metrics, h-index tracking, and citation counts have become
 *   ritualized performance indicators, with editors and institutional actors
 *   maintaining selection bias practices while simultaneously acknowledging
 *   their epistemically corrupting effects.
 *
 * KEY AGENTS:
 *   - Negative Result Researchers: Primary victims (powerless/trapped) — bear full career cost of null findings; cannot exit publication-dependent career systems
 *   - Early-Career Researchers: Secondary victims (moderate/constrained) — face publication pressure and tenure requirements; can exit to lower-status venues at career cost
 *   - Elite Research Institutions: Primary beneficiaries (institutional/arbitrage) — capture citation advantage and prestige concentration; can arbitrage alternative metrics
 *   - High-Status Researchers: Secondary beneficiaries (institutional/arbitrage) — benefit from prestige concentration and selectivity bias favoring established narratives
 *   - Open Science Advocates: Organized agents (organized/constrained) — building alternative pathways with sunset logic; face institutional resistance but have coalition power
 *   - Journal Editors and Publishers: Institutional enforcers (institutional/arbitrage) — maintain bias-generating selection criteria; see system as degraded but persist through inertia
 *   - Field Knowledge Commons: Victim (powerless/trapped) — abstract collective good corrupted by selective publication; cannot organize or demand truthfulness
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(publication_bias_narrative_dominance, 0.58).
domain_priors:suppression_score(publication_bias_narrative_dominance, 0.68).
domain_priors:theater_ratio(publication_bias_narrative_dominance, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(publication_bias_narrative_dominance, extractiveness, 0.58).
narrative_ontology:constraint_metric(publication_bias_narrative_dominance, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(publication_bias_narrative_dominance, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(publication_bias_narrative_dominance, tangled_rope).
narrative_ontology:human_readable(publication_bias_narrative_dominance, "Publication Bias and Narrative Dominance in Knowledge Production").
narrative_ontology:topic_domain(publication_bias_narrative_dominance, "epistemology/institutional_science").

domain_priors:requires_active_enforcement(publication_bias_narrative_dominance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(publication_bias_narrative_dominance, high_status_researchers).
narrative_ontology:constraint_beneficiary(publication_bias_narrative_dominance, funded_research_groups).
narrative_ontology:constraint_beneficiary(publication_bias_narrative_dominance, journals_with_impact_metrics).
narrative_ontology:constraint_victim(publication_bias_narrative_dominance, negative_result_researchers).
narrative_ontology:constraint_victim(publication_bias_narrative_dominance, unfunded_independent_researchers).
narrative_ontology:constraint_victim(publication_bias_narrative_dominance, field_epistemic_reliability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NEGATIVE RESULT RESEARCHER (SNARE) — Trapped by career consequences of negative or null findings. Cannot exit without abandoning publication-dependent career advancement. High suppression: journals reject null results, institutions devalue them, funding bodies penalize them. Pure extraction with no coordination benefit — the constraint extracts time, resources, and career capital while offering no collective good in return.
constraint_indexing:constraint_classification(publication_bias_narrative_dominance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EARLY-CAREER RESEARCHER (TANGLED ROPE) — Constrained by publication pressure and tenure requirements. Experiences both extraction (incentive to pursue publishable results over truthful results) and coordination benefit (publication system does organize knowledge dissemination, despite bias). Significant suppression via career risk; moderate exit cost through alternative venues that offer lower status.
constraint_indexing:constraint_classification(publication_bias_narrative_dominance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ELITE RESEARCH INSTITUTION (ROPE) — Benefits from publication bias through prestige concentration and competitive advantage. Experiences the constraint as pure coordination: the bias system allocates attention and resources to their researchers. High exit capacity through alternative metrics, international publishing, and reputation. Net beneficiary with low experienced extraction.
constraint_indexing:constraint_classification(publication_bias_narrative_dominance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN SCIENCE MOVEMENT (SCAFFOLD) — Organized agents (preprint servers, registered reports, open-access mandates) see publication bias as a temporary institutional failure with a sunset mechanism. Building alternative pathways: preprints reduce publication gate-keeping, registered reports eliminate hypothesizing after results known, open data enables post-publication peer review. Sunset timeline: 15-25 years for norms to mature across disciplines.
constraint_indexing:constraint_classification(publication_bias_narrative_dominance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL JOURNAL SYSTEM (PITON) — Maintains selection bias rituals (impact factor, rejection rates, editorial preferences) through institutional inertia despite awareness that the biases degrade field knowledge. Theater ratio elevated: peer review is performative quality control that masks selection bias. The system sees its own process as degraded (editors acknowledge publication bias) but maintains it because alternatives haven't fully displaced it.
constraint_indexing:constraint_classification(publication_bias_narrative_dominance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scope, publication bias appears inherent to any knowledge selection system: you cannot publish everything, therefore selection creates bias, therefore bias is a law of information scarcity. This perspective naturalizes what is actually a contingent institutional choice (which results get published, which metrics drive selection). Engine detects false summit: scarcity exists, but bias direction and magnitude are institutional, not natural.
constraint_indexing:constraint_classification(publication_bias_narrative_dominance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(publication_bias_narrative_dominance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(publication_bias_narrative_dominance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(publication_bias_narrative_dominance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(publication_bias_narrative_dominance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(publication_bias_narrative_dominance, TR),
    TR >= 0.70.

:- end_tests(publication_bias_narrative_dominance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Publication bias extracts researcher time and career capital through incentive misalignment — the bias system rewards publishable narratives over truthful findings. The extraction is substantial but not maximal because many published results are truthful; the bias is directional, not universal. Suppression (0.68): Moderately high. Barriers to exit include tenure requirements, funding body metrics, institutional prestige systems, and disciplinary norms. Researchers can exit by publishing in lower-status venues, but at significant career cost. The suppression is not total (some alternative venues exist) but severe enough to trap most career-dependent researchers. Theater ratio (0.64): Moderately high. Traditional peer review and impact metrics are substantially performative. Peer review assesses novelty and plausibility but does not catch reporting bias or narratively framed misinterpretation. Impact factor and citation counts measure prestige concentration rather than knowledge quality. The theater has increased over the interval as quantitative metrics have replaced qualitative editorial judgment. The constraint's extractiveness has increased from 0.35 to 0.58 over the measurement interval, indicating gradual accumulation of extraction as metrics become more refined and institutional incentives sharpen around them.
 *
 * PERSPECTIVAL GAP:
 *   The publication bias constraint demonstrates maximum perspectival divergence across power levels. Negative result researchers see pure extraction (Snare): the system offers no coordination benefit and extracts career capital. Early-career researchers see mixed coordination and extraction (Tangled Rope): the system both disseminates knowledge and biases what gets disseminated. Elite institutions see pure coordination (Rope): the bias system organizes attention to their work without subjective extraction cost. The open science coalition sees a temporary problem with sunset (Scaffold): preprints and registered reports are building alternative pathways. Journal editors see their own degraded ritual (Piton): they acknowledge publication bias while maintaining it. The civilizational analytical observer sees natural law (Mountain): information scarcity necessitates selection, therefore bias. This perspectival gap reveals that publication bias is not an inevitable feature of knowledge systems but a contingent institutional arrangement that extracts from the powerless while claiming to coordinate knowledge production.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality is determined by their structural relationship to the publication bias system. Negative result researchers with trapped exit bear maximum extraction (d ≈ 0.95, f(d) ≈ 1.42). Early-career researchers with constrained exit experience moderate extraction (d ≈ 0.65, f(d) ≈ 1.00). Elite institutions with arbitrage exit experience low or negative extraction — they benefit from the bias (d ≈ 0.10, f(d) ≈ -0.05). Editors and publishers with institutional/arbitrage position have artificially low d (≈ 0.05) derived from their beneficiary status, but identity_locked perspective suggests actual d should be higher (0.25-0.35) because their identity as 'quality guardians' prevents them from perceiving their own role in perpetuating bias. The directionality override captures this gap: institutional editors should have d ≈ 0.30 (constrained by identity lock) rather than d ≈ 0.05 (beneficiary with arbitrage exit).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that the Tangled Rope classification (claimed type) accurately captures the hybrid structure: genuine coordination function (journals do organize knowledge dissemination) exists alongside asymmetric extraction (bias systematically advantages certain narratives). The classification prevents two mislabelings: (1) treating publication bias as pure coordination (Rope) disguises the directional extraction from negative-result researchers, and (2) treating it as pure extraction (Snare) ignores the real coordination benefit journals provide. The Tangled Rope type forces acknowledgment that both functions are real and structurally coupled. The mandatrophy is resolved by recognizing that removing the extraction (moving to pure Rope) requires simultaneously changing the coordination mechanism — you cannot have neutral knowledge dissemination through profit-maximizing journals with impact-factor-driven selectivity. The constraint's persistence depends on naturalizing the coupling as inevitable (Mountain view) when it is actually institutional (contingent on metrics, career incentives, and journal economic models).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bias_directionality_threshold,
    'At what publication rejection rate does selection bias transition from coordination cost to extractive mechanism?',
    'Comparative study of field knowledge quality with varying rejection rates (measured by post-publication replication success, predictive accuracy, and critical flaw discovery); historical analysis of rejection rate changes and corresponding knowledge degradation',
    'If threshold < 40% rejection: current rates (60-90% in top journals) are clearly extractive. If threshold > 80%: high rejection rates may be justified coordination cost. Determines whether the constraint is Tangled Rope or Snare from powerless agent perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bias_directionality_threshold, empirical, 'Rejection rate threshold distinguishing coordination cost from extraction').

omega_variable(
    narrative_dominance_mechanism,
    'Does publication bias primarily operate through outcome reporting bias (selective result publication) or through narrative framing bias (selective interpretation of published results)?',
    'Meta-analysis comparing registered report protocols vs published narratives; analysis of how same datasets are narratively framed in different publication venues; tracking of narrative coherence vs statistical evidence in published findings',
    'If outcome reporting dominates: constraint operates through gate-keeping (snare dynamic). If narrative framing dominates: constraint operates through incentivized misinterpretation within published results (more systemic, affects institutional actors as well). Determines suppression magnitude and whether beneficiaries intentionally enforce bias or passively benefit from it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(narrative_dominance_mechanism, empirical, 'Whether bias operates through outcome reporting or narrative framing').

omega_variable(
    preprint_field_penetration,
    'Do preprint servers (arXiv, bioRxiv, medRxiv) actually change publication bias dynamics in their respective disciplines, or do they create a parallel unvetted literature while journals maintain their bias?',
    'Longitudinal study of citation patterns pre/post-preprint adoption per discipline; tracking whether preprint availability changes journal selectivity; measurement of whether journal-published papers reference preprints equally with journal-only results',
    'If preprints genuinely displace journal bias: scaffold perspective confirmed, sunset mechanism is real. If preprints create parallel literature: journals maintain control over prestige-marked knowledge, scaffold is aspirational, sunset is delayed significantly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preprint_field_penetration, empirical, 'Whether preprints effectively displace journal publication bias').

omega_variable(
    institutional_identity_lock,
    'To what extent are journal editors, publishers, and institutional administrators locked into bias-perpetuating incentive systems by their own identity as stewards of ''quality control'' and ''rigor''?',
    'Qualitative analysis of editorial decisions and institutional policies; tracking whether recognition of publication bias changes institutional behavior or is acknowledged but structurally ignored; documentation of institutions that claim to oppose bias while maintaining bias-generating metrics',
    'If identity-locked: institutional actors see themselves as preventing lower-quality work rather than extracting from researchers; this cognitive frame prevents exit from bias system. Reclassifies institutional actors from arbitrage (low-extraction perception) to identity_locked (high constraint coupling). If not identity-locked: institutional actors could easily change metrics and incentives but choose not to.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_identity_lock, conceptual, 'Whether institutional identity prevents recognition of bias perpetuation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(publication_bias_narrative_dominance, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pubbias_tr_t0, publication_bias_narrative_dominance, theater_ratio, 0, 0.48).
narrative_ontology:measurement(pubbias_tr_t10, publication_bias_narrative_dominance, theater_ratio, 10, 0.58).
narrative_ontology:measurement(pubbias_tr_t20, publication_bias_narrative_dominance, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(pubbias_be_t0, publication_bias_narrative_dominance, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pubbias_be_t10, publication_bias_narrative_dominance, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(pubbias_be_t20, publication_bias_narrative_dominance, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(publication_bias_narrative_dominance, information_standard).
narrative_ontology:affects_constraint(publication_bias_narrative_dominance, research_funding_concentration).
narrative_ontology:affects_constraint(publication_bias_narrative_dominance, institutional_prestige_hierarchy).
narrative_ontology:affects_constraint(publication_bias_narrative_dominance, researcher_career_path_dependency).

% DUAL FORMULATION NOTE:
% Publication bias decomposes into outcome reporting bias (selective publication of null vs positive results) and narrative framing bias (selective interpretation within published results). These constraints have distinct ε values: outcome reporting bias is more extractive (ε ≈ 0.65) because it operates through gate-keeping; narrative framing bias is less directly extractive (ε ≈ 0.45) but more systemic because it affects interpretation within the literature. This story focuses on the integrated constraint. Decomposition into separate stories recommended for detailed institutional analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(publication_bias_narrative_dominance, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
