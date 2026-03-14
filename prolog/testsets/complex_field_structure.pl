% ============================================================================
% CONSTRAINT STORY: complex_field_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_complex_field_structure, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: complex_field_structure
 *   human_readable: Complex Field Structure: Coordination and Extraction in Institutional Knowledge Domains
 *   domain: institutional/epistemology
 *
 * SUMMARY:
 *   Complex field structures in academia, science, and professional knowledge
 *   domains exhibit a fundamental tension: genuine coordination problems
 *   (organizing knowledge, maintaining standards, training new practitioners)
 *   are solved through credential systems, publication gatekeeping, and
 *   institutional specialization. But these same coordination mechanisms
 *   enable extraction: gatekeepers capture rents through access control, new
 *   entrants are suppressed through artificial complexity barriers, and
 *   knowledge is trapped in siloed institutional domains despite cross-field
 *   applicability. The constraint classifies as tangled_rope because it is
 *   genuinely both — the field structure solves real coordination problems
 *   (specialization enables depth) while simultaneously extracting from
 *   powerless agents (new entrants facing accumulated complexity barriers)
 *   and suppressing interdisciplinary knowledge synthesis. The theater_ratio
 *   (0.68) indicates that traditional peer review, credential signaling, and
 *   journal organization have become substantially performative: they
 *   maintain field gatekeeping and boundary policing more effectively than
 *   they validate knowledge or facilitate discovery. Open-science platforms
 *   (arXiv, GitHub, Stack Exchange, open-source tools) represent an
 *   alternative coordination pathway that maintains specialization benefits
 *   while reducing theatrical overhead — distributed scrutiny replaces
 *   journal gatekeeping, transparent methods replace credential signaling.
 *   The constraint exhibits the full six-type spectrum depending on observer
 *   position, making it a diagnostic exemplar for how institutional
 *   coordination becomes extraction.
 *
 * KEY AGENTS:
 *   - New Entrants: Primary victims (powerless/trapped) — face accumulated complexity, gatekeeping through credentials, implicit norms, tacit knowledge barriers; no viable exit except external knowledge reproduction
 *   - Field Gatekeepers: Primary beneficiaries (institutional/arbitrage) — universities, research institutes, professional societies benefit from specialization network effects, credential monopolies, publication control
 *   - Mid-Career Interdisciplinary Researchers: Secondary victims (moderate/constrained) — bridges multiple fields but penalized by incentive structures; constrained exit prevents full integration into either field
 *   - Field Accessibility: Collective victim (powerless/trapped) — abstract good representing knowledge diffusion and cross-field synthesis that cannot organize or exit
 *   - Open Science Movement: Organized agents (organized/constrained) — arXiv, GitHub, open-access publishers, institutional repositories building alternative coordination pathways with sunset logic
 *   - Peer Review Apparatus: Institutional degradation (institutional/arbitrage) — maintains performative gatekeeping ritual; sees own process as increasingly theater (piton perspective)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements (specialization, credential gatekeeping, journal gatekeeping) as inherent properties of knowledge itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(complex_field_structure, 0.38).
domain_priors:suppression_score(complex_field_structure, 0.52).
domain_priors:theater_ratio(complex_field_structure, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(complex_field_structure, extractiveness, 0.38).
narrative_ontology:constraint_metric(complex_field_structure, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(complex_field_structure, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(complex_field_structure, tangled_rope).
narrative_ontology:human_readable(complex_field_structure, "Complex Field Structure: Coordination and Extraction in Institutional Knowledge Domains").
narrative_ontology:topic_domain(complex_field_structure, "institutional/epistemology").

domain_priors:requires_active_enforcement(complex_field_structure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(complex_field_structure, field_gatekeepers).
narrative_ontology:constraint_beneficiary(complex_field_structure, established_institutions).
narrative_ontology:constraint_victim(complex_field_structure, field_accessibility).
narrative_ontology:constraint_victim(complex_field_structure, new_entrants).
narrative_ontology:constraint_victim(complex_field_structure, interdisciplinary_researchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% New researchers face accumulated complexity barriers: specialized jargon, prerequisite knowledge assumed rather than taught, implicit norms, gatekeeping through credential requirements. Exit requires abandoning career trajectory or reproducing the entire knowledge foundation externally. Maximum experienced extraction — the field structure extracts obedience and conformity as price of entry.
constraint_indexing:constraint_classification(complex_field_structure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Bridges multiple fields but penalized by institutional incentives that reward depth specialization over breadth. Genuine coordination benefit: cross-field insight and synthesis. But asymmetric extraction: dual-field researchers cite and train field specialists more than fields cite them; institutional recognition (tenure, funding) privileges established-field membership. Constrained exit: can move to one field but loses intellectual community and methodological toolkit.
constraint_indexing:constraint_classification(complex_field_structure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Universities, research institutes, professional societies experience field structure as pure coordination: training pipelines, credential systems, literature organization all solve genuine collective action problems. Field institutions benefit from network effects and brand value. Exit options abundant — institutions can reshape research focus, recruit from other fields, establish new departments. Net beneficiary with low experienced extraction.
constraint_indexing:constraint_classification(complex_field_structure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Organized agents (preprint archives, open-access publishers, open-source tools) treat field complexity as a temporary coordination failure being solved through distributed knowledge representation (GitHub, Stack Exchange, institutional repositories). Building alternative pathways that bypass traditional gatekeeping. Low effective extraction because organized agents have agency and see exit routes through technological and institutional innovation with sunset logic — as open infrastructure matures, field gatekeeping loses enforcement power.
constraint_indexing:constraint_classification(complex_field_structure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Peer review for complex field-specific claims has become substantially performative: reviewers assess conformance to field norms and internal consistency but lack mechanisms to verify empirical adequacy across fields or detect systematic bias in field-specific assumptions. Theater persists through institutional inertia — journals maintain review rituals despite reduced epistemic function. Field institutions maintain peer review because alternatives haven't fully replaced it, not because it validates cross-field claims effectively.
constraint_indexing:constraint_classification(complex_field_structure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From civilizational perspective, some field structure is inherent to knowledge accumulation: specialization increases power, complexity increases barriers, deep expertise requires years of training. This view naturalizes field structure as inevitable. However, the structural data contradicts pure mountain classification — measurement shows extractiveness increasing over time and theater ratio rising (normalized knowledge representation being displaced by performative credential displays), indicating a contingent institutional arrangement rather than irreducible natural law.
constraint_indexing:constraint_classification(complex_field_structure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(complex_field_structure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(complex_field_structure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(complex_field_structure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(complex_field_structure, TR),
    TR >= 0.70.

:- end_tests(complex_field_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Field complexity creates genuine coordination benefits (specialization enables depth, credential systems enable trust) but also enables extraction through access control. The value reflects that extraction is real but not maximal — some of the complexity is legitimate, some is gatekeeping. Trajectory from 0.28 to 0.38 over the interval indicates accumulating rent-seeking layered onto coordination. Suppression (0.52): Moderate-high. Barriers to entry include jargon, prerequisite knowledge, implicit norms, credential requirements, and tacit knowledge. But suppression is not total — some practitioners develop outside traditional institutions and open-access platforms are reducing barriers. Theater ratio (0.68): Moderate-high. Peer review for complex field-specific claims is substantially performative — reviewers assess conformance to field norms but cannot validate cross-field assumptions or detect systematic field-specific bias. Journal organization, credential signaling, and publication prestige increasingly reflect theatrical boundary maintenance rather than epistemic function. Trajectory from 0.52 to 0.68 indicates theater is rising as complexity accumulates and institutional gatekeeping becomes more sophisticated. Open-science pathways (arXiv, GitHub, Stack Exchange) have lower theater because distributed scrutiny and transparent methods bypass traditional gatekeeping rituals.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximal perspectival divergence. Field institutions experience pure coordination (Rope) — specialization solves legitimate knowledge problems. New entrants experience pure extraction (Snare) — complexity barriers prevent entry. Mid-career researchers experience mixed coordination and extraction (Tangled Rope) — genuine intellectual benefits alongside career penalties. Open science movement experiences temporary problem with sunset (Scaffold) — distributed knowledge systems are building alternative pathways. Peer review apparatus experiences degraded ritual (Piton) — theater persists through institutional inertia, not epistemic function. Analytical observer risks seeing natural law (Mountain) — specialization is inevitable — but measurement reveals this as false summit: the increasing theater_ratio and rising extractiveness show institutional arrangements shifting toward gatekeeping, not toward efficiency.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position within the knowledge flow. New entrants (trapped + powerless) experience maximum directionality toward extraction (d ≈ 0.95); field institutions (arbitrage + institutional) benefit (d ≈ 0.10); mid-career interdisciplinary researchers (constrained + moderate) experience asymmetric extraction (d ≈ 0.60). Open science movement (organized agents with constrained exit) experience moderate extraction (d ≈ 0.45) because they have exit routes and can reshape the knowledge commons. The beneficiary/victim declarations flow directly from this structure: field institutions and gatekeepers benefit; new entrants, accessibility, and interdisciplinary synthesis bear costs. No overrides needed — structural derivation produces accurate directionalities.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that field structure IS simultaneously coordination and extraction, viewed from different positions. The tension is not resolvable into a single type — it is a genuine tangled_rope where the coordination function (specialization, standards, training pipelines) and the extraction mechanism (gatekeeping, suppression of interdisciplinary synthesis, barriers to entry) are structurally inseparable at the institutional level. The scaffold perspective reveals a genuine exit path (open-science infrastructure with 10-15 year maturation), which prevents infinite extraction. The piton perspective reveals degradation (theater rising, peer review becoming performative), which indicates the constraint is not optimizing its coordination function but rather drifting toward pure gatekeeping. Resolution requires observing whether open-science pathways mature fast enough to constitute real sunset, or whether institutional gatekeeping reasserts control over distributed knowledge infrastructure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    specialization_necessity_threshold,
    'What degree of specialization is genuinely necessary for productive knowledge accumulation versus what degree is extractive gatekeeping disguised as specialization?',
    'Comparative analysis of high-productivity cross-field integration (interdisciplinary breakthroughs) versus low-productivity siloed specialization; measurement of citation diversity and knowledge reuse across field boundaries',
    'If threshold is high: field specialization is necessary coordination (Rope from more perspectives). If threshold is low: most field complexity is extractive barrier maintenance (Snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(specialization_necessity_threshold, empirical, 'Threshold distinguishing necessary specialization from extractive gatekeeping').

omega_variable(
    tacit_knowledge_transferability,
    'Is the accumulated tacit knowledge in a field (methods, intuitions, judgment calls) genuinely difficult to transfer across fields, or is the difficulty artificially maintained through socialization practices and credential gatekeeping?',
    'Measurement of knowledge transfer rates in open-source and open-science contexts versus closed institutional contexts; tracking of successful self-taught practitioners and cross-field migrations',
    'If genuinely difficult: field structure is legitimate coordination overhead (moderate extraction). If artificially maintained: field structure is extractive suppression mechanism with inflated theater (high extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tacit_knowledge_transferability, empirical, 'Whether tacit knowledge barriers are structural or socially maintained').

omega_variable(
    open_knowledge_pathway_maturity,
    'Are decentralized open-science platforms (arXiv, GitHub, Stack Exchange, institutional repositories) maturing rapidly enough to constitute a genuine sunset for traditional field gatekeeping, or is open-science infrastructure still too nascent to replace disciplinary structures?',
    'Longitudinal tracking of knowledge discovery rates, career advancement rates for practitioners outside traditional institutions, and citation patterns for open-science outputs versus traditional journal outputs',
    'If maturing rapidly: scaffold classification is accurate and sunset is structural (10-15 year timeline). If slow: open-science is aspirational and traditional field structure is more durable (30+ year timeline or persistent).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_knowledge_pathway_maturity, empirical, 'Rate of open-science pathway maturation and traditional gatekeeping replacement').

omega_variable(
    field_boundary_permeability,
    'Are institutional boundaries between fields permeable enough to allow genuine integration and knowledge flow, or do institutional incentive structures (funding silos, hire-within-discipline norms, journal specialty organization) maintain de facto boundaries that suppress cross-field synthesis?',
    'Analysis of hiring patterns, funding allocation by discipline, citation patterns across fields, and measurement of research output diversity in integrated versus siloed institutional structures',
    'If permeable: field structure is coordination (Rope/Scaffold). If impermeable: field structure is extraction mechanism maintaining disciplinary monopolies (Snare/Tangled Rope from more perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(field_boundary_permeability, empirical, 'Institutional permeability of field boundaries and cross-field integration capacity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(complex_field_structure, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cfs_tr_t0, complex_field_structure, theater_ratio, 0, 0.52).
narrative_ontology:measurement(cfs_tr_t5, complex_field_structure, theater_ratio, 5, 0.6).
narrative_ontology:measurement(cfs_tr_t10, complex_field_structure, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(cfs_be_t0, complex_field_structure, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cfs_be_t5, complex_field_structure, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(cfs_be_t10, complex_field_structure, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(complex_field_structure, information_standard).
narrative_ontology:affects_constraint(complex_field_structure, credential_gatekeeping).
narrative_ontology:affects_constraint(complex_field_structure, journal_prestige_hierarchy).
narrative_ontology:affects_constraint(complex_field_structure, knowledge_commons_fragmentation).

% DUAL FORMULATION NOTE:
% Complex field structure is upstream of specific gatekeeping mechanisms. Credential gatekeeping focuses on access control; journal hierarchy focuses on publication prestige; knowledge fragmentation focuses on cross-field synthesis barriers. All three are downstream of the general field structure constraint and amplify its extractive effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
