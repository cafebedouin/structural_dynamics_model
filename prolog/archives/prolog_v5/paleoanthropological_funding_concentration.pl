% ============================================================================
% CONSTRAINT STORY: paleoanthropological_funding_concentration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paleoanthropological_funding_concentration, []).

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
 *   constraint_id: paleoanthropological_funding_concentration
 *   human_readable: Paleoanthropological Funding Concentration
 *   domain: academic_research/anthropology/funding
 *
 * SUMMARY:
 *   Paleoanthropological funding exhibits a structural asymmetry where
 *   research capital (grants, equipment, site permits, training) concentrates
 *   in elite Northern institutions, while the evidence base itself is
 *   geographically distributed across Africa, Asia, and South America. This
 *   creates a coordination problem — who decides which sites get studied,
 *   which hypotheses get tested, which research questions matter? — that is
 *   nominally solved through merit-based competitive funding. However, the
 *   merit-based system is corrupted by path dependence: elite institutions
 *   have advantages in the grant competition (track record, networks,
 *   resources for proposal development) that are difficult for emerging
 *   competitors to overcome. The result is a tangled coordination mechanism
 *   (genuine need to concentrate resources for complex fieldwork) layered
 *   with asymmetric extraction (wealth and priority flowing toward
 *   established institutions, intellectual labor and site access mediated
 *   through Northern researchers). The constraint has intensified over the
 *   past 20 years as funding has become more competitive and more
 *   concentrated in response to post-2008 austerity. Simultaneously,
 *   open-science movements (digital specimen databases, collaborative methods
 *   papers, crowdfunded expeditions, open-access journals) are creating
 *   alternative pathways that bypass the traditional funding bottleneck,
 *   suggesting a potential sunset to the concentration mechanism.
 *
 * KEY AGENTS:
 *   - Early-Career Researchers: Victims (powerless/trapped) — face credential barriers to funding and employment; require established publications and institutional affiliation to access competitive grants
 *   - Emerging Research Institutions: Victims (moderate/constrained) — regional universities in Africa, Asia, South America face barriers building paleoanthropology programs; funding agencies favor established centers; site access mediated by Northern institutions
 *   - Underfunded Regional Research Communities: Hybrid (moderate/constrained) — benefit from international collaborations and funding inflows but bear asymmetric extraction; intellectual property and authorship patterns favor Northern teams
 *   - Established Research Institutions: Beneficiaries (institutional/arbitrage) — elite universities experience funding concentration as coordination mechanism; reputation and endowments create compounding advantages in grant competition
 *   - Federal and Private Funding Agencies: Institutional agents (institutional/arbitrage) — structure funding mechanisms that nominally select by merit but actually reproduce institutional hierarchies
 *   - Open-Access Paleoanthropology Movement: Organized agents (organized/constrained) — arXiv-like repositories, data consortia, collaborative databases building alternative pathways; represent potential sunset mechanism
 *   - Peer Review and Grant Committee System: Institutional theater (institutional/arbitrage) — maintains performative review while reproducing in-group bias; composed primarily of established researchers
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional arrangement as immutable limit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paleoanthropological_funding_concentration, 0.58).
domain_priors:suppression_score(paleoanthropological_funding_concentration, 0.62).
domain_priors:theater_ratio(paleoanthropological_funding_concentration, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paleoanthropological_funding_concentration, extractiveness, 0.58).
narrative_ontology:constraint_metric(paleoanthropological_funding_concentration, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(paleoanthropological_funding_concentration, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paleoanthropological_funding_concentration, tangled_rope).
narrative_ontology:human_readable(paleoanthropological_funding_concentration, "Paleoanthropological Funding Concentration").
narrative_ontology:topic_domain(paleoanthropological_funding_concentration, "academic_research/anthropology/funding").

domain_priors:requires_active_enforcement(paleoanthropological_funding_concentration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paleoanthropological_funding_concentration, established_research_institutions).
narrative_ontology:constraint_beneficiary(paleoanthropological_funding_concentration, elite_universities).
narrative_ontology:constraint_beneficiary(paleoanthropological_funding_concentration, well_connected_researchers).
narrative_ontology:constraint_victim(paleoanthropological_funding_concentration, early_career_researchers).
narrative_ontology:constraint_victim(paleoanthropological_funding_concentration, underfunded_regions).
narrative_ontology:constraint_victim(paleoanthropological_funding_concentration, emerging_institutions).
narrative_ontology:constraint_victim(paleoanthropological_funding_concentration, field_diversity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY-CAREER RESEARCHER (SNARE) — Trapped by funding structure requiring established publications and institutional affiliation to access grants. Cannot exit without abandoning career. Must work within the concentration system or leave the field entirely. Maximum extraction from this position.
constraint_indexing:constraint_classification(paleoanthropological_funding_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EMERGING RESEARCH INSTITUTION (SNARE) — Regional institution seeking to build paleoanthropology program faces structural barriers: funding agencies concentrate grants in elite universities, limiting access to African field sites and international collaborations. Constrained exit through gradual reputation building, but extraction is severe during growth phase. High-cost alternative is relocation to established institution.
constraint_indexing:constraint_classification(paleoanthropological_funding_concentration, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: UNDERFUNDED REGIONAL RESEARCH COMMUNITY (TANGLED ROPE) — In Africa, Asia, and South America, paleoanthropological research benefits from international funding and collaborations, enabling knowledge advancement and local capacity building. Simultaneously, these researchers bear asymmetric extraction: funding flows through Northern institutions, intellectual property favors Northern teams, and site access is mediated by grants controlled by established centers. Genuine coordination coexists with embedded asymmetry.
constraint_indexing:constraint_classification(paleoanthropological_funding_concentration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ESTABLISHED RESEARCH INSTITUTION (ROPE) — Elite universities with endowments and existing grant portfolios experience the constraint as coordination: federal and private funding favors institutions with track records, enabling them to attract talent and fund expeditions. The constraint solves the coordination problem of 'where should funding go?' by using reputation as a proxy. Benefits from first-mover advantage and institutional inertia.
constraint_indexing:constraint_classification(paleoanthropological_funding_concentration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN-ACCESS PALEOANTHROPOLOGY MOVEMENT (SCAFFOLD) — Organized agents (digital repositories, collaborative databases, open-access journals) are building alternative funding and dissemination pathways. Crowdfunding, international consortia, and data-sharing platforms create sunset logic for the traditional concentration mechanism. As open methods reduce dependency on elite institutions for site access and data interpretation, the extraction mechanism weakens. Estimated sunset: 15-25 years for new norms to mature.
constraint_indexing:constraint_classification(paleoanthropological_funding_concentration, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: PEER REVIEW AND GRANT COMMITTEE SYSTEM (PITON) — Grant review committees are largely performative: they assess proposal quality using metrics (publication record, institutional prestige, preliminary data) that are themselves shaped by the concentration system. Committee members are drawn from established institutions, reproducing in-group bias. The ritual persists through institutional inertia despite evidence that review outcomes correlate with institutional affiliation, not proposal merit. High theater ratio reflects that the review process does not reliably identify best science but reproduces existing hierarchies.
constraint_indexing:constraint_classification(paleoanthropological_funding_concentration, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, funding concentration might appear as an immutable natural law: limited research funding always concentrates in proven institutions; complex fieldwork requires infrastructure that only established centers can provide; expert networks are path-dependent and cannot be rapidly redistributed. However, the structural data contradicts the mountain classification — the constraint emerges from policy choices (grant mechanisms, peer review composition, site access mediation), not physical laws. The engine's false summit detector identifies this as naturalization of a contingent institutional arrangement.
constraint_indexing:constraint_classification(paleoanthropological_funding_concentration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paleoanthropological_funding_concentration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(paleoanthropological_funding_concentration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(paleoanthropological_funding_concentration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(paleoanthropological_funding_concentration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(paleoanthropological_funding_concentration, TR),
    TR >= 0.70.

:- end_tests(paleoanthropological_funding_concentration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and increasing. The baseline (0.35 at t=0) reflects legitimate coordination need — complex paleoanthropological fieldwork requires sustained institutional capacity and cannot be freely distributed. However, the trajectory from 0.35 to 0.58 shows that concentration has increased beyond what coordination requires. The current level reflects that funding mechanisms extract wealth/priority from emerging researchers and institutions, not just coordinating resource allocation. Suppression (0.62): High. Multiple barriers constrain alternatives: (1) Geopolitical — site access requires government permits often mediated by historically-connected institutions; (2) Infrastructural — equipment, lab facilities, and training networks concentrate in elite institutions; (3) Career — publications and credentials required for grants are themselves generated within the concentration system; (4) Informational — informal networks dominate funding decisions, disadvantaging outsiders. Theater ratio (0.65): Moderate-high and increasing. Grant review committees assess proposals using metrics (publication record, institutional prestige, preliminary data, network connections) that are themselves products of the concentration system, creating circular validation. The review process does not reliably identify best science but reproduces hierarchies. The trajectory from 0.48 to 0.65 shows increasing performativity as funding has tightened and criteria have formalized, paradoxically making the system more ritualistic even as it claims greater rigor.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a classic extraction-disguised-as-coordination asymmetry. The nominal function is coordination — directing limited research resources to important discoveries. The actual extraction is: priority in claim-staking (which researchers publish first), control of narrative (which interpretations dominate), mediation of site access (which questions get studied), and wealth concentration (funding flows toward established institutions). Early-career researchers and underfunded regions experience the constraint as Snare or Tangled Rope. Established institutions experience it as Rope. The gap between these perspectives reveals the constraint's hybrid nature: it genuinely solves a coordination problem while simultaneously enabling asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position: who benefits and who bears costs. Established institutions with arbitrage options (can move research elsewhere, retain researchers) experience low effective extraction — the system works for them. Early-career researchers and emerging institutions without alternative pathways experience high extraction — trapped or constrained exit options apply the sigmoid transformation to produce high f(d). Regional researchers in Africa, Asia, South America experience moderate extraction — they benefit from funding inflows and international collaborations (lowering d somewhat) but lose control over site access, publication narrative, and career trajectories (raising d). The beneficiary/victim structure differentiates these positions mechanically: those declared as victims have high d → high f(d) → high chi, even when power levels are moderate or powerful, because their exit options are constrained or trapped within the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that Tangled Rope classification is appropriate. The constraint has a genuine coordination function (complex fieldwork requires sustained institutional capacity, geographic distribution of evidence requires coordinated international effort). It also has asymmetric extraction: funding concentration, career barriers, and site-access mediation flow benefits toward established institutions and bearers of costs toward early-career researchers and underfunded regions. The presence of both coordination and asymmetric extraction satisfies the Tangled Rope requirements: beneficiaries (established institutions), victims (early-career researchers, emerging institutions), and active enforcement (funding mechanisms, peer review, permit systems). The alternative classifications (pure Rope if only coordination; pure Snare if only extraction) would mislabel the constraint. The Tangled Rope classification holds: genuine coordination with embedded extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    funding_concentration_threshold,
    'What distribution of funding across institutions constitutes unequal concentration vs. natural clustering around research excellence?',
    'Gini coefficient of funding distribution; comparison against null model of random allocation; analysis of funding concentration vs. output metrics (discoveries, publications, field advancement)',
    'If concentration correlates with research output: system may be meritocratic (Rope). If concentration exceeds output variance: system is extractive (Snare/Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(funding_concentration_threshold, empirical, 'Threshold distinguishing natural clustering from excessive concentration').

omega_variable(
    alternative_funding_effectiveness,
    'Do alternative funding models (crowdfunding, international consortia, regional grants) produce equally significant discoveries at lower concentration levels?',
    'Comparative analysis of discovery significance, methodological rigor, and field impact for research funded through alternative pathways vs. traditional agencies. Citation analysis and expert assessment of innovation quality.',
    'If alternative funding produces equivalent science: scaffold sunset is real and extraction mechanism is contingent on funding structure. If alternative funding produces lower-quality work: concentration may reflect genuine epistemic inequality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_funding_effectiveness, empirical, 'Whether alternative funding models can sustain high-quality paleoanthropology').

omega_variable(
    site_access_mediation_mechanism,
    'To what extent does funding concentration derive from control of site access vs. control of research capital (equipment, training, publication channels)?',
    'Analysis of site access patterns: which institutions have permits, which researchers lead expeditions, how much site access correlates with grant funding. Disaggregation of funding flow: equipment/infrastructure vs. personnel vs. direct research costs.',
    'If primarily site access: constraint is geopolitically driven (governments control permits, Northern institutions have historical relationships). If primarily research capital: constraint is institutional-structural and more malleable through policy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(site_access_mediation_mechanism, empirical, 'Whether concentration derives from site access control or research capital monopoly').

omega_variable(
    identity_lock_in_institutional_affiliation,
    'Do researchers and institutions experience funding concentration as an unchangeable feature of career structure (identity lock) or as a high-cost but surmountable barrier (constrained exit)?',
    'Qualitative analysis of researcher narratives; career trajectory data for researchers who attempted to build programs outside elite institutions; exit rate analysis for early-career researchers',
    'If identity-locked: constraint is experienced as ''how things are'' even when structural change is possible. If constrained: agents perceive alternatives but face high costs. Identity lock prevents organizational coordination for reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_institutional_affiliation, conceptual, 'Whether institutional position is internalized as identity or perceived as contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paleoanthropological_funding_concentration, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(paleo_tr_t0, paleoanthropological_funding_concentration, theater_ratio, 0, 0.48).
narrative_ontology:measurement(paleo_tr_t10, paleoanthropological_funding_concentration, theater_ratio, 10, 0.58).
narrative_ontology:measurement(paleo_tr_t20, paleoanthropological_funding_concentration, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(paleo_be_t0, paleoanthropological_funding_concentration, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(paleo_be_t10, paleoanthropological_funding_concentration, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(paleo_be_t20, paleoanthropological_funding_concentration, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paleoanthropological_funding_concentration, resource_allocation).
narrative_ontology:affects_constraint(paleoanthropological_funding_concentration, paleoanthropological_methodological_pluralism).
narrative_ontology:affects_constraint(paleoanthropological_funding_concentration, indigenous_knowledge_intellectual_property).

% DUAL FORMULATION NOTE:
% Funding concentration is upstream of specific epistemological debates in paleoanthropology (e.g., methodological pluralism around trait interpretation, integration of indigenous knowledge). The funding mechanism controls which research questions get asked and which researchers can ask them, making it a structural constraint on the field's epistemic diversity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(paleoanthropological_funding_concentration, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
