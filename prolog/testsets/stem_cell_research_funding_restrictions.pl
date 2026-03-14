% ============================================================================
% CONSTRAINT STORY: stem_cell_research_funding_restrictions
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stem_cell_research_funding_restrictions, []).

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
 *   constraint_id: stem_cell_research_funding_restrictions
 *   human_readable: Stem Cell Research Funding Restrictions
 *   domain: biomedical_research/regulatory_policy
 *
 * SUMMARY:
 *   Stem cell research funding restrictions represent a structural constraint
 *   where genuine coordination function (protecting widely-held ethical
 *   positions regarding embryo moral status) coexists with asymmetric
 *   extraction (researchers and patients bear time and opportunity costs).
 *   The constraint operates across multiple institutional levels: federal
 *   categorical funding restrictions, state-level alternative funding
 *   programs, institutional compliance mechanisms, and international funding
 *   flows. Over the interval analyzed, the constraint has shifted toward
 *   higher extractiveness as scientific alternatives (iPSCs, organoids,
 *   direct reprogramming) have matured, making the original restriction
 *   increasingly selective rather than universal — restricting a specific
 *   pathway rather than blocking research directions entirely.
 *   Simultaneously, theater ratio has increased as enforcement focuses on
 *   categorical compliance (embryonic origin) rather than actual research
 *   risk (embryonic cell function), and workarounds proliferate that achieve
 *   restricted research goals through alternative methodologies. The
 *   constraint exhibits all stages of the DR taxonomy from different agent
 *   perspectives: pure extraction for trapped researchers (Snare), mixed
 *   coordination-extraction for institutions (Tangled Rope), genuine
 *   coordination for the ethical advocacy coalition (Rope), market
 *   opportunity for alternative funders (Rope), degraded theater for the
 *   funding agency (Piton), and temporary problem with sunset pathway for the
 *   scientific workaround coalition (Scaffold).
 *
 * KEY AGENTS:
 *   - Stem Cell Researchers (Powerless/Trapped): Structurally trapped within restricted jurisdictions; cannot pursue specific research directions without geographic relocation, institutional loss, or career abandonment. Primary victims.
 *   - Patient Populations (Powerless/Trapped): Geographically trapped; bear the cost of delayed therapeutic development with no agency in policy decisions. Primary victims.
 *   - Research Institutions (Powerful/Mobile): Coordinate across restricted and unrestricted funding sources; enforce restrictions as compliance requirement; benefit from regulatory simplification and reduced competitor access. Secondary beneficiary.
 *   - Religious/Ethical Advocacy Coalition (Institutional/Arbitrage): Primary beneficiary; successfully coordinates around embryo moral status; experiences restriction as legitimate protection mechanism with minimal extraction cost.
 *   - Alternative Funding Coalition (Organized/Arbitrage): Public and private funders explicitly funding restricted research; benefit from regulatory fragmentation creating market opportunity; coordinate alternative research flows.
 *   - Funding Agency Bureaucracy (Institutional/Constrained): Maintains categorical enforcement increasingly divorced from actual research risk; theater increases as workarounds proliferate; constrained by statute predating scientific developments.
 *   - Scientific Workaround Coalition (Organized/Constrained): Deliberately developing functionally equivalent alternatives to render original restriction scientifically irrelevant; structured toward sunset as alternatives mature.
 *   - Analytical Observer (Analytical/Analytical): Sees constraint as genuine coordination-extraction hybrid requiring mandatrophy resolution across civilizational timescale.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stem_cell_research_funding_restrictions, 0.58).
domain_priors:suppression_score(stem_cell_research_funding_restrictions, 0.68).
domain_priors:theater_ratio(stem_cell_research_funding_restrictions, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stem_cell_research_funding_restrictions, extractiveness, 0.58).
narrative_ontology:constraint_metric(stem_cell_research_funding_restrictions, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(stem_cell_research_funding_restrictions, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stem_cell_research_funding_restrictions, tangled_rope).
narrative_ontology:human_readable(stem_cell_research_funding_restrictions, "Stem Cell Research Funding Restrictions").
narrative_ontology:topic_domain(stem_cell_research_funding_restrictions, "biomedical_research/regulatory_policy").

domain_priors:requires_active_enforcement(stem_cell_research_funding_restrictions).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stem_cell_research_funding_restrictions, religious_advocacy_groups).
narrative_ontology:constraint_beneficiary(stem_cell_research_funding_restrictions, alternative_research_funders).
narrative_ontology:constraint_beneficiary(stem_cell_research_funding_restrictions, political_gatekeepers).
narrative_ontology:constraint_victim(stem_cell_research_funding_restrictions, stem_cell_researchers).
narrative_ontology:constraint_victim(stem_cell_research_funding_restrictions, patient_populations).
narrative_ontology:constraint_victim(stem_cell_research_funding_restrictions, basic_research_advancement).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RESEARCHER (SNARE) — Trapped within a funding jurisdiction with categorical restrictions on embryonic stem cell research. Cannot pursue specific research directions without relocating, losing institutional affiliation, or abandoning career path. Bears full extraction cost of foregone research opportunities and career delays. Maximum suppression from both regulatory barriers and reputational risk.
constraint_indexing:constraint_classification(stem_cell_research_funding_restrictions, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PATIENT POPULATION (SNARE) — Structurally trapped: cannot exit the geography where stem cell therapies are restricted. Bears the extraction cost of delayed therapeutic development. No alternative pathway; no agency in the decision structure. Maximum extraction with maximum suppression.
constraint_indexing:constraint_classification(stem_cell_research_funding_restrictions, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: RESEARCH INSTITUTION (TANGLED ROPE) — Coordinates across departments and funding sources (some restricted, some unrestricted). Benefits from federal funding allocations that avoid restricted categories. Experiences mixed extraction and coordination: must enforce restrictions as a condition of funding (active enforcement), but also benefits from the simplified compliance landscape when competitors are similarly constrained. Mobile enough to shift research emphasis but constrained by existing faculty commitments and institutional reputation.
constraint_indexing:constraint_classification(stem_cell_research_funding_restrictions, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: RELIGIOUS ADVOCACY COALITION (ROPE) — Primary beneficiary with arbitrage options. Successfully coordinates a coalition around shared ethical framework (embryo moral status). Sees the funding restriction as a legitimate coordination mechanism protecting core values. Experiences minimal extraction; benefits directly from regulatory alignment with their ethical positions. Can exit by accepting alternative framing without material loss.
constraint_indexing:constraint_classification(stem_cell_research_funding_restrictions, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ALTERNATIVE FUNDING COALITION (ROPE) — Includes private foundations, state-level programs, and international funding bodies that explicitly fund research restricted at the federal level. Experiences the restriction as creating market opportunity rather than constraint. Coordinates research flows into alternative pathways (state funds, private biotech, international collaborations). Benefits from the arbitrage created by regulatory fragmentation.
constraint_indexing:constraint_classification(stem_cell_research_funding_restrictions, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: FUNDING AGENCY BUREAUCRACY (PITON) — Maintains categorical restrictions that have become increasingly performative as the science has shifted toward induced pluripotent stem cells (iPSCs) and organoids, which often bypass the original restriction logic. Theater manifests in categorical enforcement divorced from actual research risk: 'embryonic' is blocked while functionally equivalent lines created through alternative methods are allowed. Perpetuated through institutional inertia and statutory language that predates scientific developments.
constraint_indexing:constraint_classification(stem_cell_research_funding_restrictions, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: SCIENTIFIC WORKAROUND COALITION (SCAFFOLD) — Organized researchers and institutions developing functionally equivalent alternatives (iPSCs, direct reprogramming, organoids from adult cells) that achieve research goals while technically complying with restrictions. Sees the bottleneck as temporary: as alternatives mature and deliver equivalent results, the original restriction becomes vestigial. Scaffold logic applies because the workaround pathway is time-bound and deliberately sunset-oriented — the goal is to render the original restriction scientifically irrelevant.
constraint_indexing:constraint_classification(stem_cell_research_funding_restrictions, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, the restriction exhibits genuine coordination function (protecting a widely-held ethical position around embryo status) AND asymmetric extraction (researchers and patients bear time and opportunity costs; beneficiary coalition gains regulatory alignment). The constraint is neither pure coordination nor pure extraction but a hybrid requiring active enforcement from funding agencies. Requires mandatrophy resolution: coordinate a tradeoff (protect embryos) while extracting from those who disagree with the premise.
constraint_indexing:constraint_classification(stem_cell_research_funding_restrictions, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stem_cell_research_funding_restrictions_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(stem_cell_research_funding_restrictions, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(stem_cell_research_funding_restrictions, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(stem_cell_research_funding_restrictions, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(stem_cell_research_funding_restrictions, TR),
    TR >= 0.70.

:- end_tests(stem_cell_research_funding_restrictions_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The restriction creates measurable career delays and research opportunity costs for targeted researchers and patients. However, extraction is not maximal because: (1) alternative funding sources exist at state and private levels, providing partial exit; (2) scientific workarounds (iPSCs, organoids) offer pathways to equivalent research goals; (3) some restriction beneficiaries (ethical coalition) genuinely believe the protection justifies the cost. The value reflects that extraction is real but not absolute. Suppression (0.68): Moderate-high. Multiple suppression mechanisms combine: regulatory barriers (federal funding categoricals), reputational barriers (careers in restricted domains carry stigma in some jurisdictions), institutional barriers (compliance costs for dual-track funding), exit barriers (geographic mobility constraints, sunk institutional investments). Theater ratio (0.65): Moderate-high. Theater manifests in categorical enforcement increasingly divorced from actual risk. The original restriction (protecting embryo moral status) was intellectually coherent when embryonic stem cells were unique. As functionally equivalent alternatives proliferated, enforcement shifted toward maintaining categorical boundaries rather than achieving the underlying ethical goal. A researcher using iPSCs derived from embryo-equivalent developmental stages faces no restriction despite functionally similar research; this selective enforcement increases theater. Claimed type (Tangled Rope): Dual classification requirement satisfied — genuine coordination function (protecting embryo moral status, widely held ethical commitment) AND asymmetric extraction (researchers and patients bear concentrated costs; beneficiary coalition captures regulatory alignment). Requires active enforcement (federal funding agency compliance mechanisms). Beneficiary/victim declarations: Clear beneficiaries (advocacy coalition, alternative funders); clear victims (researchers, patients).
 *
 * PERSPECTIVAL GAP:
 *   The restriction creates maximum perspectival distance between trapped researchers and advocacy beneficiaries. The researcher in a restricted jurisdiction sees immutable constraint (mountain-like at immediate timescale) because exit is materially blocked; the advocacy coalition sees legitimate coordination (rope) because their exit costs are minimal; the analytical observer sees tangled hybrid because they can measure both genuine coordination function AND asymmetric extraction simultaneously. This gap reveals the restriction's dual nature: it is not pure extraction (the ethical commitment is real) but also not pure coordination (the cost distribution is highly asymmetric).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) vary sharply across agent positions based on structural relationship to the extraction flow. Trapped researchers experience high d (~0.95) because they are full victims with no exit options: suppression multiplier and power deficit both push toward maximum χ. Advocacy coalition experiences low d (~0.10) as primary beneficiary with arbitrage options (can accept alternative framing, mobilize around competing issues) — low f(d). Research institutions experience moderate-high d (~0.65) as secondary beneficiaries caught between compliance burden and funding advantage: constrained exit (cannot ignore federal funds) but also benefits. The analytical observer at civilizational scope experiences d~0.72 as a structured observer without power advantage or deficit — captures the system-level extraction asymmetry. Scope modifier σ(S) shifts χ values: national scope (σ=1.0) applies to researcher perspective; global scope (σ=1.2) applies to analytical observer, amplifying perceived extraction since consequences are geographically distributed.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION VIA DUAL CLASSIFICATION: The constraint satisfies the mandatrophy by acknowledging that it coordinates genuine ethical commitment (embryo protection) while extracting from researchers and patients (time, opportunity, therapeutic delays). The resolution is not to deny one or the other, but to measure both simultaneously. From the advocacy coalition's perspective, the restriction is ethically necessary coordination with unavoidable but acceptable costs. From the trapped researcher's perspective, the restriction is extraction that should not be tolerated. Both are structurally correct from their observational positions. The analytical resolution is to (1) acknowledge the coordination function is genuine, (2) measure the extraction cost accurately, (3) ask whether alternative mechanisms could achieve the coordination goal with lower extraction (e.g., enhanced oversight rather than categorical ban), and (4) recognize that rejection of the coordination goal itself ('embryo protection is not legitimate') is a value choice, not a structural discovery. The mandatrophy persists at the value level: reconciling embryo protection with research freedom requires negotiating the tradeoff, not proving one side is wrong.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_status_ontology,
    'What is the ontological basis for treating embryonic stem cell research as morally distinct from functionally equivalent research using iPSCs or organoids?',
    'Formal analysis of ethical frameworks underlying the restriction; comparison of actual biological properties of embryonic vs alternative cell sources; empirical study of whether restriction-adopters can articulate consistent principle across functionally similar technologies',
    'If embryo moral status is unjustified or inconsistently applied: restriction is pure extraction dressed as coordination. If embryo status is justified: restriction is genuine coordination with unavoidable asymmetric costs. If inconsistently applied: classification shifts toward snare (enforcement via selective interpretation).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_status_ontology, conceptual, 'Ontological basis for embryo moral status distinction').

omega_variable(
    alternative_pathway_sufficiency,
    'Do iPSCs, organoids, and other alternatives provide equivalent research capability to embryonic stem cells, or do they carry permanent research gaps?',
    'Comparative analysis of research questions answerable via each pathway; longitudinal tracking of scientific progress using alternatives; expert consensus on remaining irreplaceable applications',
    'If alternatives are sufficient: scaffold sunset is real and the restriction becomes purely extractive (researchers achieve goals elsewhere). If permanent gaps exist: some extraction is unavoidable and restriction has legitimate coordination function protecting research that cannot be redirected.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_pathway_sufficiency, empirical, 'Whether alternative cell sources provide equivalent research capability').

omega_variable(
    coalition_identity_fusion,
    'Is the religious/ethical advocacy coalition identity-locked into the embryo protection frame, or would they accept alternative regulations that achieve the same ethical goals via different mechanisms?',
    'Analysis of coalition response to alternative proposals (e.g., enhanced oversight, derivation limits, consent frameworks) that protect embryo dignity without blanket restrictions; testing whether coalition prioritizes outcome (embryo protection) or method (restriction as enforcement mechanism)',
    'If identity-locked: the constraint serves to express coalition identity more than to achieve protection, potentially signaling Snare elements. If outcome-focused: coalition would negotiate alternative mechanisms, suggesting genuine coordination rather than extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_identity_fusion, empirical, 'Whether advocacy coalition is identity-locked into embryo restriction frame').

omega_variable(
    researcher_exit_capacity,
    'What proportion of restricted-jurisdiction researchers have realistic exit options (geographic relocation, career transition, institutional migration)?',
    'Survey of career trajectories for researchers in restricted jurisdictions; measurement of geographic mobility barriers; analysis of career costs associated with exit',
    'If exit is realistic for most researchers: classification should upgrade from ''trapped'' to ''constrained''. If exit is blocked by network effects or sunk costs: ''trapped'' classification is sustained and extraction is maximum.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(researcher_exit_capacity, empirical, 'Researcher exit capacity from restricted jurisdictions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stem_cell_research_funding_restrictions, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stemfund_tr_t0, stem_cell_research_funding_restrictions, theater_ratio, 0, 0.48).
narrative_ontology:measurement(stemfund_tr_t5, stem_cell_research_funding_restrictions, theater_ratio, 5, 0.58).
narrative_ontology:measurement(stemfund_tr_t10, stem_cell_research_funding_restrictions, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(stemfund_be_t0, stem_cell_research_funding_restrictions, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(stemfund_be_t5, stem_cell_research_funding_restrictions, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(stemfund_be_t10, stem_cell_research_funding_restrictions, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stem_cell_research_funding_restrictions, identity_coordination).
narrative_ontology:affects_constraint(stem_cell_research_funding_restrictions, induced_pluripotent_stem_cell_development).
narrative_ontology:affects_constraint(stem_cell_research_funding_restrictions, therapeutic_translation_timelines).
narrative_ontology:affects_constraint(stem_cell_research_funding_restrictions, international_research_brain_drain).

% DUAL FORMULATION NOTE:
% Stem cell funding restrictions decompose into two functionally distinct constraints: (1) embryo_moral_status_protection (ε≈0.15, Rope) — genuine coordination among those who accept embryo moral status as binding; (2) researcher_research_direction_blocking (ε≈0.58, Snare/Tangled Rope) — extraction effect on those who reject the premise. These stories are linked because acceptors/rejecters interact within the same regulatory system. The integrated story (this file) shows the constraint as experienced by agents navigating both functions simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(stem_cell_research_funding_restrictions, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
