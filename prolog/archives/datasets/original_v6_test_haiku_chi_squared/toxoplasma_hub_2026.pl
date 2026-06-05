% ============================================================================
% CONSTRAINT STORY: toxoplasma_hub_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_toxoplasma_hub_2026, []).

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
 *   constraint_id: toxoplasma_hub_2026
 *   human_readable: The Toxoplasma Cyst as an Active Hub
 *   domain: biological/medical/parasitology
 *
 * SUMMARY:
 *   Single-cell RNA sequencing (scRNA-seq) of Toxoplasma gondii tissue cysts
 *   has revealed that the parasitic stage long described as 'latent' and
 *   'metabolically dormant' is actually metabolically active, with continuous
 *   transcript expression reflecting biosynthesis, energy production, and
 *   immune interaction. This discovery creates a structural constraint
 *   spanning from the chronic infected host through parasitology research
 *   institutions to the clinical understanding of toxoplasmosis. The cyst's
 *   active metabolism extracts resources from the host while simultaneously
 *   maintaining immune equilibrium necessary to prevent disseminated
 *   infection — creating a tangled hybrid of coordination and extraction. The
 *   institutional response reveals a second constraint: the 'latency'
 *   paradigm persists in textbooks and clinical language despite scRNA-seq
 *   evidence of activity, indicating a Piton-type degraded institutional
 *   model maintained through inertia.
 *
 * KEY AGENTS:
 *   - Chronically Infected Host (Billions): Primary victim (powerless/trapped) — subject to continuous metabolic extraction by active cysts; cannot eliminate infection without pharmaceutical intervention
 *   - Host Immune System: Secondary actor (moderate/constrained) — enforces containment while tolerating cyst presence; benefits from metabolic equilibrium but constrained by inability to fully eliminate
 *   - Parasite Transmission Niche: Structural beneficiary (institutional/arbitrage) — active cyst metabolism coordinates behavioral manipulation and transmission without explicit parasitic control; derives transmission efficiency from metabolic activity
 *   - Clinical Neuroscience Community (Organized): Mixed actor (organized/mobile) — benefits from new therapeutic targets but constrained by paradigm shift from 'latency' model; has mobile exit (adopt new model or ignore it)
 *   - Parasitology Research Community (Organized): Secondary beneficiary (institutional/arbitrage) — expands research scope with new discoveries; captures novelty premium from active-cyst hypothesis
 *   - The 'Latent Dormancy' Paradigm (Institutional): Piton structure (institutional/arbitrage) — maintained through textbook inertia, funding path-dependence, and citation lag despite contradicting evidence; theater ratio indicates performative maintenance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(toxoplasma_hub_2026, 0.38).
domain_priors:suppression_score(toxoplasma_hub_2026, 0.48).
domain_priors:theater_ratio(toxoplasma_hub_2026, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(toxoplasma_hub_2026, extractiveness, 0.38).
narrative_ontology:constraint_metric(toxoplasma_hub_2026, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(toxoplasma_hub_2026, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(toxoplasma_hub_2026, tangled_rope).
narrative_ontology:human_readable(toxoplasma_hub_2026, "The Toxoplasma Cyst as an Active Hub").
narrative_ontology:topic_domain(toxoplasma_hub_2026, "biological/medical/parasitology").

domain_priors:requires_active_enforcement(toxoplasma_hub_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(toxoplasma_hub_2026, cyst_metabolism_researchers).
narrative_ontology:constraint_beneficiary(toxoplasma_hub_2026, parasite_transmission_models).
narrative_ontology:constraint_victim(toxoplasma_hub_2026, latent_infection_interpretations).
narrative_ontology:constraint_victim(toxoplasma_hub_2026, host_immune_equilibrium_models).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CHRONICALLY INFECTED HOST (SNARE) — Cannot exit chronic infection without pharmaceutical intervention; subject to continuous metabolic extraction by active cyst metabolism. Single-cell RNA data reveals cysts are not passive dormant structures but metabolically active entities continuously consuming host resources. Host immune system cannot eliminate or fully suppress cysts. d≈0.92, f(d)≈1.39, σ=1.2 → χ≈0.65.
constraint_indexing:constraint_classification(toxoplasma_hub_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: HOST IMMUNE SYSTEM (TANGLED ROPE) — Constrained by inability to fully eliminate chronic infection; benefits from metabolic equilibrium with active cysts (parasite is contained rather than disseminated). The immune system experiences coordination: it must maintain activation sufficient to prevent dissemination while tolerating cyst presence. scRNA-seq reveals immune infiltration is continuous and specialized (CSF1R+ macrophages, IL-10 producing T cells). This is both enforced coexistence and mutual regulation. d≈0.68, f(d)≈1.05, σ=1.2 → χ≈0.48.
constraint_indexing:constraint_classification(toxoplasma_hub_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: PARASITE TRANSMISSION NICHE (ROPE) — Benefits from cyst metabolic activity as coordination mechanism for transmission. Active cyst metabolism drives neuroinflammation, behavioral changes (reduced aversion to predators), and predation by felids. The parasitic life cycle requires this active hub to complete transmission. Cyst activity coordinates behavioral manipulation without the parasite needing explicit motor control. d≈0.15, f(d)≈0.01, σ=1.0 → χ≈0.004.
constraint_indexing:constraint_classification(toxoplasma_hub_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: CLINICAL NEUROSCIENCE COMMUNITY (TANGLED ROPE) — Benefits from new understanding of cyst mechanisms (enables therapeutic targeting). Constrained by prior dogma that cysts are metabolically dormant (extraction from theoretical investment in 'latency' model). scRNA-seq forces reconceptualization of chronic CNS infection. Active cyst metabolism opens research pathways but invalidates decades of 'latent' infection literature. d≈0.52, f(d)≈0.68, σ=1.1 → χ≈0.31.
constraint_indexing:constraint_classification(toxoplasma_hub_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: 'LATENT DORMANCY' PARADIGM (PITON) — Institutional description of cyst state that persists through inertia despite scRNA-seq evidence of active metabolism. The term 'latent' is now functionally inaccurate but remains in textbooks, clinical guidelines, and funding language. Theater ratio high (0.61): continued use of 'dormancy' language in abstracts and reviews despite single-cell evidence of metabolic activity. The paradigm is degraded but maintained through publication lag and conceptual inertia. theater_ratio=0.61.
constraint_indexing:constraint_classification(toxoplasma_hub_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risks framing the active cyst hub as an immutable constraint of parasitology: 'latency is inherent to chronic infection' or 'metabolic suppression is a natural limit of dormancy.' However, the structural data (ε=0.38, suppression=0.48, theater=0.61) contradicts mountain classification. The active hub is a contingent evolutionary strategy, not a law of biology. This perspective represents the false summit risk — naturalizing what is actually an enforced institutional arrangement (the dormancy paradigm).
constraint_indexing:constraint_classification(toxoplasma_hub_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(toxoplasma_hub_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(toxoplasma_hub_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(toxoplasma_hub_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(toxoplasma_hub_2026, TR),
    TR >= 0.70.

:- end_tests(toxoplasma_hub_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The active cyst continuously consumes host metabolic resources (glucose, lipids, amino acids) at measurable rates. However, extraction is not severe (ε ≤ 0.46) because the parasite maintains metabolic homeostasis — chronic infection does not rapidly escalate into disseminated disease. The extraction is sustainable parasitism, not acute resource depletion. The measure reflects continuous resource drain balanced against parasite's own metabolic maintenance. Suppression (0.48): Moderate. The chronically infected host cannot eliminate cysts through immune response alone; suppression is structural. However, suppression is not total (≥0.60) because pharmaceutical (antiparasitic drugs) and surgical (lesion removal) interventions exist and are used clinically. Exit is blocked without external intervention but not absolutely blocked. Theater ratio (0.61): Moderate-high. Institutional use of 'latent' and 'dormant' language persists in medical literature, textbooks, and clinical guidelines despite scRNA-seq evidence of metabolic activity. This reflects lag between data generation (2016+) and institutional language shift. The term 'latency' now functions more as a conventional description than an accurate characterization of cyst state. Theater ratio increased over interval as evidence accumulated but language remained stable, indicating growing performative maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits sharp perspectival variation. The infected host experiences a Snare (extraction, no exit, trapped). The immune system experiences Tangled Rope (mixed coordination and enforcement). The parasite experiences Rope (coordination of transmission without explicit motor control). The research community experiences Tangled Rope (benefits and theoretical constraints). The dormancy paradigm is revealed as Piton (degraded institutional model maintained theatrically). The analytical observer risks Mountain framing ('latency is inherent to chronic infection') but the structural data reveals this as false — the active hub is a contingent evolutionary strategy visible only through modern techniques. The perspectival gap is unusually sharp because the scRNA-seq discovery fundamentally altered the empirical basis for classification, yet institutional language has not caught up.
 *
 * DIRECTIONALITY LOGIC:
 *   Chronically infected host: Victim + trapped → d≈0.92, f(d)≈1.39. Near-maximum extraction; no voluntary exit. Host immune system: Mixed (victim of suppression constraint, beneficiary of equilibrium maintenance) + constrained → d≈0.68, f(d)≈1.05. Moderate-high extraction. Parasite transmission niche: Beneficiary + arbitrage → d≈0.15, f(d)≈0.01. Net beneficiary; low extraction (actually subsidized by cyst metabolic coordination). Clinical neuroscience community: Victim of paradigm shift (extraction from prior theoretical investment) + mobile → d≈0.52, f(d)≈0.68. Moderate extraction. Dormancy paradigm: Institutional + arbitrage → d≈0.05 (if treated as direct actor); Piton classification derives from theater_ratio gate, not high chi. Analytical observer: analytical → d≈0.72, f(d)≈1.15. False summit risk.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy through perspectival differentiation and data rebaselining. The scRNA-seq discovery changed the empirical foundation: extractiveness increased from prior estimates (~0.15 in pre-scRNA paradigm) to 0.38 because metabolic measurements replaced assumptions. The 'latent' language now extracts value from the research community (who must cite and explain the paradigm shift) while providing no functional coordination benefit — this is the mandatrophy signature. The resolution requires accepting that the scRNA-seq data makes the classification: this is not a Snare masquerading as Rope (which would be unresolvable), but rather a genuine Tangled Rope where the active cyst simultaneously extracts and coordinates. The paradigm lag (theater_ratio=0.61) indicates institutional maintenance of outdated language, not fundamental ambiguity about the constraint's type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cyst_metabolic_function,
    'What is the specific metabolic function of active cyst metabolism relative to parasite survival vs. transmission timing?',
    'Metabolomic profiling of cysts; knockout of specific metabolic pathways; correlations between metabolic activity levels and transmission success in experimental infections',
    'If primary function is transmission timing: cyst activity is coordination mechanism (Rope classification strengthened). If primary function is escape from immune attack: cyst activity is extraction mechanism (Snare classification strengthened).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cyst_metabolic_function, empirical, 'Functional role of active cyst metabolism in parasite lifecycle').

omega_variable(
    immune_tolerance_mechanism,
    'Does host immune tolerance of cysts require continuous sensing of cyst metabolic state, or does tolerance rely on permanent structural isolation?',
    'Single-cell immune profiling of cyst microenvironment; perturbation of immune infiltration; measurement of immune activation distance from cyst wall',
    'If continuous sensing required: host-cyst relationship is enforced coexistence (Tangled Rope). If isolation sufficient: relationship is pure equilibrium (closer to Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(immune_tolerance_mechanism, empirical, 'Mechanism of host immune tolerance of active cysts').

omega_variable(
    paradigm_replacement_timeline,
    'How quickly will institutional language shift from ''dormant'' to ''metabolically active'' in clinical and research communities?',
    'Longitudinal text analysis of abstracts, textbook chapters, clinical guidelines; surveys of clinician understanding; measurement of citation lag between scRNA-seq discovery and mainstream acceptance',
    'If replacement < 5 years: piton classification temporary, paradigm genuinely shifts. If replacement > 15 years: piton is stable institutional constraint; ''latency'' becomes purely theatrical term.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paradigm_replacement_timeline, empirical, 'Timeline for replacement of dormancy paradigm with metabolic activity model').

omega_variable(
    behavioral_manipulation_mechanism,
    'Is the behavioral phenotype (reduced predator aversion, increased activity) directly driven by cyst metabolic products, by cyst-induced neuroinflammation, or by parasite-controlled synaptic modification?',
    'Spatial transcriptomics of behavioral brain regions; measurement of metabolic product diffusion from cysts; perturbation of specific inflammatory cytokines; mapping of cyst-to-neuron signaling',
    'If metabolic products: cyst is active hub (Rope strengthened). If neuroinflammation mediated: cyst is indirect extraction mechanism (Tangled Rope). If direct synaptic control: cyst is sophisticated coordination mechanism (Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_manipulation_mechanism, empirical, 'Mechanism linking cyst metabolic activity to behavioral phenotype').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(toxoplasma_hub_2026, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tox_tr_t0, toxoplasma_hub_2026, theater_ratio, 0, 0.38).
narrative_ontology:measurement(tox_tr_t8, toxoplasma_hub_2026, theater_ratio, 8, 0.52).
narrative_ontology:measurement(tox_tr_t16, toxoplasma_hub_2026, theater_ratio, 16, 0.61).

% Extraction over time
narrative_ontology:measurement(tox_be_t0, toxoplasma_hub_2026, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(tox_be_t8, toxoplasma_hub_2026, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(tox_be_t16, toxoplasma_hub_2026, base_extractiveness, 16, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(toxoplasma_hub_2026, resource_allocation).
narrative_ontology:affects_constraint(toxoplasma_hub_2026, chronic_cnc_inflammation).
narrative_ontology:affects_constraint(toxoplasma_hub_2026, behavioral_parasite_manipulation).
narrative_ontology:affects_constraint(toxoplasma_hub_2026, latent_infection_transmission_risk).

% DUAL FORMULATION NOTE:
% The active cyst hub constraint is upstream of behavioral manipulation and neuroinflammation constraints. The scRNA-seq discovery revealed that 'latency' was two distinct constraints: (1) metabolic activity (active hub, ε=0.38) and (2) immune tolerance (enforced equilibrium, ε≈0.25). These were collapsed in prior literature under the label 'dormancy.' The network links capture the downstream effects of recognizing metabolic activity on transmission models and clinical understanding.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(toxoplasma_hub_2026, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
