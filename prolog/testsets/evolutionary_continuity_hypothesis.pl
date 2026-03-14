% ============================================================================
% CONSTRAINT STORY: evolutionary_continuity_hypothesis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_evolutionary_continuity_hypothesis, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: evolutionary_continuity_hypothesis
 *   human_readable: Evolutionary Continuity Hypothesis
 *   domain: evolutionary_biology/philosophy_of_science
 *
 * SUMMARY:
 *   The Evolutionary Continuity Hypothesis asserts that evolutionary change
 *   proceeds by infinitesimal steps without significant jumps or
 *   discontinuities—natura non facit saltus. This constraint represents a
 *   foundational institutional consensus in evolutionary biology, yet
 *   exhibits internal tension between its coordinating function (providing a
 *   unifying narrative for curricula and funding) and its extractive function
 *   (suppressing alternative explanatory frameworks that accommodate
 *   punctuated patterns in the fossil record). The constraint demonstrates
 *   classic Tangled Rope structure: genuine coordination problem (how to
 *   teach evolution as a coherent process) combined with asymmetric
 *   extraction (institutional power favoring gradualist interpretations).
 *   Theater increases over the 150-year interval as the hypothesis becomes
 *   more culturally entrenched and less empirically tested — it functions
 *   increasingly as heritage narrative rather than active research principle.
 *
 * KEY AGENTS:
 *   - Discontinuist Paleontologists: Primary victims (powerless/trapped) — observe fossil record patterns suggesting rapid transitions; career-trapped in a field where gradualist interpretation is institutional default
 *   - Transitional Forms Researchers: Secondary victims (moderate/constrained) — study morphological intermediates; forced to interpret ambiguous evidence within continuity framework despite alternative patterns being evident
 *   - Institutional Evolutionary Biology: Primary beneficiary (institutional/arbitrage) — universities, funding bodies, textbook publishers benefit from continuity as unifying narrative; maintains institutional coherence
 *   - Pluralist Theoretical Community: Organized reformers (organized/constrained) — philosophers and methodologists building multi-scale causation alternatives; see continuity as temporary consensus with sunset
 *   - Darwin's Legacy Narrative: Institutional actor (institutional/arbitrage) — cultural momentum of Darwinian gradualism; maintained as celebratory frame independent of empirical adequacy
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as universal principles of nature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(evolutionary_continuity_hypothesis, 0.38).
domain_priors:suppression_score(evolutionary_continuity_hypothesis, 0.42).
domain_priors:theater_ratio(evolutionary_continuity_hypothesis, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(evolutionary_continuity_hypothesis, extractiveness, 0.38).
narrative_ontology:constraint_metric(evolutionary_continuity_hypothesis, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(evolutionary_continuity_hypothesis, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(evolutionary_continuity_hypothesis, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(evolutionary_continuity_hypothesis, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(evolutionary_continuity_hypothesis, tangled_rope).
narrative_ontology:human_readable(evolutionary_continuity_hypothesis, "Evolutionary Continuity Hypothesis").
narrative_ontology:topic_domain(evolutionary_continuity_hypothesis, "evolutionary_biology/philosophy_of_science").

domain_priors:requires_active_enforcement(evolutionary_continuity_hypothesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(evolutionary_continuity_hypothesis, gradualist_research_programs).
narrative_ontology:constraint_beneficiary(evolutionary_continuity_hypothesis, institutional_evolutionary_biology).
narrative_ontology:constraint_victim(evolutionary_continuity_hypothesis, discontinuist_research_perspectives).
narrative_ontology:constraint_victim(evolutionary_continuity_hypothesis, field_explanatory_diversity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISCONTINUIST PALEONTOLOGISTS (SNARE) — Career-trapped researchers who observe fossil record patterns suggesting rapid transitions and punctuated change. Cannot exit the field without abandoning professional identity. Suppressed from publishing in top venues; their work is reinterpreted through gradualist frameworks or marked as controversial. Maximum extraction: data is mined to support gradualist narrative, researchers bear reputational cost.
constraint_indexing:constraint_classification(evolutionary_continuity_hypothesis, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TRANSITIONAL FORMS RESEARCHERS (TANGLED ROPE) — Study intermediate morphologies; experience both coordination benefits (the continuity hypothesis legitimizes their research domain) and extraction (forced to interpret ambiguous specimens as supporting gradualism even when discontinuities are evident). Constrained by publication bias and funding allocation favoring smoothness narratives.
constraint_indexing:constraint_classification(evolutionary_continuity_hypothesis, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL EVOLUTIONARY BIOLOGY (ROPE) — Universities, funding bodies, textbook authors benefit from continuity hypothesis as coordinating principle. Solves the institutional problem: a single narrative makes curriculum coherent and funding allocations defensible. Arbitrage: institutions can shift emphasis between gradualism and punctuationism while maintaining the over-arching continuity frame. Net beneficiary.
constraint_indexing:constraint_classification(evolutionary_continuity_hypothesis, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PLURALIST THEORETICAL COMMUNITY (SCAFFOLD) — Organized philosophers and methodologists recognize continuity as a temporary consensus with explicit sunset: multi-scale causation frameworks, hierarchical selection theory, and niche construction models are creating alternative explanatory pathways. Sunset: 15-25 years as these frameworks mature and integrate into curricula.
constraint_indexing:constraint_classification(evolutionary_continuity_hypothesis, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: DARWIN'S LEGACY NARRATIVE (PITON) — The continuity hypothesis persists through historical inertia and cultural momentum. 'Nature does not make jumps' (Natura non facit saltus) is maintained as celebratory narrative about Darwin despite mounting empirical complications. Primarily performative: the narrative function (honoring Darwin's methodological preference) has decoupled from functional explanatory power. Theater ratio: 0.65 reflects the performative maintenance of the heritage claim.
constraint_indexing:constraint_classification(evolutionary_continuity_hypothesis, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the continuity hypothesis appears as a fundamental principle of nature: evolution must proceed by infinitesimal steps; large changes cannot occur suddenly. This perspective risks treating institutional consensus as natural law. The engine's false summit detector will flag this — the structural data reveals contingent institutional enforcement, not an immutable limit.
constraint_indexing:constraint_classification(evolutionary_continuity_hypothesis, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(evolutionary_continuity_hypothesis_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(evolutionary_continuity_hypothesis, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(evolutionary_continuity_hypothesis, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(evolutionary_continuity_hypothesis, TR),
    TR >= 0.70.

:- end_tests(evolutionary_continuity_hypothesis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint imposes real costs on discontinuist researchers through publication bias and career penalties, but does not prevent all alternative work — some punctuationist research does get published and cited. The extraction is substantial but not total suppression. Suppression (0.42): Moderate. Barriers include publication bias, textbook dominance of gradualist framing, funding allocation favoring continuity-supporting projects, and cultural prestige of Darwin's gradualist legacy. But suppression is incomplete — discontinuist work appears in specialized venues and has influenced theoretical development (Eldredge & Gould's punctuated equilibrium remains institutionally visible). Theater ratio (0.65): Rising over time. The hypothesis began as a productive research principle but increasingly functions as heritage narrative. Darwin's preference for gradual change is maintained as celebratory claim about scientific method even as empirical justification has weakened. The rising theater (0.42 → 0.65) reflects this decoupling: the constraint persists through cultural inertia rather than evidence-driven reasoning.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between institutional beneficiaries who experience coordination (Rope) and career-trapped researchers who experience extraction (Snare). From the beneficiary perspective, continuity is an elegant unifying principle. From the victim perspective, it is an institutional constraint that erases inconvenient data. The scaffold perspective (pluralist theorists) sees both — genuine coordination function combined with transitional nature. The piton perspective reveals degradation: the heritage narrative maintains the constraint even as empirical justification has weakened. The false-summit mountain perspective shows the naturalizing trap: treating institutional consensus as universal principle.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim status and exit options. Discontinuist paleontologists are victims with no exit (trapped) — they have invested in professional identity in the field and cannot leave without abandoning career. This yields high d (~0.90) mapping to high f(d), producing high χ from their perspective. Institutional evolutionary biology are beneficiaries with arbitrage (can shift emphasis between frameworks while maintaining institutional narrative) — low d (~0.15) producing negative χ (they experience extraction flowing toward them, not away). Transitional forms researchers are intermediate: some constraint-driven extraction (they must force data into continuity frame) but also benefit from the coordinating function (their domain is legitimate because continuity requires transitional forms). Constrained exit and mixed beneficiary/victim status yields mid-range d (~0.55). The directionality pipeline computes these automatically from the beneficiary/victim declarations and exit options; high-d victims experience stronger extraction than beneficiaries with mobile exits.
 *
 * MANDATROPHY ANALYSIS:
 *   EXEMPLAR OF MIXED COORDINATION AND EXTRACTION: The continuity hypothesis resolves the mandatrophy by showing genuine coordination function (unifying narrative for curricula, funding, institutional coherence) combined with real asymmetric extraction (suppressing alternative interpretations, career penalties for discontinuists). This is the textbook Tangled Rope case: both elements are structural, not perspectival artifacts. The beneficiaries genuinely coordinate through this constraint — it solves institutional problems. The victims genuinely bear extraction — their research is suppressed. The mandatrophy is resolved by recognizing that the constraint serves BOTH functions simultaneously. Classification as Tangled Rope (not pure Rope, not pure Snare) reflects this duality. The rising theater ratio (0.42 → 0.65) indicates potential degradation: if the coordination function weakens while extraction persists, the constraint would reclassify toward Snare. Conversely, if alternative frameworks mature and institutional actors retain multiple options, the constraint would reclassify toward Scaffold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fossil_record_data_interpretation,
    'Are observed gaps in the fossil record genuine evolutionary discontinuities or artifacts of preservation and sampling bias?',
    'Quantitative paleontological modeling: simulation of preservation patterns under continuous vs punctuated evolution; comparison of predicted vs observed gap distributions across multiple depositional environments and taxa',
    'If discontinuities are artifacts: continuity hypothesis is strengthened (Mountain). If genuine signal persists after bias correction: Snare classification confirmed — data is being suppressed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fossil_record_data_interpretation, empirical, 'Fossil record gaps as preservation artifacts vs evolutionary signal').

omega_variable(
    morphological_intermediacy_definition,
    'What constitutes a ''transitional form'' — geometric proximity in morphospace or functional continuity of adaptation?',
    'Phylogenetic comparative analysis: map morphological trajectories against adaptive landscapes; assess whether observed intermediates are functionally continuous or represent distinct adaptive regimes connected by rapid transition',
    'If intermediacy means proximity in morphospace: continuity hypothesis is self-fulfilling (Piton — narrative maintenance). If it requires functional continuity: the hypothesis requires active enforcement (Tangled Rope confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(morphological_intermediacy_definition, conceptual, 'Definition of transitional form: geometric vs functional continuity').

omega_variable(
    publication_bias_quantification,
    'Do journals and funding bodies systematically over-represent gradualist interpretations of paleontological data?',
    'Bibliometric analysis: comparison of acceptance rates, citation patterns, and editorial decision timelines for continuity-supporting vs discontinuity-supporting manuscripts; survey of reviewer comments and editorial feedback',
    'If bias is strong: suppression is institutional (Snare confirmed). If bias is weak: discontinuist researchers face only normal scientific skepticism (reclassifies toward Rope or Scaffold).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(publication_bias_quantification, empirical, 'Publication bias favoring gradualist over punctuationist interpretations').

omega_variable(
    molecular_clock_calibration_uncertainty,
    'Do molecular clock estimates of divergence times constrain the permissible rates of morphological change, or do they share sufficient calibration uncertainty to accommodate punctuated patterns?',
    'Bayesian analysis of clock uncertainty; sensitivity testing of divergence time estimates to calibration prior choices; cross-validation against paleontological branch duration estimates',
    'If molecular clocks tightly constrain rates: continuity is empirically necessary (Mountain). If calibration uncertainty is large: rates compatible with punctuation are defensible (Tangled Rope / Scaffold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(molecular_clock_calibration_uncertainty, empirical, 'Molecular clock calibration uncertainty and rate constraints').

omega_variable(
    adaptive_landscape_topology,
    'Is the phenotypic adaptive landscape (Wright''s surface) continuous and smooth, or does it contain sharp ridges, valleys, and discontinuous plateaus that permit rapid transitions?',
    'Agent-based modeling: simulate evolution across empirically-derived fitness landscapes; compare trajectory statistics (mean transition time, frequency of rapid jumps) to observed paleontological patterns',
    'If landscape is smooth: continuity is necessary (Mountain). If landscape is fragmented: rapid transitions are evolutionarily plausible (Snare suppresses this possibility).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(adaptive_landscape_topology, empirical, 'Adaptive landscape topology and plausibility of rapid transitions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(evolutionary_continuity_hypothesis, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ech_tr_t0, evolutionary_continuity_hypothesis, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ech_tr_t50, evolutionary_continuity_hypothesis, theater_ratio, 50, 0.58).
narrative_ontology:measurement(ech_tr_t100, evolutionary_continuity_hypothesis, theater_ratio, 100, 0.65).

% Extraction over time
narrative_ontology:measurement(ech_be_t0, evolutionary_continuity_hypothesis, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ech_be_t50, evolutionary_continuity_hypothesis, base_extractiveness, 50, 0.34).
narrative_ontology:measurement(ech_be_t100, evolutionary_continuity_hypothesis, base_extractiveness, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(evolutionary_continuity_hypothesis, information_standard).
narrative_ontology:boltzmann_floor_override(evolutionary_continuity_hypothesis, 0.08).
narrative_ontology:affects_constraint(evolutionary_continuity_hypothesis, fossil_record_interpretation_framework).
narrative_ontology:affects_constraint(evolutionary_continuity_hypothesis, morphological_change_rate_constraints).
narrative_ontology:affects_constraint(evolutionary_continuity_hypothesis, paleontological_evidence_weighting).

% DUAL FORMULATION NOTE:
% The continuity hypothesis is upstream of specific paleontological claims (fossil record interpretation, morphological rate constraints). It establishes the institutional framing within which alternative claims are evaluated. Downstream constraints inherit the suppression function: a discontinuist interpretation of a specific fossil sequence faces extraction both from the hypothesis itself and from the normative framework it establishes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(evolutionary_continuity_hypothesis, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
