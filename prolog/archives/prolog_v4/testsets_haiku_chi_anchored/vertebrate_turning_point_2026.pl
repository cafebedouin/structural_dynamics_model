% ============================================================================
% CONSTRAINT STORY: vertebrate_turning_point_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vertebrate_turning_point_2026, []).

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
 *   constraint_id: vertebrate_turning_point_2026
 *   human_readable: The Genetic Turning Point for Vertebrate Evolution (Scientific Model)
 *   domain: biological/scientific
 *
 * SUMMARY:
 *   The 'Genetic Turning Point' for vertebrate evolution encodes the
 *   scientific consensus that whole-genome duplication events (approximately
 *   2 rounds in early vertebrate lineages) provided the raw genetic material
 *   enabling the morphological complexity of backbones, nervous systems, and
 *   chambered hearts. This narrative has dominated evolutionary developmental
 *   biology textbooks and funding priorities for 20+ years, but increasingly
 *   faces empirical pressure: (1) alternative lineages (tunicates, hagfish)
 *   achieve comparable complexity without documented whole-genome
 *   duplications; (2) cis-regulatory network reorganization and polyploidy
 *   without tandem duplication can generate morphological complexity; (3)
 *   some Hox cluster expansions predate the proposed duplication events; (4)
 *   experimental developmental biology shows that morphological innovations
 *   (neural crest, segmentation) can be achieved through regulatory rewiring
 *   without additional gene copies. The constraint exhibits the full DR
 *   spectrum: from the consensus research program's perspective (Rope), it
 *   solves a real coordination problem by unifying morphological and
 *   molecular timescales. From the perspective of suppressed alternative
 *   mechanisms (Snare), it is pure extraction — alternatives are excluded
 *   from curricula and peer review. From the analytical observer's
 *   perspective (false Mountain), it naturalizes an institutional model as a
 *   law of nature. From the comparative genomics coalition's perspective
 *   (Scaffold), it is a temporary gate-keeping mechanism with a sunset
 *   clause: as sequencing and computational capacity distribute downward, the
 *   consensus-model's institutional monopoly on 'correct' interpretation
 *   becomes irrelevant.
 *
 * KEY AGENTS:
 *   - Consensus Evolutionary Narrative: Primary beneficiary (institutional/arbitrage) — unified framework provides textbook authority, funding concentration, and citation advantage for conforming researchers
 *   - Developmental Biology Research Program: Primary beneficiary (institutional/arbitrage) — the duplication model creates clear research questions and experimental protocols; provides scaffolding for generations of researchers
 *   - Alternative Evolutionary Mechanisms: Primary victim (powerless/trapped) — cis-regulatory evolution, polyploidy-without-duplication, network reorganization are systematically excluded from curricula and high-prestige journals; cannot exit the citation penalty
 *   - Experimental Replication Community: Secondary victim (moderate/constrained) — face resource barriers and paradigm pressure; benefit from shared sequence databases and collaborative framework structured around consensus model
 *   - Comparative Genomics Coalition: Organized agents (organized/mobile) — arXiv preprints, open sequence repositories, citizen science phylogenetics building alternative verification infrastructure; possess genuine exit options
 *   - Textbook Authority System: Institutional actor (institutional/arbitrage) — maintains narrative through pedagogical inertia; serves real coordination function (teaching morphological evolution) while mechanistic claim degrades
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the institutional consensus model as a universal law of biology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vertebrate_turning_point_2026, 0.38).
domain_priors:suppression_score(vertebrate_turning_point_2026, 0.48).
domain_priors:theater_ratio(vertebrate_turning_point_2026, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vertebrate_turning_point_2026, extractiveness, 0.38).
narrative_ontology:constraint_metric(vertebrate_turning_point_2026, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(vertebrate_turning_point_2026, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vertebrate_turning_point_2026, tangled_rope).
narrative_ontology:human_readable(vertebrate_turning_point_2026, "The Genetic Turning Point for Vertebrate Evolution (Scientific Model)").
narrative_ontology:topic_domain(vertebrate_turning_point_2026, "biological/scientific").

domain_priors:requires_active_enforcement(vertebrate_turning_point_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vertebrate_turning_point_2026, consensus_evolutionary_narrative).
narrative_ontology:constraint_beneficiary(vertebrate_turning_point_2026, developmental_biology_research_program).
narrative_ontology:constraint_victim(vertebrate_turning_point_2026, alternative_evolutionary_mechanisms).
narrative_ontology:constraint_victim(vertebrate_turning_point_2026, experimental_replicability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALTERNATIVE MECHANISMS (SNARE) — Competing explanations for vertebrate evolution (regulatory network expansion without whole-genome duplication, polyploidy without tandem duplication, cis-regulatory evolution) cannot exit the citation penalty and are systematically excluded from graduate curriculum. No independent verification pathway; bear full extraction cost. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.65.
constraint_indexing:constraint_classification(vertebrate_turning_point_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EXPERIMENTAL REPLICATION COMMUNITY (TANGLED ROPE) — Constrained by the paradigm dominance and funding bias toward whole-genome duplication models, but also benefits from the shared framework and collaborative access to sequence databases structured around the consensus model. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.44.
constraint_indexing:constraint_classification(vertebrate_turning_point_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSENSUS RESEARCH PROGRAM (ROPE) — Benefits from unified narrative architecture, institutional funding concentration, and textbook authority. The whole-genome duplication model solves legitimate coordination problems: it provides a single explanatory framework linking Hox cluster expansion to morphological complexity. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.04. Net beneficiary through coordination function.
constraint_indexing:constraint_classification(vertebrate_turning_point_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMPARATIVE GENOMICS COALITION (SCAFFOLD) — Organized agents (arXiv genomics preprints, lab-neutral sequence annotation repositories, citizen science phylogenetics) are building alternative verification infrastructure that bypasses the consensus-model gate-keeping. These pathways have genuine sunset logic: as sequencing costs collapse and machine learning annotation improves, distributed reanalysis makes the institutional consensus model's gate-keeping mechanism irrelevant. d≈0.38, f(d)≈0.38, σ=1.2 → χ≈0.18.
constraint_indexing:constraint_classification(vertebrate_turning_point_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: TEXTBOOK AUTHORITY SYSTEM (PITON) — The consensus narrative persists in textbooks and graduate curricula primarily through institutional inertia, not because the empirical case for a singular 'turning point' remains robust under modern scrutiny. theater_ratio=0.65 reflects substantial performative content: the pedagogical narrative ('then, one day, a duplication event happened') maintains its function (teaching morphological evolution) while the mechanistic claim has degraded. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.03. Piton is maintained by narrative authority, not extraction force.
constraint_indexing:constraint_classification(vertebrate_turning_point_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, some vertebrate morphological transitions require increased genetic complexity — this is a logical/structural consequence of developmental elaboration. The specific mechanism (whole-genome duplication vs other sources of complexity) is contingent, but the requirement for increased regulatory capacity is not. However, structural data (ε=0.38, suppression=0.48, theater=0.65) contradicts mountain classification — the engine detects this as a false summit, revealing that 'vertebrate morphology requires genetic complexity' naturalizes the consensus model's specific mechanistic claim.
constraint_indexing:constraint_classification(vertebrate_turning_point_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vertebrate_turning_point_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(vertebrate_turning_point_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(vertebrate_turning_point_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(vertebrate_turning_point_2026, TR),
    TR >= 0.70.

:- end_tests(vertebrate_turning_point_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The consensus model captures genuine empirical reach — whole-genome duplication did occur in early vertebrate lineages and may have enabled some morphological innovations. However, the claim that there exists A singular 'turning point' is increasingly contested, and the extraction comes from suppressing mechanistic alternatives that may have contributed to different features in different lineages. The reduced extractiveness (vs earlier estimates of 0.55) reflects that modern genomics is eroding the model's exclusive explanatory authority. Suppression (0.48): Moderate. Barriers to alternative hypothesis visibility include: publication bias in top journals toward consensus-confirming studies, graduate curricula structured around the duplication model, funding agencies' preference for well-established frameworks, and the high computational barriers to independent sequence reanalysis. But suppression is not absolute — alternative studies are published in second-tier journals and preprint servers; some major labs (e.g., Markov group, Borenstein lab) are actively exploring regulatory alternatives. Theater ratio (0.65): Moderate-high. The pedagogical narrative ('then, one day, a duplication event happened, and vertebrates arose') maintains substantial performative content — it teaches students a memorable linear causality, but the actual empirical basis has become more diffuse (duplication is one factor among several; timing is debated; necessity is unproven). As new genomic data accumulate, the narrative's function (organizing student understanding) persists while its explanatory power (accounting for observed complexity) decreases.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits pronounced perspectival disagreement on whether the consensus model is coordination or extraction. The beneficiary research program sees it as Rope — a shared framework enabling collective work on vertebrate evolution. Suppressed alternatives see it as Snare — pure exclusion. The analytical observer may fall into the trap of seeing it as a Mountain — 'vertebrate complexity requires genetic innovation' — which the engine detects as a false summit. The experimental replication community sees Tangled Rope — they use the consensus framework for resources and collaboration but face constraints in exploring alternatives. The comparative genomics coalition sees Scaffold — institutional gate-keeping with a sunset clause, as sequencing and computational capacity democratize reanalysis. The textbook system sees Piton — the narrative persists through inertia, serving pedagogical function but with degraded mechanistic accuracy.
 *
 * DIRECTIONALITY LOGIC:
 *   Consensus research program: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary through coordination of research infrastructure. Alternative mechanisms: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction — no alternative verification pathway, systematic exclusion from high-prestige venues. Experimental replication community: Victim + constrained → d≈0.72, f(d)≈1.15. Significant extraction through paradigm pressure, but benefits from shared infrastructure. Comparative genomics coalition: Organized + mobile → d≈0.35, f(d)≈0.35. Low effective extraction; coalition has agency and distributed verification capacity. Textbook system: Institutional + arbitrage → d≈0.08, f(d)≈-0.10. Piton classification comes from theater gate (0.65 ≥ 0.70 threshold approached), not from extraction force. Analytical observer: analytical → d≈0.72, f(d)≈1.15. False mountain detection: naturalizing the consensus model masks its institutional contingency.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is NOT RESOLVED because the constraint exhibits genuine ambiguity between two structurally distinct mechanisms: (1) COORDINATION HYPOTHESIS: The whole-genome duplication model solves a real scientific problem — integrating morphological complexity with molecular timescales — and provides shared explanatory framework. Under this reading, the constraint is Rope or Tangled Rope with legitimate asymmetry (beneficiaries get first-mover advantage; victims bear costs of exploring alternatives). (2) EXTRACTION HYPOTHESIS: The institutional consensus model actively suppresses alternative mechanisms that may have empirical merit, extracting from researchers who pursue them through citation penalties and funding scarcity. Under this reading, the constraint is Snare with performative justification. The unresolved mandatrophy reflects genuine uncertainty about whether the consensus model is a scientific result or an institutional commitment. This is not a defect — it is the correct diagnosis: constraints at ε=0.38, suppression=0.48 with high theater_ratio naturally exhibit this ambiguity. Resolution requires empirical work: (a) independent tests of whether alternative mechanisms can generate observed complexity; (b) bibliometric analysis of citation bias; (c) replication of duplication timing across lineages with modern genomic methods. The constraint remains active (MANDATROPHY UNRESOLVED) pending this evidence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    singularity_vs_plurality_mechanism,
    'Was there a singular ''genetic turning point'' for vertebrate evolution, or did multiple independent mechanisms (polyploidy, cis-regulatory expansion, network reorganization) contribute to different lineages?',
    'Phylogenetic reconstruction of duplication events across jawless fish lineages; comparison of timing and scope of duplication events in tunicates vs early vertebrates; identification of functional constraints that require whole-genome duplication vs alternatives',
    'If singular: tangled_rope classification confirmed — the consensus model is a genuine coordinating framework with real extraction asymmetry. If plural: snare classification dominates — the singular-event narrative actively suppresses legitimate alternative explanations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(singularity_vs_plurality_mechanism, empirical, 'Whether vertebrate evolution had one genetic turning point or multiple independent mechanisms').

omega_variable(
    functional_necessity_of_duplication,
    'Do the specific morphological innovations of vertebrates (neural crest cells, chambered heart, paired fins) require whole-genome duplication, or could they emerge from more parsimonious sources of regulatory complexity?',
    'Experimental developmental biology: ectopic expression of paralogous Hox genes in invertebrate models to test whether duplication was necessary or sufficient; identification of non-duplicated regulatory networks that achieve similar morphological outcomes; analysis of whether Amphioxus achieves comparable complexity without whole-genome duplication events',
    'If duplication is necessary: constraint is closer to rope (genuine coordination mechanism). If duplication is sufficient but not necessary: constraint is closer to snare (institutional preference for a particular explanation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_necessity_of_duplication, empirical, 'Whether whole-genome duplication is functionally necessary for vertebrate morphology').

omega_variable(
    paleontological_timing_resolution,
    'Can fossil evidence more precisely constrain the timing and tempo of the genetic turning point relative to the fossil record of morphological change?',
    'High-resolution stratigraphic analysis of early vertebrate fossils; developmental analysis of fossil material to test morphological predictions of the whole-genome duplication model; reanalysis of molecular clock estimates with updated genome sequences',
    'If fossil timing is independent of genetic duplication timing: the genetic turning point model may be explaining a different phenomenon than the observed morphological radiation. If tightly coupled: the model has stronger explanatory reach.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(paleontological_timing_resolution, empirical, 'Whether fossil evidence independently constrains genetic turning point timing').

omega_variable(
    institutional_consensus_measurement,
    'How much of the consensus model''s dominance reflects genuine empirical support vs institutional/pedagogical inertia?',
    'Bibliometric analysis of citation patterns; survey of researchers on likelihood of alternative models given new genomic data; comparison of funding allocation to consensus vs alternative hypotheses; measurement of time-to-publication bias for dissenting studies',
    'If institutional inertia dominates: constraint is piton (degraded) or snare (extractive). If empirical support dominates: constraint is rope (genuine coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_consensus_measurement, empirical, 'The balance between empirical support and institutional dominance for the consensus model').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vertebrate_turning_point_2026, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vtp_tr_t0, vertebrate_turning_point_2026, theater_ratio, 0, 0.42).
narrative_ontology:measurement(vtp_tr_t15, vertebrate_turning_point_2026, theater_ratio, 15, 0.55).
narrative_ontology:measurement(vtp_tr_t30, vertebrate_turning_point_2026, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(vtp_be_t0, vertebrate_turning_point_2026, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(vtp_be_t15, vertebrate_turning_point_2026, base_extractiveness, 15, 0.3).
narrative_ontology:measurement(vtp_be_t30, vertebrate_turning_point_2026, base_extractiveness, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vertebrate_turning_point_2026, information_standard).
narrative_ontology:affects_constraint(vertebrate_turning_point_2026, hox_cluster_expansion_mechanism).
narrative_ontology:affects_constraint(vertebrate_turning_point_2026, neural_crest_cell_origin).
narrative_ontology:affects_constraint(vertebrate_turning_point_2026, early_vertebrate_body_plan_constraint).

% DUAL FORMULATION NOTE:
% The Genetic Turning Point represents a macroevolutionary coordination mechanism (unifying genomic and morphological timescales) AND an institutional gate-keeping mechanism (suppressing alternative mechanisms). These are structurally distinct constraints: one at ε≈0.10 (genuine coordination), one at ε≈0.55 (extraction). Current story (ε=0.38) represents their entanglement. Downstream constraints (Hox expansion, neural crest origin) depend on the macroevolutionary mechanism but not on the gate-keeping; decomposition may be warranted as genomic evidence accumulates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vertebrate_turning_point_2026, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
