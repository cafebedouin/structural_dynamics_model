% ============================================================================
% CONSTRAINT STORY: whole_genome_duplication_verification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_whole_genome_duplication_verification, []).

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
 *   constraint_id: whole_genome_duplication_verification
 *   human_readable: Whole Genome Duplication Verification Bottleneck
 *   domain: evolutionary_biology/genomics/experimental_verification
 *
 * SUMMARY:
 *   Whole genome duplication (WGD) detection in plants and animals involves
 *   phylogenomic analysis of synteny blocks, paralog Ks distributions, and
 *   reconciliation between gene trees and species trees. The structural
 *   constraint emerges from: (1) massive computational costs to verify
 *   claimed duplications (~100-1000 CPU-hours per organism genome), (2)
 *   methodological disagreement across independent detection pipelines, (3)
 *   publication bias against contradicting earlier claims, and (4)
 *   career/funding incentives that reward novel duplication dating over
 *   incremental verification. This generates a bottleneck where early WGD
 *   claims establish interpretive frameworks before independent verification
 *   can proceed. The constraint exhibits extractive dynamics (original
 *   claimants capture priority) combined with genuine coordination (the field
 *   needs shared reference predictions). Theater ratio shows degradation over
 *   the measurement interval: peer reviewers increasingly acknowledge
 *   inability to verify computational claims, yet certification rituals
 *   persist unchanged. The open genomics movement (1KP, GIGA, EBGP)
 *   represents a structural challenge to the bottleneck through standardized
 *   annotation pipelines and preprint-based hypothesis sharing, establishing
 *   a sunset mechanism.
 *
 * KEY AGENTS:
 *   - Original WGD Claimant Consortium: Primary beneficiary (institutional/arbitrage) — secures priority, interpretive authority, and continued funding for duplication-focused research. Benefits from being first to propose duplication hypothesis.
 *   - Independent Verification Groups: Primary victim (moderate/constrained) — face teraflop-scale computational barriers, specialized expertise requirements, publication bias against negative findings, and delayed access to high-coverage genomes. Structurally mobile (funded, employed) but constrained by resource asymmetry.
 *   - Field's Evolutionary Hypothesis Reliability: Powerless victim (powerless/trapped) — abstract collective good that cannot exit or organize; premature WGD claims corrupt phylogenetic inference infrastructure. Bears extraction with no advocacy mechanism.
 *   - Open Genomics Coalition: Organized agent (organized/constrained) — 1KP, GIGA, EBGP, bioRxiv preprint networks building alternative verification pathways. Constrained by institutional transition periods (5-15 year sunset) but has genuine agency and exit path.
 *   - Peer Review Certification System: Institutional actor (institutional/arbitrage) — maintains performative review ritual; journal endorsement signals verification despite reviewer acknowledged incapacity. Sees own process as degraded (piton perspective).
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements (computational access asymmetry, publication incentives) as inherent genomic limits.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(whole_genome_duplication_verification, 0.52).
domain_priors:suppression_score(whole_genome_duplication_verification, 0.58).
domain_priors:theater_ratio(whole_genome_duplication_verification, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(whole_genome_duplication_verification, extractiveness, 0.52).
narrative_ontology:constraint_metric(whole_genome_duplication_verification, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(whole_genome_duplication_verification, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(whole_genome_duplication_verification, tangled_rope).
narrative_ontology:human_readable(whole_genome_duplication_verification, "Whole Genome Duplication Verification Bottleneck").
narrative_ontology:topic_domain(whole_genome_duplication_verification, "evolutionary_biology/genomics/experimental_verification").

domain_priors:requires_active_enforcement(whole_genome_duplication_verification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(whole_genome_duplication_verification, original_duplication_claimants).
narrative_ontology:constraint_beneficiary(whole_genome_duplication_verification, large_sequencing_consortia).
narrative_ontology:constraint_victim(whole_genome_duplication_verification, replication_research_groups).
narrative_ontology:constraint_victim(whole_genome_duplication_verification, evolutionary_hypothesis_field).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FIELD EVOLUTIONARY HYPOTHESIS RELIABILITY (SNARE) — The field cannot exit the verification crisis for whole genome duplication (WGD) claims. Phylogenetic inference depends on accurate duplication dating and synteny verification. Premature or incorrect WGD claims propagate through downstream evolutionary reconstructions. The field bears extraction with no exit mechanism — powerless collective good that cannot organize or refuse participation.
constraint_indexing:constraint_classification(whole_genome_duplication_verification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDEPENDENT VERIFICATION GROUP (TANGLED ROPE) — Constrained by massive computational costs (teraflop-scale sequence alignment, synteny block detection across megabase regions), specialized expertise requirements, and publication bias against negative WGD findings. Also benefits from WGD-focused databases, consortial tools (Ensembl, Phytozome), and collaborative method development. Significant extraction but mixed with genuine coordination — some agency but at high cost.
constraint_indexing:constraint_classification(whole_genome_duplication_verification, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ORIGINAL WGD CLAIMANT CONSORTIUM (ROPE) — Benefits from first-mover advantage: early WGD publications establish interpretive frameworks, secure funding, and claim evolutionary significance before independent verification. Experiences constraint as coordination mechanism — communicating duplication hypotheses via consortium databases enables hypothesis refinement. Net beneficiary with arbitrage options — can reposition findings if contradicted or pivot to new organism groups.
constraint_indexing:constraint_classification(whole_genome_duplication_verification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN GENOMICS COALITION (SCAFFOLD) — Organized actors (1KP project, Earth BioGenome Project, GIGA consortium) see WGD verification as a temporary coordination failure with a sunset. Distributed genome annotation, preprint-based hypothesis sharing (bioRxiv), and standardized synteny detection pipelines are building alternative verification pathways. Low effective extraction because coalition has agency and sees convergence to open data norms. Sunset logic: as reference genome availability and automated synteny tools mature (5-15 years), verification becomes commodified rather than monopolized.
constraint_indexing:constraint_classification(whole_genome_duplication_verification, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PEER REVIEW CERTIFICATION SYSTEM (PITON) — Traditional peer review for WGD claims is substantially performative. Reviewers cannot verify terabytes of alignment data, synteny block homology inference, or phylogenetic model assumptions from a manuscript. Review ritual persists through institutional inertia — journal acceptance still signals 'verified' despite reviewers admitting inability to reproduce the core computational claims. Theater ratio high because certification function is degraded but the theater persists.
constraint_indexing:constraint_classification(whole_genome_duplication_verification, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scale, some verification lag in duplication detection is inherent to genomic complexity: polyploid events leave degenerate signatures that fade over evolutionary time, computational methods diverge on inference thresholds, and ancient duplications are intrinsically harder to distinguish from speciation. This perspective naturalizes the bottleneck as an immutable property of evolutionary genomics. However, structural data reveals this as false summit — the bottleneck reflects institutional arrangements (publication incentives, computational access asymmetry, funding concentration) not inherent limits.
constraint_indexing:constraint_classification(whole_genome_duplication_verification, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(whole_genome_duplication_verification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(whole_genome_duplication_verification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(whole_genome_duplication_verification, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(whole_genome_duplication_verification, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(whole_genome_duplication_verification, TR),
    TR >= 0.70.

:- end_tests(whole_genome_duplication_verification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The original WGD claimants capture interpretive authority during the 3-7 year verification window, establishing duplication frameworks and securing continued funding before independent verification. This is extraction because later verifiers can only confirm/refute the frame, not compete to propose the original hypothesis. However, extraction is not maximal because: (a) the field coordinates on shared reference genomes and methods, (b) some independent verification does occur, and (c) data access is improving. Suppression (0.58): Moderate-high. Significant barriers exist: computational costs ($10k-50k per verification), specialized expertise in phylogenomics, publication bias against negative WGD results, delayed access to high-quality genomes, and methodological plurality that allows claimants to choose favorable inference parameters. Barriers are not insurmountable — some groups replicate — but they are substantial. Theater ratio (0.68): High and rising. Peer reviewers for top-tier genomics journals explicitly acknowledge they cannot verify terabyte-scale alignment data or validate synteny inference assumptions. Review process focuses on plausibility, novelty, and presentation quality rather than reproducibility. Theater has increased as genomic datasets have outpaced reviewer capacity. The measurement trajectory shows theater rising from 0.42 to 0.68 while extractiveness rises from 0.35 to 0.52, indicating Piton dynamics: the certification function is degrading (theater increasing) while the constraint persists (extractiveness not declining).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how different structural positions generate incompatible classifications from identical base properties. The original consortium sees coordination (Rope) — they are solving the legitimate problem of dating duplication events and sharing predictions. Independent groups see mixed coordination and extraction (Tangled Rope) — the system enables hypothesis testing but constrains their ability to compete. The open genomics coalition sees a temporary problem with a sunset (Scaffold) — standardized pipelines and preprint scrutiny are alternative verification pathways maturing over 5-15 years. The peer review system sees its own degraded ritual (Piton) — the certification function persists through institutional inertia despite acknowledged reviewer incapacity. The field's epistemic reliability sees pure extraction (Snare) — premature duplication claims corrupt phylogenetic inference with no self-correction mechanism. The civilizational analytical observer risks seeing an immutable natural law (Mountain) — duplication detection is inherently hard because ancient signals fade and methods diverge — but the structural data reveals this as false summit: the institutional arrangements (computational access asymmetry, publication incentives, funding capture) are contingent, not laws of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Original WGD claimants experience low effective extraction (beneficiaries with arbitrage options, institutional power, immediate time horizon). They derive d ≈ 0.15-0.25: benefits from coordination, but also captures first-mover rent. Independent verification groups experience high effective extraction (victims with constrained exit, moderate power, biographical horizon). They derive d ≈ 0.65-0.75: face resource barriers but have some agency. The field's epistemic reliability experiences maximum extraction (powerless victim, trapped exit, global scope). It derives d ≈ 0.95: cannot organize or exit. The open-science coalition experiences moderate extraction (organized power with exit path to alternative verification, constrained exit but with sunset logic). It derives d ≈ 0.45-0.55: high agency balances moderate extraction cost. Perspectival gaps emerge from these d values: beneficiaries see rope (low chi), organized actors see scaffold (moderate chi with sunset), moderate victims see tangled rope (high chi), powerless agents see snare (maximum chi), and analytical observers risk mountain (naturalization of institutional constraints).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that all six types are legitimate perspectives without a single 'correct' classification. The mandatrophy question ('Is this Rope coordination or Snare extraction?') dissolves when recognizing that different agents experience structurally different constraints. The beneficiary truly experiences Rope — they coordinate predictions. The powerless field truly experiences Snare — it bears extraction with no exit. The organized coalition truly experiences Scaffold — it has exit paths materializing. The certification system truly experiences Piton — it maintains a degraded ritual. The analytical observer's Mountain is a false summit — natural law language naturalizes institutional contingency. The resolution is not to choose one type but to recognize that the presheaf over the observation site IS the answer: the constraint manifests differently depending on structural position, and all six manifestations are real. The framework diagnoses false summits by checking whether the mountain's natural law framing ('duplication detection is inherently hard') actually eliminates the structural features that make extraction possible (computational barriers, timing asymmetry, incentive structure). It doesn't — these are contingent, not necessary — so the mountain is false.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    synteny_signature_decay_threshold,
    'What evolutionary age threshold distinguishes ancient WGD signals from background genomic rearrangement noise?',
    'Comparative analysis of WGD claims across age ranges; validation against paleontological calibration points; investigation of whether claimed signal persists under alternative synteny block detection thresholds',
    'If threshold < 50 Mya: many legitimate ancient duplications classified as spurious. If threshold > 200 Mya: extractive redating claims persist unchallenged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(synteny_signature_decay_threshold, empirical, 'Evolutionary age threshold for WGD signal degradation').

omega_variable(
    computational_method_convergence,
    'Do independent computational WGD detection pipelines (synteny-based, phylogenomic, Ks distribution, tree reconciliation) produce correlated predictions or orthogonal measurements?',
    'Cross-method correlation study on standardized datasets; identification of shared systematic biases vs independent error sources; reanalysis of disputed WGD claims with alternative pipelines',
    'If methods converge: verification bottleneck is legitimate complexity (coordination problem). If methods diverge: inference framework is underdetermined (extraction mechanism — claimants choose methods favoring their hypothesis).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(computational_method_convergence, empirical, 'Whether independent WGD detection methods show correlated predictions').

omega_variable(
    preprint_scrutiny_catch_rates,
    'Does distributed preprint review on bioRxiv and Zenodo catch WGD methodological errors at rates comparable to or exceeding traditional journal peer review?',
    'Comparison of error detection rates: preprint comments/revisions vs journal review rounds for identical manuscripts; tracking of WGD claims posted as preprints first vs direct journal submission; post-publication correction rates',
    'If effective: open-science scaffold is real, sunset is structural. If ineffective: many-eyes cannot catch specialized computational errors, and scaffold perspective is aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preprint_scrutiny_catch_rates, empirical, 'Whether bioRxiv distributed scrutiny catches WGD methodological errors').

omega_variable(
    funding_capture_mechanism,
    'Are WGD claims preferentially fundable if they exceed posterior probability bounds, creating economic pressure for inflated duplication signatures?',
    'Analysis of funding success rates for WGD proposals with different confidence thresholds; survey of replication group funding requests relative to first-mover claims; comparison of NIH/NSF awards to original-claim vs verification-focused research',
    'If funding captures: extraction mechanism is economic incentive structure (snare). If funding neutral: extraction is mainly epistemic authority asymmetry (tangled rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(funding_capture_mechanism, empirical, 'Whether WGD funders preferentially reward novel claims over verification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(whole_genome_duplication_verification, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wgd_tr_t0, whole_genome_duplication_verification, theater_ratio, 0, 0.42).
narrative_ontology:measurement(wgd_tr_t3, whole_genome_duplication_verification, theater_ratio, 3, 0.55).
narrative_ontology:measurement(wgd_tr_t6, whole_genome_duplication_verification, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(wgd_be_t0, whole_genome_duplication_verification, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(wgd_be_t3, whole_genome_duplication_verification, base_extractiveness, 3, 0.44).
narrative_ontology:measurement(wgd_be_t6, whole_genome_duplication_verification, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(whole_genome_duplication_verification, resource_allocation).
narrative_ontology:affects_constraint(whole_genome_duplication_verification, phylogenetic_inference_reliability).
narrative_ontology:affects_constraint(whole_genome_duplication_verification, computational_biology_expertise_scarcity).
narrative_ontology:affects_constraint(whole_genome_duplication_verification, sequencing_consortium_priority_setting).

% DUAL FORMULATION NOTE:
% WGD verification is downstream of specific duplication claims in individual organisms but represents a distinct structural constraint on the verification process itself. Upstream constraints (individual WGD hypotheses in specific lineages) have their own extractiveness values reflecting empirical status; this constraint captures the institutional bottleneck that impedes verification across the field.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(whole_genome_duplication_verification, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
