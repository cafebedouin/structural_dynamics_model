% ============================================================================
% CONSTRAINT STORY: hominin_evolutionary_bottleneck
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hominin_evolutionary_bottleneck, []).

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
 *   constraint_id: hominin_evolutionary_bottleneck
 *   human_readable: The Hominin Evolutionary Bottleneck & Replacement Event
 *   domain: scientific/biological/paleoanthropology
 *
 * SUMMARY:
 *   The hominin fossil record presents a structural constraint that has
 *   shaped evolutionary narratives for over a century. The sparse,
 *   geographically concentrated, and institutionally curated fossil record
 *   creates a bottleneck where narrative authority concentrates in
 *   established research institutions with control over specimen access,
 *   publication gatekeeping, and canonical storytelling. This constraint
 *   exhibits all six DR types from different perspectives, revealing how the
 *   same empirical limitation (incomplete fossil preservation) can
 *   simultaneously function as a coordination mechanism (rope), an extraction
 *   apparatus (snare/tangled rope), a performative institutional ritual
 *   (piton), and a natural law (false summit mountain). The constraint's
 *   theater ratio (0.65) reflects that much paleoanthropological discourse
 *   centers on pedagogical narratives and linear story-telling rather than
 *   empirical falsification. The canonical linear sequence
 *   (australopithecines → Homo habilis → H. erectus → H. neanderthalensis →
 *   H. sapiens) persists in textbooks and museums despite accumulating
 *   evidence from ancient DNA, morphological reanalysis, and phylogenetic
 *   methods supporting parallel lineages, gene flow, and non-linear
 *   branching. Ancient DNA methodologies represent a structural exit from the
 *   fossil record bottleneck — they provide independent evidence streams
 *   (genome sequences from preserved bones and teeth) that bypass
 *   preservation bias and institutional gatekeeping, enabling the scaffold
 *   perspective's sunset logic.
 *
 * KEY AGENTS:
 *   - Established Paleoanthropological Institutions: Primary beneficiary (institutional/arbitrage) — museums, major universities (Harvard, UC Berkeley, Max Planck), research centers control fossil collections, publication channels, and narrative authority. Extraction flows toward this agent through prestige, funding concentration, and agenda-setting power.
 *   - Fossil Record & Preservation Bias: Primary victim (powerless/trapped) — the incomplete empirical commons is systematically exploited to support institutional narratives. No self-correction mechanism; cannot exit or organize.
 *   - Field Paleoanthropologists: Secondary victim (moderate/constrained) — individual researchers dependent on institutional access to collections, funding from establishment sources, publication through gatekept journals. Constrained but with some agency through methodological innovation.
 *   - Ancient DNA Coalition: Organized agents (organized/constrained) — aDNA researchers, genomicists, paleoproteomicists building parallel evidence streams. Funded separately (newer funding lines), organized around novel methodologies, possess clear exit path from fossil record dependence.
 *   - Alternative Hypothesis Advocates: Organized agents (moderate/mobile) — researchers proposing parallel lineages, gene flow models, non-linear branching. Some have arbitrage options (private funding, academic freedom niches), but face publication resistance and prestige penalties.
 *   - Linear Narrative Canon: Institutional performative system (institutional/arbitrage) — the pedagogical story persists through textbook reproduction, museum exhibits, lecture tradition. Experiences constraint as degraded ritual maintained by inertia.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hominin_evolutionary_bottleneck, 0.38).
domain_priors:suppression_score(hominin_evolutionary_bottleneck, 0.62).
domain_priors:theater_ratio(hominin_evolutionary_bottleneck, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hominin_evolutionary_bottleneck, extractiveness, 0.38).
narrative_ontology:constraint_metric(hominin_evolutionary_bottleneck, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(hominin_evolutionary_bottleneck, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hominin_evolutionary_bottleneck, tangled_rope).
narrative_ontology:human_readable(hominin_evolutionary_bottleneck, "The Hominin Evolutionary Bottleneck & Replacement Event").
narrative_ontology:topic_domain(hominin_evolutionary_bottleneck, "scientific/biological/paleoanthropology").

domain_priors:requires_active_enforcement(hominin_evolutionary_bottleneck).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hominin_evolutionary_bottleneck, established_paleoanthropological_institutions).
narrative_ontology:constraint_beneficiary(hominin_evolutionary_bottleneck, narrative_continuity_advocates).
narrative_ontology:constraint_victim(hominin_evolutionary_bottleneck, alternative_evolutionary_hypotheses).
narrative_ontology:constraint_victim(hominin_evolutionary_bottleneck, field_empirical_credibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FOSSIL RECORD GAPS (SNARE) — The incomplete fossil record cannot exit the constraint imposed by preservation bias, diagenesis, and accessibility. These gaps are systematically exploited to support narratives that align with institutional preferences. The empirical commons bears the cost of suppressed alternative hypotheses while lacking any mechanism for self-correction.
constraint_indexing:constraint_classification(hominin_evolutionary_bottleneck, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FIELD PALEOANTHROPOLOGIST (TANGLED ROPE) — Individual researchers are constrained by funding dependencies, institutional prestige hierarchies, and publication gatekeeping, yet also benefit from access to fossil collections, methodological infrastructure, and collaborative networks maintained by establishment institutions. Significant extraction with genuine mixed benefits — constrained but not entirely powerless.
constraint_indexing:constraint_classification(hominin_evolutionary_bottleneck, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ESTABLISHED INSTITUTIONS (ROPE) — Major museums, universities, and research centers benefit from curating the dominant narrative and controlling access to fossil collections. They experience the constraint primarily as a coordination mechanism: managing the sparse record, controlling publication channels, setting research agendas. Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(hominin_evolutionary_bottleneck, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANCIENT DNA COALITION (SCAFFOLD) — New genomic and paleogenomic methods (aDNA sequencing, proteomics, isotope analysis) are building parallel verification pathways that bypass traditional fossil-based narratives. These technologies bypass the fossil record's preservation bias and provide independent evidence streams. Organized researchers see the bottleneck as temporary — sun-setting as sequencing costs drop and methodology matures. Low effective extraction because this coalition has technical agency and clear exit path.
constraint_indexing:constraint_classification(hominin_evolutionary_bottleneck, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LINEAR NARRATIVE CANON (PITON) — The canonical story of human evolution (australopithecines → Homo habilis → H. erectus → H. neanderthalensis → H. sapiens) persists through institutional inertia despite mounting evidence of parallel lineages, gene flow, and non-linear branching. The narrative ritual maintains itself through textbooks, museum displays, and pedagogical theater. Function has degraded — the narrative no longer captures evolutionary complexity — but institutional momentum preserves it. Theater ratio reflects the performative maintenance of this linear structure.
constraint_indexing:constraint_classification(hominin_evolutionary_bottleneck, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / TAPHONOMIC NATURAL LAW (MOUNTAIN) — From a civilizational perspective, some empirical gaps are inherent to the fossil record: diagenesis, differential preservation, and collection bias are immutable properties of paleontological evidence. This perspective naturalizes the bottleneck as an inevitable constraint on knowledge. However, structural data contradicts the mountain classification — the engine's false summit detector identifies this as naturalization of what is actually a contingent institutional arrangement of narrative control and evidence suppression.
constraint_indexing:constraint_classification(hominin_evolutionary_bottleneck, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hominin_evolutionary_bottleneck_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hominin_evolutionary_bottleneck, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hominin_evolutionary_bottleneck, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(hominin_evolutionary_bottleneck, TR),
    TR >= 0.70.

:- end_tests(hominin_evolutionary_bottleneck_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The fossil record bottleneck does extract value — institutional gatekeeping controls prestige, funding allocation, and narrative authority. But the extraction is not total; alternative methodologies are emerging, some funding flows to novel approaches, and internet-age dissemination reduces publication gatekeeping. The value (0.38 vs. higher 0.60+ for pure snares) reflects that the constraint is weakening and contested. Suppression (0.62): Moderate-high. Significant barriers to alternative hypotheses include publication bias (higher rejection rates for non-canonical models), funding concentration toward establishment institutions, prestige penalties for challenging canonical narratives, and collection access restrictions. But suppression is not totalitarian — alternative views are published in specialist venues, independent funding sources exist (private grants, international collaboration), and disciplinary pressure is diffuse rather than centrally enforced. Theater ratio (0.65): High. Pedagogical narratives dominate public and student-facing discourse. The linear sequence is taught as canonical fact despite internal inconsistencies, and museum exhibits present the linear story as settled rather than contested. The theater has increased over the interval (0.35→0.65) as the gap between the canonical narrative and actual empirical evidence has widened.
 *
 * PERSPECTIVAL GAP:
 *   The bottleneck produces stark perspectival divergence. Established institutions experience coordination (Rope) — they are solving the legitimate problem of managing sparse evidence. The ancient DNA coalition experiences a sunset constraint (Scaffold) — methodological alternatives provide genuine exit. Field researchers experience mixed extraction with benefit (Tangled Rope) — constrained access but also genuine professional opportunity. The fossil record experiences pure extraction (Snare) — systematically exploited with no self-defense. The linear narrative experiences its own degradation (Piton) — maintained through inertia despite empirical hollowing. The civilizational analytical observer risks naturalizing contingency (false summit Mountain) — treating institutional narrative control as an immutable property of paleontological evidence. The perspectival gap is widening as genomic methods mature and the performance character of the linear narrative becomes more visible.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from its structural position relative to the bottleneck. Established institutions benefit from controlling the sparse record (low d, negative f(d), net subsidy through constraint). Field paleonthrologists depend on institutional access (high d, moderate exit options, medium f(d)). The fossil record itself has no agency (trapped powerless, d≈1.0, maximum f(d)). Ancient DNA researchers have organized alternatives and arbitrage options (constrained exit, medium d, medium-low f(d)). The piton classification derives from theater ratio rather than high chi. The mountain perspective at the analytical context is a false summit — taphonomic limits are real but are being systematically exploited to suppress empirical alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates mandatrophy resolution through perspectival multiplicity and temporal degradation. The constraint begins as legitimate coordination (Rope, t=0: ε=0.18, theater=0.35) — institutions genuinely solving the problem of sparse evidence. Over ten years, it accumulates extraction (ε→0.38) and theater (0.65), shifting toward Tangled Rope classification. The mandatrophy question — 'Is this coordination or extraction?' — is answered by showing that it IS BOTH, and the balance shifts over time. The ancient DNA coalition's emergence represents a structural escape from the mandatrophy: by providing independent evidence pathways, they create the conditions for distinguishing legitimate coordination (explaining why the fossil record is sparse) from institutional extraction (explaining why alternative hypotheses are suppressed). The piton perspective reveals that the linear narrative's function has degraded — it no longer maps to empirical reality — but institutional momentum preserves the theatrical performance. The constraint will resolve its mandatrophy as genomic methods mature and institutional control over narrative authority weakens. At that point, the coordination function will separate from the extraction mechanism, and the constraint will decompose into (a) a rope (legitimate empirical constraints of fossil preservation), and (b) a piton (theatrical maintenance of outdated narrative) that will degrade toward irrelevance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    preservation_bias_quantification,
    'What fraction of the hominin fossil record''s narrative authority derives from genuine evidence versus from taphonomic and institutional collection bias?',
    'Statistical modeling of fossilization probability by site, age, and accessibility; comparison of collection effort intensity across geographic regions and institutions; Bayesian inference of true lineage diversity from available specimens',
    'If preservation bias > 70%: the bottleneck is primarily taphonomic (mountain view gains support). If preservation bias < 40%: the bottleneck is primarily institutional (snare/tangled rope views confirmed). If 40-70%: genuine hybrid constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preservation_bias_quantification, empirical, 'Quantifying preservation bias versus institutional gatekeeping in fossil record authority').

omega_variable(
    alternative_lineage_suppression,
    'Have viable alternative evolutionary hypotheses (parallel lineages, gene flow networks, non-linear branching) been systematically suppressed by institutional gatekeeping, or do they remain marginal for empirical reasons?',
    'Citation analysis of alternative hypotheses pre/post-aDNA revolution; funding allocation patterns for conventional vs alternative research approaches; publication bias testing (rejection rates, review wait times, editorial acceptance patterns); longitudinal tracking of hypothesis acceptance trajectories',
    'If suppression is primary: snare and tangled rope classifications strengthened. If empirical reasons dominate: constraint is legitimate knowledge coordination problem (rope). If mixed: tangled rope confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_lineage_suppression, empirical, 'Whether alternative hypotheses were suppressed institutionally or rejected empirically').

omega_variable(
    genomic_evidence_independence,
    'Do ancient DNA and paleogenomic methods provide genuinely independent evidence of hominin relationships, or are they constrained by reference genome selection and calibration dependencies on fossil-based dating?',
    'Cross-validation of aDNA phylogenies with independent genomic markers; sensitivity analysis of divergence time estimates to reference genome choice; test for circular inference between fossil calibrations and sequence-based rates',
    'If truly independent: scaffold perspective confirmed — aDNA provides alternative verification pathway with sunset logic. If constrained by fossil calibrations: genomic methods are still dependent on the bottleneck, no real exit path.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(genomic_evidence_independence, empirical, 'Independence of genomic evidence from fossil-based constraints').

omega_variable(
    narrative_replacement_timeline,
    'Over what timespan will the linear narrative canon be replaced by a network/gene-flow model in mainstream paleoanthropology curricula and institutional canon?',
    'Longitudinal analysis of textbook content evolution; survey of museum exhibit updates; tracking of curriculum adoptions in major universities; citation analysis of competing frameworks in high-impact journals',
    'If replacement < 10 years: scaffold sunset is real and near; piton theater will decline. If replacement > 25 years: institutional inertia is severe; theater ratio will remain high. If replacement in 10-20 years: scaffold timeline matches estimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrative_replacement_timeline, empirical, 'Timeline for narrative replacement from linear to network evolutionary model').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hominin_evolutionary_bottleneck, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hominin_tr_t0, hominin_evolutionary_bottleneck, theater_ratio, 0, 0.35).
narrative_ontology:measurement(hominin_tr_t5, hominin_evolutionary_bottleneck, theater_ratio, 5, 0.5).
narrative_ontology:measurement(hominin_tr_t10, hominin_evolutionary_bottleneck, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(hominin_be_t0, hominin_evolutionary_bottleneck, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(hominin_be_t5, hominin_evolutionary_bottleneck, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(hominin_be_t10, hominin_evolutionary_bottleneck, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hominin_evolutionary_bottleneck, information_standard).
narrative_ontology:affects_constraint(hominin_evolutionary_bottleneck, human_origins_narrative_authority).
narrative_ontology:affects_constraint(hominin_evolutionary_bottleneck, paleoanthropological_funding_concentration).

% DUAL FORMULATION NOTE:
% The hominin evolutionary bottleneck decomposes into two structurally distinct constraints: (1) the taphonomic gap in the fossil record (legitimate preservation bias, approaching mountain), and (2) the institutional narrative control exploiting that gap (extraction mechanism, snare/tangled rope). These are separate stories with different ε values. The fossil preservation limit is ε≈0.12 (mountain). The institutional gatekeeping is ε≈0.38 (tangled rope). They are linked through network.affects_constraints because institutional control prevents alternative methodologies from addressing the taphonomic gaps.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hominin_evolutionary_bottleneck, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
