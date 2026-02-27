% ============================================================================
% CONSTRAINT STORY: vertebrate_turning_point_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   The vertebrate turning-point model posits that a singular genetic event —
 *   whole-genome duplication coupled with acquisition of developmental
 *   toolkit genes (Hox clusters, regulatory networks) — enabled the evolution
 *   of the vertebrate body plan. This model has become dominant in evo-devo
 *   textbooks and research synthesis despite ongoing empirical contestation.
 *   The constraint exhibits tangled_rope structure: the model provides
 *   genuine coordination benefit (unifying disparate developmental and fossil
 *   findings into a teachable framework) while simultaneously suppressing
 *   alternative evolutionary narratives through institutional gatekeeping,
 *   publication bias, and career incentives. The theater ratio (0.65)
 *   reflects that the model's pedagogical power (coherent narrative of
 *   singular transformation) often overshadows empirical complexity. The
 *   suppression level (0.42) captures barriers facing researchers proposing
 *   alternative pathways: they must work against editorial bias, funding
 *   allocation favoring consensus research, and career risk. Extractiveness
 *   (0.38) is moderate because the coordination function is real (the model
 *   genuinely organizes knowledge and enables research), but the asymmetric
 *   extraction from anomalous data and dissenting researchers is significant.
 *   The constraint is maintained through active enforcement: textbooks and
 *   senior scientists reinforce the model; alternative frameworks receive
 *   less institutional support. This is not a natural law but a contingent
 *   scientific consensus under institutional pressure.
 *
 * KEY AGENTS:
 *   - Consensus Model Proponents: Institutional actors (institutional/arbitrage) — major evo-devo laboratories, textbook authors, synthesis editors. Benefit from model authority through career advancement, grant success, curriculum influence.
 *   - Empirical Researchers (Fossil/Developmental): Primary victims of moderate power (moderate/constrained) — paleontologists and developmental biologists whose findings must be framed through the model or face peer pressure and citation delay.
 *   - Anomalous Empirical Data: Powerless victims (powerless/trapped) — fossil sequences, developmental constraints, molecular patterns that contradict the model cannot exit discourse or organize. Suppressed through reinterpretation or exclusion.
 *   - Researchers Challenging Consensus: Marginalized researchers (powerless/trapped) — scientists proposing alternative evolutionary pathways face publication bias, citation suppression, career risk. Career advancement requires eventual alignment.
 *   - Textbook Narrative Machinery: Institutional maintenance system (institutional/arbitrage) — educational systems, curriculum standards, popular science media that reproduce the model through inertia and institutional convenience.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a contingent institutional choice as inherent scientific process.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vertebrate_turning_point_2026, 0.38).
domain_priors:suppression_score(vertebrate_turning_point_2026, 0.42).
domain_priors:theater_ratio(vertebrate_turning_point_2026, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vertebrate_turning_point_2026, extractiveness, 0.38).
narrative_ontology:constraint_metric(vertebrate_turning_point_2026, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(vertebrate_turning_point_2026, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vertebrate_turning_point_2026, tangled_rope).
narrative_ontology:human_readable(vertebrate_turning_point_2026, "The Genetic Turning Point for Vertebrate Evolution (Scientific Model)").
narrative_ontology:topic_domain(vertebrate_turning_point_2026, "biological/scientific").

domain_priors:requires_active_enforcement(vertebrate_turning_point_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vertebrate_turning_point_2026, consensus_model_proponents).
narrative_ontology:constraint_beneficiary(vertebrate_turning_point_2026, developmental_biology_frameworks).
narrative_ontology:constraint_victim(vertebrate_turning_point_2026, alternative_hypotheses).
narrative_ontology:constraint_victim(vertebrate_turning_point_2026, empirical_anomalies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANOMALOUS EMPIRICAL DATA (SNARE) — Fossil sequences, developmental constraints, and molecular phylogenies that don't fit the turning-point narrative cannot exit the discourse or organize. They are suppressed through data reinterpretation, alternative phylogenetic rooting, or exclusion from synthesis papers. Maximum extraction: contradictory observations bear full cost of the model's integrity maintenance without recourse.
constraint_indexing:constraint_classification(vertebrate_turning_point_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RESEARCHERS CHALLENGING THE CONSENSUS (SNARE) — Scientists proposing alternative evolutionary pathways face publication bias, citation suppression, and career risk. Their work is not refuted but marginalized through resource allocation and institutional gatekeeping. Suppression is high (0.42): editorial review favors confirmatory framing; funding priorities align with consensus model; senior colleagues signal reputational cost. Trapped exit: career advancement requires eventual alignment or silence.
constraint_indexing:constraint_classification(vertebrate_turning_point_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: CONSENSUS MODEL PROPONENTS (ROPE) — Institutional actors (major evo-devo labs, textbook authors, synthesis paper editors) experience the turning-point model as coordination mechanism. It unifies disparate findings, enables teaching, and provides research continuity across generations. Benefits from model authority: career advancement, grant success, influence on curriculum. Low experienced extraction: they are extractors, not targets. The model solves genuine coordination problems in organizing vertebrate knowledge.
constraint_indexing:constraint_classification(vertebrate_turning_point_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EMPIRICAL RESEARCHERS (TANGLED ROPE) — Paleontologists and developmental biologists conducting primary research face mixed constraints. The model provides framework and funding opportunity (coordination benefit), but also constrains interpretation of findings: novel developmental pathways must be narrated through the turning-point lens or face peer pressure. Constrained exit: can publish outside consensus but face resource scarcity and citation delays. Asymmetric extraction: receive pressure to frame findings as confirmatory rather than exploratory.
constraint_indexing:constraint_classification(vertebrate_turning_point_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TEXTBOOK NARRATIVE (PITON) — The vertebrate turning-point story persists in education and popular science despite ongoing empirical contestation. Textbooks present the model as settled fact; alternatives are relegated to 'current debates' sections or omitted entirely. Theater ratio (0.65): the narrative emphasizes dramatic transformation (singular genetic event, enabled backbones) over the messy reality of multiple independent innovations, developmental constraints, and contingent ecological opportunities. The story is maintained through institutional inertia — it has worked as an organizational framework for so long that updating it requires coordinating across countless curricula, textbooks, and course syllabi. Primary function (unifying understanding) has partially atrophied; performance (coherent narrative) has become primary.
constraint_indexing:constraint_classification(vertebrate_turning_point_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some narrative simplification is inherent to science: complex evolutionary transitions always require selection of features for emphasis, and the gap between full phylogenetic reality and teachable model is structural. However, this perspective risks naturalizing what is actually a contingent institutional choice. The structural data (high suppression, theater, and active enforcement) indicates not a natural law but a maintained consensus under institutional pressure.
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
    constraint_indexing:constraint_classification(vertebrate_turning_point_2026, TypeOther, context(agent_power(institutional), _, _, _)),
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
 *   Extractiveness (0.38): Moderate. The model extracts value from proponents (who gain authority and research continuity) at cost to anomalous researchers and alternative hypotheses. The extraction is not maximal because the model does provide genuine coordination benefit — it does organize vertebrate knowledge more effectively than pure fragmentation. The value reflects asymmetric benefit (proponents gain substantially; challengers lose substantially) while acknowledging the functional contribution. Suppression (0.42): Moderate-high. Barriers to independent verification and alternative framing include: editorial preference for consensus-confirming manuscripts, funding allocation favoring established model research, institutional status advantages for senior proponents, graduate training that embeds the model as foundational, and publication bias against negative results (findings that don't fit the model). The suppression is not total (alternative researchers do publish and occasionally gain influence) but is substantial. Theater ratio (0.65): Moderate-high. The model's explanatory power is partially theatrical: the 'turning point' narrative is inherently more memorable and teachable than the complex reality of multiple innovations distributed across millions of years with contingent ecological opportunities. Textbooks emphasize the dramatic singular event over the messy gradual reality. Theater has increased over the interval as the model has become more embedded in curricula and popular science, prioritizing narrative coherence over empirical nuance.
 *
 * PERSPECTIVAL GAP:
 *   The original account (Perspective 1-2) sees extraction (snare) — anomalous data and dissenting researchers experience suppression and institutional gatekeeping with no recourse. The proponent account (Perspective 3) sees coordination (rope) — the model solves genuine problems in organizing knowledge. The empirical researcher account (Perspective 4) sees mixed coordination and extraction (tangled_rope) — the model provides framework and opportunity but constrains interpretation. The textbook system (Perspective 5) sees itself as degraded (piton) — performing a function (teachable narrative) that no longer requires deep verification, maintained by inertia. The civilizational observer (Perspective 6) risks seeing natural law (mountain) — narrative simplification is inherent to science — but structural data contradicts this: the active enforcement, suppression levels, and theater ratio indicate a contingent institutional choice, not a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation for each perspective reflects the agent's power level, exit capacity, and structural position relative to extraction flows. Consensus proponents with institutional power and arbitrage options experience low effective extraction — they are extractors, not targets. Empirical researchers with moderate power and constrained exit experience asymmetric pressure: the model creates opportunity (funding, coordinated research programs) but also constrains their interpretive freedom and slows citation of alternative framings. Researchers directly challenging the consensus face maximum extraction with trapped exit: speaking against the model carries career cost that constrains their ability to operate independently. Anomalous data has no power and no exit — it can be reinterpreted or excluded but cannot organize in self-defense. The analytical observer at civilizational scope risks naturalizing this institutional arrangement as inherent to science (false summit), when the structural data reveals it as maintained consensus under pressure.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by disambiguating the pedagogical function (real coordination benefit) from the institutional extraction (suppression of alternatives). The turning-point model genuinely coordinates knowledge — it provides a unifying framework that enables teaching, research, and knowledge transfer across generations. This is the rope function. But the model simultaneously suppresses alternative evolutionary narratives through publication bias, funding allocation, and career incentives. This is the snare function. The tangled_rope classification captures both: the constraint provides coordination value while enabling asymmetric extraction from those who would propose alternatives. The model is neither 'true' nor 'false' — it is a useful simplification that has become institutionally entrenched. The mandatrophy resolves not by determining which type is correct but by showing that the model serves both coordination and extraction functions simultaneously, and the institutional pressure maintains the coordination benefit while suppressing awareness of the extraction cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    singular_vs_multiple_genetic_events,
    'Did vertebrate evolution result from a single ''turning point'' genetic event (whole-genome duplication + toolkit gene acquisition) or from multiple independent innovations distributed across millions of years?',
    'Comparative genomics of early-diverging vertebrate lineages; phylogenetic reconstruction of toolkit gene duplication events; fossil sequence analysis for temporal clustering of morphological innovations',
    'If singular: turning-point model is structurally correct, classification remains tangled_rope. If multiple: model is pedagogical simplification rather than empirical claim, classification shifts toward piton (performative narrative). If mixed (both): model is partially correct but suppresses legitimate debate about causality and timing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(singular_vs_multiple_genetic_events, empirical, 'Whether vertebrate evolution resulted from singular genetic event or multiple innovations').

omega_variable(
    necessity_vs_sufficiency_of_toolkit_genes,
    'Were duplicated toolkit genes (Hox, regulatory) necessary for vertebrate body plan evolution, or merely sufficient under specific ecological conditions that happened to occur?',
    'Experimental evidence (transgenic studies, comparative developmental analysis); phylogenetic comparative methods testing co-evolution of gene duplications with morphological innovations; ecological reconstruction of vertebrate divergence environment',
    'If necessary: model is mechanistic explanation. If merely sufficient: model conflates correlation with causation; represents extractive imposition of genetic determinism on contingent ecological history. Shifts classification weight toward snare (suppressing ecological alternatives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_vs_sufficiency_of_toolkit_genes, empirical, 'Whether toolkit genes were necessary or merely sufficient for vertebrate evolution').

omega_variable(
    alternative_developmental_pathways_viability,
    'Could vertebrate-like body plans have evolved through alternative developmental trajectories without the specific genetic toolkit emphasized in the turning-point model?',
    'Computational developmental models; phylogenetic analysis of non-vertebrate chordates (amphioxus, tunicates) for suppressed developmental capacities; fossil evidence of lost lineages with alternative morphological strategies',
    'If viable alternatives existed: turning-point narrative becomes contingent historical account rather than mechanistic necessity. Increases suppression estimate and shifts burden of proof to model proponents. If alternatives required other genetic preconditions: model becomes network of interdependencies rather than singular event.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_developmental_pathways_viability, empirical, 'Whether alternative developmental pathways could produce vertebrate-like body plans').

omega_variable(
    institutional_investment_vs_empirical_confidence,
    'To what degree does the persistence and authority of the turning-point model reflect institutional investment (textbooks, research programs, career incentives) versus empirical confidence (replicability, explanatory power, predictive success)?',
    'Bibliometric analysis of citation patterns (citing consensus vs. alternatives); survey of active researchers on confidence in model components; tracking of model revisions vs. model reaffirmations in the literature over time',
    'High institutional investment relative to empirical confidence indicates the model is partially performative (piton or tangled_rope with high theater). Lower ratio indicates genuine consensus. This meta-analysis directly measures suppression mechanisms and theater ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_investment_vs_empirical_confidence, empirical, 'Degree to which model persistence reflects institutional investment versus empirical confidence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vertebrate_turning_point_2026, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vtp_tr_t0, vertebrate_turning_point_2026, theater_ratio, 0, 0.48).
narrative_ontology:measurement(vtp_tr_t10, vertebrate_turning_point_2026, theater_ratio, 10, 0.58).
narrative_ontology:measurement(vtp_tr_t20, vertebrate_turning_point_2026, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(vtp_be_t0, vertebrate_turning_point_2026, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(vtp_be_t10, vertebrate_turning_point_2026, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(vtp_be_t20, vertebrate_turning_point_2026, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vertebrate_turning_point_2026, information_standard).
narrative_ontology:affects_constraint(vertebrate_turning_point_2026, whole_genome_duplication_verification).
narrative_ontology:affects_constraint(vertebrate_turning_point_2026, developmental_toolkit_gene_causality).
narrative_ontology:affects_constraint(vertebrate_turning_point_2026, alternative_vertebrate_evolution_pathways).

% DUAL FORMULATION NOTE:
% The vertebrate turning-point model is the consensus scientific narrative about how vertebrate body plans originated. This story itself — as a model maintained under institutional pressure — is the constraint. Downstream constraints include the specific empirical claims within the model (WGD timing, toolkit gene acquisition, developmental innovations). This constraint story analyzes the model as an institutional structure; the downstream stories analyze the specific biological claims the model encodes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
