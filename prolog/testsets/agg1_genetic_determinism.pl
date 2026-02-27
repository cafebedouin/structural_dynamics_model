% ============================================================================
% CONSTRAINT STORY: agg1_genetic_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_agg1_genetic_determinism, []).

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
 *   constraint_id: agg1_genetic_determinism
 *   human_readable: The institutional claim that the AGG-1 gene deterministically causes aggression
 *   domain: genetics/behavioral_science/institutional_authority
 *
 * SUMMARY:
 *   The AGG-1 genetic determinism constraint operates as a claim of
 *   institutional authority that simplified genetic behavioral causation
 *   benefits specific institutional actors while imposing costs on skeptics
 *   and on individuals/populations labeled as genetically aggressive. The
 *   constraint exhibits characteristics of both pure coordination (behavioral
 *   science has legitimate complexity reduction needs) and asymmetric
 *   extraction (toward research establishment, pharmaceutical industry, and
 *   criminal justice system) with significant suppression of alternative
 *   framings and mechanisms. The theater ratio (0.68) reflects that the
 *   claim's performative authority in policy, clinical, and legal contexts
 *   persists despite growing replication concerns and mechanistic unknowns.
 *   The extractiveness (0.58) reflects moderate institutional capture — the
 *   claim provides genuine simplification benefits while substantially
 *   overstating empirical support. The constraint's persistence despite
 *   skepticism reveals institutional inertia (piton dynamics) overlaid on
 *   genuine extraction mechanisms. The analytical observer sees a tangled
 *   rope: coordination problem (behavior is complex, institutions need
 *   heuristics) combined with asymmetric extraction (institutional
 *   beneficiaries maintain simplified narratives beyond empirical warrant).
 *   The skeptic coalitions (genetic determinism critics, gene-environment
 *   researchers, civil rights advocates) represent a scaffold perspective —
 *   they are building alternative framings (polygenic models,
 *   gene-environment interaction, epigenetics) that will gradually sunset the
 *   determinism claim as mechanistic understanding advances and institutional
 *   incentives realign.
 *
 * KEY AGENTS:
 *   - Genetically Labeled Individuals: Primary victim (powerless/trapped) — face deterministic attribution with no exit option; immutable genetic label affects career, legal, insurance outcomes
 *   - Populations Subject to Genetic Racism: Primary victim (moderate/constrained) — communities experiencing criminalization and justification for surveillance/incarceration when AGG-1 is applied within ancestry-correlated frameworks
 *   - Behavioral Genetics Research Establishment: Primary beneficiary (institutional/arbitrage) — captures funding, publication prestige, and institutional authority from maintaining AGG-1 determinism framing
 *   - Pharmaceutical Industry: Secondary beneficiary (organized/arbitrage) — enables drug development for 'genetic aggression'; markets therapeutic interventions built on determinism frame
 *   - Criminal Justice System: Mixed actor (powerful/constrained) — benefits from deterministic framing for sentencing simplification but constrained by growing skepticism undermining legitimacy
 *   - Genetic Determinism Skeptics & Reform Coalitions: Organized resistance (organized/constrained) — critical geneticists, civil rights organizations, neuroscientists building alternative frameworks with sunset logic
 *   - Eugenics-Era Institutional Frameworks: Historical inertia (institutional/arbitrage) — determinism framing persists through institutional maintenance despite scientific degradation
 *   - Analytical Observer: Sees integrated structure (analytical/analytical) — recognizes coordination and extraction simultaneously; identifies false summit risk if determinism is reframed as 'risk factor'
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(agg1_genetic_determinism, 0.58).
domain_priors:suppression_score(agg1_genetic_determinism, 0.62).
domain_priors:theater_ratio(agg1_genetic_determinism, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(agg1_genetic_determinism, extractiveness, 0.58).
narrative_ontology:constraint_metric(agg1_genetic_determinism, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(agg1_genetic_determinism, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(agg1_genetic_determinism, tangled_rope).
narrative_ontology:human_readable(agg1_genetic_determinism, "The institutional claim that the AGG-1 gene deterministically causes aggression").
narrative_ontology:topic_domain(agg1_genetic_determinism, "genetics/behavioral_science/institutional_authority").

domain_priors:requires_active_enforcement(agg1_genetic_determinism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(agg1_genetic_determinism, behavioral_genetics_research_establishment).
narrative_ontology:constraint_beneficiary(agg1_genetic_determinism, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(agg1_genetic_determinism, criminal_justice_system).
narrative_ontology:constraint_victim(agg1_genetic_determinism, genetic_determinism_skeptics).
narrative_ontology:constraint_victim(agg1_genetic_determinism, populations_labeled_genetically_aggressive).
narrative_ontology:constraint_victim(agg1_genetic_determinism, nuanced_scientific_discourse).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GENETICALLY LABELED INDIVIDUAL (SNARE) — An individual identified as carrying the AGG-1 variant experiences deterministic labeling with no escape. Career prospects, sentencing recommendations, insurance eligibility, and social stigma lock them into a trajectory. No exit option exists; the genetic label is immutable and globally applied. Maximum extraction: bears full cost of deterministic attribution without capacity to contest or exit.
constraint_indexing:constraint_classification(agg1_genetic_determinism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RACIAL/ETHNIC POPULATIONS (SNARE) — When AGG-1 determinism is applied within frameworks that correlate genetic variation with population ancestry, entire communities experience the constraint as criminalization and justification for policing/incarceration. Constrained exit: populations cannot opt out of the framing, but some agency exists through collective organizing and demand for replication/scrutiny.
constraint_indexing:constraint_classification(agg1_genetic_determinism, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: BEHAVIORAL GENETICS ESTABLISHMENT (ROPE) — Primary beneficiary with arbitrage exit. Funding, publication prestige, and institutional authority accrue from maintaining the AGG-1 determinism claim. Experiences the constraint as coordination: simplified narratives reduce complexity and align incentives. Net beneficiary through priority-claim capture and research funding concentration. Can pivot to new targets if pressured.
constraint_indexing:constraint_classification(agg1_genetic_determinism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PHARMACEUTICAL INDUSTRY (ROPE) — Beneficiary. Drug development for 'genetic aggression' enables market expansion and therapeutic positioning. Experiences the constraint as coordination with pharmaceutical endpoints. Net beneficiary; can arbitrage to other genetic targets if AGG-1 loses institutional support.
constraint_indexing:constraint_classification(agg1_genetic_determinism, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CRIMINAL JUSTICE SYSTEM (TANGLED ROPE) — Both beneficiary and constraint bearer. Deterministic genetic framing simplifies sentencing narratives and reduces accountability for systemic factors (poverty, segregation, over-policing). Coordination benefit: genetic attribution justifies technocratic interventions. But also constrained by the claim: growing skepticism of genetic determinism undermines the legitimacy of sentencing frameworks built on genetic risk. Asymmetric extraction toward this institution; some suppression of alternative explanatory frameworks.
constraint_indexing:constraint_classification(agg1_genetic_determinism, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: SKEPTIC COALITIONS (SCAFFOLD) — Organized agents (critical geneticists, civil rights organizations, neuroscientists emphasizing gene-environment interaction) view AGG-1 determinism as a temporary institutional arrangement. Replication failures, mechanism unknowns, and environmental factor prominence are creating alternative framings. Low effective extraction because coalition has agency, evidence-gathering capacity, and a clear sunset path: as epigenetics, gene-environment interaction, and polygenic models mature, deterministic single-gene narratives lose institutional authority. Estimated sunset: 15-25 years as genomic understanding advances.
constraint_indexing:constraint_classification(agg1_genetic_determinism, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: EUGENICS-ERA FRAMEWORKS (PITON) — Historical framing (genetic determinism of behavior across racialized populations) persists through institutional inertia despite substantial scientific degradation. The constraint maintains itself through funding mechanisms, textbook canonicity, and inter-institutional alignment (criminal justice, insurance, pharmaceutical) rather than through empirical force. Theater ratio is high: the claim's performative authority ('genes cause aggression') persists longer than its explanatory power.
constraint_indexing:constraint_classification(agg1_genetic_determinism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/universal view, the claim combines coordination (simplifying behavioral etiology for research purposes) with asymmetric extraction (toward institutional beneficiaries and against skeptics/labeled populations). The constraint's persistence despite replication failures and mechanistic unknowns reveals both genuine coordination challenge (behavior is complex) and genuine extraction dynamic (institutional interests in simple narratives). Not a mountain — the constraint depends on institutional enforcement, not natural law. The false summit candidate is rebranding determinism as 'genetic contribution' or 'risk factor,' which performs the same extractive function while appearing more nuanced.
constraint_indexing:constraint_classification(agg1_genetic_determinism, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(agg1_genetic_determinism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(agg1_genetic_determinism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(agg1_genetic_determinism, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(agg1_genetic_determinism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(agg1_genetic_determinism, TR),
    TR >= 0.70.

:- end_tests(agg1_genetic_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The AGG-1 determinism claim provides genuine simplification benefit (coordination) but substantially overstates empirical support, enabling institutional beneficiaries to capture authority and funding. The extractiveness reflects the gap between the warrant (weak replication evidence, no identified mechanism) and the institutional authority (high citation, policy incorporation, criminal justice use). The metric rises from 0.35 to 0.58 over 30 years as the institutional embedding deepens despite growing skeptical evidence. Suppression (0.62): Moderate-high. Significant barriers to skeptical discourse include: peer review capture by determinism advocates, funding concentration in determinism-aligned labs, publication bias against null results, and institutional incentives in criminal justice and pharma that benefit from simple genetic narratives. But suppression is not total — alternative frameworks (gene-environment, epigenetics, polygenic models) have publishing venues and some institutional footing. Theater ratio (0.68): High. The claim's performative authority in policy and legal contexts far exceeds its mechanistic grounding. Sentencing recommendations reference AGG-1 without clear evidence; pharmaceutical marketing invokes 'genetic aggression' despite unknown mechanism; textbook canonicity persists despite replication concerns. The theater has increased over the interval as institutional embedding has grown while empirical support has stalled.
 *
 * PERSPECTIVAL GAP:
 *   Six distinct perspectives on the same constraint: The labeled individual sees pure extraction (Snare) — immutable deterministic attribution with no escape. Racial/ethnic populations see snare dynamics with collective scale — determinism framing justifies systemic extraction through criminalization. The research establishment sees coordination (Rope) — simplifying behavioral complexity for research purposes while maintaining institutional authority. Pharma sees pure beneficiary coordination (Rope) — therapeutic market opportunities with arbitrage optionality. Criminal justice sees mixed coordination and extraction (Tangled Rope) — determinism simplifies sentencing but growing skepticism undermines legitimacy. Skeptics see a temporary institutional problem (Scaffold) — determinism claim is being systematically replaced by gene-environment and epigenetic models. Historical eugenics frameworks persist through institutional inertia (Piton) — the determinism claim maintains itself performatively rather than through empirical force. The analytical observer (Tangled Rope) recognizes both the genuine coordination problem (behavior is complex, institutions need simplified heuristics) and the genuine extraction dynamic (institutional beneficiaries maintain overstated claims). The perspectival gap is maximal: from Snare (victim) through Rope (beneficiary) to Scaffold (skeptic) to Piton (historical inertia). The false summit candidate is semantic rebranding: relabeling 'determinism' as 'genetic contribution' or 'risk factor' performs the same extractive institutional function while appearing more epistemically cautious.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position: who benefits from the claim, who bears costs, and what exit options each actor has. Genetically labeled individuals (d ≈ 0.95) experience maximum extraction because they cannot exit the genetic label and have no capacity to dispute it. Skeptics (d ≈ 0.70-0.80) experience high extraction through publication barriers and funding disadvantage but retain some agency through alternative-framework development. Research establishment (d ≈ 0.10-0.20) experiences negative extraction — the claim subsidizes their authority and funding. Criminal justice (d ≈ 0.35-0.45) experiences mixed extraction: benefits from determinism framing but constrained by growing skepticism that undermines legitimacy. The behavioral economics of institutional authority create asymmetric incentives: maintaining the determinism claim concentrates benefits (toward institutional beneficiaries) while dispersing costs (across skeptics and labeled populations). The sigmoid f(d) amplifies high-d experienced extraction while dampening low-d institutional benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The AGG-1 determinism constraint avoids the mandatrophy trap (false choice between 'pure coordination' and 'pure extraction') by recognizing the genuine mixed structure: behavior is legitimately complex and institutions do need heuristics (coordination function), AND institutional beneficiaries have incentives to maintain simplified narratives beyond empirical warrant (extraction function). Both aspects are real. The constraint is not misclassified as Rope when it should be Snare, nor vice versa — it genuinely occupies the tangled rope territory where both coordination and asymmetric extraction coexist. The resolution manifests in the perspectival gap: different observers (victim, beneficiary, skeptic, analyst) legitimately classify differently because they occupy genuinely different structural positions relative to the constraint. The Piton perspective (institutional inertia) reveals the mechanism by which extraction persists: the claim is maintained performatively (theater ratio ≥ 0.68) rather than through continuous empirical validation. The Scaffold perspective (skeptics building alternatives) reveals the sunset mechanism: as epigenetics and gene-environment models mature, determinism narratives lose authority not through refutation but through institutional replacement by superior frameworks. This is the pathway from extraction to obsolescence without requiring abstract denouncement of the original claim as 'false' — it simply becomes less useful institutionally.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    agg1_replication_status,
    'Does AGG-1 actually show significant association with measured aggression in independent replication cohorts, or is the original claim a false positive in an underpowered study?',
    'Meta-analysis of independent replication attempts; power calculations for original study; assessment of p-hacking vulnerability',
    'If replication fails: constraint degrades from Tangled Rope (mixed extraction/coordination) to Snare (pure extraction masquerading as science). If replication succeeds: classification may shift toward Rope (legitimate coordination mechanism).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(agg1_replication_status, empirical, 'Whether AGG-1 association with aggression replicates independently').

omega_variable(
    gene_environment_interaction_threshold,
    'What is the relative contribution of AGG-1 genetic variation vs. environmental factors (early trauma, poverty, social adversity, substance exposure) to measured aggression outcomes?',
    'Gene-environment interaction studies; longitudinal tracking of AGG-1 carriers across environmental conditions; mediation analysis separating genetic from contextual pathways',
    'If environmental contribution > 80%: framing shifts from ''genetic determinism'' to ''minor genetic risk factor,'' collapsing the extraction mechanism. If environmental < 30%: current determinism framing is robust. Most likely intermediate value (40-70% environmental) reveals the institutional choice to highlight genetic determinism as selection of one valid statistical decomposition over others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gene_environment_interaction_threshold, empirical, 'Relative contribution of AGG-1 vs. environmental factors to aggression').

omega_variable(
    mechanism_identification_completeness,
    'Is a clear molecular and cellular mechanism linking AGG-1 genetic variation to aggression-relevant neural circuits established, or does the claim remain a statistical association without mechanistic understanding?',
    'Structural biology of AGG-1 protein; functional genomics in relevant cell types; circuit-level neuroscience connecting AGG-1 expression to aggression-related behaviors in animal models',
    'If mechanism fails to materialize: the determinism claim is revealed as institutional commitment to association data without causal understanding — pure extraction without coordination. If mechanism is established: the claim gains legitimacy as coordination around genuine biological understanding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mechanism_identification_completeness, empirical, 'Whether clear biological mechanism links AGG-1 to aggression').

omega_variable(
    institutional_interest_circularity,
    'Is the persistence of AGG-1 determinism framing driven by genuine empirical evidence or by institutional incentive structures (pharmaceutical development, criminal justice simplification, grant concentration) that benefit from genetic determinism narratives?',
    'Citation network analysis tracking self-citation and circular reinforcement; funding flow analysis from pharmaceutical/criminal justice to behavioral genetics research; comparison of citation rates for AGG-1 papers vs. critical papers',
    'If institutional interests dominate: constraint is classified as Snare (pure extraction). If empirical evidence dominates: constraint is Rope or Tangled Rope depending on victim declaration.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_interest_circularity, preference, 'Whether AGG-1 persistence is driven by evidence or institutional interests').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(agg1_genetic_determinism, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(agg1_tr_t0, agg1_genetic_determinism, theater_ratio, 0, 0.42).
narrative_ontology:measurement(agg1_tr_t15, agg1_genetic_determinism, theater_ratio, 15, 0.58).
narrative_ontology:measurement(agg1_tr_t30, agg1_genetic_determinism, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(agg1_be_t0, agg1_genetic_determinism, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(agg1_be_t15, agg1_genetic_determinism, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(agg1_be_t30, agg1_genetic_determinism, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(agg1_genetic_determinism, information_standard).
narrative_ontology:affects_constraint(agg1_genetic_determinism, behavioral_genetics_reductionism).
narrative_ontology:affects_constraint(agg1_genetic_determinism, genetic_determinism_in_criminal_justice).
narrative_ontology:affects_constraint(agg1_genetic_determinism, pharmaceutical_behavioral_targeting).

% DUAL FORMULATION NOTE:
% AGG-1 genetic determinism decomposes into three distinct constraint stories: (1) the scientific claim itself (AGG-1 association with aggression, ε ≈ 0.42, high replication uncertainty), (2) the institutional embedding of the claim in criminal justice (determinism justifies sentencing frameworks, ε ≈ 0.65, extraction mechanism), (3) the pharmaceutical market enabled by genetic aggression framing (ε ≈ 0.48, coordination with drug development). The present story focuses on the institutional authority claim and its extraction mechanisms. Upstream scientific constraint has lower extractiveness (empirical uncertainty); downstream constraints have higher extractiveness (institutional embedding). All three are linked by network relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(agg1_genetic_determinism, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
