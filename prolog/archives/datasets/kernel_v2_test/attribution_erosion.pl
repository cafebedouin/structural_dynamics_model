% ============================================================================
% CONSTRAINT STORY: attribution_erosion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_attribution_erosion, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: attribution_erosion
 *   human_readable: Attribution Erosion in AI-Generated Mathematical Content
 *   domain: science_policy/professional_ethics/technology_governance
 *
 * SUMMARY:
 *   The attribution erosion constraint emerges from the structural tension
 *   between AI developers' need for large training corpora and the academic
 *   norm of citation-based credit allocation. Large language models trained
 *   on mathematical literature (arXiv papers, textbooks, MathOverflow posts,
 *   published proofs) generate outputs that synthesize training data without
 *   preserving attribution metadata. This creates asymmetric extraction: AI
 *   developers capture value (commercial products, research acceleration,
 *   reduced licensing costs) while mathematical authors bear costs (citation
 *   erasure, career impact from lost credit, norm degradation). The
 *   constraint exhibits genuine coordination function — AI tools democratize
 *   access to mathematical knowledge, enable proof assistance, reduce
 *   barriers to entry for students — making this a definitional tangled rope
 *   rather than pure extraction. The theater_ratio (0.48) reflects that
 *   copyright enforcement mechanisms have moderate performative content:
 *   high-profile lawsuits create some deterrent effect, but core verification
 *   and attribution mechanisms cannot scale to model inference. Suppression
 *   has increased over the interval as AI adoption has made opt-out
 *   increasingly costly: authors who refuse to publish openly (to avoid
 *   training corpus inclusion) sacrifice academic visibility and career
 *   advancement.
 *
 * KEY AGENTS:
 *   - Mathematical Authors: Primary victims (powerless/trapped) — work used without credit, no exit from training ecosystem once published, career harm from citation erasure
 *   - AI Developers: Primary beneficiaries (institutional/arbitrage) — capture value through reduced licensing costs and expanded training corpus, can arbitrage jurisdictions
 *   - AI Platform Operators: Primary beneficiaries (institutional/arbitrage) — monetize models trained on commons, experience constraint as pure coordination
 *   - Downstream Researchers: Mixed position (moderate/constrained) — benefit from AI tools but victimized by citation trail corruption, can verify manually at cost
 *   - Academic Publishers: Mixed position (institutional/constrained) — benefit from AI processing tools, victimized by training corpus extraction undermining subscription models
 *   - Open Mathematics Coalition: Organized agents (organized/mobile) — building technical attribution standards, see current extraction as solvable coordination problem with technical sunset
 *   - Copyright Office: Institutional enforcer (institutional/constrained) — enforcement mechanisms largely performative for AI use cases, ritual persists through inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees genuine coordination function coexisting with substantial asymmetric extraction, definitional tangled rope
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(attribution_erosion, 0.58).
domain_priors:suppression_score(attribution_erosion, 0.62).
domain_priors:theater_ratio(attribution_erosion, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(attribution_erosion, extractiveness, 0.58).
narrative_ontology:constraint_metric(attribution_erosion, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(attribution_erosion, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(attribution_erosion, tangled_rope).
narrative_ontology:human_readable(attribution_erosion, "Attribution Erosion in AI-Generated Mathematical Content").
narrative_ontology:topic_domain(attribution_erosion, "science_policy/professional_ethics/technology_governance").

domain_priors:requires_active_enforcement(attribution_erosion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(attribution_erosion, ai_developers).
narrative_ontology:constraint_beneficiary(attribution_erosion, ai_platform_operators).
narrative_ontology:constraint_beneficiary(attribution_erosion, end_users_seeking_efficiency).
narrative_ontology:constraint_victim(attribution_erosion, mathematical_authors).
narrative_ontology:constraint_victim(attribution_erosion, academic_attribution_norms).
narrative_ontology:constraint_victim(attribution_erosion, downstream_researchers).
narrative_ontology:constraint_vindicates(attribution_erosion, training_as_transformative_use_doctrine).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL MATHEMATICAL AUTHOR (SNARE) — Cannot exit the AI training ecosystem once work is published; no practical mechanism to enforce attribution when models generate derivative content. Faces career damage from citation erasure while bearing full cost of norm collapse. Maximum extraction: work is used without credit, alternatives (not publishing, paywalling) destroy academic viability.
constraint_indexing:constraint_classification(attribution_erosion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DOWNSTREAM RESEARCHER (TANGLED ROPE) — Benefits from AI-assisted literature review and proof assistance but also victimized by citation trail corruption. Can verify sources manually (constrained exit from full AI dependence) but at significant time cost. Experiences both coordination (faster access to mathematical knowledge) and extraction (degraded citation reliability, difficulty tracing intellectual lineage).
constraint_indexing:constraint_classification(attribution_erosion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: AI PLATFORM OPERATOR (ROPE) — Primary beneficiary. Experiences the constraint as pure coordination: training on mathematical commons enables valuable service (proof assistance, theorem search, pedagogical tools). Can arbitrage between jurisdictions with different copyright regimes. Net beneficiary: extraction flows toward this agent through reduced licensing costs and expanded training corpus access.
constraint_indexing:constraint_classification(attribution_erosion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ACADEMIC PUBLISHER (TANGLED ROPE) — Mixed position. Benefits from AI tools that increase manuscript processing efficiency and plagiarism detection. Victimized by training corpus extraction that undermines subscription models and citation-based impact metrics. Can partially exit through licensing agreements with AI developers but faces collective action problem: individual publisher opt-outs don't prevent training on open-access corpus.
constraint_indexing:constraint_classification(attribution_erosion, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: OPEN MATHEMATICS COALITION (SCAFFOLD) — Organized agents (arXiv, MathOverflow, open-access advocates) see attribution erosion as a temporary coordination failure solvable through technical standards: cryptographic provenance tracking, citation-aware model architectures, and community-enforced attribution protocols. Mobile exit: can build alternative platforms with mandatory attribution. Views current extraction as transitional problem with technical sunset, not permanent structural feature.
constraint_indexing:constraint_classification(attribution_erosion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: COPYRIGHT OFFICE (PITON) — Traditional copyright enforcement mechanisms (DMCA takedowns, infringement litigation) are largely performative for AI training use cases. Cannot practically audit training corpora, cannot detect derivative generation, cannot enforce attribution at model inference time. The enforcement ritual persists through institutional inertia despite low functional effectiveness. Theater ratio is moderate rather than high because some enforcement actions (high-profile lawsuits) do create deterrent effects, but the core verification and attribution mechanisms have atrophied.
constraint_indexing:constraint_classification(attribution_erosion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, this constraint exhibits genuine coordination function (AI tools democratize access to mathematical knowledge, enable new proof techniques, reduce barriers to entry) AND asymmetric extraction (citation erasure concentrates career benefits with AI developers while distributing costs across mathematical authors and academic attribution norms). The coordination function is real and substantial, not merely cover. The extraction is also real and substantial, not merely coordination overhead. This is the definitional case for tangled rope: both functions coexist and neither can be eliminated without destroying the other.
constraint_indexing:constraint_classification(attribution_erosion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(attribution_erosion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(attribution_erosion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(attribution_erosion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(attribution_erosion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(attribution_erosion, TR),
    TR >= 0.70.

:- end_tests(attribution_erosion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Substantial. AI developers capture significant value (commercial products, research tools, reduced licensing costs) while mathematical authors bear costs (citation erasure, career impact, norm degradation). The extraction is real and measurable through licensing opt-out rates, copyright complaints, and citation trail corruption. However, extraction is not maximal (not 0.8+) because the coordination function is also real: AI tools genuinely democratize access and enable new research. Suppression (0.62): Moderate-high and increasing. Authors face significant barriers to exit: refusing to publish openly (to avoid training inclusion) sacrifices academic visibility and career advancement. Legal remedies are largely inaccessible (litigation costs exceed individual author resources, jurisdictional arbitrage by AI developers, transformative use doctrine ambiguity). Collective action is difficult (prisoner's dilemma: individual opt-outs don't prevent training on remaining corpus). Suppression has increased over the interval as AI adoption has made participation increasingly mandatory for career viability. Theater ratio (0.48): Moderate. Copyright enforcement has substantial performative content (DMCA takedowns cannot audit training corpora, infringement litigation cannot detect derivative generation at inference time, enforcement actions are selective and symbolic) but is not purely theatrical (high-profile lawsuits do create some deterrent effect, licensing agreements do constrain some developers). The theater has increased as the gap between enforcement ritual and actual attribution protection has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the tangled rope signature: different agents experience the same structural phenomenon as coordination, extraction, or both depending on their position. AI platform operators see pure coordination (Rope) — they are solving the legitimate problem of democratizing mathematical knowledge access. The open mathematics coalition sees a temporary coordination failure with technical sunset (Scaffold) — attribution-aware architectures and cryptographic provenance are building alternative pathways. Individual mathematical authors see pure extraction (Snare) — their work is used without credit and they have no exit. Downstream researchers and academic publishers see mixed coordination and extraction (Tangled Rope) — they benefit from AI tools while bearing costs from citation corruption and training corpus extraction. The copyright office sees its own degraded enforcement ritual (Piton) — mechanisms persist through inertia despite low functional effectiveness. The analytical observer sees the definitional tangled rope: both coordination and extraction are real, substantial, and structurally inseparable. The perspectival gap is not a measurement error — it is the constraint's actual structure as experienced from different positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Mathematical authors are declared victims with trapped exit → high d → high effective extraction. They cannot exit the training ecosystem once work is published, and alternatives (not publishing, paywalling) destroy academic viability. AI developers and platform operators are declared beneficiaries with arbitrage exit → low d → low or negative effective extraction (they experience net subsidy from the constraint). Downstream researchers are both beneficiaries (AI tools) and victims (citation corruption) with constrained exit → moderate d → moderate effective extraction. Academic publishers are similarly mixed with constrained exit. The open mathematics coalition has mobile exit (can build alternative platforms) → lower d than constrained agents. The copyright office has constrained exit (cannot abandon enforcement role but enforcement is largely ineffective) and is neither clear beneficiary nor victim → moderate d. The analytical observer uses analytical exit and sees the full structure: genuine coordination coexisting with asymmetric extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled rope classification resolves the mandatrophy by acknowledging that BOTH the coordination function (democratizing mathematical knowledge, enabling proof assistance, reducing barriers) AND the extraction mechanism (citation erasure, career harm, norm degradation) are real and substantial. This is not a rope with high overhead costs, nor a snare with a coordination cover story. The coordination function is genuine: AI tools do enable research that would otherwise be infeasible. The extraction is also genuine: mathematical authors do bear costs that AI developers do not compensate. The constraint cannot be decomposed into separate coordination and extraction stories because the same training process that enables the coordination function also causes the extraction. Removing the extraction (mandatory attribution, licensing requirements) would degrade the coordination function (model performance, training corpus size, inference speed). This is the structural signature of tangled rope: the coordination and extraction are coupled through the same mechanism and cannot be separated without destroying both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformative_use_boundary,
    'Does AI training on mathematical works constitute transformative use (fair use defense) or derivative work creation (copyright infringement)?',
    'Judicial precedent in major jurisdictions; legislative clarification of AI training exemptions; empirical analysis of model output similarity to training corpus',
    'If transformative: extraction is legally permissible coordination overhead, beneficiaries'' position is legitimate. If derivative: extraction is copyright violation, victims have legal remedy and constraint reclassifies toward snare from more perspectives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transformative_use_boundary, conceptual, 'Legal status of AI training as transformative use vs derivative work').

omega_variable(
    attribution_technical_feasibility,
    'Is it technically feasible to build citation-aware language models that reliably attribute generated mathematical content to training sources without catastrophic performance degradation?',
    'Experimental results from attribution-aware model architectures; performance benchmarks comparing standard vs citation-tracking models; analysis of computational overhead',
    'If feasible with <10% performance cost: scaffold perspective confirmed, technical sunset is real. If infeasible or >30% cost: attribution erosion is structural feature of current AI paradigm, not solvable coordination problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attribution_technical_feasibility, empirical, 'Technical feasibility of citation-aware model architectures').

omega_variable(
    norm_collapse_threshold,
    'At what adoption rate does AI-generated content without attribution cause irreversible collapse of academic citation norms in mathematics?',
    'Longitudinal analysis of citation practices in mathematics publications; survey data on researcher attitudes toward AI-generated content; measurement of citation trail integrity over time',
    'If threshold >50% adoption: current extraction levels are sustainable, norms adapt. If threshold <20% adoption: we are past the point of norm preservation, extraction has already caused permanent damage to attribution commons.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(norm_collapse_threshold, empirical, 'Adoption threshold for irreversible attribution norm collapse').

omega_variable(
    career_impact_asymmetry,
    'Does citation erasure cause measurable career harm to mathematical authors, or is academic reputation sufficiently robust to alternative recognition mechanisms?',
    'Econometric analysis of citation counts vs career outcomes (hiring, promotion, funding) in mathematics; comparison of pre-AI and post-AI career trajectories; survey data on hiring committee practices',
    'If measurable harm: extraction is career-damaging, victims'' claims are empirically grounded. If no measurable harm: extraction is primarily symbolic, actual career mechanisms are resilient to citation erosion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(career_impact_asymmetry, empirical, 'Measurability of career harm from citation erasure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(attribution_erosion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(attr_ero_theater_t0, attribution_erosion, theater_ratio, 0, 0.25).
narrative_ontology:measurement(attr_ero_theater_t2, attribution_erosion, theater_ratio, 2, 0.32).
narrative_ontology:measurement(attr_ero_theater_t4, attribution_erosion, theater_ratio, 4, 0.38).
narrative_ontology:measurement(attr_ero_theater_t6, attribution_erosion, theater_ratio, 6, 0.43).
narrative_ontology:measurement(attr_ero_theater_t8, attribution_erosion, theater_ratio, 8, 0.46).
narrative_ontology:measurement(attr_ero_theater_t10, attribution_erosion, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(attr_ero_extract_t0, attribution_erosion, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(attr_ero_extract_t2, attribution_erosion, base_extractiveness, 2, 0.28).
narrative_ontology:measurement(attr_ero_extract_t4, attribution_erosion, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(attr_ero_extract_t6, attribution_erosion, base_extractiveness, 6, 0.51).
narrative_ontology:measurement(attr_ero_extract_t8, attribution_erosion, base_extractiveness, 8, 0.56).
narrative_ontology:measurement(attr_ero_extract_t10, attribution_erosion, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(attr_ero_suppress_t0, attribution_erosion, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(attr_ero_suppress_t3, attribution_erosion, suppression_requirement, 3, 0.48).
narrative_ontology:measurement(attr_ero_suppress_t6, attribution_erosion, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(attr_ero_suppress_t10, attribution_erosion, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(attribution_erosion, information_standard).
narrative_ontology:affects_constraint(attribution_erosion, academic_credit_allocation).
narrative_ontology:affects_constraint(attribution_erosion, open_access_sustainability).
narrative_ontology:affects_constraint(attribution_erosion, ai_training_corpus_licensing).

% DUAL FORMULATION NOTE:
% Attribution erosion is one component of a larger constraint family around AI training on academic commons. Related constraints include: (1) academic_credit_allocation — the broader norm system that attribution erosion degrades; (2) open_access_sustainability — the economic model that training corpus extraction undermines; (3) ai_training_corpus_licensing — the legal framework that determines whether extraction is permissible. Each has its own extractiveness value reflecting different structural dynamics, but all are linked through the training corpus access question.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
