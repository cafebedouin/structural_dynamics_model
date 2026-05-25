% ============================================================================
% CONSTRAINT STORY: creator_labor_classification_ambiguity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_creator_labor_classification_ambiguity, []).

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
 *   constraint_id: creator_labor_classification_ambiguity
 *   human_readable: Creator Labor Classification Ambiguity
 *   domain: economic/labor/digital_platforms
 *
 * SUMMARY:
 *   The classification of creator labor on digital platforms exists in
 *   structural ambiguity: creators are formally independent contractors, yet
 *   platforms exercise algorithmic control over task assignment, earnings,
 *   content moderation, and account suspension. This ambiguity is not
 *   accidental — it is actively maintained because it enables platforms to
 *   access labor cost reductions (no benefits, no wage floors, no collective
 *   bargaining) while avoiding labor law obligations. The constraint creates
 *   asymmetric extraction coexisting with genuine coordination: platforms
 *   solve the real problem of connecting creators to audiences efficiently,
 *   yet simultaneously extract value through information asymmetry, network
 *   effects, and suppression of creator collective organizing. The
 *   extractiveness has risen over the measurement interval as platforms have
 *   tightened algorithmic control while maintaining the independent
 *   contractor classification. The theater ratio has also risen as platforms
 *   engage in increasingly elaborate framing narratives ('creators are
 *   entrepreneurs,' 'the algorithm is neutral,' 'flexibility is the creator
 *   benefit') to justify the classification and suppress regulatory
 *   intervention.
 *
 * KEY AGENTS:
 *   - Content Creators: Primary victim (powerless/trapped) — classified as independent contractors to evade labor protections; algorithmic dependency creates material lock-in despite formal independence
 *   - Creator Coalitions: Secondary victim (moderate/constrained) — organized groups with some bargaining power but multi-platform dependency and audience fragmentation limit leverage
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — benefit from classification ambiguity enabling flexible labor cost management; can exit to new classification schemes
 *   - Regulatory Reform Coalition: Secondary actor (organized/constrained) — labor boards, courts, and reform advocates pushing toward classification clarity; see constraint as temporary scaffold
 *   - Labor Law System: Institutional observer (institutional/arbitrage) — traditional employment law increasingly performative when applied to algorithmic labor relationships
 *   - Analytical Observer: Cross-position analysis (analytical/analytical) — identifies simultaneous coordination function and extraction asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(creator_labor_classification_ambiguity, 0.58).
domain_priors:suppression_score(creator_labor_classification_ambiguity, 0.62).
domain_priors:theater_ratio(creator_labor_classification_ambiguity, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(creator_labor_classification_ambiguity, extractiveness, 0.58).
narrative_ontology:constraint_metric(creator_labor_classification_ambiguity, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(creator_labor_classification_ambiguity, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(creator_labor_classification_ambiguity, tangled_rope).
narrative_ontology:human_readable(creator_labor_classification_ambiguity, "Creator Labor Classification Ambiguity").
narrative_ontology:topic_domain(creator_labor_classification_ambiguity, "economic/labor/digital_platforms").

domain_priors:requires_active_enforcement(creator_labor_classification_ambiguity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(creator_labor_classification_ambiguity, platform_operators).
narrative_ontology:constraint_beneficiary(creator_labor_classification_ambiguity, content_aggregators).
narrative_ontology:constraint_victim(creator_labor_classification_ambiguity, content_creators).
narrative_ontology:constraint_victim(creator_labor_classification_ambiguity, labor_protections).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRECARIOUS CREATOR (SNARE) — Classified as independent contractor to evade labor law protections. Cannot exit without abandoning audience, income stream, and algorithmic visibility. Algorithmic dependency creates material capture despite formal independence status. Suppression is structural: no collective bargaining, no wage floors, no benefits, no dispute resolution. Maximum experienced extraction.
constraint_indexing:constraint_classification(creator_labor_classification_ambiguity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ORGANIZED CREATOR COALITION (TANGLED ROPE) — Coalition of moderately successful creators with some bargaining power (audience capture, portfolio leverage). Constrained by multi-platform dependency and audience fragmentation. Genuine coordination benefit (platform reaches audience efficiently) coexists with asymmetric extraction (revenue share, algorithm opacity, data collection). Some agency to negotiate but not equivalent to employee-level protections.
constraint_indexing:constraint_classification(creator_labor_classification_ambiguity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Experiences the constraint as coordination: connecting creators to audiences solves a genuine matching problem. Arbitrage exit (can switch creator bases, shift revenue models, reshape classification). Net beneficiary — classification ambiguity is functional feature enabling flexible labor cost management. Sees constraint as necessary coordination infrastructure.
constraint_indexing:constraint_classification(creator_labor_classification_ambiguity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY REFORM COALITION (SCAFFOLD) — Labor boards, courts, and gig-economy reform advocates. Sunset clause: classification clarification through regulation (workers classification tests, portable benefits, collective bargaining rights). Theater ratio declining as regulatory frameworks (e.g., AB5 in California, pending EU DSA provisions) reduce classification ambiguity. Constraint seen as temporary until legal clarity solidifies.
constraint_indexing:constraint_classification(creator_labor_classification_ambiguity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LABOR LAW SYSTEM (PITON) — Traditional employment law categories (employee vs. contractor) are increasingly performative when applied to platform creators. The legal framework persists through institutional inertia despite structural mismatch with algorithmic labor. Courts engage in theatrical application of outdated categories rather than genuine classification work. The constraint's function (protecting labor) has atrophied; the formal process continues.
constraint_indexing:constraint_classification(creator_labor_classification_ambiguity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees genuine coordination function (platforms efficiently match creators to audiences) coexisting with structural extraction (algorithmic control, data capture, revenue asymmetry, suppression of worker organizing). Classification ambiguity is not accidental — it is actively maintained because it enables asymmetric extraction while preserving platform-as-neutral-infrastructure framing. Constraint exhibits both real coordination and active extraction asymmetry.
constraint_indexing:constraint_classification(creator_labor_classification_ambiguity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(creator_labor_classification_ambiguity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(creator_labor_classification_ambiguity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(creator_labor_classification_ambiguity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(creator_labor_classification_ambiguity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(creator_labor_classification_ambiguity, TR),
    TR >= 0.70.

:- end_tests(creator_labor_classification_ambiguity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. Platforms extract through revenue share (30-50%), data collection, algorithmic control enabling selective demonetization, and ability to alter creator terms unilaterally. However, extraction is not maximal because some creators do generate substantial income and genuine audience reach. The rise from 0.35 to 0.58 over the interval reflects increasing platform control: early platforms offered genuine optionality and audience reach; current platforms have tightened algorithmic control while maintaining contractor classification. Suppression (0.62): Moderately high. Structural barriers include algorithmic opacity (creators cannot understand demonetization), limited dispute resolution (no appeals process), platform terms-of-service unilateralism, suppression of organizing (platforms ban creator unions, demonetize unionizing accounts). Suppression is substantial but not total — creators can exit (though at high cost) and have organized in some jurisdictions. Theater ratio (0.68): High. Platform rhetoric about 'creator empowerment,' 'algorithm neutrality,' and 'entrepreneur flexibility' is increasingly performative as algorithmic control has tightened. The theater serves to naturalize the classification ambiguity and frame it as benefit rather than extraction mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The precarious creator experience (snare) versus platform operator experience (rope) represents the fundamental perspectival gap. The platform honestly experiences the constraint as coordination — they are solving a real matching problem and the constraint enables this coordination to happen. The precarious creator honestly experiences the constraint as extraction — they cannot exit and bear the extraction cost. Both perceptions are structurally correct. The gap is not one of perception error but of genuine structural asymmetry: the coordination benefit accrues differentially, and the extraction burden falls entirely on powerless agents. The organized coalition and regulatory observers see the ambiguity itself as the mechanism — maintaining classification opacity is what enables simultaneous coordination and extraction claims. The labor law system's piton perspective reveals that traditional legal categories have become ornamental: the system continues to process employment classification questions but does so performatively, unable to capture the specificity of algorithmic labor relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values vary sharply across perspectives based on beneficiary/victim status and exit options. Precarious creators (powerless/trapped) derive d ≈ 0.95 from victim status + trapped exit → high f(d) → high chi. Organized coalitions (moderate/constrained) derive d ≈ 0.55 from victim status + constrained exit → moderate f(d). Platform operators (institutional/arbitrage) derive d ≈ 0.10 from beneficiary status + arbitrage exit → low f(d) → negative or near-zero chi. The regulatory coalition (organized/constrained) derives d ≈ 0.45 from victim status (defending labor law) + constrained exit (regulatory process is slow). The analytical observer (analytical/analytical) derives d ≈ 0.72 from structural analyst position revealing asymmetry. Each perspective's experienced extraction chi reflects these directionality values, producing the full perspectival range from snare (high chi) to rope (low chi).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the tension between coordination and extraction by distinguishing the levels at which each operates. Platform-as-audience-distribution is a genuine coordination function (solves creator-audience matching with real value generation). Platform-as-labor-controller creates asymmetric extraction (algorithmic control, revenue asymmetry, suppression). These are not contradictory — they coexist in the same constraint. Classification ambiguity enables both to persist simultaneously: the platform claims the coordination justifies independence status (avoiding labor law), while exercising extraction-enabling control. The tangled rope classification is stable precisely because the coordination is real, not rhetorical. This distinguishes the constraint from a pure snare (where coordination would be absent or purely performative). The Goodhart drift in theater_ratio reflects that platform rhetoric about creator empowerment has become increasingly disconnected from actual algorithmic control patterns — the coordination narrative is doing more work to justify the extraction as time passes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithm_control_threshold,
    'At what level of algorithmic control does a platform cease being a neutral distribution channel and become an employer for labor law purposes?',
    'Comparative analysis of platform algorithmic control (recommendation systems, demonetization criteria, account suspension triggers, content moderation automation) vs. traditional employer control mechanisms (supervision, scheduling, task assignment, performance metrics). Legal precedent analysis across jurisdictions (AB5 control test, UK worker tests, EU DSA provisions).',
    'If control threshold is crossed: reclassify as Snare from creator perspective and Tangled Rope as institutional constraint. If threshold is not crossed: reclassifies as Rope (pure coordination) and suppression drops to 0.35. This determines whether platforms have labor law obligations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithm_control_threshold, empirical, 'Algorithmic control threshold for labor classification').

omega_variable(
    exit_option_optionality,
    'Are alternative platforms and income streams genuinely available to creators, or is the ''arbitrage'' exit option illusory due to network effects and algorithmic lock-in?',
    'Empirical measurement of creator multi-platform dependency, switching costs, audience portability, algorithm opacity preventing creator migration. Survey of creators attempting platform switching and measuring audience/income loss.',
    'If exit is genuine (true arbitrage): creators move from ''trapped'' to ''constrained'' → snare shifts toward tangled rope, suppression drops to 0.45. If exit is illusory: suppression rises to 0.75+ → stronger snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_option_optionality, empirical, 'Whether creator exit options are genuinely available').

omega_variable(
    revenue_share_coordination_or_extraction,
    'Is the platform''s revenue share (typically 30-50%) a market-clearing coordination fee or extractive rent enabled by information asymmetry and network effects?',
    'Comparative analysis of platform revenue share vs. transaction costs (payment processing, fraud prevention, hosting, moderation labor). Comparison across platforms and jurisdictions with different competitive structures. Empirical test: does creator revenue share decline when platform faces labor supply shortage or regulatory pressure?',
    'If fee is coordination cost: extractiveness drops to 0.35, classification shifts toward Rope. If fee is rent: extractiveness rises to 0.65+, snare classification strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revenue_share_coordination_or_extraction, empirical, 'Whether revenue share reflects coordination or extraction').

omega_variable(
    suppression_mechanism_internalization,
    'Is creator suppression (lack of collective voice, algorithmic opacity, limited dispute resolution) structural (platform-enforced) or substantially internalized (creators self-censor, accept opacity, internalize platform''s framing of independence)?',
    'Ethnographic analysis of creator perception of platform control. Historical comparison of creator behavior before/after platform policy changes. Measurement of creator organizing attempts and platform response. Post-exit suppression persistence: do creators who leave platforms retain internalized self-suppression?',
    'If largely structural: suppression value of 0.62 is accurate measure of exit cost. If substantially internalized: effective suppression is higher than structural measure suggests; creator remains locked even after platform exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized').

omega_variable(
    regulatory_sunset_viability,
    'Will legal classification clarification (via regulation or court precedent) actually reduce platform extraction, or will platforms shift to new ambiguous categories (automation, algorithmic assignment, new platform models)?',
    'Historical analysis of how platforms responded to prior labor regulation (contractor vs. employee classification in ride-sharing, gig work). Observation of whether platform model innovation follows regulatory constraint or circumvents it. Monitoring of emerging platform categories (DAO-based, blockchain-based, AI-agent-mediated) and whether they reintroduce classification ambiguity.',
    'If regulation is durable: scaffold sunset is real and theater_ratio declines. If platforms circumvent: scaffold is aspirational; constraint persists in new forms; theater_ratio rises.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_sunset_viability, empirical, 'Whether regulatory clarification will enduringly reduce extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(creator_labor_classification_ambiguity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(creator_tr_t0, creator_labor_classification_ambiguity, theater_ratio, 0, 0.55).
narrative_ontology:measurement(creator_tr_t5, creator_labor_classification_ambiguity, theater_ratio, 5, 0.62).
narrative_ontology:measurement(creator_tr_t10, creator_labor_classification_ambiguity, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(creator_be_t0, creator_labor_classification_ambiguity, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(creator_be_t5, creator_labor_classification_ambiguity, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(creator_be_t10, creator_labor_classification_ambiguity, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(creator_labor_classification_ambiguity, resource_allocation).
narrative_ontology:affects_constraint(creator_labor_classification_ambiguity, algorithmic_labor_opacity).
narrative_ontology:affects_constraint(creator_labor_classification_ambiguity, platform_data_extraction).
narrative_ontology:affects_constraint(creator_labor_classification_ambiguity, creator_organizing_suppression).

% DUAL FORMULATION NOTE:
% Creator labor classification ambiguity decomposes into three structurally distinct constraints: (1) algorithmic control mechanisms that create functional employment without legal recognition (this story, ε=0.58); (2) data extraction from creator activity enabling surveillance capitalism (ε=0.72, separate story); (3) platform suppression of creator collective organizing (ε=0.68, separate story). Each has different extractiveness because the observables differ: labor classification ambiguity is measured by regulatory status and control mechanisms; data extraction is measured by value of creator behavioral data; organizing suppression is measured by platform enforcement against unionizing activity. All three affect each other: data extraction justifies classification ambiguity (platforms need behavioral data to operate recommendation algorithms); organizing suppression maintains classification ambiguity (organizing could force reclassification). Link all three via network.affects_constraints to enable contamination propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(creator_labor_classification_ambiguity, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
