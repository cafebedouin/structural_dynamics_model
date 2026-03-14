% ============================================================================
% CONSTRAINT STORY: catalytic_pathway_research_portfolio
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catalytic_pathway_research_portfolio, []).

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
 *   constraint_id: catalytic_pathway_research_portfolio
 *   human_readable: Catalytic Pathway Research Portfolio Extraction and Coordination
 *   domain: chemical_research/resource_allocation
 *
 * SUMMARY:
 *   The catalytic pathway research portfolio system creates a structural
 *   tension between the legitimate need to allocate scarce research resources
 *   and the extractive mechanisms that concentrate funding and prestige
 *   within established research groups. This constraint operates at the
 *   intersection of resource allocation coordination and institutional
 *   gatekeeping, making it a canonical exemplar of Tangled Rope extraction.
 *   Research portfolio committees, funding agencies, and peer review systems
 *   all ostensibly serve coordination functions — distributing limited
 *   resources to maximize impact, filtering for quality, concentrating
 *   expertise in capable hands. Yet these same mechanisms simultaneously
 *   extract value from junior researchers through unpaid labor, suppress
 *   novel catalytic pathways through publication bias and peer review
 *   conservatism, and concentrate research autonomy and prestige
 *   asymmetrically. The constraint's theater_ratio (0.65) reflects that peer
 *   review for catalytic chemistry proposals is substantially performative:
 *   reviewers assess proposal clarity and team qualifications but cannot
 *   verify synthetic accessibility, experimental feasibility, or catalyst
 *   performance across diverse reaction conditions without significant
 *   experimental work. This gap allows publication bias (bias toward
 *   incremental improvements over established pathways, toward pathways
 *   aligned with reviewer expertise) to masquerade as merit-based filtering.
 *   The theater has increased over the measurement interval (0.48 → 0.71) as
 *   specialization has outpaced reviewer capacity and publication volume has
 *   grown faster than review resources.
 *
 * KEY AGENTS:
 *   - Funding Institutions: Primary beneficiary (institutional/arbitrage) — control research direction through portfolio design, achieve coordination goals (directing research toward priority problems), but also capture prestige and influence through grant-giving authority
 *   - Established Research Groups: Primary beneficiary (institutional/arbitrage) — receive disproportionate funding, attract junior talent, control publication pipelines and peer review; experience constraint as pure coordination enabling their research program
 *   - Junior Researchers: Primary victim (powerless/trapped) — face credential requirements, publication metrics, and funding dependency; cannot exit portfolio system without career penalty; bear extraction through labor-intensive grant applications, delayed funding, constrained autonomy
 *   - Underrepresented Research Communities: Secondary victim (moderate/constrained) — face gatekeeping bias in portfolio selection; can pursue alternative funding but at cost; experience extraction through resource scarcity and publication bias
 *   - Novel Catalytic Mechanisms: Abstract victim — innovative pathways filtered by peer review bias and publication gatekeeping; bear extraction through suppressed exploration
 *   - Open Research Coalition: Organized agents (organized/constrained) — preprint servers, open-access journals, crowdfunding platforms, institutional repositories building alternative research ecosystems with lower extraction barriers
 *   - Peer Review System: Institutional actor (institutional/arbitrage) — maintains performative grant and journal review ritual; sees own process as degraded (Piton perspective) but persists through inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements (portfolio scarcity, credential requirements, peer review gatekeeping) as immutable constraints of scientific research
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catalytic_pathway_research_portfolio, 0.52).
domain_priors:suppression_score(catalytic_pathway_research_portfolio, 0.58).
domain_priors:theater_ratio(catalytic_pathway_research_portfolio, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catalytic_pathway_research_portfolio, extractiveness, 0.52).
narrative_ontology:constraint_metric(catalytic_pathway_research_portfolio, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(catalytic_pathway_research_portfolio, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catalytic_pathway_research_portfolio, tangled_rope).
narrative_ontology:human_readable(catalytic_pathway_research_portfolio, "Catalytic Pathway Research Portfolio Extraction and Coordination").
narrative_ontology:topic_domain(catalytic_pathway_research_portfolio, "chemical_research/resource_allocation").

domain_priors:requires_active_enforcement(catalytic_pathway_research_portfolio).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catalytic_pathway_research_portfolio, funding_institutions).
narrative_ontology:constraint_beneficiary(catalytic_pathway_research_portfolio, established_research_groups).
narrative_ontology:constraint_victim(catalytic_pathway_research_portfolio, junior_researchers).
narrative_ontology:constraint_victim(catalytic_pathway_research_portfolio, underrepresented_research_communities).
narrative_ontology:constraint_victim(catalytic_pathway_research_portfolio, novel_catalytic_mechanisms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: JUNIOR RESEARCHER (SNARE) — Trapped by credential requirements, publication metrics, and funding dependency. Cannot exit the portfolio system without abandoning career prospects. Must pursue grant-defined pathways within established groups. Bears extraction through unpaid labor, delayed funding, and constrained research autonomy. Maximum experienced extraction with no exit option.
constraint_indexing:constraint_classification(catalytic_pathway_research_portfolio, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EMERGING RESEARCH GROUP (TANGLED ROPE) — Constrained by portfolio gatekeeping and peer review bias, but benefits from network effects and collaborative access to established infrastructure. Can exit through independent funding or alternative institutions, but at significant cost. Experiences both coordination benefits (shared methods, collaborative access) and extraction (resource scarcity, publication bias against novel pathways).
constraint_indexing:constraint_classification(catalytic_pathway_research_portfolio, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ESTABLISHED RESEARCH GROUP (ROPE) — Primary beneficiary with full arbitrage options. Receives disproportionate funding, attracts junior talent, and controls journal editorial boards and peer review. Experiences the portfolio system as pure coordination: distributing grant funds to subordinates, publishing their work, and reinforcing group reputation. Net beneficiary — extraction flows toward this institutional actor.
constraint_indexing:constraint_classification(catalytic_pathway_research_portfolio, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN RESEARCH COALITION (SCAFFOLD) — Organized advocates (preprint servers, open-access journals, data repositories, crowdfunded research) see the portfolio extraction as a temporary coordination problem with a real sunset. Alternative funding models (citizen science, blockchain-based collaboration, institutional repositories) are building parallel research ecosystems with lower extraction barriers. Sunset clause: decentralized funding and verification mechanisms maturing over 15-20 years.
constraint_indexing:constraint_classification(catalytic_pathway_research_portfolio, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PEER REVIEW RITUAL (PITON) — Traditional grant peer review and journal review for catalytic chemistry is substantially performative. Reviewers assess proposal quality and novelty but cannot verify experimental protocols, synthetic accessibility, or catalyst performance in downstream applications without significant resources. The review theater persists through institutional inertia: funding agencies and journals maintain the ritual despite knowing many novel pathways are filtered by publication bias rather than actual validity. Theater ratio (0.65) reflects partial degradation of review function.
constraint_indexing:constraint_classification(catalytic_pathway_research_portfolio, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, research prioritization necessarily requires some gatekeeping: not all catalytic pathways can be pursued simultaneously, and resource constraints always create scarcity. This perspective frames the extraction as an inevitable consequence of limited funding, treating portfolio selection as a natural law of resource allocation. However, structural data contradicts this classification — the extraction mechanisms are institutional choices (peer review bias, publication gate-keeping, credential requirements), not immutable constraints. The engine will identify this as a false summit, revealing that 'necessary scarcity' naturalizes contingent institutional arrangements.
constraint_indexing:constraint_classification(catalytic_pathway_research_portfolio, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catalytic_pathway_research_portfolio_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(catalytic_pathway_research_portfolio, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(catalytic_pathway_research_portfolio, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(catalytic_pathway_research_portfolio, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(catalytic_pathway_research_portfolio, TR),
    TR >= 0.70.

:- end_tests(catalytic_pathway_research_portfolio_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The portfolio system extracts significant value from junior researchers through credential requirements, publication delays, and autonomy constraints, but the extraction is not maximal because established groups do provide genuine coordination benefits — infrastructure access, mentorship, collaborative opportunities. Much of the extraction is reframed as 'career investment' or 'training,' creating narrative cover. The value (0.52 vs. original 0.65) reflects that genuine coordination coexists with extraction. Suppression (0.58): Moderate-high. Significant barriers include economic dependency on established group funding, credential requirements for independent funding, publication bias filtering novel work, and career risk of pursuing non-consensus research directions. But suppression is not total — some junior researchers do exit, some novel pathways do get published, and alternative funding mechanisms are emerging. Suppression reflects the real constraints junior researchers face, not total coercion. Theater ratio (0.65): Moderate-high. Peer review for catalytic research proposals is substantially performative because reviewers cannot verify synthetic accessibility and downstream performance without substantial experimental work. The theater has increased over time (0.48 → 0.71) as: (1) specialization has grown and reviewer expertise has become less universal, (2) proposal complexity has increased beyond review capacity, (3) novel catalytic pathways have become harder to evaluate against reviewer background knowledge, (4) publication volume has grown and review funding has not kept pace. The theater rise indicates degradation of review function over time, suggesting transition toward Piton characteristics if the trend continues.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits the full range of DR classification types from different structural positions within the same research portfolio system. Established research groups (institutional/arbitrage) perceive pure coordination (Rope) — the portfolio system allocates resources efficiently and enables their research. The open research coalition (organized/constrained) perceives a temporary problem with an emerging sunset (Scaffold) — alternative funding mechanisms and open publication are building exit pathways that will eventually bypass traditional gatekeeping. The peer review system (institutional/arbitrage but degraded) perceives its own ritual as performative (Piton) — reviewers know their role is increasingly theater, but the institutional structure maintains it through inertia. Emerging research groups (moderate/constrained) perceive genuine mixed experience (Tangled Rope) — they benefit from portfolio infrastructure while simultaneously suppressed by gatekeeping bias. Junior researchers (powerless/trapped) perceive pure extraction (Snare) — they bear the full cost of portfolio mechanisms while gaining minimal autonomy. The civilizational analytical observer risks perceiving immutable natural law (Mountain) — 'research always requires prioritization, scarcity is inevitable' — but structural data reveals this as naturalization of contingent institutional arrangements. The perspectival gap is maximal: beneficiaries see Rope, victims see Snare, organized agents see Scaffold, degraded institutions see Piton, mixed agents see Tangled Rope, false naturalization sees Mountain. This divergence is diagnostic: the constraint's legitimacy depends on which perspective is granted authority.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from power level, exit options, and structural position in the extraction flow. Funding institutions and established groups (beneficiaries + arbitrage) derive low d (0.05-0.15), producing minimal or negative effective extraction from their position. Junior researchers (victims + trapped) derive high d (0.90-0.95), producing maximum extraction — they cannot exit, must participate, and bear the constraint's costs. Emerging groups (mixed: constrained exit + both benefits and costs) derive moderate d (0.50-0.60), producing medium extraction. Open research coalition (organized + constrained) derives lower d (0.35-0.45) because organization enables agency and alternative pathways are visible. The dispersed piton actor (institutional + arbitrage but degraded function) derives low d because it has no victims who are forced to participate — the peer review ritual is maintained through inertia, not extraction. The analytical observer's mountain perspective is based on false naturalization of contingent institutional choices, so its directionality would be derived from observation position (0.72-0.75 analytical canonical), revealing the false summit when compared against empirical structural data.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVES MANDATROPHY: The constraint's claimed_type (Tangled Rope) correctly identifies that it functions as both a coordination mechanism and an extraction mechanism. The portfolio system genuinely coordinates scarce research resources (coordination function: 'research needs prioritization'), AND it simultaneously extracts value from junior researchers through gatekeeping and credential requirements (extraction function: 'established groups capture prestige and autonomy'). Both functions are structural, not contingent. The mandatrophy is resolved by recognizing that Tangled Rope is the analytically correct classification from the system-level view: beneficiaries see Rope (pure coordination), victims see Snare (pure extraction), but the actual mechanism is hybrid. The engine must flag this as Tangled Rope to prevent mislabeling it either as 'legitimate resource allocation' (Rope, missing extraction) or 'pure gatekeeping' (Snare, missing coordination). The false summit (Mountain) risk is real: the constraint uses natural-law framing ('research requires prioritization') to justify institutional choices. The open research coalition's Scaffold perspective is structural, not aspirational — decentralized alternatives are demonstrably reducing extraction barriers, making the sunset clause analytically defensible rather than wishful. The Piton classification of peer review is correct: the ritual function has degraded as reviewer capacity has failed to keep pace with specialization, but the institutional structure persists through inertia rather than efficacy. This multi-type perspectival landscape is diagnostic of a system in transition: the coordination function is real (justifying Rope-like elements), the extraction is real (justifying Snare-like elements from victims' perspective), and the theatrical degradation is real (Piton elements), while alternative pathways are emerging (Scaffold elements becoming concrete). Claiming any single type would miss the structural reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    review_bias_versus_merit,
    'How much of the portfolio extraction results from genuine merit-based filtering versus peer review bias against novel catalytic mechanisms?',
    'Longitudinal analysis of rejected catalytic proposals: correlation between rejection rationale and eventual publication success elsewhere; bias detection in reviewer comments; comparison of rejection rates for novel versus incremental pathways controlled for proposal quality metrics',
    'If primarily merit-based: extraction is justified coordination, reclassify closer to Rope. If significantly bias-driven: extraction is unjustified gatekeeping, reclassify toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(review_bias_versus_merit, empirical, 'Distinction between merit-based filtering and peer review bias in portfolio selection').

omega_variable(
    alternative_catalyst_viability,
    'Are rejected novel catalytic pathways structurally viable but portfolio-filtered, or genuinely less viable than selected pathways?',
    'Retrospective validation: track rejected catalytic approaches that were eventually pursued by other groups; measure success rates (publication, patenting, industrial adoption) for in-portfolio versus out-of-portfolio research; identify systematic differences in downstream application success',
    'If rejected pathways are viable: portfolio gate-keeping extracts innovation value unjustly (Snare mechanism stronger). If genuinely less viable: portfolio selection serves legitimate resource optimization (Rope mechanism stronger).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_catalyst_viability, empirical, 'Viability of rejected novel catalytic pathways pursued elsewhere').

omega_variable(
    junior_researcher_exit_cost,
    'What is the actual exit cost for junior researchers leaving established research groups to pursue independent catalytic research?',
    'Career trajectory analysis: compare funding, publication rates, and career advancement for junior researchers who exit versus those who remain; measure cost of credential deficit and network loss; track success rates of independent research programs launched by exiting juniors',
    'If exit cost is low (< 20% career penalty): exit option is ''constrained,'' reclassify some junior perspectives toward Tangled Rope. If exit cost is high (> 50% penalty): exit is effectively impossible, reinforce Snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(junior_researcher_exit_cost, empirical, 'Cost of junior researcher exit from established research groups').

omega_variable(
    open_research_timeline,
    'What is the realistic timeline for decentralized funding mechanisms (crowdfunding, blockchain, institutional repositories) to achieve sufficient scale to bypass traditional research portfolio gatekeeping?',
    'Trend analysis of alternative funding mechanisms: growth rates in citizen science projects, preprint citation impact, institutional data repository usage, blockchain-based research funding; capability comparison with traditional peer review; barrier identification for scaling alternatives',
    'If timeline < 10 years: scaffold sunset is imminent, structural change is underway. If timeline > 25 years: scaffold is aspirational, current extraction will persist across the measurement interval.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(open_research_timeline, empirical, 'Timeline for alternative funding mechanisms to achieve critical mass').

omega_variable(
    suppression_mechanism_type,
    'Is the measured suppression (0.58) primarily structural (economic dependency, institutional barriers) or internalized (junior researchers have internalized the portfolio narrative as legitimate)?',
    'Qualitative analysis: interviews with junior researchers on barriers; exit trajectory analysis after group departure; identification of persistent suppression post-exit (indication of internalized mechanism); comparison of suppression levels in research communities with versus without alternative funding access',
    'If primarily structural: suppression dissolves when barriers are removed, exit enables autonomy. If internalized: junior researchers carry suppression forward, extraction mechanism more durable than structural barriers suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_type, empirical, 'Whether measured suppression is structural or internalized in junior researchers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catalytic_pathway_research_portfolio, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cprp_tr_t0, catalytic_pathway_research_portfolio, theater_ratio, 0, 0.48).
narrative_ontology:measurement(cprp_tr_t3, catalytic_pathway_research_portfolio, theater_ratio, 3, 0.56).
narrative_ontology:measurement(cprp_tr_t6, catalytic_pathway_research_portfolio, theater_ratio, 6, 0.65).
narrative_ontology:measurement(cprp_tr_t9, catalytic_pathway_research_portfolio, theater_ratio, 9, 0.71).

% Extraction over time
narrative_ontology:measurement(cprp_be_t0, catalytic_pathway_research_portfolio, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cprp_be_t3, catalytic_pathway_research_portfolio, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(cprp_be_t6, catalytic_pathway_research_portfolio, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(cprp_be_t9, catalytic_pathway_research_portfolio, base_extractiveness, 9, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catalytic_pathway_research_portfolio, resource_allocation).
narrative_ontology:affects_constraint(catalytic_pathway_research_portfolio, catalytic_innovation_suppression).
narrative_ontology:affects_constraint(catalytic_pathway_research_portfolio, research_credential_lock_in).
narrative_ontology:affects_constraint(catalytic_pathway_research_portfolio, publication_bias_chemistry).

% DUAL FORMULATION NOTE:
% The catalytic pathway research portfolio is a hybrid coordination-extraction mechanism. Three related but structurally distinct constraints feed into it: (1) catalytic innovation suppression (ε~0.62, Snare) — novel mechanisms filtered by review bias; (2) research credential lock-in (ε~0.58, Tangled Rope) — credentials concentrate within established groups; (3) publication bias in chemistry (ε~0.48, Tangled Rope) — incrementalism bias in journal acceptance. The portfolio constraint integrates these three downstream constraints. It affects research direction across chemistry broadly and has secondary effects on adjacent fields (materials science, biochemistry, catalytic engineering) through shared research culture and journal systems.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catalytic_pathway_research_portfolio, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
