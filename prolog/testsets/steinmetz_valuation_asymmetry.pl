% ============================================================================
% CONSTRAINT STORY: steinmetz_valuation_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_steinmetz_valuation_asymmetry, []).

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
 *   constraint_id: steinmetz_valuation_asymmetry
 *   human_readable: The Steinmetz Chalk Mark (Knowledge Valuation Asymmetry)
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The Steinmetz constraint models the structural asymmetry between physical
 *   labor and specialized knowledge as economic value. The apocryphal story
 *   illustrates the mechanism: a factory breaks down, the specialist
 *   diagnoses the problem with a chalk mark on a motor, and charges $10,000
 *   (equivalent to $300,000+ today) — $1 for the chalk, $9,999 for knowing
 *   where to put it. The story encapsulates the extreme valuation gap:
 *   knowledge is nearly priceless when embedded in institutional context but
 *   worthless when extracted from it. The constraint operates through
 *   suppression mechanisms that prevent independent knowledge monetization:
 *   capital requirements for business formation, institutional lock-in of
 *   skills and credentials, information asymmetries favoring employers, and
 *   cultural narratives that naturalize the hierarchy as inherent to
 *   economics. Extractiveness has increased over 50 years as automation has
 *   eliminated mid-skill positions while credential requirements have
 *   expanded, narrowing the pathways to independent knowledge valuation.
 *   Theater ratio has increased as credentialing systems have proliferated
 *   despite declining signal fidelity, indicating performative gatekeeping.
 *
 * KEY AGENTS:
 *   - Knowledge Workers / Skilled Technicians: Primary victim (powerless/trapped, constrained/moderate) — bears the extraction of differential value; has limited independent monetization pathways; credentialed knowledge cannot be deployed outside institutional contexts
 *   - Capital Owners / Industrial Enterprises: Primary beneficiary (institutional/arbitrage) — benefits from coordination function; captures differential value; controls institutional context necessary for knowledge monetization
 *   - Labor Coalitions / Unions / Professional Guilds: Secondary actor (organized/constrained) — provides partial protection through collective action; enables wage-setting standards; but faces institutional suppression of alternative compensation models
 *   - Credentialing Institutions / Universities: Institutional gatekeeper (institutional/arbitrage) — performs performative filtering function (piton perspective); extracts rents through credential scarcity; maintains theater that justifies institutional lock-in
 *   - Open Source / Knowledge Commons Communities: Organized challenger (organized/mobile) — provides temporary scaffold alternative; but faces recapture by platform monopolies that recreate the valuation asymmetry in digital form
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional suppression mechanisms as inherent economic laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(steinmetz_valuation_asymmetry, 0.58).
domain_priors:suppression_score(steinmetz_valuation_asymmetry, 0.72).
domain_priors:theater_ratio(steinmetz_valuation_asymmetry, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(steinmetz_valuation_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(steinmetz_valuation_asymmetry, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(steinmetz_valuation_asymmetry, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(steinmetz_valuation_asymmetry, snare).
narrative_ontology:human_readable(steinmetz_valuation_asymmetry, "The Steinmetz Chalk Mark (Knowledge Valuation Asymmetry)").
narrative_ontology:topic_domain(steinmetz_valuation_asymmetry, "economic/technological").

domain_priors:requires_active_enforcement(steinmetz_valuation_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(steinmetz_valuation_asymmetry, knowledge_monopolists).
narrative_ontology:constraint_beneficiary(steinmetz_valuation_asymmetry, capital_owners).
narrative_ontology:constraint_victim(steinmetz_valuation_asymmetry, knowledge_workers).
narrative_ontology:constraint_victim(steinmetz_valuation_asymmetry, skilled_labor).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SKILLED KNOWLEDGE WORKER (SNARE) — Trapped by credential requirements, specialized skill depreciation, and inability to independently monetize knowledge. The constraint extracts the differential value between physical labor compensation and knowledge-derived value. Zero exit options: knowledge is worthless without institutional context, and employment is the only pathway to subsistence. Maximum extraction is experienced.
constraint_indexing:constraint_classification(steinmetz_valuation_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LABOR COALITION (TANGLED ROPE) — Organized labor benefits from coordination mechanisms (wage-setting, skill-based compensation standards) but bears extraction through institutional capacity ceilings and bargaining asymmetries. Has agency through collective action but faces suppression of alternative value-distribution models. Mixed experience: genuine coordination benefits exist alongside structural extraction.
constraint_indexing:constraint_classification(steinmetz_valuation_asymmetry, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CAPITAL OWNER / INDUSTRIAL ENTERPRISE (ROPE) — Benefits from the constraint as pure coordination: knowledge workers exist to solve specific problems efficiently. The constraint enables arbitrage between knowledge acquisition cost and value extraction. Sees the relationship as legitimate market coordination with minimal suppression cost — workers are paid, problems are solved, profit is captured. Low experienced extraction because the beneficiary structure is self-reinforcing.
constraint_indexing:constraint_classification(steinmetz_valuation_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MID-CAREER TECHNICIAN (SNARE) — Constrained but not trapped. Has acquired tacit knowledge and some reputation, enabling lateral mobility within institutional contexts. However, mobility is heavily suppressed: knowledge is institutionally owned, not personally owned. Attempting freelance or startup work faces barrier of needing capital, legal structures, and customer acquisition. Extraction is severe — differential value flows upward — but not absolute because some exit capacity exists through institutional reputation.
constraint_indexing:constraint_classification(steinmetz_valuation_asymmetry, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: OPEN SOURCE / KNOWLEDGE COMMONS COALITION (SCAFFOLD) — Organized alternative pathway (open-source projects, online communities, technical content creation) provides genuinely lower-extraction routes to knowledge monetization. However, this pathway has sunset logic: as platform monopolies (GitHub, Stack Overflow, YouTube) mature, they recreate the valuation asymmetry in digital form. The scaffold suppresses institutional knowledge extraction temporarily but exhibits degradation as it scales.
constraint_indexing:constraint_classification(steinmetz_valuation_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: CREDENTIALING INSTITUTION (PITON) — The university and professional credentialing system performs a gatekeeping function that is increasingly theatrical. Knowledge is widely available (online, open courses, communities) but credentialing ritual persists through institutional inertia: employers signal quality through degree requirements even when actual knowledge comes from work experience. Theater ratio is high because the credential's verification function has atrophied while its signaling function remains. The institution extracts rents from credential scarcity rather than from knowledge curation.
constraint_indexing:constraint_classification(steinmetz_valuation_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE SUMMIT VIEW (MOUNTAIN) — At the civilizational level, some information asymmetry appears irreducible: knowledge worker creates value through cognitive work; owner coordinates deployment; differential value exists as a natural feature of economic organization. However, the structural data reveals this as naturalization: the extreme asymmetry (10x-100x differential) is not inherent to the knowledge-capital relationship but to the suppression mechanisms (credential monopoly, institutional lock-in, capital-dependent startup costs) that prevent knowledge workers from independent monetization. This is a false summit.
constraint_indexing:constraint_classification(steinmetz_valuation_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(steinmetz_valuation_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(steinmetz_valuation_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(steinmetz_valuation_asymmetry, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(steinmetz_valuation_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(steinmetz_valuation_asymmetry, TR),
    TR >= 0.70.

:- end_tests(steinmetz_valuation_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts genuine differential value — knowledge workers produce value that significantly exceeds their compensation. The extraction is not absolute because: (1) workers do receive subsistence-plus compensation (not pure surplus extraction like slavery), (2) coordination benefits are real (institutional deployment enables knowledge monetization), (3) some worker segments (senior technicians, specialized consultants) negotiate closer to differential value. The 0.58 figure reflects the median across worker segments — powerless technicians face 0.85+, senior engineers face 0.35-0.45. Suppression (0.72): Very high. The constraint is maintained through multiple suppression layers: capital barriers to independent business formation, credential gatekeeping, institutional ownership of knowledge assets, information asymmetries in hiring, lack of transparent pricing mechanisms for knowledge, and cultural narratives naturalizing the hierarchy. Knowledge workers face substantial barriers to exit — moving from employment to independent practice requires capital accumulation, legal structure, customer acquisition, and loss of institutional signaling. Theater ratio (0.48): Moderate. Unlike pure performative constraints, the Steinmetz constraint has genuine functional content: knowledge workers do need coordination, institutional validation does provide real value-add. However, theater has increased over time as credentialing has proliferated beyond actual knowledge requirements and as performative gatekeeping has become more costly. Institutional theater maintains the suppression mechanisms that prevent knowledge workers from independent monetization.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the beneficiary (capital owner, sees Rope) and victim (knowledge worker, sees Snare) perspectives is maximal — they are observing the same constraint from opposite ends of the extraction flow. The capital owner experiences coordination benefits: knowledge workers exist to solve their problems efficiently, contracts are voluntary, compensation is paid, profit is legitimate. The knowledge worker experiences extraction: they cannot independently monetize their knowledge, their compensation is a fraction of generated value, their negotiating position is weak, institutional lock-in prevents exit. Both perspectives are structurally accurate from their positions. The constraint's classification depends on who is measuring it, not on objective constraint properties. This is the core insight: knowledge valuation asymmetry is not a natural law but a contingent institutional arrangement that could be reclassified by changing exit options (enabling independent knowledge monetization) or bargaining power (strengthening labor's negotiating position).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality computation from structural data: Knowledge workers (beneficiary: none, victim: yes, power: powerless, exit: trapped) → d ≈ 0.95 → high f(d). Capital owners (beneficiary: yes, victim: none, power: institutional, exit: arbitrage) → d ≈ 0.05 → negative f(d). Organized labor (beneficiary: partial, victim: partial, power: organized, exit: constrained) → d ≈ 0.50 → moderate f(d). The constraint's enforcement mechanisms suppress alternative directionalities. If knowledge workers could arbitrage their knowledge (freelance, consulting, startup) without massive capital and institutional barriers, their d would decline to 0.20-0.30 and the constraint would appear as Rope from their perspective (fair exchange of knowledge for compensation). Suppression mechanisms include: capital requirements for business formation (prevents arbitrage exit), credential gatekeeping (prevents recognition of knowledge outside institutional context), employment law (strengthens employer IP ownership), and cultural narratives (naturalizes hierarchy). These are not natural constraints but political economy choices.
 *
 * MANDATROPHY ANALYSIS:
 *   The Steinmetz constraint resolves mandatrophy by showing that all six types are legitimate readings of a single structural phenomenon from different observational contexts. The snare (knowledge worker perspective) is not a mislabeling of coordination but the genuine experience of a trapped agent. The rope (capital owner perspective) is not a mislabeling of extraction but the genuine experience of a beneficiary. The analytical observer's false mountain reveals the error: naturalizing institutional suppression mechanisms as inherent economic laws. The scaffold perspective shows the real structural point: the constraint could be temporary if alternative knowledge monetization pathways (open-source sustainability, cooperative platforms, direct-to-consumer knowledge services) can be defended against platform recapture. The piton perspective reveals the theatrical gatekeeping that maintains suppression. The mandatrophy is fully resolved by the indexical framework: the constraint is a Snare from the powerless worker's perspective, a Rope from the capital owner's perspective, and these perspectives are not contradictory but structurally complementary. The question 'is it extraction or coordination?' resolves to 'it is coordination for beneficiaries and extraction for victims,' and the classification depends on which agent's perspective is adopted. The system works correctly when all perspectives are present and the perspectival gap is explicit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    knowledge_ownership_boundary,
    'At what point does knowledge become the worker''s property versus the employer''s institutional asset?',
    'Analysis of IP law jurisdictions with different ownership regimes; comparison of outcome metrics (startup formation, wealth creation, retention in originating institution) across regimes',
    'If knowledge is assignable to worker: constraint becomes Rope (fair coordination). If knowledge is institutional asset: constraint becomes Snare (extraction from worker perspective). Current: regime-dependent, majority extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(knowledge_ownership_boundary, conceptual, 'Boundary between worker and institutional knowledge ownership').

omega_variable(
    capital_necessity_for_monetization,
    'Is startup capital truly necessary to convert knowledge into independent income, or is it a contingent barrier created by institutional structure?',
    'Historical analysis of solo practitioners, freelance networks, and knowledge-based service formation with and without capital. Comparison to pre-industrial knowledge worker models (craftspeople, healers). Analysis of successful low-capital knowledge monetization paths.',
    'If capital is necessary: suppression is structural (constraint remains Snare). If capital is institutional artifact: suppression could be dramatically reduced through alternative infrastructure (cooperative platforms, guild structures, peer funding). Current: appears institutional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_necessity_for_monetization, empirical, 'Whether capital necessity is inherent or institutional').

omega_variable(
    credential_signal_fidelity,
    'How much of credential requirement in hiring is genuine quality screening versus institutional signal and gatekeeping?',
    'Comparison of performance metrics: credentialed vs non-credentialed workers in same roles. Employer surveys on credential usage. Analysis of credential inflation over time relative to actual knowledge requirements.',
    'If fidelity is high: credentialing is legitimate coordination mechanism (Rope). If fidelity is low: credentialing is pure rent extraction (Piton). Current: declining fidelity as credentials proliferate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(credential_signal_fidelity, empirical, 'Signal fidelity of credentials in hiring').

omega_variable(
    open_source_sustainability,
    'Can distributed, commons-based knowledge work sustainably replace institutional employment, or does it inherently recreate institutional intermediaries?',
    'Longitudinal analysis of open-source communities: income distribution, sustainability metrics, knowledge worker retention. Comparison of platform effects in GitHub, Stack Overflow, Kaggle. Analysis of second-order platform monetization.',
    'If sustainable: scaffold perspective is real (genuine temporary alternative). If inherently recreates intermediaries: scaffold is aspirational (piton masquerading as scaffold). Current: evidence suggests platform recapture is inevitable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_source_sustainability, empirical, 'Long-term sustainability of commons-based knowledge work').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(steinmetz_valuation_asymmetry, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(steinmetz_tr_t0, steinmetz_valuation_asymmetry, theater_ratio, 0, 0.28).
narrative_ontology:measurement(steinmetz_tr_t25, steinmetz_valuation_asymmetry, theater_ratio, 25, 0.38).
narrative_ontology:measurement(steinmetz_tr_t50, steinmetz_valuation_asymmetry, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(steinmetz_be_t0, steinmetz_valuation_asymmetry, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(steinmetz_be_t25, steinmetz_valuation_asymmetry, base_extractiveness, 25, 0.5).
narrative_ontology:measurement(steinmetz_be_t50, steinmetz_valuation_asymmetry, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(steinmetz_valuation_asymmetry, resource_allocation).
narrative_ontology:affects_constraint(steinmetz_valuation_asymmetry, credential_inflation).
narrative_ontology:affects_constraint(steinmetz_valuation_asymmetry, startup_capital_barrier).
narrative_ontology:affects_constraint(steinmetz_valuation_asymmetry, intellectual_property_lock_in).

% DUAL FORMULATION NOTE:
% The Steinmetz constraint is upstream of specific labor market phenomena (credential inflation, capital barriers, IP lock-in). The base constraint has extractiveness 0.58 reflecting general knowledge valuation asymmetry. Downstream constraints inherit this asymmetry but add domain-specific mechanisms (e.g., credential inflation adds theater_ratio as compensation for declining signal fidelity; IP lock-in adds institutional enforcement costs).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(steinmetz_valuation_asymmetry, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
