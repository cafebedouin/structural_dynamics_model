% ============================================================================
% CONSTRAINT STORY: elite_institutional_reputation_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_elite_institutional_reputation_extraction, []).

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
 *   constraint_id: elite_institutional_reputation_extraction
 *   human_readable: Elite Institutional Reputation Extraction
 *   domain: institutional_dynamics/social_hierarchy
 *
 * SUMMARY:
 *   Elite institutional reputation extraction operates as a two-layer system:
 *   genuine coordination of knowledge production coupled with asymmetric
 *   concentration of prestige and resources. Prestigious institutions
 *   function as organizing nodes in global research networks — they set
 *   standards, develop infrastructure, attract talent — creating real
 *   coordination benefits. Simultaneously, they extract excessive prestige
 *   and resources through gatekeeping mechanisms (citation bias, hiring
 *   discrimination, funding concentration) that far exceed the value of their
 *   coordination function. The constraint exhibits the full spectrum of DR
 *   types because different agents experience fundamentally different
 *   structural realities. Elite institutions genuinely experience this as
 *   beneficial coordination. Peripheral scholars experience it as a trap with
 *   no exit. The open science movement sees a temporary sunset mechanism. The
 *   prestige ranking system perpetuates the constraint through performative
 *   theater (rankings validating what they measure). The analytical observer
 *   risks naturalizing a contingent institutional hierarchy as inevitable
 *   stratification. The extractiveness has increased over the 20-year
 *   interval as citation concentration, hiring homogeneity, and funding
 *   concentration have all intensified — the constraint tightens as
 *   institutional prestige becomes more predictive of career outcomes than
 *   research quality.
 *
 * KEY AGENTS:
 *   - Elite Institutions: Primary beneficiary (institutional/arbitrage) — capture prestige concentration, preferential citations, funding flows, and talent recruitment with full exit and arbitrage options
 *   - Peripheral Scholars: Primary victim (powerless/trapped) — trapped by credential gatekeeping; ideas may be appropriated while affiliation prevents career advancement
 *   - Non-Elite Institutions: Secondary victim (moderate/constrained) — constrained by funding asymmetry and ranking disadvantage; benefit from research coordination but experience asymmetric extraction of prestige and resources
 *   - Field Meritocratic Legitimacy: Abstract victim (powerless/trapped) — research quality becomes decoupled from institutional prestige; field's epistemic reliability degrades as prestige substitutes for merit
 *   - Open Science Movement: Organized agents (organized/constrained) — preprint servers, open peer review, decentralized credentials building alternative pathways; see sunset mechanism
 *   - Prestige Ranking Systems: Institutional actor (institutional/arbitrage) — maintains performative legitimacy through self-referential rankings; theater persists through inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risk of naturalizing institutional stratification as inherent to knowledge systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(elite_institutional_reputation_extraction, 0.58).
domain_priors:suppression_score(elite_institutional_reputation_extraction, 0.62).
domain_priors:theater_ratio(elite_institutional_reputation_extraction, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(elite_institutional_reputation_extraction, extractiveness, 0.58).
narrative_ontology:constraint_metric(elite_institutional_reputation_extraction, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(elite_institutional_reputation_extraction, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(elite_institutional_reputation_extraction, tangled_rope).
narrative_ontology:human_readable(elite_institutional_reputation_extraction, "Elite Institutional Reputation Extraction").
narrative_ontology:topic_domain(elite_institutional_reputation_extraction, "institutional_dynamics/social_hierarchy").

domain_priors:requires_active_enforcement(elite_institutional_reputation_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(elite_institutional_reputation_extraction, elite_institutions).
narrative_ontology:constraint_beneficiary(elite_institutional_reputation_extraction, institutional_gatekeepers).
narrative_ontology:constraint_victim(elite_institutional_reputation_extraction, peripheral_scholars).
narrative_ontology:constraint_victim(elite_institutional_reputation_extraction, non_elite_institutions).
narrative_ontology:constraint_victim(elite_institutional_reputation_extraction, field_meritocratic_legitimacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERIPHERAL SCHOLAR (SNARE) — Structurally trapped by credential gatekeeping and citation networks concentrated in elite institutions. Cannot exit without sacrificing career legitimacy. Maximum extraction: ideas are borrowed, citations are withheld, hiring advantage flows to elite-affiliated authors regardless of work quality.
constraint_indexing:constraint_classification(elite_institutional_reputation_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-ELITE INSTITUTION (TANGLED ROPE) — Constrained by funding asymmetry and reputational rankings, but also benefits from national research coordination, infrastructure sharing, and collaborative networks. Genuine coordination function exists (knowledge diffusion, method standardization) alongside asymmetric extraction (prestige and resource concentration).
constraint_indexing:constraint_classification(elite_institutional_reputation_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ELITE INSTITUTION (ROPE) — Experiences the constraint as pure coordination: prestige enables knowledge diffusion, attracts talent, and facilitates international collaboration. Net beneficiary with full arbitrage options — can relocate resources, absorb or reject researchers, set standards without constraint.
constraint_indexing:constraint_classification(elite_institutional_reputation_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN SCIENCE MOVEMENT (SCAFFOLD) — Organized actors (preprint servers, open peer review, decentralized credentials) are building alternative legitimacy pathways that reduce dependence on elite gatekeeping. Lower effective extraction because this constituency sees an exit mechanism and has agency. Sunset clause implicit: as digital-native credentialing matures (blockchain credentials, open CV systems), elite institutional monopoly on legitimation will weaken.
constraint_indexing:constraint_classification(elite_institutional_reputation_extraction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PRESTIGE RANKING SYSTEM (PITON) — University rankings (QS, THE, Shanghai Rankings) are largely performative theater: they measure resource concentration and citation bias rather than research quality or impact. The rankings persist through institutional inertia and are used to justify resource allocation decisions that reinforce the rankings themselves. Degraded function: originally designed to guide student choice, now used to legitimize inequality.
constraint_indexing:constraint_classification(elite_institutional_reputation_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risk of false summit: from a civilizational perspective, institutional stratification appears immutable — 'some institutions will always be more prestigious' reads like a natural law. But the structural data contradicts this naturalization: the extraction mechanism is contingent on specific enforcement practices (citation concentration, hiring bias, funding gatekeeping) that could be changed. The constraint is not a law of nature but an institutionalized extraction system sustained by active practices.
constraint_indexing:constraint_classification(elite_institutional_reputation_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(elite_institutional_reputation_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(elite_institutional_reputation_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(elite_institutional_reputation_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(elite_institutional_reputation_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(elite_institutional_reputation_extraction, TR),
    TR >= 0.70.

:- end_tests(elite_institutional_reputation_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Elite institutions extract prestige, citations, and resources disproportionate to their research contribution through active gatekeeping — hiring bias, citation concentration, funding preferentiality. The value is elevated (vs historical 0.35) because empirical evidence now shows hiring discrimination is 60-70% non-meritocratic, citation bias concentrates citations toward elite authors independent of research quality, and funding concentration is self-perpetuating lock-in rather than merit-driven. However, extraction is not extreme (snare-level) because genuine coordination functions exist: elite institutions do set standards, provide infrastructure, and facilitate international collaboration. The coordination value is real but smaller than the extraction overhead. Suppression (0.62): High. Barriers to peripheral scholars and non-elite institutions include: credential gatekeeping (hiring requires elite affiliation), citation bias (citations concentrated regardless of work quality), funding scarcity (elite institutions receive disproportionate funding), and career risk (challenging the system requires sacrificing opportunities). These barriers are enforced through active institutional practices, not just passive market forces. Theater ratio (0.68): High. Prestige rankings are substantially performative: they measure resource concentration and endowment size rather than research quality or impact. Hiring decisions use elite credentials as proxy for capability rather than assessing capability directly. Citation practices use institutional affiliation as signal rather than evaluating individual work. The theater has increased over time as rankings have become more determinative of resource allocation and hiring decisions.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates radical perspectival divergence. Elite institutions see this as beneficial coordination (Rope) — prestige enables networking, attracts talent, facilitates collaboration. They have full arbitrage options and experience zero extraction. Peripheral scholars see this as a trap (Snare) — trapped by credential gatekeeping, ideas are appropriated, careers are blocked. They experience maximum extraction and cannot exit. Non-elite institutions see this as mixed (Tangled Rope) — they benefit from research coordination and infrastructure access but are constrained by funding disadvantage and reputation damage from lower rankings. The open science movement sees this as temporary (Scaffold) — organized agents are building alternative credentialing pathways that reduce dependence on elite gatekeeping. The prestige ranking system sees itself as degraded (Piton) — rankings persist through institutional inertia and are used to justify decisions that reinforce the rankings, but they don't actually measure research quality. The analytical observer risks seeing this as natural law (Mountain) — 'some institutions will always be more prestigious' — but the structural data contradicts this naturalization. The extraction mechanisms (hiring discrimination, citation bias, funding concentration) are contingent institutional practices, not laws of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from structural position: beneficiary status, victim status, and exit options within this specific constraint. Elite institutions derive low d (0.10-0.20) from beneficiary status + arbitrage exit → negative effective extraction (they are subsidized by the system). Peripheral scholars derive high d (0.90-0.95) from victim status + trapped exit → maximum effective extraction. Non-elite institutions derive moderate d (0.55-0.65) from mixed beneficiary/victim status + constrained exit. Open science coalition derives moderate-low d (0.35-0.45) from victim status (being harmed by current gatekeeping) + constrained exit (building alternatives but not yet dominant). Prestige ranking system derives moderate d (0.50-0.60) from institutional power with mixed exit options. The derivation chain produces clear differentiation: beneficiaries with escape routes experience low chi; trapped agents experience high chi; mixed agents experience moderate chi. This mapping directly generates the perspectival gap observed across the six perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves through demonstrating that all six types are perspectivally coherent. This is NOT 'which type is correct?' but 'what does each position experience?' The elite institution's rope is genuine coordination benefit. The open science coalition's scaffold is a real structural feature (alternative credentialing emerging). The prestige ranking system's piton is real observation (performative theater). The peripheral scholar's snare is the structural reality from their position. The non-elite institution's tangled rope is the mixture they experience. The analytical observer's mountain is a false summit (naturalizing contingent institutional practices). The resolution shows that institutional stratification is not immutable but contingent on specific enforcement practices that could be modified: if hiring discrimination were eliminated, citation bias corrected, and funding distributed more equally, elite institutional extraction would decline dramatically. The mountain classification fails because the underlying mechanisms are changeable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    meritocratic_signal_degradation,
    'To what degree does elite institutional affiliation signal research quality versus conferring unearned legitimacy independent of work quality?',
    'Blind evaluation studies of research quality across institution type; citation analysis controlling for self-citation and coauthor affiliation bias; hiring audit studies controlling for credentials',
    'If signal is >70% quality: extraction is minimal and institutional stratification is meritocratic. If signal is <50% quality: institutional affiliation is primarily extractive theater, and prestige concentration represents pure rent-seeking.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meritocratic_signal_degradation, empirical, 'Degree to which elite affiliation signals quality vs confers unearned legitimacy').

omega_variable(
    citation_concentration_mechanism,
    'Is citation concentration toward elite institutions a result of better research quality, preferential citation by elite authors, or structural citation network effects?',
    'Citation bias analysis: compare citation rates for identical papers posted under elite vs non-elite author affiliations; network modeling of citation flow controlling for research topic and methodology',
    'If quality-driven: citation concentration reflects meritocracy. If preferential citation: concentration is actively maintained extraction mechanism. If network effects: concentration is structural lock-in without intentional gatekeeping.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(citation_concentration_mechanism, empirical, 'Causal mechanism driving citation concentration toward elite institutions').

omega_variable(
    hiring_discrimination_magnitude,
    'What proportion of hiring preference for elite-affiliated candidates reflects perceived capability differences versus credential discrimination?',
    'Resume audit studies; comparison of hiring rates for identical CVs with elite vs non-elite institutional affiliations; longitudinal tracking of hiring outcomes and publication records controlling for institutional prestige',
    'If >60% discrimination-driven: hiring preference constitutes direct extraction mechanism beyond legitimate meritocratic sorting. If <20% discrimination: observed hiring stratification reflects actual quality differences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hiring_discrimination_magnitude, empirical, 'Magnitude of hiring discrimination based on elite institutional affiliation').

omega_variable(
    funding_concentration_lock_in,
    'Does funding concentration in elite institutions reflect superior research capability or does it create structural lock-in that perpetuates concentration regardless of research quality?',
    'Randomized funding allocation experiments; analysis of research output controlling for funding level; historical analysis of funding shifts and their correlation with research impact',
    'If lock-in dominant: funding concentration is self-sustaining extraction mechanism requiring structural intervention. If capability-driven: concentration reflects legitimate resource allocation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(funding_concentration_lock_in, empirical, 'Whether funding concentration is merit-driven or lock-in-driven').

omega_variable(
    open_credentialing_viability,
    'Can decentralized, transparent credentialing systems (blockchain-based reputation, open publication records, transparent peer review) achieve sufficient legitimacy to reduce dependence on elite institutional gatekeeping?',
    'Adoption rates and hiring success of open-credential pathways; comparison of career outcomes for researchers using decentralized credentials vs traditional institutional affiliation; employer acceptance rates for non-traditional credentials',
    'If viable: scaffold sunset is real and elite extraction mechanisms will weaken. If non-viable: open credentialing movement is aspirational and institutional gatekeeping will persist.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(open_credentialing_viability, empirical, 'Viability of decentralized credentialing as alternative to elite institutional gatekeeping').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(elite_institutional_reputation_extraction, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eire_tr_t0, elite_institutional_reputation_extraction, theater_ratio, 0, 0.5).
narrative_ontology:measurement(eire_tr_t10, elite_institutional_reputation_extraction, theater_ratio, 10, 0.61).
narrative_ontology:measurement(eire_tr_t20, elite_institutional_reputation_extraction, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(eire_be_t0, elite_institutional_reputation_extraction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(eire_be_t10, elite_institutional_reputation_extraction, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(eire_be_t20, elite_institutional_reputation_extraction, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(elite_institutional_reputation_extraction, identity_coordination).
narrative_ontology:affects_constraint(elite_institutional_reputation_extraction, academic_citation_network_concentration).
narrative_ontology:affects_constraint(elite_institutional_reputation_extraction, research_funding_concentration).
narrative_ontology:affects_constraint(elite_institutional_reputation_extraction, university_ranking_legitimacy).

% DUAL FORMULATION NOTE:
% Elite institutional reputation extraction decomposes into three sub-constraints: (1) citation concentration (ε=0.42, Tangled Rope — genuine method standardization coupled with preferential citation), (2) funding concentration (ε=0.65, Snare — lock-in mechanism with minimal coordination benefit), and (3) hiring discrimination (ε=0.55, Tangled Rope — legitimate credentialing filter coupled with non-merit gatekeeping). This story addresses the system-level constraint; decomposed stories model the specific mechanisms. All three are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(elite_institutional_reputation_extraction, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
