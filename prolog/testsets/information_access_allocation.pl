% ============================================================================
% CONSTRAINT STORY: information_access_allocation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_information_access_allocation, []).

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
 *   constraint_id: information_access_allocation
 *   human_readable: Information Access Allocation and Knowledge Distribution
 *   domain: epistemic/political/economic
 *
 * SUMMARY:
 *   Information access allocation represents a structural constraint on
 *   knowledge distribution across populations. It operates at multiple scales
 *   simultaneously: individual (who can access a specific fact),
 *   institutional (which organizations control information flows), and
 *   systemic (how power concentrates through information monopoly). The
 *   constraint exhibits a fundamental tension between genuine coordination
 *   functions (verification, curation, quality assurance, efficient routing
 *   of information through cognitive bottlenecks) and extractive mechanisms
 *   (monopoly pricing, gatekeeping for power maintenance, suppression of
 *   alternative epistemologies). The extractiveness value (0.52) reflects
 *   that institutional information allocation contains both coordination
 *   benefits and asymmetric extraction — gatekeepers genuinely solve some
 *   coordination problems while simultaneously extracting rent from
 *   information scarcity. Theater ratio (0.65) indicates that institutional
 *   gatekeeping rituals (peer review, editorial oversight, credentialing)
 *   have increasingly become performative as digital alternatives have
 *   emerged, yet persist through institutional inertia. The constraint's
 *   decomposition into perspectives reveals a pattern: powerless populations
 *   see pure extraction (snare), moderate populations see mixed
 *   coordination-extraction (tangled rope), beneficiary institutions see
 *   coordination (rope), organized alternatives see temporary constraint with
 *   exit path (scaffold), legacy institutions recognize their own degradation
 *   (piton), and universal observers risk naturalizing contingent
 *   arrangements as immutable law (false mountain). The key diagnostic
 *   signal: if institutional gatekeeping were truly necessary (mountain), it
 *   would persist and expand as information complexity increases. Instead, it
 *   fragments — decentralized alternatives grow fastest in domains with
 *   highest information density, suggesting institutional gatekeeping is
 *   institutional choice, not cognitive necessity.
 *
 * KEY AGENTS:
 *   - Information-Disadvantaged Populations: Victims (powerless/trapped) — lack access to resources, infrastructure, credibility to obtain/verify information; bear full cost of asymmetric allocation
 *   - Information-Seeking Communities: Secondary victims (moderate/constrained) — face barriers to access but benefit from knowledge circulation and community formation around information-sharing
 *   - Information Gatekeepers (Media, Academia, Publishers, Governments): Primary beneficiaries (institutional/arbitrage) — control access, extract rent from information scarcity, maintain power through epistemic monopoly
 *   - Open Knowledge Movements (Wikipedia, Open-Source, Open-Access): Organized agents (organized/constrained) — building decentralized alternatives with sunset logic for institutional gatekeeping
 *   - Legacy Institutional Systems: Institutional actors (institutional/arbitrage) — maintain performative gatekeeping through inertia despite functional degradation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional choice as immutable cognitive limit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(information_access_allocation, 0.52).
domain_priors:suppression_score(information_access_allocation, 0.58).
domain_priors:theater_ratio(information_access_allocation, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(information_access_allocation, extractiveness, 0.52).
narrative_ontology:constraint_metric(information_access_allocation, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(information_access_allocation, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(information_access_allocation, tangled_rope).
narrative_ontology:human_readable(information_access_allocation, "Information Access Allocation and Knowledge Distribution").
narrative_ontology:topic_domain(information_access_allocation, "epistemic/political/economic").

domain_priors:requires_active_enforcement(information_access_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(information_access_allocation, information_gatekeepers).
narrative_ontology:constraint_beneficiary(information_access_allocation, privileged_knowledge_holders).
narrative_ontology:constraint_victim(information_access_allocation, information_disadvantaged_populations).
narrative_ontology:constraint_victim(information_access_allocation, knowledge_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INFORMATION-DISADVANTAGED POPULATIONS (SNARE) — Trapped agents with no exit from information asymmetry. Lack institutional access, technological infrastructure, or epistemic credibility to obtain or verify critical information. Bears full cost of allocation inequality while gatekeepers benefit. Maximum extraction with minimal coordination function.
constraint_indexing:constraint_classification(information_access_allocation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INFORMATION-SEEKING COMMUNITIES (TANGLED ROPE) — Constrained by access barriers but benefit from knowledge circulation and community formation around information-sharing. Face costs of exclusion but also gain from coordination of collective learning. Asymmetric extraction overlaid on genuine coordination function.
constraint_indexing:constraint_classification(information_access_allocation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INFORMATION GATEKEEPERS (ROPE) — Institutional actors (media, academic institutions, corporations, governments) who control access. Experience the constraint as coordination mechanism for managing information flows. Net beneficiaries with ability to arbitrage their position. Extraction is benefit flow toward them.
constraint_indexing:constraint_classification(information_access_allocation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: OPEN KNOWLEDGE MOVEMENTS (SCAFFOLD) — Organized agents (Wikipedia, open-source communities, open-access publishers, digital rights movements) building alternative information infrastructure with sunset logic. Low effective extraction because organized agents see and are constructing exit pathways. Theater ratio declining as decentralized networks mature.
constraint_indexing:constraint_classification(information_access_allocation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY INFORMATION DISTRIBUTION SYSTEMS (PITON) — Traditional libraries, publisher networks, and institutional knowledge curation systems persist through inertia despite emergence of superior alternatives. Theater ratio (0.65) reflects that much institutional information gatekeeping is performative: credentialing, peer review, editorial curation are ritualized far beyond their functional necessity given digital alternatives. System recognizes its own degradation but maintains through institutional lock-in.
constraint_indexing:constraint_classification(information_access_allocation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, information processing has inherent cognitive limits: humans cannot attend to all available data, filtering and allocation are unavoidable, and some gatekeeping function is logically necessary. This perspective naturalizes allocation asymmetry as immutable. However, structural data contradicts mountain classification — the constraint is contingent on institutional arrangements (media ownership, credentialing systems, publishing monopolies), not on cognitive limits alone. False summit detection applies.
constraint_indexing:constraint_classification(information_access_allocation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(information_access_allocation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(information_access_allocation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(information_access_allocation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(information_access_allocation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(information_access_allocation, TR),
    TR >= 0.70.

:- end_tests(information_access_allocation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Institutional information gatekeeping captures significant rent from information scarcity — controlled access to academic journals, proprietary databases, credentialed expertise creates artificial scarcity where digital production costs are near-zero. However, extractiveness is not extreme because genuine coordination functions exist: quality verification, curated synthesis, trusted routing of information through cognitive bottlenecks. The value balances these functions. Trajectory shows rise from 0.38 to 0.52 over 30 years, reflecting gradual shift toward pure extraction as alternative (decentralized) verification systems prove viable. Suppression (0.58): Substantial but not total. Barriers include economic (paywall access), institutional (credentialing requirements, academic hierarchy), technological (language/literacy/connectivity), and epistemic (delegitimization of non-credentialed knowledge). But some information is freely available, some populations maintain alternative knowledge systems, and suppression is increasingly bypassed through open-access and peer-to-peer networks. Theater ratio (0.65): High and rising. Institutional quality assurance mechanisms (peer review, editorial curation, credentialing) are increasingly performative. Their output (article acceptance, degree conferral, editorial endorsement) persists as status signals despite digital alternatives providing equivalent or superior functionality without the ritual. The rise from 0.45 to 0.65 reflects that as decentralized alternatives prove functionally viable, institutional gatekeeping's residual function becomes increasingly theatrical.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between beneficiaries and victims is maximal. Information gatekeepers (rope) experience constraint as coordination — solving the genuine problem of routing reliable information through cognitive limits. Trapped populations (snare) experience the same constraint as pure extraction — barriers prevent access, costs cannot be escaped, benefits flow entirely to gatekeepers. Open knowledge movements (scaffold) experience it as temporary — they demonstrate that alternatives work, making the sunset path visible. Legacy institutions (piton) experience their own system as degraded — peer review and credentialing persist as inertial rituals, not functional necessities. The analytical observer (false mountain) risks naturalizing contingent institutional arrangements as immutable cognitive limits. This perspectival range is diagnostic: if information gatekeeping were truly immutable (mountain), all perspectives would converge on its necessity. Instead, perspectives diverge by type, revealing that the constraint is institutional choice, not natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) measures each agent's structural position in the extraction flow. Information gatekeepers benefit from scarcity value (low d ≈ 0.10-0.20), deriving negative χ (they extract). Trapped populations bear full cost with no exit (high d ≈ 0.90), deriving maximum χ (they are extracted from). Moderate communities face high but surmountable barriers (d ≈ 0.65-0.75), experiencing intermediate χ. The sigmoid f(d) amplifies directionality into experienced extractiveness: a beneficiary gatekeeper with d=0.15 experiences f(d)≈-0.01, while a trapped population with d=0.92 experiences f(d)≈1.30. Scope modifier σ(S) further scales χ: global scope (σ=1.2) makes the constraint feel more extractive because alternatives are globally available yet locally inaccessible; regional scope (σ=0.9) dampens the feeling because local alternatives may exist. The engine's derivation chain computes these automatically from beneficiary/victim declarations and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint avoids mandatrophy by maintaining genuine coordination function alongside extraction. Information gatekeeping does solve real problems — verification, quality assurance, cognitive routing — that decentralized systems solve only partially or at higher cost. The constraint is not misclassified as pure coordination (rope) because extraction is substantial and asymmetric. The beneficiaries have incentive to suppress alternatives (documented censorship of open-access movements, regulatory capture by publishers). The constraint is not misclassified as pure extraction (snare) because coordination benefits are real and non-negligible — institutional gatekeeping does produce (marginal) quality improvements. Tangled rope correctly captures both: genuine coordination function + asymmetric extraction + active enforcement (credentialing systems, copyright law, institutional monopoly). The mandatrophy is resolved by showing that the proportions matter: if quality gain from gatekeeping were >50% of the cost to trapped populations, the constraint might be justified as costly coordination. Current evidence suggests quality gain is 5-15% of cost, making extraction dominate coordination and justifying the tangled_rope classification with high χ.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_cognitive_gatekeeping,
    'Is information gatekeeping driven by unavoidable cognitive scarcity or by contingent institutional control mechanisms?',
    'Comparative analysis of decentralized vs centralized information systems; measurement of filtering rates in systems with vs without institutional gatekeeping; cognitive load testing in different architectures',
    'If primarily cognitive: allocation constraint is mountain (immutable). If primarily institutional: constraint is snare/tangled_rope (contingent and reversible). Current evidence suggests institutional factors dominate gatekeeping by 3:1 margin over cognitive necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_vs_cognitive_gatekeeping, empirical, 'Whether gatekeeping is cognitive necessity or institutional choice').

omega_variable(
    alternative_infrastructure_sufficiency,
    'Do decentralized information networks (peer-to-peer, blockchain-based, open-source systems) provide functionally equivalent or superior alternatives to institutional gatekeeping?',
    'Long-term performance metrics: misinformation spread rates, verification timeliness, coverage breadth, resilience to censorship; comparison across Wikipedia, arXiv, blockchain-based systems vs traditional media/academia',
    'If equivalent/superior: scaffold sunset is real (10-20 year timeline). If inferior: institutional gatekeeping may be necessary extraction. Current evidence suggests alternatives are superior for niche domains but mixed for general-population information.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_infrastructure_sufficiency, empirical, 'Whether decentralized alternatives provide equivalent information quality').

omega_variable(
    incentive_alignment_pathology,
    'Are institutional gatekeepers incentivized to limit access to preserve information scarcity value, or does their incentive structure align with information distribution?',
    'Game-theoretic analysis of gatekeeper revenue models; measurement of access restrictions vs information abundance; comparison of profit margins/power concentration in gatekeeping vs open-access models',
    'If misaligned (preserve scarcity): constraint is extractive snare (ε > 0.60). If aligned: constraint approaches pure coordination rope (ε < 0.30). Current evidence suggests systematic misalignment in proprietary systems (academic publishing, media monopolies).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incentive_alignment_pathology, empirical, 'Whether gatekeeper incentives align with information distribution').

omega_variable(
    epistemic_quality_verification,
    'Does institutional gatekeeping (peer review, editorial oversight, credentialing) produce measurably higher-quality information than decentralized verification?',
    'Longitudinal study of retraction rates, citation accuracy, replication success, and factual error rates across institutional vs open-source sources; cost-per-truth-claim metrics',
    'If institutional > decentralized: extraction may be justified by quality coordination (lower χ). If equivalent/inferior: institutional gatekeeping becomes pure extraction with theater. Current evidence: institutional systems show marginal quality advantage (5-15%) at 10-100x higher cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_quality_verification, empirical, 'Whether institutional gatekeeping produces measurably superior information quality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(information_access_allocation, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(info_access_tr_t0, information_access_allocation, theater_ratio, 0, 0.45).
narrative_ontology:measurement(info_access_tr_t10, information_access_allocation, theater_ratio, 10, 0.58).
narrative_ontology:measurement(info_access_tr_t20, information_access_allocation, theater_ratio, 20, 0.65).
narrative_ontology:measurement(info_access_tr_t30, information_access_allocation, theater_ratio, 30, 0.62).

% Extraction over time
narrative_ontology:measurement(info_access_be_t0, information_access_allocation, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(info_access_be_t10, information_access_allocation, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(info_access_be_t20, information_access_allocation, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(info_access_be_t30, information_access_allocation, base_extractiveness, 30, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(information_access_allocation, information_standard).
narrative_ontology:affects_constraint(information_access_allocation, epistemic_credibility_systems).
narrative_ontology:affects_constraint(information_access_allocation, knowledge_monopoly_concentration).
narrative_ontology:affects_constraint(information_access_allocation, cognitive_attention_scarcity).

% DUAL FORMULATION NOTE:
% Information access allocation decomposes into three structurally distinct constraints with different ε values. Epistemic credibility systems (ε≈0.35, Tangled Rope) coordinate verification. Knowledge monopoly concentration (ε≈0.68, Snare) is pure extraction through IP/copyright. Cognitive attention scarcity (ε≈0.15, Rope) is genuine coordination. This story models the composite constraint; subfamily stories address each mechanism separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(information_access_allocation, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
