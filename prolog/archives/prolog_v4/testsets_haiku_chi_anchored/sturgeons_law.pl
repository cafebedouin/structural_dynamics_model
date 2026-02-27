% ============================================================================
% CONSTRAINT STORY: sturgeons_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sturgeons_law, []).

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
 *   constraint_id: sturgeons_law
 *   human_readable: Sturgeon's Law (90% of everything is crap)
 *   domain: sociological/artistic/epistemology
 *
 * SUMMARY:
 *   Sturgeon's Law is a statistical observation about creative fields that
 *   has become a organizing principle for quality judgment and resource
 *   allocation. The law states that '90% of everything is crap,' implying
 *   that only the top 10% merits serious attention. This constraint operates
 *   at the intersection of statistical inevitability and institutional
 *   gatekeeping: it may reflect a genuine property of production
 *   distributions (mountain), or it may describe and reinforce the
 *   gatekeeping mechanisms that extract value from emerging creators
 *   (snare/tangled rope). The structural tension emerges between those who
 *   use Sturgeon's Law as a justification for gatekeeping authority
 *   (beneficiaries: established curators, publishers, credentialing
 *   institutions) and those trapped below the filtering boundary (victims:
 *   emerging creators, experimental work, unconventional approaches). The
 *   constraint's theater ratio has increased from 0.35 to 0.68 over the
 *   measurement interval, indicating that gatekeeping mechanisms have become
 *   increasingly performative—relying more on ritualistic credentialing than
 *   on actual quality detection. Simultaneously, decentralized discovery
 *   networks (fan communities, algorithmic recommendations, peer curation)
 *   have emerged as alternative filtering mechanisms, creating a potential
 *   sunset trajectory: as these alternatives mature, the traditional
 *   gatekeeper monopoly on the 10% classification fragments.
 *
 * KEY AGENTS:
 *   - Established Creators/Publishers: Primary beneficiary (institutional/arbitrage) — gatekeep access to distribution, critical attention, and audience; define what constitutes the 10%
 *   - Emerging Creators: Primary victim (powerless/trapped) — statistically assigned to the 90% category regardless of merit; face systematic exclusion from distribution channels
 *   - Audience Members: Secondary victim (moderate/constrained) — constrained by gatekeeper-defined discovery mechanisms; benefit from filtering but pay cost of missing unconventional work
 *   - Quality Gatekeepers (Publishers, Curators, Festivals): Institutional beneficiary (institutional/arbitrage) — legitimize their authority through Sturgeon's Law; extract attention/credibility allocation power
 *   - Decentralized Discovery Networks: Organized agents (organized/mobile) — arXiv-style peer sharing, fan communities, algorithmic recommendations, blockchain portfolios; building alternative 10% identification mechanisms
 *   - Traditional Credentialing Systems: Institutional performer (institutional/constrained) — MFA programs, juried shows, peer review; maintain gatekeeping function through ritualistic performance despite reduced epistemic utility
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a contingent distribution as immutable statistical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sturgeons_law, 0.52).
domain_priors:suppression_score(sturgeons_law, 0.65).
domain_priors:theater_ratio(sturgeons_law, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sturgeons_law, extractiveness, 0.52).
narrative_ontology:constraint_metric(sturgeons_law, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sturgeons_law, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sturgeons_law, tangled_rope).
narrative_ontology:human_readable(sturgeons_law, "Sturgeon's Law (90% of everything is crap)").
narrative_ontology:topic_domain(sturgeons_law, "sociological/artistic/epistemology").

domain_priors:requires_active_enforcement(sturgeons_law).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sturgeons_law, quality_gatekeepers).
narrative_ontology:constraint_beneficiary(sturgeons_law, established_creators).
narrative_ontology:constraint_victim(sturgeons_law, emerging_creators).
narrative_ontology:constraint_victim(sturgeons_law, audience_discovery_capacity).
narrative_ontology:constraint_victim(sturgeons_law, creative_experimentation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING CREATOR (SNARE) — Faces systematic filtering by gatekeepers before reaching audiences. Lacks platform, distribution access, or critical mass of followers. Trapped in the 90% category with no mechanism for escape except through gatekeeping authority. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.75.
constraint_indexing:constraint_classification(sturgeons_law, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: AUDIENCE MEMBER (TANGLED ROPE) — Benefits from filtering that removes obvious low-quality content and saves search costs; constrained by limited discovery mechanisms and algorithmic curation that may exclude unconventional work. Suppressed alternative pathways (direct peer-to-peer recommendation, random sampling) exist but are expensive. d≈0.70, f(d)≈1.05, σ=1.0 → χ≈0.54.
constraint_indexing:constraint_classification(sturgeons_law, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: QUALITY GATEKEEPERS (ROPE) — Institutional actors (publishers, curators, platforms, festival organizers) benefit from the authority to define and enforce the 10% boundary. Experience Sturgeon's Law as a coordination mechanism: it justifies their filtering role and allocates scarce attention resources. d≈0.10, f(d)≈0.05, σ=1.2 → χ≈0.06. Low effective extraction because gatekeepers experience coordination benefit.
constraint_indexing:constraint_classification(sturgeons_law, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DECENTRALIZED DISCOVERY NETWORKS (SCAFFOLD) — Organized agents building alternative filtering: fan communities, algorithmic recommendations, social proof mechanisms, collaborative curation, blockchain-verified portfolios. These networks reduce reliance on traditional gatekeepers and provide temporary scaffolding toward a flatter quality landscape. Sunset clause: as algorithmic and peer-curated systems mature, the monopoly on quality judgment fragments. d≈0.35, f(d)≈0.35, σ=1.2 → χ≈0.24.
constraint_indexing:constraint_classification(sturgeons_law, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL INSTITUTIONAL CREDENTIALING (PITON) — MFA programs, juried shows, peer review in academic publishing, guild membership. These systems claim to identify the 10% but increasingly rely on ritualistic performance (application fees, formal submission protocols, credential signaling) rather than actual quality detection. Theater ratio = 0.68 reflects that credentialing is partly performative: prestige derives from scarcity of access, not accuracy of judgment. Institution persists through inertia despite reduced epistemic utility.
constraint_indexing:constraint_classification(sturgeons_law, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / STATISTICAL NECESSITY (MOUNTAIN) — From a universal view, Sturgeon's Law may reflect an inherent statistical property: if production scales with population or opportunity, and if quality is normally distributed around a moderate mean, then a 90/10 split is mathematically inevitable. Base rates of competence, talent distribution, and effort investment naturally produce this ratio. However, the suppression (0.65) and extractiveness (0.52) values contradict pure mountainhood — the structure involves active enforcement and gatekeeper advantage, not just statistics.
constraint_indexing:constraint_classification(sturgeons_law, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sturgeons_law_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sturgeons_law, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sturgeons_law, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sturgeons_law, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sturgeons_law, TR),
    TR >= 0.70.

:- end_tests(sturgeons_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Gatekeeping mechanisms extract significant value from emerging creators by controlling access to distribution and attention. However, extractiveness is not maximal because: (1) some gatekeeping provides real filtering benefit (audiences genuinely save time through curation), (2) emerging creators can bypass traditional gates through direct publication/sharing, (3) the 90/10 ratio may have statistical basis independent of gatekeeper intent. The moderate-high value reflects active enforcement (gatekeepers actively suppress alternative discovery pathways) combined with genuine coordination benefit. Suppression (0.65): Moderate-high. Multiple mechanisms suppress alternatives to traditional gatekeeping: editorial review requirements, formal submission protocols, credentialing prerequisites, platform algorithm opacity, funding concentration in institutional hands, career penalties for publication outside canonical venues. But suppression is not total—self-publishing, fan networks, and algorithmic discovery are functioning alternatives, just less resourced. Theater ratio (0.68): Elevated. Credentialing systems (MFA programs, juried shows, peer review) increasingly rely on performative elements—application fees signal commitment, formal submission formats signal professionalism, credential prestige derives from scarcity rather than demonstrated quality prediction accuracy. The theater has increased over the interval as credentialing systems have formalized their rituals while their actual quality detection utility has decreased relative to decentralized alternatives.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same statistical observation ('90% is low-quality') classifies radically differently depending on observer position. Established gatekeepers experience Sturgeon's Law as a coordination mechanism (Rope)—it justifies their filtering role and allocates scarce critical attention. Emerging creators experience it as a snare—a statistical trap with no exit mechanism. Audiences experience it as mixed (Tangled Rope)—gatekeeping saves search costs but also excludes unconventional work. Decentralized discovery networks see it as temporary (Scaffold)—algorithmic and peer-curated alternatives are building a sunset for traditional gatekeeping authority. Credentialing institutions see their own degradation (Piton)—MFA programs and juried shows increasingly perform ritual functions divorced from quality detection. The analytical observer risks seeing statistical inevitability (Mountain)—quality distributions naturally produce a 90/10 split—but the suppression (0.65) and active enforcement requirements contradict mountainhood; the observed ratio reflects architectural choices, not statistical necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Emerging creators: Victim + trapped → d≈0.95, f(d)≈1.42. Maximal extraction—no exit from the 90% category without gatekeeper approval. Audience members: Victim + constrained (but also beneficiary of time-saving) → d≈0.70, f(d)≈1.05. High extraction of attention allocation, but genuine coordination benefit in filtering reduces directionality. Gatekeepers (established publishers, curators, festival organizers): Beneficiary + arbitrage → d≈0.10, f(d)≈0.05. Net beneficiary; they experience Sturgeon's Law as enabling their authority. Decentralized discovery networks: Organized + mobile → d≈0.35, f(d)≈0.35. Low-moderate extraction because these agents have agency and genuine exit options; they see constraint as temporary. Credentialing systems: Institutional + constrained → d≈0.45, f(d)≈0.50. Moderate extraction; they maintain the gatekeeping function but under increasing pressure from decentralized alternatives. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification (statistical necessity) is perspectival and vulnerable to false summit detection.
 *
 * MANDATROPHY ANALYSIS:
 *   Sturgeon's Law resolves the mandatrophy by decomposing into at least two distinct structural claims: (1) STATISTICAL CLAIM: Production distributions are approximately normal; top performers cluster around 10% of population. This is a mountain—it follows from basic statistics of competence distribution. (2) INSTITUTIONAL CLAIM: Gatekeeping mechanisms define quality as the top 10% and systematically exclude the remaining 90% from distribution/attention. This is snare/tangled rope—it involves active enforcement, suppression of alternatives, and extraction of value from emerging creators. The constraint story treats both claims as operative: the statistical claim provides cover for the institutional extraction. The theater ratio rise (0.35→0.68) indicates that credentialing systems increasingly rely on the mountain framing to justify extraction. The decentralized discovery perspective introduces a sunset: if peer-curated and algorithmic systems can identify high-quality work as effectively as traditional gatekeeping, the institutional claim fails, and only the statistical claim (which affects no one) remains. This is a genuine mandatrophy resolution: the extractive institutional mechanism parasitizes the statistical fact but is structurally severable from it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quality_metric_definition,
    'What constitutes ''quality'' in the domain under observation? Is Sturgeon''s Law describing an objective property or a subjective filtering mechanism?',
    'Domain-specific analysis: in music, compare critical acclaim vs listener preference distributions; in literature, compare canon selection vs bestseller ratings; in software, compare expert reviews vs user satisfaction',
    'If quality is objective: Sturgeon''s Law describes a natural distribution (mountain). If quality is gatekeeper-defined: law describes an extraction mechanism (snare/tangled rope). If both: depends on observer position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quality_metric_definition, conceptual, 'Whether quality is objective property or subjective filtering').

omega_variable(
    ratio_stability_across_domains,
    'Does the 90/10 split hold constant across domains with different production costs, barriers to entry, and gatekeeper structures?',
    'Comparative analysis: music (low barrier, algorithmic discovery), academic publishing (high barrier, peer review), film (high barrier, industry gatekeeping), software (low barrier, modular contribution), visual art (moderate barrier, gallery networks). Test whether 90% characterization varies by domain or remains stable.',
    'If stable: suggests mathematical inevitability (mountain). If highly variable: suggests domain-specific gatekeeper architecture (snare/rope/tangled rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ratio_stability_across_domains, empirical, 'Whether 90/10 ratio holds across production domains').

omega_variable(
    discovery_pathway_efficacy,
    'Do decentralized discovery mechanisms (fan communities, algorithm-driven recommendations, peer curation) actually identify and elevate unconventional high-quality work at rates comparable to traditional gatekeeping?',
    'Historical tracking: identify works initially rejected by traditional gates but discovered and sustained by decentralized networks; compare discovery time, audience growth, and critical recognition eventual paths',
    'If effective: scaffold sunset is real, constraint will degrade as decentralized discovery matures. If ineffective: decentralized discovery is aspirational theater; snare/extraction persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discovery_pathway_efficacy, empirical, 'Whether decentralized discovery matches traditional gatekeeping efficacy').

omega_variable(
    gatekeeper_accuracy_bias,
    'Do established gatekeepers preferentially select work that resembles prior canonical work over genuinely novel approaches? Is the 90% ''crap'' category systematically biased toward novelty/deviation?',
    'Content analysis: measure deviation-from-canon in rejected vs accepted works; survey gatekeepers on criteria; analyze whether rejected works later gain acceptance as genre norms shift',
    'If bias exists: gatekeeping extracts not just attention but creative direction (strong snare). If gatekeepers are unbiased: they provide real filtering service (rope/coordinate). Determines whether suppression is necessary or extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeper_accuracy_bias, empirical, 'Whether gatekeeper selection is biased toward canonical similarity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sturgeons_law, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sturg_tr_t0, sturgeons_law, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sturg_tr_t25, sturgeons_law, theater_ratio, 25, 0.52).
narrative_ontology:measurement(sturg_tr_t50, sturgeons_law, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(sturg_be_t0, sturgeons_law, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(sturg_be_t25, sturgeons_law, base_extractiveness, 25, 0.4).
narrative_ontology:measurement(sturg_be_t50, sturgeons_law, base_extractiveness, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sturgeons_law, information_standard).
narrative_ontology:affects_constraint(sturgeons_law, cultural_credentialism).
narrative_ontology:affects_constraint(sturgeons_law, attention_scarcity).
narrative_ontology:affects_constraint(sturgeons_law, gatekeeping_monopolies).

% DUAL FORMULATION NOTE:
% Sturgeon's Law decomposes into statistical inevitability (mountain) and institutional gatekeeping (tangled rope/snare). The statistical component is causally upstream—quality distributions naturally produce skewed outcomes. The institutional component is downstream—gatekeeping mechanisms use statistical inevitability as justification for extraction and suppression of alternatives. These are structurally distinct constraints linked by the narrative that conflates them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
