% ============================================================================
% CONSTRAINT STORY: artistic_risk_aversion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_artistic_risk_aversion, []).

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
 *   constraint_id: artistic_risk_aversion
 *   human_readable: Artistic Risk Aversion in Cultural Funding and Exhibition
 *   domain: cultural_economics/artistic_governance
 *
 * SUMMARY:
 *   Artistic risk aversion in cultural funding operates through a system
 *   where gatekeeping institutions (museums, galleries, grant bodies)
 *   allocate resources toward established aesthetic categories while
 *   marginalizing experimental work. The constraint exhibits mixed
 *   coordination and extraction: it does coordinate aesthetic standards and
 *   resource allocation, enabling efficient cultural governance. But it
 *   simultaneously extracts rent through gatekeeping, concentrating
 *   decision-making power in institutional hands while suppressing aesthetic
 *   innovation. The theater_ratio (0.64) reflects that grant processes and
 *   curatorial decisions employ elaborate procedural theater — rubrics, peer
 *   review panels, mission statements — that mask conservative filtering of
 *   risk. The extractiveness (0.52) indicates moderate but asymmetric
 *   extraction: established artists and institutions profit from the
 *   constraint through market stabilization and prestige concentration, while
 *   experimental artists bear the cost of suppressed innovation and
 *   constrained career options. Alternative pathways (artist collectives,
 *   digital platforms, DIY venues) represent an emerging sunset mechanism,
 *   though their sufficiency for career sustainability remains contested.
 *
 * KEY AGENTS:
 *   - Experimental Artists: Primary victims (powerless/trapped) — face barriers to institutional validation; career survival requires either conformity or financial precarity
 *   - Established Artists: Primary beneficiaries (institutional/arbitrage) — market position protected; access to institutional validation; can take risks from position of security
 *   - Major Cultural Institutions: Primary beneficiaries (institutional/arbitrage) — receive donor confidence and prestige from risk-averse programming; control aesthetic standards
 *   - Mid-Career Emerging Artists: Secondary victims (moderate/constrained) — experience mixed extraction and coordination; can access some funding but face editorial control asymmetries
 *   - Grant Gatekeepers: Primary beneficiaries (institutional/arbitrage) — control resource allocation through performative review processes; maintain gatekeeping authority
 *   - Alternative Art Networks: Organized agents (organized/mobile) — artists collectives, DIY venues, digital platforms building parallel pathways with lower institutional gatekeeping
 *   - Arts Policy Bodies: Institutional intermediaries (institutional/constrained) — coordinate aesthetic standards but trapped in conservative patterns by political and donor pressure
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as laws of cultural nature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(artistic_risk_aversion, 0.52).
domain_priors:suppression_score(artistic_risk_aversion, 0.58).
domain_priors:theater_ratio(artistic_risk_aversion, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(artistic_risk_aversion, extractiveness, 0.52).
narrative_ontology:constraint_metric(artistic_risk_aversion, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(artistic_risk_aversion, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(artistic_risk_aversion, tangled_rope).
narrative_ontology:human_readable(artistic_risk_aversion, "Artistic Risk Aversion in Cultural Funding and Exhibition").
narrative_ontology:topic_domain(artistic_risk_aversion, "cultural_economics/artistic_governance").

domain_priors:requires_active_enforcement(artistic_risk_aversion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(artistic_risk_aversion, established_artists).
narrative_ontology:constraint_beneficiary(artistic_risk_aversion, mainstream_cultural_institutions).
narrative_ontology:constraint_beneficiary(artistic_risk_aversion, grant_gatekeepers).
narrative_ontology:constraint_beneficiary(artistic_risk_aversion, commercial_gallery_networks).
narrative_ontology:constraint_victim(artistic_risk_aversion, experimental_artists).
narrative_ontology:constraint_victim(artistic_risk_aversion, marginal_cultural_traditions).
narrative_ontology:constraint_victim(artistic_risk_aversion, aesthetic_innovation).
narrative_ontology:constraint_victim(artistic_risk_aversion, underrepresented_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXPERIMENTAL ARTIST (SNARE) — Faces structural barriers: grant panels require demonstrated track record (cannot show what hasn't been funded), galleries demand market-tested styles, and rejection feedback is opaque. Career survival requires conformity to established aesthetic categories. No exit: pursuing experimental work means financial precarity; abandoning it means ceasing to be an artist as self-defined. Maximum experienced extraction — resources flow toward safe, institutionally-legible work.
constraint_indexing:constraint_classification(artistic_risk_aversion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MAJOR CULTURAL INSTITUTION (ROPE) — Benefits from risk aversion: established reputation attracts donors and visitors predictably. Exhibits safe, validated artists. Experiences the constraint as coordination: shared aesthetic standards enable efficient curation and donor confidence. Net beneficiary — the constraint allocates prestige and resources toward their programming.
constraint_indexing:constraint_classification(artistic_risk_aversion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: MID-CAREER EMERGING ARTIST (TANGLED ROPE) — Has enough track record to access some funding but faces asymmetric extraction: curators profit from artistic innovation while maintaining editorial control and claiming curatorial vision. Benefits from collaborative ecosystem but bears disproportionate labor and risk of failure. Constrained exit: could pursue commercial work but risks losing artistic identity and access to the institutional art world.
constraint_indexing:constraint_classification(artistic_risk_aversion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ESTABLISHED ARTIST WITH GLOBAL RECOGNITION (TANGLED ROPE) — Experiences coordination: the constraint protects their market position and allows strategic risk-taking from a position of security. But also experiences extraction: retroactively designated as 'the important one,' their early experimental work becomes institutionally valorized only after markets validate it, and dealers extract rent from speculative secondary markets. Mobile exit but strategically chooses to remain within the system because it amplifies their cultural authority.
constraint_indexing:constraint_classification(artistic_risk_aversion, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ALTERNATIVE ART NETWORKS (SCAFFOLD) — Artist collectives, DIY venues, digital platforms (Instagram, TikTok), and international artist residencies create parallel funding and exhibition pathways with lower institutional gatekeeping. These networks have sunset logic: as digital distribution and community funding mature, dependence on traditional funding bottlenecks declines. High agency and visible exit paths — artists can bootstrap visibility without institutional validation. Theater_ratio declining as alternative pathways demonstrate viability.
constraint_indexing:constraint_classification(artistic_risk_aversion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: GRANT ADMINISTRATION APPARATUS (PITON) — Funding bodies (NEA, arts councils, private foundations) maintain elaborate application processes, review committees, and funding categories that are substantially performative. The categories themselves (dance, visual art, literature, etc.) ossify and constrain what counts as fundable innovation. Reviewers use proxy metrics (previous awards, institutional affiliation, demographic boxes) that are theater masking actual judgment. The apparatus persists through institutional inertia — funding flows through channels because they exist, not because they optimally allocate artistic resources.
constraint_indexing:constraint_classification(artistic_risk_aversion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational distance, aesthetic risk aversion might appear as an immutable property of culture: societies always marginalize the unfamiliar, funding always favors the proven, innovation always emerges from margins. This perspective risks naturalizing what is actually a contingent institutional arrangement. However, cross-cultural evidence (medieval Islamic patronage networks, Renaissance workshop models, 20th-century avant-garde funding) shows radically different models. The mountain classification is a false summit — revealed by the structural data showing extractive gatekeeping rather than inherent cultural law.
constraint_indexing:constraint_classification(artistic_risk_aversion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: ARTS POLICY BODY (TANGLED ROPE) — Government or foundation staff responsible for allocating cultural funding experience mixed extraction and coordination. They coordinate aesthetic standards and resource distribution (genuine function) while extracting power through gatekeeping decisions. Constrained exit: abandoning the institutional role means losing influence over cultural narrative. Policy bodies experience the constraint as both enabling their mission and trapping them in conservative patterns — they want to fund innovation but face political/donor pressure for safe choices.
constraint_indexing:constraint_classification(artistic_risk_aversion, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(artistic_risk_aversion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(artistic_risk_aversion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(artistic_risk_aversion, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(artistic_risk_aversion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(artistic_risk_aversion, TR),
    TR >= 0.70.

:- end_tests(artistic_risk_aversion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint concentrates decision-making power in institutional hands and creates asymmetric information (gatekeepers know their criteria; artists guess). But extraction is not maximal because (1) some alternative pathways exist, (2) individual institutions vary in openness to risk, and (3) long-term reputational incentives occasionally push institutions toward innovation. The measurement trajectory shows increasing extractiveness (0.38→0.52 over interval), reflecting institutional gatekeeping tightening as competition for cultural prestige intensifies. Suppression (0.58): Moderate-high. Barriers include: competitive grant processes with low funding rates (~5-10% typical), publication bias (successful artists become visible, unsuccessful ones remain invisible), tacit knowledge of what 'counts' as fundable, demographic gatekeeping, and career risk. Exit barriers are substantial but not total — artists can pursue commercial work, move internationally, or bootstrap alternative networks. Theater_ratio (0.64): High. Grant panels use elaborate evaluation frameworks, peer review processes, and institutional affiliation checks that are substantially performative. Actual decisions often correlate with demographic factors (institutional prestige of applicant's training) and network access (knowing who to ask for letters of support) more than objective artistic merit. The theater has increased over the interval as gatekeeping bodies have professionalized their processes without fundamentally changing conservative filtering patterns.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates perspectival divergence across institutional positions. Established artists see coordination and protection (Rope) — the constraint stabilizes their market and enables strategic risk-taking from secure position. Experimental artists see pure extraction (Snare) — the same constraint blocks their access to resources and audiences. Alternative networks see a temporary problem with an exit (Scaffold) — digital distribution and community funding are building pathways that reduce dependence on institutional gatekeeping. The grant apparatus sees its own gatekeeping as necessary curation (Piton) — maintains elaborate review processes that are theater masking conservative filtering. Policy bodies see mixed extraction and coordination (Tangled Rope) — genuine resource allocation function shadowed by power asymmetries and political pressure. The analytical observer risks naturalizing this as immutable (Mountain) — 'artistic innovation always emerges from margins, funding always favors the proven' — but cross-cultural evidence shows radically different patronage models. The false summit reveals the constraint as institutional arrangement, not law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by structural position relative to the extraction flow. Experimental artists (powerless/trapped) have zero exit capacity within artistic identity constraints — their d approaches 1.0, producing high f(d) and maximum experienced extraction. Established artists (institutional/arbitrage) have arbitrage options — they can retreat to commercial markets, international venues, or reputational security — yielding low d (~0.20) and negative experienced extraction (they benefit). Mid-career artists (moderate/constrained) occupy intermediate position: constrained exit (significant cost to leave but not impossible) and mixed beneficiary/victim status (access to some resources, but unfavorable terms) yields intermediate d (~0.55). Alternative networks (organized/mobile) have multiple exit options and are building new infrastructure, so their experienced extraction is low despite appearing in the network structure. The grant apparatus (institutional/arbitrage) is a beneficiary with arbitrage options — can shift funding patterns if political pressure emerges — yielding low d and net positive resource flow.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that classification depends on structural position, not intrinsic type. The constraint is simultaneously rope (coordination function), snare (extraction for powerless), tangled rope (mixed extraction and coordination), scaffold (alternative pathways emerging), and piton (performative gatekeeping). The mandatrophy dissolves when the question shifts from 'what type is this constraint?' to 'which structural position is being observed?' The false summit (analytical/mountain) reveals that naturalizing conservative gatekeeping as immutable cultural law obscures the contingent institutional mechanisms that could be reformed. The presence of all six types within a single constraint family signals a mature system: the constraint coordinates resources (rope function), extracts from experimental artists (snare mechanism), mixes both (tangled rope reality), maintains performance theater (piton degradation), creates escape routes (scaffold alternatives), and risks theoretical naturalization (mountain false summit).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_artistic_risk,
    'What counts as legitimate ''artistic risk'' vs. performative transgression or market posturing?',
    'Longitudinal tracking of works classified as risky: which are adopted into canon vs. which prove to be fashionable novelties? Comparison of critical reception (1970) vs. institutional validation (2026) for same works.',
    'If risk is well-defined and measurable: artistic risk aversion is a clear snare mechanism. If risk is context-dependent and retrospective: the constraint is more ambiguous — institutions face genuine uncertainty about which risks are worth taking.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_artistic_risk, conceptual, 'Whether ''artistic risk'' is a stable category or retrospectively constructed').

omega_variable(
    alternative_pathway_sufficiency,
    'Do digital platforms, artist collectives, and DIY venues provide sufficient alternative funding and audience development for experimental artists to sustain careers outside institutional patronage?',
    'Career longevity analysis: compare income stability and audience growth for artists using alternative pathways vs. traditional institutional routes over 10-year periods. Track cross-over rates.',
    'If sufficient: scaffold sunset is real — trapped exit becomes mobile within 10-15 years. If insufficient: alternative pathways are supplementary, not substitutive, and the snare persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_pathway_sufficiency, empirical, 'Whether alternative art distribution provides career viability').

omega_variable(
    institutional_intent_paradox,
    'Do funding bodies genuinely want to support artistic risk, or does the appearance of openness to risk serve as cover for reproducing established hierarchies?',
    'Content analysis of grant rejections: are explicitly risky/experimental proposals rejected more often than safe ones, controlling for quality? Track demographic patterns of risk-averse funding.',
    'If genuine openness: risk aversion is structural inertia (modifiable). If performative: risk aversion is strategic gatekeeping (requires external pressure to change).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_intent_paradox, empirical, 'Whether institutional risk aversion is structural inertia or strategic gatekeeping').

omega_variable(
    innovation_locality_hypothesis,
    'Does artistic innovation emerge primarily from institutional margins or primarily from outside institutions entirely?',
    'Genealogical mapping of major aesthetic movements: trace originating artists'' funding sources and institutional affiliations at time of innovation. Classify as institutional, marginal, or external.',
    'If institutional margins: tangled rope is accurate — innovation happens within mixed coordination-extraction systems. If external: snare is accurate — genuine innovation requires exit from institutional constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_locality_hypothesis, empirical, 'Whether artistic innovation emerges from institutional margins or external sources').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(artistic_risk_aversion, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(art_risk_tr_t0, artistic_risk_aversion, theater_ratio, 0, 0.52).
narrative_ontology:measurement(art_risk_tr_t10, artistic_risk_aversion, theater_ratio, 10, 0.64).
narrative_ontology:measurement(art_risk_tr_t20, artistic_risk_aversion, theater_ratio, 20, 0.71).

% Extraction over time
narrative_ontology:measurement(art_risk_be_t0, artistic_risk_aversion, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(art_risk_be_t10, artistic_risk_aversion, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(art_risk_be_t20, artistic_risk_aversion, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(artistic_risk_aversion, identity_coordination).
narrative_ontology:affects_constraint(artistic_risk_aversion, cultural_gatekeeping).
narrative_ontology:affects_constraint(artistic_risk_aversion, aesthetic_standardization).
narrative_ontology:affects_constraint(artistic_risk_aversion, institutional_legitimacy_hierarchies).

% DUAL FORMULATION NOTE:
% Artistic risk aversion decomposes into two structurally distinct constraints: (1) risk_aversion_as_resource_allocation (ε~0.35, rope with snare elements) — how institutions coordinate aesthetic categories and distribute funding; (2) risk_aversion_as_identity_gatekeeping (ε~0.62, snare with tangled rope elements) — how gatekeeping power suppresses artists' identity self-definition. This story focuses on the resource allocation surface. See cultural_gatekeeping story for the identity-coordination dimension.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(artistic_risk_aversion, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
