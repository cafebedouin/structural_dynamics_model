% ============================================================================
% CONSTRAINT STORY: international_publishing_access
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_international_publishing_access, []).

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
 *   constraint_id: international_publishing_access
 *   human_readable: International Publishing Access Constraint
 *   domain: academic/publishing/global_knowledge_distribution
 *
 * SUMMARY:
 *   The international publishing access constraint creates a structural
 *   asymmetry between knowledge producers in resource-rich institutions and
 *   those in low-resource or Global South contexts. Legacy academic
 *   publishers maintain control over global knowledge distribution through
 *   subscription-based access, creating extraction that flows from
 *   researchers, libraries, and institutions toward publisher profit centers.
 *   Simultaneously, the system coordinates legitimate peer review, quality
 *   filtering, and curation functions — making it a hybrid
 *   coordination-extraction mechanism (Tangled Rope) rather than pure
 *   extraction. The constraint exhibits all six types from different
 *   perspectives. The powerless researcher at an underfunded institution sees
 *   pure extraction (Snare): $3,000+ annual journal subscriptions are
 *   unaffordable, creating systematic exclusion. The Global South scientific
 *   community experiences generational suppression (Snare) — both
 *   participation barriers (publication fees) and access barriers
 *   (subscription walls) create systematic exclusion across decades. Legacy
 *   publishers experience coordination (Rope) — they genuinely solve problems
 *   of peer review, copyediting, indexing, and distribution infrastructure.
 *   Well-resourced universities experience mixed coordination and extraction
 *   (Tangled Rope) — they benefit from access but face escalating costs that
 *   squeeze other budgets. The open access coalition sees a temporary problem
 *   with an exit path (Scaffold) — arXiv, PubMed Central, and open-access
 *   mandates are building alternative distribution mechanisms with a
 *   generational sunset. The copyright and IP legal framework appears
 *   functionally necessary but is increasingly performed theater (Piton) —
 *   elaborate licensing restricts digitally-copyable information to justify
 *   artificial scarcity. The analytical observer risks naturalizing this as
 *   inherent to knowledge governance (Mountain) — treating publisher
 *   gatekeeping as immutable — but the structural data reveals contingent
 *   institutional arrangements. Theater ratio (0.68) reflects substantial
 *   performance: impact factor manipulation, peer review theater, prestige
 *   signaling, and copyright enforcement create impression of necessity for
 *   arrangements whose actual digital distribution costs are minimal. Base
 *   extractiveness (0.58) reflects sustained revenue capture that has
 *   accelerated as journal prices increase 5-7% annually while publication
 *   volume grows — an extraction trajectory. Suppression (0.65) reflects
 *   systematic barriers to access and participation concentrated on
 *   resource-poor actors.
 *
 * KEY AGENTS:
 *   - Researchers at Low-Resource Institutions: Primary victims (powerless/trapped) — cannot afford subscription access; systematic exclusion from knowledge commons
 *   - Global South Scientific Communities: Primary victims (powerless/trapped, generational horizon) — face both publication cost barriers and access barriers; creates regional scientific capacity collapse
 *   - Knowledge Commons Itself: Secondary victim (powerless/trapped) — abstract collective good that cannot organize; bears cost of fragmentary knowledge distribution
 *   - Legacy Academic Publishers: Primary beneficiaries (institutional/arbitrage) — capture subscription revenue, author processing fees, and control over prestige signaling
 *   - Well-Resourced Universities: Secondary beneficiary constrained victim (institutional/constrained) — benefit from access but experience cost escalation; partially extractive relationship through license agreements
 *   - Open Access Coalition: Organized challengers (organized/constrained) — building alternative distribution pathways; experience constraint as temporary with exit mechanism
 *   - Copyright/IP Legal Framework: Institutional architecture (institutional/arbitrage) — maintains extraction through legal apparatus; increasingly performative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(international_publishing_access, 0.58).
domain_priors:suppression_score(international_publishing_access, 0.65).
domain_priors:theater_ratio(international_publishing_access, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(international_publishing_access, extractiveness, 0.58).
narrative_ontology:constraint_metric(international_publishing_access, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(international_publishing_access, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(international_publishing_access, tangled_rope).
narrative_ontology:human_readable(international_publishing_access, "International Publishing Access Constraint").
narrative_ontology:topic_domain(international_publishing_access, "academic/publishing/global_knowledge_distribution").

domain_priors:requires_active_enforcement(international_publishing_access).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(international_publishing_access, legacy_publishers).
narrative_ontology:constraint_beneficiary(international_publishing_access, wealthy_institutions).
narrative_ontology:constraint_victim(international_publishing_access, researchers_low_resource_institutions).
narrative_ontology:constraint_victim(international_publishing_access, global_south_scientific_communities).
narrative_ontology:constraint_victim(international_publishing_access, knowledge_accessibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RESEARCHER AT LOW-RESOURCE INSTITUTION (SNARE) — Cannot access published research due to subscription costs ($3,000+ per journal annually); trapped by institutional budget constraints with no alternative access mechanism. Bears full extraction cost; no meaningful exit options within the formal publishing system.
constraint_indexing:constraint_classification(international_publishing_access, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GLOBAL SOUTH SCIENTIFIC COMMUNITIES (SNARE) — Structural inability to participate in global research conversation; publication fees ($2,500+ per article) exclude researchers from publishing their own work; subscription barriers prevent accessing others' research. Creates generational suppression — entire regions systematically excluded from knowledge commons.
constraint_indexing:constraint_classification(international_publishing_access, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: LEGACY ACADEMIC PUBLISHERS (ROPE) — Experience the constraint as coordination: managing peer review, copyediting, indexing, and distribution infrastructure for the global research community. Capture subscription revenue and author processing fees. Net beneficiary with exit options (can transition pricing models, negotiate licenses) — extraction runs toward this agent.
constraint_indexing:constraint_classification(international_publishing_access, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: WELL-RESOURCED UNIVERSITY LIBRARY (TANGLED ROPE) — Constrained by rising subscription costs (budget increases 5-7% annually while publication volume grows); benefits from access to full journal portfolio; partially extractive relationship where libraries subsidize publisher profits through license agreements. Some agency through consortial negotiation but significant cost escalation.
constraint_indexing:constraint_classification(international_publishing_access, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: OPEN ACCESS MOVEMENT COALITION (TANGLED ROPE) — Organized agents (arXiv, PubMed Central, Creative Commons, funder mandates) building alternative distribution pathways; benefits from coordination mechanisms (shared infrastructure, author incentives); constrained by publisher control of prestige/peer review and institutional inertia favoring legacy systems. Ongoing active enforcement struggle with some coordination function emerging.
constraint_indexing:constraint_classification(international_publishing_access, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: COPYRIGHT AND IP FRAMEWORK (PITON) — The legal architecture (copyright extension, Digital Millennium Copyright Act, licensing restrictions) that maintains publisher extraction persists through institutional inertia despite functional arguments for open access. Theater ratio high: elaborate legal frameworks create appearance of necessity while actual knowledge transfer cost is minimal. Degraded system maintained by formal apparatus rather than genuine function.
constraint_indexing:constraint_classification(international_publishing_access, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN RISK) — From civilizational perspective, risks naturalizing the constraint as inevitable: 'peer review always requires gatekeepers,' 'quality control requires filtering,' 'distribution infrastructure always has costs.' However, structural data reveals these are contingent institutional choices, not laws of nature. False summit alert: the constraint appears unchangeable only when alternative coordination mechanisms are ignored.
constraint_indexing:constraint_classification(international_publishing_access, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(international_publishing_access_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(international_publishing_access, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(international_publishing_access, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(international_publishing_access, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(international_publishing_access, TR),
    TR >= 0.70.

:- end_tests(international_publishing_access_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Publishers capture significant margin beyond real distribution costs; subscription revenue growth (5-7% annually) exceeds publication volume growth and inflation. The omega on real distribution costs is critical — if digital distribution costs are 10% of subscription revenue, extractiveness is justified at 0.58+; if costs are 30%, extractiveness may be inflated. Suppression (0.65): High. Multiple barriers reduce access: subscription costs, publication fees ($2,500+ per article), lack of institutional bandwidth for negotiation, copyright restrictions on sharing, and technical barriers to discovery in Global South. Global South researchers face geographic, economic, and linguistic suppression integrated into the system. Theater ratio (0.68): High-moderate. Peer review is genuine coordination function but increasingly performs secondary role in prestige gatekeeping and artificial scarcity maintenance. Impact factor manipulation, prestige signaling, and copyright enforcement apparatus create elaborate theater around what is fundamentally an information distribution problem with minimal real cost in digital age.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. Powerless researchers trapped at underfunded institutions perceive pure extraction (Snare) with no exit options — they pay high costs or forgo knowledge access. Publishers perceive genuine coordination (Rope) — they solve real problems of peer review, editorial curation, and quality control. Well-resourced universities perceive mixed experience (Tangled Rope) — they benefit from access but experience extraction through escalating costs. The open access coalition perceives a temporary problem with a clear exit path (Scaffold) — alternative distribution mechanisms are maturing. The copyright framework appears necessary by design (legal theater, Piton). The civilizational analytical observer risks the false summit of seeing this as immutable (Mountain) — treating publisher gatekeeping as inherent to knowledge governance rather than contingent institutional arrangement. The perspectival gap reveals that the constraint's classification depends entirely on the observer's structural position: access and power, not objective truth about what publishing requires.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation maps structural positions to effective extractiveness experienced. Publishers as beneficiaries with arbitrage options (can shift pricing models, transition to hybrid OA) experience low directionality d ≈ 0.15-0.20, producing negative or minimal effective extraction χ. They are net extractors from the system. Powerless trapped researchers at low-resource institutions experience maximum directionality d ≈ 0.95, producing maximum effective extraction χ. They are the extraction targets. Well-resourced universities as constrained institutional actors experience mid-range d ≈ 0.60, moderate extraction χ. The open access coalition as organized agents with some exit capacity experience lower d ≈ 0.45, experiencing the constraint as solvable rather than immutable. The copyright framework as institutional infrastructure with institutional arbitrage options experiences low d ≈ 0.15-0.25. The Global South scientific community as powerless trapped agents at civilizational timescale experiences d ≈ 0.92, maximum extraction. This directionality structure reflects that the constraint systematically extracts from those least able to resist and least able to exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing genuine coordination function within an extractive architecture. The legitimate coordination services (peer review, editorial curation, indexing, global distribution) are not illusory — they are real and necessary. But the constraint's extractiveness (0.58) cannot be explained by coordination function alone. The base extractiveness far exceeds what legitimate peer review and distribution infrastructure cost in the digital age. The mandatrophy dissolves when we recognize: (1) Coordination function is real (justifying some suppression and cost); (2) Extraction mechanism is also real (subscription pricing and artificial scarcity extraction beyond coordination cost); (3) Both coexist because the coordination function provides legitimate justification for the extraction apparatus. The architecture captures value from powerless agents by bundling essential services (peer review, quality filtering) with exploitative pricing. Preventing mandatrophy misclassification requires the tangled_rope classification to specify both the coordination function (peer review that cannot be easily replicated) and the asymmetric extraction (pricing power that bears no relationship to actual cost). The piton classification of the legal framework reveals that copyright apparatus is increasingly performative theater maintaining artificial scarcity rather than serving legitimate IP protection.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    peer_review_necessity,
    'Is peer review gatekeeping essential for research quality or a contingent institutional filter that could be replaced by distributed post-publication scrutiny?',
    'Comparative analysis of preprint arXiv communities vs journal-gated research; tracking error correction rates and citation impact; identifying domains where post-publication review is functionally equivalent',
    'If gatekeeping essential: peer review as coordination function justified (higher base coordination cost, lower extractiveness). If contingent: gatekeeping is pure extraction mechanism, extractiveness increases to 0.68+',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peer_review_necessity, empirical, 'Whether peer review gatekeeping is structurally necessary or contingent').

omega_variable(
    real_distribution_costs,
    'What are actual per-article distribution and curation costs versus captured subscription revenue ($10,000-50,000 per journal annually from universities)?',
    'Direct cost accounting from open-access publishers; comparison with publisher claimed infrastructure costs; analysis of digital distribution overhead in information age',
    'If real costs 5-10% of revenue: extractiveness confirmed at high level (0.58+). If real costs approach 50% of revenue: base extractiveness should be reduced to 0.35-0.40 (higher legitimate coordination cost)',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(real_distribution_costs, empirical, 'Actual per-article distribution costs versus subscription revenue').

omega_variable(
    global_south_participation_ceiling,
    'Is the exclusion of Global South researchers from publishing/access a suppression mechanism of the constraint or a separate structural inequality with different ε?',
    'Decomposition analysis: separate story for global inequality vs. publishing access constraint; correlation between access barriers and systemic resource inequality',
    'If integrated: suppression is 0.65 as measured (Global South bears disproportionate cost). If separate: publishing access constraint may have lower suppression (0.50) and global inequality is distinct constraint family member',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_south_participation_ceiling, conceptual, 'Whether Global South exclusion is intrinsic to publishing constraint or separate inequality').

omega_variable(
    open_access_sustainability,
    'Can distributed open-access infrastructure (arXiv, preprint servers, institutional repositories, open-journal platforms) sustainably replace legacy publisher functions at comparable quality and permanence?',
    'Long-term viability analysis of OA platforms; funding stability of community infrastructure; comparison of archival reliability and discovery mechanisms; adoption curves in different disciplines',
    'If sustainable replacement possible: scaffold perspective confirmed with clear sunset (extractiveness trajectory declining as OA matures). If OA models fragile: current alternative architecture unreliable, scaffold sunset is aspirational not structural (extractiveness remains sustained)',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_access_sustainability, empirical, 'Sustainability of open-access infrastructure as legacy publisher replacement').

omega_variable(
    prestige_as_coordination_vs_extraction,
    'Is journal prestige and impact factor a coordination mechanism enabling quality signaling or primarily an extraction mechanism enabling artificial scarcity?',
    'Citation analysis of preprint vs journal versions of same work; comparison of discovery efficiency in prestige vs non-prestige journals; correlation between prestige and actual research quality vs. prestige and author institution status',
    'If primarily coordination: justifies moderate suppression and base extractiveness. If primarily extraction: prestige is theater masking scarcity mechanism, increases theater_ratio understanding and may justify extractiveness increase',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prestige_as_coordination_vs_extraction, empirical, 'Whether journal prestige functions as quality coordination or artificial scarcity extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(international_publishing_access, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pubaccess_tr_t0, international_publishing_access, theater_ratio, 0, 0.55).
narrative_ontology:measurement(pubaccess_tr_t10, international_publishing_access, theater_ratio, 10, 0.62).
narrative_ontology:measurement(pubaccess_tr_t20, international_publishing_access, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(pubaccess_be_t0, international_publishing_access, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(pubaccess_be_t10, international_publishing_access, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(pubaccess_be_t20, international_publishing_access, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(international_publishing_access, information_standard).
narrative_ontology:affects_constraint(international_publishing_access, research_reproducibility_verification).
narrative_ontology:affects_constraint(international_publishing_access, global_scientific_capacity_inequality).
narrative_ontology:affects_constraint(international_publishing_access, academic_career_prestige_signaling).

% DUAL FORMULATION NOTE:
% International publishing access constrains three distinct downstream constraints: (1) reproducibility verification (blocked by paywall access to methods/data); (2) scientific capacity in Global South (participation barriers exclude researchers from capability development); (3) prestige signaling in academia (artificial scarcity maintained through journal gatekeeping). Each downstream constraint has distinct ε and should be analyzed separately while understanding this upstream constraint as causal factor.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(international_publishing_access, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
