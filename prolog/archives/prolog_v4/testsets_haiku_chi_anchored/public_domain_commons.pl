% ============================================================================
% CONSTRAINT STORY: public_domain_commons
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_domain_commons, []).

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
 *   constraint_id: public_domain_commons
 *   human_readable: The Public Domain as a Cultural Commons
 *   domain: legal/economic/social
 *
 * SUMMARY:
 *   The public domain constraint is the legal-structural mechanism defining
 *   which creative works belong to no one and everyone. This constraint
 *   exhibits a critical tension: it is simultaneously the foundation of
 *   cultural commons, a victim of institutional capture, and an object of
 *   systematic extraction through term extension, technological restriction,
 *   and complexity. From 1923 to 2023, the effective size and accessibility
 *   of the public domain has degraded through multiple mechanisms: (1)
 *   Copyright Term Extension Acts (Sonny Bono, EU harmonization) moved the
 *   entry threshold forward; (2) Digital Rights Management and API walls
 *   restrict access to public domain works; (3) Orphan works and digitization
 *   barriers make nominally free works practically inaccessible; (4)
 *   Corporate consolidation of publishing and distribution concentrates
 *   control over public domain access. The constraint demonstrates all six
 *   types from different structural positions: pure extraction (Snare for
 *   powerless creators), mixed coordination-extraction (Tangled Rope for
 *   memory institutions), net benefit (Rope for publishers), temporary
 *   problem with reform path (Scaffold for open-culture movement), degraded
 *   ritual (Piton for copyright apparatus), and false naturalization
 *   (Mountain for civilizational scarcity view). The theater_ratio has risen
 *   from 0.35 (1923) to 0.58 (2023), indicating increasing performative
 *   content in copyright administration — the regime maintains ceremonial
 *   registration and enforcement while actual control flows through corporate
 *   technological and contractual mechanisms outside the legal system.
 *
 * KEY AGENTS:
 *   - The Public Domain Itself: Victim (powerless/trapped) — abstract collective good bearing costs of term extension and access barriers
 *   - Downstream Creators (Small & Independent): Victims (powerless/trapped) — face legal uncertainty, access costs, and resource barriers despite theoretical free use
 *   - Cultural Memory Institutions: Mixed (moderate/constrained) — benefit from preservation function but constrained by costs and legal complexity
 *   - Publishing & Distribution Intermediaries: Beneficiaries (institutional/arbitrage) — profit from low-cost access to public domain content for digitization and repackaging
 *   - IP-Dependent Corporations: Primary extractors (powerful/mobile) — actively restrict public domain through term extension, DRM, and control of distribution channels
 *   - Open Culture Movement: Organized reformers (organized/constrained) — see constraint as temporary, building alternative pathways (Creative Commons, Wikipedia, open-access infrastructure)
 *   - Copyright Regime Apparatus: Institutional inertia (institutional/arbitrage) — maintains performative enforcement ritual while losing functional control to private actors
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing policy failure as scarcity law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_domain_commons, 0.52).
domain_priors:suppression_score(public_domain_commons, 0.65).
domain_priors:theater_ratio(public_domain_commons, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_domain_commons, extractiveness, 0.52).
narrative_ontology:constraint_metric(public_domain_commons, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(public_domain_commons, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_domain_commons, tangled_rope).
narrative_ontology:human_readable(public_domain_commons, "The Public Domain as a Cultural Commons").
narrative_ontology:topic_domain(public_domain_commons, "legal/economic/social").

domain_priors:requires_active_enforcement(public_domain_commons).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_domain_commons, cultural_creators).
narrative_ontology:constraint_beneficiary(public_domain_commons, public_access_stakeholders).
narrative_ontology:constraint_beneficiary(public_domain_commons, derivative_work_producers).
narrative_ontology:constraint_victim(public_domain_commons, public_domain_integrity).
narrative_ontology:constraint_victim(public_domain_commons, downstream_creators).
narrative_ontology:constraint_victim(public_domain_commons, cultural_memory_institutions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PUBLIC DOMAIN INTEGRITY (SNARE) — The public domain as an abstract commons cannot advocate for itself. It bears the cost of expansionist IP law (term extension, technological restriction, orphan works capture) with no exit mechanism. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.77. Pure extraction dressed as cultural stewardship.
constraint_indexing:constraint_classification(public_domain_commons, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DOWNSTREAM CREATORS (SNARE) — Emerging artists, educators, and small cultural institutions lack resources to navigate orphan works, digitization barriers, and legal uncertainty around public domain reuse. Trapped by access costs and legal risk despite theoretical free use. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(public_domain_commons, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: CULTURAL MEMORY INSTITUTIONS (TANGLED ROPE) — Libraries and archives benefit from the public domain (enabling their preservation mission) while being constrained by digital rights management, term extension, and resource scarcity for digitization. d≈0.70, f(d)≈1.06, σ=1.0 → χ≈0.55. Mixed: coordination function (preservation) + extraction (cost barriers).
constraint_indexing:constraint_classification(public_domain_commons, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PUBLISHING & DISTRIBUTION INTERMEDIARIES (ROPE) — Large publishers and platforms benefit from low-friction access to public domain content for digitization, indexing, and repackaging. The constraint enables their coordination function (distributing cultural goods). d≈0.10, f(d)≈-0.05, σ=1.2 → χ≈-0.03. Net beneficiary; negative effective extraction.
constraint_indexing:constraint_classification(public_domain_commons, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: IP-DEPENDENT CORPORATIONS (SNARE) — Large entertainment, pharmaceutical, and tech corporations extract value by expanding IP terms, restricting public domain access through technological means (DRM, API restrictions), and controlling derivative works. Organized powerful agents using suppression (legal complexity, technological barriers) to reduce the public domain's functional size. d≈0.20, f(d)≈0.15, σ=1.2 → χ≈0.36. But their mobility and power means they are not trapped — they CHOOSE extraction because it's profitable. Snare with voluntary participation from extractors.
constraint_indexing:constraint_classification(public_domain_commons, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: OPEN CULTURE MOVEMENT (SCAFFOLD) — Creative Commons, Wikipedia, open-access advocates, and copyright reformers see the public domain bottleneck as temporary, solvable through licensing, policy reform, and decentralized access infrastructure. has_sunset_clause: true (estimated 15-30 year horizon for full reform). d≈0.45, f(d)≈0.48, σ=1.2 → χ≈0.30. Low extraction because organized agents have agency and see a path forward.
constraint_indexing:constraint_classification(public_domain_commons, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: COPYRIGHT REGIME (PITON) — Patent offices, copyright registries, and enforcement bureaucracies persist largely as performative theater. Most enforcement is by private actors (corporations), not state machinery. The regime's nominal function (protecting authors) has been degraded by capture to serve corporate interests. theater_ratio≈0.65 (ritualized registration, ceremony without function for marginal creators). d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.07.
constraint_indexing:constraint_classification(public_domain_commons, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / SCARCITY VIEW (MOUNTAIN) — From a civilizational perspective, the public domain is subject to an irreducible scarcity of human attention, capital, and institutional capacity for stewardship. Some works will degrade, some will be lost, some will be locked behind paywalls simply because no one has bandwidth to preserve them. This perspective risks naturalizing what is actually policy failure as an immutable law of cultural memory. ε=0.52, suppression=0.65, theater=0.58 contradicts this framing — institutional decisions (term extension, DRM legalization, funding cutbacks) are not laws of nature.
constraint_indexing:constraint_classification(public_domain_commons, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_domain_commons_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(public_domain_commons, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(public_domain_commons, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_domain_commons, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(public_domain_commons, TR),
    TR >= 0.70.

:- end_tests(public_domain_commons_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The public domain faces systematic extraction through multiple vectors: (1) term extension that moves entry threshold forward; (2) DRM legalization that restricts access to nominally free works; (3) orphan works capture through deliberate resource starvation; (4) corporate consolidation that gatekeeps distribution. The extraction is not absolute (many works are accessible, digitization is progressing), but the trajectory is toward shrinkage and consolidation. Suppression (0.65): High. Significant barriers to public domain function include: legal complexity (orphan works, rights-holder traceability), technological restrictions (DRM, API walls), institutional barriers (digitization costs, preservation capacity), and active legal obstruction (DMCA circumvention rules, term extension). The constraint has been deliberately complicated to make free use expensive. Theater ratio (0.58): Moderate-high. The copyright regime combines real function (some author protection, some legitimate rights management) with substantial theater: ceremonial registration processes, enforcement rituals largely delegated to private actors, and performative appeals to author welfare that mask corporate capture. The rise from 0.35 (1923) to 0.58 (2023) reflects increasing corporate control through technological and contractual means outside the ceremonial legal system.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates fundamental disagreement on classification across structural positions. The public domain itself (powerless) sees pure extraction (Snare) — no defense mechanism, no exit. Downstream creators (powerless but organized) see snare-like conditions (trapped by cost and legal uncertainty). Memory institutions (moderate) see mixed function and constraint (Tangled Rope) — they benefit from the commons but are squeezed by digitization costs and legal complexity. Publishers (institutional) see coordination benefit (Rope) — low-cost access to materials for repackaging and distribution. Corporations (powerful) see snare-like conditions in reverse — they extract by restricting the commons. The open-culture movement (organized) sees a temporary problem with a reform path (Scaffold) — current restrictions are real but solvable through licensing, policy, and decentralized infrastructure. The copyright apparatus itself (institutional) sees degraded function (Piton) — nominally controlling access but actually dominated by corporate private ordering. The analytical observer risks seeing immutable scarcity (Mountain) — 'cultural memory is inherently scarce, some works will be lost' — but the base properties reveal this as naturalization of policy choices, not law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Public domain itself: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction for an agent that cannot exit. Downstream creators: Victim + trapped → d≈0.92, f(d)≈1.38. High extraction for those bearing costs without exit. Memory institutions: Victim + constrained + beneficiary of commons → d≈0.70, f(d)≈1.06. Mixed because they have some agency (digitization initiatives, institutional partnerships) but constrained by resources and legal complexity. Publishers: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.05. Net beneficiary; can arbitrage access. IP corporations: Powerful + mobile but choosing extraction → d≈0.20, f(d)≈0.15. Moderate extraction because they have power to exit (they could compete on innovation instead of restriction) but choose not to. Copyright apparatus: Institutional + arbitrage → d≈0.05, f(d)≈-0.12. Piton classification from theater gate, not from high chi. Open-culture movement: Organized + constrained but with agency → d≈0.45, f(d)≈0.48. Low extraction because they see a path forward. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival and false.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the public domain is a structural constraint that produces genuinely different classifications from different positions. The mandate (to maintain a vibrant public domain) is neither uniformly achievable nor uniformly thwarted. For powerless agents (the public domain itself, small creators), the constraint classifies as Snare: pure extraction with no exit. For institutional beneficiaries (publishers), it classifies as Rope: coordination with mutual benefit. For reformers (open-culture movement), it classifies as Scaffold: a temporary problem being solved. For the apparatus itself, it classifies as Piton: degraded ritual maintained by inertia. The constraint is NOT misclassified as pure coordination (Rope) — the multiple victim declarations (public_domain_integrity, downstream_creators, cultural_memory_institutions) ensure that asymmetric extraction is captured. The constraint is NOT misclassified as pure extraction (Snare) — the beneficiary declarations (cultural_creators, public_access_stakeholders, derivative_work_producers) ensure that coordination function is captured. The Tangled Rope classification at the analytical level correctly models that the constraint simultaneously enables creation (coordination function) and constrains creators (extraction mechanism). The mandatrophy is thus resolved: all six types are locally valid from different positions; Tangled Rope is the global classification because the constraint simultaneously serves coordination and extraction functions, with the asymmetry (benefiting publishers and IP corporations at the expense of creators and memory institutions) being structural rather than accidental.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    orphan_works_boundary,
    'What proportion of public domain works are functionally inaccessible due to orphan status (authors untraceable), and does this constitute a failure of the public domain constraint itself?',
    'Empirical survey: systematic sampling of pre-1930 published works; tracking digitization rates and access availability; correlation between copyright status and actual user access',
    'If >60% functionally orphaned: public domain is nominal (Piton from memory institution view). If <20%: public domain is functionally accessible (Rope or Scaffold from reformer view).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(orphan_works_boundary, empirical, 'What fraction of public domain works are functionally inaccessible due to orphan status').

omega_variable(
    term_extension_escape_velocity,
    'Has copyright term extension reached a point where the public domain is shrinking in absolute terms (new works entering faster than old works pass into public domain)?',
    'Historical analysis of copyright law changes; modeling of effective copyright duration across jurisdictions; projection of entry-into-public-domain rates under current law',
    'If escape velocity reached: public domain is trapped in secular decline (Snare from institutional view). If not: reform still has time (Scaffold realistic).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(term_extension_escape_velocity, empirical, 'Has copyright term extension created irreversible public domain shrinkage').

omega_variable(
    digital_preservation_sufficiency,
    'Can decentralized digital preservation (Wikipedia Commons, Internet Archive, institutional repositories) functionally replace state-mandated public domain enforcement, or does the constraint require centralized legal backing?',
    'Empirical analysis of digital preservation sustainability: funding models, redundancy, legal risks; comparison of preservation outcomes under decentralized vs state-backed models',
    'If decentralized sufficient: scaffold sunset is realistic, open-culture path viable. If not: constraint remains snare-like absent legal reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_preservation_sufficiency, empirical, 'Whether decentralized digital preservation can sustain public domain without state enforcement').

omega_variable(
    technological_restriction_scope,
    'Do technological restrictions (DRM, API walls, terms of service) on public domain content constitute a legal circumvention of the public domain, or are they merely market friction?',
    'Analysis of case law (DMCA circumvention, API ToS disputes); empirical measurement of practical access rates with and without restrictions; assessment of legal enforceability of restrictions on public domain material',
    'If restrictions are enforceable: public domain is hollow (Snare from creator view). If not enforceable: restrictions are theater (Piton observation confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_restriction_scope, conceptual, 'Whether technological restrictions effectively override legal public domain status').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_domain_commons, 1923, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pubdom_theater_1923, public_domain_commons, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pubdom_theater_1973, public_domain_commons, theater_ratio, 50, 0.48).
narrative_ontology:measurement(pubdom_theater_2023, public_domain_commons, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(pubdom_extract_1923, public_domain_commons, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(pubdom_extract_1973, public_domain_commons, base_extractiveness, 50, 0.42).
narrative_ontology:measurement(pubdom_extract_2023, public_domain_commons, base_extractiveness, 100, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_domain_commons, global_infrastructure).
narrative_ontology:affects_constraint(public_domain_commons, copyright_term_extension).
narrative_ontology:affects_constraint(public_domain_commons, orphan_works_capture).
narrative_ontology:affects_constraint(public_domain_commons, digital_rights_management).
narrative_ontology:affects_constraint(public_domain_commons, knowledge_commons_fragmentation).

% DUAL FORMULATION NOTE:
% The public domain constraint forms a family with four related constraints: (1) copyright_term_extension (ε≈0.42, Snare) — the specific legal mechanism for shrinking the public domain; (2) orphan_works_capture (ε≈0.48, Tangled Rope) — the specific mechanism through which nominally free works become functionally inaccessible; (3) digital_rights_management (ε≈0.55, Snare) — the specific technological mechanism for restricting access to public domain materials; (4) knowledge_commons_fragmentation (ε≈0.38, Tangled Rope) — the broader structural consequence when public domain access becomes fragmented across proprietary platforms. The public_domain_commons constraint is upstream of all four — these are implementation mechanisms. ε values differ because each constraint has different observable (legal term length, orphan status tracking, DRM technical specificity, commons fragmentation measure) but all depend on the underlying public domain constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(public_domain_commons, powerful, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
