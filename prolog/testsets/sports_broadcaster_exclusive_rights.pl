% ============================================================================
% CONSTRAINT STORY: sports_broadcaster_exclusive_rights
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sports_broadcaster_exclusive_rights, []).

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
 *   constraint_id: sports_broadcaster_exclusive_rights
 *   human_readable: Sports Broadcaster Exclusive Rights Licensing
 *   domain: media/sports/entertainment
 *
 * SUMMARY:
 *   Sports broadcaster exclusive rights licensing creates a licensing
 *   constraint that simultaneously funds sports production infrastructure
 *   (coordination function) and restricts fan access, eliminates secondary
 *   media coverage, and enforces geographic blackouts (extraction function).
 *   The constraint exhibits structural characteristics of tangled rope:
 *   genuine coordination (leagues and broadcasters must coordinate to
 *   monetize sports content) paired with asymmetric extraction (fans in
 *   excluded regions bear costs; secondary media outlets are contractually
 *   prohibited; streaming platforms face premium licensing barriers).
 *   Extractiveness has increased over the interval as digital streaming
 *   disrupted traditional broadcast markets, forcing leagues to tighten
 *   exclusive licensing enforcement to maintain revenue concentration. The
 *   constraint's theater ratio (0.35) remains moderate because the
 *   coordination function is real — exclusive licensing does fund
 *   higher-quality production — though regulatory justifications rely on
 *   outdated scarcity assumptions undermined by digital technology.
 *
 * KEY AGENTS:
 *   - League Governing Body: Primary beneficiary (institutional/arbitrage) — captures concentrated broadcast licensing revenue; benefits from revenue predictability and infrastructure funding capacity
 *   - Exclusive Broadcaster: Primary beneficiary (institutional/arbitrage) — receives exclusive distribution rights; benefits from monopoly pricing and subscriber retention lock-in
 *   - Professional Athletes: Secondary beneficiary (moderate/mobile) — salaries funded by broadcast licensing fees; experience constraint as revenue-supporting coordination
 *   - Geographically Restricted Fans: Primary victim (powerless/trapped) — blackout rules prevent access to local teams; geographic fate and contractual enforcement eliminate alternatives
 *   - Secondary Media Outlets: Primary victim (powerless/trapped) — contractual licensing restrictions prohibit historical sports coverage; career contingency on exclusive rights holder decisions
 *   - Streaming Platforms: Secondary victim (moderate/constrained) — must pay premium licensing rates or abandon sports content; strategic options exist but carry high cost
 *   - Sports Fan Coalition: Organized agent (organized/constrained) — advocacy groups and cord-cutting communities challenging exclusivity; have agency but face structural barriers
 *   - Antitrust Regulatory Framework: Institutional actor (institutional/arbitrage) — maintains sports broadcasting exemptions through institutional inertia; sees own regulatory structure as degraded
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sports_broadcaster_exclusive_rights, 0.58).
domain_priors:suppression_score(sports_broadcaster_exclusive_rights, 0.72).
domain_priors:theater_ratio(sports_broadcaster_exclusive_rights, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sports_broadcaster_exclusive_rights, extractiveness, 0.58).
narrative_ontology:constraint_metric(sports_broadcaster_exclusive_rights, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sports_broadcaster_exclusive_rights, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sports_broadcaster_exclusive_rights, tangled_rope).
narrative_ontology:human_readable(sports_broadcaster_exclusive_rights, "Sports Broadcaster Exclusive Rights Licensing").
narrative_ontology:topic_domain(sports_broadcaster_exclusive_rights, "media/sports/entertainment").

domain_priors:requires_active_enforcement(sports_broadcaster_exclusive_rights).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sports_broadcaster_exclusive_rights, league_governing_body).
narrative_ontology:constraint_beneficiary(sports_broadcaster_exclusive_rights, exclusive_broadcaster).
narrative_ontology:constraint_beneficiary(sports_broadcaster_exclusive_rights, professional_athletes).
narrative_ontology:constraint_victim(sports_broadcaster_exclusive_rights, secondary_media_outlets).
narrative_ontology:constraint_victim(sports_broadcaster_exclusive_rights, sports_fans_geographically_restricted).
narrative_ontology:constraint_victim(sports_broadcaster_exclusive_rights, streaming_platforms_excluded).
narrative_ontology:constraint_victim(sports_broadcaster_exclusive_rights, public_sports_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GEOGRAPHICALLY LOCKED FAN (SNARE) — Fans in blackout regions or with language barriers face total exclusion from watching their preferred teams live. No legitimate alternative exists; they cannot watch their local team through conventional means. Maximum suppression and extraction from this perspective — trapped by geographic fate and contractual enforcement.
constraint_indexing:constraint_classification(sports_broadcaster_exclusive_rights, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: SECONDARY MEDIA OUTLET (SNARE) — Regional newspapers, local sports broadcasters, and community media face contractual prohibition from covering games they historically covered. Suppression is near-total: explicit licensing restrictions prevent any alternative content path. Career continuity in sports journalism becomes contingent on exclusive rights holder approval.
constraint_indexing:constraint_classification(sports_broadcaster_exclusive_rights, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: STREAMING PLATFORM (TANGLED ROPE) — Streaming services (Netflix, Amazon Prime, Apple TV+) must either pay premium rates to acquire sports rights or remain excluded from sports content entirely. They benefit from coordination function (sports content drives subscriber retention) but face asymmetric extraction through licensing fees that concentrate bargaining power with leagues. Exit options exist (abandon sports content, negotiate different terms) but carry high strategic cost.
constraint_indexing:constraint_classification(sports_broadcaster_exclusive_rights, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: EXCLUSIVE BROADCASTER (ROPE) — The rights holder experiences the constraint as pure coordination: exclusive rights enable production investment certainty, allowing higher-quality broadcasts that benefit viewers who do have access. Revenue certainty permits long-term infrastructure planning. This perspective experiences extraction flowing TO them, not FROM them — the constraint subsidizes their operations.
constraint_indexing:constraint_classification(sports_broadcaster_exclusive_rights, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LEAGUE GOVERNING BODY (ROPE) — Exclusive licensing creates coordination benefits: leagues can plan revenue, invest in player development, and manage game scheduling with financial predictability. Leagues experience the constraint as solving the collective action problem of monetizing sports entertainment. The constraint is designed to benefit them; they experience it as coordination, not extraction.
constraint_indexing:constraint_classification(sports_broadcaster_exclusive_rights, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PROFESSIONAL ATHLETE (ROPE) — Players benefit from league revenue concentration (higher salaries funded by exclusive broadcasting fees) and from promotional value of broadcasts reaching concentrated audiences. Athletes can exit (retire, move leagues, go independent) but at significant cost. They experience the constraint as coordination: exclusive rights fund their sport's infrastructure.
constraint_indexing:constraint_classification(sports_broadcaster_exclusive_rights, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: SPORTS FAN COALITION (ORGANIZED/GENERATIONAL) — Organized fan groups, cord-cutting advocates, and public access movements see exclusive licensing as extraction mechanism: coordination function exists (funding sports production) but is packaged with asymmetric suppression (blackouts, geofencing, pricing concentration). Organized agents have agency (legal advocacy, streaming piracy alternatives, regulatory pressure) but face high structural costs to exit the extraction.
constraint_indexing:constraint_classification(sports_broadcaster_exclusive_rights, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANTITRUST REGULATORY FRAMEWORK (PITON) — Sports broadcasting has inherited exemptions and special carve-outs from antitrust law (dating from baseball's 1922 exemption). The regulatory framework persists through institutional inertia despite reduced functional justification — modern broadcasting technology no longer requires exclusive licensing for financial viability, yet exemptions persist. The regulatory theater maintains an outdated structure.
constraint_indexing:constraint_classification(sports_broadcaster_exclusive_rights, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / SCARCITY VIEW (MOUNTAIN) — From a long-term analytical view, broadcasting rights create scarcity through contract law: only one entity can broadcast a game live to mass audiences. This scarcity is structurally immutable given current technology (live events can only be watched once, by finite audiences). However, digital distribution has undermined the empirical basis for this view — the constraint is increasingly contingent institutional choice rather than natural scarcity.
constraint_indexing:constraint_classification(sports_broadcaster_exclusive_rights, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sports_broadcaster_exclusive_rights_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sports_broadcaster_exclusive_rights, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sports_broadcaster_exclusive_rights, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sports_broadcaster_exclusive_rights, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sports_broadcaster_exclusive_rights, TR),
    TR >= 0.70.

:- end_tests(sports_broadcaster_exclusive_rights_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint generates significant extraction through licensing fees, blackout rules, and geographic restrictions, but coordination function is genuine — exclusive rights do fund sports infrastructure that benefits all stakeholders. The tension between coordination and extraction is the defining feature of tangled_rope classification. Extractiveness has increased from 0.35 to 0.58 over the interval as streaming disruption forced leagues to tighten licensing enforcement and expand geographic restrictions to defend traditional broadcast licensing models. Suppression (0.72): High. Multiple suppression mechanisms combine: contractual licensing restrictions (secondary media), blackout enforcement (geographic fans), subscription requirements (paywall), technological geofencing (platform-level access control), and antitrust exemptions (regulatory suppression of competitive alternatives). These mechanisms are active, not accidental — the constraint's persistence depends on continuous enforcement. Theater ratio (0.35): Moderate-low. The coordination function is substantive — exclusive licensing does enable production investment and financial planning — so the constraint is not primarily performative. However, regulatory justifications invoke 1920s-era scarcity assumptions that digital technology has undermined. The theater reflects the gap between operational necessity (real production coordination) and regulatory narrative (natural scarcity).
 *
 * PERSPECTIVAL GAP:
 *   Maximum perspectival divergence demonstrates that observed classification depends entirely on structural position. The same constraint appears as rope (pure coordination benefiting all) from the league perspective, snare (pure extraction with no alternatives) from the geographically locked fan perspective, and tangled rope (mixed coordination and extraction with constrained exits) from the streaming platform perspective. This gap is not measurement error — it reflects real structural differences in how the constraint operates on different agents. The constraint's claimed type (tangled rope) is the analytical consensus that incorporates all perspectives: coordination is real, extraction is real, and the mechanism requires active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position within the licensing constraint. Leagues and broadcasters (beneficiaries with arbitrage options) experience low d → negative χ (extraction flows TO them). Geographically restricted fans (victims, trapped) experience high d → high χ (maximum experienced extraction). Secondary media outlets (victims, trapped by contractual restriction) experience high d → high χ. Streaming platforms (victims with mobile options but high switching costs) experience moderate d → moderate χ. Athletes (beneficiaries but with constrained mobility — exit requires changing careers) experience low d from beneficiary status but are partially offset by constrained exit options. Organized fan coalitions (victims with agency and exit options) experience moderate d → moderate χ because their constraint is high-cost but not insurmountable. The piton perspective derives from regulatory theater (exemptions persist despite reduced functional justification) rather than from high experienced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint resolves mandatrophy by demonstrating that sports licensing is genuinely hybrid. The coordination function is not pretense: exclusive licensing does fund infrastructure that benefits athletes, produces higher-quality broadcasts, and enables stable revenue planning for leagues and broadcasters. The extraction is not incidental: suppression mechanisms (blackouts, geofencing, licensing restrictions) are active, intentional, and asymmetric, concentrating costs on fans and secondary media while concentrating benefits on leagues and broadcasters. Neither function parasitizes the other — both are structural and necessary. The constraint persists because the coordination benefits to leagues/broadcasters exceed the extraction costs they face, while the extraction benefits to leagues/broadcasters exceed the coordination benefits to suppressed agents. This is the defining structure of tangled rope: genuine coordination plus genuine asymmetric extraction, mutually enabling, continuously enforced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_boundary,
    'Does exclusive licensing primarily fund sports production (coordination function) or primarily extract rents from fans and secondary media (extraction function)?',
    'Comparative analysis: (1) Production value increases with exclusivity; (2) Fan welfare metrics (access, affordability, viewing quality); (3) League revenue correlation with broadcast quality; (4) Counterfactual: would sports production investment exist under shared-licensing models?',
    'If coordination-dominant: constraint is primarily rope, benefiting all stakeholders through infrastructure funding. If extraction-dominant: constraint is primarily snare, with league/broadcaster coordination parasitic on fan/media suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Whether exclusivity funds production or primarily extracts rents').

omega_variable(
    technological_scarcity_decay,
    'Has digital distribution technology eliminated the natural scarcity that justified exclusive broadcasting rights, or does scarcity persist for live event viewership?',
    'Technical analysis: (1) Simultaneous viewing capacity of digital platforms vs traditional broadcast; (2) Production cost differentials between exclusive and non-exclusive models; (3) International examples with shared-licensing or open-access models (public broadcasters in Europe, cricket streaming in India); (4) Historical trends in broadcast infrastructure costs',
    'If scarcity eliminated: constraint transitions from mountain (natural law) to tangled_rope/snare (institutional choice). Mountain classification becomes false summit. If scarcity persists: mountain classification is accurate — exclusivity remains structurally necessary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_scarcity_decay, empirical, 'Whether technological change has eliminated broadcasting scarcity').

omega_variable(
    antitrust_exemption_justification,
    'Do sports broadcasting antitrust exemptions remain justified by public-interest coordination functions, or have they become rent-preservation mechanisms?',
    'Regulatory analysis: (1) Historical rationale for exemptions vs current sports industry structure; (2) Comparison with other exempt industries (telecommunications before deregulation); (3) Public welfare metrics under exemption vs hypothetical competitive regime; (4) Legislative intent review and subsequent industry structural changes',
    'If justified: exemptions represent legitimate recognition that sports coordination requires collective action. If unjustified: exemptions enable extraction that would be illegal in other sectors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(antitrust_exemption_justification, conceptual, 'Whether antitrust exemptions serve legitimate sports coordination').

omega_variable(
    blackout_mechanism_justification,
    'Do local blackout rules coordinate venue attendance and local broadcast revenue (coordination function) or primarily extract maximum price from different fan segments (extraction function)?',
    'Economic data: (1) Correlation between blackouts and stadium attendance; (2) Revenue comparison of teams with aggressive blackouts vs open-access policies; (3) Fan surveys on blackout perception; (4) International examples without blackouts (most non-US sports leagues); (5) Substitution analysis: do blacked-out fans attend games, or simply watch illegal streams?',
    'If attendance-driven: blackouts are coordination mechanism protecting venue revenue. If substitution-driven: blackouts extract through price discrimination, not venue attendance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(blackout_mechanism_justification, empirical, 'Whether blackouts coordinate attendance or extract through price discrimination').

omega_variable(
    geographic_restriction_mechanism,
    'Are geographic restrictions (geofencing, regional exclusivity) necessary to fund regional production and local broadcast infrastructure, or are they pure price discrimination mechanisms?',
    'Production cost analysis: (1) Incremental cost of regional production vs national broadcast; (2) Regional broadcast licensing rates vs marginal cost of serving additional regions; (3) Comparative analysis of sports with and without regional restrictions; (4) Digital service deployment analysis: technical cost of simultaneous global distribution vs contractual restrictions',
    'If production-justified: geofencing is tangled_rope coordination. If price-discrimination: geofencing is snare extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_restriction_mechanism, empirical, 'Whether geographic restrictions justify regional production costs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sports_broadcaster_exclusive_rights, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sber_tr_t0, sports_broadcaster_exclusive_rights, theater_ratio, 0, 0.28).
narrative_ontology:measurement(sber_tr_t10, sports_broadcaster_exclusive_rights, theater_ratio, 10, 0.31).
narrative_ontology:measurement(sber_tr_t20, sports_broadcaster_exclusive_rights, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(sber_be_t0, sports_broadcaster_exclusive_rights, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sber_be_t10, sports_broadcaster_exclusive_rights, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(sber_be_t20, sports_broadcaster_exclusive_rights, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sports_broadcaster_exclusive_rights, resource_allocation).
narrative_ontology:affects_constraint(sports_broadcaster_exclusive_rights, sports_content_distribution_access).
narrative_ontology:affects_constraint(sports_broadcaster_exclusive_rights, media_industry_gatekeeping).

% DUAL FORMULATION NOTE:
% Sports broadcaster exclusive rights is upstream of sports content distribution access (different constraint, different epsilon: distribution addresses availability and format; licensing addresses exclusivity and pricing). The network links show how exclusive licensing enforces downstream distribution barriers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sports_broadcaster_exclusive_rights, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
