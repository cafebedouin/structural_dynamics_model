% ============================================================================
% CONSTRAINT STORY: access_arbitrage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_access_arbitrage, []).

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
 *   constraint_id: access_arbitrage
 *   human_readable: Access Arbitrage: Reporters Pay in Framing for Institutional Access
 *   domain: political_economy/media_institutions
 *
 * SUMMARY:
 *   Access arbitrage describes the structural constraint that emerges from
 *   the asymmetry between journalists' need for institutional access and
 *   institutions' control over that access flow. Reporters at major news
 *   organizations require regular access to elite institutions (White House,
 *   Federal Reserve, State Department, Pentagon) to gather timely information
 *   and maintain journalistic credibility. These institutions, in turn, use
 *   access control as a tool to shape narrative framing. The constraint
 *   exhibits a clear extraction mechanism: institutions implicitly condition
 *   access on favorable or at least non-damaging coverage. Journalists
 *   internalize framing expectations (rarely violated), news organizations
 *   develop institutional dependencies (risk career advancement of
 *   journalists who disrupt relationships), and the collective result is
 *   narrative conformity that serves institutional interests. The constraint
 *   generates theatrical compliance: the daily White House briefing persists
 *   as ritual despite declining information value; background briefings shape
 *   story framing before publication; press credential revocation is
 *   threatened but rarely executed (maintaining the performance of openness).
 *   Over the past 30 years, the constraint has intensified as institutional
 *   messaging discipline has professionalized and as alternative information
 *   sources (digital, independent, leaked) have emerged without access
 *   requirements. The access-based sourcing model is increasingly revealed as
 *   contingent rather than necessary — yet the constraint persists through
 *   institutional inertia and competitive pressure among news organizations.
 *
 * KEY AGENTS:
 *   - Beat Reporters: Primary victims (powerless/trapped) — require access for career advancement; cannot refuse framing constraints without career termination; bear full cost of narrative control
 *   - News Organizations (Individual): Mixed position (moderate/constrained) — benefit from institutional relationships (reliable sourcing) but extracted from through access denial threats and framing constraints; can abandon coverage but at competitive cost
 *   - Elite Institutions: Primary beneficiaries (institutional/arbitrage) — control access flow; achieve narrative control and agenda-setting; can arbitrage between competing news organizations; experience constraint as pure coordination
 *   - Institutional Media Cartel: Collective actor (organized/constrained) — collectively benefit from shared institutional access but individually extracted from by access controls; maintain constraint through competitive pressure to conform
 *   - Independent/Digital Media Coalition: Alternative pathway actor (organized/constrained) — building sunset alternative through direct sourcing (leaks, FOIA, whistleblowers, primary data); operates outside access constraint; suppression declining over interval
 *   - Journalism Profession: Institutional actor (institutional/arbitrage) — maintains access-based sourcing norms through professional credibility (piton): ritual persists from inertia despite declining functional output; theater increasing over interval
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(access_arbitrage, 0.52).
domain_priors:suppression_score(access_arbitrage, 0.65).
domain_priors:theater_ratio(access_arbitrage, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(access_arbitrage, extractiveness, 0.52).
narrative_ontology:constraint_metric(access_arbitrage, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(access_arbitrage, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(access_arbitrage, tangled_rope).
narrative_ontology:human_readable(access_arbitrage, "Access Arbitrage: Reporters Pay in Framing for Institutional Access").
narrative_ontology:topic_domain(access_arbitrage, "political_economy/media_institutions").

domain_priors:requires_active_enforcement(access_arbitrage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(access_arbitrage, elite_institutions).
narrative_ontology:constraint_beneficiary(access_arbitrage, institutional_administrators).
narrative_ontology:constraint_victim(access_arbitrage, field_editorial_independence).
narrative_ontology:constraint_victim(access_arbitrage, public_information_quality).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BEAT REPORTER (SNARE) — Individual journalists covering elite institutions (White House, Federal Reserve, State Department) have no exit from the access constraint. Career advancement requires access; access requires favorable framing of institutional leadership and policy. Cannot refuse without career termination. Full extraction: reporters bear the cost of constrained framing while institutions capture the benefit of narrative control.
constraint_indexing:constraint_classification(access_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: NEWS ORGANIZATION (TANGLED ROPE) — Derives coordination benefit from institutional relationships (reliable sourcing, attribution, background briefings) but also extracted from through access denial threats and framing constraints. Can abandon beat coverage but at cost of competitive disadvantage vs organizations with stronger institutional relationships. Active enforcement: institutions revoke press credentials, deny interviews, restrict access as punishment for unfavorable coverage. Mixed: coordination function (relationship as information channel) + asymmetric extraction (institutional control over access flow).
constraint_indexing:constraint_classification(access_arbitrage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ELITE INSTITUTION (ROPE) — Experiences the constraint as pure coordination: controls which media outlets get access, which reporters get briefings, which stories are shaped pre-publication through background briefings and messaging discipline. Benefit: narrative control and agenda-setting power. Can arbitrage between multiple news organizations' desire for access. No suppression cost — constraint is self-policing (reporters internalize framing expectations). Net beneficiary.
constraint_indexing:constraint_classification(access_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: JOURNALISM PROFESSION (PITON) — Professional norms of objectivity, access-based sourcing, and institutional credibility are increasingly theatrical as institutional messaging discipline has professionalized. The profession maintains the access ritual (daily briefings, background calls, credential-checking) despite declining functional verification. Alternative information sources (social media, whistleblower platforms, citizen journalism) bypass traditional access entirely. Theater_ratio high (0.58 → 0.68 over interval) because the ritual persists from institutional inertia even as its verification function degrades. Piton: degraded coordination mechanism maintained by institutional habit.
constraint_indexing:constraint_classification(access_arbitrage, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INDEPENDENT/DIGITAL MEDIA COALITION (SCAFFOLD) — Organized actors (Substack writers, independent newsletters, TikTok journalists, nonprofit news outlets) are building alternative verification pathways that bypass institutional access constraints entirely. They source directly from leaked documents, FOIA requests, primary data, and interview subjects outside official channels. These pathways have lower suppression and higher editorial independence. Theater lower because alternative sources test claims directly rather than through institutional attribution. This is a sunset pathway: as audience trust in institutional media declines, economic incentives shift toward independent sourcing. High suppression is tolerated only because the coalition sees it declining over the time horizon.
constraint_indexing:constraint_classification(access_arbitrage, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INSTITUTIONAL MEDIA CARTEL (TANGLED ROPE) — Large news organizations (NYT, WaPo, WSJ, Bloomberg) collectively benefit from institutional access coordination through the access constraint. But individually each is extracted from by the same institutions that depend on them. The cartel maintains the constraint through competitive pressure: if one organization abandons access-based sourcing norms, it gains short-term advantage but destabilizes the whole institutional relationship system. Active enforcement via exclusion and access denial. Cartel members experience both genuine coordination (shared institutional relationships, industry-standard sourcing norms) and extraction (framing constraints imposed by institutions).
constraint_indexing:constraint_classification(access_arbitrage, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some asymmetry between information-seekers and information-holders is inherent to the structure of institutions: those controlling resources have structural advantages in narrative. Access is a necessary mechanism for any journalism. The bottleneck appears as an immutable property of how institutions and media interact. However, this classification is a false summit: the structural data (suppression 0.65, theater 0.58, extractiveness 0.52) reveals that the specific contemporary arrangement of access-based sourcing norms is contingent and institutionally reinforced, not a law of nature. The mountain perspective naturalizes what is actually institutional inertia.
constraint_indexing:constraint_classification(access_arbitrage, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(access_arbitrage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(access_arbitrage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(access_arbitrage, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(access_arbitrage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(access_arbitrage, TR),
    TR >= 0.70.

:- end_tests(access_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The extraction mechanism is real and structural: institutions condition access on favorable framing; reporters internalize constraints; narrative conformity results. But extractiveness is not at snare levels (≥0.66) because: (1) institutions generally cannot afford to cut off major news organizations entirely (they also need media coverage), (2) journalists retain some editorial freedom (not every story is shaped), (3) alternative sourcing pathways are emerging. The value reflects a hybrid constraint with genuine coordination function (access as information channel) plus significant asymmetric extraction (framing control). Suppression (0.65): Moderate-high. Significant barriers to independent reporting outside institutional access include: tacit knowledge in institutional operations (accessible only through insider relationships), speed advantages of institutional briefing access (competitors move faster with briefed information), career risk of access denial (advancement depends on institutional relationships), publication bias toward attributed quotes from officials (journalism norms privilege official sourcing). But suppression is not total (≥0.70) because alternative sourcing mechanisms exist and are viable, and because institutional access denial is rarely enforced to extremes. Theater ratio (0.58): Moderate. The daily White House briefing persists despite low information value; background briefings shape framing pre-publication; credential-checking rituals maintain appearance of gatekeeping. But theater is not high (≥0.70) because some institutional access does yield genuine information advantage, and because alternative sourcing creates verification pressure. Theater is increasing over interval as institutional messaging discipline professionalized while institutional openness (actual information flow) stagnated.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a clear perspectival gap between extractors and targets. Beat reporters (powerless/trapped) experience pure extraction (Snare): they cannot refuse framing constraints without career termination. The institutions they cover (institutional/arbitrage) experience pure coordination (Rope): they control access, achieve narrative management, face no suppression cost. News organizations (moderate/constrained) experience mixed extraction-coordination (Tangled Rope): they benefit from institutional relationships but are also extracted from through access denial threats. The independent/digital media coalition (organized/constrained) experiences a sunset constraint (Scaffold): they are building alternative verification pathways with lower suppression and higher editorial independence. The journalism profession (institutional/arbitrage) experiences degraded coordination (Piton): the access-based sourcing model is increasingly theatrical, persisting from institutional inertia rather than functional necessity. The analytical observer (analytical/analytical) risks seeing this as an immutable natural law (Mountain) — 'institutions always control access' — but the structural data reveals this as contingent: alternative sourcing, digital platforms, and declining trust in institutional media are shifting the landscape.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) reflect each agent's structural position in the extraction flow. Beat reporters are trapped victims with no arbitrage options (d ≈ 0.95): they experience maximum extraction as they cannot exit without career cost. Elite institutions are arbitrage-enabled beneficiaries (d ≈ 0.05): they control access and experience constraint as coordination with negative extraction cost. News organizations are constrained agents with mixed positions (d ≈ 0.55-0.65): they can exit (abandon institutional coverage) but at significant competitive cost. The independent media coalition is organized with constrained options within the institutional system but can arbitrage outside it (d ≈ 0.40-0.50): they experience extraction pressure from institutions but have genuine exit pathways. The pipeline computes effective extraction (χ) by applying the sigmoid f(d) to each directionality value, then scaling by scope modifier σ(S). Beat reporters experience χ at maximum (d ≈ 0.95, f(d) ≈ 1.42) while institutions experience negative or near-zero χ (d ≈ 0.05, f(d) ≈ -0.12). This perspectival gap in experienced extractiveness is central to why the constraint exhibits all six types from different perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating how access arbitrage is genuinely a tangled rope — it contains both real coordination function (institutions and journalists both benefit from reliable access relationships) AND real asymmetric extraction (institutions use access control to shape narrative). The coordination function is not epiphenomenal: institutional background briefings do provide genuine information advantage to journalists; institutional relationships are real channels for sourcing. But the coordination function is asymmetric and extractive: institutions capture disproportionate value (narrative control) while journalists bear costs (framing constraints, career risk of access denial). The constraint is not purely extractive (snare) because the coordination benefit is real, not theatrical. The constraint is not purely coordination (rope) because the asymmetric extraction is structural and enforced. The mandatrophy is resolved by recognizing that both elements are genuine: the constraint solves a real coordination problem (journalists need reliable sourcing; institutions need media coverage) while simultaneously using that coordination function as a mechanism for asymmetric extraction (institutions control narrative framing through access control). This is the canonical structure of tangled rope: coordination function + asymmetric extraction + active enforcement (access denial threats). The rising theater_ratio (0.42 → 0.68 over interval) indicates that the coordination function's relative importance is declining while the performative/extractive function is increasing — suggesting possible lifecycle drift toward snare, but the constraint remains tangled rope at the current measurement point.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    framing_threshold,
    'What degree of editorial independence from institutional messaging constitutes breaking the access constraint vs operating within acceptable journalistic norms?',
    'Content analysis: comparison of framing patterns when journalists have vs lose institutional access; correlation between access level and critical coverage; longitudinal tracking of institutional responses to critical reporting',
    'If threshold is high (organizations must publish institutional messaging verbatim): constraint is snare for all actors. If threshold is low (minor deviations from preferred framing cause access loss): constraint tightens from tangled_rope toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_threshold, conceptual, 'Threshold for acceptable editorial independence under access constraint').

omega_variable(
    alternative_sourcing_viability,
    'Can non-institutional journalists produce equivalent informational quality about elite institutions without direct access through alternative sources (leaks, FOIA, whistleblowers, primary data)?',
    'Comparative analysis of investigative coverage: institutional vs independent outlets on same stories; tracking of follow-up, correction rates, and institutional response to independent coverage; audience epistemic trust metrics',
    'If viable: scaffold perspective confirmed — alternative pathways are real and suppression will decline. If not viable: scaffold is aspirational; institutional access remains necessary structural bottleneck; constraint solidifies as snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_sourcing_viability, empirical, 'Whether alternative sourcing can replace institutional access').

omega_variable(
    institutional_messaging_professionalization,
    'To what degree has institutional messaging discipline (spin, framing discipline, access control) become a formal budgeted function vs informal peer pressure?',
    'Structural analysis of institutional communications budgets, press office staffing, messaging discipline procedures; comparison across 30-year interval of messaging sophistication and consistency',
    'If high professionalization: constraint is enforced, tangled_rope classification confirmed. If low: enforcement is informal and weaker; constraint might degrade toward rope. If professionalization is increasing over interval: theater_ratio should increase (rising performative burden with declining functional output).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_messaging_professionalization, empirical, 'Level of formalization of institutional message discipline').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(access_arbitrage, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(access_tr_t0, access_arbitrage, theater_ratio, 0, 0.42).
narrative_ontology:measurement(access_tr_t10, access_arbitrage, theater_ratio, 10, 0.52).
narrative_ontology:measurement(access_tr_t20, access_arbitrage, theater_ratio, 20, 0.62).
narrative_ontology:measurement(access_tr_t30, access_arbitrage, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(access_be_t0, access_arbitrage, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(access_be_t10, access_arbitrage, base_extractiveness, 10, 0.44).
narrative_ontology:measurement(access_be_t20, access_arbitrage, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(access_be_t30, access_arbitrage, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(access_arbitrage, information_standard).
narrative_ontology:affects_constraint(access_arbitrage, institutional_messaging_discipline).
narrative_ontology:affects_constraint(access_arbitrage, journalistic_sourcing_norms).
narrative_ontology:affects_constraint(access_arbitrage, media_ownership_concentration).

% DUAL FORMULATION NOTE:
% Access arbitrage is a constraint family spanning three related structural mechanisms: (1) individual reporter-institution access asymmetry (this story, ε=0.52), (2) institutional messaging discipline as the enforcement mechanism (upstream, ε=0.45, Tangled Rope), (3) media ownership concentration enabling cartel enforcement (upstream, ε=0.58, Snare). Each has distinct empirical status and failure modes. This story focuses on the reporter-level experience of access-based sourcing; the network links to upstream institutional and ownership-level constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(access_arbitrage, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
