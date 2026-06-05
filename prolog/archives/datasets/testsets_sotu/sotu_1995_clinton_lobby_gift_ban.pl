% ============================================================================
% CONSTRAINT STORY: sotu_1995_clinton_lobby_gift_ban
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1995_clinton_lobby_gift_ban, []).

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
 *   constraint_id: sotu_1995_clinton_lobby_gift_ban
 *   human_readable: Voluntary Cessation of Lobbyist Gifts to Congressional Members (SOTU 1995)
 *   domain: governance/political_economy
 *
 * SUMMARY:
 *   In the 1995 State of the Union address, President Clinton proposed that
 *   Congress unilaterally cease accepting gifts, trips, and perks from
 *   lobbyists without waiting for statutory legislation. The proposal
 *   positioned voluntary self-restraint as a signal of institutional reform,
 *   addressing public concern about corruption while avoiding the political
 *   cost of formal enforcement. This constraint exhibits classic tangled-rope
 *   structure: it coordinates Congressional behavior around a shared
 *   legitimacy signal while simultaneously extracting material and
 *   reputational costs from members and access leverage from lobbyists. The
 *   voluntary nature of the constraint is its defining feature — by
 *   substituting norm-based coordination for statutory enforcement, the
 *   proposal trades the clarity of legal prohibition for the flexibility of
 *   institutional self-governance. This choice reveals a fundamental
 *   structural tension in the constraint: voluntary norms are robust to
 *   organizational change but fragile under competitive pressure, while
 *   statutory constraints are cumbersome but stable.
 *
 * KEY AGENTS:
 *   - Congressional Members (Non-Reform Faction): Primary target (moderate/constrained) — bear loss of gift revenue and reputational risk of accepting visible gifts
 *   - Congressional Members (Reform Faction): Secondary beneficiary (organized/constrained) — gain reputational benefit from compliance but lose material perks
 *   - Constituent Public: Primary victim (powerless/trapped) — cannot verify compliance or exit political system; dependent on institutional honor
 *   - Lobbyist Interests: Secondary target (powerful/mobile) — lose gift-giving access lever but retain alternative mechanisms
 *   - Institutional Reform Movement: Primary beneficiary (institutional/arbitrage) — gains legitimacy from appearance of self-policing without statutory burden
 *   - Watchdog / Transparency Organizations: Organized actor (organized/constrained) — see norm as transitional mechanism toward statutory enforcement
 *   - Congressional Ethics Apparatus: Institutional actor (institutional/arbitrage) — maintains oversight theater without active enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1995_clinton_lobby_gift_ban, 0.52).
domain_priors:suppression_score(sotu_1995_clinton_lobby_gift_ban, 0.48).
domain_priors:theater_ratio(sotu_1995_clinton_lobby_gift_ban, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1995_clinton_lobby_gift_ban, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1995_clinton_lobby_gift_ban, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sotu_1995_clinton_lobby_gift_ban, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1995_clinton_lobby_gift_ban, tangled_rope).
narrative_ontology:human_readable(sotu_1995_clinton_lobby_gift_ban, "Voluntary Cessation of Lobbyist Gifts to Congressional Members (SOTU 1995)").
narrative_ontology:topic_domain(sotu_1995_clinton_lobby_gift_ban, "governance/political_economy").

domain_priors:requires_active_enforcement(sotu_1995_clinton_lobby_gift_ban).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1995_clinton_lobby_gift_ban, public_trust_institution).
narrative_ontology:constraint_beneficiary(sotu_1995_clinton_lobby_gift_ban, congressional_reformers).
narrative_ontology:constraint_victim(sotu_1995_clinton_lobby_gift_ban, congressional_members_revenue_flow).
narrative_ontology:constraint_victim(sotu_1995_clinton_lobby_gift_ban, lobbyist_access_leverage).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTITUENT PUBLIC (SNARE) — Cannot exit the political system or verify compliance. Trapped in a system where voluntary norms replace legal enforcement. Maximum experienced extraction — constituents have no mechanism to enforce the norm, no visibility into compliance, and no exit. The norm appears as systemic improvement but operates as extraction with performative cover.
constraint_indexing:constraint_classification(sotu_1995_clinton_lobby_gift_ban, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REFORM-ALIGNED CONGRESSIONAL BLOC (TANGLED ROPE) — Organized members (those publicly committed to reform) are constrained by competitive disadvantage if other members continue accepting gifts. They experience both coordination benefit (legitimate legislative collaboration without gift obligations) and asymmetric extraction (loss of material perks, career signaling cost of appearing to reject lobbying money). Active enforcement mechanism: public comparison of gift acceptance rates creates reputational pressure.
constraint_indexing:constraint_classification(sotu_1995_clinton_lobby_gift_ban, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL REFORM MOVEMENT (ROPE) — Benefits from the norm as a coordination mechanism for institutional legitimacy. Can exit by framing voluntary reform as evidence of self-policing (arbitrage into renewed public trust). Net beneficiary — the constraint enables the institution to present as self-correcting without statutory enforcement. Low theater from this perspective.
constraint_indexing:constraint_classification(sotu_1995_clinton_lobby_gift_ban, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LOBBYIST INTERESTS / ACCESS LEVERAGE MECHANISM (SNARE) — Faces extraction: the norm removes a primary access mechanism (gift-giving as relationship-building). However, lobbyist power is mobile (can redirect to campaign contributions, direct lobbying, revolving-door employment). The constraint targets gift-giving specifically but does not address substitute mechanisms. Experienced as snare because the norm removes one extraction lever while others remain open — net extraction is lower but not zero.
constraint_indexing:constraint_classification(sotu_1995_clinton_lobby_gift_ban, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: NON-REFORM CONGRESSIONAL MEMBERS (TANGLED ROPE) — Face mixed pressures: continued gift-acceptance is now publicly visible reputational cost (extraction), but abstaining means loss of relationship-building mechanism and material benefit (also extraction). Constrained by public scrutiny and peer comparison. The norm operates as hybrid coordination (members coordinate on visibility standards) and extraction (reputational penalty for accepting gifts). Enforcement is social/reputational rather than statutory.
constraint_indexing:constraint_classification(sotu_1995_clinton_lobby_gift_ban, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CONGRESSIONAL ETHICS COMMITTEE (PITON) — Voluntary norm eliminates need for active enforcement infrastructure. The ethics apparatus becomes theatrical — it maintains the appearance of oversight without needing to prosecute violations (since the norm is voluntary, not statutory). Theater persists through institutional inertia even as the actual enforcement function degrades. The apparatus sees itself as maintaining standards while standards enforcement mechanisms remain dormant.
constraint_indexing:constraint_classification(sotu_1995_clinton_lobby_gift_ban, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: WATCHDOG / TRANSPARENCY MOVEMENT (SCAFFOLD) — Sees voluntary norm as a transition mechanism toward statutory enforcement. Organized actors (good-government groups, transparency advocates) experience the norm as temporary coordination with sunset logic: if voluntary gift-ban holds, it creates political momentum for statutory legislation. If voluntary ban fails, it demonstrates the need for enforcement. Either outcome builds case for formal regulation. Constrained by dependence on Congressional adoption but have clear exit narrative.
constraint_indexing:constraint_classification(sotu_1995_clinton_lobby_gift_ban, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the norm appears as an immutable feature of human political economy: access mechanisms inevitably substitute for one another (gifts → campaign contributions → revolving door → media access). Attempting voluntary restraint on one mechanism is futile because underlying incentive structure remains unchanged. The constraint appears as fighting gravity — a natural law that extraction mechanisms proliferate when one is blocked. However, the structural data reveals this as a false summit: the substitution pattern is not inevitable but a contingent outcome of how incentives are framed and measured.
constraint_indexing:constraint_classification(sotu_1995_clinton_lobby_gift_ban, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1995_clinton_lobby_gift_ban_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1995_clinton_lobby_gift_ban, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1995_clinton_lobby_gift_ban, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1995_clinton_lobby_gift_ban, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1995_clinton_lobby_gift_ban, TR),
    TR >= 0.70.

:- end_tests(sotu_1995_clinton_lobby_gift_ban_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint removes one significant access mechanism (gift-giving as relationship-building) but does not address the underlying incentive structure that drives lobbying access. Members experience extraction through loss of material benefit and reputational cost of any gift-taking. Lobbyists experience extraction through loss of access leverage, but this is constrained by availability of substitute mechanisms. The extractiveness value reflects the fact that the constraint genuinely reduces one pathway but does not eliminate the underlying access economy. The trajectory from 0.38 to 0.52 reflects increasing theater and substitution dynamics: as members and lobbyists adapt to the norm, the theater ratio (appearance of compliance) rises while the actual constraint on access leverage remains limited. Suppression (0.48): Moderate. Suppression mechanisms include reputational pressure (observable gift-taking becomes costly), peer comparison (members track each other's compliance), and media scrutiny (watchdog groups monitor disclosure). However, suppression is incomplete: members in safe electoral positions face lower reputational pressure, and substitution mechanisms (campaign contributions, revolving-door employment) remain available. The constraint does not eliminate the exit options — it makes one option more costly while leaving others open. Theater ratio (0.65): Moderate-high. The norm operates partly as genuine coordination (members legitimately benefit from reduced gift-obligation dynamics) and partly as performance (public disclosure of compliance as evidence of reform without addressing underlying access structures). The theater rises over time as members learn to manage the appearance of compliance — publicly rejecting gifts while privately cultivating access through campaign contributions and post-Congressional employment arrangements.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between the reform movement and the constituent public is maximal. The reform movement sees a coordination solution: the norm enables Congress to present as self-policing, which increases institutional legitimacy at minimal cost (no need for formal enforcement apparatus). The constituent public sees pure extraction with performative cover: they bear the cost of a system that depends on voluntary compliance from actors with material incentives to violate the norm, yet the public has no mechanism to verify or enforce compliance. The ethics apparatus sees degraded function: the voluntary norm eliminates the need for active enforcement, so the ethics infrastructure becomes theatrical. Non-reform members see asymmetric cost: reform members gain reputational benefit while they lose material benefit if they accept the norm's social pressure. Lobbyists see constrained access but not eliminated access: the gift-giving channel closes but alternative channels (campaign contributions, direct lobbying, employment offers) remain open. The watchdog movement sees a transitional structure: the voluntary norm either succeeds (building momentum for statutory enforcement) or fails (demonstrating the need for statutory enforcement). The analytical observer risks seeing an immutable natural law: access mechanisms inevitably substitute for one another, making voluntary restraint futile. The structural data reveals this as a false summit: substitution is not inevitable but a contingent outcome of how incentives are framed and how transparency mechanisms operate.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness is determined by their structural position relative to the gift-ban norm. Congressional members who oppose the norm (non-reform faction) experience high extraction: they lose material benefits (gifts, trips) and face reputational cost if they continue accepting. Their d-value is high (full target). Congressional members who support the norm (reform faction) experience lower extraction: they gain reputational benefit (appearing reform-minded) that offsets the loss of material perks. Their d-value is moderate. Lobbyists experience extraction through loss of access leverage, but their d-value is constrained by the availability of substitute mechanisms — they can redirect access efforts to campaign contributions and direct lobbying without a corresponding loss of total influence. The constituent public appears as a powerless agent (trapped, unable to exit) whose extraction is maximized by the voluntary nature of the constraint: they have no mechanism to verify compliance or enforce the norm. The reform movement benefits from the norm as a coordination mechanism for institutional legitimacy (low d, beneficiary status). The ethics apparatus maintains enforcement theater while actual enforcement is minimized (low d, beneficiary status through reduced burden). Directionality derives from beneficiary/victim declarations plus exit options: members face medium exit cost (reputational but not legal), lobbyists face low exit cost (can substitute), public faces maximal exit cost (trapped in system).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by clarifying the structural difference between voluntary coordination and coercive constraint. If the norm were purely voluntary (members and lobbyists adopt it out of shared belief in anti-corruption principle), the constraint would classify as Rope — genuine coordination with minimal coercion. But the structural data shows active enforcement through reputational pressure and peer comparison, revealing the constraint as Tangled Rope: genuine coordination function (reducing gift-obligation dynamics in legislative relationships) combined with asymmetric extraction (reputational cost imposed on non-compliant members, access leverage removed from lobbyists). The mandatrophy is resolved by noting that the voluntary label obscures the actual enforcement mechanism: social pressure and competitive disadvantage operate as effectively as statutory enforcement in policing the norm. The constraint is hybrid coordination (legitimate legislative problem it solves) and extraction (penalty for non-compliance) precisely because the enforcement is social rather than statutory. The volunteer nature of the label does not make it less coercive — it makes the coercion less visible, which is itself part of the extractive mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_norm_enforcement_mechanism,
    'What mechanism enforces voluntary compliance once the norm is announced? Is it reputational cost, internal honor, or external surveillance?',
    'Empirical tracking of gift acceptance patterns post-announcement; correlation between public visibility of gift-taking and adoption rate; measurement of compliance across members with different career stages and electoral security',
    'If enforcement is primarily external (reputation): the constraint is snare-like for members with low visibility (rural districts, safe seats). If enforcement is primarily internal (honor code): compliance depends on member internalization of norm value — identity_locked exit dynamics become central. If enforcement is surveillance: the apparatus is not voluntary but coercive with voluntary labeling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voluntary_norm_enforcement_mechanism, empirical, 'Mechanism that enforces voluntary compliance to the gift-ban norm').

omega_variable(
    substitution_mechanism_closure,
    'Do lobbyists substitute gift-giving with alternative access mechanisms (campaign contributions, revolving-door employment, direct paid consulting), or does the norm create genuine reduction in access leverage?',
    'Longitudinal analysis of lobbying expenditure patterns pre/post-announcement; tracking of revolving-door flows; correlation between members accepting gifts and members receiving campaign contributions from same donor pools',
    'If substitution is complete: the constraint is purely theatrical (Piton). Extraction mechanisms shift but total access leverage remains unchanged. If substitution is partial: the constraint genuinely reduces one access pathway (Tangled Rope confirmed). If substitution fails: lobbyist leverage is materially reduced (Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_mechanism_closure, empirical, 'Whether lobbyists substitute gift-giving with alternative access mechanisms').

omega_variable(
    identity_lock_vs_coercion,
    'Do members who comply with the gift-ban do so because they have internalized the norm (identity_locked) or because of external reputational pressure (constrained)?',
    'Qualitative analysis of member statements; correlation between compliance and electoral vulnerability; tracking of members who comply when invisible (no local media coverage) vs. those who comply when visible',
    'If identity_locked: compliance is stable but depends on maintaining member''s reformer identity. If constrained: compliance erodes when reputational pressure decreases. Matters for long-term sustainability of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_coercion, empirical, 'Whether compliance is identity-internalized or externally coerced').

omega_variable(
    false_summit_natural_law,
    'Is the substitution of access mechanisms (gift → campaign contribution) an immutable natural law of political economy, or a contingent institutional outcome dependent on how incentives are structured?',
    'Comparative analysis across political systems with different gift-ban enforcement mechanisms; testing of whether transparency (making all access mechanisms visible) reduces total access leverage even when mechanisms substitute',
    'If law-like: voluntary bans are futile (mountain perspective confirmed; constraint is unresolvable). If contingent: the natural-law framing is a false summit that naturalizes avoidable institutional choices. The constraint becomes tractable through different institutional designs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_summit_natural_law, conceptual, 'Whether access mechanism substitution is an immutable law or contingent institutional outcome').

omega_variable(
    public_trust_measurement_validity,
    'Does the voluntary gift-ban actually increase public trust in Congress, or does it increase public perception of compliance while real corruption mechanics continue unchanged?',
    'Public opinion tracking pre/post-announcement; analysis of whether trust increase correlates with actual policy changes favoring public interest vs. continuation of extraction-aligned policy outcomes',
    'If real trust increase correlates with policy change: the constraint has genuine coordination and trust-building function (Rope from public perspective). If trust increase is disconnected from policy: the constraint is pure theater (Piton), and the norm serves to create appearance of reform while extraction continues.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_trust_measurement_validity, empirical, 'Whether voluntary gift-ban increases genuine public trust or merely appearance of reform').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1995_clinton_lobby_gift_ban, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotugift_tr_t0, sotu_1995_clinton_lobby_gift_ban, theater_ratio, 0, 0.48).
narrative_ontology:measurement(sotugift_tr_t2, sotu_1995_clinton_lobby_gift_ban, theater_ratio, 2, 0.58).
narrative_ontology:measurement(sotugift_tr_t4, sotu_1995_clinton_lobby_gift_ban, theater_ratio, 4, 0.65).

% Extraction over time
narrative_ontology:measurement(sotugift_be_t0, sotu_1995_clinton_lobby_gift_ban, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(sotugift_be_t2, sotu_1995_clinton_lobby_gift_ban, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(sotugift_be_t4, sotu_1995_clinton_lobby_gift_ban, base_extractiveness, 4, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1995_clinton_lobby_gift_ban, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1995_clinton_lobby_gift_ban, campaign_finance_contribution_cap).
narrative_ontology:affects_constraint(sotu_1995_clinton_lobby_gift_ban, revolving_door_employment_restriction).
narrative_ontology:affects_constraint(sotu_1995_clinton_lobby_gift_ban, congressional_disclosure_requirements).

% DUAL FORMULATION NOTE:
% The voluntary gift-ban is structurally distinct from statutory campaign finance limits and revolving-door restrictions. Each mechanism addresses a different channel through which lobbyists access members. The gift-ban targets personal relationship-building; campaign finance mechanisms target financial leverage; revolving-door restrictions target employment incentives. These are separate constraints with different ε values, linked through network effects: if the gift-ban succeeds, lobbyists substitute to campaign contributions and post-Congressional employment, making campaign finance and revolving-door constraints more critical.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1995_clinton_lobby_gift_ban, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
