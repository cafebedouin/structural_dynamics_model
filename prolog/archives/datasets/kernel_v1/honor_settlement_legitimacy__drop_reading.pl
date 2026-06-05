% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__drop_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__drop_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: honor_settlement_legitimacy__drop_reading
 *   human_readable: Honor Settlement Legitimacy: Drop Reading (Dueling as Fringe Persistence)
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   The DROP reading of the honor settlement legitimacy kernel posits that
 *   dueling persisted as a fringe practice among residual honor-culture
 *   adherents even as dominant legal frameworks criminalized it and
 *   mainstream society abandoned the honor code. This reading emphasizes
 *   geographic and social niches where honor culture remained locally
 *   legitimate: frontier regions with weak state presence, local lineage
 *   networks that preserved honor norms across generations, immigrant
 *   communities maintaining honor codes from source cultures, and rural areas
 *   where state enforcement capacity was minimal. The constraint in this
 *   reading is not that dueling was eliminated from the normative repertoire
 *   but that it was geographically and socially partitioned — suppressed in
 *   state-integrated urban centers, but maintained as a live option in
 *   peripheral communities. The persistence reflects identity-lock
 *   (individuals whose self-concept was fused with honor codes could not exit
 *   without existential cost) and geographic/institutional isolation that
 *   preserved older norms. This reading contrasts with the CONTRACTION
 *   reading (honor code became cognitively unthinkable across the entire
 *   social fabric) and the COMPOSITE reading (dueling's decline was
 *   overdetermined by multiple reinforcing mechanisms). The DROP reading
 *   emphasizes that honor culture did not disappear but became residual and
 *   fringe.
 *
 * KEY AGENTS:
 *   - Honor-culture carriers (powerless/identity_locked): Individuals whose identity is constituted through honor codes; face identity dissolution if they exit
 *   - Local lineage networks (organized/constrained): Family-based enforcement of honor norms; coordinate legitimacy claims and dispute resolution; face state suppression
 *   - Informal magistracy (institutional/arbitrage): Local elders and authorities who adjudicate honor disputes; arbitrage between honor code and state law based on context
 *   - State legal apparatus (institutional/arbitrage): Formal prohibition of dueling; actual enforcement is selective and performative, creating theater
 *   - Nationalist reform coalition (powerful/mobile): State builders and legal reformers; see dueling as incompatible with rational-legal authority; structure generational sunset
 *   - Analytical observer (analytical/analytical): Long-duration view; sees dueling persistence as institutional inertia rather than functional necessity or natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__drop_reading, 0.38).
domain_priors:suppression_score(honor_settlement_legitimacy__drop_reading, 0.52).
domain_priors:theater_ratio(honor_settlement_legitimacy__drop_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__drop_reading, tangled_rope).
narrative_ontology:human_readable(honor_settlement_legitimacy__drop_reading, "Honor Settlement Legitimacy: Drop Reading (Dueling as Fringe Persistence)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__drop_reading, "historical_sociology/legal_history/cultural_anthropology").

domain_priors:requires_active_enforcement(honor_settlement_legitimacy__drop_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__drop_reading, '365df175-0053-46e7-b146-c90b9f043714').
narrative_ontology:cs_kernel_codification('365df175-0053-46e7-b146-c90b9f043714', distributed).
narrative_ontology:cs_authority_grounding('365df175-0053-46e7-b146-c90b9f043714', practice).
narrative_ontology:cs_interpretation_layer_present('365df175-0053-46e7-b146-c90b9f043714').
narrative_ontology:cs_reading_relation('365df175-0053-46e7-b146-c90b9f043714', honor_settlement_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('365df175-0053-46e7-b146-c90b9f043714', honor_settlement_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('365df175-0053-46e7-b146-c90b9f043714', foundational, honor_culture_legitimacy_locally_holdable).
narrative_ontology:cs_axiom_status(honor_culture_legitimacy_locally_holdable, holdable).
narrative_ontology:cs_axiom_grounding('365df175-0053-46e7-b146-c90b9f043714', honor_culture_legitimacy_locally_holdable, conventional).
narrative_ontology:cs_axiom('365df175-0053-46e7-b146-c90b9f043714', foundational, geographic_variation_structural_not_temporal).
narrative_ontology:cs_axiom_status(geographic_variation_structural_not_temporal, holdable).
narrative_ontology:cs_axiom_grounding('365df175-0053-46e7-b146-c90b9f043714', geographic_variation_structural_not_temporal, empirically_contingent).
narrative_ontology:cs_reference_frame('365df175-0053-46e7-b146-c90b9f043714', honor_culture_as_live_normative_option).
narrative_ontology:cs_drift_state('365df175-0053-46e7-b146-c90b9f043714', contemporary_nation_state, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('365df175-0053-46e7-b146-c90b9f043714', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, honor_culture_carriers).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, local_enforcement_networks).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, state_monopoly_on_violence).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, individual_exit_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HONOR-BOUND INDIVIDUAL (SNARE) — Structurally mobile (could physically relocate, could legally refuse the duel) but identity-fused with honor code. Exit would require abandoning identity as a honorable person, a claim-holder in the community. The constraint extracts maximum cost because the target perceives no exit as compatible with remaining themselves. Dueling persists precisely because exit is unthinkable within the identity frame, not because exit is materially impossible.
constraint_indexing:constraint_classification(honor_settlement_legitimacy__drop_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: HONOR CULTURE NETWORK (TANGLED ROPE) — Organized carriers of honor norms (family lineages, local magistrates, informal enforcement groups) benefit from the legitimacy dueling provides for status claims and dispute resolution. They also coordinate genuine grievance-settlement functions (dueling as alternative to feuds, protection of reputation stakes). But they face state suppression and legal barriers that make enforcement costly. Hybrid: real coordination function (legitimacy, dispute resolution, status claims) + asymmetric extraction (state pressure, legal risk borne unevenly, younger members bear higher cost).
constraint_indexing:constraint_classification(honor_settlement_legitimacy__drop_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INFORMAL MAGISTRACY (ROPE) — Local authorities and community elders who adjudicate honor disputes experience dueling as coordination mechanism: it legitimizes their role as settlement authorities (even if they verbally condemn dueling) and provides alternative to state courts for dispute resolution. Low extraction because these actors can arbitrage between honor code and state law, choosing which framework suits their interests. They see dueling as a functional service to their community, not as a coercive mechanism.
constraint_indexing:constraint_classification(honor_settlement_legitimacy__drop_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: NATIONALIST REFORM COALITION (SCAFFOLD) — State builders, legal reformers, and nationalist intellectuals see dueling as a temporary coordination problem with a built-in sunset: honor culture is incompatible with rational-legal authority, and as literacy, centralized institutions, and commercial law expand, honor settlements will become unthinkable in the dominant framework. Low effective extraction because this perspective has agency (state power, institutional expansion) and sees an explicit exit path (cultural framework replacement via national education, law reform, institutional growth). The sunset is not formally codified but is structural (generational replacement of the honor frame).
constraint_indexing:constraint_classification(honor_settlement_legitimacy__drop_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE LEGAL APPARATUS (PITON) — States formally prohibit dueling through written law, yet the prohibition persists as ritual performance: magistrates announce the law against dueling while locally accepting honor settlements; courts prosecute notorious duelists (theater of enforcement) while ignoring small-scale affairs; newspapers condemn dueling in editorials while reporting duels with fascination. The legal apparatus sees its own enforcement as degraded and performative — prohibition persists through institutional inertia and symbolic authority, not through actual capacity to suppress honor culture at the local level. High theater ratio reflects that legal enforcement is mostly proclamation.
constraint_indexing:constraint_classification(honor_settlement_legitimacy__drop_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / INERTIAL VIEW (PITON) — From a long-duration view, dueling persists as institutional inertia: the constraint (honor settlement legitimacy) survives because established institutions carry it forward even as the framework that generated it (embedded honor economy) has atrophied. Dueling is maintained through performative enforcement and cultural transmission, not through active functional necessity. Piton classification reflects theater gate: the persistence is mostly institutional drag, not structural extraction or coordination. The constraint lingers because exit from the institution is diffuse and unmotivated, not because the constraint is functional or immutably natural.
constraint_indexing:constraint_classification(honor_settlement_legitimacy__drop_reading, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__drop_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(honor_settlement_legitimacy__drop_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(honor_settlement_legitimacy__drop_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(honor_settlement_legitimacy__drop_reading, TR),
    TR >= 0.70.

:- end_tests(honor_settlement_legitimacy__drop_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts through honor-culture enforcement: individuals must accept dueling invitations or face reputational destruction; refusing a duel brands one as cowardly or dishonorable, creating social death within the community. But the extraction is not maximal because the honor code also provides benefits — it legitimizes status claims, enables reputation protection, and structures dispute resolution in the absence of functional state courts. The constraint is tangled: it coordinates legitimacy claims while extracting from those who wish to exit the honor frame. Rising extractiveness over the interval (0.18→0.38) reflects increasing state suppression making honor culture more costly to maintain — as state law expands and enforcement intensifies, adhering to honor norms requires more active defense and risk-taking. Suppression (0.52): Moderate-high. State legal prohibition is real and escalates over the interval (0.35→0.52), but is selectively enforced: visible prosecutions of high-status duelists (theater) while low-level disputes are tolerated or ignored. Geographic variation: suppression is high in state-integrated urban centers, low in frontier and isolated regions. Theater ratio (0.58): Moderate-high, rising over interval (0.32→0.58). State enforcement is increasingly performative: the apparatus announces prohibition while locally tolerating informal settlements; prosecution of notorious duelists serves as theater while routine affairs proceed uninterrupted. The rise reflects growing gap between formal prohibition and actual tolerance in peripheral communities.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximum perspectival divergence between the honor-bound victim (snare), the honor-network organizer (tangled rope), the local magistrate (rope), the reform coalition (scaffold), the state apparatus (piton), and the analytical observer (piton). The same structural phenomenon — formal prohibition of dueling + informal persistence in fringe communities — appears as immutable extraction trap (snare), functional coordination hybrid (tangled rope), service mechanism (rope), temporary problem with built-in sunset (scaffold), degraded enforcement ritual (piton), or institutional inertia (piton). The perspectival gap is not resolved by showing one perspective is 'correct' — rather, the gap itself reveals the constraint's true structure: it coordinates honor claims for networks while extracting from individuals; it provides alternatives for magistrates while constraining reformers; it exists through state performance rather than actual capacity. The drop reading's distinctive gap is between the geographic niches where honor persists (fringe perspectives see rope/tangled rope) and the integrated state centers (state apparatus perspective sees successfully suppressed piton). The reading claims both perspectives are structurally accurate for their contexts — dueling is both suppressed AND persistent, depending on location.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural position within this specific constraint. Honor-culture carriers are victims (d ≈ 0.89): they are fully targeted by the extraction mechanism (honor demands they duel or face reputation loss), identity-locked so they cannot exercise exit options, and face escalating state suppression. Local enforcement networks are beneficiaries (d ≈ 0.25): they benefit from the legitimacy dueling provides and the coordination it enables; they have constrained but real exit options (can relocate or abandon honor enforcement). State apparatus is institutional beneficiary (d ≈ 0.10 via arbitrage): suppression enables state authority claims without requiring actual enforcement capacity. Reform coalition has low d via mobile/powerful exit: they see the constraint as temporary and benefit from its eventual dissolution. The perspectival gaps reflect these differential positions: victims see snare (maximum extraction, identity-locked), networks see tangled rope (mixed coordination and extraction, organized but constrained), beneficiaries see rope (coordination function, low extraction experienced). The constraint's effective extractiveness is scaled by each agent's exit modulation: identity_locked agents experience higher chi than mobile agents facing the same base extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The DROP reading resolves the mandatrophy by showing that honor settlement legitimacy contains both genuine coordination and genuine extraction, partitioned by geography and social structure. In honor-culture networks, dueling coordinates legitimacy claims and dispute resolution (rope function); in state-integrated society, dueling is extraction and theater (snare + piton). The constraint is tangled rope at the network level (real coordination + asymmetric extraction), snare at the individual level (for those identity-locked to honor codes), and piton at the state apparatus level (performative suppression). The drop reading's distinctive contribution is to deny that this partitioning is temporary (as the scaffold perspective claims) or cognitive-only (as contraction reading claims) — instead, dueling persists as structurally stable in niches where state power is weak and honor networks remain functional. The mandatrophy is resolved by showing that all six types are simultaneously true for different populations and geographic scales: the constraint is not one thing, it is a heterogeneous ensemble of types organized by geography and network structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    honor_cognitive_vs_structural_binding,
    'Is the persistence of honor settlement norms primarily a matter of cognitive/identity fusion (honor code internalized and constitutive of self) or structural/material constraint (geographic isolation, economic dependence on local networks, legal vulnerability)?',
    'Ethnographic/historical analysis of post-exit trajectories: Do individuals who leave honor-culture communities retain honor commitments? Do they re-adopt honor norms upon return? Do relocated individuals report identity dissolution or successful identity reformation? Longitudinal tracking of frame shifts across generations.',
    'If primarily cognitive: identity_locked exit classification is accurate; constraint persists through internalized framing even after material barriers drop. If primarily structural: constrained or trapped exit classification is more accurate; constraint dissolves when material barriers are removed. Classification sensitive to which mechanism dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_cognitive_vs_structural_binding, empirical, 'Whether honor settlement persistence is identity-based or structurally constrained').

omega_variable(
    state_suppression_mechanism_effectiveness,
    'Does state legal prohibition of dueling actually suppress dueling practice, or does prohibition merely displace dueling into informal/hidden contexts while producing theater of enforcement (visible prosecutions)?',
    'Comparative analysis of dueling frequency before/after legal prohibition in specific jurisdictions. Distinction between reported duels (prosecuted cases, visible theater) and actual duels (archival correspondence, family records, medical records). Assessment of prosecution selectivity: are prosecutions concentrated on high-status cases (theater) or representative of actual incidence?',
    'If prohibition effectively suppresses: state power is structural, suppression metric reflects actual enforcement capacity. If prohibition displaces into informality: theater_ratio is underestimated; state power is more symbolic than material. Affects classification of state perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_suppression_mechanism_effectiveness, empirical, 'Whether state prohibition actually suppresses dueling or produces theater').

omega_variable(
    alternative_dispute_resolution_sufficiency,
    'Did commercial law and state courts offer genuine functional alternatives to dueling for honor settlement and reputation protection, or were they structurally inadequate (slow, expensive, unable to address reputational harm)?',
    'Analysis of dispute resolution patterns: what proportion of honor disputes were addressable through state courts? Time-to-resolution comparison (court vs duel). Economic analysis of court access costs. Reputation protection analysis: could state court victory restore reputation as effectively as duel victory?',
    'If alternatives were adequate: dueling persistence reflects identity-lock and institutional inertia (piton), not functional necessity. If alternatives were inadequate: dueling persists because honor culture provided solutions state law could not (tangled_rope with real coordination function). Affects classification of whether constraint is functional or theatrical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_dispute_resolution_sufficiency, empirical, 'Whether state law alternatives adequately addressed honor settlement needs').

omega_variable(
    reading_contrastive_delta,
    'This DROP reading posits that dueling persisted as fringe practice in residual honor-culture niches. How structurally different is this from the CONTRACTION reading (dueling became cognitively unthinkable) and the COMPOSITE reading (overdetermined decline)?',
    'Comparative textual and institutional analysis: (1) Geographic/social mapping — where did dueling persist (drop reading)? Where did it disappear from cognitive repertoire (contraction reading)? (2) Temporal analysis — did contraction precede or follow suppression (composite)? (3) Axiom verification — does drop reading''s axiom (honor_culture_legitimacy_locally_holdable) distinguish it from contraction''s axiom (honor_code_replaced_by_rational_legal_frame)?',
    'If drop and contraction readings describe different populations and time periods: they coexist (both true for different groups/times) — reading_relation is coexists_with. If drop describes later survival in fringe groups after contraction in mainstream: reading_relation involves influences. If drop''s persistence requires contraction to be false: reading_relation is forecloses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contrastive_delta, conceptual, 'Structural differentiation between drop, contraction, and composite readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__drop_reading, 1750, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(honor_drop_tr_t0, honor_settlement_legitimacy__drop_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(honor_drop_tr_t30, honor_settlement_legitimacy__drop_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement(honor_drop_tr_t60, honor_settlement_legitimacy__drop_reading, theater_ratio, 60, 0.58).

% Extraction over time
narrative_ontology:measurement(honor_drop_be_t0, honor_settlement_legitimacy__drop_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(honor_drop_be_t30, honor_settlement_legitimacy__drop_reading, base_extractiveness, 30, 0.28).
narrative_ontology:measurement(honor_drop_be_t60, honor_settlement_legitimacy__drop_reading, base_extractiveness, 60, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(honor_drop_su_t0, honor_settlement_legitimacy__drop_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(honor_drop_su_t30, honor_settlement_legitimacy__drop_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement(honor_drop_su_t60, honor_settlement_legitimacy__drop_reading, suppression_requirement, 60, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__drop_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% The honor settlement legitimacy kernel has three constraint stories representing three competing readings: drop_reading (dueling persisted in fringe niches), contraction_reading (honor code became unthinkable across society), and composite_reading (decline was overdetermined by multiple mechanisms). Each reading has its own epsilon value and perspectival structure. The drop reading emphasizes geographic persistence and identity-lock; contraction emphasizes cognitive transformation; composite emphasizes multiple reinforcing mechanisms. All three readings are structurally linked through reading_relations (coexists_with or influences) because they address the same contested kernel from different interpretive positions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_settlement_legitimacy__drop_reading, powerless, 0.89).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
