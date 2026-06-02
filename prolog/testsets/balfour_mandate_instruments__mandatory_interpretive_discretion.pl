% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__mandatory_interpretive_discretion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__mandatory_interpretive_discretion, []).

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
 *   constraint_id: balfour_mandate_instruments__mandatory_interpretive_discretion
 *   human_readable: British Mandatory Interpretive Discretion Under the Balfour Declaration and League of Nations Mandate
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   The British Mandate for Palestine (1920-1948) under the League of Nations
 *   Covenant embedded a structural contradiction: the Balfour Declaration
 *   committed Britain to 'facilitate the establishment in Palestine of a
 *   national home for the Jewish people' while the Mandate document required
 *   Britain to protect the 'rights and position' of existing non-Jewish
 *   communities and to hold the territory in trust for its inhabitants. These
 *   two commitments were textually ambiguous about their hierarchy and scope.
 *   This constraint story models one specific reading of the contested
 *   kernel: that the Mandate system itself constitutes a constraint mechanism
 *   where British interpretive discretion over the conflicting obligations
 *   becomes the operational reality, enabling the mandatory power to
 *   adjudicate between competing claims without external review or fixed
 *   textual meaning. The discretion itself — the authority to reinterpret
 *   what the obligations require — becomes the operative constraint that
 *   locks both Arab and Jewish communities into dependency and strategic
 *   uncertainty. Between 1920 and 1948, British policy oscillated
 *   significantly: the 1922 White Paper limited the Jewish national home to
 *   Israel proper (excluding Transjordan); the 1930 Passfield White Paper
 *   suggested limiting Jewish land purchase; the 1939 White Paper capped
 *   Jewish immigration and restricted land transactions. Each reversal
 *   (sometimes driven by community pressure, sometimes by internal British
 *   reassessment) reframed what the Mandate 'required,' but neither Arab nor
 *   Jewish communities could appeal beyond British interpretation. This
 *   constraint is distinguished from the competing readings
 *   (jewish_national_home_primacy, which privileges one textual
 *   interpretation; dual_obligation_indigenous_rights, which privileges the
 *   other) by its focus on the interpretive authority structure itself rather
 *   than on which interpretation should prevail. The extraction mechanism is
 *   the strategic uncertainty created by discretion: both communities must
 *   constantly anticipate policy reversals and adjust negotiating positions
 *   accordingly, giving the mandatory power asymmetric advantage in
 *   divide-and-rule administration.
 *
 * KEY AGENTS:
 *   - British Colonial Administration: Primary beneficiary (institutional/arbitrage) — captures policy flexibility to manage competing constituencies, divides communities through oscillation, avoids locking into either commitment
 *   - Arab Communities: Primary victim (powerless/trapped) — cannot exit territorial governance regime; face policy reversals without appeal mechanism; bear suppression costs of reinterpretation
 *   - Jewish Communities: Primary victim (powerless/trapped) — cannot exit Mandate system; face strategic uncertainty from policy oscillation; bear suppression costs of reinterpretation
 *   - League of Nations: Secondary institutional actor (organized/constrained) — benefits from mandate legitimacy; constrained by inability to enforce interpretation; reports function is performative
 *   - Mandate Text Itself: Epistemic Commons (powerless/trapped) — the Balfour/Covenant language is the victim; interpretive authority vests entirely in mandatory power; no neutral adjudication
 *   - Analytical Observer: Civilizational view (analytical/analytical) — at risk of naturalizing contingent institutional design as inherent feature of sovereignty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.58).
domain_priors:suppression_score(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.72).
domain_priors:theater_ratio(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, extractiveness, 0.58).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__mandatory_interpretive_discretion, snare).
narrative_ontology:human_readable(balfour_mandate_instruments__mandatory_interpretive_discretion, "British Mandatory Interpretive Discretion Under the Balfour Declaration and League of Nations Mandate").
narrative_ontology:topic_domain(balfour_mandate_instruments__mandatory_interpretive_discretion, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__mandatory_interpretive_discretion).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__mandatory_interpretive_discretion, '2840ad2d-50f8-4cbf-aed8-4828cf3961ed').
narrative_ontology:cs_kernel_codification('2840ad2d-50f8-4cbf-aed8-4828cf3961ed', formalized).
narrative_ontology:cs_authority_grounding('2840ad2d-50f8-4cbf-aed8-4828cf3961ed', extraction).
narrative_ontology:cs_interpretation_layer_present('2840ad2d-50f8-4cbf-aed8-4828cf3961ed').
narrative_ontology:cs_reading_relation('2840ad2d-50f8-4cbf-aed8-4828cf3961ed', balfour_mandate_instruments__jewish_national_home_primacy, coexists_with).
narrative_ontology:cs_reading_relation('2840ad2d-50f8-4cbf-aed8-4828cf3961ed', balfour_mandate_instruments__dual_obligation_indigenous_rights, coexists_with).
narrative_ontology:cs_axiom('2840ad2d-50f8-4cbf-aed8-4828cf3961ed', foundational, interpretive_authority_unconstrained_by_appeal).
narrative_ontology:cs_axiom_status(interpretive_authority_unconstrained_by_appeal, holdable).
narrative_ontology:cs_axiom_grounding('2840ad2d-50f8-4cbf-aed8-4828cf3961ed', interpretive_authority_unconstrained_by_appeal, empirically_contingent).
narrative_ontology:cs_axiom('2840ad2d-50f8-4cbf-aed8-4828cf3961ed', foundational, discretion_creates_strategic_uncertainty_lock_in).
narrative_ontology:cs_axiom_status(discretion_creates_strategic_uncertainty_lock_in, holdable).
narrative_ontology:cs_axiom_grounding('2840ad2d-50f8-4cbf-aed8-4828cf3961ed', discretion_creates_strategic_uncertainty_lock_in, empirically_contingent).
narrative_ontology:cs_reference_frame('2840ad2d-50f8-4cbf-aed8-4828cf3961ed', mandatory_power_unilateral_interpretive_authority).
narrative_ontology:cs_drift_state('2840ad2d-50f8-4cbf-aed8-4828cf3961ed', contemporary_post_mandate, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2840ad2d-50f8-4cbf-aed8-4828cf3961ed', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administrators).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, arab_communities).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, jewish_communities).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, mandate_epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ARAB COMMUNITIES (SNARE) — Trapped by policy reversals (1920s settlement restrictions vs 1940s restrictions on Jewish immigration; the 1939 White Paper reversal vs previous ambiguity). No appeal mechanism to fixed textual meaning of the Mandate; cannot exit the territorial governance regime. Suppression operates through legal reinterpretation: what was permitted one decade becomes prohibited the next, with no stable juridical ground. Maximum extraction — communities bear costs of strategic uncertainty and path-dependent lock-in.
constraint_indexing:constraint_classification(balfour_mandate_instruments__mandatory_interpretive_discretion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: JEWISH COMMUNITIES (SNARE) — Trapped by discretionary policy shifts (1922 White Paper limiting scope of Jewish national home to Transjordan; 1939 White Paper limiting immigration and land purchase). Cannot appeal beyond British interpretive authority; exit from the mandate system is not available. Suppression operates through reinterpretation of the Balfour commitment itself: the mandatory power can redefine what 'Jewish national home' means without external review. Strategic uncertainty locks both communities into dependency on British administrative discretion.
constraint_indexing:constraint_classification(balfour_mandate_instruments__mandatory_interpretive_discretion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: BRITISH COLONIAL ADMINISTRATION (ROPE) — Experiences the constraint as coordination: interpretive discretion enables managing two competing constituencies through policy oscillation. The mandatory power solves the coordination problem (how to satisfy conflicting obligations) through divide-and-rule — each community interprets silence as future possibility. Net beneficiary of the discretionary regime; can arbitrage between competing claims without locking into either. Theater is moderate — the legal cover (League mandate, Balfour legitimacy) provides institutional legitimacy for what is functionally administrative discretion.
constraint_indexing:constraint_classification(balfour_mandate_instruments__mandatory_interpretive_discretion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LEAGUE OF NATIONS AS SUPERVISORY AUTHORITY (TANGLED ROPE) — Constrained by structural limits on mandate oversight; cannot compel British reinterpretation without withdrawing the mandate entirely. Experiences both coordination function (legitimizing the mandate system) and asymmetric extraction (the mandatory power retains unilateral interpretive authority). The League benefits from the mandate structure (demonstrates collective governance) while bearing the cost of powerlessness over interpretation. Scope limited to formal written appeals and annual reports; no real-time intervention capacity.
constraint_indexing:constraint_classification(balfour_mandate_instruments__mandatory_interpretive_discretion, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: MANDATE SYSTEM AS LEGALIZED DISCRETION (PITON) — From a civilizational perspective, the mandate regime represents the degradation of a prior international law ideal: that colonialism could be reformed into trusteeship with accountability. The mandate system provides performative legitimacy (international oversight, fixed text, neutral arbitration) while retaining full discretionary authority in the mandatory power. Theater_ratio is high (0.68) because the institutional machinery (annual reports, covenant language, League supervision) creates the appearance of constraint while the operational reality is unchecked interpretive discretion. The piton persists because the system's legitimacy depends on the myth of constraint, even as the myth has atrophied into performative ritual.
constraint_indexing:constraint_classification(balfour_mandate_instruments__mandatory_interpretive_discretion, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, this perspective holds that interpretive discretion by a sovereign power in international affairs is an immutable feature of sovereignty itself: no international system can constrain a powerful state's reading of its own commitments without external enforcement, and external enforcement requires military capacity that the League lacks. This reading naturalizes the constraint as a law of international governance rather than a contingent institutional arrangement. The engine will identify this as a false summit: the structural data reveals that interpretive discretion is not inherent to all sovereignty, but rather emerges from the specific combination of vague treaty language + absent enforcement + power asymmetry.
constraint_indexing:constraint_classification(balfour_mandate_instruments__mandatory_interpretive_discretion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__mandatory_interpretive_discretion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(balfour_mandate_instruments__mandatory_interpretive_discretion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(balfour_mandate_instruments__mandatory_interpretive_discretion, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__mandatory_interpretive_discretion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(balfour_mandate_instruments__mandatory_interpretive_discretion, TR),
    TR >= 0.70.

:- end_tests(balfour_mandate_instruments__mandatory_interpretive_discretion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising over interval. Initial extractiveness (0.35 at 1920) reflects genuine ambiguity in the Balfour/Mandate language — both communities could initially plausibly interpret obligations in their favor. As the Mandate matured and British policy made successive interpretive choices (1922, 1930, 1939), extractiveness rose (0.52 by 1927, 0.58 by 1948) because it became clear that British discretion was the operative constraint, not textual meaning. The trajectory reflects the shift from ambiguity-based uncertainty to discretion-based lock-in. Suppression (0.72): High and rising. Suppression operates through reinterpretation: communities cannot appeal beyond British juridical authority; the legal ground shifts beneath them. The rise from 0.55 to 0.72 tracks the accumulation of policy reversals and the hardening of community expectations that appeal is impossible. Theater ratio (0.68): High and rising. The League's oversight machinery (annual reports, covenant language, international legitimacy) creates performative constraint while operational discretion remains unchecked. Theater rises as the gap widens between the mandate system's legitimacy claims (neutral trusteeship, international oversight) and its operational reality (unilateral British reinterpretation). The British administration benefits from the institutional theater — it legitimizes what is functionally unrestricted administrative discretion.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of classification across institutional and community perspectives. The British beneficiary sees coordination (Rope) — they are solving a genuine problem of managing competing obligations. The League sees both coordination function (international legitimacy for the mandate system) and asymmetric extraction (powerlessness over interpretation) — Tangled Rope. The piton perspective identifies the mandate system as degraded trusteeship: the institutional machinery (reports, covenant, oversight) persists through inertia and legitimacy claims while actual function (constraining the mandatory power) has atrophied. The powerless communities see extraction (Snare) — no appeal mechanism, policy oscillation, strategic uncertainty lock-in. The analytical observer risks naturalizing discretion as inherent to sovereignty (Mountain), but the structural data reveals this as a false summit: the constraint emerges from specific institutional choices (vague language + absent enforcement + power asymmetry), not from immutable features of international law.
 *
 * DIRECTIONALITY LOGIC:
 *   British institutional beneficiaries experience low directionality (d ≈ 0.20): they are net beneficiaries with arbitrage options (can reinterpret without exit cost). Their effective extraction (χ) is negative — the constraint subsidizes their administrative position. Arab and Jewish powerless victims experience high directionality (d ≈ 0.90): they are trapped with no arbitrage, bearing maximum extraction. The League occupies an intermediate position (d ≈ 0.55): organized but constrained by structural limits on oversight capacity. The perspectival gap is the directionality range: the beneficiary's low d produces χ ≤ 0 (no felt extraction); the victims' high d produces χ ≥ 1.0 (maximum felt extraction) — a spread of 1.0+ indicating fundamental disagreement about whether the constraint is beneficial or harmful.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is not present in this constraint — extractiveness (0.58) is below the 0.70 threshold requiring mandatrophy resolution. However, the constraint exhibits the diagnostic signature of potential mandatrophy: the piton perspective (institutional degradation) runs parallel to the snare perspective (extraction). If the League's oversight machinery had actually been capable of constraining British discretion, the constraint would resolve into tangled_rope (mixed coordination and extraction). The fact that the machinery failed to constrain suggests either institutional design failure or inherent structural limitation — which is precisely what the false-summit mandate debate captures. The constraint is stable at ε=0.58 without mandatrophy because the extraction is clear (not masked by perceived coordination) and the institutional theater is transparent (theater_ratio=0.68, not masked as essential function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is this constraint instantiated by the Balfour Declaration''s inherent textual ambiguity, or by British deliberate policy oscillation, or by both in feedback?',
    'Archival analysis of British Cabinet papers, policy memoranda, and internal debates (declassified 1974 onward). Track whether policy shifts responded to external pressure (community demands, League complaints) or were initiated internally. Examine contemporaneous legal opinions on the mandatory power''s interpretive scope.',
    'If textual ambiguity dominates: constraint is partially structural (rooted in vague language) and could be partially resolved by clarification. If deliberate oscillation dominates: constraint is fully structural (British policy is the mechanism) and clarification would not resolve it. If feedback: the reading''s core claim (discretion constitutes the constraint) is fully vindicated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Locus of interpretive discretion: textual ambiguity vs. deliberate policy oscillation').

omega_variable(
    sibling_reading_foreclosure,
    'Do the competing readings (jewish_national_home_primacy vs dual_obligation_indigenous_rights vs mandatory_interpretive_discretion) logically foreclose each other, or do they coexist as live readings held by different parties?',
    'Examine whether a single institutional framework (the League mandate, the British government, or a hypothetical international court) could simultaneously affirm two readings. If the readings make contradictory claims about what the Mandate text requires (e.g., primacy of Jewish national home vs. primacy of Arab rights), they foreclose. If different parties hold them simultaneously without internal contradiction, they coexist.',
    'If foreclosure: one reading''s acceptance logically eliminates the others from any single framework. If coexistence: the readings represent genuinely competing political positions, not resolvable through textual interpretation alone. This affects whether the constraint is a problem of ambiguous language or a problem of asymmetric power to choose which reading to apply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Logical structure among sibling readings of the Balfour kernel').

omega_variable(
    discretion_vs_constraint_boundary,
    'At what degree of policy oscillation does coordination (legitimate adaptive administration) become extraction (strategic uncertainty lock-in)?',
    'Quantify policy volatility: track changes to land regime, immigration quotas, settlement restrictions, and interpretive declarations across administrations (1920-1948). Measure community capacity to anticipate and plan against policy reversals. Examine whether reversals followed genuine new information or were strategic responses to community pressure.',
    'If high-frequency reversals with no coordination mechanism: extraction is maximal. If reversals follow consultative process: constraint is partially coordination. If reversals are predictable: suppression is lower (communities adapt). Current analysis assumes high volatility with minimal community input, supporting the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discretion_vs_constraint_boundary, empirical, 'Threshold between legitimate policy adaptation and extractive strategic uncertainty').

omega_variable(
    suppression_mechanism_internalization,
    'Did the strategic uncertainty become internalized in community politics (both Arab and Jewish actors adopting maximalist positions as insurance against future reversals), and does this internalization persist as suppression even after the Mandate ends?',
    'Post-mandate analysis (1948 onward): track whether community negotiating positions remain locked into maximalist framings. Examine whether trust barriers persist decades after British discretion is no longer operational. Identify whether the internalized suppression (expectation of deception) becomes a structural feature of Israeli-Palestinian relations independent of the original constraint mechanism.',
    'If internalization occurs: the constraint''s suppression extends beyond the Mandate period through psychological lock-in. The original mechanism (British discretion) ends, but the suppression it created becomes self-perpetuating. This would mark the constraint as having created path-dependent institutional structures that survive its formal termination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Internalization of suppression beyond the Mandate period').

omega_variable(
    league_mandate_legitimacy_paradox,
    'Does the League''s inability to constrain British interpretive discretion constitute a failure of the mandate system, or does the mandate system structurally require that the mandatory power retain discretion to be operationally viable?',
    'Comparative analysis of other League mandates (French in Syria/Lebanon, Belgian in Congo, Japanese in Pacific). Track whether all mandatory powers claimed similar interpretive discretion, whether League attempted enforcement, and what mechanisms (if any) constrained them. Examine League''s own legal interpretations of mandatory power scope in official decisions and committee reports.',
    'If discretion is systemic to mandate structure: the constraint is not unique to Balfour but emerges from the mandate form itself. This would suggest the constraint is better understood as a structural feature of international trusteeship rather than a pathology of British administration. If British discretion is exceptional: the constraint reflects deliberate policy choice rather than inherent structural limitation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(league_mandate_legitimacy_paradox, conceptual, 'Whether interpretive discretion is inherent to or pathological within the mandate system').

omega_variable(
    false_summit_detection,
    'Is the Mountain reading (interpretive discretion as an immutable feature of sovereignty) naturalizing a contingent institutional arrangement?',
    'Contrast the Balfour mandate constraint with cases of genuinely constrained interpretive authority: treaty adjudication by neutral international courts, binding arbitration mechanisms with enforcement, or treaty systems with third-party interpretation rights. If such systems exist and function, then interpretive discretion is not inherent to sovereignty — it is contingent on institutional design. The Mountain reading would then be a false summit.',
    'If genuine constraints on interpretive discretion are possible: the Mountain is a false summit. The constraint is not a law of nature but a choice of institutional design. Beneficiaries (the mandatory power) have naturalized what is actually a structural advantage. If no system constrains interpretive discretion: the Mountain reading is vindicated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_detection, empirical, 'Whether interpretive discretion is inherent to sovereignty or contingent on institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__mandatory_interpretive_discretion, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balfour_discr_tr_t0, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 0, 0.52).
narrative_ontology:measurement(balfour_discr_tr_t14, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 14, 0.62).
narrative_ontology:measurement(balfour_discr_tr_t28, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 28, 0.68).

% Extraction over time
narrative_ontology:measurement(balfour_discr_be_t0, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(balfour_discr_be_t7, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 7, 0.52).
narrative_ontology:measurement(balfour_discr_be_t28, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 28, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(balfour_discr_su_t0, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(balfour_discr_su_t7, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 7, 0.68).
narrative_ontology:measurement(balfour_discr_su_t28, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 28, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__mandatory_interpretive_discretion, enforcement_mechanism).
narrative_ontology:affects_constraint(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments__jewish_national_home_primacy).
narrative_ontology:affects_constraint(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments__dual_obligation_indigenous_rights).

% DUAL FORMULATION NOTE:
% The Balfour mandate constraint family decomposes into three structurally distinct constraint stories, each with different ε values reflecting different aspects of the kernel reading. This reading (mandatory_interpretive_discretion, ε=0.58, Snare) models the constraint as institutional authority structure. The sibling readings model it as textual interpretation problem: jewish_national_home_primacy (ε≈0.35, Rope) assumes British discretion is constrained by commitment to facilitate the national home; dual_obligation_indigenous_rights (ε≈0.55, Tangled Rope) assumes British discretion is constrained by fiduciary duty to existing communities. The epsilon values differ because they measure different observables: mandatory_interpretive_discretion measures whether community actors can appeal beyond British authority; jewish_national_home_primacy measures whether the Balfour language constrains policy; dual_obligation_indigenous_rights measures whether trusteeship principles constrain policy. All three stories share the same base facts but represent genuinely different constraint mechanisms. Linking them via network.affects_constraints routes the analysis through commitment-system decomposition rather than forcing one ε value to cover multiple mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(balfour_mandate_instruments__mandatory_interpretive_discretion, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
