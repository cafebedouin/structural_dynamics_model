% ============================================================================
% CONSTRAINT STORY: 1968_johnson_nuclear_nonproliferation_treaty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_1968_johnson_nuclear_nonproliferation_treaty, []).

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
 *   constraint_id: 1968_johnson_nuclear_nonproliferation_treaty
 *   human_readable: Nuclear Nonproliferation Treaty Framework (1968)
 *   domain: foreign_policy/international_security
 *
 * SUMMARY:
 *   The Nuclear Nonproliferation Treaty (NPT), which entered into force in
 *   1970, represents a foundational institutional constraint in the
 *   international system. It creates a categorical boundary between 'nuclear'
 *   states (five permanent UN Security Council members: US, USSR/Russia, UK,
 *   France, China) and 'non-nuclear' states, institutionalizing a
 *   hierarchical structure justified on grounds of global security. The
 *   treaty binds non-nuclear signatories to forgo weapons development and
 *   accept intrusive international verification (IAEA inspections) in
 *   exchange for security guarantees and access to peaceful nuclear
 *   technology. Nuclear signatories retain arsenals, maintain asymmetric
 *   verification burden (no equivalent inspections), and provide security
 *   umbrella commitments. The constraint exhibits strong tangled-rope
 *   characteristics: it solves a genuine collective action problem
 *   (preventing unrestricted proliferation and arms races) while
 *   simultaneously extracting benefits to existing nuclear powers through
 *   institutionalized hierarchy, asymmetric verification, and categorical
 *   inequality in the international system. The measurement trajectory shows
 *   rising extractiveness (0.48 → 0.62 over 56 years) and rising theater
 *   ratio (0.45 → 0.64), indicating that as post-Cold War security contexts
 *   have diverged from the 1968 bipolarity model, the NPT's functional
 *   verification role has been increasingly supplemented by performative
 *   compliance theater and ritualized diplomacy. Non-nuclear states face
 *   persistent trade-off between security guarantees (often unreliable, as
 *   demonstrated by Ukraine, Libya, and Iran cases) and constraints on
 *   sovereign weapons development, creating perverse outcomes where
 *   security-threatened states either remain trapped in the NPT framework or
 *   exit into non-signatory status (India, Pakistan, Israel, North Korea) and
 *   face severe sanctions. The analytical paradox: the treaty prevents some
 *   proliferation but may generate proliferation pressure precisely in states
 *   that see the security guarantee as inadequate. The categorical boundary
 *   (nuclear/non-nuclear) is an institutional artifice that creates both
 *   coordination gains and extraction losses, making the NPT's classification
 *   depend entirely on whether the observer values security stability or
 *   state autonomy.
 *
 * KEY AGENTS:
 *   - Existing Nuclear Powers (P5): Institutional/arbitrage beneficiaries — retain arsenals, escape verification, control technology markets, use security guarantees as leverage over non-nuclear states
 *   - Non-Nuclear Aspirant States: Powerless/trapped victims — face categorical prohibition on weapons development, subject to IAEA inspections, dependent on security guarantees, constrained from sovereign defense capabilities
 *   - Emerging Regional Powers (Japan, Germany, South Korea): Powerful/constrained beneficiaries-with-costs — gain security through non-nuclear status and great-power guarantees, but constrained from weapons development despite regional threats; experience tangled-rope mixed benefits and extraction
 *   - Non-Signatory Proliferators (India, Pakistan, Israel, North Korea): Moderate/constrained trapped outside — face sanctions and isolation for weapons pursuit; trapped between security necessity and international prohibition; trapped in either direction
 *   - IAEA / International Verification Regime: Institutional/constrained actors — genuine coordination function (detecting weapons programs) combined with asymmetric burden (non-nuclear states inspected, nuclear states self-monitored)
 *   - Non-Nuclear Security-Threatened States: Powerless/trapped inside — signatories like Ukraine, Iraq (pre-2003), Iran face security threats despite non-nuclear compliance and security guarantees; trapped in the treaty framework with unreliable guarantees
 *   - Analytical Observer: Civilizational/analytical — sees the structural paradox that NPT creates both security coordination AND institutionalized inequality; notes categorical boundary as contingent institutional artifact rather than natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(1968_johnson_nuclear_nonproliferation_treaty, 0.58).
domain_priors:suppression_score(1968_johnson_nuclear_nonproliferation_treaty, 0.68).
domain_priors:theater_ratio(1968_johnson_nuclear_nonproliferation_treaty, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(1968_johnson_nuclear_nonproliferation_treaty, extractiveness, 0.58).
narrative_ontology:constraint_metric(1968_johnson_nuclear_nonproliferation_treaty, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(1968_johnson_nuclear_nonproliferation_treaty, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(1968_johnson_nuclear_nonproliferation_treaty, tangled_rope).
narrative_ontology:human_readable(1968_johnson_nuclear_nonproliferation_treaty, "Nuclear Nonproliferation Treaty Framework (1968)").
narrative_ontology:topic_domain(1968_johnson_nuclear_nonproliferation_treaty, "foreign_policy/international_security").

domain_priors:requires_active_enforcement(1968_johnson_nuclear_nonproliferation_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(1968_johnson_nuclear_nonproliferation_treaty, existing_nuclear_powers).
narrative_ontology:constraint_beneficiary(1968_johnson_nuclear_nonproliferation_treaty, global_security_stability).
narrative_ontology:constraint_victim(1968_johnson_nuclear_nonproliferation_treaty, non_nuclear_aspirant_states).
narrative_ontology:constraint_victim(1968_johnson_nuclear_nonproliferation_treaty, sovereign_weapons_development_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-NUCLEAR ASPIRANT STATE (SNARE) — Structurally trapped by the NPT's categorical prohibition on weapons development. Possesses no exit pathway: treaty withdrawal incurs massive international sanctions, technology embargoes, and security isolation. The state experiences maximum extraction: constrained to permanent non-nuclear status while existing nuclear powers retain arsenals. No meaningful coordination benefit — the treaty provides security guarantees that are historically unreliable (Ukraine, Libya cases). Pure suppression mechanism with minimal coordination function.
constraint_indexing:constraint_classification(1968_johnson_nuclear_nonproliferation_treaty, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EMERGING REGIONAL POWER (TANGLED ROPE) — Powerful but constrained by NPT verification and international pressure. Experiences genuine coordination benefit: the treaty reduces regional arms races and provides legitimacy through non-nuclear status (Japan, Germany, South Korea). Also experiences extraction: constrained from weapons development despite security threats; subject to intrusive IAEA inspections; dependent on security guarantees from nuclear powers. Mixed extraction and coordination — neither pure coordination nor pure suppression.
constraint_indexing:constraint_classification(1968_johnson_nuclear_nonproliferation_treaty, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: EXISTING NUCLEAR POWER—SIGNATORY (ROPE) — Experiences the NPT as pure coordination. The treaty legitimizes existing arsenals, provides verification asymmetry (only non-nuclear states face intrusive inspections), and enables strategic partnerships with non-nuclear allies. No meaningful extraction — the signatory nuclear power gains security, strategic influence, and access to nuclear technology markets. Exit is costless (France withdrew from integrated NATO command for decades; Russia and China maintain ambiguous compliance). Net beneficiary.
constraint_indexing:constraint_classification(1968_johnson_nuclear_nonproliferation_treaty, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SECONDARY REGIONAL POWER—NON-SIGNATORY (SNARE) — States outside the NPT (India, Pakistan, Israel, North Korea) face severe extraction from alternative mechanisms: weapons development provokes sanctions, military intervention threats, and isolation. Some develop arsenals anyway (at enormous cost), creating a perverse outcome: NPT + sanctions + security threats produce de facto weapons development under maximum suppression. Trapped between treaty constraints and military necessity, paying extraction cost (sanctions, isolation) either way. Theater high because the international response is performative (Iran sanctions theater, North Korea isolation theater).
constraint_indexing:constraint_classification(1968_johnson_nuclear_nonproliferation_treaty, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL VERIFICATION REGIME / IAEA (TANGLED ROPE) — Coordination function: provides transparent monitoring and early warning of proliferation. Extraction function: intrusive inspections on non-nuclear signatories create sovereignty costs; asymmetric burden (nuclear powers not subject to equivalent scrutiny); IAEA itself becomes venue for great-power politics and resource control. The regime both solves a genuine coordination problem (detecting weapons development) and enforces asymmetric constraints. Theater moderate — inspections are real but selective and politically influenced.
constraint_indexing:constraint_classification(1968_johnson_nuclear_nonproliferation_treaty, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: COLD WAR DETERRENCE LOGIC (PITON) — The NPT's original function (stabilizing superpowers' mutual assured destruction through preventing proliferation) has degraded. Post-Cold War, the treaty persists through institutional inertia: renewal conferences every five years, ritualized negotiations, performative compliance theater. The actual security mechanism that justified the treaty (bipolarity + mutual deterrence + verification) has transformed (unipolarity, emerging multipolarity, asymmetric threats, cyber-enabled deception). The treaty remains because alternatives haven't fully replaced it, not because the Cold War logic still works. Organized agents can reform the framework (NPT Review Conferences), but they choose performance over revision. Theater ratio (0.64) reflects that much NPT activity is procedural rather than functionally verifying actual nonproliferation.
constraint_indexing:constraint_classification(1968_johnson_nuclear_nonproliferation_treaty, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the NPT is a tangled rope: it genuinely coordinates against unrestricted proliferation AND systematically extracts from non-nuclear states by institutionalizing a permanent hierarchical divide. The constraint creates categorical inequality in the international system (haves/have-nots in nuclear weapons) and uses verification asymmetry to enforce it. The paradox: the treaty prevents some proliferation but may induce proliferation in states that see the security guarantee as untrustworthy (causing India, Pakistan, Israel, North Korea to pursue weapons outside the framework). The framework's core tension is that it treats a continuous spectrum of nuclear capability as a binary categorical boundary — states are either 'nuclear' (protected, legitimate, unverified) or 'non-nuclear' (threatened, constrained, verified). This categorical artificiality is what produces both coordination and extraction.
constraint_indexing:constraint_classification(1968_johnson_nuclear_nonproliferation_treaty, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(1968_johnson_nuclear_nonproliferation_treaty_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(1968_johnson_nuclear_nonproliferation_treaty, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(1968_johnson_nuclear_nonproliferation_treaty, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(1968_johnson_nuclear_nonproliferation_treaty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(1968_johnson_nuclear_nonproliferation_treaty, TR),
    TR >= 0.70.

:- end_tests(1968_johnson_nuclear_nonproliferation_treaty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The NPT distributes benefits and costs asymmetrically: nuclear powers gain legitimacy, security, and control over technology markets without equivalent constraints; non-nuclear signatories forgo weapons development and endure intrusive verification. The extraction is not maximal (pure suppression = 1.0) because some non-nuclear states genuinely benefit from security guarantees and participation in the peaceful nuclear technology regime. However, the measurement of 0.58 reflects that the net extraction flow runs from non-nuclear to nuclear powers, particularly for security-threatened non-nuclear states who receive unreliable guarantees. Rising trajectory (0.48→0.62) indicates that as security contexts have diverged from Cold War bipolarity (post-Soviet breakup, emergence of China, 9/11, proliferation of asymmetric threats), the treaty's functional verification role has degraded relative to its hierarchical legitimation function. Suppression (0.68): High. Non-nuclear signatories face significant barriers to exit: economic sanctions, military intervention threats, isolation from international institutions, and diplomatic costs. The only states that have exited (North Korea withdrew from NPT in 2003) face comprehensive sanctions. Aspiring nuclear powers face a dilemma: stay in the treaty and constrain sovereignty, or exit and face severe extraction. Theater ratio (0.64): Moderate-high. NPT Review Conferences have become increasingly performative: the 2005 and 2015 conferences produced no final consensus documents, retreating into procedural statements. Compliance theater (ritualized inspections, bureaucratic verification) has increased as the actual security threats have shifted to domains the NPT doesn't address (cyber proliferation, dual-use technology, state-sponsored terrorism, hypersonic weapons). IAEA inspections in countries like Iraq and Iran have detected weapons programs, demonstrating genuine coordination function, but much of the NPT apparatus (the five-yearly review conferences, the indefinite extension negotiation in 1995, the permanent committee structure) functions more as institutional performance than as active verification. Claimed type (tangled_rope): Required both beneficiaries (existing nuclear powers, global stability) and victims (non-nuclear aspirants, sovereign weapons development capacity), as well as active enforcement (IAEA inspections, sanctions regime, security guarantees). The constraint is not pure coordination (Rope) because the extraction for non-nuclear states is substantial and constrain sovereignty. It is not pure extraction (Snare) because some non-nuclear states genuinely benefit from security guarantees and peaceful nuclear technology, and the treaty does reduce proliferation pressures through coordination of a shared problem.
 *
 * PERSPECTIVAL GAP:
 *   The NPT generates maximum perspectival divergence across the indexical tuple. The non-nuclear aspirant state (powerless/trapped/global scope) sees a Snare — pure suppression with unreliable security guarantees and no exit option. The existing nuclear power (institutional/arbitrage/global scope) sees a Rope — pure coordination that legitimizes their position and enables partnerships. The emerging regional power (powerful/constrained/regional scope) sees a Tangled Rope — genuine security benefits from non-nuclear status and great-power guarantee, but constrained from weapons development despite regional threats. The analytical observer (analytical/analytical/universal scope) sees a Tangled Rope structured as institutional hierarchy — the treaty solves a genuine coordination problem but through mechanisms that extract legitimacy and power asymmetry to existing nuclear states. The piton perspective captures the institutional inertia: the Cold War logic that justified the NPT (bipolarity, mutual deterrence, verification as confidence-building) no longer describes the security environment, yet the treaty persists through bureaucratic ritual rather than active functional verification. The gap between beneficiary (nuclear power: Rope) and victim (non-nuclear aspirant: Snare) is maximal — they experience the same constraint as almost opposite types because their structural positions are nearly inverted.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies dramatically across perspectives based on structural position relative to the extraction flow. Existing nuclear powers occupy d ≈ 0.05-0.15 (beneficiaries with arbitrage exit) — they derive low d because they benefit from the constraint and can exit costlessly. The sigmoid f(d) produces negative or near-zero χ for these agents — they experience zero or negative effective extraction. Non-nuclear aspirant states occupy d ≈ 0.90-0.95 (victims with trapped exit) — constrained from weapons development with no exit option except to accept isolation. The sigmoid f(d) produces maximum χ ≈ 1.42, making the constraint maximally extractive from their perspective. Emerging regional powers occupy d ≈ 0.55-0.65 (mixed beneficiary-victim with constrained exit) — they derive moderate d because they receive security guarantees (beneficiary characteristics) but face regional threats and weapons constraints (victim characteristics). Non-signatory proliferators occupy d ≈ 0.70-0.80 (victims with constrained exit) — they face sanctions and isolation for pursuing weapons development, but have chosen to exit the treaty framework rather than accept the NPT's suppression. The IAEA as institutional actor occupies d ≈ 0.50-0.60 (symmetric position with constrained exit) — the regime has coordination function (genuine verification) and enforcement function (asymmetric verification burden on non-nuclear states). All directionality values are derived from beneficiary/victim declarations (existing nuclear powers and global security stability are beneficiaries; non-nuclear aspirants and sovereign weapons capacity are victims) plus exit options (arbitrage for nuclear powers, trapped for aspirants, constrained for others). No overrides are necessary because the structural relationships are clearly defined: nuclear powers benefit from the constraint and can exit; non-nuclear states bear costs and cannot exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The NPT resolves the mandatrophy through explicit acknowledgment of tangled structure: the constraint is BOTH a coordination mechanism AND an extraction mechanism, and these functions are inseparable in the institutional design. The coordination problem it solves is genuine — an arms-race dilemma where all states prefer fewer weapons than they would develop absent coordination — and the NPT does reduce proliferation pressure relative to an unregulated counterfactual. The extraction it enables is also genuine — non-nuclear states pay sovereignty costs, verification costs, and security-guarantee uncertainty while nuclear powers retain capabilities and escape equivalent constraints. The mandatrophy is NOT resolved by claiming one function dominates the other; it's resolved by recognizing that the institutional design (categorical boundary, asymmetric verification, security guarantees to non-nuclear signatories) necessarily produces both effects simultaneously. The rising theater ratio (0.45→0.64) indicates that the coordination function has been increasingly supplemented by performance as security contexts have diverged from 1968 assumptions. The constraint remains tangled_rope (not degrading to Piton) because IAEA verification still detects real weapons programs (Iraq, Iran cases) and prevents some proliferation. However, the increasing theater ratio suggests that IF the theater continues rising above 0.75 or extractiveness continues rising above 0.70, the engine would flag this as potential mandatrophy crisis — a constraint that began as genuine tangled_rope (solving real coordination problem with real extraction cost) but is devolving into pure extraction mechanism (Snare) masked by performance theater. The counterfactual history would be needed to resolve whether this is degradation or realization: did the NPT prevent a potential proliferation explosion (coordination success) or merely redirect proliferation into non-signatory channels (extraction without gain)? The omegas identify this as the irreducible empirical uncertainty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_asymmetry_legitimacy,
    'Does asymmetric verification (non-nuclear states inspected; nuclear states self-monitored) represent a necessary structural asymmetry or institutionalized inequality that delegates legitimacy to great powers?',
    'Counterfactual analysis: modeling symmetric verification (all signatories subject to equivalent IAEA inspections); empirical assessment of whether symmetric verification would reduce proliferation motivation or increase false-alarm detection rates',
    'If necessary asymmetry: NPT is efficient coordination mechanism (Rope from beneficiary perspective, Tangled Rope from victim perspective, justified). If institutionalized inequality: NPT is hierarchical constraint extracting legitimacy and asymmetric power (shifts victim perspective further toward Snare)',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_asymmetry_legitimacy, conceptual, 'Whether verification asymmetry is necessary structural feature or institutionalized inequality').

omega_variable(
    security_guarantee_credibility,
    'Are security guarantees provided by NPT to non-nuclear signatories credible commitment devices or rhetorical theater masking abandonment risk?',
    'Historical case analysis: Ukraine (NPT signatory, invaded despite security assurance), Japan/South Korea (NPT signatories, face existential threats, maintain non-nuclear status despite uncertainty), Iran (NPT signatory, faced military strikes threat despite compliance). Empirical test: do states that have received security guarantees behave as though they trust the commitment?',
    'If credible: NPT provides genuine coordination benefit to non-nuclear states (classification shifts toward Rope). If rhetorical: security guarantees are theater, non-nuclear states are effectively trapped (classification shifts toward Snare)',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(security_guarantee_credibility, empirical, 'Credibility of security guarantees in NPT framework').

omega_variable(
    proliferation_trajectory_counterfactual,
    'Would nuclear proliferation have been significantly higher absent the NPT, or has the treaty merely redirected proliferation into non-signatory pathways?',
    'Comparative historical analysis of proliferation timelines pre-/post-1968; assessment of proliferator motivations (security threats vs. regime prestige vs. technological capability); modeling of whether NPT prevented proliferation or merely enforced non-signatory status on proliferators',
    'If prevented proliferation: NPT functions as effective coordination mechanism reducing global weapons stockpiles (supports Rope/Tangled Rope classification). If redirected proliferation: NPT functions as categorization mechanism that permits nuclear powers while suppressing non-signatories (supports extraction interpretation, shifts toward Snare)',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proliferation_trajectory_counterfactual, empirical, 'Whether NPT prevented proliferation or merely created non-signatory category').

omega_variable(
    collective_action_problem_vs_hierarchical_imposition,
    'Is the NPT a solution to a genuine collective action problem (arms race dilemma) that all states prefer to escape, or a hierarchical imposition that some non-nuclear states would reject if exit costs were lower?',
    'Survey of non-nuclear states regarding treaty exit preferences absent sanctions; analysis of treaty ratification patterns (forced vs. voluntary); examination of proliferation pressure from security threats vs. great-power demand for non-proliferation',
    'If genuine collective action problem: extraction is justified coordination cost (Rope perspective for beneficiaries, Tangled Rope for constrained states). If hierarchical imposition: extraction represents domination of weak by strong, and the constraint is better classified as Snare from non-nuclear state perspective',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_action_problem_vs_hierarchical_imposition, preference, 'Whether NPT solves collective action problem or imposes hierarchy').

omega_variable(
    theater_ratio_measurement_basis,
    'How much of NPT compliance activity is functional (genuine verification, detection of weapons programs) versus performative (ritualized review conferences, bureaucratic compliance theater)?',
    'Content analysis of NPT Review Conference outcomes (proportion of binding decisions vs. rhetorical statements); IAEA inspection effectiveness data (percentage of weapons programs detected vs. missed); comparison to alternative transparency mechanisms (bilateral inspections, open-source intelligence)',
    'If mostly functional (theater_ratio < 0.40): the constraint is primarily coordination mechanism. If mostly performative (theater_ratio > 0.70): the constraint functions as institutional theater maintaining hierarchical legitimacy rather than actual proliferation control',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_ratio_measurement_basis, empirical, 'Functional vs. performative content in NPT compliance mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(1968_johnson_nuclear_nonproliferation_treaty, 0, 56).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_theater_1968, 1968_johnson_nuclear_nonproliferation_treaty, theater_ratio, 0, 0.45).
narrative_ontology:measurement(npt_theater_1988, 1968_johnson_nuclear_nonproliferation_treaty, theater_ratio, 20, 0.52).
narrative_ontology:measurement(npt_theater_1998, 1968_johnson_nuclear_nonproliferation_treaty, theater_ratio, 30, 0.61).
narrative_ontology:measurement(npt_theater_2018, 1968_johnson_nuclear_nonproliferation_treaty, theater_ratio, 50, 0.64).

% Extraction over time
narrative_ontology:measurement(npt_extract_1968, 1968_johnson_nuclear_nonproliferation_treaty, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(npt_extract_1988, 1968_johnson_nuclear_nonproliferation_treaty, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(npt_extract_1998, 1968_johnson_nuclear_nonproliferation_treaty, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(npt_extract_2018, 1968_johnson_nuclear_nonproliferation_treaty, base_extractiveness, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(1968_johnson_nuclear_nonproliferation_treaty, enforcement_mechanism).
narrative_ontology:affects_constraint(1968_johnson_nuclear_nonproliferation_treaty, iaea_verification_inspections).
narrative_ontology:affects_constraint(1968_johnson_nuclear_nonproliferation_treaty, great_power_security_guarantees).
narrative_ontology:affects_constraint(1968_johnson_nuclear_nonproliferation_treaty, nuclear_technology_market_access).

% DUAL FORMULATION NOTE:
% The NPT framework decomposes into three structurally distinct constraints: (1) the categorical boundary enforcement (ε≈0.58, Tangled Rope) — the mechanism that defines and maintains the nuclear/non-nuclear distinction; (2) verification regime asymmetry (ε≈0.45, Tangled Rope) — the IAEA inspection structure that monitors non-nuclear signatories; (3) security guarantee reliability (ε≈0.72, Snare) — the credibility of promises to protect non-nuclear signatories from nuclear threats. All three are linked: the categorical boundary enforcement is the primary constraint; verification asymmetry is the mechanism that maintains the boundary; security guarantee reliability is the quid pro quo that justifies non-nuclear compliance. All stories should link via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
