% ============================================================================
% CONSTRAINT STORY: non_proliferation_treaty_enforcement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_non_proliferation_treaty_enforcement, []).

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
 *   constraint_id: non_proliferation_treaty_enforcement
 *   human_readable: Non-Proliferation Treaty Enforcement Regime
 *   domain: geopolitical/security/international_law
 *
 * SUMMARY:
 *   The Non-Proliferation Treaty enforcement regime represents a structural
 *   tension between its stated goal (prevent proliferation) and its actual
 *   function (enforce nuclear weapons monopoly on behalf of five treaty
 *   signatories). Signed in 1968 during the Cold War and implemented through
 *   the International Atomic Energy Agency, the NPT coordinates a security
 *   arrangement that legitimizes weapons states' nuclear arsenals while
 *   constraining non-nuclear states from developing similar capabilities. The
 *   regime exhibits characteristics of a Tangled Rope: it solves a genuine
 *   coordination problem (uncontrolled proliferation would undermine all
 *   signatories' security) while simultaneously extracting security autonomy
 *   and technological access from non-nuclear states. The constraint has
 *   degraded over 34 years (1990–2024): its core verification mechanism (IAEA
 *   inspections) has become increasingly asymmetric, its enforcement
 *   credibility has been undermined by non-compliance without penalty (North
 *   Korea, Iran, Pakistan precedents), and alternative security mechanisms
 *   (regional hegemony, conventional deterrence, unilateral sanctions) now do
 *   much of the work that NPT enforcement was designed for. Theater_ratio has
 *   risen from 0.48 to 0.64, indicating that diplomatic and verification
 *   activities are increasingly performative relative to their actual
 *   enforcement impact. The extractiveness trajectory reflects accumulating
 *   evidence that weapons states use the NPT framework to prevent peer
 *   competition rather than to genuinely coordinate proliferation prevention
 *   on symmetric terms.
 *
 * KEY AGENTS:
 *   - Nuclear Weapons States (US, Russia, UK, France, China): Primary beneficiaries (institutional/arbitrage) — use NPT to legitimize monopoly, prevent rival nuclear powers, maintain hegemonic deterrence structures
 *   - Non-Nuclear Threshold States (Iran, Brazil, South Korea, Turkey, etc.): Primary victims (powerless/trapped) — constrained by treaty obligations, fuel supply embargoes, intrusive inspections, denied access to enrichment/reprocessing technology
 *   - IAEA: Institutional enforcer (institutional/constrained) — coordinates verification but structurally constrained to avoid accountability for weapons states; experiences suppression from political pressure
 *   - Non-Aligned Movement: Organized coalition (organized/constrained) — calls for disarmament reforms and restructuring; has blocked reform through coalitional pressure but faces suppression from weapons state veto
 *   - Regional Hegemonic Powers (Israel, India, Pakistan): Constraint-violating actors (powerful/mobile) — developed nuclear weapons outside NPT framework; demonstrate that non-compliance is viable if regional power and external guarantees are sufficient
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a contingent institutional arrangement as inherent to proliferation physics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(non_proliferation_treaty_enforcement, 0.58).
domain_priors:suppression_score(non_proliferation_treaty_enforcement, 0.68).
domain_priors:theater_ratio(non_proliferation_treaty_enforcement, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(non_proliferation_treaty_enforcement, extractiveness, 0.58).
narrative_ontology:constraint_metric(non_proliferation_treaty_enforcement, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(non_proliferation_treaty_enforcement, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(non_proliferation_treaty_enforcement, tangled_rope).
narrative_ontology:human_readable(non_proliferation_treaty_enforcement, "Non-Proliferation Treaty Enforcement Regime").
narrative_ontology:topic_domain(non_proliferation_treaty_enforcement, "geopolitical/security/international_law").

domain_priors:requires_active_enforcement(non_proliferation_treaty_enforcement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(non_proliferation_treaty_enforcement, nuclear_weapons_states).
narrative_ontology:constraint_beneficiary(non_proliferation_treaty_enforcement, hegemonic_security_alliance).
narrative_ontology:constraint_victim(non_proliferation_treaty_enforcement, non_nuclear_threshold_states).
narrative_ontology:constraint_victim(non_proliferation_treaty_enforcement, global_proliferation_liability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-NUCLEAR THRESHOLD STATE (SNARE) — Countries with technical capacity but treaty obligation face structural extraction: denied access to fuel cycle technology, subject to IAEA inspections (nuclear states self-exempt), trapped in security dependency on nuclear umbrella states. Exit is theoretically possible (withdraw from NPT) but costs are near-total — economic sanctions, security isolation, international pariah status. The constraint extracts security autonomy while offering only the coordination benefit of 'not being attacked' — a coordination problem the treaty itself created by concentrating deterrent power among signatories. Suppression is maximal: technical barriers (enrichment/reprocessing embargoes), legal constraints (inspection regime asymmetry), economic barriers (fuel supply controls), and informational barriers (tacit knowledge in weapons science is guarded by nuclear states).
constraint_indexing:constraint_classification(non_proliferation_treaty_enforcement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL NON-ALIGNED COALITION (TANGLED ROPE) — Organized threshold states (India, Pakistan, Iran proxy networks, potential Brazilian breakout) experience both coordination and extraction. The NPT coordinates a security arrangement that prevents regional nuclear arms races (coordination function). But enforcement is asymmetric: regional rivals can build covert programs with limited detection capacity, while signatory states face escalating inspection regimes and fuel supply restrictions. Exit is costly but not fatal — constrained by sanctions and isolation, but viable if regional hegemony is achieved or external guarantors shift. The constraint has enforced suppression (inspection regimes, fuel controls) but also genuine coordination (prevents every regional power from pursuing simultaneously).
constraint_indexing:constraint_classification(non_proliferation_treaty_enforcement, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: NUCLEAR WEAPONS STATE BLOC (ROPE) — The five treaty signatories (US, USSR successor states, UK, France, China) experience the NPT as pure coordination: it legitimizes their monopoly on nuclear weapons, prevents destabilizing proliferation that would undermine deterrence stability, and enables arms control agreements among themselves. Exit is costless — they can and do withdraw from ancillary protocols (ABM Treaty, JCPOA side constraints) with minimal penalty. The constraint extracts nothing from them; they are the primary architects and beneficiaries. From their perspective, this is coordination solving the collective action problem of preventing uncontrolled proliferation.
constraint_indexing:constraint_classification(non_proliferation_treaty_enforcement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: IAEA AND INSPECTION REGIME (TANGLED ROPE) — The International Atomic Energy Agency coordinates verification (genuine function) but also enforces asymmetry: inspections of non-nuclear states are comprehensive and intrusive; inspections of weapons states are negotiated, limited, and often avoided. The IAEA experiences suppression through political constraints (cannot inspect without state consent, cannot mandate enforcement) but also benefits from institutional legitimacy and funding that the regime provides. Exit is constrained — IAEA director-general cannot speak freely without losing state support; agency has incentive to suppress evidence that enforcement is failing. The regime coordinates verification but extracts deference to powerful states.
constraint_indexing:constraint_classification(non_proliferation_treaty_enforcement, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: NON-ALIGNED MOVEMENT AND REFORM COALITION (SCAFFOLD) — Organized non-nuclear states have called for NPT amendment requiring nuclear disarmament timetables ('stepping stone' framing, New Agenda Coalition). This perspective sees the regime as temporary scaffold requiring sunset — a coordination mechanism for the Cold War that should sunset into universal disarmament. Organized states have exit options: they can bloc-vote for reform, threaten non-renewal (treaty renewal conferences occur every 5 years), or build alternatives (New START asymmetries suggest bilateral instead of universal frames). Suppression exists (powerful states block reform) but is not total — organized agents can constrain the powerful through coalitional dynamics. Theater remains high (disarmament conferences produce rhetoric without binding outcomes), but the coalition perspective is that theater is declining as enforcement credibility erodes.
constraint_indexing:constraint_classification(non_proliferation_treaty_enforcement, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: COLD WAR-ERA INSTITUTIONAL FRAMEWORK (PITON) — At civilizational scale, the NPT enforcement regime is largely inertial. The treaty was designed for a bipolar security environment (US-USSR standoff) where nuclear escalation was mutually assured destruction. That environment has degraded: US hegemony enabled unilateral withdrawal from ABM Treaty; multiple nuclear powers now exist; proliferation motivations have shifted from superpowers' proxy wars to regional hegemony, terrorism prevention, and status signaling. The enforcement mechanism (IAEA inspections, fuel supply controls, diplomatic pressure) persists through institutional inertia despite declining fit to actual security dynamics. Theater_ratio is high (inspection reports are produced and circulated but often ignored; disarmament conferences occur with no enforcement; Security Council resolutions are vetoed). The regime's core function — preventing proliferation in a bipolar world — is increasingly performed by alternative mechanisms (sanctions regimes, covert military strikes on weapons programs, regional security arrangements). NPT enforcement persists because dismantling it would be diplomatically costly, not because it is the primary mechanism preventing proliferation.
constraint_indexing:constraint_classification(non_proliferation_treaty_enforcement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal scope and civilizational horizon, some proliferation constraint is inherent to physics and resource availability: uranium enrichment is technologically difficult, weapons-grade material production leaves detectable signatures, weaponization requires tacit knowledge and testing. From this view, the 'constraint' is the physical limit to proliferation, not the diplomatic regime. NPT enforcement is epiphenomenal — countries that cannot proliferate cannot; countries that can (sufficient uranium, technical talent, willingness to absorb sanctions) will, regardless of treaty status. This perspective naturalizes the constraint as immutable law. However, the structural data contradicts this: the NPT's extractiveness (0.58) and suppression (0.68) are institutional, not physical, and the theater_ratio (0.64) reflects political performance, not natural law. The engine will flag this as a false summit — naturalization of what is actually a contingent institutional arrangement designed to concentrate power among signatories.
constraint_indexing:constraint_classification(non_proliferation_treaty_enforcement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(non_proliferation_treaty_enforcement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(non_proliferation_treaty_enforcement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(non_proliferation_treaty_enforcement, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(non_proliferation_treaty_enforcement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(non_proliferation_treaty_enforcement, TR),
    TR >= 0.70.

:- end_tests(non_proliferation_treaty_enforcement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The NPT extraction is not maximal (weapons states do offer coordination benefits: prevent regional arms races, establish mutual restraint norms, enable inspections of competitors). But extraction is substantial: non-nuclear states surrender fuel cycle sovereignty, accept asymmetric inspections, and remain dependent on weapons state security guarantees. The trajectory from 0.42 to 0.58 reflects increasing awareness that the regime primarily serves weapons state interests — post-Cold War, extraction has become more visible as coordination benefits (mutual deterrence stability) have declined in relevance. Suppression (0.68): High. Multiple suppression layers exist: (1) technical suppression — uranium enrichment embargoes, fuel supply controls, export controls on dual-use equipment; (2) legal suppression — inspection regimes that exclude weapons states, asylum from Security Council enforcement; (3) economic suppression — sanctions for non-compliance (Iran model); (4) informational suppression — tacit knowledge in weapons science is protected by weapons states, IAEA detection capabilities are limited. Theater ratio (0.64): Moderate-high. The regime's performative content has increased: disarmament conferences produce rhetoric without enforcement, IAEA reports are circulated but ignored by Security Council, sanctions regimes exist alongside NPT compliance expectations. The theater reflects degradation of enforcement credibility — the regime performs legitimacy without delivering enforcement.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how institutional framing collapses the perspectival gap. Weapons states and non-weapons states negotiate the same treaty text but experience it differently because the underlying structural relationship is asymmetric. Weapons states see NPT as legitimizing their monopoly (Rope, low extraction). Threshold states see NPT as enforcing dependency (Snare, high extraction). The treaty text is identical; the structural difference is the exit option: weapons states can withdraw and absorb minor costs; threshold states face economic and security isolation for withdrawal. This asymmetry is not accidental — it is engineered into the regime through the P5 veto structure and the asymmetric inspection provisions. The analytical observer risks seeing this as 'natural' proliferation prevention (Mountain) when it is actually a contingent institutional choice that could be restructured through symmetric disarmament obligations.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation chain operates as follows: (1) Beneficiaries/victims declared: weapons states are beneficiaries (NPT legitimizes their arsenal), threshold states are victims (NPT constrains their options), global proliferation liability is victim (regime fails to prevent proliferation holistically). (2) Power levels assigned: weapons states institutional (they design and enforce), threshold states powerless/organized (depending on coalition capacity), IAEA institutional (but constrained), Non-Aligned coalition organized (can bloc-vote). (3) Exit options assigned: weapons states arbitrage (can withdraw with minor cost), threshold states trapped/constrained (high cost to withdraw), organized threshold states constrained (can exit at political cost if sufficient coalition support exists), IAEA constrained (cannot refuse inspections, cannot mandate enforcement without state support). (4) Sigmoid f(d) applied: beneficiaries with arbitrage → d ≈ 0.05, f(d) ≈ -0.12 (negative extraction experienced). Victims with trapped → d ≈ 0.95, f(d) ≈ 1.42 (maximal extraction experienced). (5) Scope modifier σ(S) applied: global scope σ=1.2 amplifies verification difficulty and jurisdictional ambiguity, increasing χ for all agents. The mathematics reveals why the perspectival gap exists: the same base ε produces very different χ values because d values are structurally divergent.
 *
 * MANDATROPHY ANALYSIS:
 *   The NPT enforcement regime resolves the mandatrophy by demonstrating that all six types are legitimate perspectival readings. The mandate (prevent proliferation on symmetric terms) has been captured by beneficiaries (prevent proliferation in ways that protect our monopoly). This is the core mandatrophy: coordination is claimed, but extraction is performed. The analytical observer at universal/civilizational scale sees a Mountain (proliferation as inherent to physics) — but this is a false summit. The constraint is not immutable; it is contingent on institutional design. If the NPT were restructured to impose symmetric disarmament obligations on weapons states, or if enforcement were truly symmetric, the constraint would shift from Snare (from threshold state view) toward Tangled Rope or even Rope. The fact that weapons states have prevented such restructuring (through P5 veto and diplomatic pressure) is itself evidence that the extraction is intentional. The mandatrophy resolves when we recognize that the constraint's true type is Tangled Rope (genuine coordination function + asymmetric extraction), not Mountain (immutable law). The regime is neither pure coordination nor pure coercion; it is a hybrid that solves the coordination problem while extracting security asymmetry from non-weapons states.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    asymmetric_verification_legitimacy,
    'Does the asymmetry in IAEA inspection regimes (comprehensive for non-weapons states, negotiated for weapons states) constitute a coordination mechanism or pure extraction?',
    'Historical analysis of detection rates and policy outcomes: do asymmetric inspections increase overall security (coordination benefit), or do they primarily protect weapons states from accountability (extraction)? Counterfactual: what would happen if inspections were symmetric?',
    'If primarily coordination: NPT is a Tangled Rope with meaningful collective action solution. If primarily extraction: NPT is a Snare with enforcement legitimacy failing. Classification shifts from Tangled Rope to Snare from threshold state perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetric_verification_legitimacy, empirical, 'Whether asymmetric verification is coordination or extraction').

omega_variable(
    alternative_security_substitution,
    'Are alternative security mechanisms (regional hegemony, conventional deterrence, security alliances outside NPT) functionally substituting for NPT coordination?',
    'Cross-case analysis: comparison of proliferation rates and regional security stability in NPT-compliant vs non-compliant regions; assessment of whether Israel, India, Pakistan, North Korea security outcomes are explained by treaty membership or by regional power balances and external guarantees.',
    'If substitution is occurring: NPT enforcement is degrading to Piton status (inertial, performative). If NPT remains primary mechanism: constraint remains Tangled Rope from analytical view.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_security_substitution, empirical, 'Whether alternative security mechanisms are substituting for NPT enforcement').

omega_variable(
    extractive_intent_vs_security_necessity,
    'Do nuclear weapons states'' refusal to disarm reflect genuine security necessity or extractive preference for hegemonic power?',
    'Analysis of weapons state strategic doctrine and disarmament negotiations: if security necessity, disarmament should accelerate as security environment improves (post-Cold War); if extractive preference, disarmament should stall despite improving environment. Empirical signal: has the rate of weapons state disarmament accelerated or decelerated since 1991?',
    'If security necessity: extraction is coordination cost, not asymmetric rent-seeking. If extractive preference: NPT is core mechanism for enforcing nuclear monopoly on behalf of weapons states. Classification shifts from Tangled Rope to Snare from global analytical view.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractive_intent_vs_security_necessity, empirical, 'Whether weapons state non-disarmament reflects security necessity or extractive preference').

omega_variable(
    enforcement_credibility_degradation,
    'Has NPT enforcement credibility degraded since the Cold War, or has it stabilized at a new equilibrium?',
    'Time-series analysis of proliferation rates, IAEA detection capabilities, and enforcement action success: comparison of pre-1991 and post-1991 periods; assessment of whether recent nuclear programs (Iran, North Korea, Syria) were deterred by NPT enforcement or by alternative mechanisms (sanctions, military strikes, regional containment).',
    'If degraded: theater_ratio should increase and enforcement effectiveness should decline over the measurement interval, supporting Piton classification from institutional perspective. If stabilized: tangled_rope remains appropriate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_credibility_degradation, empirical, 'Whether NPT enforcement credibility has degraded').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(non_proliferation_treaty_enforcement, 0, 34).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_tr_t0, non_proliferation_treaty_enforcement, theater_ratio, 0, 0.48).
narrative_ontology:measurement(npt_tr_t17, non_proliferation_treaty_enforcement, theater_ratio, 17, 0.56).
narrative_ontology:measurement(npt_tr_t34, non_proliferation_treaty_enforcement, theater_ratio, 34, 0.64).

% Extraction over time
narrative_ontology:measurement(npt_be_t0, non_proliferation_treaty_enforcement, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(npt_be_t17, non_proliferation_treaty_enforcement, base_extractiveness, 17, 0.51).
narrative_ontology:measurement(npt_be_t34, non_proliferation_treaty_enforcement, base_extractiveness, 34, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(non_proliferation_treaty_enforcement, enforcement_mechanism).
narrative_ontology:affects_constraint(non_proliferation_treaty_enforcement, uranium_enrichment_technology_transfer).
narrative_ontology:affects_constraint(non_proliferation_treaty_enforcement, nuclear_fuel_supply_monopoly).
narrative_ontology:affects_constraint(non_proliferation_treaty_enforcement, iaea_verification_asymmetry).

% DUAL FORMULATION NOTE:
% NPT enforcement is downstream of the post-WWII security architecture (US hegemony, Soviet containment). It coordinates verification but exists primarily to enforce weapons state monopoly. This story links to three related constraints: uranium enrichment technology control (ε=0.52, institutional extraction preventing dual-use access), fuel supply monopoly (ε=0.61, economic extraction through cartel-like control), and IAEA verification asymmetry (ε=0.48, institutional extraction through inspection regime manipulation). Each has its own extractiveness value reflecting domain-specific mechanisms; NPT enforcement represents the meta-constraint that coordinates these mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(non_proliferation_treaty_enforcement, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
