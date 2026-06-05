% ============================================================================
% CONSTRAINT STORY: intermediate_range_nuclear_forces
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_intermediate_range_nuclear_forces, []).

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
 *   constraint_id: intermediate_range_nuclear_forces
 *   human_readable: Intermediate Range Nuclear Forces (INF) Treaty Architecture and Verification Paradox
 *   domain: geopolitical/military/arms_control
 *
 * SUMMARY:
 *   The Intermediate Range Nuclear Forces (INF) Treaty represents a
 *   structural constraint on military capability development across nuclear
 *   and non-nuclear states, generating asymmetric costs and benefits across
 *   multiple institutional levels. Signed in 1987 between the US and Soviet
 *   Union, the INF treaty banned ground-launched ballistic and cruise
 *   missiles with ranges of 500–5,500 kilometers. The constraint operates
 *   through a verification regime (on-site inspections, data exchanges,
 *   notification requirements) that monitors compliance and enforces
 *   restrictions. However, the constraint exhibits radically different
 *   structural characters depending on the observer's position: nuclear-armed
 *   signatories experience it as coordination enabling stability
 *   (Rope/Tangled Rope); non-nuclear allies experience it as entrapment under
 *   extended deterrence (Snare); non-signatories experience it as both
 *   constraint and opportunity (Snare through strategic competition, Rope
 *   through their own development pathways); verification institutions
 *   experience it as a source of authority and resource (Rope with arbitrage
 *   exit); and the verification ritual itself has become increasingly
 *   theatrical as technological workarounds proliferate faster than detection
 *   capabilities. The constraint's theater ratio rose from 0.42 in 1987 to
 *   0.58 by 2019 as on-site inspections became less reliable at detecting
 *   violations and as treaty compliance became performance of cooperation
 *   rather than technical assurance. Extractiveness increased correspondingly
 *   from 0.48 to 0.68 as rivals developed sophisticated workarounds (Russian
 *   Novator 9M729 cruise missile, Chinese DF-ZF hypersonic glide vehicle, US
 *   hypersonic development programs) that evade traditional verification. The
 *   US withdrawal from INF in 2019 marked a critical phase transition: the
 *   constraint shifted from a shared institutional framework to a fragmented
 *   regime where compliant powers (Russia, China, France) face different
 *   enforcement pressures and non-aligned states experience increased
 *   regional instability. The post-withdrawal extractiveness (0.72) reflects
 *   that the constraint now operates through unequal enforcement and alliance
 *   politics rather than through a symmetrical treaty framework.
 *
 * KEY AGENTS:
 *   - Non-nuclear states under extended deterrence (Japan, South Korea, NATO allies): Primary victims (powerless/trapped) — cannot develop independent intermediate-range capabilities without abandoning security guarantees; trapped in alliance structure with no exit
 *   - Regional security coalitions (NATO, US-Japan-Korea bilateral arrangements): Organized beneficiaries/mixed actors (organized/constrained) — benefit from restrictions on rivals but constrained by internal alliance commitments
 *   - Treaty enforcement institutions (OSCE INF Inspection Regime, UN verification bodies): Institutional beneficiaries (institutional/arbitrage) — derive legitimacy and authority from enforcement role; maintain maximum exit flexibility
 *   - Compliant nuclear powers (initially US, Soviet Union, now Russia as nominal signatory): Powerful mixed actors (powerful/mobile to constrained) — experience genuine coordination benefits from stability but also bear extraction costs from asymmetric compliance and workaround development
 *   - Non-signatory or violating powers (China, non-signatories): Powerful actors with mobile exit (powerful/mobile) — experience snare dynamics through strategic competition equilibrium despite nominal freedom to violate
 *   - Verification apparatus and protocol implementation: Institutional actor (institutional/constrained) — performs coordination function but increasingly theatrical as detection reliability declines
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(intermediate_range_nuclear_forces, 0.68).
domain_priors:suppression_score(intermediate_range_nuclear_forces, 0.72).
domain_priors:theater_ratio(intermediate_range_nuclear_forces, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(intermediate_range_nuclear_forces, extractiveness, 0.68).
narrative_ontology:constraint_metric(intermediate_range_nuclear_forces, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(intermediate_range_nuclear_forces, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(intermediate_range_nuclear_forces, snare).
narrative_ontology:human_readable(intermediate_range_nuclear_forces, "Intermediate Range Nuclear Forces (INF) Treaty Architecture and Verification Paradox").
narrative_ontology:topic_domain(intermediate_range_nuclear_forces, "geopolitical/military/arms_control").

domain_priors:requires_active_enforcement(intermediate_range_nuclear_forces).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(intermediate_range_nuclear_forces, treaty_enforcement_institutions).
narrative_ontology:constraint_victim(intermediate_range_nuclear_forces, non_nuclear_states).
narrative_ontology:constraint_victim(intermediate_range_nuclear_forces, regional_security_actors).
narrative_ontology:constraint_victim(intermediate_range_nuclear_forces, verification_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-NUCLEAR POWER (SNARE) — States under extended nuclear deterrence cannot exit the constraint without abandoning security guarantees. Trapped by both the INF framework and dependence on nuclear allies. Bears full cost of intermediate-range weapon restrictions while dependent on deterrence umbrella. Maximum suppression: material alternatives (conventional capability development, independent deterrence) blocked by treaty and ally pressure.
constraint_indexing:constraint_classification(intermediate_range_nuclear_forces, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL SECURITY COALITION (TANGLED ROPE) — Organized actors benefit from INF restrictions on rivals (Russia, China) while bearing constraints on their own capability development. Mixed incentive: coordination function (preventing regional destabilization through uncontrolled INF deployment) coexists with asymmetric extraction (treaty restricts coalition members' options more than rivals who never signed or now violate). Constrained exit: withdrawing from INF consensus risks fracturing alliance credibility.
constraint_indexing:constraint_classification(intermediate_range_nuclear_forces, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: TREATY ENFORCEMENT REGIME (ROPE) — Institutional beneficiary with maximum exit flexibility (arbitrage). Derives legitimacy, funding, and institutional authority from treaty compliance monitoring. Enforcement mechanism (verification inspections, dispute resolution) is genuine coordination solving collective action problem: preventing arms race spiral. Low experienced extraction because regime operators control exit (can redefine verification standards, apply selective enforcement, dissolve mechanisms without consequence).
constraint_indexing:constraint_classification(intermediate_range_nuclear_forces, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMPLIANT NUCLEAR POWER (TANGLED ROPE) — Initially compliant signatory experiences the constraint as mixed. Coordination function: INF restrictions prevent destabilizing arms races and reduce mutual risk. But asymmetric extraction: treaty binds compliant powers while rivals develop workarounds (cruise missiles classified as air-defense, hypersonic platforms, space-based systems). Mobile exit: can withdraw with political cost, or can develop treaty-compliant but functionally equivalent systems. Experienced extraction increases as rivals develop asymmetries.
constraint_indexing:constraint_classification(intermediate_range_nuclear_forces, tangled_rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: NON-SIGNATORY POWER (SNARE) — Paradoxically, even the power that violates or rejects INF still experiences snare dynamics through the constraint's institutional inertia: the rhetoric of INF enforcement, verification demands, and counter-force requirements structure military competition. Mobile exit is available (withdraw, violate openly) but carries high cost (sanctions, alliance realignment, first-strike vulnerability if Western powers maintain credible defense). The snare is not the treaty itself but the global strategic equilibrium that INF nominally regulates — exiting carries unacceptable risk even for powerful states.
constraint_indexing:constraint_classification(intermediate_range_nuclear_forces, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: TREATY VERIFICATION RITUAL (PITON) — The INF treaty's verification procedures (on-site inspections, data exchanges, notification requirements) are substantially theatrical: inspectors cannot verify treaty compliance with certainty (classified designs, covert testing, technological workarounds), and the theater persists despite low detection reliability. Theater ratio (0.58) reflects that much verification activity is performative — it signals cooperation and provides political theater rather than technical assurance. The ritual persists through institutional inertia even as rivals develop genuine workarounds that inspections cannot detect.
constraint_indexing:constraint_classification(intermediate_range_nuclear_forces, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From civilizational and universal scope, the INF constraint can appear as a natural law of strategic stability: the mathematical impossibility of preventing intermediate-range weapons development without perfect verification, the inevitable divergence between treaty text and technological reality, the structural requirement that any enforceable constraint must tolerate some violation. This perspective risks naturalizing contingent institutional choices (the choice to sign, the choice to enforce selectively, the choice to treat verification ritual as sufficient) as immutable limits. The engine's false summit detector flags this: the structural data reveals the mountain as perspectival naturalization rather than an inherent feature of strategic physics.
constraint_indexing:constraint_classification(intermediate_range_nuclear_forces, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(intermediate_range_nuclear_forces_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(intermediate_range_nuclear_forces, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(intermediate_range_nuclear_forces, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(intermediate_range_nuclear_forces, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(intermediate_range_nuclear_forces, TR),
    TR >= 0.70.

:- end_tests(intermediate_range_nuclear_forces_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. The constraint manifests as systematic asymmetry in capability restrictions across signatory and non-signatory powers. Initially (1987), extractiveness was moderate (0.48) because the US-Soviet symmetry aligned with mutual interest in stability. Extractiveness increased to 0.62 by the early 2010s as China developed intermediate-range capabilities outside the treaty framework and as Russian workarounds (9M729) created functional asymmetry while maintaining treaty compliance rhetoric. Post-US withdrawal (2019), extractiveness jumped to 0.72 as the constraint fragmented into unequal enforcement: Russia faces verification pressure, China faces none, allies of the US face pressure to support counter-INF strategies. Non-nuclear allies experience maximum extraction (near 1.0) because they cannot develop independent intermediate-range deterrence even if extended deterrence fails. Suppression (0.72): Very high and structural. Multiple layers suppress alternatives: (1) material barriers — intermediate-range capability development requires industrial capacity and testing ranges that most states lack; (2) institutional barriers — INF signatories face verification burden and diplomatic cost for capability development; (3) strategic barriers — developing intermediate-range weapons without nuclear deterrent invites conventional escalation and power imbalance; (4) alliance barriers — NATO members face internal consensus requirements and US opposition to independent European nuclear development. Non-nuclear allies face additional layer: abandoning extended deterrence requires unilateral nuclear development, which triggers sanctions and international isolation. Theater ratio (0.58, rising to 0.61 post-withdrawal): Moderately high. INF verification procedures involve on-site inspections and data exchanges that signal cooperation but have declining technical reliability. The 9M729 case exemplifies the theater: the missile was developed and deployed for several years before being formally identified as INF-violating, suggesting inspections either failed to detect it or detected it but lacked authority to enforce correction. Theater has increased post-withdrawal because verification now serves primarily political purpose (signaling compliance intent) rather than technical purpose (detecting violations with confidence).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates radical perspectival divergence across structural positions. The treaty enforcement regime sees Rope (coordination mechanism with institutional authority). Compliant nuclear powers see mixed Tangled Rope (benefits from rivals' restrictions, costs from workarounds and alliance commitments). Non-nuclear allies see Snare (trapped under deterrence umbrella with no capability development path). Non-signatories see Snare through different mechanism (forced into strategic competition arms race with no legitimizing framework). The analytical observer risks seeing Mountain (natural law of strategic stability) but the structural data contradicts this: the rising theater ratio (0.42→0.61) indicates that the constraint's mechanisms are not natural laws but institutions losing functional capacity. The perspectival gap between the benign Rope view (enforcement regime's experience) and the predatory Snare view (non-nuclear allies' experience) reveals that the constraint's coordination function is concentrated in great-power stability while extraction is distributed to weaker actors.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies systematically by agent's structural position relative to extraction flow. The treaty enforcement regime derives d ≈ 0.05 (full beneficiary): they control verification standards, can apply selective enforcement, and can redefine compliance criteria. Compliant nuclear powers derive d ≈ 0.40 (mixed): they benefit from rivals' restrictions but bear costs from workarounds and asymmetric development. Non-signatory powers derive d ≈ 0.75 (significant target): they are excluded from treaty framework but face verification pressure and strategic disadvantage. Non-nuclear allies derive d ≈ 0.95 (maximum target): they are trapped in dependent position with no capability development path and no exit from extended deterrence. The sigmoid f(d) function applies: allies at d=0.95 experience f(d)≈1.42 (powerless factor), multiplying their effective experienced extraction to high levels. The enforcement regime at d≈0.05 experiences f(d)≈-0.12 (negative extraction — they gain institutional authority). Scope modifier σ(S) is universal (σ=1.0) for the treaty framework but varies by regional perspective: local European perspective sees σ≈0.9 (regional competition), continental perspective sees σ≈1.0 (alliance structure), global perspective sees σ≈1.2 (US-Russia strategic balance). The chi formula produces: allies with (d=0.95, f(d)=1.42, σ=1.0) experience χ ≈ 0.68 × 1.42 × 1.0 ≈ 0.96 (extreme extraction); enforcement regime with (d=0.05, f(d)=-0.12, σ=1.2) experiences χ ≈ 0.68 × (-0.12) × 1.2 ≈ -0.10 (negative extraction — institutional gain).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The INF constraint requires resolution because it appears structurally different from different perspectives. The Snare classification (claimed_type) captures the dominant structural character: suppression is high (0.72), effective extraction for the primary victims is extreme, and the constraint's existence depends on continuous enforcement against actors' alternative preferences. However, the Rope and Tangled Rope perspectives are not false — they capture the genuine coordination function that exists for great powers managing mutual strategic stability. The mandatrophy resolves by recognizing that INF is fundamentally a **two-level constraint structure**: (1) at the great-power level, it functions as Tangled Rope — genuine coordination (preventing destabilizing arms race) coexists with asymmetric extraction (certain signatories develop workarounds); (2) at the alliance level, it functions as Snare — non-nuclear allies are trapped in dependent position with no capability development path and no exit. The constraint is not one thing viewed from two angles; it is two structurally distinct constraints that share the same institutional framework. The network relationship (affects_constraints linking to regional_deterrence_stability) captures this decomposition. The mandatrophy is fully resolved when the engine recognizes that the claimed Snare type applies to the victim-perspective views while the underlying Tangled Rope coordination function persists at the great-power level. This is not a contradiction in the framework — it demonstrates that indexical classification operates at the level of individual perspectives, not at the level of the constraint as a whole.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_technical_sufficiency,
    'Can INF compliance be verified with technical certainty given modern camouflage, dual-use technologies, and rapid prototyping?',
    'Forensic analysis of detected violations (Russian Novator 9M729); correlation between technical detection capability and actual compliance rates across treaty history; comparison with other arms control regimes (NPT, CTBT) verification success rates',
    'If verification insufficient: constraint is effectively pure suppression (Snare for all actors). If verification adequate: constraint enables genuine coordination (Rope/Tangled Rope become more stable). Current evidence suggests insufficient technical certainty, shifting classification toward Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(verification_technical_sufficiency, empirical, 'Technical sufficiency of INF verification protocols').

omega_variable(
    alliance_burden_distribution,
    'Are INF restrictions distributed symmetrically across NATO alliance members, or do extended deterrence asymmetries create systematic extraction from smaller allies?',
    'Comparative analysis of military capability development paths for US, France, Germany, Japan, South Korea under INF constraints; measurement of alliance resource flows and constraint costs by member state; historical analysis of INF exceptions and waivers by alliance member',
    'If symmetric: Tangled Rope classification holds across coalition. If asymmetric: smaller members experience Snare while great powers experience Rope, revealing hidden extraction mechanism. Recent NATO INF waiver debates suggest asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alliance_burden_distribution, empirical, 'Symmetry of INF burden distribution across alliance members').

omega_variable(
    treaty_withdrawal_reversibility,
    'If a major signatory withdraws (as happened with US INF withdrawal in 2019), is the treaty regime recoverable or does withdrawal constitute permanent institutional collapse?',
    'Analysis of post-2019 INF negotiations; measurement of verification regime robustness to withdrawal; assessment of whether intermediate-range weapons development accelerated irreversibly post-withdrawal',
    'If reversible: constraint is Scaffold with managed exit. If irreversible: constraint was Snare all along — the institutional framework depended on continuous great-power commitment. Empirical evidence suggests irreversibility, confirming Snare structural character.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_withdrawal_reversibility, empirical, 'Reversibility of treaty withdrawal and institutional collapse').

omega_variable(
    technological_workaround_inevitability,
    'Is the development of INF-compliant yet functionally equivalent systems (cruise missiles classified as air-defense, hypersonic platforms, space-based interceptors) an inevitable feature of arms control treaties or a fixable policy failure?',
    'Historical analysis of workaround patterns across arms control regimes (ABM, SALT, CTBT); technical assessment of whether future INF iterations can close loopholes or whether treaty text itself cannot constrain strategic intent at sufficiently granular level',
    'If inevitable: INF is fundamentally Snare — suppression cannot prevent underlying strategic competition. If fixable: alternative treaty architectures could achieve Tangled Rope or even Rope characteristics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_workaround_inevitability, conceptual, 'Whether INF loopholes reflect treaty design failures or inherent limits of arms control').

omega_variable(
    extended_deterrence_structural_necessity,
    'Is extended nuclear deterrence (NATO nuclear umbrella, US guarantees to Japan/Korea) structurally necessary for non-nuclear states'' security, or do alternatives (conventional deterrence, coalition defense, independent capability) exist but are foreclosed by treaty commitments?',
    'Counterfactual analysis of non-nuclear state defense capabilities without INF constraints; historical analysis of pre-INF military developments in allied states; game-theoretic assessment of security equilibria without extended deterrence',
    'If structural necessity: non-nuclear state entrapment (Snare) is genuine unavoidable feature. If foreclosed alternative: entrapment is contingent — institutional choice by allies to enforce constraints, revealing extraction. Policy implications differ significantly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extended_deterrence_structural_necessity, conceptual, 'Necessity of extended deterrence for non-nuclear allies under INF').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(intermediate_range_nuclear_forces, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inte_tr_t0, intermediate_range_nuclear_forces, theater_ratio, 0, 0.42).
narrative_ontology:measurement(inte_tr_t15, intermediate_range_nuclear_forces, theater_ratio, 15, 0.52).
narrative_ontology:measurement(inte_tr_t30, intermediate_range_nuclear_forces, theater_ratio, 30, 0.58).
narrative_ontology:measurement(inf_theater_post_withdrawal, intermediate_range_nuclear_forces, theater_ratio, 32, 0.61).

% Extraction over time
narrative_ontology:measurement(inte_be_t0, intermediate_range_nuclear_forces, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(inte_be_t15, intermediate_range_nuclear_forces, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(inte_be_t30, intermediate_range_nuclear_forces, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(inf_extractiveness_post_withdrawal, intermediate_range_nuclear_forces, base_extractiveness, 32, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(intermediate_range_nuclear_forces, enforcement_mechanism).
narrative_ontology:affects_constraint(intermediate_range_nuclear_forces, regional_deterrence_stability).
narrative_ontology:affects_constraint(intermediate_range_nuclear_forces, conventional_military_balance_europe).
narrative_ontology:affects_constraint(intermediate_range_nuclear_forces, us_alliance_extended_deterrence).

% DUAL FORMULATION NOTE:
% INF constraint family decomposes into two structurally distinct constraints sharing institutional framework: (1) great_power_strategic_stability (Tangled Rope, ε=0.45) — coordination function for US-Russia-China strategic balance; (2) intermediate_range_nuclear_forces_snare (Snare, ε=0.68) — entrapment of non-nuclear allies under extended deterrence. The unified INF treaty institution masks these two distinct constraint types operating simultaneously at different institutional levels. Decomposition reveals that post-withdrawal fragmentation (2019) effectively separated these into distinct regulatory regimes: Russia under verification pressure (Snare dynamics for Russia despite treaty text); China outside treaty framework entirely; US pursuing counter-INF strategy. The claimed_type (Snare) applies to the victim-perspective analysis; the coordination function (Tangled Rope) persists as a substructure of great-power interaction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(intermediate_range_nuclear_forces, powerful, 0.4).
constraint_indexing:directionality_override(intermediate_range_nuclear_forces, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
