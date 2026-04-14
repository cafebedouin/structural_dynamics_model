% ============================================================================
% CONSTRAINT STORY: nuclear_site_safety_norms
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_site_safety_norms, []).

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
 *   constraint_id: nuclear_site_safety_norms
 *   human_readable: International Nuclear Site Non-Proliferation and Safety Norms
 *   domain: geopolitical/technological
 *
 * SUMMARY:
 *   The international norm against military attacks on nuclear power sites
 *   represents a geopolitical constraint operating simultaneously as
 *   coordination mechanism and power-locking extraction system. The norm
 *   emerged from the mutually recognized danger of nuclear accidents and the
 *   shared interest in predictable coexistence (coordination logic: 'we won't
 *   attack your plants if you won't attack ours'). However, the norm's
 *   enforcement mechanisms and structural consequences reveal significant
 *   asymmetry: nuclear-armed states benefit from a rule prohibiting attacks
 *   on their critical infrastructure, while non-nuclear states are prevented
 *   from using nuclear facilities as military leverage and locked into
 *   conventional military inferiority. The constraint exhibits the six DR
 *   types across different observational contexts, making it a diagnostic
 *   case for how geopolitical rules embed power relationships. The
 *   theater_ratio increase from 0.42 to 0.58 reflects the growing
 *   performative character of IAEA verification as inspection access has
 *   become more restricted and cyberattacks have created attack vectors
 *   beyond traditional kinetic monitoring. The extractiveness increase from
 *   0.38 to 0.52 tracks the accumulating asymmetry as more states approach
 *   nuclear capability while remaining norm-bound.
 *
 * KEY AGENTS:
 *   - Non-Nuclear Threshold States: Primary victims (powerless/trapped) — cannot build deterrent, cannot threaten nuclear infrastructure, face asymmetric vulnerability
 *   - Nuclear Operator States: Primary beneficiary (moderate/constrained) — gain protection for civilian and military nuclear infrastructure; experience constrained military options
 *   - IAEA and International Monitoring Regime: Institutional beneficiary (institutional/arbitrage) — captures verification authority and norm-maintenance legitimacy; inspectors and diplomats gain professional roles
 *   - Nuclear-Armed States (P5 + Others): Secondary beneficiary (powerful/arbitrage) — benefit from norm preventing attacks on military nuclear sites; maintain deterrent credibility
 *   - Anti-Nuclear Civil Society: Organized agent (organized/constrained) — view norm as temporary protection during transition to renewables; advocate for sunset clause via policy change
 *   - Cold War Verification Apparatus: Institutional actor (institutional/arbitrage) — persists through inertia; increasingly performative as cyberattacks and satellite surveillance change technical landscape
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing geopolitical hierarchy as immutable consequence of physics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_site_safety_norms, 0.52).
domain_priors:suppression_score(nuclear_site_safety_norms, 0.68).
domain_priors:theater_ratio(nuclear_site_safety_norms, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_site_safety_norms, extractiveness, 0.52).
narrative_ontology:constraint_metric(nuclear_site_safety_norms, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(nuclear_site_safety_norms, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_site_safety_norms, tangled_rope).
narrative_ontology:human_readable(nuclear_site_safety_norms, "International Nuclear Site Non-Proliferation and Safety Norms").
narrative_ontology:topic_domain(nuclear_site_safety_norms, "geopolitical/technological").

domain_priors:requires_active_enforcement(nuclear_site_safety_norms).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_site_safety_norms, nuclear_operator_states).
narrative_ontology:constraint_beneficiary(nuclear_site_safety_norms, civilian_populations_near_plants).
narrative_ontology:constraint_victim(nuclear_site_safety_norms, militarily_weaker_states).
narrative_ontology:constraint_victim(nuclear_site_safety_norms, non_nuclear_threshold_states).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-NUCLEAR THRESHOLD STATE (SNARE) — A militarily weaker state adjacent to a nuclear-armed neighbor faces maximum extraction under the norm. It cannot build nuclear deterrent (violates international pressure), cannot attack nuclear sites (universal norm suppression), and cannot exit the asymmetric military relationship. The constraint enforces structural vulnerability. Full experienced extraction — trapped in asymmetric power relationship with zero alternatives.
constraint_indexing:constraint_classification(nuclear_site_safety_norms, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: OPERATOR STATE WITH REGIONAL NUCLEAR ASSETS (TANGLED ROPE) — A state with nuclear plants experiences mixed extraction and coordination. The norm protects its plants from attack (genuine coordination benefit: mutual security), but also constrains its military options and reduces deterrent credibility in regional conflicts. Constrained exit — state can pursue limited conventional deterrence but not nuclear escalation. Significant but not total extraction.
constraint_indexing:constraint_classification(nuclear_site_safety_norms, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: IAEA AND INTERNATIONAL MONITORING REGIME (ROPE) — The institutional enforcement apparatus experiences the norm as pure coordination: IAEA inspections, Non-Proliferation Treaty, and verification protocols solve the collective action problem of verifying compliance without enforcement wars. Institutional actors with arbitrage options (inspectors, diplomats, verification specialists) benefit from the norm's coordination function. Low or negative effective extraction — the regime captures institutional authority and legitimacy through norm maintenance.
constraint_indexing:constraint_classification(nuclear_site_safety_norms, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANTI-NUCLEAR CIVIL SOCIETY (SCAFFOLD) — Organized anti-nuclear movements (Greenpeace, International Physicians for Prevention of Nuclear War, etc.) see the norm as temporary protection while underlying nuclear dependence is phased out. The constraint has a sunset: decarbonization via renewables would eliminate nuclear sites and the attack-surface entirely. Theater ratio is moderate-high (performative arms-control rhetoric masks continuing nuclear expansion); sunset clause is genuine (energetic transition pathways exist). Organized agents have constrained exit via policy advocacy.
constraint_indexing:constraint_classification(nuclear_site_safety_norms, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: COLD WAR VERIFICATION APPARATUS (PITON) — The institutional machinery that emerged from Cold War arms control (on-site inspection protocols, mutual assured destruction verification, technical confidence-building measures) persists substantially through inertia. Many inspection protocols and treaty structures remain performative theater — they provide political reassurance rather than genuine verification capability. Theater ratio is high because modern verification increasingly relies on commercial satellite data rather than treaty-mandated inspections. The apparatus is degraded but maintained because alternatives (unilateral security, coalition verification) haven't fully replaced it.
constraint_indexing:constraint_classification(nuclear_site_safety_norms, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / PHYSICS AND CONSEQUENCE VIEW (MOUNTAIN) — From a civilizational/universal perspective, the constraint might appear as a natural law: nuclear weapons make nuclear sites inherently vulnerable military targets, and the only stable outcome is a norm against attacking them (prisoner's dilemma equilibrium). However, the structural data reveals this as a false summit — the norm is enforced through geopolitical hierarchy and military imbalance, not through physics. If states had equal nuclear capability, the strategic logic changes entirely. The 'immutable necessity' framing naturalizes a contingent power structure.
constraint_indexing:constraint_classification(nuclear_site_safety_norms, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_site_safety_norms_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nuclear_site_safety_norms, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nuclear_site_safety_norms, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nuclear_site_safety_norms, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nuclear_site_safety_norms, TR),
    TR >= 0.70.

:- end_tests(nuclear_site_safety_norms_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, reflecting the dual nature of the constraint. For nuclear operator states, extractiveness is lower (~0.30) because they genuinely benefit from mutual site protection. For non-nuclear threshold states, extractiveness is very high (~0.75) because the norm prevents them from using nuclear infrastructure as deterrent while offering no security gain. The aggregate value (0.52) reflects the population-weighted average: many nuclear operator states exist, but the structural asymmetry for threshold states drives the mean upward. The trend toward higher extractiveness (0.38→0.52) reflects the increasing number of states approaching nuclear threshold without the ability to break the norm. Suppression (0.68): High. Multiple mechanisms suppress alternatives: (1) international law (Non-Proliferation Treaty, UN General Assembly resolutions); (2) economic sanctions targeting nuclear-capable states; (3) military coercion (threat of preventive strikes on nuclear programs); (4) technical barriers (monitoring and export controls); (5) reputational cost (diplomatic isolation). But suppression is not total (~0.80) because some states have pursued covert programs and some explicit challenges to the norm exist. Theater ratio (0.58): Moderate-high. IAEA inspections provide genuine verification in many cases, but access restrictions, technical limitations, and reliance on commercial satellite imagery (rather than treaty-mandated on-site inspection) have increased performative content. The theater increased from 0.42 to 0.58 as cyber-attack vectors emerged (Stuxnet 2009, ICS compromises) that inspection protocols don't detect.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival divergence. The IAEA and monitoring institutions see Rope (pure coordination solving collective verification problems). Nuclear operator states see mixed Tangled Rope (protection + constraint). Non-nuclear threshold states see Snare (pure extraction with no exit). Anti-nuclear movements see Scaffold (temporary constraint with a sunset via energetic transition). The Cold War apparatus sees itself as degraded Piton (performative rather than functional). The civilizational analytical observer risks seeing Mountain (naturalized physics of nuclear deterrence requiring site protection) when the structural data reveals contingent geopolitical hierarchy. The perspectival gap arises from the radically different exit options and structural positions: agents with arbitrage options (institutions, powerful states) experience coordination; agents with trapped exits experience extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) reflect each agent's structural position relative to the extraction flow. Non-nuclear threshold states have d ≈ 0.90 (full targets): they face maximum suppression, zero arbitrage options, trapped exit — the sigmoid f(d) produces high experienced extractiveness. Nuclear operator states have d ≈ 0.45 (near-symmetric): they both benefit (site protection) and bear costs (military constraints), mobile exit options via treaty withdrawal, organized power — produces moderate experienced extractiveness. The IAEA and monitoring regime have d ≈ 0.15 (beneficiary): they capture institutional authority, have arbitrage options (inspection contracts, technical expertise), and experience negative effective extraction (they are subsidized by the norm). The Cold War apparatus has d ≈ 0.20 (slight beneficiary): persistence through inertia suggests some constituency benefits; but degradation (piton status) indicates the beneficiary groups are themselves weakening. Anti-nuclear civil society has d ≈ 0.55 (slightly victimized): they lack exit options (policy change requires international consensus) but also lack full trapment (they can advocate, protest, and build alternative movements). The directionality derivation shows why the constraint classifies as Tangled Rope rather than pure Snare: multiple agent types experience it differently, ranging from pure coordination (IAEA) to pure extraction (threshold states), with net asymmetric extraction requiring active enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question for this constraint is: 'Does the norm primarily solve a collective action problem (pure coordination that all parties benefit from) or does it primarily lock in geopolitical hierarchy (extraction masked as coordination)?' The constraint resolves this by explicitly declaring beneficiaries (nuclear operator states, IAEA) and victims (non-nuclear threshold states, militarily weaker neighbors). The presence of both genuine coordination (IAEA verification, mutual site protection) and asymmetric extraction (preventing threshold states from using nuclear leverage) classifies it as Tangled Rope, not as pure Rope or pure Snare. The mandatrophy is avoided by recognizing that the indexical context matters: from the IAEA's perspective, it's Rope; from a threshold state's perspective, it's Snare. The constraint doesn't collapse to a single type because the structural data supports multiple readings. The classification 'Tangled Rope at the institutional/global context' represents the meta-level fact that the norm serves both coordination and extraction simultaneously, with the balance depending on observer position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    norm_vs_power_asymmetry,
    'Is the nuclear site safety norm a genuine coordination mechanism that benefits all parties, or does it primarily lock in military asymmetry favoring nuclear-armed states?',
    'Comparative analysis of: (a) security gains for non-nuclear states from norm stability vs. deterrence losses from inability to threaten nuclear infrastructure; (b) strategic doctrine evolution in threshold states post-norm; (c) conflict outcome analysis comparing norm-compliant and norm-violating scenarios',
    'If genuine coordination: all perspectives should converge toward Rope at some context parameters. If power-locking: non-nuclear and weak-state perspectives remain Snare even at institutional/organized levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norm_vs_power_asymmetry, empirical, 'Whether norm provides mutual security coordination or locks in military hierarchy').

omega_variable(
    iaea_verification_sufficiency,
    'Does IAEA inspection regime provide genuine verification of non-military use of nuclear materials, or is the inspection process substantially performative (theater) given limited access and technical constraints?',
    'Historical analysis of IAEA catches vs. undetected military programs (Iran pre-2015, North Korea timeline); assessment of inspection frequency vs. detection capability; comparison of IAEA findings to subsequent declassified intelligence on actual non-compliance',
    'If verification is genuine: theater_ratio should be lower (~0.35). If substantially performative: theater_ratio justified at 0.58+.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(iaea_verification_sufficiency, empirical, 'Whether IAEA inspections provide genuine verification capability').

omega_variable(
    retaliation_threshold_clarity,
    'What defines ''attack on nuclear site''? Is the norm about kinetic strikes on reactors, or does it extend to cyberattacks, sabotage, and infrastructure disruption? Ambiguity in the threshold affects enforcement credibility.',
    'Legal analysis of treaty language; review of UN General Assembly resolutions on nuclear site protection; case study analysis of incidents (Stuxnet, Syrian reactor strike) and international response patterns',
    'If threshold is clear and narrow (kinetic only): norm is easier to enforce, suppression higher, extractiveness lower. If threshold is vague: ambiguity increases military temptation, suppression lower, extractiveness higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retaliation_threshold_clarity, conceptual, 'Definitional clarity of what constitutes prohibited attack on nuclear site').

omega_variable(
    nuclear_phase_out_timeline,
    'How credible is the energetic transition away from nuclear power? If renewable energy and storage costs continue declining, will nuclear dependency actually decline, enabling the Scaffold sunset clause?',
    'Techno-economic analysis of renewable + storage LCOE trajectory; policy commitments from major nuclear operators (France, Germany, Japan) and threshold states; sensitivity analysis on capital cost learning curves',
    'If transition is credible (post-2050): Scaffold perspective has structural validity; norm is genuinely temporary. If transition stalls: Scaffold is aspirational rather than structural; constraint calcifies at current extraction levels.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nuclear_phase_out_timeline, empirical, 'Credibility of nuclear phase-out enabling norm sunset').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_site_safety_norms, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nssn_tr_t0, nuclear_site_safety_norms, theater_ratio, 0, 0.42).
narrative_ontology:measurement(nssn_tr_t25, nuclear_site_safety_norms, theater_ratio, 25, 0.55).
narrative_ontology:measurement(nssn_tr_t50, nuclear_site_safety_norms, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(nssn_be_t0, nuclear_site_safety_norms, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(nssn_be_t25, nuclear_site_safety_norms, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(nssn_be_t50, nuclear_site_safety_norms, base_extractiveness, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_site_safety_norms, enforcement_mechanism).
narrative_ontology:affects_constraint(nuclear_site_safety_norms, nuclear_weapons_proliferation_prevention).
narrative_ontology:affects_constraint(nuclear_site_safety_norms, uranium_export_controls).
narrative_ontology:affects_constraint(nuclear_site_safety_norms, international_iaea_authority).

% DUAL FORMULATION NOTE:
% The nuclear site safety norm decomposes into three structurally distinct constraints: (1) prevention of military attacks on civilian/military reactors (this story, ε≈0.52); (2) prevention of nuclear weapons proliferation via access to fissile material (ε≈0.65, higher extraction); (3) IAEA institutional authority and verification regime (ε≈0.35, lower extraction). These share enforcement mechanisms but have different beneficiaries and victims. The proliferation prevention constraint has higher extractiveness because it prevents threshold states from accessing technology; the site safety norm has moderate extractiveness because it offers genuine mutual protection.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nuclear_site_safety_norms, powerless, 0.9).
constraint_indexing:directionality_override(nuclear_site_safety_norms, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
