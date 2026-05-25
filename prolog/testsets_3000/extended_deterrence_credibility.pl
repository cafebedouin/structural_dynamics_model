% ============================================================================
% CONSTRAINT STORY: extended_deterrence_credibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_extended_deterrence_credibility, []).

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
 *   constraint_id: extended_deterrence_credibility
 *   human_readable: Extended Deterrence Credibility in Alliance Architecture
 *   domain: geopolitical/security/alliance_dynamics
 *
 * SUMMARY:
 *   Extended deterrence credibility — the promise by one state to defend
 *   another militarily in response to hypothetical future aggression —
 *   creates a structural tension between genuine security coordination and
 *   asymmetric extraction. The constraint exhibits all seven perspectival
 *   classifications from a single set of base properties, making it a
 *   diagnostic exemplar for alliance dynamics. The protected population must
 *   bear military expenditure and strategic vulnerability while remaining
 *   unable to independently verify the credibility of the external guarantee.
 *   The allied security establishment experiences mixed coordination (joint
 *   interoperability, mutual defense planning) and extraction (forced
 *   expenditure levels, strategic subordination). The guarantor experiences
 *   pure coordination (the alliance enables global influence). The rival
 *   power experiences extraction (deterrence explicitly targets this actor)
 *   combined with coordination benefit (the rivalry provides strategic
 *   definition). The economic integration coalition sees the entire
 *   deterrence apparatus as temporary theater being superseded by
 *   supply-chain interdependence. The Cold War military establishment
 *   maintains procedural credibility-validation (exercises, force
 *   modernization) that has become substantially performative. The
 *   civilizational analytical observer risks seeing deterrence credibility
 *   gaps as unchangeable laws of international relations, but the structural
 *   data reveals these as contingent institutional artifacts. The theater
 *   ratio (0.68) reflects that contemporary extended deterrence credibility
 *   is increasingly performed through exercises, strategic communications,
 *   and force posturing rather than tested through actual commitment
 *   scenarios.
 *
 * KEY AGENTS:
 *   - Extended Deterrent Guarantor: Primary beneficiary (institutional/arbitrage) — gains geopolitical influence, basing rights, intelligence access, and market positioning through alliance commitment
 *   - Protected Population: Primary victim (powerless/trapped) — bears military expenditure and strategic vulnerability without independent verification capacity; cannot exit
 *   - Allied Security Establishment: Secondary beneficiary and victim (moderate/constrained) — coordinates genuine defense capabilities but experiences forced expenditure and career dependency on alliance structure
 *   - Rival Power: Moderate victim with coordination benefit (powerful/constrained) — targeted by deterrence but derives strategic definition and competitive structure from the rivalry
 *   - Economic Integration Coalition: Organized agents (organized/constrained) — building alternative security architecture through supply-chain interdependence with sunset logic
 *   - Cold War Alliance Apparatus: Institutional actor (institutional/arbitrage) — maintains performative credibility procedures; sees own role as degraded
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing institutional credibility gaps as unchangeable security dilemma properties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(extended_deterrence_credibility, 0.58).
domain_priors:suppression_score(extended_deterrence_credibility, 0.65).
domain_priors:theater_ratio(extended_deterrence_credibility, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(extended_deterrence_credibility, extractiveness, 0.58).
narrative_ontology:constraint_metric(extended_deterrence_credibility, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(extended_deterrence_credibility, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(extended_deterrence_credibility, tangled_rope).
narrative_ontology:human_readable(extended_deterrence_credibility, "Extended Deterrence Credibility in Alliance Architecture").
narrative_ontology:topic_domain(extended_deterrence_credibility, "geopolitical/security/alliance_dynamics").

domain_priors:requires_active_enforcement(extended_deterrence_credibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(extended_deterrence_credibility, extended_deterrent_guarantor).
narrative_ontology:constraint_beneficiary(extended_deterrence_credibility, allied_security_establishment).
narrative_ontology:constraint_victim(extended_deterrence_credibility, allied_populations).
narrative_ontology:constraint_victim(extended_deterrence_credibility, fiscal_resource_base).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROTECTED POPULATION (SNARE) — Trapped within the deterrence architecture; cannot exit or verify the credibility of the guarantee. Bears suppression (military expenditure, strategic vulnerability, strategic dependency on external actor) without meaningful exit. Maximum experienced extraction because population has no independent verification of deterrent credibility and cannot organize collective defection from the alliance.
constraint_indexing:constraint_classification(extended_deterrence_credibility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALLIED SECURITY ESTABLISHMENT (TANGLED ROPE) — Constrained by career dependency on the alliance (military leaders' legitimacy and promotion tied to alliance structure) but also coordinating genuine military interoperability and defense against mutual threats. Experiences both extraction (forced expenditure levels, strategic subordination to guarantor's interests) and real coordination benefit (access to advanced systems, joint planning, collective deterrence). Moderate extraction with high suppression of exit alternatives.
constraint_indexing:constraint_classification(extended_deterrence_credibility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EXTENDED DETERRENT GUARANTOR (ROPE) — Institutional actor with arbitrage capacity (can reallocate deterrent commitment, shift alliance priorities, withdraw). Experiences the constraint as pure coordination: maintaining credible commitment to allied defense enables broader geopolitical influence, intelligence access, basing rights, and market position. Net beneficiary — the alliance structure subsidizes guarantor's global reach.
constraint_indexing:constraint_classification(extended_deterrence_credibility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: RIVAL POWER (TANGLED ROPE) — Constrained by the deterrence structure itself (strategic landscape is organized around the extended guarantee). Experiences extraction (deterrence explicitly targeted at containing this actor) but also coordination benefit (rivalry provides strategic definition, enables signaling, structures deterrent competition). Powerful but constrained — exit would mean dissolution of the strategic architecture that defines great-power competition.
constraint_indexing:constraint_classification(extended_deterrence_credibility, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: ECONOMIC INTEGRATION COALITION (SCAFFOLD) — Organized agents (trade blocs, supply-chain networks, green-transition coalitions) are building economic interdependencies that reduce the functional role of military deterrence. They experience the deterrence constraint as temporary theater that will gradually be superseded by economic coordination mechanisms. Sunset logic: as economic integration deepens and mutual vulnerability increases through supply chains and climate dependencies, traditional military deterrence loses extractive force.
constraint_indexing:constraint_classification(extended_deterrence_credibility, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: COLD WAR ALLIANCE APPARATUS (PITON) — Military structure and procedural credibility-maintenance (NATO exercises, extended air defense drills, strategic force modernization) has become substantially performative. The original function (clear counterbalance to identifiable peer competitor) has degraded as threat architecture has become multipolar and diffuse. The alliance persists through institutional inertia — maintained because the alternative of reorganization is costly, not because it solves contemporary deterrence problems. High theater ratio as exercises perform credibility rather than build it.
constraint_indexing:constraint_classification(extended_deterrence_credibility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / GAME-THEORETIC VIEW (MOUNTAIN) — From a civilizational perspective, some deterrence credibility gap is inherent to the security dilemma: no state can fully verify another's commitment to an abstract future scenario, so all extended deterrence involves irreducible uncertainty. This perspective sees the credibility problem as a natural law of international relations. However, the structural data contradicts the mountain classification — the engine's false summit detector will identify this as naturalization of contingent institutional arrangements. The credibility gap is not unchangeable; it depends on institutional design, communication bandwidth, military integration, and intelligence sharing — all observable and alterable features.
constraint_indexing:constraint_classification(extended_deterrence_credibility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(extended_deterrence_credibility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(extended_deterrence_credibility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(extended_deterrence_credibility, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(extended_deterrence_credibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(extended_deterrence_credibility, TR),
    TR >= 0.70.

:- end_tests(extended_deterrence_credibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The guarantor captures significant geopolitical rents from the alliance structure — basing rights, political influence, market access, intelligence sharing — beyond the security coordination that would justify the commitment. The protected populations bear military expenditure that exceeds what independent threat assessment would recommend, suggesting extraction above the pure coordination baseline. However, extractiveness is not at snare levels (≥0.66) because genuine mutual defense coordination occurs and the guarantor does maintain costly capabilities. The value reflects that extraction is real but coexists with legitimate security functions. Suppression (0.65): High. Significant barriers to exit and verification include: (1) military vulnerability during any attempted exit period, (2) economic retaliation by the guarantor (market access withdrawal, technology embargoes), (3) strategic ambiguity about the rival's true intentions, (4) domestic political costs of questioning the guarantee (perceived as weakness), (5) information asymmetry — the guarantor controls the intelligence estimates that justify the deterrent commitment. Theater ratio (0.68): High and increasing. Extended deterrence credibility is increasingly maintained through performative activity — NATO exercises, nuclear posturing, strategic communication campaigns — rather than structural changes that would genuinely alter deterrent capacity. The interval measurements show theater increasing from 0.45 to 0.72 as actual military relevance has declined and symbolic credibility-maintenance has expanded.
 *
 * PERSPECTIVAL GAP:
 *   The guarantor sees Rope (pure coordination enabling geopolitical influence). The protected population sees Snare (extraction with suppression and no exit). These are not different measurements of the same thing — they are genuinely different structural experiences. The same deterrent commitment that coordinates the guarantor's global strategy extracts from the protected population's resources. The perspectival gap reveals that 'extended deterrence' is not a single constraint — it is a presheaf over the alliance structure, with different classification outcomes depending on the observer's power level, time horizon, exit capacity, and spatial scope. The gap between the guarantor's Rope and the population's Snare is the diagnostic signature of successful extraction: the extracting party experiences their own extraction as coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural relationship declarations. The guarantor's beneficiary status + arbitrage exit yields d ≈ 0.15 (institutional canonical). The protected population's victim status + trapped exit yields d ≈ 0.95 (powerless canonical). The allied security establishment's mixed beneficiary/victim status + constrained exit yields d ≈ 0.55 (moderate canonical). These derivatives feed the sigmoid f(d) function, producing experienced extractiveness χ = ε × f(d) × σ(S). The guarantor experiences low χ (ε=0.58, f(0.15)≈-0.01, σ(global)=1.2 → χ≈-0.007 ≈ net subsidy). The protected population experiences high χ (ε=0.58, f(0.95)≈1.42, σ(global)=1.2 → χ≈0.99, near-maximum extraction). The allied establishment experiences moderate χ (ε=0.58, f(0.55)≈0.75, σ(regional)=0.9 → χ≈0.39). The perspectival gap in directionality — from guarantor's low d to population's high d — is not a measurement error; it is the core structural feature that enables extraction: the same flows benefit one party and harm another depending on their position in the alliance.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: The constraint resolves mandatrophy by showing that Tangled Rope is the correct primary classification because the system exhibits BOTH genuine coordination (mutual defense, joint planning, interoperability) AND asymmetric extraction (beneficiary captures geopolitical rents, victims bear disproportionate costs, suppression of alternatives). The competing perspectives are legitimate readings from different structural positions, not classification errors. The guarantor's Rope experience is real (they coordinate mutual defense at low cost to themselves). The population's Snare experience is also real (they bear costs they cannot verify or exit). The disparity between these two experiences is not a bug in the framework — it is the diagnostic signature that this is Tangled Rope, not pure Rope or pure Snare. The mountain perspective from civilizational analysis is a false summit: the credibility gap appears unchangeable only when institutional design choices (transparency, integration depth, information architecture) are invisible. Making these choices visible transforms the mountain into tangled rope with measurable institutional leverage points. The mandatrophy is resolved by the presheaf: no single type is 'correct'; the classification landscape IS the answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credibility_verification_impossibility,
    'Is the gap between stated and actual extended deterrent commitment an irreducible uncertainty or a function of measurable institutional design choices?',
    'Comparative analysis of alliance designs with different information architectures (NATO transparency vs Chinese military opacity); historical cases where credibility was tested (Berlin crises, Taiwan straits); correlation between institutional design features and actual commitment when deterrence was challenged',
    'If irreducible: supports mountain perspective; deterrence credibility is a law of international relations. If institutional: supports tangled_rope; credibility gaps are artifacts of organizational secrecy, strategic ambiguity, and incentive misalignment — measurably reducible through institutional reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credibility_verification_impossibility, empirical, 'Whether deterrence credibility gap is natural or institutional').

omega_variable(
    extraction_vs_burden_sharing,
    'What percentage of allied military expenditure is genuine burden-sharing for mutual defense vs extractive subsidy to the guarantor''s global posture?',
    'Cost accounting for alliance infrastructure: basing costs, interoperability investments, capabilities developed for alliance vs unilateral use; comparison of cost distribution to threat distribution; counterfactual analysis of what military posture each ally would maintain absent the deterrent commitment',
    'If burden-sharing ≥70%: constraint is closer to pure Rope (coordination with fair cost division). If extraction ≥40%: constraint is clearly Tangled Rope or Snare depending on exit options. Determines victim designation and suppression interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_vs_burden_sharing, empirical, 'Ratio of burden-sharing to extractive subsidy in alliance expenditure').

omega_variable(
    rival_threat_credibility,
    'Does the rival power pose a sufficiently credible threat to justify the deterrent commitment, or is the threat partially theater-generated to justify the alliance''s existence?',
    'Capability assessment of rival vs alliance; trend analysis of threat metrics (military spending growth, capability development, aggressive actions); analysis of how deterrent commitment influences threat perception (security dilemma spiral) vs responds to independent threat growth',
    'If threat is genuine and growing: deterrent extraction is justified; constraint may reduce toward Rope. If threat is partly theater-maintained: deterrent becomes more piton-like; theater ratio should increase over measurement interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rival_threat_credibility, empirical, 'Whether rival threat justifies deterrent commitment or is partly self-generated').

omega_variable(
    economic_integration_substitution,
    'Are economic integration mechanisms genuinely building alternative security architecture, or are they merely supplementary to military deterrence?',
    'Historical analysis of trade bloc security outcomes (EU avoiding conflict despite high integration and weak deterrence structures); measurement of supply-chain vulnerability and mutual economic coercion capacity; analysis of whether trade blocs reduce military spending proportionally',
    'If genuine substitution: scaffold perspective is structurally valid; deterrence constraint has a real sunset and will degrade to piton within 20-30 years. If supplementary: scaffold is aspirational; economic integration does not reduce deterrence extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_integration_substitution, empirical, 'Whether economic integration substitutes for or supplements military deterrence').

omega_variable(
    identity_lock_in_guarantor,
    'Has the guarantor become identity-locked into the extended deterrence role, such that abandoning the commitment would fragment the guarantor''s own institutional coherence and global standing?',
    'Institutional analysis of how much of the guarantor''s military-industrial complex, alliance leadership identity, and strategic doctrine depends on extended deterrence commitment; counterfactual of what the guarantor''s security posture would look like absent allied dependencies; analysis of domestic political barriers to reallocation',
    'If identity-locked: guarantor cannot credibly commit to reducing the constraint even if it wanted to; the credibility problem is partly psychological rather than structural. If strategically optional: guarantor maintains the commitment as a choice, not a lock; credibility is genuine rather than trapped.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_in_guarantor, conceptual, 'Whether guarantor is identity-locked into extended deterrence role').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(extended_deterrence_credibility, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(edc_tr_t0, extended_deterrence_credibility, theater_ratio, 0, 0.45).
narrative_ontology:measurement(edc_tr_t10, extended_deterrence_credibility, theater_ratio, 10, 0.58).
narrative_ontology:measurement(edc_tr_t20, extended_deterrence_credibility, theater_ratio, 20, 0.68).
narrative_ontology:measurement(edc_tr_t30, extended_deterrence_credibility, theater_ratio, 30, 0.72).

% Extraction over time
narrative_ontology:measurement(edc_be_t0, extended_deterrence_credibility, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(edc_be_t10, extended_deterrence_credibility, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(edc_be_t20, extended_deterrence_credibility, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(edc_be_t30, extended_deterrence_credibility, base_extractiveness, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(extended_deterrence_credibility, enforcement_mechanism).
narrative_ontology:affects_constraint(extended_deterrence_credibility, arms_race_spiral_dynamics).
narrative_ontology:affects_constraint(extended_deterrence_credibility, security_dilemma_signaling).
narrative_ontology:affects_constraint(extended_deterrence_credibility, basing_rights_extraction).

% DUAL FORMULATION NOTE:
% Extended deterrence credibility is part of a constraint family with related security architecture claims. It affects arms race dynamics (deterrence credibility justifies military spending escalation), security dilemma signaling (credibility maintenance requires visible posturing that escalates rival threat perception), and basing rights extraction (guarantor's deterrence commitment enables market-rate-plus extraction on forward military bases). Each constraint has distinct ε values reflecting different measurement observables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(extended_deterrence_credibility, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
