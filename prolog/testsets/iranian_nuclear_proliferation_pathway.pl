% ============================================================================
% CONSTRAINT STORY: iranian_nuclear_proliferation_pathway
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_iranian_nuclear_proliferation_pathway, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: iranian_nuclear_proliferation_pathway
 *   human_readable: Iranian Nuclear Proliferation Pathway
 *   domain: geopolitical/security/nonproliferation
 *
 * SUMMARY:
 *   The Iranian nuclear proliferation pathway represents a structural
 *   constraint spanning security, economics, and identity. Iran faces
 *   escalating sanctions in response to nuclear enrichment advancement, while
 *   simultaneously being trapped in a security dilemma where external
 *   pressure (military threats, economic isolation) reinforces the regime's
 *   rationality for pursuing nuclear deterrence. The constraint exhibits
 *   snare characteristics: suppression is high (military encirclement,
 *   economic isolation, diplomatic exclusion), extractiveness has grown over
 *   time (escalating sanctions create asymmetric costs), and alternative
 *   pathways have been foreclosed or delegitimized. The JCPOA represented a
 *   temporary scaffold — a negotiated pathway to normalize Iran's nuclear
 *   program in exchange for transparency. Its 2018 collapse by the US
 *   restored the snare structure. The constraint is experienced differently
 *   across structural positions: the Iranian civilian population bears
 *   maximum extraction with no exit; regional competitors and US-led
 *   coalition experience coordination benefits; the nonproliferation regime
 *   itself is corrupted by selective enforcement; diplomatic infrastructure
 *   can see a potential sunset through renewed negotiation. The rising
 *   extractiveness and theater_ratio over the interval reflect both the
 *   intensification of sanctions (effective extraction increasing) and the
 *   increasing performative content of enforcement (theater ratio rising as
 *   selective application becomes more visible).
 *
 * KEY AGENTS:
 *   - Iranian Civilian Population: Primary victim (powerless/trapped) — bears economic costs of sanctions, currency devaluation, medical shortages, inflation without capacity to influence policy
 *   - Iranian State Apparatus: Primary victim (powerless/identity_locked) — structurally mobile but identity-locked through regime legitimacy narratives; exit would require abandoning foundational claims about national resistance
 *   - US-Led Sanctions Coalition: Primary beneficiary (institutional/arbitrage) — defines enforcement rules, benefits from first-mover advantage in technology restrictions, can exit constraint unilaterally
 *   - Regional Competitors (Saudi Arabia, Israel, UAE): Secondary beneficiary (institutional/arbitrage) — benefit from Iranian capability constraints, US military commitments, arms sales opportunities
 *   - International Nonproliferation Regime (IAEA, NPT): Secondary victim (organized/constrained) — chartered to coordinate verification but extracted from by great-power unilateralism; experiences constraint as both coordination problem and extraction mechanism
 *   - Nuclear Diplomacy Infrastructure: Organized actor (organized/constrained) — sees sunset pathway through JCPOA-like negotiations; constrained by enforcement coalition's exit from diplomacy
 *   - Cold War Frameworks (Export controls, IAEA enforcement): Institutional actor (institutional/arbitrage) — maintains performative institutional structures through inertia; applies selective enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(iranian_nuclear_proliferation_pathway, 0.68).
domain_priors:suppression_score(iranian_nuclear_proliferation_pathway, 0.75).
domain_priors:theater_ratio(iranian_nuclear_proliferation_pathway, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(iranian_nuclear_proliferation_pathway, extractiveness, 0.68).
narrative_ontology:constraint_metric(iranian_nuclear_proliferation_pathway, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(iranian_nuclear_proliferation_pathway, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(iranian_nuclear_proliferation_pathway, snare).
narrative_ontology:human_readable(iranian_nuclear_proliferation_pathway, "Iranian Nuclear Proliferation Pathway").
narrative_ontology:topic_domain(iranian_nuclear_proliferation_pathway, "geopolitical/security/nonproliferation").

domain_priors:requires_active_enforcement(iranian_nuclear_proliferation_pathway).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(iranian_nuclear_proliferation_pathway, us_led_sanctions_regime).
narrative_ontology:constraint_beneficiary(iranian_nuclear_proliferation_pathway, regional_competitors).
narrative_ontology:constraint_victim(iranian_nuclear_proliferation_pathway, iranian_civilian_population).
narrative_ontology:constraint_victim(iranian_nuclear_proliferation_pathway, iranian_economy).
narrative_ontology:constraint_victim(iranian_nuclear_proliferation_pathway, nuclear_nonproliferation_regime).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IRANIAN CIVILIAN POPULATION (SNARE) — Trapped by sanctions regime with no exit mechanism. Bears economic costs of asset freezes, currency collapse, medical supply shortages, and inflation without capacity to influence nuclear policy. Suppression is structural: capital controls, banking isolation, and collective punishment create barriers to exit (relocation, economic opportunity). Maximum experienced extraction relative to power.
constraint_indexing:constraint_classification(iranian_nuclear_proliferation_pathway, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: IRANIAN STATE APPARATUS (SNARE, IDENTITY_LOCKED) — Structurally mobile (could theoretically abandon nuclear program, normalize relations) but identity-locked: the Islamic Republic's foundational legitimacy claims and regional strategic posture are constituted through nuclear resistance to external pressure. Exit would require the regime to abandon its core identity narrative ('standing against imperialism'). The state could pay material costs to rejoin the international system, but exit would require becoming a different political entity. The binding mechanism is cognitive/ideological rather than purely material.
constraint_indexing:constraint_classification(iranian_nuclear_proliferation_pathway, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: INTERNATIONAL NONPROLIFERATION REGIME (TANGLED ROPE) — Organized actors (IAEA, NPT signatories, enforcement institutions) experience the Iranian pathway as both a coordination problem and an extraction mechanism. The regime must coordinate inspection protocols and information-sharing while simultaneously being extracted from by state actors using nonproliferation rhetoric to justify unilateral sanctions and military threats. Constrained by sovereignty norms that limit enforcement capacity. Genuine coordination function (preventing nuclear accidents, sharing safety information) exists alongside asymmetric extraction (great powers define the rules unilaterally).
constraint_indexing:constraint_classification(iranian_nuclear_proliferation_pathway, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: US-LED SANCTIONS COALITION (ROPE) — Institutional beneficiary (powerful/arbitrage). Experiences the constraint as coordination: maintaining sanctions coalition, intelligence sharing on nuclear progress, and allied military positioning enable collective action against Iranian capabilities. Benefits from first-mover advantage in defining escalation pathways and technology restrictions. Can exit the constraint (lift sanctions, recognize nuclear Iran) without systemic cost to their own position. Net beneficiary with high agency.
constraint_indexing:constraint_classification(iranian_nuclear_proliferation_pathway, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: REGIONAL COMPETITORS (ROPE) — Institutional beneficiaries (powerful/arbitrage) benefit from the constraint's maintenance. Iranian nuclear capability advancement is offset by US military commitments, arms sales to regional allies, and tactical coordination mechanisms. Regional competitors can exit (normalize with Iran, allow Iranian capability) without fundamental loss of security — they have alternatives (deterrence, alliances, arms acquisitions). Experience constraint as coordination of regional stability mechanisms, though at lower risk to themselves than Iranian targets.
constraint_indexing:constraint_classification(iranian_nuclear_proliferation_pathway, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: NUCLEAR DIPLOMACY INFRASTRUCTURE (SCAFFOLD) — Organized actors (diplomatic channels, technical working groups, JCPOA institutional mechanisms) experience this as a temporary coordination problem with potential sunset. The JCPOA represented a scaffold: structured pathway to normalize Iran's nuclear program in exchange for transparency. Has_sunset_clause_rationale: the agreement stipulated that nuclear restrictions would be removed once verification requirements were met and time horizons expired. Although the US withdrew in 2018, the scaffold structure remains active in European and multilateral diplomacy efforts to reconstruct verification pathways. Sunset depends on restored trust and institutional rebuilding.
constraint_indexing:constraint_classification(iranian_nuclear_proliferation_pathway, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: COLD WAR NONPROLIFERATION FRAMEWORKS (PITON) — Institutional actor (powerful/arbitrage) maintains Cold War-era institutional frameworks (NPT, IAEA enforcement, export control regimes) through inertia despite changing structural conditions. The frameworks were designed for bipolarity and great-power coordination; their application to multipolar security environments is increasingly performative. Theater ratio reflects that enforcement mechanisms are selective (applied to Iran, North Korea, but not to Israel, India, or Pakistan for comparable capabilities). The frameworks persist through institutional path dependence, not because they effectively prevent proliferation. Theater_ratio 0.58 reflects mixed functionality: some real inspection and verification (functional), much performative diplomacy and selective enforcement (theater).
constraint_indexing:constraint_classification(iranian_nuclear_proliferation_pathway, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / PHYSICS VIEW (MOUNTAIN) — From a civilizational/universal perspective, certain aspects of the Iranian pathway appear as immutable natural law: once uranium enrichment technology is known and centrifuge designs exist, the pathway to weapons-capable fissile material is irreversible at the physics level. The cascade mathematics (cascade enrichment, critical mass), the decay constants of uranium isotopes, the electromagnetic separation principles — these are invariant across all political contexts. However, this perspective risks naturalizing the policy constraint as a physics constraint. The structural data shows the constraint is socially constructed (sanctions are political, not physical; JCPOA terms are negotiable; enrichment restrictions are human-imposed). The mountain classification captures a real feature (technical irreversibility of knowledge) while obscuring the contingency of the policy regime.
constraint_indexing:constraint_classification(iranian_nuclear_proliferation_pathway, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(iranian_nuclear_proliferation_pathway_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(iranian_nuclear_proliferation_pathway, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(iranian_nuclear_proliferation_pathway, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(iranian_nuclear_proliferation_pathway, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(iranian_nuclear_proliferation_pathway, TR),
    TR >= 0.70.

:- end_tests(iranian_nuclear_proliferation_pathway_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. The constraint extracts primarily through sanctions, which impose severe asymmetric costs on Iran's civilian economy while providing minimal direct extraction to the US/coalition (extraction is indirect, through geopolitical advantage and deterrence). The extractiveness metric reflects the ratio of imposed costs to realized benefits for the constraint-holder. Rising from 0.35 to 0.68 over the interval correlates with escalation: JCPOA withdrawal (2018) triggered maximum pressure sanctions, intensifying economic extraction. The metric captures not whether extraction is 'fair' but whether it is severe and asymmetric — which it demonstrably is. Suppression (0.75): Very high. Multiple reinforcing suppression mechanisms: capital controls (banking isolation), military encirclement (US fleet presence, allied bases), diplomatic exclusion (UN Security Council restrictions), technology denial (export controls on dual-use equipment), and ideological framing (sanctions presented as universal law rather than political choice). Barriers to exit are structural and multiple; no single escape route exists. Theater ratio (0.58): Moderate-high. Reflects mixed functionality and performativity. Real functions: IAEA inspections do detect enrichment progress; export controls do slow technology acquisition; sanctions do reduce available capital. Performative functions: selective enforcement (Israel/India/Pakistan have comparable or greater capabilities with no comparable pressure); diplomatic theater (negotiations with Iran while continuing arms sales to regional competitors); framing of selective enforcement as universal nonproliferation principle. Rising from 0.42 to 0.58 reflects increasing visibility of selective enforcement and declining credibility of universal framing, particularly after JCPOA collapse.
 *
 * PERSPECTIVAL GAP:
 *   The constraint is experienced as qualitatively different phenomena across structural positions. For US-led coalition, the constraint is coordination (rope) — aligning sanctions policy, intelligence sharing, technology restrictions, military positioning to prevent Iranian nuclear breakout. For Iranian state apparatus, it is identity-constituting resistance (snare with identity_lock) — the constraint's existence reinforces regime legitimacy narratives. For civilians, it is pure extraction (snare) — economic costs imposed with no offsetting coordination benefit. For nonpreliferation regime, it is a corrupted mechanism (tangled_rope or piton) — nominally coordinates nonproliferation but executed selectively to advantage some states. For diplomacy infrastructure, it could be scaffold — JCPOA demonstrated that structured agreements could provide exit pathway with sunset clauses. The piton perspective (Cold War frameworks) captures real institutional inertia — NPT/IAEA structures designed for bipolarity persist in multipolar security environment with increasing performative content. The mountain perspective (physics inevitability) risks naturalizing policy choices as law of nature — once enrichment technology is known, further proliferation is 'inevitable,' which obscures the contingency of the sanctions regime.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) measures the agent's structural relationship to extraction flow for THIS constraint. Iranian civilian population: d ≈ 0.95 (trapped, powerless, bearing full extraction costs) → f(d) ≈ 1.40 (very high experienced extractiveness). Iranian state: d ≈ 0.85 (identity_locked, can theoretically exit but binding is cognitive) → f(d) ≈ 1.15 (high experienced extraction with reduced severity due to cognitive rather than purely material binding). US coalition: d ≈ 0.10 (institutional beneficiary with arbitrage exit, high agency) → f(d) ≈ -0.01 (negative effective extraction — extraction flows toward them). IAEA/nonproliferation regime: d ≈ 0.60 (organized, constrained, mixed position) → f(d) ≈ 0.80 (moderate extraction as they are partly extracted from by great-power selectivity while maintaining some coordination function). The scope modifier σ(S) = 1.0 (global constraint, no scope dampening or amplification beyond base). The principal beneficiary (US/coalition) has low d due to institutional power and arbitrage capacity; the principal victim (civilians) has high d due to power asymmetry and trapped status.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves potential mandatrophy through explicit perspectival differentiation. The core tension is whether the constraint is a justified response to proliferation risk (mountain/rope/scaffold framings) or an extractive hegemonic mechanism (snare/piton framings). Both are structurally correct from their respective positions. The mandatrophy resolves by showing that this is not ambiguity about 'which is true?' but rather which agent's structural position we are analyzing. The constraint IS coordination (beneficiary perspective) AND extraction (victim perspective) simultaneously. The resolution requires accepting that a single constraint can have different classification types depending on observational position, and that this is not a logical contradiction but rather a feature of the indexical system. The JCPOA collapse demonstrates the falsity of mountain framing: the constraint was not inevitable law but a contingent institutional choice that was unmade. The persistent theater ratio demonstrates the piton element: selective enforcement reveals that the nonproliferation framing is partially theatrical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentions_versus_capabilities,
    'Is the Iranian nuclear program driven by genuine deterrence intentions (defensive posture against US/Israel military threats) or by proliferation intentions (strategic weapons acquisition for regional hegemony)?',
    'Comparative analysis of Iranian strategic statements across regime changes; tracking of declared vs actual enrichment levels; assessment of weaponization-specific activities (neutron initiator development, warhead design work, missile integration). Intelligence declassification. Post-regime-change policy analysis if political conditions change.',
    'If primarily deterrence: constraint is defensive security-seeking (snare becomes partially justified, but sanctions become more extractive). If primarily proliferation: constraint is offensive capability-building (snare is appropriate, sanctions gain legitimacy). Classification remains snare either way, but moral/strategic justification changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentions_versus_capabilities, empirical, 'Whether Iranian nuclear program is driven by deterrence or proliferation intent').

omega_variable(
    sanctions_regime_effectiveness,
    'Do economic sanctions actually constrain Iranian nuclear progress, or do they accelerate it by making externalization of nuclear ambition a matter of national pride and regime legitimacy?',
    'Correlation analysis between sanctions intensity and nuclear advancement rate; comparative analysis of Iranian nuclear progress under different sanction regimes (pre-2006, 2006-2015 UNSC sanctions, post-2015 JCPOA, post-2018 maximum pressure); economic modeling of actual vs estimated cost impact on nuclear program operations.',
    'If sanctions constrain: snare classification is justified — extraction mechanism slows proliferation. If sanctions accelerate: snare classification remains but the mechanism is inverted — the constraint produces the outcome it claims to prevent. Omega reveals whether suppression strategy is counterproductive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sanctions_regime_effectiveness, empirical, 'Whether sanctions constrain or accelerate Iranian nuclear advancement').

omega_variable(
    identity_lock_reversibility,
    'Can the Islamic Republic''s identity-lock on nuclear resistance be reversed through leadership change, diplomatic normalization, or reframing of national security interests, or is the nuclear program now constitutive of regime legitimacy irreversibly?',
    'Historical analysis of Iranian regime evolution and shifting ideological emphases; elite opinion surveys on nuclear abandonment scenarios; comparative cases of states exiting nuclear programs after identity-lock (South Africa, Libya, South Korea post-1990s). Monitoring of generational shifts in Iranian political consciousness.',
    'If reversible: identity_locked classification is appropriate but points to a potential exit pathway through regime change or cognitive reframing. If irreversible: identity_locked characterizes a structural lock-in that can only be broken by political transformation. Changes policy implications for exit strategies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Whether regime''s nuclear identity-lock is reversible').

omega_variable(
    great_power_commitment_credibility,
    'Do credible security guarantees from great powers (e.g., NATO membership, explicit security treaty with the United States) actually reduce Iranian incentive to pursue nuclear weapons, or is the regional threat environment sufficiently uncertain that such guarantees cannot substitute for indigenous deterrent?',
    'Analysis of comparable cases: Ukraine''s reliance on Budapest Memorandum security guarantees vs South Korea''s dual deterrent (US commitment + indigenous capabilities). Modeling of Iranian threat perceptions vis-a-vis Israel, Saudi Arabia, and historical US-Iran hostility. Game-theoretic analysis of commitment credibility under regime change.',
    'If guarantees sufficient: scaffold diplomacy has genuine exit pathway — verified security agreements could reduce extraction from powerless agents. If guarantees insufficient: snare persists because Iranian strategic rationality demands independent deterrent regardless of foreign commitments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(great_power_commitment_credibility, conceptual, 'Whether security guarantees can substitute for indigenous nuclear deterrent').

omega_variable(
    selective_enforcement_legitimacy,
    'Does the NPT/IAEA regime''s selective enforcement (strict inspection of Iran, loose inspection of Israel/India/Pakistan) undermine the piton classification, suggesting instead that nonproliferation is purely theatrical and the constraint is actually a snare of US hegemonic power disguised as universal law?',
    'Comparative analysis of inspection frequency, access restrictions, and enforcement consequences across NPT states. Legal/historical analysis of IAEA mandate selectivity and great-power veto patterns. Assessment of whether double standards are structural (inherent to the NPT framework) or contingent (could be reformed).',
    'If purely theatrical: piton classification stands — selective enforcement maintains theater of nonproliferation while preserving great-power options. If double standards are reformable: nonproliferation regime has latent coordination function and could transition from piton to rope through institutional reform. Classification implications differ significantly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(selective_enforcement_legitimacy, empirical, 'Whether selective nonproliferation enforcement undermines regime legitimacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(iranian_nuclear_proliferation_pathway, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irnp_tr_t0, iranian_nuclear_proliferation_pathway, theater_ratio, 0, 0.42).
narrative_ontology:measurement(irnp_tr_t5, iranian_nuclear_proliferation_pathway, theater_ratio, 5, 0.51).
narrative_ontology:measurement(irnp_tr_t10, iranian_nuclear_proliferation_pathway, theater_ratio, 10, 0.58).
narrative_ontology:measurement(irnp_tr_t3, iranian_nuclear_proliferation_pathway, theater_ratio, 3, 0.47).

% Extraction over time
narrative_ontology:measurement(irnp_be_t0, iranian_nuclear_proliferation_pathway, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(irnp_be_t5, iranian_nuclear_proliferation_pathway, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(irnp_be_t10, iranian_nuclear_proliferation_pathway, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(irnp_be_t3, iranian_nuclear_proliferation_pathway, base_extractiveness, 3, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(iranian_nuclear_proliferation_pathway, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(iranian_nuclear_proliferation_pathway, 0.18).
narrative_ontology:affects_constraint(iranian_nuclear_proliferation_pathway, us_iran_hegemonic_rivalry).
narrative_ontology:affects_constraint(iranian_nuclear_proliferation_pathway, regional_security_balance_middle_east).
narrative_ontology:affects_constraint(iranian_nuclear_proliferation_pathway, nonproliferation_regime_selective_enforcement).

% DUAL FORMULATION NOTE:
% The Iranian pathway constraint is downstream of hegemonic US-Iran rivalry and upstream of regional security balance. The nonproliferation regime serves as both coordinating framework and enforcement mechanism; its selective enforcement patterns constitute a separate constraint story (nonproliferation_regime_selective_enforcement) with ε≈0.55 (Tangled Rope) reflecting the corruption of coordination by great-power preferences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(iranian_nuclear_proliferation_pathway, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
