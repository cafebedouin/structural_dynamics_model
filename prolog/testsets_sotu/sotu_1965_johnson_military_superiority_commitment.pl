% ============================================================================
% CONSTRAINT STORY: sotu_1965_johnson_military_superiority_commitment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1965_johnson_military_superiority_commitment, []).

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
 *   constraint_id: sotu_1965_johnson_military_superiority_commitment
 *   human_readable: Perpetual Military Superiority Commitment (Johnson 1965 SOTU)
 *   domain: military/geopolitical/fiscal_policy
 *
 * SUMMARY:
 *   President Lyndon B. Johnson's 1965 State of the Union address commits the
 *   Executive Branch to maintaining 'military strength second to none' — a
 *   rhetorical pledge that binds successive administrations to continuous
 *   defense investment and technological advancement to sustain overwhelming
 *   strategic force dominance. This commitment operates at the intersection
 *   of deterrence theory (rational choice to prevent war through cost
 *   asymmetry), institutional incentives (defense contractors, military
 *   hierarchy, allied states), and fiscal policy (crowding out domestic
 *   spending). The constraint exhibits high extractiveness (0.58) because it
 *   concentrates benefits (defense contractors, military personnel, allied
 *   security umbrella) while distributing costs (fiscal opportunity costs to
 *   social spending, reduced strategic flexibility, arms race instability for
 *   adversaries). The constraint is a Tangled Rope at the baseline: it
 *   genuinely coordinates deterrence (rope element) while simultaneously
 *   extracting from domestic priorities and locking future administrations
 *   into spending paths (tangled extraction element). Over 60 years,
 *   extractiveness has risen from 0.35 (1965) to 0.62 (2010) as technological
 *   sophistication and threat redefinition have increased the cost baseline,
 *   while theater ratio has stayed relatively stable (0.32 to 0.48),
 *   suggesting the performative content of deterrence maintenance has grown
 *   but not explosively. The constraint is mandatrophy-unresolved: it
 *   presents as justified coordination (deterrence prevents war) but operates
 *   as extraction (fiscal lock-in, arms race participation).
 *
 * KEY AGENTS:
 *   - Lyndon B. Johnson / Executive Branch: Institutional beneficiary (institutional/arbitrary) — establishes commitment that expands executive autonomy in foreign policy while constraining fiscal options for successors
 *   - Defense Contractors: Institutional beneficiary (institutional/arbitrage) — continuous procurement funding guaranteed by superiority commitment; massive extraction benefit
 *   - Military Personnel / Joint Chiefs: Institutional beneficiary (institutional/arbitrage) — career advancement, budget growth, institutional prestige tied to commitment; arbitrage capacity through doctrine development
 *   - Allied Nations (NATO, Japan, South Korea): Institutional beneficiary (institutional/arbitrage) — security externalized; defense burdens reduced; moderate arbitrage through alliance leverage
 *   - Domestic Social Spending Constituencies: Victim (powerless/trapped) — fiscal priority hierarchy subordinates education, health, infrastructure to military floor; no exit from crowding-out mechanism
 *   - Adversarial Powers (USSR, China, peer competitors): Victim (powerless/trapped) — forced into matching investment spiral; no credible stand-down option under deterrence logic; escalation trap
 *   - Congressional Peace Advocates / Democratic Base: Constrained victim (organized/constrained) — can mobilize politically but faces high costs (national security framing, electoral penalties); some exit capacity but expensive
 *   - Global Arms Race Participants: Victim (powerless/trapped) — proliferation accelerates as rising powers respond to U.S. commitment; no coordination mechanism to arrest escalation
 *   - Analytical Observer: Neutral (analytical/analytical) — risks naturalizing contingent institutional arrangement as inherent to statecraft
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1965_johnson_military_superiority_commitment, 0.58).
domain_priors:suppression_score(sotu_1965_johnson_military_superiority_commitment, 0.65).
domain_priors:theater_ratio(sotu_1965_johnson_military_superiority_commitment, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1965_johnson_military_superiority_commitment, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_1965_johnson_military_superiority_commitment, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sotu_1965_johnson_military_superiority_commitment, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1965_johnson_military_superiority_commitment, tangled_rope).
narrative_ontology:human_readable(sotu_1965_johnson_military_superiority_commitment, "Perpetual Military Superiority Commitment (Johnson 1965 SOTU)").
narrative_ontology:topic_domain(sotu_1965_johnson_military_superiority_commitment, "military/geopolitical/fiscal_policy").

domain_priors:requires_active_enforcement(sotu_1965_johnson_military_superiority_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1965_johnson_military_superiority_commitment, defense_contractors).
narrative_ontology:constraint_beneficiary(sotu_1965_johnson_military_superiority_commitment, military_personnel).
narrative_ontology:constraint_beneficiary(sotu_1965_johnson_military_superiority_commitment, allied_nations_security_umbrella).
narrative_ontology:constraint_beneficiary(sotu_1965_johnson_military_superiority_commitment, executive_branch_autonomy).
narrative_ontology:constraint_victim(sotu_1965_johnson_military_superiority_commitment, domestic_social_spending).
narrative_ontology:constraint_victim(sotu_1965_johnson_military_superiority_commitment, fiscal_flexibility).
narrative_ontology:constraint_victim(sotu_1965_johnson_military_superiority_commitment, strategic_restraint_capacity).
narrative_ontology:constraint_victim(sotu_1965_johnson_military_superiority_commitment, global_arms_race_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ADVERSARIAL NATIONS (SNARE) — Trapped in escalatory competition. U.S. commitment to permanent superiority forces matching investment to avoid strategic inferiority, but no symmetrical commitment to mutual restraint. Exit options vanish under the logic of deterrence — standing down appears as vulnerability. Suppression is structural: military investment becomes non-negotiable, resources diverted from civilian infrastructure. Extraction benefits the U.S. security posture at the cost of others' fiscal autonomy and strategic flexibility.
constraint_indexing:constraint_classification(sotu_1965_johnson_military_superiority_commitment, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DOMESTIC SOCIAL WELFARE (SNARE) — Trapped by fiscal priority hierarchy. Commitment to permanent military superiority establishes a non-negotiable budget floor. Educational, health, infrastructure, and poverty reduction spending compete downward against this floor. Exit requires fundamentally challenging the commitment — a political barrier that has proven insurmountable across partisan cycles. Suppression operates through nationalism (questioning superiority is framed as weakness) and security theater (fear narratives justify continuous extraction).
constraint_indexing:constraint_classification(sotu_1965_johnson_military_superiority_commitment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: CONGRESSIONAL DEMOCRATIC BASE (TANGLED ROPE) — Constrained but not trapped. Peace advocates and social spending advocates benefit from the same institutional framework that enables military spending (stable governance, rule of law, institutional continuity) but suffer extraction through fiscal opportunity costs. Coalition ability exists — organizing can shift budget priorities — but faces high political costs (framing as weak on defense, losing centrist voters, security establishment opposition). Mixed experience: genuine coordination benefit + real extraction cost.
constraint_indexing:constraint_classification(sotu_1965_johnson_military_superiority_commitment, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DEFENSE CONTRACTORS (ROPE) — Primary beneficiary. Commitment to permanent superiority guarantees continuous procurement and R&D funding. This agent has arbitrage options: contractors can lobby for budget increases, can exit underperforming contracts to competitors, can invest capital in adjacent commercial markets. The constraint is experienced as pure coordination: the commitment aligns contractor interests with government objectives. Suppression is minimal for this agent — the system rewards rather than constrains. Chi is negative or near-zero (beneficiary with mobility).
constraint_indexing:constraint_classification(sotu_1965_johnson_military_superiority_commitment, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ALLIED NATIONS UNDER SECURITY UMBRELLA (ROPE) — Benefit from U.S. commitment to permanent superiority. Defense burdens are reduced; deterrence is externalized to the U.S. military budget. These nations have arbitrage options: they can invest their own defense spending more lightly, reinvest savings domestically, or shift alliances if the arrangement becomes disadvantageous. The constraint is experienced as pure coordination: it solves their security problem. Exit costs are real but moderate (strategic isolation, reduced leverage in negotiations). Chi is low or moderate negative (beneficiary with meaningful exit capacity).
constraint_indexing:constraint_classification(sotu_1965_johnson_military_superiority_commitment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: EXECUTIVE BRANCH (TANGLED ROPE) — Ambiguous structural position. The commitment to superiority EXPANDS executive autonomy in foreign policy: Presidents can intervene globally knowing military capacity is guaranteed. But it also CONSTRAINS: no President can credibly reduce military budgets without risking the 'superiority' commitment, and this locks future administrations into continuous high-level spending. Benefits (expanded autonomy) are immediate; costs (reduced fiscal flexibility, path dependency) are generational. Constrained exit: individual Presidents can modulate implementation but cannot rescind the commitment without appearing to break with strategic doctrine.
constraint_indexing:constraint_classification(sotu_1965_johnson_military_superiority_commitment, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: DETERRENCE FRAMEWORK (PITON) — The rational strategic logic for permanent superiority is that it deters war through overwhelming cost asymmetry: no rational adversary attacks when defeat is certain. This logic once drove genuine strategic choice. But over 60+ years, the maintenance of this framework has become largely performative. Strategic stability is now undermined by the commitment's own logic: adversaries cannot credibly stand down (escalation trap), arms races accelerate (Stability-Instability Paradox), and the technological frontier becomes the actual constraint rather than superiority margin (hypersonic weapons, AI systems, cyber effects). The institutional theater persists through inertia — 'maintaining superiority' is recalibrated endlessly to incorporate new threat categories, but the core function (making war unthinkable) has been replaced by an extraction mechanism (continuous military investment justifying itself).
constraint_indexing:constraint_classification(sotu_1965_johnson_military_superiority_commitment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: MULTIPOLAR RESTRAINT MOVEMENT (SCAFFOLD) — Emerging perspective from organized international actors (rising powers, environmental movements, global civil society) that frame the commitment as a temporary historical moment. As U.S. relative economic power declines, the sustainability of permanent superiority becomes questioned. The constraint has a sunset: either it relaxes through negotiated arms control, or it breaks through fiscal exhaustion or peer competitor technological breakthrough. Organized actors pushing for mutual arms limitation treaties, cybersecurity norms, space weaponry restrictions, and AI governance see the superiority commitment as a coordinate to be renegotiated, not a permanent law. Sunset horizon: 15-30 years as multipolarity solidifies. Exit costs are real (requires U.S. acceptance of peer competition) but increasingly unavoidable.
constraint_indexing:constraint_classification(sotu_1965_johnson_military_superiority_commitment, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a long-term strategic perspective, some commitment to military capacity is inherent to statecraft: nations must maintain defense capability against potential threats. This perspective naturalizes the superiority logic as inevitable, drawing on neorealist international relations theory (anarchy requires self-help, balance of power is natural). However, the structural data contradicts the mountain classification: the commitment benefits identifiable agents (defense contractors, military hierarchy, allied nations), requires active enforcement through rhetorical commitment and procurement manipulation, and has concentrable victims (social spending, fiscal autonomy, arms race participants). The engine's false summit detector will identify this as naturalization of a contingent institutional arrangement.
constraint_indexing:constraint_classification(sotu_1965_johnson_military_superiority_commitment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1965_johnson_military_superiority_commitment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1965_johnson_military_superiority_commitment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1965_johnson_military_superiority_commitment, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1965_johnson_military_superiority_commitment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1965_johnson_military_superiority_commitment, TR),
    TR >= 0.70.

:- end_tests(sotu_1965_johnson_military_superiority_commitment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising over time. Base value reflects that the superiority commitment concentrates clear benefits (defense industry gains ~3-5% of federal budget annually, allies gain security at reduced cost, executive gains policy autonomy) while distributing costs across dispersed constituencies (U.S. domestic spending foregone, global adversaries forced into arms races, future administrations locked into spending paths). The rising trend (0.35 → 0.62) reflects two mechanisms: (1) technological complexity increasing the cost baseline for each new generation of superiority, and (2) threat redefinition expanding what 'superiority' must encompass (cyber, space, AI, electromagnetic in addition to legacy naval/air/ground). This is classic extraction treadmill dynamics. Suppression (0.65): Moderate-high. Suppression mechanisms include: national security framing that makes questioning superiority politically costly ('weakness' narrative), institutional capture of threat assessment (military establishment defines threat scope), information asymmetry (classified threat briefings limit public debate), alliance structures that lock allied nations into support, and path dependency (breaking commitment requires coordinating on new doctrine across Pentagon, Congress, allied partners). Suppression is not total — domestic debate exists, peace movements mobilize — but the political cost of exit is high. Theater ratio (0.48): Moderate. The commitment maintains performative content (public demonstrations of force, capability announcements, deterrence messaging) but this is not the dominant mechanism. The real mechanism is fiscal allocation: the commitment guarantees budget floors and procurement pipelines regardless of strategic rationale. Theater has increased slightly over 60 years (0.32 → 0.48) as more of the commitment's justification has shifted to rhetorical reassurance (ally confidence, domestic morale) rather than military necessity. Claimed type (Tangled Rope): The commitment satisfies tangled rope gates: (1) genuine coordination function (prevents peer war through cost asymmetry), (2) asymmetric extraction (benefits concentrate on defense sector, costs on social spending), (3) active enforcement (rhetorical renewal required each administration, procurement decisions enforcing the commitment). This is not a pure rope (it extracts) and not a pure snare (it prevents catastrophic outcomes).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme, reflecting a constraint where some agents experience pure coordination and others experience pure extraction from the same structural commitment. Defense contractors see a rope (their problem of demand uncertainty is solved; they experience only benefit). Domestic social spending advocates see a snare (their fiscal options are trapped; costs are non-negotiable). The executive sees tangled rope (autonomy gain + fiscal constraint). Allies see rope (security problem solved). Adversaries see snare (arms race spiral with no exit). The piton perspective (deterrence framework as degraded ritual) captures how the rational-strategic logic has been replaced by institutional maintenance. The scaffold perspective (multipolar restraint as emerging sunset) captures how the constraint may be becoming unsustainable. The mountain perspective (natural law of statecraft) reveals the false summit: naturalizing contingent institutional arrangements as inevitable strategic requirements. This perspectival diversity indicates a constraint at peak instability — the coordination function is real but increasingly undermined by the extraction treadmill.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies radically by agent perspective. Defense contractors and military hierarchy: d ≈ 0.05-0.15 (near-full beneficiaries with arbitrage capacity); effective extraction chi is negative or near-zero — they experience pure coordination benefit. Allied nations: d ≈ 0.10-0.20 (beneficiaries with exit costs); chi is low — they experience coordination with modest opportunity cost. Executive branch: d ≈ 0.45-0.55 (mixed — gains autonomy in foreign policy, loses fiscal flexibility); chi is moderate — constrained benefit. Domestic social spending victims: d ≈ 0.75-0.90 (high targets with limited exit capacity); chi is high — concentrated extraction. Adversarial powers: d ≈ 0.85-0.95 (full targets with no credible exit); chi is very high — maximum experienced extraction. The derived d values from beneficiary/victim declarations and exit options produce this perspectival gap naturally. No overrides needed — the structural data captures real positional differences.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY: The constraint claims to coordinate deterrence (prevent major-power war) while operating as extraction (fiscal lock-in, arms race participation, ally dependence, technology treadmill). The mandatrophy is resolvable at two levels: (1) EMPIRICAL: Does the superiority commitment actually deter war, or does it provoke asymmetric responses (cyber, terrorism, proliferation) that increase conflict risk? Historical data on conflict initiation before/after 1965 SOTU, comparative stability before/after, and adversary strategic document analysis can resolve whether deterrence works. (2) FISCAL: Is the commitment sustainable across the 60-year horizon? If fiscal burden becomes unsustainable within 30 years, the constraint breaks and mandatrophy resolves by failure. Current trajectory suggests unsustainability risk at 25-35 year horizon (relative GDP-to-defense ratios vs. China, debt servicing costs). RECOMMENDATION: Treat this as a tangled rope with rising extraction treadmill dynamics. The coordination function (deterrence) is real but declining in salience; the extraction function (fiscal lock-in, technology procurement mandates) is rising. Recommend measurement-driven resolution: if deterrence failures increase, reclassify to snare. If fiscal pressure forces spending cuts, reclassify to scaffold with sunset logic. Current classification is analytically sound but unstable — this constraint is transitional between types.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    superiority_definition_erosion,
    'What constitutes ''military superiority'' as technological domains expand (cyber, space, AI, electromagnetic)? Can superiority in legacy domains (naval, air, ground) substitute for parity in emerging domains?',
    'Historical analysis of how ''superiority'' requirements have evolved; military doctrine reviews; strategic assessment reports documenting sufficiency vs. superiority debates',
    'If superiority definition is unstable: the commitment becomes a treadmill (always requiring new capabilities). If substitutable: older capabilities can be reduced. The extraction mechanism depends on perpetual redefinition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(superiority_definition_erosion, empirical, 'Erosion and redefinition of military superiority metric').

omega_variable(
    deterrence_stability_paradox,
    'Does the commitment to permanent superiority actually enhance or undermine strategic stability? Are adversaries deterred or provoked into asymmetric strategies (cyber, terrorism, proliferation)?',
    'Comparative security analysis: conflict initiation rates before/after superiority commitment; adversary strategic document analysis; arms race dynamics modeling; empirical measurement of deterrence success vs. proliferation rates',
    'If deterrence works: the constraint is genuine coordination preventing worse outcomes (rope/scaffold logic). If paradoxical: the commitment generates instability it claims to prevent (snare logic for adversaries, extractive treadmill logic). Classification chain would shift dramatically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_stability_paradox, empirical, 'Whether superiority commitment enhances or undermines strategic stability').

omega_variable(
    fiscal_sustainability_horizon,
    'At what point does the fiscal burden of permanent superiority become economically unsustainable relative to peer competitors? Is the commitment self-refuting (bankrupting the state pursuing it)?',
    'Long-term fiscal modeling; comparative GDP-to-defense ratios (U.S. vs. China, Russia, peer coalitions); debt sustainability analysis; historical precedent (Soviet military spending trajectory)',
    'If sustainable indefinitely: snare logic for adversaries continues. If unsustainable < 30 years: scaffold sunset logic applies. If unsustainable < 10 years: constraint breaks catastrophically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_sustainability_horizon, empirical, 'Fiscal sustainability of permanent superiority commitment').

omega_variable(
    identity_fusion_lock,
    'Has U.S. national identity become fused with military dominance such that questioning the superiority commitment is psychologically interpreted as existential threat? How much of the commitment''s persistence is cognitive capture vs. material incentives?',
    'Survey data on American attitudes toward military spending and strategic role; discourse analysis of political rhetoric; comparative identity framing in allied vs. peer nations; longitudinal tracking of support for alternatives (collective defense, arms control, multipolarity)',
    'If identity-locked: even rational actors cannot exit (constraint operates via internalized suppression). If material incentives dominate: restructured incentives could enable exit (reframing possible). Classification of executive branch perspective shifts from constrained to identity_locked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_lock, conceptual, 'Identity fusion with military dominance narrative').

omega_variable(
    technological_substitution_frontier,
    'Can emerging technologies (hypersonic missiles, AI-enabled systems, quantum sensing, autonomous swarms) deliver superiority at lower cost than legacy systems? Does this reduce or perpetuate the extraction mechanism?',
    'Defense budget trend analysis; R&D allocation tracking; cost-per-unit analysis of emerging vs. legacy capabilities; strategic doctrine shifts; arms control treaty negotiations around new domains',
    'If substitution is cost-reducing: superiority could be maintained with lower fiscal extraction (scaffold dynamics toward restraint). If cost-increasing: treadmill accelerates (snare dynamics intensify). Determines mandatrophy trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_substitution_frontier, empirical, 'Technological cost substitution in military superiority maintenance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1965_johnson_military_superiority_commitment, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu_mil_theater_1965, sotu_1965_johnson_military_superiority_commitment, theater_ratio, 0, 0.32).
narrative_ontology:measurement(sotu_mil_theater_1980, sotu_1965_johnson_military_superiority_commitment, theater_ratio, 15, 0.4).
narrative_ontology:measurement(sotu_mil_theater_1995, sotu_1965_johnson_military_superiority_commitment, theater_ratio, 30, 0.46).
narrative_ontology:measurement(sotu_mil_theater_2010, sotu_1965_johnson_military_superiority_commitment, theater_ratio, 45, 0.48).

% Extraction over time
narrative_ontology:measurement(sotu_mil_extract_1965, sotu_1965_johnson_military_superiority_commitment, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sotu_mil_extract_1980, sotu_1965_johnson_military_superiority_commitment, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(sotu_mil_extract_1995, sotu_1965_johnson_military_superiority_commitment, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(sotu_mil_extract_2010, sotu_1965_johnson_military_superiority_commitment, base_extractiveness, 45, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1965_johnson_military_superiority_commitment, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1965_johnson_military_superiority_commitment, cold_war_deterrence_doctrine).
narrative_ontology:affects_constraint(sotu_1965_johnson_military_superiority_commitment, allied_nuclear_dependency).
narrative_ontology:affects_constraint(sotu_1965_johnson_military_superiority_commitment, defense_contractor_procurement_lock).
narrative_ontology:affects_constraint(sotu_1965_johnson_military_superiority_commitment, global_arms_race_spirals).

% DUAL FORMULATION NOTE:
% The superiority commitment decomposes into four downstream constraints: (1) deterrence doctrine (ε ≈ 0.25, mountain-like, strategic rationale); (2) allied dependency (ε ≈ 0.45, tangled rope, security + subordination); (3) contractor procurement lock (ε ≈ 0.72, snare for fiscal autonomy); (4) global arms race participation (ε ≈ 0.65, snare for adversaries). The upstream constraint (superiority commitment) has stable ε ≈ 0.58 and functions as tangled rope. As determinants (1) weakens (deterrence logic questioned), the downstream constraints become harder to justify.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1965_johnson_military_superiority_commitment, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
