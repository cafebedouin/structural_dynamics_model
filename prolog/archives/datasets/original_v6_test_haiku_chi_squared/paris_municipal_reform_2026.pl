% ============================================================================
% CONSTRAINT STORY: paris_municipal_reform_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_municipal_reform_2026, []).

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
 *   constraint_id: paris_municipal_reform_2026
 *   human_readable: Paris Municipal Reform 2026 (Loi Maillard/PLM Reform)
 *   domain: political/legal/municipal_governance
 *
 * SUMMARY:
 *   The 2026 Paris Municipal Reform (Loi du 11 août 2025) reduces the
 *   Majority Premium (Prime Majoritaire) for the Council of Paris from 50% to
 *   25% of seats. This reform exemplifies the structural tension between
 *   electoral legitimacy (proportional representation) and executive
 *   stability (concentrated power). The constraint exhibits a tangled_rope
 *   structure: it simultaneously enables minority representation
 *   (coordination function) and suppresses full proportionality (extraction
 *   function). The reform emerged as a compromise between the incumbent
 *   center-right coalition's interest in maintaining governing stability and
 *   growing pressure from left-green opposition and electoral reform
 *   advocates for proportionality. The 50%→25% reduction appears as a
 *   negotiated settlement, but it masks underlying uncertainties: Is this the
 *   final equilibrium or a stepping stone toward full PR? Will administrative
 *   inertia absorb the change or produce real governance shifts? Do small
 *   parties below the 12% threshold gain meaningful voice or remain
 *   structurally trapped?
 *
 * KEY AGENTS:
 *   - Incumbent Center-Right Coalition (LR/Renaissance/Modem): Primary beneficiary (institutional/arbitrage) — maintains supermajority capacity with modest legitimacy gain from 'proportionality reform'
 *   - Left-Green Opposition (Socialist/Écologiste): Secondary beneficiary (organized/constrained) — gains better proportional representation and coalition leverage, though still constrained by Paris electoral base
 *   - Small Parties (< 12% vote share): Primary victim (powerless/trapped) — structural disadvantage remains despite bonus reduction; cannot exit Paris politics
 *   - Electoral Proportionality Principle: Systemic victim (powerless/trapped) — abstract collective good suppressed by majority premium structure
 *   - Paris Municipal Administration: Institutional actor (institutional/arbitrage) — maintains continuity through administrative networks regardless of coalition composition
 *   - Electoral Reform Movement: Organized advocate (organized/constrained) — sees reform as scaffolding toward full PR; committed to generational trajectory of bonus reduction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_municipal_reform_2026, 0.52).
domain_priors:suppression_score(paris_municipal_reform_2026, 0.58).
domain_priors:theater_ratio(paris_municipal_reform_2026, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_municipal_reform_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(paris_municipal_reform_2026, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(paris_municipal_reform_2026, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_municipal_reform_2026, tangled_rope).
narrative_ontology:human_readable(paris_municipal_reform_2026, "Paris Municipal Reform 2026 (Loi Maillard/PLM Reform)").
narrative_ontology:topic_domain(paris_municipal_reform_2026, "political/legal/municipal_governance").

domain_priors:requires_active_enforcement(paris_municipal_reform_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_municipal_reform_2026, incumbent_center_right_coalition).
narrative_ontology:constraint_beneficiary(paris_municipal_reform_2026, fragmented_opposition_blocs).
narrative_ontology:constraint_victim(paris_municipal_reform_2026, electoral_proportionality).
narrative_ontology:constraint_victim(paris_municipal_reform_2026, minority_representation_principle).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL MINORITY PARTY (SNARE) — Parties below the 12% threshold for bonus allocation have no exit. The 50%→25% reduction was meant to help them, but the majority premium still suppresses proportional representation. They cannot exit Paris politics; they are trapped in a system that systematically underweights their votes. d≈0.92, f(d)≈1.38, σ=0.8 → χ≈0.60. High extraction: structural disadvantage with no alternative.
constraint_indexing:constraint_classification(paris_municipal_reform_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: LEFT-GREEN COALITION (TANGLED ROPE) — Organized actors with significant vote share (20-30% estimated) benefit from the reduced majority premium relative to pre-2026 rules, gaining negotiating power in coalition scenarios. Constrained because they depend on Paris electoral outcomes and cannot easily relocate political base. The reform simultaneously extracts from them (smaller bonus than before) and enables them (better proportional representation). d≈0.58, f(d)≈0.74, σ=0.8 → χ≈0.31. Mixed extraction; genuine coordination improvement through proportionality.
constraint_indexing:constraint_classification(paris_municipal_reform_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CENTER-RIGHT INCUMBENT (ROPE) — Benefits from the majority premium structure even at 25%, maintaining supermajority capacity with ~35-40% of votes. Has arbitrage: could exit Paris politics entirely or shift national focus. The reform is largely cosmetic for them — redistribution within the bonus zone. d≈0.15, f(d)≈0.02, σ=0.8 → χ≈-0.01. Minimal extraction; sees the reform as coordination (democratic legitimacy through modest proportionality gain).
constraint_indexing:constraint_classification(paris_municipal_reform_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ELECTORAL REFORM MOVEMENT (SCAFFOLD) — Organized advocates for proportional representation see the 2026 reform as a stepping stone toward full PR. The reduction from 50% to 25% is a sunset step: each successive reform erodes the majority premium further until true proportionality emerges. Constrained by political feasibility but seeing a trajectory. d≈0.42, f(d)≈0.48, σ=0.8 → χ≈0.20. Moderate extraction from legacy system, but the sunset clause is embedded in the coalition platform (committed to further PR reforms).
constraint_indexing:constraint_classification(paris_municipal_reform_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PARIS MUNICIPAL ADMINISTRATION (PITON) — The administrative apparatus (maire's office, councilors' staff, committee structure) largely persists regardless of coalition composition. The reform requires rhetorical adaptation (proportionality theater) but minimal functional reorganization. Theater ratio ≈0.64: the reform appears to democratize but administrative continuity absorbs whatever changes the coalition composition might imply. d≈0.05, f(d)≈-0.12, σ=0.8 → χ≈-0.02. Institutional inertia masks functional change.
constraint_indexing:constraint_classification(paris_municipal_reform_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/comparative perspective, the majority premium is both a coordination mechanism (ensures stable executive leadership) and an extraction mechanism (suppresses proportionality). The reduction from 50% to 25% suggests a hybrid system: partial coordination (bonus still exists), partial extraction (suppression remains). The analytical view depends on the jurisdiction's constitutional intent. Does the system prioritize stable governance (coordination frame) or representative legitimacy (extraction frame)? Both are defensible; the reform attempts to balance them. d≈0.65, f(d)≈1.02, σ=0.8 → χ≈0.43. Genuine ambiguity resolved by constitutional design, not by logic alone.
constraint_indexing:constraint_classification(paris_municipal_reform_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_municipal_reform_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(paris_municipal_reform_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(paris_municipal_reform_2026, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(paris_municipal_reform_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(paris_municipal_reform_2026, TR),
    TR >= 0.70.

:- end_tests(paris_municipal_reform_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The majority premium, even at 25%, systematically extracts legitimacy from smaller parties. Pre-2026 (50%), extraction was severe (ε≈0.68); post-2026 (25%), the mechanism persists but with reduced force. The trajectory shows declining extraction as the system moves toward proportionality (t0=0.68 → t6=0.52). However, 25% is not proportionality; it still privileges the largest coalition bloc by a factor of 2.5-3x relative to vote share. Suppression (0.58): Moderate-high. Structural barriers include ballot access costs, media attention concentration on large parties, voter psychology (threshold effects), and coalition negotiation complexity for small parties. The 2026 reform partially lowers these barriers but does not eliminate them. Parties in the 8-15% range now have realistic hopes for meaningful representation, improving from previous complete exclusion. Theater ratio (0.64): High-moderate. The reform is partly performative: it appears to democratize through proportionality language while preserving majority premium mechanics. The incumbent coalition frames the change as 'balancing stability with legitimacy,' which is theater — the real function (executive stability via bonus) persists. However, the theater is not maximal (0.70+) because the bonus reduction is genuine, not purely symbolic.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates significant perspectival divergence. The incumbent coalition sees coordination (Rope: 'we maintain stable governance while improving legitimacy'). The left-green opposition sees mixed extraction and coordination (Tangled Rope: 'we benefit from proportionality but remain constrained by the residual bonus'). Small parties see structural trap (Snare: 'the bonus reduction does not eliminate our disadvantage'). Electoral reform advocates see scaffolding (Scaffold: 'this is a step toward full proportionality; the sunset is committed in platform'). The administration sees inertia masked by cosmetics (Piton: 'coalition composition may shift but governance continuity persists'). The analytical observer sees genuine ambiguity (Tangled Rope: 'the system is simultaneously stabilizing and legitimizing; which is primary depends on constitutional intent'). This range of types from a single set of metrics illustrates how indexical position determines classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Center-right incumbent: Beneficiary + arbitrage → d≈0.12, f(d)≈-0.04. Net beneficiary. The reform preserves their governing advantage; arbitrage means they could shift focus nationally if municipal politics became unfavorable. Left-green coalition: Victim (from prior extraction under 50% bonus) + constrained → d≈0.58, f(d)≈0.74. Moderate extraction. They benefit from the reform but remain constrained by Paris electoral outcomes; they cannot arbitrage to another jurisdiction. Small parties: Victim + trapped → d≈0.92, f(d)≈1.38. High extraction. They bear structural disadvantage with no exit; Paris politics is not optional for localized parties. Electoral proportionality principle: Victim (abstract) + trapped → d≈0.95, f(d)≈1.42. Maximum extraction. The abstract principle of proportional representation cannot exit; it is systematically suppressed by the bonus structure. Administrative apparatus: Institutional + arbitrage → d≈0.05, f(d)≈-0.12. Minimal extraction; inertia dominates. Electoral reform movement: Organized + constrained → d≈0.40, f(d)≈0.40. Low-moderate extraction from legacy system; organized actors see a path forward (sunset to full PR).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the 50%→25% reduction is a genuine compromise rather than a false equilibrium. The reform both extracts (suppresses proportionality relative to a 1-person-1-vote ideal) and coordinates (stabilizes executive power, enables coalition-building). The classification as Tangled Rope reflects this irreducible ambiguity. The reform cannot be decomposed into 'pure extraction that looks like coordination' (Snare disguised) or 'pure coordination with minor extraction as theater' (Rope with theater). Instead, the extraction and coordination are structurally independent: the majority premium would suppress proportionality even if it perfectly ensured stable governance, and governance stability would require concentrated power even without proportionality suppression. The tangled_rope classification is terminal, not false. The perspectival divergence (incumbent→Rope, opposition→Tangled Rope, small parties→Snare, reformers→Scaffold, administration→Piton, analyst→Tangled Rope) confirms the hybrid structure: different structural positions genuinely experience different constraint types because they differently benefit from coordination and suffer from extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    majority_premium_terminal_target,
    'Is the 25% majority premium the final stable equilibrium, or will further reforms continue eroding it toward full proportionality?',
    'Monitor coalition platforms in 2028 elections; track legislative proposals for electoral law revisions at national and municipal levels; survey elite consensus on ''ideal'' Paris governance structure',
    'If stable at 25%: tangled_rope classification is terminal. If eroding further: scaffold classification is correct — the system is a stepping stone. If pressures reverse toward 50%: snare extraction is hidden behind temporary cosmetics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majority_premium_terminal_target, preference, 'Whether 25% is final or interim majority premium target').

omega_variable(
    coalition_fragmentation_boundary,
    'What vote-share fragmentation point makes executive stability impossible, forcing government collapse or grand coalitions?',
    'Simulate 2026 and 2028 election scenarios with varying fragmentation; track coalitional stability over 6-year terms; compare to other French cities with varying bonus structures',
    'If boundary < 35%: 25% bonus insufficient to prevent fragmentation crises; system degrades to gridlock. If boundary > 45%: 25% bonus still concentrates power excessively; reform insufficient. If 35-45%: reform hits a narrow stability band.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coalition_fragmentation_boundary, empirical, 'Coalition stability boundary as function of vote-share fragmentation').

omega_variable(
    small_party_mobilization_response,
    'Will the improved proportionality (50%→25% bonus reduction) trigger mobilization of parties in the 8-15% range, or do structural barriers prevent coalition-building?',
    'Analyze 2026 campaign messaging and voter targeting by small parties; measure vote gains in groups previously below threshold; track coalition negotiation difficulty in post-2026 period',
    'If mobilization strong: proportionality improvement is real; snare classification for small parties improves to constrained. If weak: structural barriers (ballot access, media access, threshold psychology) persist; snare classification remains despite bonus reduction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(small_party_mobilization_response, empirical, 'Whether sub-threshold parties will mobilize effectively under reduced bonus').

omega_variable(
    administrative_continuity_mechanism,
    'Does the majority premium reduction actually change governance outcomes (coalition composition, policy direction, committee leadership) or do administrative networks preserve continuity regardless?',
    'Compare committee leadership, policy outcomes, and administrative decision-making pre-2026 vs post-2026; measure coalition volatility; track whether aide-mémoires and administrative culture shift or persist',
    'If outcomes change substantially: reform has real effects; piton classification is too pessimistic. If outcomes persist: piton theater ratio (0.64) is accurate; administrative inertia masks political change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(administrative_continuity_mechanism, empirical, 'Whether majority premium reduction produces governance change or masks continuity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_municipal_reform_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pmr_theater_t0, paris_municipal_reform_2026, theater_ratio, 0, 0.55).
narrative_ontology:measurement(pmr_theater_t3, paris_municipal_reform_2026, theater_ratio, 3, 0.6).
narrative_ontology:measurement(pmr_theater_t6, paris_municipal_reform_2026, theater_ratio, 6, 0.64).

% Extraction over time
narrative_ontology:measurement(pmr_extract_t0, paris_municipal_reform_2026, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(pmr_extract_t3, paris_municipal_reform_2026, base_extractiveness, 3, 0.58).
narrative_ontology:measurement(pmr_extract_t6, paris_municipal_reform_2026, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_municipal_reform_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(paris_municipal_reform_2026, french_municipal_suffrage).
narrative_ontology:affects_constraint(paris_municipal_reform_2026, european_proportional_representation).

% DUAL FORMULATION NOTE:
% The Paris reform is downstream of broader French municipal governance principles and European electoral systems. The majority premium itself (constraint_id: majority_premium_principle) would be a separate higher-level constraint; this story focuses on the 50%→25% reform event as a distinct structural shift.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(paris_municipal_reform_2026, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
