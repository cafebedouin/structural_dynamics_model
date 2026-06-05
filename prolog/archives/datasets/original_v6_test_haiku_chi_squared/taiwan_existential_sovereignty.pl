% ============================================================================
% CONSTRAINT STORY: taiwan_existential_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_taiwan_existential_sovereignty, []).

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
 *   constraint_id: taiwan_existential_sovereignty
 *   human_readable: The Taiwan Existential Sovereignty Constraint
 *   domain: political/economic/technological
 *
 * SUMMARY:
 *   Taiwan's existential sovereignty constraint models the structural
 *   condition under which a de facto independent state exists in permanent
 *   threat of absorption by a larger, militarily ascending power. This
 *   constraint has persisted for 75 years (since 1949) and has become more
 *   acute over the past 15 years as PRC military capacity has expanded and
 *   the US-China strategic rivalry has intensified. The constraint is not a
 *   discrete event or negotiable arrangement; it is a permanent structural
 *   fact that governs all Taiwan policy domains — military spending (2-3% of
 *   GDP), diplomatic isolation (non-UN membership), economic dependency
 *   management, and psychological readiness for existential risk. The
 *   constraint exhibits characteristics of a pure snare from Taiwan's
 *   perspective: high coercion (military threat, economic pressure, political
 *   isolation), substantial suppression of alternatives (no exit option
 *   without loss of statehood), and minimal coordination benefit (the
 *   deterrent is maintained by others, not negotiated). From institutional
 *   perspectives, the constraint appears as a tangled rope — the US and PRC
 *   both benefit from the deterrent stability that keeps the threat credible
 *   without triggering kinetic conflict, but both are also bound by
 *   commitment logic that prevents de-escalation. The constraint has degraded
 *   in functional terms (theater ratio rising from 0.42 to 0.58) as
 *   diplomatic theater has expanded relative to real negotiation, while
 *   extractiveness has increased (from 0.48 to 0.68) as military asymmetry
 *   has widened and Taiwan's economic integration with China has created new
 *   leverage points. The regional stability coalition sees this constraint as
 *   a temporary scaffold with a 10-15 year sunset horizon driven by
 *   supply-chain resilience and technological autonomy; the geopolitical
 *   realist risks naturalizing the constraint as an immutable law of
 *   great-power competition.
 *
 * KEY AGENTS:
 *   - Taiwan Populace: Primary victim (powerless/trapped) — bears full existential risk; has no exit option without ceasing to exist as political entity
 *   - Taiwan Institutional State: Powerful victim (powerful/constrained) — can conduct sophisticated diplomacy but remains fundamentally constrained; allocates 2-3% GDP to military necessity
 *   - United States Extended Deterrence System: Primary beneficiary (institutional/arbitrage) — gains geopolitical leverage, alliance consolidation, technology justification; but bound by credibility commitment
 *   - PRC Military Establishment: Secondary beneficiary (institutional/constrained) — coordinates state unification goal; cannot escalate without losing global standing, cannot retreat without losing deterrent credibility
 *   - Regional Stability Coalition: Organized actor (organized/mobile) — see constraint as temporary; building supply-chain resilience and technological autonomy to create exit pathway
 *   - One-China Diplomatic System: Institutional theater (institutional/arbitrage) — maintains performative protocols while constraint remains structurally unresolved
 *   - Geopolitical Realist Observer: Analytical observer (analytical/analytical) — risks naturalizing contingent institutional arrangement as immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(taiwan_existential_sovereignty, 0.68).
domain_priors:suppression_score(taiwan_existential_sovereignty, 0.78).
domain_priors:theater_ratio(taiwan_existential_sovereignty, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(taiwan_existential_sovereignty, extractiveness, 0.68).
narrative_ontology:constraint_metric(taiwan_existential_sovereignty, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(taiwan_existential_sovereignty, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(taiwan_existential_sovereignty, snare).
narrative_ontology:human_readable(taiwan_existential_sovereignty, "The Taiwan Existential Sovereignty Constraint").
narrative_ontology:topic_domain(taiwan_existential_sovereignty, "political/economic/technological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(taiwan_existential_sovereignty, prc_military_deterrent_credibility).
narrative_ontology:constraint_beneficiary(taiwan_existential_sovereignty, us_extended_deterrence_role).
narrative_ontology:constraint_victim(taiwan_existential_sovereignty, taiwan_civil_society).
narrative_ontology:constraint_victim(taiwan_existential_sovereignty, taiwan_economic_independence).
narrative_ontology:constraint_victim(taiwan_existential_sovereignty, regional_stability_apparatus).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TAIWAN POPULACE (SNARE) — Structurally trapped. Cannot exit the constraint without ceasing to exist as a political entity. Extraction manifests as permanent militarization, subordination of domestic policy to military necessity, and existential risk premium. d≈0.98, f(d)≈1.44, σ=0.9 → χ≈0.96. Pure snare: high coercion, zero coordination benefit.
constraint_indexing:constraint_classification(taiwan_existential_sovereignty, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: TAIWAN INSTITUTIONAL STATE (SNARE) — Powerful actor (state apparatus) but existentially constrained. Can conduct diplomacy and build coalitions but cannot resolve the fundamental threat. Must allocate 2-3% GDP to military; cannot pursue aggressive independent foreign policy. d≈0.85, f(d)≈1.16, σ=1.0 → χ≈0.79.
constraint_indexing:constraint_classification(taiwan_existential_sovereignty, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: US EXTENDED DETERRENCE (TANGLED ROPE) — Institutional beneficiary (arbitrage exit). Gains geopolitical leverage, alliance consolidation, technology transfer justification, defense industry revenue. But also bound by Taiwan commitment credibility — must maintain deterrent posture or lose entire regional architecture. d≈0.15, f(d)≈-0.01, σ=1.2 → χ≈-0.01. Net beneficiary. Receives coordination function (maintains regional order) plus asymmetric extraction from Taiwan (dependency).
constraint_indexing:constraint_classification(taiwan_existential_sovereignty, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PRC MILITARY ESTABLISHMENT (TANGLED ROPE) — Institutional actor constrained by the logic of deterrent credibility. Coordinates with state strategy (resolving Taiwan unification goal) but extraction mechanism is constant military spending, cyber/intelligence operations, and political coercion. Cannot escalate without losing global standing; cannot retreat without losing deterrent credibility. d≈0.52, f(d)≈0.67, σ=1.2 → χ≈0.55. Mixed: coordination (deterrent stability) plus extraction (military burden).
constraint_indexing:constraint_classification(taiwan_existential_sovereignty, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: REGIONAL STABILITY COALITION (SCAFFOLD) — Organized non-state actors (business, civil society, academic networks across Taiwan/Japan/South Korea/US/Australia). See the constraint as temporary coordination problem with a sunset: supply chain diversification, technological autonomy, and institutional maturation are building a world where Taiwan's existential dependency decreases. Theater ratio for this coalition is low (functional problem-solving); sunset clause is implicit in supply-chain resilience roadmaps (10-15 year horizon). d≈0.45, f(d)≈0.48, σ=1.1 → χ≈0.25. Low extraction; coalition has agency.
constraint_indexing:constraint_classification(taiwan_existential_sovereignty, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ONE-CHINA DIPLOMATIC SYSTEM (PITON) — UN structures, diplomatic protocols, and recognition frameworks maintain the fiction that the Taiwan question is 'internal Chinese affairs' despite Taiwan's 75 years of de facto sovereignty. Theater ratio ≈0.58 (formal performativity: diplomatic statements, official visits, non-interference doctrine) masks zero functional coordination — the constraint persists through institutional inertia and great-power mutual interest, not because the diplomatic system actually resolves anything. d≈0.40, f(d)≈0.40, σ=1.0 → χ≈0.23. Piton classification follows from theater gate; the system is degraded (it doesn't work) but persists (alternative frameworks are more costly than maintaining ritual).
constraint_indexing:constraint_classification(taiwan_existential_sovereignty, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 7: GEOPOLITICAL REALIST / MOUNTAIN VIEW (FALSE SUMMIT) — Tempting but incorrect naturalization: Taiwan's existential threat is an immutable feature of great-power competition, historical inevitability, geographic determinism. The analytical observer risks calling this a mountain (natural law) because the constraint appears unchangeable at civilizational timescales. However, the structural data (ε=0.68, suppression=0.78, theater=0.58) contradicts the mountain classification. Accessibility collapse and resistance metrics would fail the mountain gate. This is a snare maintained by institutional power structures, not a law of nature.
constraint_indexing:constraint_classification(taiwan_existential_sovereignty, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(taiwan_existential_sovereignty_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(taiwan_existential_sovereignty, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(taiwan_existential_sovereignty, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(taiwan_existential_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(taiwan_existential_sovereignty, TR),
    TR >= 0.70.

:- end_tests(taiwan_existential_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68): High and rising. The constraint extracts substantial resources (military spending), time (constant security planning), and autonomy (foreign policy subordination to military necessity). Extractiveness increased from 0.48 to 0.68 over the 30-year interval as PRC military capability asymmetry widened and Taiwan's economic integration with China created new coercion vectors. This is not maximal extraction (not yet 0.78+) because Taiwan retains significant institutional capacity, business dynamism, and international support; the extraction is real but not total. Suppression (0.78): Very high. Taiwan has minimal exit options: cannot militarily match PRC, cannot diplomatically resolve the issue (one-China framework forecloses negotiations), cannot economically decouple entirely (supply chains integrated), cannot appeal to international law (UN structures exclude Taiwan). Suppression increased as military balance tipped and economic dependency deepened. Theater ratio (0.58): Moderate-high and rising. Diplomatic theater has expanded (annual statements, official visits, non-interference doctrine) while the actual negotiation function has shrunk — the one-China framework is pure theater, a frozen performance that preserves the constraint without resolving it. Theater increased from 0.42 to 0.58 as real diplomatic pathways closed and performative alternatives multiplied.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a stark perspectival gap between victim and beneficiary perspectives. Taiwan (powerless/trapped) perceives a pure snare: coercive threat with no coordination benefit, no exit, no negotiation path. The US deterrence system (institutional/arbitrage) perceives a tangled rope: it coordinates regional stability while extracting value from Taiwan's dependency. The PRC military (institutional/constrained) perceives a different tangled rope: it coordinates state strategy (unification) but is bound by deterrence logic that prevents escalation. The regional stability coalition (organized/mobile) perceives a scaffold: the constraint is temporary, bounded by supply-chain diversification and technological autonomy roadmaps with 10-15 year horizons. The one-China diplomatic system perceives a piton: the framework is degraded (doesn't resolve anything) but persists through institutional inertia and great-power interest. The geopolitical realist observer risks a false mountain: naturalizing a contingent power structure as a law of nature. These gaps are not measurement errors — they reflect real structural differences in how agents experience the constraint. Taiwan's snare is the ground truth; the beneficiary's tangled rope and the coalition's scaffold are legitimate but second-order observations.
 *
 * DIRECTIONALITY LOGIC:
 *   Taiwan Populace: Victim + trapped → d≈0.98, f(d)≈1.44. Maximum extraction. No exit option, no alternative path, no coordination benefit — the constraint is pure coercion. Taiwan Institutional State: Victim + constrained (not trapped, as the state retains diplomatic capacity) → d≈0.85, f(d)≈1.16. High extraction but not maximal; the state can conduct diplomacy, build coalitions, and maintain institutional viability. US Extended Deterrence: Beneficiary + arbitrage → d≈0.15, f(d)≈-0.01. Net beneficiary; can exit the commitment at cost but chooses to maintain it. Extraction to Taiwan is positive; extraction to US is negative. PRC Military: Beneficiary (relative to threat reduction) + constrained (by deterrence logic) → d≈0.52, f(d)≈0.67. Mixed position; gains from unification goal coordination but loses autonomy to deterrent credibility requirements. Regional Coalition: Actor + mobile → d≈0.45, f(d)≈0.48. Low extraction; coalition members have alternatives and agency. One-China Diplomatic System: Institutional + arbitrage → d≈0.40, f(d)≈0.40. Piton classification overrides directionality — the system persists through theater, not through effective coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATE MANDATROPHY RESOLUTION (extractiveness=0.68 > 0.70 threshold exceeded; mandatrophy_resolved=true): This constraint demonstrates how mandatrophy arises when a seemingly 'immutable law' (great-power competition, geographic determinism) actually masks contingent institutional extraction. The temptation to classify the constraint as a mountain (natural law) is strong — Taiwan's threat appears unchangeable at civilizational timescales. However, the structural data reveals the snare: suppression is 0.78 (sustained by institutional choices, not physics), extractiveness is 0.68 (institutional power dynamics, not natural law), and theater is 0.58 (frozen diplomatic performance, not negotiated settlement). The mandatrophy resolves by disaggregating the claim 'Taiwan's existential threat is inevitable' into its component pieces: (1) Military asymmetry is increasing (empirical, measurable, not inevitable — could be reversed by deterrence investment or technological breakthrough). (2) Economic interdependency increases vulnerability (institutional choice, not geographically determined — could be reversed by supply-chain diversification). (3) Diplomatic isolation persists (one-China theater, not law — could be resolved by framework change). (4) US commitment remains steady (preference, not inevitability — could shift with domestic politics). The constraint is not a natural law but a set of institutional decisions and power asymmetries. Calling it a mountain naturalizes extraction as fate. The proper classification — snare with emerging scaffold pathways — reveals that the constraint could be fundamentally altered through coordinated institutional change. This is the mandatrophy resolution: naming the constraint as contingent extraction (snare) rather than inevitable law (false mountain) enables the intellectual and political possibility of alternative arrangements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrent_credibility_threshold,
    'What level of military asymmetry between PRC and Taiwan triggers an existential shift from manageable constraint to active warfare scenario?',
    'Military capability modeling; historical analysis of past conflict escalation thresholds; strategic communication analysis from military planners in both PRC and US',
    'If threshold is crossed (estimated 2028-2035): constraint transitions from snare (deterrence works) to active conflict (deterrence fails). Classification becomes irrelevant — system moves to kinetic phase.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrent_credibility_threshold, empirical, 'Military capability threshold for deterrence failure').

omega_variable(
    economic_decoupling_viability,
    'Can Taiwan achieve economic independence from China through supply-chain diversification and alternative markets before military balance tips decisively?',
    'Semiconductor supply-chain modeling; trade pattern analysis; comparison of Taiwan tech sector integration vs South Korea/Japan alternatives',
    'If viable (achievable by 2032): Taiwan transitions from snare to scaffold — existential dependency decreases and constraint becomes temporary. If not viable: Taiwan remains in snare for foreseeable future, extraction deepens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_decoupling_viability, empirical, 'Whether economic decoupling from China is achievable').

omega_variable(
    us_commitment_credibility,
    'Will the United States maintain extended deterrence commitment to Taiwan over the next 15-30 years, or will domestic political shifts force a strategic pivot to accommodation with PRC?',
    'Congressional voting records; Department of Defense strategic guidance documents; historical precedent analysis (South Korea, Japan, Philippines); domestic political trend analysis',
    'If commitment holds: deterrent architecture persists, snare constraint remains stable. If commitment wanes: Taiwan''s exit options disappear entirely, constraint becomes pure snare with no institutional backing — existential threat becomes acute.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(us_commitment_credibility, preference, 'Long-term US commitment to Taiwan deterrence').

omega_variable(
    cross_strait_institutional_convergence,
    'Is a negotiated settlement (e.g., Hong Kong-style autonomy variant, independent confederation, or formalized asymmetric federalism) structurally possible, or are PRC unification demands and Taiwan autonomy demands incompatible by definition?',
    'Analysis of historical precedent cases (German reunification, Irish partition resolution); strategic communication from PRC and Taiwan leadership; feasibility assessment of confidence-building institutional designs',
    'If convergence possible: constraint could transition from snare to scaffold (temporary, negotiable). If incompatible: snare is terminal — no structural solution exists, only management or escalation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cross_strait_institutional_convergence, conceptual, 'Whether negotiated political settlement is structurally possible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(taiwan_existential_sovereignty, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twn_sov_theater_t0, taiwan_existential_sovereignty, theater_ratio, 0, 0.42).
narrative_ontology:measurement(twn_sov_theater_t15, taiwan_existential_sovereignty, theater_ratio, 15, 0.51).
narrative_ontology:measurement(twn_sov_theater_t30, taiwan_existential_sovereignty, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(twn_sov_extract_t0, taiwan_existential_sovereignty, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(twn_sov_extract_t15, taiwan_existential_sovereignty, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(twn_sov_extract_t30, taiwan_existential_sovereignty, base_extractiveness, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(taiwan_existential_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(taiwan_existential_sovereignty, semiconductor_supply_chain_concentration).
narrative_ontology:affects_constraint(taiwan_existential_sovereignty, us_china_technology_decoupling).
narrative_ontology:affects_constraint(taiwan_existential_sovereignty, regional_military_balance_asymmetry).

% DUAL FORMULATION NOTE:
% Taiwan's existential sovereignty constraint is upstream of three specific structural constraints: semiconductor supply concentration (Taiwan's economic leverage point), technology decoupling (US-China bifurcation affecting Taiwan), and regional military balance (direct causal input). Each downstream constraint has its own ε and type; the sovereignty constraint represents the political/existential frame within which those specific constraints operate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(taiwan_existential_sovereignty, institutional, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
