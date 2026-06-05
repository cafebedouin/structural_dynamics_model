% ============================================================================
% CONSTRAINT STORY: us_china_strategic_competition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_china_strategic_competition, []).

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
 *   constraint_id: us_china_strategic_competition
 *   human_readable: US-China Strategic Competition Constraint
 *   domain: geopolitical/security/economic
 *
 * SUMMARY:
 *   US-China strategic competition operates as a tangled hybrid of genuine
 *   coordination and asymmetric extraction across multiple domains. The
 *   constraint exhibits real coordination functions — mutual deterrence,
 *   maintenance of technological leadership incentives, security guarantee
 *   structures — alongside severe extractive mechanisms that concentrate
 *   costs on developing nations, fragment global supply chains, and impose
 *   alliance costs on middle powers. The constraint's evolution from the
 *   post-Cold War unipolar moment (1990s) through gradual Chinese rise
 *   (2000s-2010s) to acute competition (2020s) shows increasing
 *   extractiveness and theater. The theatrical component has grown as
 *   competition shifted from conventional military balance (where functional
 *   deterrence is measurable) to asymmetric domains (technology, information,
 *   economic coercion) where proving effectiveness is difficult and
 *   performative display dominates. The constraint operates simultaneously as
 *   rope (for both primary powers who experience low extraction and genuine
 *   coordination benefit), tangled rope (for middle powers experiencing both
 *   security benefits and extraction), scaffold (for multilateral
 *   institutions attempting to contain competition), piton (for Cold War-era
 *   institutions like NATO experiencing degraded function), snare (for
 *   developing nations with no exit), and false mountain (from perspectives
 *   that naturalize competition as inevitable rather than contingent).
 *
 * KEY AGENTS:
 *   - US Strategic Establishment: Primary beneficiary (institutional/arbitrage) — maintains global influence, captures technology leadership rents, coordinates allied bloc
 *   - Chinese State Apparatus: Primary beneficiary (institutional/arbitrage) — consolidates regional dominance, accelerates technology development, builds alternative institutional structures
 *   - Developing Nations: Primary victim (powerless/trapped) — no exit from bloc alignment pressure, bear costs of supply chain fragmentation, face sanctions and exclusion threats
 *   - Allied Nations & Middle Powers: Secondary victim (moderate/constrained) — security dependencies create constraints, but alliance benefits and economic access provide coordination function
 *   - Global Supply Chain & Finance: Powerful but mobile victim (powerful/mobile) — both benefit from trade flows and suffer from decoupling mandates; can partially arbitrage but face regulatory pressure
 *   - Multilateral Institutions: Organized actor attempting containment (organized/constrained) — try to preserve functional negotiating frameworks; see sunset as traditional governance models become obsolete
 *   - Cold War Institutional Legacy: Degraded institutional persistence (institutional/arbitrage) — NATO, bilateral treaties maintain theater despite reduced functional relevance
 *   - Analytical Observer: Civilizational risk perspective (analytical/analytical) — may naturalize competition as structural necessity rather than recognizing contingent institutional choices
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_china_strategic_competition, 0.58).
domain_priors:suppression_score(us_china_strategic_competition, 0.65).
domain_priors:theater_ratio(us_china_strategic_competition, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_china_strategic_competition, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_china_strategic_competition, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(us_china_strategic_competition, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_china_strategic_competition, tangled_rope).
narrative_ontology:human_readable(us_china_strategic_competition, "US-China Strategic Competition Constraint").
narrative_ontology:topic_domain(us_china_strategic_competition, "geopolitical/security/economic").

domain_priors:requires_active_enforcement(us_china_strategic_competition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_china_strategic_competition, us_military_industrial_complex).
narrative_ontology:constraint_beneficiary(us_china_strategic_competition, chinese_state_apparatus).
narrative_ontology:constraint_beneficiary(us_china_strategic_competition, defense_technology_sectors).
narrative_ontology:constraint_victim(us_china_strategic_competition, global_supply_chain_stability).
narrative_ontology:constraint_victim(us_china_strategic_competition, developing_nations).
narrative_ontology:constraint_victim(us_china_strategic_competition, multilateral_institutions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVELOPING NATIONS (SNARE) — Trapped between pressures to align with either bloc. No genuine exit from the constraint; bear costs of supply chain disruption, technology fragmentation, forced geopolitical allegiance. Maximum extraction with no coordination benefit. Cannot organize alternative to the US-China dyad without facing retaliation or exclusion.
constraint_indexing:constraint_classification(us_china_strategic_competition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALLIED NATIONS & MIDDLE POWERS (TANGLED ROPE) — Constrained by security dependencies and economic integration with primary blocs, but also benefit from security guarantees and trade access. Genuine coordination function (deterrence of escalation, provision of security architecture) exists alongside asymmetric extraction. High suppression through implicit threat of exclusion or economic pressure.
constraint_indexing:constraint_classification(us_china_strategic_competition, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: US STRATEGIC ESTABLISHMENT (ROPE) — Primary beneficiary with arbitrage optionality. Views competition as legitimate coordination mechanism: deterrence framework, alliance management, technology leadership. Experiences low extraction because competition serves institutional interests in maintaining global influence. Can arbitrage between military procurement, technology investment, and diplomatic positioning.
constraint_indexing:constraint_classification(us_china_strategic_competition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CHINESE STATE APPARATUS (ROPE) — Primary beneficiary with arbitrage optionality. Views competition as coordination mechanism for internal legitimacy, state capacity building, and regional dominance. Experiences low extraction because competition serves state consolidation objectives. Can arbitrage between military modernization, technological development, and economic statecraft.
constraint_indexing:constraint_classification(us_china_strategic_competition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: GLOBAL SUPPLY CHAIN & FINANCIAL SYSTEM (TANGLED ROPE) — Powerful but mobile actors who benefit from trade flows but face high costs from escalating economic competition and fragmentation. Dual constraints: genuine coordination function (efficient capital allocation, just-in-time manufacturing) coexists with asymmetric extraction (decoupling mandates, capital controls, technology restrictions). High suppression through regulatory pressure and sanctions.
constraint_indexing:constraint_classification(us_china_strategic_competition, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: MULTILATERAL INSTITUTIONS (SCAFFOLD) — International organizations (WTO, UN, ASEAN, IMF) function as temporary coordination structures attempting to contain competition within negotiating frameworks. Low effective extraction because organized agents (diplomatic coalitions, development banks) see sunset in traditional approaches. Alternative governance models (regional trading blocs, decoupled standards) represent nascent pathways. Theater ratio reflects increasing performative diplomacy with declining functional conflict management.
constraint_indexing:constraint_classification(us_china_strategic_competition, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: COLD WAR INSTITUTIONAL LEGACY (PITON) — NATO, bilateral security treaties, Cold War-era alliance structures persist through institutional inertia despite reduced functional relevance. The containment framework is largely performative: institutional actors maintain the rhetoric and theater of traditional competition while actual competition has shifted to asymmetric domains (information, technology, economic coercion). Theater is high (military exercises, diplomatic posturing) but functional coordination of genuine deterrence has degraded.
constraint_indexing:constraint_classification(us_china_strategic_competition, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risk of false summit. From a civilizational perspective, great power competition appears immutable: structural realism posits that anarchic international systems generate hegemonic competition as inherent natural law. However, the structural data reveals this as naturalization of contingent institutional choices (nuclear weapons adoption, alliance formation, technology fragmentation policy) rather than immutable constraint.
constraint_indexing:constraint_classification(us_china_strategic_competition, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_china_strategic_competition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_china_strategic_competition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_china_strategic_competition, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_china_strategic_competition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_china_strategic_competition, TR),
    TR >= 0.70.

:- end_tests(us_china_strategic_competition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately-high. The constraint extracts significantly from developing nations and middle powers through bloc pressure and supply chain disruption. But extraction is not total (snare-level) because primary beneficiaries experience genuine coordination benefits (deterrence framework prevents worse outcomes; technology competition drives innovation; security guarantees are real). The value reflects asymmetric distribution: some actors experience rope-level coordination, others experience snare-level extraction. Suppression (0.65): High. Developing nations face explicit threats of economic sanctions, technology exclusion, and alliance penalties for non-alignment. The cost of 'exit' (not choosing a side) is often higher than the cost of alignment. For middle powers, suppression is lower (they have some flexibility) but still substantial. Theater ratio (0.68): High and increasing. Much of the contemporary competition exists in asymmetric domains where effectiveness cannot be cleanly measured: information operations, technology standard-setting, economic coercion through sanctions. The performative content has increased as competition shifted from conventional military balance (where deterrence is functionally testable through non-escalation) to soft power domains where success is narrative rather than structural. The rise in theater_ratio from 0.42 to 0.68 reflects this shift.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between beneficiaries and victims is the core diagnostic signal. Beneficiaries see Rope (low χ); victims see Snare (high χ). This gap is NOT a measurement error — it is the constraint's actual structure. The same mechanism (bloc formation, deterrence requirements, technology competition) produces opposite experiences depending on structural position. For agents with arbitrage optionality, the mechanism is a coordination benefit (Rope). For agents with no exit, the mechanism is extraction (Snare). The theatre ratio increase (0.42 → 0.68) indicates that as competition moved from measurable military balance to asymmetric information domains, performative content replaced functional verification, turning what might have been a Rope into a Tangled Rope for middle powers and Snare for developing nations.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) flow from structural position: beneficiary status plus exit optionality. The US strategic establishment (institutional + arbitrage) derives d ≈ 0.15 (net beneficiary, high mobility). Chinese state apparatus (institutional + arbitrage) derives d ≈ 0.18 (net beneficiary, high mobility). Middle powers (moderate + constrained) derive d ≈ 0.60 (moderate extraction due to security dependence constraints). Developing nations (powerless + trapped) derive d ≈ 0.92 (maximum extraction due to no exit). Global supply chain (powerful + mobile) derives d ≈ 0.55 (extraction offset by mobility and power to exit specific partnerships). Multilateral institutions (organized + constrained by declining relevance) derive d ≈ 0.50 (symmetric pressure from both blocs attempting to co-opt). The sigmoid function maps these d values to experienced effective extractiveness (χ) accounting for power and scope amplification.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint exhibits genuine coordination (deterrence preventing escalation, security structures, technology innovation incentives) alongside severe asymmetric extraction (developing nations trapped, middle powers constrained, supply chains fragmented). The mandatrophy is resolved by recognizing that both descriptions are structurally correct from different perspectives. The US-China dyad solves a real coordination problem (preventing unmanaged great power conflict) while simultaneously extracting from those who depend on the coordination structure but cannot exit. This is the definition of Tangled Rope at the analytical level: genuine coordination function that requires asymmetric extraction to maintain. The false summit risk (Mountain from realist perspective) is mitigated by showing that the 'necessity' of competition rests on contingent choices (alliance architecture, decoupling policy, threat narrative framing) rather than structural inevitability. If those choices changed, the constraint would dissolve — it is not a natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mutual_escalation_trap,
    'Is the constraint genuinely symmetric (mutual escalation pressure) or asymmetrically structured (one actor benefits more from continuation)?',
    'Analysis of benefit flows: military spending as percentage of economic capacity; technology gain asymmetries; regional alliance costs vs benefits; domestic political distraction effects for each actor',
    'If symmetric: both perspectives (US institutional and Chinese institutional) correctly classify as Rope with low extraction. If asymmetric: one actor experiences higher extraction than believed, suggesting Tangled Rope or Snare from their actual structural position.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mutual_escalation_trap, empirical, 'Whether competition is genuinely mutual or asymmetrically benefiting one actor').

omega_variable(
    decoupling_reversibility,
    'Are technology and supply chain decoupling mechanisms reversible policy choices or irreversible structural fractures?',
    'Historical precedent analysis; cost estimates for re-coupling; institutional lock-in assessment; technological path dependency analysis',
    'If reversible: scaffold perspective is valid — sunset is structural and real. If irreversible: decoupling represents a permanent institutional shift, changing the constraint from coordination-extraction hybrid to durable parallel structures with different classification for each bloc.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoupling_reversibility, empirical, 'Whether technological decoupling is policy-reversible or structurally irreversible').

omega_variable(
    zero_sum_framing_contingency,
    'Is the competition structurally zero-sum (one actor''s gain is the other''s loss) or is zero-sum framing a contingent institutional choice that could yield to positive-sum alternatives?',
    'Comparative analysis of domains: military balance (zero-sum); technology standards (could be cooperative); climate technology (positive-sum potential); space exploration (mixed); economic competition (mixed); diplomatic influence (zero-sum)',
    'If structurally zero-sum: competition is necessary feature of international system (approaching Mountain). If contingent: the constraint exists because both actors choose competitive framing over cooperative alternatives — reclassifies as Snare (designed extraction) rather than natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zero_sum_framing_contingency, conceptual, 'Whether competition is structurally zero-sum or based on contingent framing choices').

omega_variable(
    internal_legitimacy_dependency,
    'How dependent is each regime''s internal legitimacy on the existence of an external strategic competitor?',
    'Discourse analysis of regime legitimacy narratives; polling on threat perception; domestic political sustainability under non-competition scenarios; succession stability analysis',
    'If high dependency: competition represents Snare for civilian populations (trapped by regimes that require external threat for internal control). If low dependency: competition represents genuine strategic choice. This is the critical difference between real geopolitical competition and extractive use of threat narrative for internal domination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_legitimacy_dependency, empirical, 'Regime legitimacy dependency on external strategic threat').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_china_strategic_competition, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uschi_tr_t0, us_china_strategic_competition, theater_ratio, 0, 0.42).
narrative_ontology:measurement(uschi_tr_t10, us_china_strategic_competition, theater_ratio, 10, 0.55).
narrative_ontology:measurement(uschi_tr_t20, us_china_strategic_competition, theater_ratio, 20, 0.68).
narrative_ontology:measurement(uschi_tr_t5, us_china_strategic_competition, theater_ratio, 5, 0.48).

% Extraction over time
narrative_ontology:measurement(uschi_be_t0, us_china_strategic_competition, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(uschi_be_t10, us_china_strategic_competition, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(uschi_be_t20, us_china_strategic_competition, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(uschi_be_t5, us_china_strategic_competition, base_extractiveness, 5, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_china_strategic_competition, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_china_strategic_competition, 0.18).
narrative_ontology:affects_constraint(us_china_strategic_competition, semiconductor_supply_chain_fragmentation).
narrative_ontology:affects_constraint(us_china_strategic_competition, rare_earth_technology_dependence).
narrative_ontology:affects_constraint(us_china_strategic_competition, multilateral_trade_framework_collapse).
narrative_ontology:affects_constraint(us_china_strategic_competition, alliance_technology_standards_divergence).

% DUAL FORMULATION NOTE:
% US-China strategic competition decomposes into multiple constraint families: (1) military balance (deterrence structure), (2) technology competition (standards and innovation), (3) supply chain decoupling (resource allocation), (4) alliance management (coordination costs). Each has different ε and different beneficiary/victim structures. The aggregate constraint story presented here models the hybrid structure at the systemic level; domain-specific constraint stories are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_china_strategic_competition, institutional, 0.16).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
