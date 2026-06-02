% ============================================================================
% CONSTRAINT STORY: settlement_fait_accompli
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_settlement_fait_accompli, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: settlement_fait_accompli
 *   human_readable: Settlement Fait Accompli: Territorial Lock-In via Infrastructure and Demographic Change
 *   domain: political_theory/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   The settlement fait accompli constraint describes the mechanism by which
 *   territorial control becomes politically and practically irreversible
 *   through the accumulation of demographic, infrastructural, and legal facts
 *   on the ground. This is not a constraint on resource allocation or service
 *   provision — it is a structural lock on territorial sovereignty claims
 *   themselves. The constraint operates by converting a political-choice
 *   scenario (territory can be allocated to different polities through
 *   negotiation) into an apparent physical-fact scenario (settlement
 *   infrastructure, population distribution, and effective control make
 *   alternative allocations infeasible). The mechanism functions through
 *   suppression (military enforcement, legal barriers to Palestinian movement
 *   and property claims, resource extraction favoring settlers) and through
 *   theater (performative peace processes, two-state rhetoric, international
 *   negotiations that proceed without material changes to settlement
 *   expansion). The constraint is extractive: it transfers territorial
 *   control, resource rights, and sovereignty options from Palestinian
 *   polities to Israeli state and settler populations. It is suppressive:
 *   resistance to settlement expansion is met with military force, legal
 *   penalties, and resource denial. It is mixed: the Israeli state genuinely
 *   coordinates security and administration across its expanded population,
 *   creating a real coordination function layered beneath the extractive
 *   mechanism.
 *
 * KEY AGENTS:
 *   - Palestinian Displaced and Occupied Populations: Primary victims (powerless/trapped, identity_locked) — structurally unable to exit settlements, cognitively bound to territorial sovereignty claims, bear full suppression and resource extraction costs
 *   - Settlement Population: Primary beneficiaries (institutional/arbitrage) — receive subsidized housing, resource access (water, land), and demographic majority status; experience constraint as coordination
 *   - Israeli State as Occupying Authority: Secondary beneficiary & primary enforcer (institutional/constrained) — experiences constraint as security coordination mixed with territorial expansion; exit constrained by ideological commitment and security doctrine
 *   - Palestinian National Authority: Victim & constrained negotiator (institutional/constrained) — formally recognized but territorially fragmented; caught between suppression and performative peace process
 *   - International Peace Process Apparatus: Institutional theater (institutional/arbitrage) — maintains negotiation protocols, two-state rhetoric, and diplomatic procedure while settlement expansion continues; experiences constraint as coordination of states' interests
 *   - External Powers & Resource Extractors: Secondary beneficiaries (institutional/arbitrage) — benefit from stabilized territorial control, trade agreements, resource access; see constraint as coordination
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent political choices as immutable geographic facts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(settlement_fait_accompli, 0.68).
domain_priors:suppression_score(settlement_fait_accompli, 0.72).
domain_priors:theater_ratio(settlement_fait_accompli, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(settlement_fait_accompli, extractiveness, 0.68).
narrative_ontology:constraint_metric(settlement_fait_accompli, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(settlement_fait_accompli, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(settlement_fait_accompli, snare).
narrative_ontology:human_readable(settlement_fait_accompli, "Settlement Fait Accompli: Territorial Lock-In via Infrastructure and Demographic Change").
narrative_ontology:topic_domain(settlement_fait_accompli, "political_theory/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(settlement_fait_accompli).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(settlement_fait_accompli, '756d1406-5ee3-4f1d-bfef-93ad633343cd').
narrative_ontology:cs_kernel_codification('756d1406-5ee3-4f1d-bfef-93ad633343cd', distributed).
narrative_ontology:cs_authority_grounding('756d1406-5ee3-4f1d-bfef-93ad633343cd', extraction).
narrative_ontology:cs_reading_relation('756d1406-5ee3-4f1d-bfef-93ad633343cd', territorial_legitimacy_historical_presence, coexists_with).
narrative_ontology:cs_reading_relation('756d1406-5ee3-4f1d-bfef-93ad633343cd', territorial_legitimacy_effective_control, influences).
narrative_ontology:cs_axiom('756d1406-5ee3-4f1d-bfef-93ad633343cd', foundational, territorial_belonging_grounded_in_historical_continuous_presence).
narrative_ontology:cs_axiom_status(territorial_belonging_grounded_in_historical_continuous_presence, holdable).
narrative_ontology:cs_axiom_grounding('756d1406-5ee3-4f1d-bfef-93ad633343cd', territorial_belonging_grounded_in_historical_continuous_presence, deontological).
narrative_ontology:cs_axiom('756d1406-5ee3-4f1d-bfef-93ad633343cd', foundational, territorial_belonging_grounded_in_effective_control_and_state_capacity).
narrative_ontology:cs_axiom_status(territorial_belonging_grounded_in_effective_control_and_state_capacity, holdable).
narrative_ontology:cs_axiom_grounding('756d1406-5ee3-4f1d-bfef-93ad633343cd', territorial_belonging_grounded_in_effective_control_and_state_capacity, instrumental).
narrative_ontology:cs_reference_frame('756d1406-5ee3-4f1d-bfef-93ad633343cd', shared_commitment_to_self_determination_and_territorial_integrity).
narrative_ontology:cs_drift_state('756d1406-5ee3-4f1d-bfef-93ad633343cd', post_oslo_settlement_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('756d1406-5ee3-4f1d-bfef-93ad633343cd', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(settlement_fait_accompli, settlement_populations).
narrative_ontology:constraint_beneficiary(settlement_fait_accompli, resource_extraction_entities).
narrative_ontology:constraint_victim(settlement_fait_accompli, palestinian_displaced_populations).
narrative_ontology:constraint_victim(settlement_fait_accompli, territorial_sovereignty_alternatives).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED PALESTINIAN COMMUNITIES (SNARE) — Structurally trapped by loss of property, legal barriers to return, and resource dependency. The fait accompli constraint operates by converting reversible political options into irreversible demographic/infrastructure facts. Exit from the settlement zone requires abandoning ancestral land, property claims, and community continuity. Maximum suppression from settlement expansion, military enforcement, and legal frameworks criminalizing resistance.
constraint_indexing:constraint_classification(settlement_fait_accompli, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PALESTINIAN NATIONAL SOVEREIGNTY CLAIM (SNARE) — The sovereignty claim is cognitively fused with territorial integrity narratives. Exit would require abandoning the claim to specific territory — not merely accepting a reduced state, but accepting that Palestinian identity and self-determination have been redefined by fait accompli settlement. The binding is identity-based: what it means to be Palestinian is structured around return, territoriality, and self-determination on specific land. The constraint appears immutable from within this identity frame.
constraint_indexing:constraint_classification(settlement_fait_accompli, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 3: ISRAELI STATE / OCCUPYING AUTHORITY (TANGLED ROPE) — Genuine coordination function (state security, resource management, population services) layered with asymmetric extraction (territorial expansion, demographic advantage, legal domination). Active enforcement required to maintain the fait accompli. The state experiences the constraint as coordination (managing its own population and territory) while simultaneously extracting through suppression of alternative sovereignty claims. Exit options are constrained by ideological commitment to settlement and security doctrine, but not structurally trapped.
constraint_indexing:constraint_classification(settlement_fait_accompli, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: EXTERNAL POWERS & BENEFICIARIES (ROPE) — Third-party states and resource extraction entities perceive the fait accompli as coordination: stabilized territorial control reduces transaction costs for trade agreements, resource access, and strategic partnerships. Settlement expansion is experienced as infrastructure development (roads, water systems, telecommunications) that benefits all connected actors. Pure coordination from the beneficiary perspective; extraction is externalized to trapped populations.
constraint_indexing:constraint_classification(settlement_fait_accompli, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: TWO-STATE SOLUTION INSTITUTIONAL APPARATUS (PITON) — The international consensus framework (Oslo Accords, peace process protocols, UN resolutions) maintains performative commitment to two-state solutions while settlements proceed that make two-state viability increasingly theatrical. The apparatus persists through institutional inertia and diplomatic procedure despite the structural fait accompli making its foundational premise less achievable. Theater ratio is high: negotiation rounds, international conferences, and status discussions continue without material changes in settlement footprint or demographic reality. The apparatus has become theater for managing the contradiction between stated commitments and structural realities.
constraint_indexing:constraint_classification(settlement_fait_accompli, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / IMMUTABILITY VIEW (MOUNTAIN) — From a sufficiently long civilizational timescale, settlement fait accompli appears to approach physical immutability: hundreds of thousands of people, infrastructure systems, water rights, and territorial control embedded in geography and demography. The analytical view risks naturalizing the settlement as an unchangeable fact of the world — a physical reality equivalent to a mountain — rather than recognizing it as a contingent institutional and political construction. This classification is a false summit candidate: what appears inevitable at the civilizational scale is maintained by continuous political choices and enforcement mechanisms.
constraint_indexing:constraint_classification(settlement_fait_accompli, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(settlement_fait_accompli_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(settlement_fait_accompli, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(settlement_fait_accompli, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(settlement_fait_accompli, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(settlement_fait_accompli, TR),
    TR >= 0.70.

:- end_tests(settlement_fait_accompli_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. Initial extraction (0.35) reflects early-stage settlement and territorial grab. Mid-interval (0.52) reflects consolidated settlement infrastructure and demographic shift. Terminal (0.68) reflects fait accompli lock-in where Palestinian territorial options have been substantially reduced and settlers have gained control over majority of West Bank water, arable land, and strategic locations. The extraction is not maximum (0.95 → snare) because Palestinian polities retain nominal governance authority in fragmented areas and international legal claims still have formal status. Suppression (0.72): High. Extraction is maintained through military enforcement (checkpoints, patrol, incursions), legal barriers (settlement zoning, permit denial for Palestinians, property seizure), and resource denial (water allocation favoring settlers, road access restrictions). Suppression has intensified over the interval (0.48 → 0.72) as resistance has grown and enforcement infrastructure has hardened. Theater ratio (0.58): Moderate-high and rising. Peace process negotiations, two-state rhetoric, international conferences, and status discussions continue with minimal material impact on settlement expansion footprint. The theater ratio increase (0.32 → 0.58) reflects growing gap between performative diplomatic activity and structural fait accompli accumulation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence. The Palestinian populations experience pure extraction with cognitive lock (snare + identity_locked). The Israeli state experiences coordination with embedded extraction (tangled_rope). External powers and beneficiaries experience pure coordination (rope). The peace apparatus experiences its own degradation (piton — performing rituals without function). The civilizational observer risks seeing immutable fact (mountain) when the constraint is actually maintained by continuous political enforcement. The perspectival gap reveals that the same settlement infrastructure appears as natural environmental fact to some observers, extractive lock-in to others, and manageable coordination problem to beneficiaries. No single perspective dominates — the presheaf over observation positions is the structural reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) for each perspective are determined by the agent's structural position relative to extraction flow. Displaced Palestinian populations: d ≈ 0.95 (full target of extraction). Settlement populations: d ≈ 0.05 (full beneficiaries; exit is arbitrage because they can relocate but choose settlement subsidy). Israeli state: d ≈ 0.55 (mixed beneficiary-enforcer; constrained exit because ideological commitment prevents abandonment). Palestinian national authority: d ≈ 0.75 (victim but with nominal institutional authority). International peace apparatus: d ≈ 0.10 (beneficiary through stabilized regional control; arbitrage exit because externally situated). The engine will derive these values from beneficiary/victim declarations plus exit modulation. The snare classification emerges from high extractiveness combined with high suppression and trapped/identity_locked exit options. The tangled_rope classification for the Israeli state reflects its mixed role as both coordinator (genuine security administration) and extractor (territorial expansion, resource concentration). The piton classification for the peace apparatus reflects its high theater ratio (performative negotiation) combined with degraded function (inability to alter settlement trajectory).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint presents a clean mandatrophy case because extractiveness is unambiguously high (0.68 > 0.66) yet the constraint is not pure snare (Israeli state perspective shows tangled_rope). The resolution requires that both the extraction (territorial lock-in, resource transfer to settlers) AND the coordination function (legitimate state administration, security provision) be recognized as simultaneously true. The constraint is snare for trapped populations but tangled_rope for the state. It is not snare-only because some of the enforcement infrastructure does provide real public goods to the expanded population (roads, water systems, administration). Mandatrophy resolves by accepting the perspectival multiplicity: the constraint is Snare+Tangled Rope as an irreducible pair. The constraint's extractiveness persists precisely because the coordination function gives it legitimacy and sustainability — extraction embedded in coordination is harder to dismantle than pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reversibility_threshold,
    'At what point does settlement expansion become irreversible — is there a demographic or infrastructure threshold beyond which territorial rearrangement becomes infeasible regardless of political will?',
    'Historical comparison with other post-conflict territorial realignments (population exchanges, partition scenarios); cost-benefit analysis of hypothetical settlement evacuation; demographic modeling of maximum feasible displacement without humanitarian crisis',
    'If reversibility threshold crossed: mountain classification confirmed (fait accompli is immutable). If threshold not yet crossed: snare classification confirmed (extraction via suppression, not immutable law). If threshold is policy-dependent: tangled_rope confirmed (whether fait accompli persists depends on enforcement choices).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reversibility_threshold, empirical, 'Whether settlement expansion crosses a reversibility threshold').

omega_variable(
    kernel_coherence_ambiguity,
    'Are competing territorial legitimacy claims readings of one kernel (shared commitment to land''s significance, disagreement on allocation), or are they incoherent claims with no shared substrate?',
    'Philosophical analysis of shared normative commitments: do both polities accept shared axioms about territorial integrity, self-determination, historical belonging? If yes, kernel. If no shared axioms, separate kernels with no coherence bridge.',
    'If one kernel, two readings: fait accompli is strategy within a shared legitimacy framework (constraint is resolvable through negotiation). If separate kernels: fait accompli is structural lock-in because no common language exists to adjudicate claims (constraint persists through incommensurability).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_coherence_ambiguity, conceptual, 'Whether territorial legitimacy claims are readings of one kernel or separate incoherent kernels').

omega_variable(
    suppression_internalization_split,
    'What proportion of suppression against Palestinian territorial claims is structural (external military/legal barriers) versus internalized (cognitive acceptance of fait accompli as inevitable)?',
    'Post-settlement analysis: if external barriers were removed, would territorial sovereignty claims revive at baseline strength or have they been cognitively embedded as unchangeable? Tracking of generational identity shifts in diaspora vs. occupied populations.',
    'If high internalization: snare persists through identity_locked exit (cognitive rather than structural trap). If mostly structural: snare is vulnerable to enforcement capacity reduction. If mixed: suppression_requirement oscillates with enforcement intensity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_split, empirical, 'Structural vs. internalized components of suppression against Palestinian claims').

omega_variable(
    international_law_kernel_drift,
    'Has the international legal kernel grounding territorial claims drifted from self-determination + decolonization to self-determination + effective control?',
    'Tracking of UN resolution language, ICJ doctrine, and state practice over time. Analysis of whether effective-control doctrine is new reading or axiom override of decolonization principle.',
    'If drift is axiom_overriding: the mountain view gains force (effective control becomes immutable through legal doctrine). If drift is new reading: snare view persists (territorial claims remain live, suppressed by enforcement not law).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_law_kernel_drift, conceptual, 'Whether international law kernel has drifted from self-determination to effective control').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(settlement_fait_accompli, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(settle_theater_t0, settlement_fait_accompli, theater_ratio, 0, 0.32).
narrative_ontology:measurement(settle_theater_t15, settlement_fait_accompli, theater_ratio, 15, 0.45).
narrative_ontology:measurement(settle_theater_t30, settlement_fait_accompli, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(settle_extract_t0, settlement_fait_accompli, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(settle_extract_t15, settlement_fait_accompli, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(settle_extract_t30, settlement_fait_accompli, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(settle_supp_t0, settlement_fait_accompli, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(settle_supp_t15, settlement_fait_accompli, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(settle_supp_t30, settlement_fait_accompli, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(settlement_fait_accompli, enforcement_mechanism).
narrative_ontology:affects_constraint(settlement_fait_accompli, palestinian_state_viability).
narrative_ontology:affects_constraint(settlement_fait_accompli, water_rights_allocation).
narrative_ontology:affects_constraint(settlement_fait_accompli, right_of_return_claim).

% DUAL FORMULATION NOTE:
% The settlement fait accompli is the trunk constraint from which multiple downstream constraints branch. Palestinian state viability is degraded by territorial fragmentation caused by settlements. Water rights allocation is biased toward settlements. Right of return claims become increasingly impossible to implement as settlement population grows. Each downstream constraint has its own epsilon value and structural dynamics, but all are causally shaped by the fait accompli mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(settlement_fait_accompli, institutional, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
