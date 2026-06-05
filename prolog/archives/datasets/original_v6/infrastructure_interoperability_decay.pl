% ============================================================================
% CONSTRAINT STORY: infrastructure_interoperability_decay
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_infrastructure_interoperability_decay, []).

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
 *   constraint_id: infrastructure_interoperability_decay
 *   human_readable: The Protocol Silo Trap
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The protocol silo trap describes a systematic degradation of universal
 *   communication standards into fragmented, proprietary sub-ecosystems. A
 *   standard originally designed as a coordination mechanism—reducing
 *   transaction costs and enabling interoperability—becomes a vehicle for
 *   lock-in when incumbent providers introduce vendor-specific extensions
 *   that are nominally optional but practically essential. Users invest in
 *   workflows and data that depend on these extensions; competitors cannot
 *   enter without costly compatibility layers; new adopters face
 *   incompatibilities that were not promised in the standard's original
 *   design. The constraint exhibits tangled rope structure: genuine
 *   coordination function (the base protocol remains common), active
 *   enforcement (standards governance maintains the facade of openness), and
 *   asymmetric extraction (lock-in surplus flows to incumbents). Theater
 *   increases over time as incumbents invest in compliance theater (standards
 *   logos, certification marks, interoperability claims) while actual
 *   compatibility declines. This is distinct from pure extraction (snare)
 *   because the standard does provide real coordination benefits to all
 *   parties initially—the extraction emerges from capture of the coordination
 *   mechanism, not from imposing a coordination-free tax.
 *
 * KEY AGENTS:
 *   - Incumbent Providers: Primary beneficiaries (institutional/arbitrage) — use proprietary extensions to lock in users while maintaining standard compliance facade
 *   - Locked-In Users: Primary victims (powerless/trapped) — invested in proprietary extensions for workflow efficiency; cannot exit without data/workflow loss
 *   - Competing Entrants: Secondary victims (moderate/constrained) — face fragmented protocol landscape; expensive to build compatible implementations or forced to license proprietary layers
 *   - Standards Body: Institutional actor (organized/constrained) — maintains nominal standard governance but lacks enforcement capacity against member companies; captures both coordination and extraction simultaneously
 *   - Interoperability Commons: Abstract victim (powerless/trapped) — transaction costs from fragmentation (debugging, duplicate development, innovation delays) accumulate across ecosystem
 *   - Regulatory Mandate (Legacy): Institutional constraint (institutional/arbitrage) — nominally requires interoperability but enforcement is theatrical; incumbents comply performatively
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(infrastructure_interoperability_decay, 0.52).
domain_priors:suppression_score(infrastructure_interoperability_decay, 0.65).
domain_priors:theater_ratio(infrastructure_interoperability_decay, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(infrastructure_interoperability_decay, extractiveness, 0.52).
narrative_ontology:constraint_metric(infrastructure_interoperability_decay, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(infrastructure_interoperability_decay, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(infrastructure_interoperability_decay, tangled_rope).
narrative_ontology:human_readable(infrastructure_interoperability_decay, "The Protocol Silo Trap").
narrative_ontology:topic_domain(infrastructure_interoperability_decay, "technological/economic").

domain_priors:requires_active_enforcement(infrastructure_interoperability_decay).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(infrastructure_interoperability_decay, incumbent_providers).
narrative_ontology:constraint_beneficiary(infrastructure_interoperability_decay, proprietary_platform_operators).
narrative_ontology:constraint_victim(infrastructure_interoperability_decay, downstream_users).
narrative_ontology:constraint_victim(infrastructure_interoperability_decay, competing_entrants).
narrative_ontology:constraint_victim(infrastructure_interoperability_decay, interoperability_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCKED-IN USER (SNARE) — End users initially adopt the standard for its universality and low switching cost. But as the protocol fragments into vendor-specific extensions and proprietary sub-layers, users become trapped. Data investments, workflow dependencies, and network effects create high exit costs. No alternative provides equivalent reach. Maximum experienced extraction — the user pays switching costs but receives no coordination benefit.
constraint_indexing:constraint_classification(infrastructure_interoperability_decay, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMPETING ENTRANT (SNARE) — New providers face a fragmented protocol landscape where true interoperability requires costly reverse-engineering or licensing fees. The standard's universal promise was the condition for their market entry; its decay traps them into proprietary sub-ecosystems. They experience high suppression (no clear alternative), constrained exits (expensive to rebuild), and extraction (must license or fork). Extraction is nearly as severe as for end users because their competitive position is defeated before launch.
constraint_indexing:constraint_classification(infrastructure_interoperability_decay, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT PROVIDER (ROPE) — Dominant firms benefit from fragmentation as a coordination mechanism: they establish proprietary extensions that lock users into their ecosystem while maintaining the appearance of standard compliance. The constraint appears as pure coordination to them — they are solving the 'efficiency' problem of proprietary optimization. Arbitrage exits (switch to proprietary-only if standards fail) make the constraint experience net-beneficial. Extraction flows toward them; they perceive coordination.
constraint_indexing:constraint_classification(infrastructure_interoperability_decay, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: STANDARDS BODY (TANGLED ROPE) — Organizations like W3C, IETF, or IEEE maintain the nominal standard while watching member companies insert proprietary extensions. The standards body provides genuine coordination (defines base protocol, reduces fragmentation risk), but also enforces a constraint through its governance structure: only incumbent members have influence over extension approval; new entrants cannot shape the standard's evolution. The body experiences extraction (limited funding, political capture by incumbents) and coordination (legitimacy, adoption reach) simultaneously. Active enforcement (governance voting, compliance certification) is mandatory.
constraint_indexing:constraint_classification(infrastructure_interoperability_decay, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: LEGACY INTEROPERABILITY MANDATE (PITON) — Early regulatory or contractual interoperability requirements (e.g., network neutrality, data portability rights, open standards mandates) persist but are largely theatrical. Incumbents have learned to comply nominally: they maintain 'open' interfaces while making proprietary layers essential. Theater ratio is high (0.48 baseline but higher in this perspective) — compliance certificates and standards logos are displayed while the actual architectural constraints trap users. The mandate has atrophied; it persists through institutional inertia, not enforcement capacity.
constraint_indexing:constraint_classification(infrastructure_interoperability_decay, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, the protocol silo trap is a hybrid mechanism: the standard provides genuine coordination function (common language reduces transaction costs), but that coordination is increasingly captured by incumbent providers who use standardization as a Trojan horse for lock-in. The constraint exhibits both coordination (base protocol interoperability) and asymmetric extraction (proprietary layers extract switching-cost surplus). Active enforcement (standards governance) maintains both functions simultaneously. This is the engine's canonical case for tangled rope classification.
constraint_indexing:constraint_classification(infrastructure_interoperability_decay, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(infrastructure_interoperability_decay_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(infrastructure_interoperability_decay, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(infrastructure_interoperability_decay, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(infrastructure_interoperability_decay, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(infrastructure_interoperability_decay, TR),
    TR >= 0.70.

:- end_tests(infrastructure_interoperability_decay_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, rising over interval. At t=0, the standard is newly universalized; extractiveness is low (0.15) because genuine interoperability reduces transaction costs for all parties and lock-in mechanisms are underdeveloped. By t=5, proprietary extensions proliferate; extractiveness rises (0.35) as users become dependent on vendor-specific features. By t=10, fragmentation is substantial; extractiveness reaches 0.52. Users and competitors bear switching costs while incumbents capture lock-in surplus. This is not maximum extraction (snare would require ε ≥ 0.46 and χ ≥ 0.66) because the base protocol retains functional value and alternative standards remain technically possible (high effort, but not impossible). Suppression (0.65): High. Users face significant barriers to exit: data investment, workflow dependencies, network effects (switching requires ecosystem migration). But suppression is not total because technically feasible alternatives exist and user coalitions could coordinate migration. Incumbent providers actively suppress alternatives through licensing restrictions and lock-in design. Theater ratio (0.48): Moderate, rising over interval. Early compliance is functional (vendors genuinely adhere to base standard). Over time, theater increases as incumbents maintain standards compliance marks while inserting proprietary layers. By t=10, theater reaches 0.48 because incumbents invest heavily in interoperability theater (certifications, public standards participation) while actual compatibility declines.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a sharp perspectival gap between beneficiaries and victims. The incumbent provider sees rope (pure coordination benefit, negligible extraction). The locked-in user sees snare (maximum extraction, no coordination benefit). The standards body sees tangled rope (both functions present, mixed experience). The competing entrant sees snare (barriers to entry, extraction of opportunity cost). The analytical observer sees the constraint's full hybrid structure and can detect the capture mechanism. The regulatory mandate sees its own degradation (piton perspective) — interoperability requirements persist nominally but lack enforcement teeth against incumbent compliance theater. No single perspective is 'correct'; the presheaf over the observation site reveals that the same structural mechanism produces radically different experiences depending on the agent's exit options and power level.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by each agent's structural position relative to the extraction flow. Beneficiaries with arbitrage exits (incumbent providers) derive low d (~0.10-0.20) because they can choose proprietary paths if standards fail, and standards currently benefit them. The standard appears as coordination to them. Trapped users with no viable alternatives derive high d (~0.85-0.95) because they have absorbed the switching costs of proprietary dependencies and cannot exit without severe loss. Constrained competitors derive moderate-high d (~0.70-0.80) because they have alternative paths (fork the standard, build proprietary ecosystems, lobby for open standards) but each path is expensive. The standards body with constrained exits and mixed beneficiary/victim status derives moderate d (~0.50-0.60) because it provides genuine coordination but also enforces the capture mechanism. The analytical observer derives analytical d (~0.72-0.75) because they can see the full structure but have no power to enforce change.
 *
 * MANDATROPHY ANALYSIS:
 *   The protocol silo trap resolves the mandatrophy by distinguishing between pure extraction (snare) and hybrid coordination-with-capture (tangled rope). The constraint is NOT a snare because the base protocol provides genuine coordination function — users benefit from the standard's existence even after fragmentation. Switching costs are high, but they are switching costs OUT of an imperfect but functional system, not switching costs within a purely extractive system (like debt bondage). The constraint is a tangled rope because: (1) the standard provides real coordination (base protocol remains common, reduces transaction costs), (2) it requires active enforcement (standards governance maintains the facade and polices member compliance), (3) it has asymmetric extraction (proprietary extensions lock in users while benefiting incumbents). The mandatrophy resolves by showing that incumbents have captured the coordination mechanism itself — they use standardization as a tool for lock-in. This is extraction-through-coordination, not pure extraction. Governance reform (entrant voting parity, public interest representation, stronger compliance enforcement) could restore the rope classification by preventing proprietary capture. The current state reflects incumbent institutional power (organized/institutional agents) and weak victim organization — competitors and users are individually powerless but collectively represent large economic value. If user coalitions or regulatory bodies organize to enforce true interoperability (forbidding vendor-specific extensions, mandating data portability, requiring open formats), the constraint could degrade from tangled rope to pure rope or collapse entirely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proprietary_layer_criticality,
    'At what threshold of proprietary extension adoption does the standard cease to be ''open'' in any meaningful sense?',
    'Measurement of data format transcoding costs, protocol version divergence, and user-facing incompatibilities across major implementations. Longitudinal tracking of how many users need proprietary extensions for core workflows.',
    'If threshold is low (20-30% proprietary): early detection enables regulatory intervention. If threshold is high (70%+): fragmentation becomes irreversible without major coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proprietary_layer_criticality, empirical, 'Proprietary extension adoption threshold for standard degradation').

omega_variable(
    user_lock_in_reversibility,
    'Can users realistically switch to interoperable alternatives once proprietary dependencies accumulate, or is lock-in permanent within a user generation?',
    'Historical case study of protocol migrations (HTTP → HTTPS, IPv4 → IPv6, SMS → RCS). Measurement of migration timelines, costs incurred, and adoption rates for competing standards.',
    'If reversible (< 5 years): snare classification may overstate permanence. If irreversible (> 15 years or permanent): snare is confirmed; user cohorts cannot escape during their economic lifetime.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(user_lock_in_reversibility, empirical, 'Whether user lock-in from protocol fragmentation is reversible').

omega_variable(
    standards_body_capture_mechanism,
    'Is the standards body''s inability to prevent proprietary extensions a structural limitation or a capture result from incumbent voting control?',
    'Analysis of standards body governance: voting power distribution by member type (incumbent vs entrant), approval rates for proprietary extension proposals, funding source concentration. Counterfactual: would a differently-governed body produce less fragmentation?',
    'If structural: governance reform cannot fix fragmentation. If capture: governance redesign (e.g., entrant voting parity, public interest seats) could restore coordination. Classification as tangled_rope vs pure extraction depends on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(standards_body_capture_mechanism, conceptual, 'Whether standards body fragmentation is structural or due to capture').

omega_variable(
    interoperability_commons_measurability,
    'Is ''interoperability commons'' a quantifiable victim, or is it an abstract collective good without measurable loss?',
    'Measurement of transaction costs imposed by fragmentation: debugging time for cross-implementation issues, duplicate development effort, innovation delays. Compare protocol with high fragmentation vs low fragmentation analogs.',
    'If measurable: commons appears in victim declarations and affects classification. If unmeasurable: abstract collective goods may be misclassified as beneficiaries when they are actually structural components of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interoperability_commons_measurability, empirical, 'Measurability of interoperability commons as a constraint victim').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(infrastructure_interoperability_decay, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(silo_tr_t0, infrastructure_interoperability_decay, theater_ratio, 0, 0.2).
narrative_ontology:measurement(silo_tr_t5, infrastructure_interoperability_decay, theater_ratio, 5, 0.35).
narrative_ontology:measurement(silo_tr_t10, infrastructure_interoperability_decay, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(silo_be_t0, infrastructure_interoperability_decay, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(silo_be_t5, infrastructure_interoperability_decay, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(silo_be_t10, infrastructure_interoperability_decay, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(infrastructure_interoperability_decay, information_standard).
narrative_ontology:affects_constraint(infrastructure_interoperability_decay, data_portability_mandates).
narrative_ontology:affects_constraint(infrastructure_interoperability_decay, network_effect_lock_in).
narrative_ontology:affects_constraint(infrastructure_interoperability_decay, standards_body_capture).

% DUAL FORMULATION NOTE:
% The protocol silo trap is a canonical case of coordination mechanism capture. It is upstream of specific data portability constraints (which arise as user coalitions attempt escape) and downstream of network effect dynamics (which create the initial efficiency incentive for standardization). The fragmentation mechanism itself is distinct from both parent constraints — it models the specific pathway from universal standard to proprietary lock-in, whereas network effects model the generic lock-in dynamic and data portability models the exit attempt.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(infrastructure_interoperability_decay, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
