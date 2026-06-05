% ============================================================================
% CONSTRAINT STORY: protocol_capture_tangled_rope
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_protocol_capture_tangled_rope, []).

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
 *   constraint_id: protocol_capture_tangled_rope
 *   human_readable: The Captured Commons (Embrace, Extend, Extinguish)
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The Embrace, Extend, Extinguish (EEE) pattern describes a strategy where
 *   a dominant technology provider adopts a decentralized or open protocol,
 *   initially maintains compatibility, then introduces proprietary extensions
 *   that fragment the ecosystem and create lock-in, effectively extinguishing
 *   the original open alternative. This constraint is structurally hybrid: it
 *   provides genuine coordination benefits (the open protocol reaches
 *   critical mass faster through the dominant provider's adoption) while
 *   simultaneously enabling asymmetric extraction (the dominant provider
 *   captures design authority over the protocol's evolution and forces users
 *   toward proprietary features). Historical examples include Microsoft's
 *   embrace of open standards followed by extension and incompatibility
 *   (OOXML, Kerberos extensions), Meta's adoption and then proprietarization
 *   of messaging protocols, and platform gatekeeping of interoperability
 *   standards. The constraint exhibits rising extractiveness over time (0.15
 *   → 0.58) as extensions accumulate and switching costs increase, while
 *   theater remains moderate throughout — the coordination benefit is real at
 *   the start, even as extraction mechanism strengthens.
 *
 * KEY AGENTS:
 *   - Dominant Platform Provider: Primary beneficiary (institutional/arbitrage) — captures user base through open-protocol adoption, then monetizes through proprietary extensions
 *   - Decentralized Protocol Community: Primary victim (powerless/trapped) — open contributors lose design authority as proprietary features become standard
 *   - Alternative Implementation Teams: Secondary victim (moderate/constrained) — resource-constrained, face ecosystem bifurcation and user migration pressure
 *   - Early Adopters of Extensions: Secondary beneficiary (organized/arbitrage) — benefit from first-mover advantage on proprietary features before lock-in hardens
 *   - Interoperability Standards Bodies: Organized observer (organized/constrained) — regulatory frameworks (DMA, DSA) increasingly pressure reversal of capture
 *   - Users: Distributed (varies) — initially benefit from ecosystem coordination, later face lock-in and reduced choice
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees pattern as recurring in technology markets, neither purely extractive nor purely coordinating
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(protocol_capture_tangled_rope, 0.58).
domain_priors:suppression_score(protocol_capture_tangled_rope, 0.65).
domain_priors:theater_ratio(protocol_capture_tangled_rope, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(protocol_capture_tangled_rope, extractiveness, 0.58).
narrative_ontology:constraint_metric(protocol_capture_tangled_rope, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(protocol_capture_tangled_rope, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(protocol_capture_tangled_rope, tangled_rope).
narrative_ontology:human_readable(protocol_capture_tangled_rope, "The Captured Commons (Embrace, Extend, Extinguish)").
narrative_ontology:topic_domain(protocol_capture_tangled_rope, "technological/economic").

domain_priors:requires_active_enforcement(protocol_capture_tangled_rope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(protocol_capture_tangled_rope, dominant_platform_provider).
narrative_ontology:constraint_beneficiary(protocol_capture_tangled_rope, early_adopters_of_extensions).
narrative_ontology:constraint_victim(protocol_capture_tangled_rope, decentralized_protocol_community).
narrative_ontology:constraint_victim(protocol_capture_tangled_rope, alternative_implementations).
narrative_ontology:constraint_victim(protocol_capture_tangled_rope, interoperability_ecosystem).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OPEN PROTOCOL COMMUNITY (SNARE) — Decentralized contributors to the original protocol face lock-in. The dominant provider's extensions become de facto standard; users migrate to proprietary features; the open alternative becomes marginalized. The community has no enforcement mechanism for the original specification and no exit option once ecosystem momentum shifts. Maximum extraction experienced — full colonization of the commons.
constraint_indexing:constraint_classification(protocol_capture_tangled_rope, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALTERNATIVE IMPLEMENTATION TEAMS (TANGLED ROPE) — Benefit from the open protocol's foundation and community trust, but face suppression as proprietary extensions fragment the ecosystem. Resources constrained by inability to match proprietary development velocity. Some benefit from coordination through open standards; significant extraction through ecosystem bifurcation and forced choice between compatibility and independence.
constraint_indexing:constraint_classification(protocol_capture_tangled_rope, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DOMINANT PLATFORM PROVIDER (ROPE) — Experiences the constraint as beneficial coordination. Embracing the open protocol captures user base and developer trust. Extending with proprietary features creates switching costs. The constraint solves the coordination problem of reaching critical mass without needing to build from zero. Net beneficiary — extraction mechanism runs toward this entity.
constraint_indexing:constraint_classification(protocol_capture_tangled_rope, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTEROPERABILITY STANDARDS BODIES (SCAFFOLD) — Organized actors (IETF, W3C, protocol foundations) see the capture as a coordination failure with a potential sunset: if regulatory frameworks mandate interoperability or if competing platforms adopt compatible extensions, the proprietary lock-in weakens. Standards enforcement through litigation or legislation could force convergence. Theater ratio low here — standards bodies' intervention is functional, not performative.
constraint_indexing:constraint_classification(protocol_capture_tangled_rope, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY COMPATIBILITY LAYERS (PITON) — As the proprietary extensions become dominant, backward-compatibility shims and translation layers persist to maintain connection to the original open protocol. These are largely performative — they allow the appearance of open-standards compliance while routing users toward proprietary features. Theater ratio high; functional extraction low because the original protocol is already captured.
constraint_indexing:constraint_classification(protocol_capture_tangled_rope, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational view, this is a recurring pattern in technology: embrace legitimizes through adoption; extend creates lock-in; extinguish removes the open alternative. The constraint exhibits genuine coordination (users benefit from reaching critical mass) and genuine extraction (beneficiary captures future design decisions). Both are structural features. The engine computes this as Tangled Rope across perspectives, confirming the hybrid nature.
constraint_indexing:constraint_classification(protocol_capture_tangled_rope, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(protocol_capture_tangled_rope_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(protocol_capture_tangled_rope, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(protocol_capture_tangled_rope, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(protocol_capture_tangled_rope, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(protocol_capture_tangled_rope, TR),
    TR >= 0.70.

:- end_tests(protocol_capture_tangled_rope_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The dominant provider captures significant value through ecosystem control and design authority over extension mechanisms, but the original open protocol remains functional — users retain some exit options through alternative implementations. The constraint is not maximal extraction (would be 0.80+) because the coordination benefit is real and the open alternative has not been fully eliminated (yet). The rising trajectory (0.15 → 0.58) reflects Goodhart drift: early in adoption, the constraint functions primarily as coordination; as proprietary extensions accumulate and become standard, extraction mechanisms strengthen. Suppression (0.65): Moderate-high. Alternative implementations face significant barriers: network effects favor the dominant provider's implementation, users are incentivized toward proprietary features, and interoperability standards lack enforcement mechanisms. However, suppression is not total — regulatory frameworks (DMA) are beginning to mandate interoperability, and open-protocol communities retain some advocacy power. Theater (0.48): Moderate. The coordination benefit of ecosystem adoption is genuine — the open protocol does reach critical mass faster through dominant-provider involvement. The extraction mechanism is also genuine — users experience real lock-in and switching costs. Neither is primarily performative, though compatibility shims may add theatrical elements later (piton perspective). The constraint is hybrid, not theatrical.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reflects who benefits from the coordination phase versus who bears the extraction cost. The dominant platform provider sees pure coordination (Rope) — they are solving the critical-mass problem. The open protocol community sees pure extraction (Snare) — their design authority is colonized with no exit option. Alternative implementations see the hybrid (Tangled Rope) — they both benefit from the ecosystem growth and suffer from the lock-in. Interoperability standards bodies see a solvable problem with a regulatory sunset (Scaffold) — legislation can force compatibility. Legacy compatibility layers are mostly performative (Piton) — they allow the appearance of openness after capture is complete. The analytical observer sees the pattern as structural to network-effect economics but contingent on market structure and regulation — whether this is inevitable (Mountain) or contestable (Tangled Rope) depends on institutional response. The perspectives cluster around Tangled Rope and Snare, confirming the hybrid constraint classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each agent is derived from their structural relationship to the extraction flow: (1) Dominant provider: beneficiary + arbitrage exit → low d → low/negative χ. They experience the constraint as beneficial. (2) Open protocol community: victim + trapped exit → high d → high χ. They bear full extraction cost with no alternatives. (3) Alternative implementations: mixed (victim + constrained exit) → moderate d → moderate χ. They have some options but face significant barriers. (4) Standards bodies: organized observer + constrained exit → moderate d. Regulatory mandate can shift the constraint, but implementation takes time. (5) Users: distributed — some beneficiaries (early adopters of extensions), some victims (locked-in users). The sigmoid f(d) applied to these d values produces the perspectival χ spread. The engine derives d automatically from beneficiary/victim declarations and exit options; the structural data here maps clearly to observed user behavior and firm strategy.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extension_incompatibility_threshold,
    'At what percentage divergence between proprietary extensions and open standard do users experience lock-in as irreversible?',
    'Empirical measurement of user migration rates, switching costs, and platform-specific feature adoption curves; comparison across protocol families (XMPP, ActivityPub, SMTP extensibility)',
    'If threshold < 15% divergence: lock-in occurs quickly (Snare dominates). If threshold > 40%: users retain meaningful exit options longer (Tangled Rope persists).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extension_incompatibility_threshold, empirical, 'Threshold for irreversible lock-in from proprietary extensions').

omega_variable(
    regulatory_interoperability_mandate,
    'Do legislative interoperability mandates (e.g., Digital Markets Act, DMA) actually constrain proprietary extension capture, or do they merely shift the capture to regulated compliance layers?',
    'Longitudinal analysis of platforms under interoperability mandate; measurement of lock-in metrics pre/post regulation; examination of whether compliance becomes performative theater',
    'If effective constraint: Scaffold sunset is real — regulation forces de-capture. If merely theatrical: constraint persists as Piton — compliance is formal, capture continues underneath.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_interoperability_mandate, empirical, 'Effectiveness of regulatory mandates against protocol capture').

omega_variable(
    alternative_protocol_viability,
    'Can decentralized alternatives (ActivityPub, Nostr, protocol coalitions) establish sufficient network effects to resist capture, or is capture an inevitable outcome of critical-mass dynamics?',
    'Game-theoretic analysis of network effect equilibria; empirical tracking of alternative protocol adoption and consolidation patterns; comparison of scenarios with and without regulatory barriers to dominant-firm extension',
    'If alternatives viable: the constraint is contingent on market structure (Rope or Scaffold depending on institutional factors). If capture is inevitable: the constraint is structural to network-effect economics (Mountain or universal Snare).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_protocol_viability, conceptual, 'Whether network effects make capture inevitable or contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(protocol_capture_tangled_rope, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(protoee_tr_t0, protocol_capture_tangled_rope, theater_ratio, 0, 0.2).
narrative_ontology:measurement(protoee_tr_t3, protocol_capture_tangled_rope, theater_ratio, 3, 0.35).
narrative_ontology:measurement(protoee_tr_t6, protocol_capture_tangled_rope, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(protoee_be_t0, protocol_capture_tangled_rope, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(protoee_be_t3, protocol_capture_tangled_rope, base_extractiveness, 3, 0.32).
narrative_ontology:measurement(protoee_be_t6, protocol_capture_tangled_rope, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(protocol_capture_tangled_rope, information_standard).
narrative_ontology:boltzmann_floor_override(protocol_capture_tangled_rope, 0.35).
narrative_ontology:affects_constraint(protocol_capture_tangled_rope, network_effect_lock_in).
narrative_ontology:affects_constraint(protocol_capture_tangled_rope, interoperability_mandate_enforcement).
narrative_ontology:affects_constraint(protocol_capture_tangled_rope, open_source_sustainability).

% DUAL FORMULATION NOTE:
% The EEE constraint is downstream of general network-effect dynamics but represents a distinct extractive strategy. The upstream constraint (network effect lock-in) is structural to technology markets; the EEE constraint shows how strategic action captures and weaponizes that structural feature. Regulatory interoperability mandates attempt to reverse capture by forcing compatibility, making the constraint contestable at the institutional level.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(protocol_capture_tangled_rope, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
