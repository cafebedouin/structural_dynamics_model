% ============================================================================
% CONSTRAINT STORY: us_security_guarantee_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_security_guarantee_dependency, []).

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
 *   constraint_id: us_security_guarantee_dependency
 *   human_readable: US Security Guarantee Dependency
 *   domain: international_relations/geopolitics/defense
 *
 * SUMMARY:
 *   The US security guarantee system creates a structural dependency that
 *   binds allied and dependent states into asymmetric strategic
 *   relationships. The constraint exhibits genuine coordination functions
 *   (regional stability, deterrence against peer aggression, enabling trade
 *   and investment) alongside asymmetric extraction (subordination of foreign
 *   policy, military subordination, fiscal burden of interoperability and
 *   procurement, loss of strategic autonomy). The dependency locks states
 *   into the arrangement through multiple binding mechanisms: the original
 *   threat that motivated the guarantee persists or is perceived as
 *   persisting; alternative security arrangements are technically difficult
 *   or strategically foreclosed; the institutional machinery (alliance
 *   structures, military integration, procurement dependencies) accumulates
 *   switching costs; and elite identity becomes fused with alliance
 *   membership. The theater ratio (0.58) reflects that increasingly, the
 *   guarantee is maintained through performative commitment ceremonies and
 *   rhetorical affirmation rather than active functional response to current
 *   threats. The extractiveness trend (0.35→0.52 over 30 years) indicates
 *   growing rent-seeking layered onto the original coordination function:
 *   expanded procurement requirements, extended basing rights, acceleration
 *   of foreign policy alignment demands, and increased US ability to
 *   condition assistance on political compliance.
 *
 * KEY AGENTS:
 *   - US Strategic Establishment: Primary beneficiary (institutional/arbitrage) — captures influence, forward basing, alliance bloc maintenance, procurement markets. Exit options abundant (can dissolve guarantees with minimal cost).
 *   - Dependent State Leadership: Primary victim (powerless/trapped) — locked into subordination by perceived existential threat and accumulated institutional dependencies. Exit costs are existential (military vulnerability); no credible alternatives apparent.
 *   - Dependent State Civil Society: Secondary victim (moderate/constrained) — bears fiscal costs (defense spending), foreign policy constraints, military casualties in US-aligned conflicts. Moderate exit costs (economic disruption) alongside genuine security benefits.
 *   - Regional Peer State: Tertiary actor (organized/constrained) — constrained by dependent state's US guarantee but also benefits from mutual deterrence credibility. Faces costs of reduced influence in dependent state's region.
 *   - Alliance Institutional Apparatus: Secondary institutional actor (institutional/arbitrage) — military commands, procurement bureaucracies, intelligence partnerships. Maintains itself through inertia and functional coordination roles.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks treating contingent institutional arrangements (alliance architecture, weapons technology, regional threat perception) as immutable laws of international anarchy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_security_guarantee_dependency, 0.52).
domain_priors:suppression_score(us_security_guarantee_dependency, 0.68).
domain_priors:theater_ratio(us_security_guarantee_dependency, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_security_guarantee_dependency, extractiveness, 0.52).
narrative_ontology:constraint_metric(us_security_guarantee_dependency, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(us_security_guarantee_dependency, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_security_guarantee_dependency, tangled_rope).
narrative_ontology:human_readable(us_security_guarantee_dependency, "US Security Guarantee Dependency").
narrative_ontology:topic_domain(us_security_guarantee_dependency, "international_relations/geopolitics/defense").

domain_priors:requires_active_enforcement(us_security_guarantee_dependency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_security_guarantee_dependency, us_military_industrial_complex).
narrative_ontology:constraint_beneficiary(us_security_guarantee_dependency, us_strategic_influence).
narrative_ontology:constraint_beneficiary(us_security_guarantee_dependency, allied_state_security_apparatus).
narrative_ontology:constraint_victim(us_security_guarantee_dependency, dependent_state_autonomy).
narrative_ontology:constraint_victim(us_security_guarantee_dependency, dependent_state_fiscal_capacity).
narrative_ontology:constraint_victim(us_security_guarantee_dependency, alternative_security_arrangements).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT STATE LEADERSHIP (SNARE) — Faces structural trap: defending against regional threats requires military capability that domestic economy cannot sustain. US guarantee appears as only exit from insecurity spiral, but accepting it locks the state into strategic subordination. Exit costs are existential (military vulnerability); staying costs include fiscal drain, foreign policy subordination, and loss of strategic autonomy. No genuine alternative.
constraint_indexing:constraint_classification(us_security_guarantee_dependency, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: DEPENDENT STATE CIVIL SOCIETY (TANGLED ROPE) — Genuine coordination benefit exists: the guarantee enables trade, investment flows, and regional stability that would be impossible under threat of invasion. But the constraint includes asymmetric extraction: dependent state must align foreign policy with US interests, accept US military presence, subordinate regional diplomacy to US strategy. Both coordination and extraction are real.
constraint_indexing:constraint_classification(us_security_guarantee_dependency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: US STRATEGIC ESTABLISHMENT (ROPE) — Experiences the guarantee as coordination: maintains alliance bloc, extends influence, secures forward bases and intelligence access, prevents regional realignment toward rival powers. Extraction flows toward the US; the dependent state subsidizes US presence through basing rights, procurement of US weapons, and foreign policy alignment. The US perceives this as legitimate coordination cost.
constraint_indexing:constraint_classification(us_security_guarantee_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGIONAL PEER STATE (TANGLED ROPE) — Constrained by the dependent state's US guarantee but also benefits from mutual deterrence stability and negotiating partner capacity. The guarantee creates genuine coordination (reduced risk of destabilizing conflict) but also enforces asymmetry: the regional peer cannot expand influence in the dependent state's territory and faces credible US escalation if it acts aggressively. Mixed extraction and coordination.
constraint_indexing:constraint_classification(us_security_guarantee_dependency, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: ALLIANCE ARCHITECTURE INSTITUTION (PITON) — The formal structure of bilateral defense treaties, NATO articles, and security commitments persists through institutional inertia long after the original strategic rationale has eroded. Theater ratio high: much of the activity around alliance maintenance is performative (summit ceremonies, treaty affirmations, rhetoric of commitment) rather than functional response to current threats. The institution sees itself as degraded but maintains itself because alternatives haven't fully replaced it.
constraint_indexing:constraint_classification(us_security_guarantee_dependency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / STRUCTURAL REALISM (MOUNTAIN) — From civilizational scale, the security guarantee dependency appears as an immutable feature of international anarchy: weaker states rationally depend on stronger patrons for survival; the patron rationally uses this dependency for influence; the structure is self-reinforcing through fundamental power asymmetry. However, the base properties reveal this as false naturalization: the dependency is contingent on specific institutional arrangements, weapons technology, regional threat perception, and domestic state capacity — all mutable.
constraint_indexing:constraint_classification(us_security_guarantee_dependency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_security_guarantee_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_security_guarantee_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_security_guarantee_dependency, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_security_guarantee_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_security_guarantee_dependency, TR),
    TR >= 0.70.

:- end_tests(us_security_guarantee_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The US captures genuine strategic benefits (influence, forward basing, alliance bloc maintenance, procurement markets) that flow directly to the dependent state's cost. The value reflects that extraction is substantial but not maximized — the dependent state retains some negotiating leverage, and genuine coordination benefits exist (regional stability, deterrence). The upward trend from 0.35 to 0.52 indicates accumulating rent-seeking on top of coordination function. Suppression (0.68): High. Multiple barriers lock states into dependency: (1) material barriers — regional threats are real or credible, alternative security arrangements are technically difficult; (2) institutional barriers — military integration, procurement lock-in, alliance bureaucracies with vested interests; (3) cognitive barriers — elite identity fusion with alliance membership, internalization of threat narratives; (4) structural barriers — switching costs from decades of integration. Theater ratio (0.58): Moderate. Significant performative content exists — alliance summits, treaty affirmations, rhetorical commitment ceremonies that signal continuity rather than respond to actual current threats. But the guarantee retains functional verification — actual force positioning, military coordination, deterrent signaling. The upward trend from 0.38 to 0.58 indicates growing theater as original Cold War rationale erodes and newer justifications become more rhetorical.
 *
 * PERSPECTIVAL GAP:
 *   The dependent state perceives snare (no exit, maximum extraction) while the US perceives rope (genuine coordination, beneficial arrangement). The dependent state's civil society perceives tangled rope (real security benefits mixed with real costs and subordination), while the strategic establishment perceives snare (they experience extraction more acutely than average citizens who benefit from regional stability). The regional peer perceives tangled rope (constrained by the guarantee, but also stabilized by it). The alliance institutional apparatus perceives piton (their formal function persists but increasingly through inertia and performative affirmation rather than active response to threats). The analytical observer risks perceiving mountain (anarchy and power asymmetry appear immutable) but the structural data reveals false naturalization. The perspectival gap reveals that the same constraint is experienced as immutable trap, mixed coordination-extraction, pure coordination, degraded ritual, and natural law depending on the observer's structural position and temporal horizon.
 *
 * DIRECTIONALITY LOGIC:
 *   The US institutional power derives from abundance of exit options (arbitrage) — the US can unilaterally dissolve guarantees, reposition forces, or shift alliance priorities with manageable costs. This produces d ≈ 0.15 (beneficiary + arbitrage exit = low extraction experienced by the US). The dependent state's powerless position combines with trapped exit options (material or cognitive inability to exit) producing d ≈ 0.92 (victim + trapped exit = maximum extraction). The regional peer's organized power and constrained exit (can act but at high cost) produces d ≈ 0.55 (victim + constrained exit = moderate extraction). The piton classification derives from theater ratio exceeding 0.55 — the alliance maintenance apparatus is substantially performative. The mountain classification at the analytical level is perspectival false naturalization: realism theory treats anarchy and power asymmetry as immutable, but the specific institutional forms (bilateral guarantees, NATO structure, procurement dependencies) are contingent and mutable.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy collapse by showing that coordination and extraction are genuinely both present and structurally necessary to the arrangement. The coordination benefit is real: the dependent state's regional stability, access to advanced weapons, and deterrence credibility would be impossible without the guarantee. But the extraction is equally real: foreign policy subordination, fiscal burden, military casualties in US-aligned conflicts, loss of strategic autonomy. The tangled rope type is the correct classification because both mechanisms are structural, not because coordination and extraction happen to coexist. The snare perspective from the dependent state reveals that the extraction is experienced as paramount — the coordination benefits are overshadowed by the lock-in mechanism. The rope perspective from the US reveals that extraction and coordination are experienced as aligned — the US genuinely coordinates while extracting. The scaffold perspective (not yet emergent but potentially available through non-aligned security arrangements, regional defense cooperation, or nuclear deterrent development) would resolve the constraint by replacing the guarantee with alternative arrangements that retain coordination benefits at lower extraction cost. The piton perspective shows that the institutional form is degrading — the theater ratio is rising, suggesting that the alliance is being maintained increasingly through inertia rather than functional response.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threat_perception_asymmetry,
    'Is the dependent state''s perception of existential threat empirically justified, or is it institutionalized through security establishment narratives?',
    'Historical analysis of actual vs claimed threats; comparison of military capabilities to plausible invasion scenarios; assessment of whether diplomatic alternatives were genuinely foreclosed or merely rejected by security elites',
    'If threat is overstated: dependency is partly identity_locked (elite capture of threat narrative) rather than trapped (material barriers). If threat is genuine: classification shifts toward mountain (natural response to anarchy). If threat is real but diplomacy works: scaffold classification more appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_perception_asymmetry, empirical, 'Whether dependent state threat perception is justified or institutionalized').

omega_variable(
    alternative_security_arrangements_feasibility,
    'Could the dependent state achieve equivalent security through alternative mechanisms: regional collective defense, nuclear deterrent, non-alignment with security guarantees from multiple powers?',
    'Comparative case analysis: states that exited US guarantees and their security outcomes; modeling of alternative arrangements against regional threat scenarios; assessment of technological and diplomatic feasibility',
    'If alternatives are structurally viable: exit is constrained rather than trapped; extraction mechanism is weaker; constraint may classify as scaffold with sunset. If alternatives are foreclosed by power asymmetry: trap is real; snare classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_security_arrangements_feasibility, empirical, 'Feasibility of alternative security arrangements').

omega_variable(
    us_commitment_credibility,
    'Is the US security guarantee credible, or is it a theater mechanism that would be abandoned if actual great-power war threatened US territory?',
    'Analysis of US strategic doctrine; comparison of nuclear escalation scenarios vs US commitment statements; historical cases of US alliance abandonment or failure to honor commitments under pressure',
    'If guarantee is credible: coordination benefit is real, suppression partly justified. If guarantee is theater: suppression is not mitigated by actual security benefit; extraction mechanism is clearer; snare classification more appropriate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(us_commitment_credibility, preference, 'Credibility of US security commitment under actual great-power war').

omega_variable(
    identity_lock_vs_trapped_mechanism,
    'For dependent state elites, is the dependency lock structural (material barriers to exiting) or cognitive (identity fusion with alliance membership, internalization of threat narrative)?',
    'Elite discourse analysis; comparison of publicly stated rationales vs private assessments; assessment of whether questioning the guarantee is treated as logical analysis or identity betrayal; exit costs for elites who propose alternatives',
    'If structural trap: powerless/trapped classification correct. If identity_locked: exit is not materially impossible but appears unthinkable from within the elite''s identity frame; classification shifts toward rope (agent perceives constraint as changeable but cannot act on that perception).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_trapped_mechanism, conceptual, 'Whether dependent state lock is structural trap or cognitive identity fusion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_security_guarantee_dependency, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ussgd_tr_t0, us_security_guarantee_dependency, theater_ratio, 0, 0.38).
narrative_ontology:measurement(ussgd_tr_t15, us_security_guarantee_dependency, theater_ratio, 15, 0.48).
narrative_ontology:measurement(ussgd_tr_t30, us_security_guarantee_dependency, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(ussgd_be_t0, us_security_guarantee_dependency, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ussgd_be_t15, us_security_guarantee_dependency, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(ussgd_be_t30, us_security_guarantee_dependency, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_security_guarantee_dependency, enforcement_mechanism).
narrative_ontology:affects_constraint(us_security_guarantee_dependency, nato_expansion_pressure).
narrative_ontology:affects_constraint(us_security_guarantee_dependency, allied_military_procurement_lock_in).
narrative_ontology:affects_constraint(us_security_guarantee_dependency, us_forward_basing_leverage).
narrative_ontology:affects_constraint(us_security_guarantee_dependency, dependent_state_foreign_policy_subordination).

% DUAL FORMULATION NOTE:
% The US security guarantee dependency is upstream of several more specific institutional constraints involving procurement, military integration, and foreign policy alignment. Each downstream constraint has its own extractiveness value reflecting domain-specific mechanisms; the guarantee itself represents the overarching structural dependency that enables all downstream extractions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_security_guarantee_dependency, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
