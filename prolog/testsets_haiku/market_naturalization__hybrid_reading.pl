% ============================================================================
% CONSTRAINT STORY: market_naturalization__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_naturalization__hybrid_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
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
 *   constraint_id: market_naturalization__hybrid_reading
 *   human_readable: Market Dominance as Hybrid Constraint: Lapsed Alternatives + Active Suppression
 *   domain: political_economy/institutional_analysis
 *
 * SUMMARY:
 *   This constraint is ONE reading of a contested kernel about how market
 *   dominance persists. The kernel asks: 'Is market dominance a naturally
 *   lapsed alternative (requiring no defense), an actively maintained
 *   monopoly (requiring continuous suppression), or a hybrid of both (some
 *   alternatives lapsed, others suppressed)?' This story instantiates the
 *   hybrid reading: market dominance combines structural closure (knowledge
 *   atrophied, institutions disappeared, trust networks dissolved) with
 *   active maintenance (selective enforcement, regulatory capture, predatory
 *   pricing on specific alternatives). The hybrid reading claims moderate
 *   extractiveness (0.58) because the constraint operates partly through
 *   lapse (passive structural closure) and partly through active suppression.
 *   The alternative readings would classify this constraint as either
 *   mountain-like (lapsed, no active defense needed) or snare-like (actively
 *   defended extraction). This reading sits between: it is a Tangled Rope
 *   with genuine coordination benefits that have become decoupled from the
 *   persistent extraction, where incumbents selectively defend profitability
 *   while allowing some alternatives to disappear through inertia.
 *
 * KEY AGENTS:
 *   - incumbent_capital_holders: institutional power, arbitrage exits, controls agenda — directly benefits from constraint
 *   - potential_market_entrants: powerless, identity-locked exits, face both structural barriers (lapsed knowledge) and active barriers (capital rationing)
 *   - labor_in_subordinate_positions: organized, constrained exits, resistance is real but constrained by absence of alternative production pathways
 *   - peripheral_producers: moderate power, constrained exits, receive coordination benefits but at extractive terms
 *   - cultural_memory_institutions: excluded from discourse, would preserve knowledge that alternatives existed
 *   - competition_authorities: analytical observers, struggle to distinguish active suppression from structural lapse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__hybrid_reading, 0.58).
domain_priors:suppression_score(market_naturalization__hybrid_reading, 0.52).
domain_priors:theater_ratio(market_naturalization__hybrid_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, accessibility_collapse, 0.63).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(market_naturalization__hybrid_reading, "Market Dominance as Hybrid Constraint: Lapsed Alternatives + Active Suppression").
narrative_ontology:topic_domain(market_naturalization__hybrid_reading, "political_economy/institutional_analysis").

domain_priors:requires_active_enforcement(market_naturalization__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__hybrid_reading, 'bd70a8a9-a20f-4859-aadb-b9754ccd474d').
narrative_ontology:cs_kernel_codification('bd70a8a9-a20f-4859-aadb-b9754ccd474d', implicit).
narrative_ontology:cs_authority_grounding('bd70a8a9-a20f-4859-aadb-b9754ccd474d', practice).
narrative_ontology:cs_interpretation_layer_present('bd70a8a9-a20f-4859-aadb-b9754ccd474d').
narrative_ontology:cs_reading_relation('bd70a8a9-a20f-4859-aadb-b9754ccd474d', market_naturalization__lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_reading_relation('bd70a8a9-a20f-4859-aadb-b9754ccd474d', market_naturalization__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_axiom('bd70a8a9-a20f-4859-aadb-b9754ccd474d', foundational, market_dominance_dual_source).
narrative_ontology:cs_axiom_status(market_dominance_dual_source, holdable).
narrative_ontology:cs_axiom_grounding('bd70a8a9-a20f-4859-aadb-b9754ccd474d', market_dominance_dual_source, empirically_contingent).
narrative_ontology:cs_axiom('bd70a8a9-a20f-4859-aadb-b9754ccd474d', foundational, coordination_extraction_decoupling).
narrative_ontology:cs_axiom_status(coordination_extraction_decoupling, holdable).
narrative_ontology:cs_axiom_grounding('bd70a8a9-a20f-4859-aadb-b9754ccd474d', coordination_extraction_decoupling, empirically_contingent).
narrative_ontology:cs_reference_frame('bd70a8a9-a20f-4859-aadb-b9754ccd474d', market_coordination_dual_necessity).
narrative_ontology:cs_drift_state('bd70a8a9-a20f-4859-aadb-b9754ccd474d', contemporary_resurrection_pressure, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('bd70a8a9-a20f-4859-aadb-b9754ccd474d', '').
narrative_ontology:cs_kernel_id(market_naturalization__hybrid_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, incumbent_capital_holders).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, potential_market_entrants).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, labor_in_subordinate_positions).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, peripheral_producers).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__hybrid_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(market_naturalization__hybrid_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_naturalization__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_naturalization__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(market_naturalization__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 to 0.58 over the interval, indicating that while the constraint persists, the extractive component is increasing relative to the coordination component (the theater_ratio rises from 0.28 to 0.41, suggesting performative defense of coordination increases while actual coordination function may be declining). Suppression requirement rises from 0.38 to 0.52, indicating incumbents must invest more in active enforcement over time — this is diagnostic: if the constraint were purely lapsed (no active defense needed), suppression would decline as the lapsed closure self-perpetuates. Instead, suppression rises, showing incumbents are actively defending against resurgent alternatives or deteriorating coordination legitimacy. Accessibility collapse (0.63) is moderate-high: some alternatives have disappeared completely (cooperative banking, craft apprenticeship, federated production) making them inaccessible; others remain possible but actively defended against (alternative supply networks, worker ownership). Resistance (0.48) is moderate: organized labor mounts real resistance; entrants push against barriers; but the resistance is constrained by the structural closure that makes alternatives hard to conceive of, let alone organize toward.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (incumbent capital holders) experiences this constraint as natural market dominance they maintain through legitimate competitive advantage and selective regulation — they see active enforcement only against truly anti-competitive actors, not against legitimate alternatives. Potential entrants and subordinate labor experience it as a system where legitimate alternatives (cooperative ownership, federated supply) are structurally impossible (they lack knowledge and financing) or actively suppressed (regulatory barriers, predatory pricing). Peripheral producers experience genuine coordination benefits (access to capital, distribution, quality systems) but at extractive terms — they experience the constraint as partly coordination and partly extraction. This reading shows how these incompatible experiences are both structurally real: the coordination function exists and is valuable; the suppression of alternatives is also real and selective.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent capital holders: d ≈ 0.1 (full beneficiaries). They collect the rents directly, control the rules, have maximal exit options through arbitrage. Potential entrants: d ≈ 0.85 (near full targets). Trapped by both structural barriers (lapsed knowledge, no capital sources) and active barriers (regulatory capture, licensing tied to incumbent relationships). Identity-locked because professional legitimacy is mediated through incumbent institutions — they must become 'legitimate entrepreneurs' via incumbent pathways or face delegitimation. Labor: d ≈ 0.75. Organized but constrained by the absence of alternative production pathways. Resistance is real but cannot exit entirely because the alternatives that would enable exit have atrophied. Peripheral producers: d ≈ 0.55 (near symmetric). They genuinely benefit from the coordination function (capital access, distribution) but bear extractive costs (inflated input prices, margin compression). Without overrides, the derivation chain (beneficiary/victim + exit) would place them closer to d=0.3 (beneficiaries with constrained exit). The story's structural claim is that they sit near symmetric because the coordination they benefit from is inseparable from the extraction they bear — no override needed.
 *
 * MANDATROPHY ANALYSIS:
 *   This hybrid reading avoids a key falsification trap. If the constraint were purely lapsed (reading: lapsed_alternative_reading), we would expect suppression_requirement to decline over time as the lapsed closure self-perpetuates — no active defense needed. Instead, suppression_requirement rises (0.38 → 0.52), indicating incumbents are investing MORE in enforcement over the interval, not less. This contradicts pure-lapse and supports the hybrid reading: as alternatives begin to re-emerge (consciousness of alternatives spreading, new financing mechanisms developing, regulatory challenges mounting), incumbents must increase active suppression to maintain dominance. The theater_ratio rising (0.28 → 0.41) shows another mandatrophy signal: as the founding coordination problem becomes increasingly solved (supply chains mature, information asymmetries decline with digitalization), the constraint's legitimacy claim shifts from 'we solve a real problem' to 'we are the natural order.' This is classical theater escalation: the real function declines, the performed function increases. The hybrid reading captures this asymmetry: the constraint is real (it does provide coordination), but the persistence is increasingly about defending extraction against emerging alternatives rather than solving the original coordination problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lapsed_vs_suppressed_boundary,
    'Which specific alternatives are structurally lapsed (knowledge atrophied, institutions dissolved, no actors remain to reconstruct them) versus actively suppressed (knowledge exists, actors want to pursue them, but face incumbent enforcement)? The boundary is blurred: some alternatives may be 80% lapsed and 20% suppressed; others the reverse.',
    'Sectoral historical analysis: document, for each major market domain, which alternative institutional forms existed historically, which disappeared and when, and why (active suppression v. market displacement v. knowledge loss). Comparative study of domains where alternatives persist (cooperative banking in some countries, cooperative agriculture, worker ownership in some sectors) versus domains where they have nearly vanished (industrial agriculture, consumer technology platforms) to identify what sustained alternatives through the dominance period.',
    'If the lapsed component is large (>70%), the constraint behaves more like a mountain: barriers are structural, not maintained; fixing would require institutional reconstruction, not just enforcement removal. If the suppressed component is large (>60%), the constraint behaves more like a snare: removing active enforcement would restore alternatives quickly. This reading assumes roughly balanced (40–60% / 40–60%), which positions type at tangled_rope; the boundary assessment would move type prediction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lapsed_vs_suppressed_boundary, empirical, 'The proportion of market dominance persistence attributable to structural lapse versus active suppression.').

omega_variable(
    identity_fusion_in_subordinate_seats,
    'For labor and potential entrants classified as identity_locked, what is the mechanism of identity fusion? Is it professional socialization (legitimate entrepreneurship mediated through incumbent pathways, making alternative ownership structures seem illegitimate)? Relational identity (career identity constituted through the relationship to incumbent employers/capital sources)? Internalized ideology (belief that incumbent-mediated structure is the natural or only viable way)?',
    'Interview and ethnographic study of labor in subordinate positions and potential entrants who attempted to exit via alternative pathways (cooperatives, informal production, federated networks): map the specific barriers they encountered (structural knowledge loss, financing collapse) against internalized beliefs (''alternatives don''t work,'' ''this is just how markets are''). Examine educational curricula and professional credentialing to assess whether alternative institutional forms are taught or presented as legitimate.',
    'If identity fusion is primarily internalized ideology (easily reversible through education and exposure to alternatives), suppression_requirement could decline rapidly if alternatives re-emerged; the constraint would be vulnerable to consciousness shifts. If fusion is relational (career paths locked into incumbent institutions), recovery would be slower; removing active suppression wouldn''t immediately restore exit options because the career infrastructure for alternatives would need rebuilding. If fusion is socialization (legitimacy beliefs), intermediate: changed institutional examples would gradually shift what counted as legitimate, but generational turnover would be required.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_fusion_in_subordinate_seats, empirical, 'The mechanism and reversibility of identity fusion in subordinate and target seats.').

omega_variable(
    reading_boundary_clarity,
    'Does the hybrid reading''s claim (some lapsed, some suppressed, mixed beneficiary structure) remain empirically distinguishable from the sibling readings at the data resolution available? Or does the measurement ambiguity at sector/time-period level collapse the three readings into one indeterminate structure?',
    'Temporal disaggregation: measure suppression_requirement, theater_ratio, and resistance separately for alternative institutional forms known to have been suppressed versus those known to have lapsed. If suppressed alternatives show high suppression and rising theater_ratio while lapsed alternatives show low enforcement and stable theater, the readings remain distinct. If both show identical patterns, the readings are observationally equivalent at this resolution.',
    'If readings remain distinct, corpus builders can separate the kernel contest into three analytically pure stories. If they collapse into indeterminacy, the kernel itself requires refinement: the distinction between lapsed and suppressed may not be a property of the constraint but a labeling choice. The hybrid reading would then be the only defensible account because it avoids choosing between indistinguishable alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_clarity, conceptual, 'Whether the three readings of the market_naturalization kernel are empirically distinguishable or observationally equivalent.').

omega_variable(
    reconstruction_cost_of_lapsed_alternatives,
    'For alternatives that have atrophied (knowledge networks, financing structures, legal frameworks), what is the cost and time horizon for reconstruction? Is it expensive but feasible (would require deliberate policy and institutional investment, 10–30 years) or prohibitively costly (would require fundamental rewiring of capital markets and legal systems, >50 years or impossible)?',
    'Policy analysis and institutional economic modeling: estimate the cost of reconstructing cooperative banking, federated supply networks, craft apprenticeship systems, and worker ownership frameworks in a domain where they have lapsed. Compare against costs of reconstruction in domains where they have survived in attenuated form. Use historical precedent from countries that maintained or rebuilt such structures (Mondragon cooperatives, Swiss apprenticeship system, Nordic cooperative banking) to establish baseline reconstruction costs.',
    'If reconstruction cost is low, the lapsed component is less structurally binding; alternatives could re-emerge quickly if suppression eased and reconstruction were attempted. If reconstruction cost is high, the lapsed component is sticky; the constraint persists partly through inertia that would be expensive to reverse. This affects the time horizon for mandatrophy resolution: if lapsed alternatives are cheap to rebuild, fixing the constraint requires enforcement removal + modest institutional investment. If they''re expensive to rebuild, fixing requires major policy and capital commitment, which changes the political feasibility landscape.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reconstruction_cost_of_lapsed_alternatives, empirical, 'The cost and feasibility of reconstructing atrophied alternative institutional forms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__hybrid_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_naturalization__hybrid_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(mark_tr_t5, market_naturalization__hybrid_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement(mark_tr_t10, market_naturalization__hybrid_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement(mark_tr_t15, market_naturalization__hybrid_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(mark_tr_t25, market_naturalization__hybrid_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(mark_tr_t35, market_naturalization__hybrid_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement(mark_tr_t50, market_naturalization__hybrid_reading, theater_ratio, 50, 0.41).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_naturalization__hybrid_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(mark_be_t5, market_naturalization__hybrid_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(mark_be_t10, market_naturalization__hybrid_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(mark_be_t15, market_naturalization__hybrid_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(mark_be_t25, market_naturalization__hybrid_reading, base_extractiveness, 25, 0.56).
narrative_ontology:measurement(mark_be_t35, market_naturalization__hybrid_reading, base_extractiveness, 35, 0.59).
narrative_ontology:measurement(mark_be_t50, market_naturalization__hybrid_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_naturalization__hybrid_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(mark_su_t5, market_naturalization__hybrid_reading, suppression_requirement, 5, 0.41).
narrative_ontology:measurement(mark_su_t10, market_naturalization__hybrid_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement(mark_su_t15, market_naturalization__hybrid_reading, suppression_requirement, 15, 0.47).
narrative_ontology:measurement(mark_su_t25, market_naturalization__hybrid_reading, suppression_requirement, 25, 0.51).
narrative_ontology:measurement(mark_su_t35, market_naturalization__hybrid_reading, suppression_requirement, 35, 0.53).
narrative_ontology:measurement(mark_su_t50, market_naturalization__hybrid_reading, suppression_requirement, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__hybrid_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_naturalization__hybrid_reading, 0.18).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, market_naturalization__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, market_naturalization__beneficiary_maintained_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel. The kernel 'market_naturalization' decomposes into three constraint stories, each representing a different theory of how market dominance persists: (1) lapsed_alternative_reading — market dominance is structurally self-perpetuating; alternatives have disappeared on merit and require no active suppression; (2) beneficiary_maintained_reading — market dominance is actively defended by incumbents; suppression is continuous and pervasive; (3) hybrid_reading (this story) — market dominance combines structural lapse (some alternatives atrophied) with active maintenance (some alternatives suppressed). Each reading instantiates a different constraint with different ε values, different beneficiary/victim structures, and different type predictions. All three are linked in network.affects_constraints as a constraint family. The ε values differ because the observables are different: the lapsed reading measures coordination cost as ε, treating suppression as negligible; the beneficiary-maintained reading measures rent extraction as ε, treating coordination as secondary; this hybrid reading measures the mixed observable (both coordination and extraction present, both lapse and suppression present) as ε ≈ 0.58 (between the other two). The three readings collectively test the kernel contest: if historical data shows suppression_requirement rising over time, the lapsed reading fails; if it shows suppression stable while alternatives re-emerge, the beneficiary-maintained reading fails. This reading's predictions match the observed data (suppression rising, theater rising, some alternatives atrophying while others are defended against) better than either sibling reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(market_naturalization__hybrid_reading, powerless, 0.85).
constraint_indexing:directionality_override(market_naturalization__hybrid_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
