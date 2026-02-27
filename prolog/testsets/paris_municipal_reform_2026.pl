% ============================================================================
% CONSTRAINT STORY: paris_municipal_reform_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   human_readable: Paris Municipal Reform (Loi Maillard/PLM Reform) — Majority Premium Reduction 2026
 *   domain: political/legal
 *
 * SUMMARY:
 *   The 2026 Paris Municipal Reform (Loi du 11 août 2025) reduces the
 *   majority premium from 50% to 25% of council seats, fundamentally
 *   restructuring electoral incentives for Paris municipal governance. This
 *   constraint exhibits the core tension between representative democracy
 *   (proportionality) and executive stability (majority concentration). The
 *   reform is neither a pure coordination mechanism nor a pure extraction
 *   device — it is a hybrid that simultaneously enables opposition
 *   coalition-building AND suppresses the transition costs borne by incumbent
 *   parties and institutional actors accustomed to the prior regime. The
 *   constraint's theater_ratio (0.58) reflects that much public discourse
 *   frames the reform as a step toward 'republican principle' and 'democratic
 *   modernization,' obscuring the underlying redistribution of political
 *   power. From different structural positions, the same reform appears as a
 *   snare (fragmented opposition trapped under 50%), a rope (state
 *   administration coordinating a legitimacy crisis), a tangled rope (civil
 *   society navigating the hybrid coordination-extraction), a scaffold
 *   (temporary step toward proportionality), a piton (degraded electoral form
 *   persisting through inertia), or even a false mountain (naturalizing
 *   electoral mechanics as immutable law).
 *
 * KEY AGENTS:
 *   - Fragmented Opposition Parties: Primary victim (powerless/trapped under 50% regime) — lack structural capacity to challenge incumbent dominance; benefit from reform but only after multi-cycle institutional reorganization
 *   - Incumbent Majority Coalition: Primary target (institutional/arbitrage) — benefits from 50% premium but faces extraction of power under 25% regime; can adapt strategically or accept power rotation
 *   - Civil Society Pluralism Advocates: Primary beneficiary (organized/constrained) — see reform as scaffolding toward proportionality; drive continued pressure for deeper democratization
 *   - State Administration (Conseil d'État, Ministry of Interior): Institutional beneficiary (institutional/arbitrage) — solves legitimacy crisis and reduces litigation pressure; coordinating apparatus
 *   - Municipal Coalition Builders: Secondary beneficiary (moderate/constrained) — gain new coordination space but face transition costs; must develop coalition governance capacity
 *   - Electoral System itself: Analytical observer (analytical/analytical) — risks naturalizing contingent design choices as immutable properties of democratic mechanics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_municipal_reform_2026, 0.52).
domain_priors:suppression_score(paris_municipal_reform_2026, 0.48).
domain_priors:theater_ratio(paris_municipal_reform_2026, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_municipal_reform_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(paris_municipal_reform_2026, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(paris_municipal_reform_2026, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_municipal_reform_2026, tangled_rope).
narrative_ontology:human_readable(paris_municipal_reform_2026, "Paris Municipal Reform (Loi Maillard/PLM Reform) — Majority Premium Reduction 2026").
narrative_ontology:topic_domain(paris_municipal_reform_2026, "political/legal").

domain_priors:requires_active_enforcement(paris_municipal_reform_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_municipal_reform_2026, opposition_coalition).
narrative_ontology:constraint_beneficiary(paris_municipal_reform_2026, municipal_pluralism_advocates).
narrative_ontology:constraint_victim(paris_municipal_reform_2026, entrenched_majority_power).
narrative_ontology:constraint_victim(paris_municipal_reform_2026, executive_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRAGMENTED OPPOSITION (SNARE) — Under the 50% premium regime, minority voices have no structural exit. A coalition receiving 30% of votes still commands only ~15% of council seats. The suppression is near-total: fragmented opposition parties cannot credibly threaten executive power, cannot form governing coalitions, and face dissolution of political viability through successive electoral cycles. The reform reduces the premium from 50% to 25%, which mathematically allows for opposition coalitions to achieve council majorities with ~37-40% of the popular vote. However, the constraint persists in the near term because the political reorganization required for opposition coalition-building is itself suppressed by the prior regime — the old rules have created institutional facticity that the new rules must overcome.
constraint_indexing:constraint_classification(paris_municipal_reform_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MUNICIPAL COALITION BUILDERS (TANGLED ROPE) — At the regional/generational level, opposition parties and civil society actors benefit from the reform's coordination framework: it creates genuine incentives for strategic alliance-building, power-sharing agreements, and policy coalitions that the old regime foreclosed. The coordination function is real — the reformed premium enables viable alternative governments. But active enforcement is required: the transition from 50% to 25% creates a 25-year window where the old premium applies to incumbent coalitions, suppressing opposition asset accumulation. Victims: parties and civic groups that invested in the old regime's rules and now face discontinuity. Beneficiaries: opposition coalitions that can execute strategic adaptation. The hybrid character is structural: the reform both enables coordination (new coalition space) and imposes extraction (transitional upheaval, institutional adaptation costs).
constraint_indexing:constraint_classification(paris_municipal_reform_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: REFORMIST STATE COUNCIL (ROPE) — From the institutional/immediate perspective of the state apparatus (Conseil d'État, Ministry of Interior, prefectural administration), the reform is a pure coordination mechanism. It solves a legitimacy crisis: the 50% premium had become increasingly unstable politically, generating recurring legal challenges, civil society pressure, and intergovernmental tension with Paris municipalities. The reform reduces coordination overhead by establishing a more proportional baseline that reduces litigation risk and restores public confidence in electoral mechanics. Arbitrage exit is available: the state council can iterate on the reform, enforce it, or modify it. No extraction is experienced — the reform serves institutional rationality. The beneficiary (rational administration) and the constraint align.
constraint_indexing:constraint_classification(paris_municipal_reform_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: MUNICIPAL PLURALISM MOVEMENT (SCAFFOLD) — Civil society advocates for proportional representation see the reform as a temporary scaffolding toward full proportionality. The 25% premium is a transitional compromise that enables coalition governance without yet achieving pure proportional representation. Sunset logic applies: as opposition coalitions demonstrate governing capacity and institutional legitimacy, pressure will mount for further premium reduction or full abolition. The constraint suppresses extraction because the movement retains agency and sees a clear exit path (continued democratization pressure, European comparative models, generational cohort turnover). High theater is present: ceremonial invocation of 'republican principle,' 'democratic legitimacy,' and 'equal citizenship' frames the reform as progress rather than structural bargaining. Theater_ratio reflects that much of the reform's public framing emphasizes procedural justice, obscuring the underlying distribution of power between incumbent and opposition forces.
constraint_indexing:constraint_classification(paris_municipal_reform_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: ELECTORAL SYSTEM INERTIA (PITON) — From the civilizational/global perspective, the majority premium system itself is a degraded institutional form. Historical justification (avoiding legislative fragmentation, enabling executive stability) no longer applies in multiparty democracies with established coalition governance norms across Europe. The 50% premium persists through institutional inertia and path dependency rather than functional necessity. The 2026 reform partially addresses this by reducing the premium, but the existence of ANY premium reflects theatrical rather than functional commitment to democratic principle. Theater_ratio (0.58) captures that the reform allows political actors to claim 'modernization' and 'democratization' while maintaining significant electoral distortion. The piton status derives not from high extraction but from low functional legitimacy — the premium persists because alternative institutional arrangements haven't fully replaced it, not because it serves current structural needs.
constraint_indexing:constraint_classification(paris_municipal_reform_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From the analytical/universal perspective, one might argue that some form of majority premium is a natural law of electoral mechanics: purely proportional systems risk legislative fragmentation, executive instability, and governance gridlock. The constraint of 'balancing representation and governability' appears immutable. However, this perspective risks naturalizing what is actually a contingent institutional choice. Comparative evidence (Germany, Austria, Belgium) demonstrates that stable, effective governance is achievable with significantly lower premiums or full proportionality plus coalition-formation norms. The mountain classification here is a FALSE SUMMIT — the framing of 'inherent tension' between representation and stability naturalizes what is actually a design choice embedded in French electoral law.
constraint_indexing:constraint_classification(paris_municipal_reform_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_municipal_reform_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(paris_municipal_reform_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(paris_municipal_reform_2026, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Extractiveness (0.52): Moderate-high. The reform transfers approximately 25 percentage points of council seats from incumbent majority coalitions to potential opposition coalitions. This is a significant structural extraction of political power. However, the extraction is not total suppression — it is a rebalancing that maintains electoral distortion (25% premium remains, preventing full proportionality). The measurement reflects the substantial but not maximal shift in power. The trajectory shows extractiveness declining from 0.72 (under full 50% premium baseline) to 0.52 (initial post-reform) to 0.38 (after multi-cycle institutional adaptation). Suppression (0.48): Moderate. The prior 50% regime suppressed opposition viability almost completely; the reform reduces suppression by enabling coalition formation and genuine electoral competition. But significant suppression remains: the 25% premium still distorts representation, transition costs inhibit opposition mobilization, and institutional inertia favors incumbent adaptation over power rotation. Suppression is not total (some exit is possible) but substantial (fragmentation dynamics persist). Theater_ratio (0.58): Moderate-high. The reform is framed in public discourse as 'democratic progress' and 'republican modernization,' yet the underlying motivation is political necessity (incumbent legitimacy crisis) rather than principled commitment to proportionality. The 25% premium is presented as an interim compromise, but its permanence is uncertain. Theater increases over the interval as political actors ritualize 'pluralism' and 'coalition governance' while maintaining electoral distortion mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. Fragmented opposition parties (powerless/trapped) experience the 50% premium as pure extraction — they have no viable exit path and bear the full cost of incumbent dominance. After the reform, they experience tangled rope characteristics: the 25% premium enables coalition viability (coordination benefit) while imposing transition costs and remaining distortion (extraction cost). Incumbent majorities experience the constraint in reverse: the prior regime was coordinating their power (rope), and the reform imposes extraction of political advantage. Civil society advocates experience scaffold: a temporary constraint with sunset logic toward full proportionality. The state administration experiences rope: a pure coordination mechanism solving a legitimacy crisis. From the civilizational/analytical perspective, the constraint risks appearing as mountain (electoral mechanics as immutable law), but this is a false summit — comparative evidence shows stable governance is achievable with lower premiums or full proportionality. The perspectival gap is widest between the incumbent beneficiary of the old regime and the opposition victim — their structural positions have been inverted by the reform.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from each agent's structural position relative to the extraction flow. The fragmented opposition under the 50% regime has d ≈ 0.95 (full target: trapped, powerless, bearing maximum extraction cost). After the reform, opposition coalitions have d ≈ 0.55 (mixed: constrained exit, but now with coalition viability and moderate power). Incumbent majorities under the old regime have d ≈ 0.05 (full beneficiary: institutional/arbitrage, capturing electoral distortion value). After reform, incumbents have d ≈ 0.65 (moderate target: institutional/arbitrage, but now facing power extraction). The state administration has d ≈ 0.50 (symmetric: solving coordination crisis, experiencing neither extraction nor clear benefit, pure coordination logic). Civil society advocates have d ≈ 0.45 (beneficiary with agency: organized/constrained, driving reform but not controlling outcomes). The engine derives these values from beneficiary/victim declarations and exit options. Overrides are not required — the structural data produces accurate directionality from the derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   The 2026 reform avoids mandatrophy by demonstrating that the constraint is genuinely hybrid, not mislabeled coordination. Mandatrophy would occur if: (A) the constraint was labeled 'rope' (pure coordination) while actually extracting from opposition parties, or (B) labeled 'snare' (pure extraction) while actually coordinating state administration. The tangled_rope classification holds because the constraint simultaneously exhibits both functions: it coordinates state electoral legitimacy while extracting from incumbent majority advantage. The coordination function is structural: the reform reduces litigation, restores public confidence, and enables viable coalition governance. The extraction function is also structural: it transfers political power from incumbent to opposition. Both are irreducible. The mandatrophy resolution is perspectival: different agents experience the constraint as coordination or extraction depending on their structural position. The state sees rope; incumbents see snare; opposition sees scaffold; civil society sees tangled rope. No single perspective is 'correct' — all are legitimate readings of the constraint from different positions. The false mountain (naturalizing electoral mechanics as immutable) is explicitly identified as such in the analytical perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    opposition_coalition_viability,
    'Will the reduced 25% premium enable opposition coalitions to actually achieve governing majorities, or will fragmentation dynamics prevent credible coalition formation?',
    'Empirical observation: outcome of next three municipal election cycles (2026, 2032, 2038). Measurement: percentage of municipalities where opposition coalitions achieve council control; stability of governing coalitions; policy coherence of coalition agreements.',
    'If coalitions succeed: scaffold and tangled_rope perspectives confirmed — the reform enables real coordination and power-sharing. If fragmentation persists: reform is performative scaffolding masking continued incumbent advantage — constraint reclassifies as piton or fake scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opposition_coalition_viability, empirical, 'Whether reduced majority premium enables viable opposition coalitions').

omega_variable(
    enforcement_implementation_capacity,
    'Does the state apparatus have the political will and administrative capacity to enforce the new 25% premium consistently across all municipalities, or will local incumbent pressure create enforcement variance?',
    'Administrative audit: compare predicted seat distributions under new premium formula with actual council composition across Paris municipalities. Measure enforcement variance and deviation from formula.',
    'If uniformly enforced: tangled_rope classification holds, with active enforcement as specified. If enforcement is captured by incumbent interests: constraint degrades toward piton (enforced but theatrically, not substantively).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_implementation_capacity, empirical, 'Degree of uniform enforcement of the 25% premium across municipalities').

omega_variable(
    proportionality_endpoint,
    'Is the 25% premium a genuine interim step toward full proportionality, or a stable resting point that will persist indefinitely?',
    'Political analysis: track parliamentary and civil society pressure for further electoral reform; comparative analysis with European peer democracies; generational cohort analysis of electoral reform preferences.',
    'If genuine interim: scaffold perspective confirmed — sunset logic is structural. If stable resting point: constraint reclassifies as rope-with-distortion (a permanent coordination asymmetry rather than transitional scaffolding).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proportionality_endpoint, conceptual, 'Whether 25% premium is transitional or permanent endpoint').

omega_variable(
    incumbent_strategic_response,
    'Will entrenched majority parties adapt strategically to the reduced premium (absorb smaller parties, form pre-election coalitions) to retain power, or will the premium reduction actually enable opposition alternative governance?',
    'Party system analysis: track party consolidation, coalition formation, and pre-election strategic alliances. Measure whether incumbent advantage persists through strategic party reorganization despite reduced premium.',
    'If incumbents adapt: constraint reclassifies as piton (form persists, function diminishes). If reform enables genuine power rotation: tangled_rope perspective confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_strategic_response, empirical, 'Incumbent strategic adaptation to reduced majority premium').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_municipal_reform_2026, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pmr_tr_t0, paris_municipal_reform_2026, theater_ratio, 0, 0.48).
narrative_ontology:measurement(pmr_tr_t10, paris_municipal_reform_2026, theater_ratio, 10, 0.58).
narrative_ontology:measurement(pmr_tr_t20, paris_municipal_reform_2026, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(pmr_be_t0, paris_municipal_reform_2026, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(pmr_be_t10, paris_municipal_reform_2026, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(pmr_be_t20, paris_municipal_reform_2026, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_municipal_reform_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(paris_municipal_reform_2026, french_electoral_legitimacy_crisis).
narrative_ontology:affects_constraint(paris_municipal_reform_2026, municipal_governance_fragmentation).
narrative_ontology:affects_constraint(paris_municipal_reform_2026, coalition_formation_capacity).

% DUAL FORMULATION NOTE:
% The Paris Municipal Reform is downstream of the broader electoral legitimacy crisis in French democracy. It also affects (and is affected by) municipal governance dynamics and coalition formation capacity across the Paris region. The constraint story models the structural mechanics of the 50%-to-25% premium reduction; sibling stories should address the upstream legitimacy crisis and downstream coalition-building challenges separately, with network links establishing the causal family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(paris_municipal_reform_2026, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
