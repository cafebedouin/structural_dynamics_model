% ============================================================================
% CONSTRAINT STORY: border_legitimacy__sovereignty_primary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__sovereignty_primary_reading, []).

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
 *   constraint_id: border_legitimacy__sovereignty_primary_reading
 *   human_readable: Border Control Legitimacy (Sovereignty-Primary Reading)
 *   domain: political_philosophy/migration_policy/constitutional_law
 *
 * SUMMARY:
 *   Border control in the sovereignty-primary reading is understood as the
 *   legitimate exercise of collective self-determination: a political
 *   community claims the right to control its territorial boundaries based on
 *   the democratic mandate of existing members. This reading grounds state
 *   authority to exclude in two linked commitments: (1) territorial
 *   sovereignty as a foundational principle of international order, and (2)
 *   democratic authority of the existing citizenry to make decisions
 *   affecting their shared political order. Under this reading, excluded
 *   migrants are positioned as non-members with no entitlement to territorial
 *   access — exclusion is not extraction but rather the exercise of
 *   legitimate community boundary-maintenance. However, this reading exhibits
 *   all six DR classifications from different structural positions, revealing
 *   it as a contested kernel with genuine alternative readings. The
 *   sovereignty-primary frame naturalizes institutional arrangements
 *   (territorial exclusivity, citizenship-based access) that have
 *   beneficiaries and can be measured for extractive content. The constraint
 *   shows increasing theater_ratio over time (visa bureaucracy, documentation
 *   processing, security theater expansion) while extraction and suppression
 *   requirements also increase, suggesting the sovereignty narrative is
 *   increasingly doing legitimation work as enforcement costs rise.
 *
 * KEY AGENTS:
 *   - Excluded Migrants: Primary victims (powerless/trapped) — structurally barred from entry within this reading's framework; bear full enforcement costs with no voice in decision-making
 *   - Existing Citizenry: Primary beneficiaries (institutional/arbitrage) — authorized to make boundary decisions; benefit from membership monopoly and labor cost discipline
 *   - State Apparatus: Secondary beneficiary (institutional/arbitrage) — exercises enforcement authority; benefits from monopoly on movement control; maintains institutional power through border functions
 *   - Subnational Border Communities: Mixed position (moderate/constrained) — bear local enforcement burden and surveillance infrastructure costs while benefiting from state protection and infrastructure investment
 *   - Capital / Multinational Actors: Secondary beneficiary (powerful/mobile) — benefit from supply chain predictability and labor cost discipline while bearing enforcement transaction costs; maintain arbitrage options across jurisdictions
 *   - Border Enforcement Apparatus: Tertiary beneficiary (institutional/arbitrage) — maintains itself through performative bureaucracy; primary beneficiary becomes the apparatus itself rather than the citizenry
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing contingent institutional arrangements as inevitable features of political order
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__sovereignty_primary_reading, 0.58).
domain_priors:suppression_score(border_legitimacy__sovereignty_primary_reading, 0.72).
domain_priors:theater_ratio(border_legitimacy__sovereignty_primary_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_primary_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_primary_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_primary_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__sovereignty_primary_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__sovereignty_primary_reading, "Border Control Legitimacy (Sovereignty-Primary Reading)").
narrative_ontology:topic_domain(border_legitimacy__sovereignty_primary_reading, "political_philosophy/migration_policy/constitutional_law").

domain_priors:requires_active_enforcement(border_legitimacy__sovereignty_primary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__sovereignty_primary_reading, 'a466cef3-2d41-4b22-8d5d-05ddf36aa669').
narrative_ontology:cs_kernel_codification('a466cef3-2d41-4b22-8d5d-05ddf36aa669', formalized).
narrative_ontology:cs_authority_grounding('a466cef3-2d41-4b22-8d5d-05ddf36aa669', extraction).
narrative_ontology:cs_interpretation_layer_present('a466cef3-2d41-4b22-8d5d-05ddf36aa669').
narrative_ontology:cs_reading_relation('a466cef3-2d41-4b22-8d5d-05ddf36aa669', border_legitimacy__freedom_of_movement_reading, coexists_with).
narrative_ontology:cs_reading_relation('a466cef3-2d41-4b22-8d5d-05ddf36aa669', border_legitimacy__economic_utility_reading, influences).
narrative_ontology:cs_axiom('a466cef3-2d41-4b22-8d5d-05ddf36aa669', foundational, state_territorial_exclusivity_legitimate).
narrative_ontology:cs_axiom_status(state_territorial_exclusivity_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('a466cef3-2d41-4b22-8d5d-05ddf36aa669', state_territorial_exclusivity_legitimate, conventional).
narrative_ontology:cs_axiom('a466cef3-2d41-4b22-8d5d-05ddf36aa669', foundational, existing_citizenry_democratic_mandate_over_boundaries).
narrative_ontology:cs_axiom_status(existing_citizenry_democratic_mandate_over_boundaries, holdable).
narrative_ontology:cs_axiom_grounding('a466cef3-2d41-4b22-8d5d-05ddf36aa669', existing_citizenry_democratic_mandate_over_boundaries, deontological).
narrative_ontology:cs_reference_frame('a466cef3-2d41-4b22-8d5d-05ddf36aa669', westphalian_territorial_sovereignty).
narrative_ontology:cs_drift_state('a466cef3-2d41-4b22-8d5d-05ddf36aa669', contemporary_human_rights_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a466cef3-2d41-4b22-8d5d-05ddf36aa669', '2026-02-26T14:22:08Z').
narrative_ontology:cs_kernel_id(border_legitimacy__sovereignty_primary_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_primary_reading, existing_citizenry).
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_primary_reading, state_apparatus).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_primary_reading, excluded_migrants).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_primary_reading, transnational_mobility_interests).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED MIGRANT (SNARE) — Structurally barred from entry. No alternative recourse within the sovereignty framework; must either accept exclusion or violate law. Maximal suppression and extraction. The sovereignty reading explicitly positions the excluded migrant as a non-beneficiary of the territorial order — legitimacy flows to existing members only. Trapped, powerless, experiences full weight of enforcement.
constraint_indexing:constraint_classification(border_legitimacy__sovereignty_primary_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SUBNATIONAL BORDER COMMUNITIES (TANGLED ROPE) — Constrained by enforcement burden and local social effects, but also benefit from state protection and labor flow coordination. Communities experience both extraction (border militarization, surveillance infrastructure) and coordination (border security, infrastructure investment). Exit is costly but possible through internal migration or advocacy. Mixed incentive structure.
constraint_indexing:constraint_classification(border_legitimacy__sovereignty_primary_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXISTING CITIZENRY / POLITICAL AUTHORITY (ROPE) — Primary beneficiaries. The sovereignty reading declares border control is the exercise of collective self-determination: citizens authorize the state to maintain territorial boundaries on their behalf. This is presented as pure coordination of a collective good (territorial integrity, democratic mandate exercise). Arbitrage options: can lobby for open borders (exit the constraint), can voice within democratic process. Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(border_legitimacy__sovereignty_primary_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CAPITAL / MULTINATIONAL ACTORS (TANGLED ROPE) — Coordinate global supply chains and labor sourcing through border regimes while also constrained by border enforcement costs. Both benefit from state-guaranteed border protection (labor cost discipline through scarcity, supply chain predictability) and bear enforcement transaction costs. Mobile exit options (regulatory arbitrage across jurisdictions). Mixed extraction and coordination.
constraint_indexing:constraint_classification(border_legitimacy__sovereignty_primary_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: BORDER ENFORCEMENT APPARATUS (PITON) — The enforcement machinery (visa systems, border patrol, deportation infrastructure) persists through institutional inertia. Theater_ratio high (extensive bureaucratic performance: visa interviews, documentation processing, security theater) masking low functional verification of actual exclusion criteria. The apparatus maintains itself via legitimating narratives (sovereignty, national security) while actual enforcement becomes increasingly costly and ritualistic. Primary beneficiary becomes the apparatus itself rather than the citizenry it claims to serve.
constraint_indexing:constraint_classification(border_legitimacy__sovereignty_primary_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational/universal perspective, state sovereignty over territory is presented as an immutable feature of international order: states necessarily possess border authority as a natural law of political organization. This perspective naturalizes what is a contingent institutional arrangement. The sovereignty reading's false summit risk: it treats democratic legitimacy and territorial control as self-evident natural phenomena rather than as contested normative commitments that benefit specific agents. Engine will flag this as a false summit candidate due to declared beneficiaries.
constraint_indexing:constraint_classification(border_legitimacy__sovereignty_primary_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__sovereignty_primary_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(border_legitimacy__sovereignty_primary_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(border_legitimacy__sovereignty_primary_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__sovereignty_primary_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(border_legitimacy__sovereignty_primary_reading, TR),
    TR >= 0.70.

:- end_tests(border_legitimacy__sovereignty_primary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The sovereignty reading justifies exclusion as legitimate exercise of democratic authority, but empirical measurement of actual enforcement reveals substantial extraction beyond what coordination of territorial order requires. Rising extractiveness over time (0.35 → 0.58) reflects accumulation of enforcement functions (labor discipline, social cost externalization) layered onto the core territorial control function. Suppression (0.72): High. Multiple enforcement mechanisms: legal prohibition on entry, violent border interception, deportation machinery, documentation requirements, visa denial. No internal exit mechanism — excluded migrants must either accept exclusion or violate law. Suppression is structural (legal and enforcement-based) rather than purely internalized. Theater ratio (0.48): Moderate. Border enforcement includes genuine coordination functions (processing, health screening, security background checks) but increasingly performs legitimating theater: extensive bureaucracy masking relatively low verification rigor for actual exclusion criteria. Rising theater over time (0.32 → 0.48) reflects expansion of visa/documentation processing that performs sovereignty without substantially improving actual vetting. The sovereignty reading sustains itself through theater — procedural legitimacy substitutes for outcome legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   The sovereignty-primary reading produces sharp perspectival divergence. Existing citizens (beneficiaries with arbitrage options) experience the constraint as rope: legitimate coordination of a collective good. The analytical observer risks mountain: seeing border authority as an immutable feature of political order. Subnational communities experience tangled rope: mixed costs and benefits. Capital experiences tangled rope: both constrained by and benefiting from border discipline. The border apparatus experiences piton: performative maintenance of degraded legitimacy. Excluded migrants experience snare: maximal extraction with no exit. This divergence is not measurement error but structural reality — the same constraint produces fundamentally different experienced classifications depending on structural position. The false summit risk is highest at the analytical/institutional level, where naturalizing language is strongest.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: existing_citizenry, state_apparatus. Victims declared: excluded_migrants, transnational_mobility_interests. This structure creates the directionality flow: benefits concentrate on citizens and state; costs concentrate on migrants. No directionality overrides needed — the structural derivation captures the relationship accurately. Citizens with arbitrage (can lobby for policy change) → d ≈ 0.15 → negative f(d). Migrants with trapped exit → d ≈ 0.95 → high f(d) ≈ 1.42. State apparatus as beneficiary with arbitrage → d ≈ 0.10 → negative f(d). This explains why chi is high for excluded migrants (experiencing the constraint severely) and low or negative for citizens and state (experiencing it as legitimate order maintenance).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves through omega variables addressing the conceptual boundary between coordination and extraction. If border enforcement is primarily coordination of territorial order, the sovereignty reading is defensible and tangled_rope is appropriate (genuine coordination + some extraction). If enforcement is primarily extraction using sovereignty as legitimation, snare is the correct classification and the sovereignty reading unmasked as false natural law. The central ambiguity: whether democratic mandate of existing citizens constitutes legitimate authorization or circular reasoning (using the constraint to define the population claiming to authorize it). The rising theater_ratio and extraction values over time suggest the legitimation work is intensifying — enforcement costs rising faster than coordination benefits, requiring more elaborate procedural theater to sustain the sovereignty narrative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    democratic_mandate_boundary,
    'Does the existing citizenry''s democratic mandate extend to exclusion decisions, or does it extend only to processes for deciding exclusion policy without pre-determining outcomes?',
    'Comparative constitutional analysis of democratic participation scope; examination of which exclusion decisions are subject to democratic reversal vs treated as structural inevitabilities',
    'If mandate includes outcome pre-determination: sovereignty reading is strengthened (extraction is legitimized by democratic process). If mandate covers process only: extraction mechanism becomes visible as undemocratic asymmetry (excluded migrants have no voice in decision affecting them); classification shifts toward snare for all perspectives except those with arbitrage power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(democratic_mandate_boundary, conceptual, 'Scope and binding force of democratic mandate for border control').

omega_variable(
    territorial_exclusivity_contingency,
    'Is territorial exclusivity necessary to any functioning state, or is it a contingent institutional choice among alternatives (open borders, graduated residency, multi-tier citizenship)?',
    'Historical counterfactual analysis: identification of states or polities that functioned with non-exclusive territorial authority; examination of what conditions enabled or prevented border-less governance; empirical assessment of whether exclusivity provides returns higher than alternative coordination mechanisms',
    'If necessary: mountain classification is defensible (immutable constraint). If contingent: sovereignty reading is a constructed constraint naturalizing a particular institutional choice; all classifications shift toward snare/tangled_rope with extraction becoming visible rather than naturalized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territorial_exclusivity_contingency, empirical, 'Whether territorial exclusivity is structurally necessary or contingently chosen').

omega_variable(
    extraction_vs_coordination_boundary,
    'What portion of border enforcement costs/benefits constitute coordination (maintaining a shared territorial order) vs extraction (capturing mobility rents, labor cost discipline, social resource scarcity)?',
    'Cost-benefit analysis decomposing border enforcement: separation of functions (genuine territorial defense vs immigration control); accounting of who captures coordination surplus vs who bears coordination costs; comparison of border enforcement scale to actual security threats vs actual labor/social scarcity',
    'If coordination > extraction: tangled_rope classification is stable, sovereignty reading legitimate. If extraction > coordination: snare classification becomes primary, sovereignty reading unmasked as extraction legitimation. Directly determines chi value and mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, empirical, 'Decomposition of border enforcement into coordination vs extraction components').

omega_variable(
    sibling_reading_foreclosure,
    'Does this sovereignty-primary reading logically foreclose the freedom-of-movement reading, or do both readings remain live options within different legitimate frameworks?',
    'Framework analysis: examination of whether commitment to sovereignty necessarily implies denial of fundamental mobility rights, or whether both can be held in tension (e.g., sovereignty over policy process, freedom of movement as constraint on outcome). Examination of international law evolution showing both readings as live.',
    'If forecloses: reading_relations should declare ''forecloses'' for freedom_of_movement sibling. If coexists: declare ''coexists_with'', indicating the kernel remains genuinely contested. Affects whether the sovereignty reading is framed as necessary or as one option among legitimate alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Logical relationship between sovereignty-primary and freedom-of-movement readings').

omega_variable(
    citizen_identity_constitution,
    'Is ''existing citizenry'' a pre-existing collective with interests independent of the border constraint, or does the border constraint partly constitute the category ''citizen''?',
    'Historical genealogy of citizenship: examination of whether citizenship status precedes or follows border enforcement; analysis of how border controls shaped who could claim membership; documentation of feedback loops between exclusion practices and citizenship definition',
    'If pre-existing: citizenry''s mandate is genuine beneficiary choice. If partly constituted: sovereignty reading circularly uses the constraint to define the population claiming to authorize it — a logical circularity that undermines the legitimacy argument. Affects whether beneficiaries are truly independent agents or artifacts of the constraint structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(citizen_identity_constitution, conceptual, 'Whether citizenship is independent of or partly constituted by border enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__sovereignty_primary_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(border_sov_tr_t0, border_legitimacy__sovereignty_primary_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(border_sov_tr_t10, border_legitimacy__sovereignty_primary_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement(border_sov_tr_t20, border_legitimacy__sovereignty_primary_reading, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(border_sov_be_t0, border_legitimacy__sovereignty_primary_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(border_sov_be_t10, border_legitimacy__sovereignty_primary_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(border_sov_be_t20, border_legitimacy__sovereignty_primary_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(border_sov_su_t0, border_legitimacy__sovereignty_primary_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(border_sov_su_t10, border_legitimacy__sovereignty_primary_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(border_sov_su_t20, border_legitimacy__sovereignty_primary_reading, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__sovereignty_primary_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(border_legitimacy__sovereignty_primary_reading, 0.25).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_primary_reading, border_legitimacy__freedom_of_movement_reading).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_primary_reading, border_legitimacy__economic_utility_reading).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_primary_reading, labor_cost_discipline_through_scarcity).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_primary_reading, citizenship_constitution_through_exclusion).

% DUAL FORMULATION NOTE:
% The border_legitimacy kernel decomposes into three constraint stories corresponding to three distinct readings: sovereignty_primary (this file), freedom_of_movement_primary, and economic_utility. Each reading has its own epsilon value, beneficiary/victim structure, and classification profile. The readings coexist in political discourse — they are not competing empirical claims but competing normative framings of the same institutional phenomenon. All three stories should be generated and linked via network.affects_constraints to show the structure of the contested kernel. This file (sovereignty_primary) influences the other two by establishing territorial control as a foundational principle; freedom_of_movement reading forecloses some claims of the sovereignty reading; economic_utility reading influences both by providing instrumental justification independent of either sovereignty or rights claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
