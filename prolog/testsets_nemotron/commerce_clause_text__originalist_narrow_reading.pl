% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__originalist_narrow_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__originalist_narrow_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: commerce_clause_text__originalist_narrow_reading
 *   human_readable: Commerce Clause — Originalist Narrow Reading (Border-Crossing Trade Only)
 *   domain: constitutional/federalism/commerce
 *
 * SUMMARY:
 *   This constraint story instantiates the originalist narrow reading of the
 *   Commerce Clause (Article I, Section 8, Clause 3): 'Congress shall have
 *   Power... To regulate Commerce... among the several States.' Under this
 *   reading, 'commerce' means trade and exchange — buying, selling,
 *   transporting — and 'among the several States' means commerce that crosses
 *   state lines or uses the instrumentalities of interstate movement
 *   (navigable waters, railroads, highways, air corridors, internet
 *   backbone). Intrastate manufacture, agriculture, services, and labor
 *   relations remain exclusively state concerns unless they are themselves
 *   the 'instrumentalities' of interstate movement. The reading was dominant
 *   until the 1937 switch, partially revived in Lopez (1995) and Morrison
 *   (2000), and remains the declared methodology of the current Court's
 *   originalist majority.
 *
 * KEY AGENTS:
 *   - state_governments: Primary beneficiary (institutional/arbitrage) — retains police power over intrastate activity
 *   - anti_federal_consolidation_advocates: Primary beneficiary (organized/mobile) — intellectual/political movement against federal consolidation
 *   - local_business_interests: Secondary beneficiary (organized/constrained) — avoids federal compliance costs but loses uniform market benefits
 *   - uniform_national_standards: Primary victim (powerless/trapped) — abstract goal of regulatory consistency denied federal authority
 *   - externality_management_entities: Primary victim (moderate/constrained) — cannot regulate intrastate sources of interstate harms
 *   - interstate_pollution_victims: Primary victim (powerless/trapped) — harmed by upstream emissions beyond federal reach
 *   - national_market_participants_seeking_uniformity: Secondary victim (powerful/mobile) — large actors burdened by 50-state patchwork
 *   - originalist_judiciary: Agenda setter (institutional/analytical) — enforces the boundary through judicial review
 *   - congress: Payer (institutional/constrained) — loses regulatory authority over intrastate economic activity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__originalist_narrow_reading, 0.15).
domain_priors:suppression_score(commerce_clause_text__originalist_narrow_reading, 0.2).
domain_priors:theater_ratio(commerce_clause_text__originalist_narrow_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__originalist_narrow_reading, rope).
narrative_ontology:human_readable(commerce_clause_text__originalist_narrow_reading, "Commerce Clause — Originalist Narrow Reading (Border-Crossing Trade Only)").
narrative_ontology:topic_domain(commerce_clause_text__originalist_narrow_reading, "constitutional/federalism/commerce").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__originalist_narrow_reading, '714913f7-03d9-45c1-9adc-d5beb754e216').
narrative_ontology:cs_kernel_codification('714913f7-03d9-45c1-9adc-d5beb754e216', fixed_text).
narrative_ontology:cs_authority_grounding('714913f7-03d9-45c1-9adc-d5beb754e216', lineage).
narrative_ontology:cs_interpretation_layer_present('714913f7-03d9-45c1-9adc-d5beb754e216').
narrative_ontology:cs_reading_relation('714913f7-03d9-45c1-9adc-d5beb754e216', commerce_clause_text__expansive_federal_reading, coexists_with).
narrative_ontology:cs_reading_relation('714913f7-03d9-45c1-9adc-d5beb754e216', commerce_clause_text__substantial_effects_limited_reading, coexists_with).
narrative_ontology:cs_axiom('714913f7-03d9-45c1-9adc-d5beb754e216', foundational, commerce_means_trade_and_exchange).
narrative_ontology:cs_axiom_status(commerce_means_trade_and_exchange, holdable).
narrative_ontology:cs_axiom_grounding('714913f7-03d9-45c1-9adc-d5beb754e216', commerce_means_trade_and_exchange, empirically_contingent).
narrative_ontology:cs_axiom('714913f7-03d9-45c1-9adc-d5beb754e216', foundational, among_the_several_states_means_crossing_borders).
narrative_ontology:cs_axiom_status(among_the_several_states_means_crossing_borders, holdable).
narrative_ontology:cs_axiom_grounding('714913f7-03d9-45c1-9adc-d5beb754e216', among_the_several_states_means_crossing_borders, empirically_contingent).
narrative_ontology:cs_axiom('714913f7-03d9-45c1-9adc-d5beb754e216', secondary, state_police_power_presumptively_exclusive).
narrative_ontology:cs_axiom_status(state_police_power_presumptively_exclusive, holdable).
narrative_ontology:cs_axiom_grounding('714913f7-03d9-45c1-9adc-d5beb754e216', state_police_power_presumptively_exclusive, deontological).
narrative_ontology:cs_reference_frame('714913f7-03d9-45c1-9adc-d5beb754e216', founding_era_commerce_understanding).
narrative_ontology:cs_drift_state('714913f7-03d9-45c1-9adc-d5beb754e216', post_new_deal_switch, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('714913f7-03d9-45c1-9adc-d5beb754e216', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__originalist_narrow_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, anti_federal_consolidation_advocates).
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, local_business_interests).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, uniform_national_standards).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, externality_management_entities).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, interstate_pollution_victims).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, national_market_participants_seeking_uniformity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, congress).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain plenary police power over all intrastate economic and social regulation. The narrow reading protects state legislative autonomy from federal preemption in areas traditionally reserved to states (health, safety, morals, local commerce). States can experiment with different regulatory regimes without federal override.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, state_governments, beneficiary,
    institutional, generational, arbitrage, national).

% Constitutional scholars, originalist jurists, and political movements that view federal power consolidation as a threat to liberty. They benefit intellectually and politically from a reading that confines Congress to its enumerated power over border-crossing commerce. Their influence operates through judicial appointments, litigation, and academic discourse.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, anti_federal_consolidation_advocates, beneficiary,
    organized, civilizational, mobile, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__originalist_narrow_reading, anti_federal_consolidation_advocates, observer).

% Small and medium enterprises operating within single states. They avoid compliance costs of federal regulatory regimes and can lobby state legislatures more accessibly than Congress. However, they also lose the benefits of a uniform national market and may face competitive disadvantages against larger interstate firms.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, local_business_interests, beneficiary,
    organized, biographical, constrained, regional).

% The abstract goal of consistent regulatory standards across state lines (product safety, labor standards, environmental protections, financial regulation). Under this reading, Congress cannot establish national floors for intrastate activity, creating a patchwork that increases compliance costs for interstate businesses and allows regulatory arbitrage.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, uniform_national_standards, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(commerce_clause_text__originalist_narrow_reading, uniform_national_standards).

% Entities (federal agencies, interstate compacts, affected states) that manage cross-border externalities — air/water pollution, financial contagion, disease spread, labor market distortions. The narrow reading denies Congress authority to regulate intrastate sources of interstate externalities, forcing reliance on voluntary compacts or state-by-state litigation.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, externality_management_entities, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_non_agent(commerce_clause_text__originalist_narrow_reading, externality_management_entities).

% Communities and individuals harmed by pollution originating in upstream/upwind states. Under this reading, Congress cannot regulate the intrastate emission sources; victims must sue in source-state courts or negotiate interstate compacts — both structurally disadvantaged positions.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, interstate_pollution_victims, payer,
    powerless, biographical, trapped, continental).

% Large corporations, trade associations, and labor unions that operate across state lines and benefit from regulatory uniformity. They lose the ability to lobby for a single federal standard and must navigate 50 different state regimes. Their exit option is regulatory arbitrage (locating in favorable states) rather than true exit from the constraint.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, national_market_participants_seeking_uniformity, payer,
    powerful, biographical, mobile, national).

% Federal judges (particularly Supreme Court) who adopt and enforce this reading. They set the constitutional boundary through judicial review, striking down federal statutes that exceed the border-crossing limit. Their institutional position insulates them from political accountability; their exit is analytical (they interpret, not live under the regime).
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, originalist_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% The federal legislative branch loses regulatory authority over vast domains of economic activity. Its power is confined to channels and instrumentalities of interstate commerce and border-crossing transactions. It can attempt to stretch definitions (instrumentalities, channels) but faces judicial reversal. Its exit is constitutional amendment — procedurally near-impossible.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, congress, payer,
    institutional, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, textually grounded boundary between federal and state regulatory authority, reducing jurisdictional conflict and preserving state laboratories of democracy. Solves the coordination problem of 'who regulates what' by tethering federal power to the text's original public meaning.
% TRANSFER_FUNCTION: Transfers regulatory authority over intrastate economic activity from Congress to state legislatures. Transfers compliance costs from national uniform standards to state-by-state variation. Transfers externality burdens from regulated sources to downwind/downstream victims.
% ABSENT_VOICES: Future generations who will inherit the climate, infrastructure, and public health consequences of fragmented externality management. Also absent: the constitutional framers themselves — originalists claim to channel them, but the founding generation did not face modern externalities (carbon, financial derivatives, pandemic spread).
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, Congress would immediately regain authority to regulate intrastate activity with substantial effects on interstate commerce (the substantial effects doctrine). Federal environmental, labor, consumer protection, and civil rights statutes would expand. State regulatory autonomy would contract. The federal-state balance would shift dramatically toward national uniformity.
% FOUNDING_PROBLEM: The Articles of Confederation failed because Congress could not regulate commerce crossing state borders, leading to trade wars, tariff barriers, and economic fragmentation among states. The Commerce Clause was designed to empower Congress to prevent state interference with interstate trade — not to displace state regulation of purely internal matters.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars (e.g., Randy Barnett, Robert Natelson) and Federalist Society jurists attest the founding problem was narrowly about border-crossing trade barriers. Progressive scholars (e.g., Jack Balkin, Akhil Amar) and New Deal historians attest the founding generation understood 'commerce' broadly and the Clause as a broad grant to address national economic problems. The corroboration is split along ideological lines; no neutral consensus exists.
narrative_ontology:disappearance_verdict(commerce_clause_text__originalist_narrow_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__originalist_narrow_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__originalist_narrow_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(commerce_clause_text__originalist_narrow_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__originalist_narrow_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__originalist_narrow_reading_tests).
:- end_tests(commerce_clause_text__originalist_narrow_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.15) because the constraint primarily *withholds* federal power rather than extracting resources — it is a coordination mechanism that allocates regulatory authority. The extraction that exists falls on national uniformity seekers and externality victims who bear the costs of fragmentation. Suppression is low (0.2) because the constraint operates through judicial review (a structural check), not active coercion of private actors. Theater is minimal (0.1) — the originalist methodology is genuinely adhered to by its proponents, not performative. Accessibility collapse is high (0.7) because the textual boundary ('among the several States') genuinely limits interpretive alternatives for committed originalists. Resistance is moderate (0.25) — the reading faces political and scholarly opposition but has institutional staying power through judicial appointments.
 *
 * PERSPECTIVAL GAP:
 *   From the state government seat, this is a genuine coordination rope: a clear constitutional boundary protecting state sovereignty. From the externality victim seat, it is a snare: a structural barrier to addressing cross-border harms. From the originalist judiciary seat, it is a mountain: the Constitution's fixed meaning. From Congress's seat, it is a piton: a constraint that once coordinated (1789-1937) but now mainly performs textual fidelity while the modern economy has rendered its boundary obsolete. The engine computes these per-seat types from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (states, anti-consolidation advocates, local businesses) gain regulatory autonomy and avoid federal preemption — directionality near 0.0 (subsidy). Victims (uniformity seekers, externality victims, pollution victims) bear costs of fragmentation and unaddressed harms — directionality near 1.0 (full target). Congress pays opportunity cost of lost legislative power — directionality ~0.7. The originalist judiciary sits at ~0.1 (beneficiary of interpretive authority). Rival payment networks equivalent: excluded_voices are structurally locked out of the constitutional conversation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing state trade barriers) is live but has been substantially solved by other means (dormant Commerce Clause, modern transportation, political integration). The arrangement persists because it now serves a different function: constraining federal regulatory ambition. This is not pure extraction (states genuinely coordinate through the boundary) but not pure coordination either (externalities go unmanaged). The mandatrophy is partial: the original coordination function atrophied; a new anti-consolidation function emerged. The constraint is a rope for states, a snare for externality victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the commerce_clause_text a single kernel with three readings, or are these three distinct constraints that merely share a textual anchor?',
    'Test ε-invariance: if measuring ''federal regulatory authority'' under each reading yields structurally different ε values (this reading ~0.15, expansive ~0.65, substantial_effects ~0.4), they are distinct constraints. The ε gap confirms decomposition.',
    'If distinct constraints, each must be authored separately with its own stakeholders and classification. The current decomposition follows this principle. If a single kernel, the framework would need a kernel-level classification mechanism (not currently implemented).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the three Commerce Clause readings are distinct constraints or observable-dependent views of one constraint.').

omega_variable(
    originalism_as_coordination_vs_extraction,
    'Does the originalist narrow reading genuinely coordinate federal-state relations, or does it extract regulatory capacity from the federal level for the benefit of state-level incumbents?',
    'Counterfactual: if originalist jurists applied the same textualism to constrain state power (e.g., under the Fourteenth Amendment), would the coordination function hold symmetrically? Asymmetric application suggests extraction.',
    'If extraction, the constraint reclassifies toward tangled_rope or snare for the congressional seat. If coordination, rope classification holds. The current low extractiveness score assumes coordination; this omega flags the asymmetry test.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_as_coordination_vs_extraction, conceptual, 'Whether originalist methodology functions symmetrically as coordination or asymmetrically as extraction.').

omega_variable(
    externality_victim_constitutional_status,
    'Do interstate pollution victims and externality management entities have constitutional standing as ''victims'' of a Commerce Clause reading, or are they merely policy losers in a federalism bargain?',
    'Doctrinal analysis: does the Constitution guarantee a right to federal regulation of interstate externalities? Current doctrine says no (no affirmative federal duty to regulate). But structural analysis: if the federalism bargain creates systematic losers, the constraint extracts from them.',
    'If victims are constitutional losers, the constraint''s extraction is structural and the snare classification for their seat is warranted. If merely policy losers, the extraction is contingent and the rope classification for the overall constraint holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(externality_victim_constitutional_status, conceptual, 'Whether the constraint''s victims are constitutional or merely political.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__originalist_narrow_reading, 1789, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1789, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1789, 0.02).
narrative_ontology:measurement(comm_tr_t1835, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1835, 0.05).
narrative_ontology:measurement(comm_tr_t1895, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1895, 0.1).
narrative_ontology:measurement(comm_tr_t1937, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1937, 0.02).
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1995, 0.08).
narrative_ontology:measurement(comm_tr_t2026, commerce_clause_text__originalist_narrow_reading, theater_ratio, 2026, 0.1).

% Extraction over time
narrative_ontology:measurement(comm_be_t1789, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1789, 0.05).
narrative_ontology:measurement(comm_be_t1835, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1835, 0.08).
narrative_ontology:measurement(comm_be_t1895, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1895, 0.12).
narrative_ontology:measurement(comm_be_t1937, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1937, 0.03).
narrative_ontology:measurement(comm_be_t1995, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1995, 0.08).
narrative_ontology:measurement(comm_be_t2026, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 2026, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1789, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1789, 0.1).
narrative_ontology:measurement(comm_su_t1835, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1835, 0.15).
narrative_ontology:measurement(comm_su_t1895, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1895, 0.25).
narrative_ontology:measurement(comm_su_t1937, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1937, 0.05).
narrative_ontology:measurement(comm_su_t1995, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1995, 0.15).
narrative_ontology:measurement(comm_su_t2026, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 2026, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__originalist_narrow_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_text__originalist_narrow_reading, 0.1).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, dormant_commerce_clause).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, state_police_power_doctrine).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, federal_preemption_doctrine).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, tenth_amendment_reserved_powers).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, commerce_clause_text__expansive_federal_reading).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, commerce_clause_text__substantial_effects_limited_reading).

% DUAL FORMULATION NOTE:
% This constraint (originalist_narrow_reading) is one of three in the commerce_clause_text family. The expansive_federal_reading (ε~0.65) and substantial_effects_limited_reading (ε~0.4) are distinct constraints with different beneficiary/victim structures. This reading provides the textualist baseline; the others expand federal authority through different doctrinal mechanisms. The family exhibits ε-invariance: each reading's ε is stable under its own measurement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commerce_clause_text__originalist_narrow_reading, institutional, 0.1).
constraint_indexing:directionality_override(commerce_clause_text__originalist_narrow_reading, powerless, 0.9).
constraint_indexing:directionality_override(commerce_clause_text__originalist_narrow_reading, moderate, 0.6).
constraint_indexing:directionality_override(commerce_clause_text__originalist_narrow_reading, powerful, 0.4).
constraint_indexing:directionality_override(commerce_clause_text__originalist_narrow_reading, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
