% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__narrow_originalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__narrow_originalist, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: commerce_clause_scope__narrow_originalist
 *   human_readable: Narrow Originalist Commerce Clause: Trade Crossing State Lines Only
 *   domain: constitutional_law/federalism/commerce_power
 *
 * SUMMARY:
 *   The narrow originalist reading of the Commerce Clause holds that
 *   'commerce among the several states' means only trade crossing state
 *   lines, 'regulate' means to make regular (facilitate) not to restrict or
 *   prohibit, and federal power extends only to removing state-imposed
 *   barriers to interstate trade and ensuring uniform commercial rules. This
 *   reading constrains the modern administrative state by denying Congress
 *   authority over intrastate non-commercial activity, aggregation of
 *   intrastate effects, and regulation via attenuated causal chains. It
 *   benefits state governments and local businesses by preserving intrastate
 *   regulatory autonomy. It extracts from citizens in recalcitrant states who
 *   lose federal civil rights and labor protections, from national businesses
 *   facing regulatory fragmentation, and from federal agencies stripped of
 *   intrastate reach. The reading requires active judicial enforcement
 *   (originalist judges striking down overreaching statutes). Its claimed
 *   type is 'rope' — pure coordination of interstate trade — but structural
 *   victims exist, creating potential for tangled_rope classification at
 *   payer seats.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__narrow_originalist, 0.28).
domain_priors:suppression_score(commerce_clause_scope__narrow_originalist, 0.35).
domain_priors:theater_ratio(commerce_clause_scope__narrow_originalist, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, extractiveness, 0.28).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__narrow_originalist, rope).
narrative_ontology:human_readable(commerce_clause_scope__narrow_originalist, "Narrow Originalist Commerce Clause: Trade Crossing State Lines Only").
narrative_ontology:topic_domain(commerce_clause_scope__narrow_originalist, "constitutional_law/federalism/commerce_power").

domain_priors:requires_active_enforcement(commerce_clause_scope__narrow_originalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__narrow_originalist, '5f3762cd-930c-48e5-8f44-f83f4fe3b387').
narrative_ontology:cs_kernel_codification('5f3762cd-930c-48e5-8f44-f83f4fe3b387', fixed_text).
narrative_ontology:cs_authority_grounding('5f3762cd-930c-48e5-8f44-f83f4fe3b387', lineage).
narrative_ontology:cs_interpretation_layer_present('5f3762cd-930c-48e5-8f44-f83f4fe3b387').
narrative_ontology:cs_reading_relation('5f3762cd-930c-48e5-8f44-f83f4fe3b387', commerce_clause_scope__broad_effects_test, forecloses).
narrative_ontology:cs_reading_relation('5f3762cd-930c-48e5-8f44-f83f4fe3b387', commerce_clause_scope__intermediate_channels, coexists_with).
narrative_ontology:cs_axiom('5f3762cd-930c-48e5-8f44-f83f4fe3b387', foundational, commerce_clause_text_fixed_meaning).
narrative_ontology:cs_axiom_status(commerce_clause_text_fixed_meaning, holdable).
narrative_ontology:cs_axiom_grounding('5f3762cd-930c-48e5-8f44-f83f4fe3b387', commerce_clause_text_fixed_meaning, deontological).
narrative_ontology:cs_axiom('5f3762cd-930c-48e5-8f44-f83f4fe3b387', foundational, regulate_means_facilitate_not_prohibit).
narrative_ontology:cs_axiom_status(regulate_means_facilitate_not_prohibit, holdable).
narrative_ontology:cs_axiom_grounding('5f3762cd-930c-48e5-8f44-f83f4fe3b387', regulate_means_facilitate_not_prohibit, deontological).
narrative_ontology:cs_axiom('5f3762cd-930c-48e5-8f44-f83f4fe3b387', secondary, state_sovereignty_preserved_in_intrastate).
narrative_ontology:cs_axiom_status(state_sovereignty_preserved_in_intrastate, holdable).
narrative_ontology:cs_axiom_grounding('5f3762cd-930c-48e5-8f44-f83f4fe3b387', state_sovereignty_preserved_in_intrastate, deontological).
narrative_ontology:cs_reference_frame('5f3762cd-930c-48e5-8f44-f83f4fe3b387', founding_era_commerce_understanding).
narrative_ontology:cs_drift_state('5f3762cd-930c-48e5-8f44-f83f4fe3b387', post_new_deal_expansion, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('5f3762cd-930c-48e5-8f44-f83f4fe3b387', '2026-07-25T14:30:00Z').
narrative_ontology:cs_kernel_id(commerce_clause_scope__narrow_originalist, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, local_businesses).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, decentralized_regulatory_experimentation_advocates).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, citizens_in_recalcitrant_states).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, national_businesses_facing_fragmentation).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, federal_regulatory_agencies).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, civil_rights_enforcement_interests).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, local_businesses).
narrative_ontology:constraint_vindicates(commerce_clause_scope__narrow_originalist, constitutional_text_fixed_at_founding).
narrative_ontology:constraint_vindicates(commerce_clause_scope__narrow_originalist, state_sovereignty_in_intrastate_domain).
narrative_ontology:constraint_vindicates(commerce_clause_scope__narrow_originalist, judicial_restraint_in_economic_regulation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain plenary authority over intrastate economic activity including labor conditions, environmental standards, and local commerce. Can experiment with regulatory models without federal preemption. Exit means asserting Tenth Amendment reservations against federal encroachment.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, state_governments, beneficiary,
    institutional, generational, arbitrage, national).

% Operate free from federal compliance costs (minimum wage, OSHA, EPA) when activity stays within state lines. But bear costs of regulatory fragmentation when operating across state lines and lose economies of scale from national standards.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, local_businesses, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__narrow_originalist, local_businesses, payer).

% Lose federal civil rights, environmental, and labor protections when state government refuses to enact them. Cannot practically exit due to economic, familial, and social ties. Bear the full cost of state-level regulatory neglect.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, citizens_in_recalcitrant_states, payer,
    powerless, biographical, trapped, local).

% Must comply with 50 different regulatory regimes for intrastate activity, increasing compliance costs and preventing national standardization. Can lobby for federal legislation but face constitutional barrier under this reading. Exit means accepting fragmentation or restructuring as purely interstate entities.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, national_businesses_facing_fragmentation, payer,
    powerful, biographical, constrained, national).

% Lose authority to regulate intrastate activities with cumulative interstate effects (e.g., EPA regulating intrastate pollution, Labor regulating intrastate wages). Retain power only over actual interstate trade barriers. Must defend narrow jurisdiction in court; cannot expand without constitutional amendment.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, federal_regulatory_agencies, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__narrow_originalist, federal_regulatory_agencies, agenda_setter).

% Cannot rely on Commerce Clause to reach private discrimination in purely local establishments (e.g., Heart of Atlanta Motel reasoning foreclosed). Must pursue Fourteenth Amendment state-action doctrine or state-level remedies, which are weaker against private actors.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, civil_rights_enforcement_interests, excluded,
    organized, generational, constrained, national).

% Enforce the narrow reading through judicial review: strike down federal laws regulating intrastate non-commercial activity, invalidate aggregation principle, require jurisdictional elements for non-economic regulation. Their interpretive authority constitutes the constraint's enforcement mechanism.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, originalist_judges, agenda_setter,
    institutional, generational, analytical, national).

% Analyze the reading as historically contested and structurally inadequate for modern governance. Document the divergence between founding-era commerce and contemporary integrated economy. Do not participate in enforcement but shape elite and public discourse.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, living_constitutionalist_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents states from erecting trade barriers against each other (tariffs, discriminatory regulations, protectionist measures) and establishes a baseline of commercial regularity across state lines — solving the collective-action problem of the Articles of Confederation era.
% TRANSFER_FUNCTION: Transfers regulatory authority over intrastate economic activity from federal government to state governments. Transfers protective benefits (civil rights, labor standards, environmental quality) from citizens in recalcitrant states to state-level political majorities. Transfers compliance cost savings to local businesses operating purely intrastate.
% ABSENT_VOICES: Citizens in recalcitrant states who would invoke federal power against their own state governments are structurally excluded — the reading denies the constitutional hook (Commerce Clause) that would bring them into court. Civil rights organizations representing these citizens are similarly excluded from the Commerce Clause forum.
% DISAPPEARANCE_RATIONALE: If the narrow reading vanished overnight, Congress would immediately regain authority to regulate intrastate activities with substantial aggregate effects on interstate commerce (Wickard/Raich restored). Federal environmental, labor, and civil rights statutes would apply uniformly nationwide. State regulatory autonomy would contract. The national regulatory state would expand to its post-1937 footprint.
% FOUNDING_PROBLEM: Under the Articles of Confederation, states imposed tariffs and trade barriers on each other, fragmenting the national economy. The Commerce Clause was designed to make interstate trade 'regular' — i.e., free from state-imposed obstructions — not to grant Congress a general police power over all economic activity.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars (e.g., Barnett, Lawson, Natelson) attest the founding problem was narrowly about interstate trade barriers, citing founding-era dictionaries and Convention records. Living constitutionalist scholars (e.g., Ackerman, Balkin, Sunstein) and New Deal-era Court majorities attest the founding problem was creating a functional national economy, requiring broad federal power. The parties dispute both the historical facts and their contemporary relevance.
narrative_ontology:disappearance_verdict(commerce_clause_scope__narrow_originalist, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__narrow_originalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__narrow_originalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(commerce_clause_scope__narrow_originalist, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__narrow_originalist, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__narrow_originalist_tests).
:- end_tests(commerce_clause_scope__narrow_originalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28) because the constraint primarily *limits* federal extraction from state sovereignty rather than extracting itself. The federal government loses regulatory capacity; states gain autonomy. Suppression (0.35) reflects judicial enforcement striking down federal laws — active but targeted. Theater is low (0.15): originalist opinions engage substantively with text and history. Accessibility collapse (0.72) is high: once the founding-era meaning is accepted, alternatives (broad effects test) appear as judicial invention. Resistance (0.58) is substantial: living constitutionalist precedent, stare decisis, and practical governance needs resist the narrow reading. The measurement series shows extractiveness rising as the administrative state expands (more federal law to strike down), theater spiking in 1937 (Court-packing crisis), and suppression rising with originalist judicial appointments post-1995.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (originalist judges), the constraint is genuine coordination: restoring the Constitution's fixed meaning, preventing judicial legislation. From payer seats (citizens in recalcitrant states), the same structure operates as enforced abandonment — the Constitution becomes a shield for state-level rights violations. From beneficiary seats (state governments), it is recovered sovereignty. The engine computes this divergence; the claim (rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and local businesses are structural beneficiaries (d near 0.0): they gain regulatory autonomy and avoid federal compliance costs. Citizens in recalcitrant states, national businesses, and federal agencies are structural payers (d near 1.0): they lose protections, face fragmentation, lose authority. Originalist judges are agenda_setters with analytical exit (d ≈ 0.5: they enforce but don't personally gain/lose). Civil rights interests are excluded — their exclusion IS the mechanism by which the constraint avoids classifying them as payers. The engine will compute per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (interstate trade barriers) is substantially solved — states no longer erect tariffs. Yet the narrow reading persists and expands (Lopez, Morrison, NFIB, West Virginia v. EPA). This suggests mandatrophy: the constraint's original coordination function has been achieved, but the constraint now serves a new function (limiting the administrative state) that benefits different parties (conservative legal movement, business interests opposing regulation). The constraint has not sunset; it has been repurposed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine natural-language commitment to the Constitution''s fixed meaning, or a constructed constraint that benefits identifiable agents (conservative legal movement, business interests opposing regulation) under cover of originalism?',
    'Trace the historical emergence of narrow originalism: does it predate the administrative state it now constrains (suggesting genuine commitment), or emerge as a response to New Deal expansion (suggesting constructed constraint)? Compare citation patterns in pre-1937 vs. post-1980 originalist scholarship.',
    'If constructed, the constraint is a false summit mountain (claims natural-law fixity but has beneficiaries) or a snare (coordination cover for extraction from national regulatory capacity). If genuine, it is a rope or mountain depending on whether victims are structural or contingent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the narrow originalist reading is a genuine fixed-meaning commitment or a constructed constraint with beneficiaries').

omega_variable(
    coordination_extraction_boundary,
    'Does preventing state trade barriers genuinely require the narrow scope (no aggregation, no non-economic regulation, no attenuation), or would the intermediate_channels framework suffice for the coordination function while preserving some federal capacity?',
    'Counterfactual analysis: in the 1995-2025 period where intermediate_channels was operative (Lopez through NFIB), did state trade barriers re-emerge? Did interstate commerce fragment? If not, the narrow scope extracts more than coordination requires.',
    'If intermediate suffices, the narrow reading''s additional restrictions are pure extraction (snare/tangled_rope) rather than coordination necessity (rope). The victims (lost civil rights, environmental, labor regulation) are the price of over-coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the narrow scope''s additional restrictions beyond intermediate_channels are coordination-necessary or extractive').

omega_variable(
    victim_structure_contingency,
    'Are the identified victims (citizens in recalcitrant states losing civil rights protections) a structural necessity of the narrow reading, or contingent on current state political configurations?',
    'Survey state-level civil rights, labor, and environmental laws: do states that would lose federal coverage under narrow reading actually provide substitute protections? If yes, victims are contingent; if no, victims are structural.',
    'If victims are contingent, the constraint may be rope at current margins (coordination without necessary extraction). If structural, the constraint is tangled_rope or snare — coordination inherently produces these victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_structure_contingency, empirical, 'Whether the victim class is structurally entailed by the reading or contingent on state policy choices').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__narrow_originalist, 1789, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(commerce_clause_narrow_orig_tr_t1789, commerce_clause_scope__narrow_originalist, theater_ratio, 1789, 0.05).
narrative_ontology:measurement(commerce_clause_narrow_orig_tr_t1830, commerce_clause_scope__narrow_originalist, theater_ratio, 1830, 0.08).
narrative_ontology:measurement(commerce_clause_narrow_orig_tr_t1890, commerce_clause_scope__narrow_originalist, theater_ratio, 1890, 0.15).
narrative_ontology:measurement(commerce_clause_narrow_orig_tr_t1937, commerce_clause_scope__narrow_originalist, theater_ratio, 1937, 0.35).
narrative_ontology:measurement(commerce_clause_narrow_orig_tr_t1995, commerce_clause_scope__narrow_originalist, theater_ratio, 1995, 0.12).
narrative_ontology:measurement(commerce_clause_narrow_orig_tr_t2025, commerce_clause_scope__narrow_originalist, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(commerce_clause_narrow_orig_be_t1789, commerce_clause_scope__narrow_originalist, base_extractiveness, 1789, 0.1).
narrative_ontology:measurement(commerce_clause_narrow_orig_be_t1830, commerce_clause_scope__narrow_originalist, base_extractiveness, 1830, 0.12).
narrative_ontology:measurement(commerce_clause_narrow_orig_be_t1890, commerce_clause_scope__narrow_originalist, base_extractiveness, 1890, 0.18).
narrative_ontology:measurement(commerce_clause_narrow_orig_be_t1937, commerce_clause_scope__narrow_originalist, base_extractiveness, 1937, 0.05).
narrative_ontology:measurement(commerce_clause_narrow_orig_be_t1995, commerce_clause_scope__narrow_originalist, base_extractiveness, 1995, 0.22).
narrative_ontology:measurement(commerce_clause_narrow_orig_be_t2025, commerce_clause_scope__narrow_originalist, base_extractiveness, 2025, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(commerce_clause_narrow_orig_su_t1789, commerce_clause_scope__narrow_originalist, suppression_requirement, 1789, 0.2).
narrative_ontology:measurement(commerce_clause_narrow_orig_su_t1830, commerce_clause_scope__narrow_originalist, suppression_requirement, 1830, 0.25).
narrative_ontology:measurement(commerce_clause_narrow_orig_su_t1890, commerce_clause_scope__narrow_originalist, suppression_requirement, 1890, 0.4).
narrative_ontology:measurement(commerce_clause_narrow_orig_su_t1937, commerce_clause_scope__narrow_originalist, suppression_requirement, 1937, 0.15).
narrative_ontology:measurement(commerce_clause_narrow_orig_su_t1995, commerce_clause_scope__narrow_originalist, suppression_requirement, 1995, 0.3).
narrative_ontology:measurement(commerce_clause_narrow_orig_su_t2025, commerce_clause_scope__narrow_originalist, suppression_requirement, 2025, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__narrow_originalist, information_standard).
narrative_ontology:boltzmann_floor_override(commerce_clause_scope__narrow_originalist, 0.02).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, commerce_clause_scope__broad_effects_test).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, commerce_clause_scope__intermediate_channels).

% DUAL FORMULATION NOTE:
% This constraint family (commerce_clause_scope) decomposes the single label 'Commerce Clause' into three structurally distinct readings with different ε, beneficiaries, victims, and types. The narrow_originalist reading has ε=0.28 (low extraction from state sovereignty), beneficiaries = state governments/local businesses, victims = citizens in recalcitrant states/national businesses/federal agencies. The broad_effects_test reading has ε≈0.65 (high federal extraction from state autonomy), beneficiaries = national regulatory interests, victims = state governments. The intermediate_channels reading sits between. They are linked via affects_constraints because each reading's doctrinal viability affects the others' — e.g., narrow_originalist gains ground when intermediate_channels proves unstable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commerce_clause_scope__narrow_originalist, institutional, 0.15).
constraint_indexing:directionality_override(commerce_clause_scope__narrow_originalist, powerless, 0.95).
constraint_indexing:directionality_override(commerce_clause_scope__narrow_originalist, powerful, 0.75).
constraint_indexing:directionality_override(commerce_clause_scope__narrow_originalist, organized, 0.4).
constraint_indexing:directionality_override(commerce_clause_scope__narrow_originalist, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
