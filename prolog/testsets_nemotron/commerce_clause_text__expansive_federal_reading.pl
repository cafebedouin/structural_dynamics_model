% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__expansive_federal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__expansive_federal_reading, []).

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
 *   constraint_id: commerce_clause_text__expansive_federal_reading
 *   human_readable: Commerce Clause — Expansive Federal Reading (Aggregate Effects Doctrine)
 *   domain: constitutional_law/federalism/commerce_regulation
 *
 * SUMMARY:
 *   The expansive federal reading of the Commerce Clause — crystallized in
 *   Wickard v. Filburn (1942) and sustained through the Civil Rights era, the
 *   New Deal regulatory state, and the modern administrative state — holds
 *   that Congress may regulate any economic activity whose aggregate effects
 *   substantially affect interstate commerce. This reading operates as a
 *   tangled rope: it solves a genuine coordination problem (national economic
 *   integration, preventing state-level free-riding and races to the bottom)
 *   while simultaneously extracting regulatory authority from states and
 *   imposing compliance costs on intrastate actors who would prefer local
 *   variation. The coordination function is real and acknowledged by all
 *   sides; the extraction is structural and asymmetric — the federal
 *   administrative state and national policy coalitions gain capacity and
 *   coherence; states and local communities lose autonomous regulatory space.
 *   The constraint requires active enforcement (judicial precedent, federal
 *   preemption doctrine, administrative rulemaking) and has no sunset clause.
 *   The claimed type (tangled_rope) reflects the structural reality of
 *   simultaneous coordination and extraction; the metrics describe the degree
 *   of each.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__expansive_federal_reading, 0.68).
domain_priors:suppression_score(commerce_clause_text__expansive_federal_reading, 0.72).
domain_priors:theater_ratio(commerce_clause_text__expansive_federal_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__expansive_federal_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__expansive_federal_reading, "Commerce Clause — Expansive Federal Reading (Aggregate Effects Doctrine)").
narrative_ontology:topic_domain(commerce_clause_text__expansive_federal_reading, "constitutional_law/federalism/commerce_regulation").

domain_priors:requires_active_enforcement(commerce_clause_text__expansive_federal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__expansive_federal_reading, 'c7c48fd0-196c-427b-b33a-e7de83f4d6ad').
narrative_ontology:cs_kernel_codification('c7c48fd0-196c-427b-b33a-e7de83f4d6ad', fixed_text).
narrative_ontology:cs_authority_grounding('c7c48fd0-196c-427b-b33a-e7de83f4d6ad', lineage).
narrative_ontology:cs_interpretation_layer_present('c7c48fd0-196c-427b-b33a-e7de83f4d6ad').
narrative_ontology:cs_reading_relation('c7c48fd0-196c-427b-b33a-e7de83f4d6ad', commerce_clause_text__originalist_narrow_reading, coexists_with).
narrative_ontology:cs_reading_relation('c7c48fd0-196c-427b-b33a-e7de83f4d6ad', commerce_clause_text__substantial_effects_limited_reading, influences).
narrative_ontology:cs_axiom('c7c48fd0-196c-427b-b33a-e7de83f4d6ad', foundational, aggregate_effects_suffices_for_federal_power).
narrative_ontology:cs_axiom_status(aggregate_effects_suffices_for_federal_power, holdable).
narrative_ontology:cs_axiom_grounding('c7c48fd0-196c-427b-b33a-e7de83f4d6ad', aggregate_effects_suffices_for_federal_power, conventional).
narrative_ontology:cs_axiom('c7c48fd0-196c-427b-b33a-e7de83f4d6ad', foundational, national_economic_integration_justifies_comprehensive_regulation).
narrative_ontology:cs_axiom_status(national_economic_integration_justifies_comprehensive_regulation, holdable).
narrative_ontology:cs_axiom_grounding('c7c48fd0-196c-427b-b33a-e7de83f4d6ad', national_economic_integration_justifies_comprehensive_regulation, instrumental).
narrative_ontology:cs_reference_frame('c7c48fd0-196c-427b-b33a-e7de83f4d6ad', new_deal_settlement).
narrative_ontology:cs_drift_state('c7c48fd0-196c-427b-b33a-e7de83f4d6ad', post_lopez_morrison_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c7c48fd0-196c-427b-b33a-e7de83f4d6ad', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__expansive_federal_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, federal_administrative_state).
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, national_policy_coherence_advocates).
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, congress_comprehensive_regulation_proponents).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, state_autonomy_in_economic_regulation).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, local_economic_variation).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, intrastate_business_operators_subject_to_federal_preemption).
narrative_ontology:constraint_vindicates(commerce_clause_text__expansive_federal_reading, national_economic_integration_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_text__expansive_federal_reading, federal_supremacy_in_commercial_regulation).
narrative_ontology:constraint_vindicates(commerce_clause_text__expansive_federal_reading, aggregate_effects_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the regulatory apparatus that implements comprehensive federal economic regulation under the commerce power. Gains institutional capacity, budgetary resources, and policy reach from the expansive reading. Can shift regulatory frameworks across administrations; exit means shrinking the administrative state itself.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, federal_administrative_state, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__expansive_federal_reading, federal_administrative_state, beneficiary).

% Legislators and coalition-builders who rely on the commerce power to enact national solutions to economic problems (labor standards, environmental protection, civil rights, healthcare). Benefit from a unified national regulatory floor. Their exit is political — they can advocate for narrower readings but lose the legislative tool.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, congress_comprehensive_regulation_proponents, beneficiary,
    institutional, biographical, mobile, national).

% Interest groups, think tanks, and policy networks that benefit from avoiding a patchwork of state regulations. Gain predictability and scale from federal uniformity. Can shift advocacy to state level if the federal reading narrows, but lose economies of scale.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, national_policy_coherence_advocates, beneficiary,
    organized, biographical, mobile, national).

% State governments that lose regulatory authority over economic activity deemed to have substantial aggregate effects. Their autonomy is subordinated to federal preemption. Exit is constrained — they cannot opt out of federal supremacy, but can resist through litigation, non-cooperation, or constitutional amendment campaigns.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, state_autonomy_in_economic_regulation, payer,
    organized, generational, constrained, regional).

% Communities and local economies that would experiment with different regulatory models (labor standards, environmental rules, licensing regimes) but are preempted by federal occupation of the field. Exit means relocating or accepting uniformity; the constraint eliminates the laboratory-of-democracy function for covered activity.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, local_economic_variation, payer,
    moderate, biographical, constrained, local).

% Businesses operating within a single state that become subject to federal regulation because their activity, in aggregate, substantially affects interstate commerce. Bear compliance costs and lose the ability to lobby state-level exemptions. Exit is constrained — they cannot restructure to avoid the aggregate-effects test without ceasing operations.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, intrastate_business_operators_subject_to_federal_preemption, payer,
    moderate, biographical, constrained, national).

% Judicial actors and legal scholars committed to a narrow originalist reading of the commerce clause. Their interpretive framework is structurally excluded from operational dominance while the expansive reading controls precedent. Exit would require abandoning their methodological identity; they remain as a persistent dissenting faction.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, originalist_judges_and_scholars, excluded,
    powerful, generational, identity_locked, national).

% Academic commentators, comparative constitutionalists, and institutional analysts who track the doctrine's evolution without holding a stake in its operational outcome. They see the full structural pattern including the coordinate extraction of federal capacity from state autonomy.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, constitutional_law_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of national economic integration: prevents states from free-riding on each other's regulations, blocks regulatory races to the bottom, and enables coherent national markets for labor, goods, and capital.
% TRANSFER_FUNCTION: Transfers regulatory authority from state legislatures and local governments to the federal administrative state and Congress. Moves the power to set economic rules — and the compliance costs of those rules — from decentralized state-level processes to centralized federal rulemaking. The federal government gains capacity; states lose autonomy; local variation is suppressed.
% ABSENT_VOICES: State and local governments that would regulate differently if not preempted; small businesses that would benefit from state-level exemptions; citizens in states whose policy preferences diverge from the national median but are bound by federal uniformity. These voices are structurally absent because the constraint's enforcement (federal preemption) operates precisely to override them.
% DISAPPEARANCE_RATIONALE: If the aggregate-effects doctrine vanished overnight, the federal regulatory state would lose its constitutional basis for vast swathes of economic regulation (labor, environment, healthcare, finance, civil rights). States would immediately reassert regulatory authority, creating a patchwork of standards. The national market would fragment; compliance costs would shift from federal to multi-state. The administrative state would shrink dramatically. The world rearranges because the constraint is the load-bearing wall of the modern federal regulatory architecture.
% FOUNDING_PROBLEM: The Articles of Confederation failed because states could not coordinate on national economic policy — trade barriers between states, inability to regulate interstate commerce collectively, no power to prevent races to the bottom. The commerce clause was designed to give the national government authority over genuinely interstate commercial problems.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars (e.g., Randy Barnett, Richard Epstein) and the originalist_narrow_reading faction attest the founding problem was narrow: only trade crossing state borders. New Deal historians and the expansive_federal_reading faction attest the founding problem was broad: any economic coordination failure with national spillovers. The substantial_effects_limited_reading faction (e.g., Lopez/Morrison majority) attests the founding problem was real but the solution has a jurisdictional limit. Corroboration comes from the text of the Constitution itself ('commerce among the several states'), the Federalist Papers (Federalist 42 on the commerce power's scope), and the historical record of the Articles' failures — sources outside any single reading's beneficiary set.
narrative_ontology:disappearance_verdict(commerce_clause_text__expansive_federal_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__expansive_federal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__expansive_federal_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(commerce_clause_text__expansive_federal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__expansive_federal_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__expansive_federal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_text__expansive_federal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_text__expansive_federal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is substantial but not maximal — the constraint enables massive federal regulation but also delivers genuine coordination value (national markets, civil rights enforcement, environmental protection). Suppression (0.72) is higher than extraction because the constraint's persistence depends on active judicial and institutional enforcement to maintain federal preemption against state resistance and originalist challenges. Theater ratio (0.28) is moderate — the coordination function is real, but a growing share of doctrinal complexity serves to defend the reading's boundaries against narrowing pressures rather than to solve coordination problems. Accessibility collapse (0.62) reflects that alternatives (state-level regulation) are legally foreclosed by preemption but politically imaginable. Resistance (0.55) captures persistent but unsuccessful state-level pushback (Lopez, Morrison, NFIB v. Sebelius, major questions doctrine) that has narrowed but not overturned the reading.
 *
 * PERSPECTIVAL GAP:
 *   From the federal administrative state's seat, the constraint is a rope (genuine coordination, net beneficiary). From state autonomy's seat, it is a snare (pure extraction, suppressed alternatives). From the observer seat, it is a tangled rope (both functions structurally present). The engine computes this divergence from the stakeholder power/exit/role data. The claimed_type = tangled_rope reflects the observer/analytical seat's structural truth; individual seats will compute differently.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal administrative state and congressional proponents are structural beneficiaries (d near 0.15) — they collect regulatory capacity and policy coherence. States and intrastate operators are structural targets (d near 0.85) — they bear preemption and compliance costs with constrained exit. Originalist judges are identity-locked excluded (d ≈ 0.5 but identity_locked raises effective extraction) — their methodological commitment prevents exit from the dispute even as their reading loses operational dominance. Observers sit at d=0.5. The derivation chain produces these directionalities from the declared beneficiary/victim structure and exit options; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (national economic coordination under the Articles) was real. The expansive reading solved it but then expanded beyond it — the aggregate-effects test now covers activity the Founders would not have recognized as commerce. The mandate has partially atrophied (original coordination problem largely solved by national market integration) but the constraint persists and grows because the federal administrative state extracts ongoing benefit from the expanded reading. This is not pure mandatrophy (the coordination function remains live for new problems like climate regulation, digital markets, pandemic response) but it is mandatrophy-adjacent: the constraint's scope exceeds its founding justification, and the excess scope is extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'Where does the genuine coordination function of national economic integration end and pure extraction of state regulatory authority begin? Is there a principled stopping point within the aggregate-effects test, or does the coordination rationale expand to cover any federal regulatory preference?',
    'Doctrinal analysis of limiting principles in Lopez, Morrison, NFIB v. Sebelius, and the major questions doctrine; empirical study of whether federal regulations passed under the commerce power since 1995 address genuine collective-action problems or merely federal policy preferences.',
    'If no principled boundary exists, the reading collapses toward snare (coordination becomes pretext). If a stable boundary exists (e.g., the substantial-effects test plus jurisdictional nexus), the tangled_rope classification holds with a definable coordination core.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the expansive reading has a structural boundary between coordination and extraction').

omega_variable(
    kernel_commitment_framing,
    'Does the commerce_clause_text kernel support one authoritative reading, or is it genuinely distributed such that multiple readings coexist as legitimate interpretations within the same constitutional framework?',
    'Analysis of the constitutional text''s original public meaning, the structure of the federal system, and the institutional practice of judicial review — whether the kernel''s authority_grounding is lineage (single chain), extraction (institutional capture), or distributed (no authoritative interpreter).',
    'If the kernel is distributed, all three readings are structurally legitimate and the engine''s per-seat divergence is the correct model. If the kernel has a single authoritative reading (lineage or extraction), the other readings are structurally foreclosed or captured — changing the mandate''s legitimacy analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_commitment_framing, conceptual, 'Framing of the kernel''s authority structure — single vs. distributed legitimacy').

omega_variable(
    state_resistance_effectiveness,
    'Can state-level resistance (litigation, non-cooperation, constitutional amendment campaigns, interstate compacts) meaningfully constrain the expansive reading''s extraction, or is resistance performative given the federal judiciary''s institutional commitment to the doctrine?',
    'Track state wins/losses in commerce clause cases since Lopez (1995); measure compliance costs of federal mandates on states; assess whether the anti-commandeering doctrine (Printz, Murphy) creates genuine exit options for states.',
    'If resistance is effective, the constraint''s suppression is lower and states have more exit than modeled. If resistance is performative, suppression is higher and the constraint trends toward snare from the state seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_resistance_effectiveness, empirical, 'Whether state resistance to federal commerce power is structurally consequential or theatrical').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__expansive_federal_reading, 1937, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1937, commerce_clause_text__expansive_federal_reading, theater_ratio, 1937, 0.12).
narrative_ontology:measurement(comm_tr_t1942, commerce_clause_text__expansive_federal_reading, theater_ratio, 1942, 0.18).
narrative_ontology:measurement(comm_tr_t1964, commerce_clause_text__expansive_federal_reading, theater_ratio, 1964, 0.22).
narrative_ontology:measurement(comm_tr_t1976, commerce_clause_text__expansive_federal_reading, theater_ratio, 1976, 0.25).
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_text__expansive_federal_reading, theater_ratio, 1995, 0.27).
narrative_ontology:measurement(comm_tr_t2005, commerce_clause_text__expansive_federal_reading, theater_ratio, 2005, 0.26).
narrative_ontology:measurement(comm_tr_t2012, commerce_clause_text__expansive_federal_reading, theater_ratio, 2012, 0.27).
narrative_ontology:measurement(comm_tr_t2026, commerce_clause_text__expansive_federal_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(comm_be_t1937, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1937, 0.35).
narrative_ontology:measurement(comm_be_t1942, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1942, 0.52).
narrative_ontology:measurement(comm_be_t1964, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1964, 0.61).
narrative_ontology:measurement(comm_be_t1976, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1976, 0.65).
narrative_ontology:measurement(comm_be_t1995, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1995, 0.63).
narrative_ontology:measurement(comm_be_t2005, commerce_clause_text__expansive_federal_reading, base_extractiveness, 2005, 0.66).
narrative_ontology:measurement(comm_be_t2012, commerce_clause_text__expansive_federal_reading, base_extractiveness, 2012, 0.67).
narrative_ontology:measurement(comm_be_t2026, commerce_clause_text__expansive_federal_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1937, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1937, 0.45).
narrative_ontology:measurement(comm_su_t1942, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1942, 0.58).
narrative_ontology:measurement(comm_su_t1964, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1964, 0.65).
narrative_ontology:measurement(comm_su_t1976, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1976, 0.68).
narrative_ontology:measurement(comm_su_t1995, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1995, 0.7).
narrative_ontology:measurement(comm_su_t2005, commerce_clause_text__expansive_federal_reading, suppression_requirement, 2005, 0.71).
narrative_ontology:measurement(comm_su_t2012, commerce_clause_text__expansive_federal_reading, suppression_requirement, 2012, 0.71).
narrative_ontology:measurement(comm_su_t2026, commerce_clause_text__expansive_federal_reading, suppression_requirement, 2026, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__expansive_federal_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(commerce_clause_text__expansive_federal_reading, 0.15).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, commerce_clause_text__originalist_narrow_reading).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, commerce_clause_text__substantial_effects_limited_reading).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, federal_preemption_doctrine).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, administrative_state_legitimacy).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, anti_commandeering_doctrine).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, dormant_commerce_clause).

% DUAL FORMULATION NOTE:
% This constraint is one member of the commerce_clause_text constraint family (kernel_id: commerce_clause_text). The family decomposes the single constitutional text into three structurally distinct constraints with different ε values, beneficiary/victim structures, and claimed types. The expansive reading has the highest extractiveness and broadest scope; the originalist reading has near-zero extractiveness but minimal coordination function; the substantial-effects-limited reading sits between. All three are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
