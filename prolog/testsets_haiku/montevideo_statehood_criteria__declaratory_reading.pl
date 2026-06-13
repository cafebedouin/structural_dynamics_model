% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__declaratory_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__declaratory_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: montevideo_statehood_criteria__declaratory_reading
 *   human_readable: Montevideo Criteria Declaratory Reading: Objective Statehood as Legal Fact
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   The declaratory reading of the Montevideo statehood criteria claims that
 *   meeting four objective conditions—defined territory, permanent
 *   population, effective government, capacity to enter foreign
 *   relations—ipso facto establishes statehood as a legal fact, independent
 *   of recognition by existing states. This reading authorizes de facto
 *   authorities and self-determination movements to claim statehood without
 *   external permission, removing the veto power of parent states and hostile
 *   recognizing powers. The claim/metric gap is intentional: this reading is
 *   CLAIMED as a natural law (objective criteria are structural facts of
 *   political reality), yet the measurement metrics show substantial
 *   extraction and suppression because the reading operates as enforcement
 *   machinery against constitutive and hybrid readings—existing states must
 *   actively suppress normative gates and recognition conditioning, and the
 *   reading's operation is partly theatrical (recognition ceremonies that
 *   appear to constitute statehood while the reading insists they merely
 *   declare it). The claim says 'this is how law works'; the metrics measure
 *   'this is how this particular reading's victory over alternatives is
 *   defended.'
 *
 * KEY AGENTS:
 *   - de_facto_authorities_meeting_criteria: breakaway territories and independence movements that benefit from objective criteria, removing parent-state vetoes
 *   - self_determination_movements: identity-locked actors whose exit attempt is legitimated by the declaratory reading as a legal entitlement
 *   - established_states__recognizing_powers: institutional agenda-setters whose structural leverage erodes because recognition becomes performative
 *   - parent_states__colonial_powers: payers who lose the veto over statehood through non-recognition
 *   - international_law_positivists: beneficiaries whose epistemology is vindicated by the reading's objective-criteria framing
 *   - human_rights_advocates__normativists: excluded voices who would inject legitimacy gates and are structurally harmed by objective criteria
 *   - geopolitical_rivals__regional_powers: payers whose regional leverage is stripped by removal of recognition veto
 *   - analytical_observer: sees the full structure and can measure the constraint's operation across both readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__declaratory_reading, 0.62).
domain_priors:suppression_score(montevideo_statehood_criteria__declaratory_reading, 0.58).
domain_priors:theater_ratio(montevideo_statehood_criteria__declaratory_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__declaratory_reading, mountain).
narrative_ontology:human_readable(montevideo_statehood_criteria__declaratory_reading, "Montevideo Criteria Declaratory Reading: Objective Statehood as Legal Fact").
narrative_ontology:topic_domain(montevideo_statehood_criteria__declaratory_reading, "international_law/political_philosophy").

domain_priors:emerges_naturally(montevideo_statehood_criteria__declaratory_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__declaratory_reading, '7951757f-85ab-4f3c-bd4c-8a43b8ad5c9b').
narrative_ontology:cs_kernel_codification('7951757f-85ab-4f3c-bd4c-8a43b8ad5c9b', fixed_text).
narrative_ontology:cs_authority_grounding('7951757f-85ab-4f3c-bd4c-8a43b8ad5c9b', lineage).
narrative_ontology:cs_interpretation_layer_present('7951757f-85ab-4f3c-bd4c-8a43b8ad5c9b').
narrative_ontology:cs_reading_relation('7951757f-85ab-4f3c-bd4c-8a43b8ad5c9b', montevideo_statehood_criteria__constitutive_reading, coexists_with).
narrative_ontology:cs_reading_relation('7951757f-85ab-4f3c-bd4c-8a43b8ad5c9b', montevideo_statehood_criteria__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('7951757f-85ab-4f3c-bd4c-8a43b8ad5c9b', foundational, statehood_objective_fact_not_gift).
narrative_ontology:cs_axiom_status(statehood_objective_fact_not_gift, holdable).
narrative_ontology:cs_axiom_grounding('7951757f-85ab-4f3c-bd4c-8a43b8ad5c9b', statehood_objective_fact_not_gift, empirically_contingent).
narrative_ontology:cs_axiom('7951757f-85ab-4f3c-bd4c-8a43b8ad5c9b', foundational, recognition_declaratory_not_constitutive).
narrative_ontology:cs_axiom_status(recognition_declaratory_not_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('7951757f-85ab-4f3c-bd4c-8a43b8ad5c9b', recognition_declaratory_not_constitutive, deontological).
narrative_ontology:cs_reference_frame('7951757f-85ab-4f3c-bd4c-8a43b8ad5c9b', objective_positivist_statehood).
narrative_ontology:cs_drift_state('7951757f-85ab-4f3c-bd4c-8a43b8ad5c9b', contemporary_mixed_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7951757f-85ab-4f3c-bd4c-8a43b8ad5c9b', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, de_facto_authorities_meeting_criteria).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, self_determination_movements).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, international_law_positivists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, established_states__recognizing_powers).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, parent_states__colonial_powers).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, human_rights_advocates__normativists).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, geopolitical_rivals__regional_powers).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__declaratory_reading, legal_positivism).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__declaratory_reading, objective_statehood_doctrine).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__declaratory_reading, sovereignty_as_fact).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Entities that control defined territory, have a permanent population, exercise effective government, and engage in foreign relations. Under the declaratory reading, they are ipso facto states once the four criteria are met, regardless of whether existing states grant recognition. Their status is vindicated by international law itself, not by external consensus. They benefit from the reading because it removes the veto power of parent states or hostile powers over their statehood claim.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, de_facto_authorities_meeting_criteria, beneficiary,
    moderate, generational, trapped, global).

% Groups seeking independence (ethnic nations, colonial territories, regional separatists) use the declaratory reading as a normative shield: they argue that if they meet the objective criteria, international law mandates recognition regardless of parent-state resistance. The reading legitimates their exit attempt as a legal entitlement, not a political negotiation. Their exit is identity-locked because disengaging from the independence project means dissolving the identity the movement itself constitutes.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, self_determination_movements, beneficiary,
    powerful, generational, identity_locked, national).

% Sovereign states that recognize or withhold recognition. Under the declaratory reading, their recognition becomes legally redundant once the criteria are met—a performative act, not a constitutive one. They retain de facto leverage (membership in institutions, trade, security coordination) but lose the structural power to define statehood itself. Some states pay a cost: parent states lose leverage over secession movements; regional powers lose the ability to veto inconvenient new states.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, established_states__recognizing_powers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__declaratory_reading, established_states__recognizing_powers, payer).

% States claiming sovereignty over territories whose populations seek independence. The declaratory reading strips them of the veto: once a breakaway authority meets the four criteria, it is a state in international law, and the parent state cannot prevent that status through non-recognition alone. Their structural leverage erodes because the criteria are objective—not subject to their agreement.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, parent_states__colonial_powers, payer,
    institutional, generational, constrained, global).

% Legal scholars, judges, and institutional actors who hold that international law derives from objective criteria and formal rules, not from political consensus or normative legitimacy assessment. The declaratory reading vindicates their entire epistemology: law is self-executing, states are facts, recognition is declaratory not constitutive. They benefit structurally because their interpretive authority is elevated—they determine whether criteria are met, which is now the determinative question.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, international_law_positivists, beneficiary,
    powerful, generational, mobile, global).

% Actors who argue statehood should require not just objective control but normative legitimacy: democratic governance, human rights compliance, non-aggression. The declaratory reading excludes them from the gate: criteria are objective, not normative. They are structurally harmed because authoritarian regimes, ethnic cleansers, and aggressive powers that nonetheless control territory and population can achieve statehood legally under the objective criteria alone. Their voice would inject legitimacy checks that would re-gate statehood on values, not facts.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, human_rights_advocates__normativists, excluded,
    powerful, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__declaratory_reading, human_rights_advocates__normativists, payer).

% Powers whose regional interests are served by the ability to refuse recognition and prevent a breakaway territory's international standing. The declaratory reading removes that leverage: rival states cannot veto statehood through non-recognition. They can deny membership in their own alliances, security arrangements, or trade blocs, but they cannot prevent the target's legal status as a state.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, geopolitical_rivals__regional_powers, payer,
    institutional, generational, constrained, global).

% Sees the full structure: the constraint operates as a legal principle, vindicating objective criteria as dispositive. Can observe the clash between declaratory and constitutive readings and measure which reading's operational framing dominates in actual recognition practice.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(montevideo_statehood_criteria__declaratory_reading, international_law_positivists).
narrative_ontology:fixing_cost_class(montevideo_statehood_criteria__declaratory_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a self-executing legal standard for statehood that does not depend on consensus of existing powers. Solves the coordination problem: how do new political units enter the international system without being subject to veto by those already inside? By making statehood a fact (meeting criteria) rather than a gift (granted by recognition), the reading enables predictable expansion of the state system.
% TRANSFER_FUNCTION: Transfers the power to determine statehood from the existing state system (recognition-granting powers) to objective criteria themselves. Existing states lose the structural leverage to condition statehood on their approval. De facto authorities that meet criteria gain automatic legal standing regardless of external consent. The transfer is of recognition authority, not material goods—it is a shift in who decides, not what is distributed.
% ABSENT_VOICES: Normativists (human rights advocates, governance legitimacy theorists) would argue that objective criteria are insufficient and dangerous—that statehood should also require democratic legitimacy, human rights compliance, and non-aggressive intent. They are excluded from the declaratory-reading gate because the reading is specifically about objective criteria, not normative assessment. Their objection would reverse the reading back toward constitutive or hybrid framings.
% DISAPPEARANCE_RATIONALE: If the declaratory reading disappeared and statehood reverted to pure constitutive framing (recognition as constitutive), the map would reorganize: breakaway territories that meet objective criteria but lack international recognition would lose legal standing; parent states and regional powers would regain veto leverage over secession; the international system would contract to only those entities recognized by existing powers. Decolonization and self-determination would become political negotiations rather than legal entitlements.
% FOUNDING_PROBLEM: Before the Montevideo Convention (1933), statehood was ambiguous: a new political unit might control territory and exercise government, but its international legal status depended entirely on whether other states agreed to recognize it. This created a coordination failure: no objective standard existed, so powerful states could indefinitely refuse recognition and deny standing to inconvenient political units. The founding problem was: how can international law operate as law if its foundational category (statehood) depends on political consensus rather than objective criteria?
% FOUNDING_PROBLEM_CORROBORATION: Positivist international lawyers (Hans Kelsen, Antonio Cassese, James Crawford in his declaratory mode) attest that objective criteria solve the coordination problem and enable international law to function as law. Constitutivists and hybrid-reading advocates attest that the founding problem is NOT solved by objective criteria alone—that purely objective statehood admits brutal regimes, violates sovereignty norms, and requires normative gates. States in practice show mixed commitment: they recognize some objective-criteria-meeting entities (Palestine, Kosovo, South Sudan) while denying recognition to others, suggesting the founding problem remains live and contested across both readings.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__declaratory_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__declaratory_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__declaratory_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(montevideo_statehood_criteria__declaratory_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__declaratory_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__declaratory_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, ExtMetricName, E),
    domain_priors:suppression_score(montevideo_statehood_criteria__declaratory_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(montevideo_statehood_criteria__declaratory_reading),
    narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(montevideo_statehood_criteria__declaratory_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed type is mountain because the reading asserts that the four criteria are objective facts of political reality—territorial control, population, effective government, and foreign-relations capacity are empirical realities, not matters of opinion. The reading's core premise is that these objective facts CONSTITUTE statehood; recognition merely DECLARES what already is. However, the metrics show substantial extraction (0.62) and suppression (0.58) because the declaratory reading must actively suppress its competing reading (the constitutive reading). To maintain the objective-criteria framing, the reading must: (1) suppress normative gates (delegitimize the hybrid reading's human-rights checks); (2) suppress the constitutive reading's premise that recognition is the constitutive act; (3) maintain theater around recognition ceremonies (which appear to matter while the reading insists they don't). The theater_ratio of 0.41 reflects this: formal recognition remains ceremonially important even as the reading insists it is legally redundant. Accessibility_collapse (0.72) is high because once the four criteria are understood, alternatives collapse—no entity can escape statehood by refusing to meet the criteria, and no state can deny statehood by refusing recognition if criteria are met. Resistance (0.68) is substantial because constitutivists and normativists actively resist the declaratory framing in academic discourse, state practice, and institutional settings. The measurement series show extraction rising and plateauing (reaching 0.62 by time 60 and holding steady), indicating the reading's framing stabilized after a period of contestation—the interpretive victory was won, but must be actively maintained.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (parent_states, geopolitical_rivals, human_rights_advocates) should compute as experiencing extraction and suppression from this reading. The beneficiary seats (de_facto_authorities, self_determination_movements, international_law_positivists) should compute as experiencing the reading as a natural law—a structural fact they depend on and that serves them. The established_states seat is dual-positioned: as agenda-setters of the international system, they nominally benefit from objectifying statehood (it makes the system predictable and rule-based); as constrained by the loss of recognition veto, they pay. The engine's per-seat computation should reveal this divergence: beneficiaries see mountain (low extraction, high accessibility_collapse, vindication of their interests), while payers see snare or tangled_rope (they are forced to accept the criteria as dispositive, cannot escape by normative re-framing, and must actively suppress alternatives to keep the reading in place).
 *
 * DIRECTIONALITY LOGIC:
 *   De_facto_authorities_meeting_criteria: full beneficiary (d ≈ 0.0–0.2). The reading vindicates their statehood claim as an objective legal fact, removing external veto. Their exit is trapped (they must govern the territory they claim or lose the criteria), but the reading removes the secondary barrier of non-recognition. Self_determination_movements: beneficiary (d ≈ 0.1–0.3), but identity-locked (the movement cannot exit the independence project without dissolving its identity). International_law_positivists: beneficiary (d ≈ 0.05–0.25), mobile exit (they can switch scholarly frameworks if international law changes), but benefit from vindication of their epistemology. Established_states__recognizing_powers: symmetric to slightly extractive (d ≈ 0.4–0.6). They lose leverage but retain de facto influence (membership, trade, security). Parent_states__colonial_powers: extractive (d ≈ 0.7–0.85). They lose the veto over secession and are forced to accept the criteria as legally determinative. Human_rights_advocates__normativists: extractive (d ≈ 0.75–0.9). They are excluded from the gate and their normative input is suppressed by the objective-criteria framing. Geopolitical_rivals__regional_powers: extractive (d ≈ 0.65–0.8). They lose regional leverage over breakaway territories. No directionality_overrides are needed; the structural derivation from beneficiary/victim + exit options produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is claimed as a mountain (natural law of objective statehood), and the mandatrophy question is: has the founding problem (how to establish statehood without veto by existing states) outlived its function? The reading says no—the problem is live because parent states and regional powers still attempt to condition statehood on recognition. The constitutive reading says yes—the problem is dead or moot because in practice, recognition remains powerful (Palestine, Kosovo, Taiwan, Northern Cyprus all show that objective criteria without recognition deny real international standing). The measurement data (extraction and suppression holding steady at 0.62 and 0.58 after reaching plateau at t=60) suggests the reading's victory is stable but requires active maintenance—mandatrophy is NOT resolved. The founding problem remains contested: positivists insist objective criteria solve it (the reading is live and necessary); constitutivists and normativists insist recognition remains the real gate (the reading is theater covering politics). No single mountain-vs-snare classification will satisfy both readings' framings—the engine should compute the type from each reading's own seat. From the positivist seat: mountain (objective criteria are natural facts). From the parent-state/normativist seat: snare (the criteria are cover for a veto-stripping mechanism).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    objective_vs_subjective_criteria_boundary,
    'What counts as ''objective'' in the criteria? Can ''effective government'' and ''capacity to enter foreign relations'' be assessed without normative judgment about legitimacy, democratic process, or human rights?',
    'Case analysis: apply the four criteria to borderline cases (Palestine, Kosovo, Northern Cyprus, Transnistria, Donbas republics) and observe whether assessors diverge on whether criteria are met. If divergence tracks beliefs about legitimacy rather than factual control, the criteria are not objective in the positivist sense.',
    'If criteria prove subjective (assessment depends on legitimacy judgment), the reading collapses toward hybrid or constitutive: objectivity was the defining claim. If criteria hold objective despite legitimacy divergence, the reading stands and normativists must accept outcomes they reject.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(objective_vs_subjective_criteria_boundary, conceptual, 'Whether the four criteria can be assessed objectively or collapse into normative judgment.').

omega_variable(
    recognition_as_declaratory_vs_constitutive,
    'In actual state practice, does recognition function as a declaratory act (acknowledging existing statehood) or a constitutive act (bringing statehood into being)? Do states treat recognition as redundant once criteria are met, or as the actual gate?',
    'Institutional analysis: measure whether states grant full diplomatic standing (UN seat, treaty participation, access to international courts) to criteria-meeting entities without recognition; observe whether recognition is sought after criteria are met or before; examine state statements about WHY recognition is granted or withheld.',
    'If recognition is truly declaratory, the reading is vindicated and constitutivists are engaged in theater. If recognition is constitutive in practice (criteria-meeting entities without recognition lack standing), the reading is vindicated in theory but falsified in operation—it becomes a snare (claiming objectivity while maintaining gatekeeping).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(recognition_as_declaratory_vs_constitutive, empirical, 'Whether recognition actually functions as the reading claims (declaratory) or operates differently (constitutive).').

omega_variable(
    normative_legitimacy_suppression_mechanism,
    'Does the declaratory reading''s suppression of normative gates constitute a structural feature (objective criteria cannot logically include values) or an arbitrary exclusion (values could be incorporated into statehood law if the reading permitted it)?',
    'Comparative constitutional law: examine jurisdictions where legal status requirements DO include normative gates (e.g., EU membership requiring democracy and human rights); observe whether objective criteria and normative gates are logically incompatible or merely politically contested.',
    'If incompatible, the suppression is structural and the reading''s claim stands. If compatible, the suppression is strategic (the reading chose to exclude normative assessment to serve beneficiaries), shifting classification toward snare or tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normative_legitimacy_suppression_mechanism, conceptual, 'Whether suppression of normative gates is a logical necessity or a strategic choice.').

omega_variable(
    parent_state_structural_leverage_under_declaratory_rule,
    'Does the declaratory reading actually strip parent states of all leverage, or do parent states retain material power (military, economic, diplomatic) sufficient to prevent breakaway statehood even without recognition veto?',
    'Historical case analysis: compare territories that met criteria and lacked recognition (Palestine, Kosovo, Northern Cyprus) with those that gained recognition after parent-state acceptance or defeat. If parent states prevent criteria-meeting despite the reading''s legal claim, material power dominates the reading.',
    'If parent states retain sufficient leverage, the reading''s structural claim (removal of veto) is partly illusory—the reading shifts leverage but does not eliminate it. Parent states transition from recognition-gating to military/economic suppression, and the constraint becomes tangled_rope rather than pure natural law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(parent_state_structural_leverage_under_declaratory_rule, empirical, 'Whether the declaratory reading actually removes parent-state leverage or shifts its form.').

omega_variable(
    kernel_reading_contest_in_academic_and_institutional_framing,
    'Which reading (declaratory, constitutive, hybrid) dominates actual state practice and international institutional interpretation? Does the contest between readings show movement toward consensus on one reading, or stable coexistence?',
    'Institutional analysis: track UN recognition votes, ICJ opinions on statehood (Kosovo Advisory Opinion, Palestine status), and state practice regarding new entities claiming statehood over a 30-year interval. Measure the proportion of explicit statements endorsing each reading''s framing.',
    'Dominance by the declaratory reading would vindicate the natural-law claim. Dominance by the constitutive or hybrid reading would shift classification toward snare (the declaratory reading is a false summit, serving beneficiary interests while posing as law). Stable contest suggests mandatrophy is not resolved and both readings remain live.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_in_academic_and_institutional_framing, empirical, 'Whether the declaratory reading has won interpretive dominance or remains contested in state practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__declaratory_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t0, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(mont_tr_t0, observed).
narrative_ontology:measurement(mont_tr_t15, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 15, 0.31).
narrative_ontology:measurement_basis(mont_tr_t15, observed).
narrative_ontology:measurement(mont_tr_t30, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 30, 0.36).
narrative_ontology:measurement_basis(mont_tr_t30, observed).
narrative_ontology:measurement(mont_tr_t45, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 45, 0.4).
narrative_ontology:measurement_basis(mont_tr_t45, observed).
narrative_ontology:measurement(mont_tr_t60, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 60, 0.41).
narrative_ontology:measurement_basis(mont_tr_t60, observed).
narrative_ontology:measurement(mont_tr_t75, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 75, 0.41).
narrative_ontology:measurement_basis(mont_tr_t75, observed).
narrative_ontology:measurement(mont_tr_t90, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 90, 0.41).
narrative_ontology:measurement_basis(mont_tr_t90, observed).

% Extraction over time
narrative_ontology:measurement(mont_be_t0, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(mont_be_t0, observed).
narrative_ontology:measurement(mont_be_t15, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement_basis(mont_be_t15, observed).
narrative_ontology:measurement(mont_be_t30, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(mont_be_t30, observed).
narrative_ontology:measurement(mont_be_t45, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 45, 0.61).
narrative_ontology:measurement_basis(mont_be_t45, observed).
narrative_ontology:measurement(mont_be_t60, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement_basis(mont_be_t60, observed).
narrative_ontology:measurement(mont_be_t75, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 75, 0.62).
narrative_ontology:measurement_basis(mont_be_t75, observed).
narrative_ontology:measurement(mont_be_t90, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 90, 0.62).
narrative_ontology:measurement_basis(mont_be_t90, observed).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t0, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(mont_su_t0, observed).
narrative_ontology:measurement(mont_su_t15, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 15, 0.48).
narrative_ontology:measurement_basis(mont_su_t15, observed).
narrative_ontology:measurement(mont_su_t30, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(mont_su_t30, observed).
narrative_ontology:measurement(mont_su_t45, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 45, 0.55).
narrative_ontology:measurement_basis(mont_su_t45, observed).
narrative_ontology:measurement(mont_su_t60, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 60, 0.57).
narrative_ontology:measurement_basis(mont_su_t60, observed).
narrative_ontology:measurement(mont_su_t75, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 75, 0.58).
narrative_ontology:measurement_basis(mont_su_t75, observed).
narrative_ontology:measurement(mont_su_t90, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 90, 0.58).
narrative_ontology:measurement_basis(mont_su_t90, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__declaratory_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(montevideo_statehood_criteria__declaratory_reading, 0.12).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria__constitutive_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria__hybrid_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, unilateral_declaration_of_independence).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, self_determination_right_as_legal_entitlement).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, parent_state_territorial_integrity_doctrine).

% DUAL FORMULATION NOTE:
% The declaratory reading is one of three structurally distinct instantiations of the Montevideo kernel. The constitutive reading asserts statehood requires external recognition (different ε, different beneficiary/victim structure). The hybrid reading asserts statehood requires objectives criteria PLUS normative legitimacy (different ε, excludes both parent states and authoritarian de facto authorities). These are not perspectives on the same constraint—they are three different constraints with different natural-law vs. constructed character, different extraction profiles, and different victim sets. The declaratory reading affects both siblings by changing the landscape of interpretive authority and institutional practice: its dominance constrains the constitutive and hybrid readings' operational space, and conversely, contestation from those siblings constrains the declaratory reading's claim to objective naturalness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
