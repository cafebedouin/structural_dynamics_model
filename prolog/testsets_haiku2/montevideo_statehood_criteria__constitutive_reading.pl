% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__constitutive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__constitutive_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: montevideo_statehood_criteria__constitutive_reading
 *   human_readable: Constitutive Recognition Requirement for Statehood (Montevideo Doctrine Reading)
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   The constitutive reading of the Montevideo statehood criteria holds that
 *   an entity becomes a state only when recognized by the existing community
 *   of states, regardless of whether it meets the four objective criteria
 *   (defined territory, permanent population, effective government, capacity
 *   to enter relations). Under this reading, unrecognized polities that
 *   control territory and population remain in permanent legal limbo—they
 *   have no UN seat, no treaty standing, no international court access, and
 *   no immunity from military intervention. The established state community
 *   retains a structural veto over new state creation, preserving their
 *   gatekeeping power over the composition of the interstate system. This
 *   reading is contested against the declaratory reading (objective criteria
 *   establish statehood regardless of recognition) and the hybrid reading
 *   (objective criteria plus normative legitimacy requirements). The
 *   constraint story describes the constitutive reading's structural
 *   operation: who benefits (established states, their allies, the
 *   international financial system), who bears the cost (unrecognized
 *   polities, liberation movements, contested territories), and why the rule
 *   persists despite its victims.
 *
 * KEY AGENTS:
 *   - established_state_community: Institutional agenda-setter; benefits from recognition veto; retains unilateral gatekeeping power over new states
 *   - unrecognized_polities: Powerless payers; identity-locked (exit requires abandoning statehood claim); trapped in legal limbo despite meeting objective criteria
 *   - liberation_movements: Moderate-power payers; constrained exit (depend on patron or military success); blocked by parent-state veto within gatekeeping community
 *   - contested_territories: Moderate-power trapped payers; control territory and population but cannot access international legal standing
 *   - parent_states: Institutional beneficiaries; retain veto over secessionist recognition through the gatekeeping mechanism
 *   - great_power_consensus: Institutional agenda-setters; de facto control recognition outcomes through diplomatic signaling and UN Security Council coordination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__constitutive_reading, 0.68).
domain_priors:suppression_score(montevideo_statehood_criteria__constitutive_reading, 0.79).
domain_priors:theater_ratio(montevideo_statehood_criteria__constitutive_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__constitutive_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__constitutive_reading, "Constitutive Recognition Requirement for Statehood (Montevideo Doctrine Reading)").
narrative_ontology:topic_domain(montevideo_statehood_criteria__constitutive_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__constitutive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__constitutive_reading, '14cc2764-0239-4568-ac32-87c1440436a8').
narrative_ontology:cs_kernel_codification('14cc2764-0239-4568-ac32-87c1440436a8', fixed_text).
narrative_ontology:cs_authority_grounding('14cc2764-0239-4568-ac32-87c1440436a8', lineage).
narrative_ontology:cs_interpretation_layer_present('14cc2764-0239-4568-ac32-87c1440436a8').
narrative_ontology:cs_reading_relation('14cc2764-0239-4568-ac32-87c1440436a8', montevideo_statehood_criteria__declaratory_reading, forecloses).
narrative_ontology:cs_reading_relation('14cc2764-0239-4568-ac32-87c1440436a8', montevideo_statehood_criteria__hybrid_reading, influences).
narrative_ontology:cs_axiom('14cc2764-0239-4568-ac32-87c1440436a8', foundational, recognition_is_constitutive).
narrative_ontology:cs_axiom_status(recognition_is_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('14cc2764-0239-4568-ac32-87c1440436a8', recognition_is_constitutive, conventional).
narrative_ontology:cs_axiom('14cc2764-0239-4568-ac32-87c1440436a8', secondary, statehood_requires_state_community_consensus).
narrative_ontology:cs_axiom_status(statehood_requires_state_community_consensus, holdable).
narrative_ontology:cs_axiom_grounding('14cc2764-0239-4568-ac32-87c1440436a8', statehood_requires_state_community_consensus, conventional).
narrative_ontology:cs_reference_frame('14cc2764-0239-4568-ac32-87c1440436a8', montevideo_gatekeeping_authority).
narrative_ontology:cs_drift_state('14cc2764-0239-4568-ac32-87c1440436a8', contemporary_geopolitical_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('14cc2764-0239-4568-ac32-87c1440436a8', '2026-06-11T00:00:00Z').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, established_state_community).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, unrecognized_polities).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, liberation_movements).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, contested_territories).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, contested_territories).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, parent_states).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, international_financial_system).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The existing interstate system, dominated by permanent UN Security Council members and de facto great-power consensus, sets and enforces the recognition standard. They determine who is admitted to treaty participation, UN membership, diplomatic immunity, and access to international capital markets. They justify the gate as ensuring international stability and preventing unilateral secession from destabilizing existing borders.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, established_state_community, agenda_setter,
    institutional, generational, arbitrage, global).

% Entities that meet the four Montevideo criteria (defined territory, permanent population, effective government, capacity to enter relations) but lack recognition from enough existing states to gain entry into the interstate system. They bear the cost of permanent legal limbo: no UN seat, no treaty standing, no recourse to international courts, no recognized borders, vulnerability to military intervention without international law protection. Their exit would require either winning recognition (dependent on agenda-setter choice) or abandoning statehood claims (identity dissolution).
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, unrecognized_polities, payer,
    powerless, biographical, identity_locked, local).

% Non-state actors claiming territorial self-determination against parent states. They pay through continued non-recognition despite controlling territory and population; they argue they meet the objective Montevideo criteria but are blocked by parent-state veto within the established state community. Their leverage is limited to military action, international advocacy, or securing a great-power patron.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, liberation_movements, payer,
    moderate, biographical, constrained, regional).

% Regions with effective local governance and distinct populations seeking statehood (Palestine, Northern Cyprus, South Ossetia, Transnistria, etc.). They control territory and populations, yet remain trapped in non-recognition because parent states and their allies block recognition votes. Paradoxically, some benefit from international legal limbo status when it provides leverage against unilateral annexation.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, contested_territories, payer,
    moderate, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__constitutive_reading, contested_territories, beneficiary).

% States harboring contested territories or independence movements benefit from the constitutive gate: they retain veto power over secessionist recognition through the international community gatekeeping mechanism. The rule protects their territorial integrity from unilateral dissolution and gives them leverage in internal disputes.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, parent_states, beneficiary,
    institutional, generational, arbitrage, national).

% The informal coalition of permanent UN Security Council members and regional hegemons that de facto decides recognition outcomes through diplomatic signaling, voting patterns, and economic incentives. They wield the recognition gate as a tool of geopolitical leverage.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, great_power_consensus, agenda_setter,
    institutional, generational, analytical, global).

% The epistemic community interpreting what statehood means and how the Montevideo criteria apply. They document that the constitutive reading has created a legal limbo for entities that meet objective criteria, and argue for clarification or reform.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, international_courts_and_lawyers, observer,
    institutional, generational, analytical, global).

% Banks, capital markets, and trade networks benefit from the recognition gate because it creates a stable roster of treaty-backed parties. Unrecognized polities cannot issue recognized bonds, join WTO/IMF, or access most interstate financial instruments, which reduces capital flight risk and counterparty ambiguity.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, international_financial_system, beneficiary,
    institutional, generational, mobile, global).

% External powers (Russia for South Ossetia/Transnistria/Crimea, Turkey for Northern Cyprus, Arab states for Palestine) that back unrecognized polities but are overridden by the established state community gatekeeping. They would benefit from a declaratory reading that grants statehood on objective criteria alone, but remain blocked by the constitutive reading's structural veto.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, secessionist_patrons, excluded,
    institutional, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(montevideo_statehood_criteria__constitutive_reading, established_state_community).
narrative_ontology:fixing_cost_class(montevideo_statehood_criteria__constitutive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of interstate order by maintaining a stable roster of recognized subjects with clear legal standing to enter treaties, occupy UN seats, and claim territorial integrity protection. Prevents the anarchic proliferation of unilateral secession claims that could destabilize borders across every region.
% TRANSFER_FUNCTION: Transfers gatekeeping power from objective legal criteria (territory, population, government, capacity) to the subjective consensus of the existing state community. Unrecognized polities that meet the objective tests still cannot access treaty participation, UN membership, international court standing, capital market access, and diplomatic immunity — those benefits flow only to recognized states. The recognition gate produces a binary: recognized = full legal standing; unrecognized = legal limbo despite material statehood.
% ABSENT_VOICES: Unrecognized polities and liberation movements are structurally excluded from the recognition decision-making process — the existing state community votes on their own admission while the applicants have no voting power. International lawyers and scholars document this exclusion; secessionist patrons advocate for change but lack veto power.
% DISAPPEARANCE_RATIONALE: If the constitutive recognition requirement vanished overnight and statehood flowed automatically from the four objective Montevideo criteria, dozens of currently unrecognized polities would immediately enter the interstate system as de jure states with UN seats, treaty standing, and claim-of-right to immunity from military intervention. Parent states would lose their veto over secessionist recognition; territorial borders would become contested in real-time rather than frozen by non-recognition; international financial markets would face dozens of new counterparties with uncertain credit profiles. The interstate system would reorganize around objective criteria rather than gatekeeping consensus.
% FOUNDING_PROBLEM: The 1933 Montevideo Convention established four objective criteria for statehood (territory, population, government, capacity to enter relations) to replace the prior imperial standard of European power recognition. The constitutive reading interprets this as codifying subjective recognition as the final criterion — you are a state only if the existing state community says you are, regardless of whether you meet the objective four.
% FOUNDING_PROBLEM_CORROBORATION: The established state community (through UN practice and diplomatic doctrine) asserts the founding problem remains live — statehood requires recognition to prevent anarchic secession. Unrecognized polities and liberation movements, supported by independent legal scholars and decolonization-era states, attest the founding problem has been inverted — the objective criteria should establish statehood independent of recognition; the requirement persists as gatekeeping rent-seeking. The International Court of Justice's Kosovo advisory opinion (2010) and the extensive scholarly literature on the declaratory vs. constitutive debate document this split.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__constitutive_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__constitutive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__constitutive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(montevideo_statehood_criteria__constitutive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__constitutive_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__constitutive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__constitutive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__constitutive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.68 because the constraint transfers gatekeeping power from objective legal criteria to subjective state community consensus, and this transfer is asymmetric: established states retain full legal standing while unrecognized polities lose all international legal personality despite meeting material statehood tests. The measurement series shows a slight rise over the 90-year interval (0.58 to 0.68) as UN Security Council vetoes have become more entrenched and geopolitical patrons more willing to block recognition for allies' secessionist conflicts (Kashmir, Palestine, Northern Cyprus, South Ossetia, etc.). Suppression is high (0.79) because the gatekeeping mechanism is enforced through explicit non-recognition votes and diplomatic coordination; unrecognized polities have no legitimate exit except winning recognition (dependent on agenda-setter) or military victory. Theater is moderate (0.41) because the stated coordination function (preventing anarchic secession) is real and functioning, but an increasing share of the gatekeeping activity defends geopolitical patron interests rather than systemic stability—the ratio rises from 0.28 to 0.42 as the constraint becomes more obviously a tool of great-power veto than of international order. Accessibility collapse is 0.72: alternatives to the recognition gate (declaring statehood unilaterally, securing patron military backing, appealing to objective legal criteria) exist but require either military strength or great-power patronage; for a weak polity, the collapse is nearly total.
 *
 * PERSPECTIVAL GAP:
 *   The established state community and their allies (parent states, beneficiary patrons) experience the constraint as genuine coordination: it preserves border stability and prevents cascading secession claims that would destabilize the interstate system. From their seats, recognition is a legitimate gatekeeping function that preserves order. Unrecognized polities and liberation movements experience the same structure as pure extraction: they meet the objective legal criteria but are denied international legal standing because existing states retain veto power to preserve their own interests. The engine computes this perspectival gap from the structural data—high directionality (0.85+) for powerless unrecognized polities, lower directionality (0.25-0.40) for institutional established states—reflecting the asymmetric power and exit-option distribution.
 *
 * DIRECTIONALITY LOGIC:
 *   Established states and the state community are structural beneficiaries: they control who enters the system, which entry requires participation in their rules and norms, and their veto cannot be overridden. Their directionality is near 0.0 (full beneficiaries). Parent states are also beneficiaries (they retain veto over secessionist recognition) with directionality around 0.15. Unrecognized polities are structural targets: they bear the cost of legal limbo (no UN seat, no treaty standing, no court access, vulnerability to military intervention) despite meeting objective statehood criteria. Their directionality is near 1.0 (full targets). Liberation movements sit around 0.80-0.85: they pay the cost of non-recognition but retain some leverage through military action and great-power patronage. Contested territories are trapped (0.85-0.90): they control population and territory but cannot access international legal standing, making their exit from the system impossible without either winning recognition (agenda-setter choice) or surrendering territorial claims (identity dissolution).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question: Is the constitutive recognition requirement a living coordination function, or has the founding mandate (preventing anarchic secession through objective criteria) been obsoleted while the gatekeeping machinery persists for rent-seeking? The evidence is split. The coordination case argues that recognition remains necessary because unilateral secession claims would destabilize borders and undermine the treaty-based interstate system; the counter-case argues that the objective Montevideo criteria already prevent frivolous claims and that recognition gatekeeping now serves geopolitical patron interests (great powers block recognition for allies' secessionist conflicts, grant recognition for patrons' breakaway territories). The theater_ratio rise from 0.28 to 0.42 indicates increasing performative activity divorced from functional coordination: the gatekeeping appears to operate increasingly on geopolitical patronage rather than objective criteria or systemic stability logic. The measurement trajectory suggests mounting mandatrophy—the founding coordination function (objectivity, non-arbitrary application) has partially atrophied, but the gatekeeping structure persists through institutional inertia and geopolitical leverage. The constraint is neither fully alive nor fully dead; it is a tangled_rope whose coordination and extraction components are increasingly separated, making it a candidate for reclassification to snare if the coordination function continues to decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutive_vs_declaratory_boundary,
    'Is statehood a legal fact that flows from the four objective Montevideo criteria (declaratory reading), or does it require recognition by the existing state community (constitutive reading)? Or is the boundary itself contestable?',
    'The International Court of Justice''s practice and advisory opinions (especially Kosovo 2010); the UN General Assembly''s evolving recognition doctrine; case-by-case state practice in recognizing unilateral declarations of independence (Kosovo, South Sudan, Palestine). If courts begin issuing decisions that treat objective criteria as establishing statehood independent of recognition, the reading shifts.',
    'A shift toward the declaratory reading would immediately dissolve the extraction: unrecognized polities would gain UN seats, treaty standing, and international court access automatically, removing the gatekeeping veto. Dozens of currently limbo entities would enter the interstate system. This would reclassify the constraint from tangled_rope (coordination + extraction) to rope (pure coordination via objective criteria) or snare (if the gatekeeping persists despite losing legal cover).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutive_vs_declaratory_boundary, conceptual, 'Whether statehood is constitutive (recognition-dependent) or declaratory (criteria-sufficient). This is the core framing dispute between the constitutive and declaratory readings of the same kernel.').

omega_variable(
    geopolitical_versus_legal_veto,
    'Is the non-recognition of unrecognized polities driven by legal principle (the constitutive doctrine itself) or by geopolitical leverage (great powers blocking recognition for patrons'' secessionist conflicts)?',
    'Pattern analysis of voting behavior in the UN General Assembly, UN Security Council vetoes, and recognition statements: if non-recognition correlates strongly with patron-state interests (Russia blocks Palestine recognition while recognizing South Ossetia; China blocks Taiwan while recognizing Transnistria-patron Russia) rather than consistent legal principle, the mechanism is geopolitical rather than doctrinal. This manifests as rising theater_ratio (performative defense of legal doctrine while actually applying geopolitical veto).',
    'If geopolitical mechanism dominates, the constraint is drifting from tangled_rope (coordination + systematic extraction) toward snare (pure extraction dressed in legal language). The theater_ratio trajectory (rising from 0.28 to 0.42) is diagnostic of this drift. A finding that the mechanism is primarily geopolitical would support reclassification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(geopolitical_versus_legal_veto, empirical, 'Whether recognition gatekeeping operates on legal principle or geopolitical patron leverage.').

omega_variable(
    collapsing_alternatives_ambiguity,
    'Are the alternatives to the recognition gate truly collapsed for unrecognized polities (measuring accessibility_collapse correctly), or do they retain options (unilateral declaration, military victory, patron backing) that we are undercounting?',
    'Detailed case histories of unrecognized polities: How many have achieved statehood through unilateral declaration alone (0 globally)? How many through military victory + patron backing (Kosovo, South Sudan, Eritrea — perhaps 3-5 in modern era)? How many remain trapped indefinitely (Palestine, Northern Cyprus, South Ossetia, Transnistria — 4+ for 40+ years)? The case distribution reveals whether alternatives are genuinely collapsed.',
    'If alternatives are more available than modeled (some unrecognized polities retain meaningful military or diplomatic leverage), accessibility_collapse should be lower (~0.55-0.65 instead of 0.72). If alternatives are even more collapsed than modeled (patron backing is the ONLY exit, and patron backing is contingent on geopolitical whim), accessibility_collapse should be higher (~0.80-0.85).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collapsing_alternatives_ambiguity, empirical, 'Whether alternatives to recognition-gate exit are available or structurally foreclosed.').

omega_variable(
    reading_kernel_relationship,
    'Does the constitutive reading represent a coherent interpretation of the Montevideo Convention, or does it read extra-legal doctrine into a treaty that made only objective claims?',
    'Textual analysis of the Montevideo Convention (1933) comparing its language against the doctrine of constitutive recognition that developed after WWII and the UN Charter. Does the treaty actually codify recognition as constitutive, or was that doctrine grafted on afterward?',
    'If constitutive recognition is a post-treaty doctrine rather than a reading of Montevideo itself, the sibling readings (declaratory, hybrid) may claim equal fidelity to the original text, collapsing the logical authority the constitutive reading claims. This would shift the reading from doctrine-grounded to convention-interpreting, potentially opening space for declaratory reading adoption.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_relationship, conceptual, 'Whether constitutive recognition is a reading of the Montevideo Convention or a later doctrine grafted onto it.').

omega_variable(
    normative_legitimacy_gate_alternative,
    'The hybrid reading adds normative legitimacy criteria (democratic governance, human rights, non-aggression) on top of objective criteria. Would adding normative gates reduce extraction or amplify it?',
    'Comparative analysis: a hybrid-reading world would be one where statehood requires both objective criteria AND normative legitimacy. How many unrecognized polities pass the objective test but fail the normative test (authoritarian governance, human rights abuses, aggressive actions)? Would that tighten or loosen the gatekeeping veto? If most unrecognized polities also fail normative criteria, the hybrid reading amplifies gatekeeping. If most pass normative criteria despite non-recognition, the hybrid reading might weaken the gate.',
    'If the hybrid reading would amplify the gate, it is an even more extractive cousin of the constitutive reading. If it would weaken the gate, it represents a structural shift toward the declaratory reading''s position. The answer determines whether the reading family has an evolutionary pressure toward more- or less-permissive statehood.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(normative_legitimacy_gate_alternative, empirical, 'Whether normative legitimacy gates would amplify or attenuate the recognition gatekeeping.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__constitutive_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t0, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(mont_tr_t15, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 15, 0.32).
narrative_ontology:measurement(mont_tr_t30, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 30, 0.36).
narrative_ontology:measurement(mont_tr_t45, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 45, 0.39).
narrative_ontology:measurement(mont_tr_t60, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 60, 0.41).
narrative_ontology:measurement(mont_tr_t75, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 75, 0.42).
narrative_ontology:measurement(mont_tr_t90, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 90, 0.41).

% Extraction over time
narrative_ontology:measurement(mont_be_t0, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(mont_be_t15, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(mont_be_t30, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(mont_be_t45, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 45, 0.67).
narrative_ontology:measurement(mont_be_t60, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 60, 0.69).
narrative_ontology:measurement(mont_be_t75, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 75, 0.68).
narrative_ontology:measurement(mont_be_t90, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 90, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t0, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(mont_su_t15, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 15, 0.75).
narrative_ontology:measurement(mont_su_t30, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 30, 0.77).
narrative_ontology:measurement(mont_su_t45, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 45, 0.79).
narrative_ontology:measurement(mont_su_t60, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 60, 0.8).
narrative_ontology:measurement(mont_su_t75, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 75, 0.79).
narrative_ontology:measurement(mont_su_t90, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 90, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__constitutive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(montevideo_statehood_criteria__constitutive_reading, 0.18).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria__declaratory_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria__hybrid_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, un_security_council_veto_structure).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, treaty_participation_gating).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, international_capital_market_access).

% DUAL FORMULATION NOTE:
% The montevideo_statehood_criteria kernel has three structurally distinct readings: constitutive_reading (this story), declaratory_reading (objective criteria sufficient), and hybrid_reading (objective + normative). Each reading instantiates a different constraint with different ε values, different beneficiary/victim structures, and different effective extraction. The three stories are linked via this network field; they share the same referent (what statehood is under international law) but diverge on mechanism (recognition-dependent vs. criteria-sufficient vs. legitimacy-gated). Sibling stories describe how each reading operates and where they differ structurally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(montevideo_statehood_criteria__constitutive_reading, institutional, 0.12).
constraint_indexing:directionality_override(montevideo_statehood_criteria__constitutive_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
