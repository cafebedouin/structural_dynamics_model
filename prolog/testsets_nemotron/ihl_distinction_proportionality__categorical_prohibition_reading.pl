% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__categorical_prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_distinction_proportionality__categorical_prohibition_reading, []).

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
 *   constraint_id: ihl_distinction_proportionality__categorical_prohibition_reading
 *   human_readable: Categorical Prohibition of Autonomous Weapons Systems under Martens Clause
 *   domain: international_humanitarian_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   The Martens Clause — a residual humanitarian safeguard in IHL — is
 *   invoked by the categorical prohibition reading to declare autonomous
 *   weapons systems (LAWS) unlawful per se. The argument: delegating the
 *   decision to kill to a machine violates the 'principles of humanity and
 *   the dictates of public conscience' because it evacuates moral judgment
 *   from the act of lethal force. This reading demands a total ban, not
 *   regulation. The constraint extracts the strategic option value of
 *   autonomous targeting from major military powers and transfers it to a
 *   prohibition regime championed by civil society and non-capable states.
 *   The claimed type is snare: the coordination story (a clear red line
 *   preventing an arms race) is the cover; the extraction is asymmetric and
 *   sustained by diplomatic suppression of the technology class. The engine
 *   computes per-seat types from the structural data; this reading's ε (0.88)
 *   reflects the near-total foreclosure of a technology class from the
 *   perspective of those who would develop it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__categorical_prohibition_reading, 0.88).
domain_priors:suppression_score(ihl_distinction_proportionality__categorical_prohibition_reading, 0.45).
domain_priors:theater_ratio(ihl_distinction_proportionality__categorical_prohibition_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__categorical_prohibition_reading, snare).
narrative_ontology:human_readable(ihl_distinction_proportionality__categorical_prohibition_reading, "Categorical Prohibition of Autonomous Weapons Systems under Martens Clause").
narrative_ontology:topic_domain(ihl_distinction_proportionality__categorical_prohibition_reading, "international_humanitarian_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__categorical_prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__categorical_prohibition_reading, '4ccefcfb-652b-4c22-9ef7-cc0ba14e770d').
narrative_ontology:cs_kernel_codification('4ccefcfb-652b-4c22-9ef7-cc0ba14e770d', fixed_text).
narrative_ontology:cs_authority_grounding('4ccefcfb-652b-4c22-9ef7-cc0ba14e770d', lineage).
narrative_ontology:cs_interpretation_layer_present('4ccefcfb-652b-4c22-9ef7-cc0ba14e770d').
narrative_ontology:cs_reading_relation('4ccefcfb-652b-4c22-9ef7-cc0ba14e770d', ihl_distinction_proportionality__human_agency_reading, forecloses).
narrative_ontology:cs_reading_relation('4ccefcfb-652b-4c22-9ef7-cc0ba14e770d', ihl_distinction_proportionality__outcomes_based_reading, forecloses).
narrative_ontology:cs_axiom('4ccefcfb-652b-4c22-9ef7-cc0ba14e770d', foundational, machine_killing_categorically_unlawful).
narrative_ontology:cs_axiom_status(machine_killing_categorically_unlawful, holdable).
narrative_ontology:cs_axiom_grounding('4ccefcfb-652b-4c22-9ef7-cc0ba14e770d', machine_killing_categorically_unlawful, deontological).
narrative_ontology:cs_axiom('4ccefcfb-652b-4c22-9ef7-cc0ba14e770d', foundational, human_dignity_requires_moral_agency_in_killing).
narrative_ontology:cs_axiom_status(human_dignity_requires_moral_agency_in_killing, holdable).
narrative_ontology:cs_axiom_grounding('4ccefcfb-652b-4c22-9ef7-cc0ba14e770d', human_dignity_requires_moral_agency_in_killing, deontological).
narrative_ontology:cs_reference_frame('4ccefcfb-652b-4c22-9ef7-cc0ba14e770d', martens_clause_1907_humanity_conscience).
narrative_ontology:cs_drift_state('4ccefcfb-652b-4c22-9ef7-cc0ba14e770d', ccw_laws_process_2013_present, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4ccefcfb-652b-4c22-9ef7-cc0ba14e770d', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, states_lacking_laws_capability).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, humanitarian_dignity_doctrine).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, military_technological_advantage_holders).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, states_with_advanced_autonomous_systems).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__categorical_prohibition_reading, martens_clause_humanity_conscience).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__categorical_prohibition_reading, human_dignity_non_delegability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% NGOs, religious bodies, and transnational advocacy networks that campaign for a total ban on lethal autonomous weapons. They benefit from the constraint's normative force — it legitimizes their advocacy, channels diplomatic momentum, and provides a legal benchmark for naming violations. They do not administer the constraint but their mobilization sustains its political salience.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society, beneficiary,
    organized, generational, arbitrage, global).

% States without indigenous LAWS development programs (most Global South states, smaller NATO members). They benefit from a categorical ban because it freezes the capability gap — they cannot be outcompeted on a technology that is prohibited. Some actively propose treaty language at CCW to advance this freeze; their exit is mobile because they can shift diplomatic alignment without domestic institutional rupture.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, states_lacking_laws_capability, beneficiary,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__categorical_prohibition_reading, states_lacking_laws_capability, agenda_setter).

% Major military powers with mature autonomous systems programs (US, China, Russia, Israel, UK). They bear the extraction: a categorical ban forces abandonment or non-deployment of sunk R&D, constrains doctrinal evolution, and surrenders perceived asymmetric advantage. Their exit is constrained — they cannot credibly commit to non-development without verification regimes they oppose, and unilateral restraint is strategically costly. They resist the constraint through interpretive narrowing ("meaningful human control") and forum-shifting.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, military_technological_advantage_holders, payer,
    powerful, biographical, constrained, global).

% The same major powers viewed as institutional administrators of the existing IHL order. They set the agenda for CCW discussions, control the pace and scope of treaty negotiations, and define operational concepts ("appropriate human judgment") that narrow the prohibition's reach. They pay the extraction as capability-holders but also administer the regime that would enforce it — a structural dual position.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, states_with_advanced_autonomous_systems, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__categorical_prohibition_reading, states_with_advanced_autonomous_systems, agenda_setter).

% Those directly subjected to autonomous targeting decisions in conflict zones. They have no voice in the diplomatic process, no exit from the battlespace, and no capacity to influence the constraint's interpretation. The constraint's categorical form is ostensibly for their protection, but they are not consulted on whether a total ban or a performance standard better serves their survival.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, frontline_combatants_civilians, excluded,
    powerless, immediate, trapped, local).

% Academic experts who interpret the Martens Clause, distinction, proportionality, and the legal status of emerging weapons. They do not collect rents or bear costs from the constraint's operation. Their analyses structure the argumentative field in which states and civil society contest the readings.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, verifiable red line: no machine may autonomously select and engage human targets. This coordinates state behavior by making LAWS development and deployment a treaty violation ex ante, avoiding case-by-case adjudication of proportionality algorithms in combat.
% TRANSFER_FUNCTION: Transfers the strategic option value of autonomous targeting from military establishments to the prohibition regime. The capability-holders lose the ability to field LAWS; the beneficiaries (civil society, non-capable states) gain a legal instrument that forecloses that capability class. The transfer is not monetary but capacitive — the freedom to develop and deploy is the extracted good.
% ABSENT_VOICES: Frontline combatants and civilian populations in conflict zones where LAWS would be tested or deployed are structurally excluded from the CCW process. Military personnel who would operate or be targeted by autonomous systems have no formal representational channel. Their absence means the constraint's humanitarian rationale is asserted on their behalf without their consent or input.
% DISAPPEARANCE_RATIONALE: If the categorical prohibition vanished overnight, major military powers would accelerate LAWS deployment within existing procurement programs. The strategic calculus shifts from "whether" to "how fast." Non-capable states lose their legal shield. Civil society loses its clearest normative anchor. The world rearranges around a new arms race dynamic.
% FOUNDING_PROBLEM: The Martens Clause (1899/1907) was crafted to ensure that populations and belligerents remain under the protection of the principles of humanity and the dictates of public conscience even where specific treaty law is absent. The founding problem for this reading: the emergence of systems that make life-and-death decisions without human moral agency creates a gap where neither existing treaty rules nor customary law adequately protect human dignity — because the very notion of a 'judgment' is evacuated.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC's 2021 position paper and the UN Secretary-General's 2023 New Agenda for Peace both affirm that the Martens Clause applies to autonomous weapons and that human control over the use of force is a prerequisite for compliance with IHL. These are institutional voices outside the anti-militarist civil society beneficiary set. However, major military powers' legal advisers contest that the founding problem is live, arguing existing IHL suffices — making the status contested in practice, though this reading holds it as live.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__categorical_prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__categorical_prohibition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__categorical_prohibition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(ihl_distinction_proportionality__categorical_prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__categorical_prohibition_reading, 0.88, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__categorical_prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_distinction_proportionality__categorical_prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ihl_distinction_proportionality__categorical_prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.88) because the constraint bans an entire capability class, extracting the full option value of LAWS from military establishments. Suppression is moderate (0.45) — the constraint operates through treaty negotiation and normative pressure, not direct coercion of individuals; its enforcement is the diplomatic cost of defiance. Theater ratio is low (0.15) — the humanitarian rationale is genuinely held by its advocates, not performative. Accessibility collapse is high (0.75) — once the categorical principle is accepted, partial measures (regulation, 'meaningful human control') appear as betrayals. Resistance is substantial (0.65) — major powers actively resist through interpretive narrowing and procedural delay at the CCW.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats, the constraint is a rope: a genuine coordination solution to an arms race that would otherwise erase humanitarian protections. From the payer seats, it is a snare: a prohibition that extracts their technological investment and strategic freedom while the coordination justification (preventing indiscriminate killing) could be met by performance standards. The engine computes this divergence — the claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Anti-militarist civil society and non-capable states are structural beneficiaries: they gain normative leverage and a capability freeze without bearing development costs. Their directionality d is low (near beneficiary end). Major military powers are structural payers: they bear the full extraction of surrendered capability and constrained doctrine. Their d is high (near target end). The dual-positioned states-with-systems-as-agenda-setters experience a split d — they administer the regime that extracts from them. Frontline populations are excluded (trapped, no voice). IHL scholars are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (protecting human dignity from machine-decided killing) is live — the technology continues to advance, and the gap the Martens Clause was meant to cover has widened, not closed. No mandatrophy: the founding problem persists and the constraint's function tracks it. However, the payer seats experience it as mandatrophy because the coordination function (preventing an arms race) has failed — the race continues in the shadows — leaving only extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    martens_clause_scope_ambiguity,
    'Does the Martens Clause''s ''principles of humanity and dictates of public conscience'' substantively prohibit autonomous weapons, or is it a residual gap-filler that applies only where specific law is silent?',
    'ICJ advisory opinion or authoritative treaty interpretation by states parties to AP I; convergence of state practice and opinio juris on LAWS specifically.',
    'If the Clause is a gap-filler, the categorical prohibition reading loses its legal anchor once specific LAWS rules are negotiated — the constraint becomes a political preference, not a legal mandate. If it is a substantive prohibition, the ban holds regardless of subsequent regulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(martens_clause_scope_ambiguity, conceptual, 'Whether the Martens Clause carries independent substantive content or is a residual reference to existing law.').

omega_variable(
    human_dignity_operationalization,
    'Can ''human dignity violation per se'' be operationalized into a verifiable legal standard, or does it remain a rhetorical claim that cannot guide compliance or enforcement?',
    'Drafting of a treaty article that translates the dignity claim into a defined prohibition (e.g., ''no weapon system shall select and engage human targets without...'') and testing whether states accept it as legally binding.',
    'If non-operationalizable, the constraint remains aspirational — high extraction rhetoric without compliance machinery, shifting toward piton. If operationalized, the extraction becomes enforceable and the constraint hardens as snare or tangled_rope depending on verification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(human_dignity_operationalization, conceptual, 'Whether the core normative claim admits of legal operationalization.').

omega_variable(
    committer_structure_kernel_reading,
    'How does this reading''s structural relationship to its sibling readings affect its classification stability?',
    'Track whether CCW negotiations produce a unified instrument (foreclosing this reading) or fragment into parallel tracks (coexistence). Monitor whether human_agency_reading becomes the dominant diplomatic frame.',
    'If human_agency_reading becomes the consensus frame, this reading''s extraction profile changes — it becomes the ''radical'' position extracting diplomatic capital from the center. If outcomes_based_reading prevails, this reading becomes a counter-regime with near-zero state adherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, empirical, 'Commitment-system framing: this reading forecloses both siblings; the kernel''s evolution determines whether this foreclosure holds or collapses.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__categorical_prohibition_reading, 2013, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl_catprohib_tr_t2013, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2013, 0.05).
narrative_ontology:measurement(ihl_catprohib_tr_t2016, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2016, 0.08).
narrative_ontology:measurement(ihl_catprohib_tr_t2018, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2018, 0.1).
narrative_ontology:measurement(ihl_catprohib_tr_t2020, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2020, 0.12).
narrative_ontology:measurement(ihl_catprohib_tr_t2022, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2022, 0.13).
narrative_ontology:measurement(ihl_catprohib_tr_t2024, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2024, 0.14).
narrative_ontology:measurement(ihl_catprohib_tr_t2026, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2026, 0.15).

% Extraction over time
narrative_ontology:measurement(ihl_catprohib_be_t2013, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2013, 0.35).
narrative_ontology:measurement(ihl_catprohib_be_t2016, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2016, 0.48).
narrative_ontology:measurement(ihl_catprohib_be_t2018, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2018, 0.62).
narrative_ontology:measurement(ihl_catprohib_be_t2020, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2020, 0.73).
narrative_ontology:measurement(ihl_catprohib_be_t2022, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2022, 0.81).
narrative_ontology:measurement(ihl_catprohib_be_t2024, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2024, 0.86).
narrative_ontology:measurement(ihl_catprohib_be_t2026, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2026, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(ihl_catprohib_su_t2013, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2013, 0.25).
narrative_ontology:measurement(ihl_catprohib_su_t2016, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2016, 0.32).
narrative_ontology:measurement(ihl_catprohib_su_t2018, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2018, 0.38).
narrative_ontology:measurement(ihl_catprohib_su_t2020, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2020, 0.41).
narrative_ontology:measurement(ihl_catprohib_su_t2022, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2022, 0.43).
narrative_ontology:measurement(ihl_catprohib_su_t2024, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2024, 0.44).
narrative_ontology:measurement(ihl_catprohib_su_t2026, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2026, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__categorical_prohibition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ihl_distinction_proportionality__categorical_prohibition_reading, 0.12).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality__human_agency_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality__outcomes_based_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, ccw_laws_negotiation_process).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of kernel ihl_distinction_proportionality. The categorical_prohibition_reading has the highest ε (0.88) because it bans the entire technology class. The human_agency_reading has lower ε (~0.45) — it permits LAWS if 'meaningful human control' is preserved, extracting only the fully-autonomous subset. The outcomes_based_reading has the lowest ε (~0.15) — it imposes performance standards, extracting only non-compliant systems. The three readings form a constraint family linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ihl_distinction_proportionality__categorical_prohibition_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
