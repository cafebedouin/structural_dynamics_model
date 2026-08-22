% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__categorical_prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_categorical_prohibition_aws, []).

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
 *   human_readable: Martens Clause Categorical Prohibition of Autonomous Lethal Weapons Systems
 *   domain: international_humanitarian_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   The Martens Clause, a foundational principle of International
 *   Humanitarian Law embedded in the Hague and Geneva Conventions, asserts
 *   that civilian conduct and public conscience impose limits on military
 *   means that transcend technical capability. This reading instantiates one
 *   interpretation of the Clause: autonomous lethal weapons systems (LAWS) —
 *   machines that select and engage targets without real-time human
 *   intervention — are categorically prohibited because delegating life-death
 *   decisions to machines violates human dignity per se, independent of
 *   whether the system's targeting accuracy matches or exceeds human
 *   performance. The constraint extracts from states with advanced autonomous
 *   systems and militaries that depend on autonomous capability, and benefits
 *   those lacking such capability and civil society movements seeking
 *   categorical bans. Enforcement depends on international agreement,
 *   verification mechanisms, and state willingness to forgo autonomous
 *   advantage.
 *
 * KEY AGENTS:
 *   - anti_militarist_civil_society: pressure coalition seeking categorical prohibition; high moral authority, constrained material power
 *   - states_lacking_autonomous_capability: benefit from a ban that locks in existing military advantage distribution; institutional power, generational time horizon
 *   - military_technological_advantage_holders: possess LAWS capability, bear the extraction if ban is enforced; institutional power, identity-locked investment in autonomous systems
 *   - states_with_advanced_autonomous_systems: principal targets of the extraction; powerful but constrained by international humanitarian law obligations, biographical to generational horizon
 *   - international_courts_and_treaty_bodies: agenda_setter and arbiter of what the Martens Clause permits; distributed institutional authority, analytical seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__categorical_prohibition_reading, 0.92).
domain_priors:suppression_score(ihl_distinction_proportionality__categorical_prohibition_reading, 0.78).
domain_priors:theater_ratio(ihl_distinction_proportionality__categorical_prohibition_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__categorical_prohibition_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__categorical_prohibition_reading, "Martens Clause Categorical Prohibition of Autonomous Lethal Weapons Systems").
narrative_ontology:topic_domain(ihl_distinction_proportionality__categorical_prohibition_reading, "international_humanitarian_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__categorical_prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__categorical_prohibition_reading, '1e04268b-4c67-439c-8a58-03c1f08dfdad').
narrative_ontology:cs_kernel_codification('1e04268b-4c67-439c-8a58-03c1f08dfdad', formalized).
narrative_ontology:cs_authority_grounding('1e04268b-4c67-439c-8a58-03c1f08dfdad', lineage).
narrative_ontology:cs_interpretation_layer_present('1e04268b-4c67-439c-8a58-03c1f08dfdad').
narrative_ontology:cs_reading_relation('1e04268b-4c67-439c-8a58-03c1f08dfdad', ihl_distinction_proportionality__human_agency_reading, coexists_with).
narrative_ontology:cs_reading_relation('1e04268b-4c67-439c-8a58-03c1f08dfdad', ihl_distinction_proportionality__outcomes_based_reading, forecloses).
narrative_ontology:cs_axiom('1e04268b-4c67-439c-8a58-03c1f08dfdad', foundational, human_dignity_per_se_prohibition).
narrative_ontology:cs_axiom_status(human_dignity_per_se_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('1e04268b-4c67-439c-8a58-03c1f08dfdad', human_dignity_per_se_prohibition, deontological).
narrative_ontology:cs_axiom('1e04268b-4c67-439c-8a58-03c1f08dfdad', foundational, categorical_martens_interpretation).
narrative_ontology:cs_axiom_status(categorical_martens_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('1e04268b-4c67-439c-8a58-03c1f08dfdad', categorical_martens_interpretation, deontological).
narrative_ontology:cs_reference_frame('1e04268b-4c67-439c-8a58-03c1f08dfdad', martens_clause_human_dignity_foundation).
narrative_ontology:cs_drift_state('1e04268b-4c67-439c-8a58-03c1f08dfdad', contemporary_autonomous_systems_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1e04268b-4c67-439c-8a58-03c1f08dfdad', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, states_lacking_autonomous_capability).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, military_technological_advantage_holders).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, states_with_advanced_autonomous_systems).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, military_personnel_and_civilians_subject_to_conflict).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocacy coalitions, human rights organizations, and non-state actors organizing pressure to ban autonomous lethal weapons on human dignity grounds. They mobilize moral authority, public opinion, and political pressure to constrain military states. They gain credibility and agenda-setting power from the categorical prohibition; they can exit the coalition if political priority shifts, but their core mission is served by the ban remaining in place.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society, beneficiary,
    organized, generational, mobile, global).

% Nations without advanced autonomous weapons programs or the technological infrastructure to develop them. They benefit from a categorical ban that freezes the existing military advantage distribution — it prevents technologically advanced states from pulling further ahead via autonomous systems. Their exit is constrained because abandoning the prohibition means accepting a widening military technology gap; their time horizon is generational because military advantage compounds over decades.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, states_lacking_autonomous_capability, beneficiary,
    institutional, generational, constrained, global).

% Defense contractors, military research establishments, and technology firms that have invested in autonomous systems development. They bear the extraction directly: a categorical prohibition forces them to abandon years of R&D investment, destroy prototypes, and forfeit a potential strategic advantage. Exit is identity-locked because their institutional identity and career structures are fused with autonomous systems development; abandoning the program means institutional restructuring, personnel displacement, and loss of strategic positioning.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, military_technological_advantage_holders, payer,
    institutional, generational, identity_locked, global).

% Nations possessing advanced autonomous weapons programs and the technological capability to deploy LAWS. They are the primary payers of the extraction: a categorical ban forces them to either comply (destroying capability and accepting strategic disadvantage) or defect (incurring diplomatic costs, legal jeopardy, and sanctions). Their exit is trapped because both options impose high costs; compliance means accepting constraint, defection means accepting international isolation. They sit between payer and agenda-setter: they can attempt to set an alternative agenda (the outcomes-based reading), but they are primarily positioned as targets of the prohibition.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, states_with_advanced_autonomous_systems, payer,
    institutional, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__categorical_prohibition_reading, states_with_advanced_autonomous_systems, agenda_setter).

% International Court of Justice, International Criminal Court, UN General Assembly, Red Cross and Red Crescent Movement, treaty conference bodies. They interpret and enforce the Martens Clause and distinguish/proportionality obligations. They set the agenda by deciding which reading (categorical prohibition, human agency, or outcomes-based) governs the legal regime. Their seat is analytical: they adjudicate but do not collect from the constraint; they set rules but bear the cost of enforcement asymmetrically distributed.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, international_humanitarian_law_courts_and_bodies, agenda_setter,
    institutional, generational, analytical, global).

% Soldiers, civilians, detainees, and non-combatants subject to military operations. They benefit from a categorical ban on autonomous targeting if it prevents inaccurate machine decisions that would kill them — but they are powerless to defend the ban and trapped in whatever conflict regime states choose. Their immediate time horizon reflects the acute danger of warfare; their universality reflects that LAWS capability affects conflicts globally.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, military_personnel_and_civilians_subject_to_conflict, beneficiary,
    powerless, immediate, trapped, universal).

% Rival defense contractors, competing militaries, and alternative technology providers who would profit from open autonomous systems markets or from being able to develop and sell autonomous capability. They are structurally excluded from this regime: a categorical ban locks them out of the market entirely. They would contest the ban if they had voice, arguing for performance-based alternatives or open development. Their exclusion is structural — the categorical prohibition exists to keep them out.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, technologically_advanced_competitors_to_autonomous_system_developers, excluded,
    organized, biographical, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents an arms race toward autonomous lethal weapons and establishes a binding commitment to human decision-making authority over life-death choices in warfare. The coordination problem being solved: absent agreement, states fear others will develop LAWS, creating pressure to develop autonomously to avoid strategic disadvantage, leading to a race toward inhumane killing. The categorical prohibition coordinates a stable equilibrium where all states forgo autonomy rather than defect and face isolation.
% TRANSFER_FUNCTION: Moves military strategic advantage FROM states with advanced autonomous systems TO states lacking autonomy, by forbidding the development and deployment of the technology that would grant asymmetric advantage. It also transfers legitimacy and moral authority FROM military establishments TO civil society and human rights frameworks by enshrining human dignity as non-negotiable in warfare.
% ABSENT_VOICES: States with advanced autonomous systems are present but structurally positioned as targets of the extraction. Technologically advanced defense contractors that would profit from open markets are excluded by the categorical ban — they have no seat at the table. Alternative framings (the outcomes-based reading, which would permit autonomy under performance conditions) are excluded from this reading's scope: this reading does not debate whether LAWS can be sufficiently accurate; it asserts categorical prohibition independent of accuracy.
% DISAPPEARANCE_RATIONALE: If the categorical prohibition vanished, states would rapidly resume autonomous weapons development programs, strategic competition for autonomous advantage would accelerate, and the military landscape would reorganize around autonomous system deployment. The constraint's disappearance would trigger a reconfiguration of military capability, international alliances, and strategic doctrine. Civilians and soldiers would face a materially different risk environment.
% FOUNDING_PROBLEM: The founding problem is the intersection of two concerns: (1) technological developments enabling delegated killing decisions (autonomous targeting), and (2) principles of human dignity enshrined in the Martens Clause prohibiting warfare methods that violate humanity and public conscience. The problem: does IHL permit machines to make life-death decisions, or does human dignity require irreducible human judgment? The categorical_prohibition_reading answers: human dignity prohibits it per se, independent of technical accuracy.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem remains contested and live. Anti-militarist civil society and human rights organizations attest that autonomous weapons development is advancing and the dignity question remains unresolved — they cite weapons development programs and lack of binding prohibition as evidence the problem is live. Military states developing autonomy dispute the categorical nature of the prohibition, citing technical accuracy improvements and arguing outcomes-based rules are appropriate. States lacking autonomy attest the problem is live because advanced states retain the capability to build and deploy LAWS. International humanitarian law commentators (Red Cross, academic lawyers outside benefiting coalitions, and prior state submissions to treaty conferences) attest the foundational problem remains contested — no consensus exists on whether the Martens Clause mandates categorical prohibition or permits conditional authorization. This corroboration is diverse enough to count: the problem is live across multiple independent seats.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__categorical_prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__categorical_prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__categorical_prohibition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ihl_distinction_proportionality__categorical_prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__categorical_prohibition_reading, 0.92, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is extremely high (0.92) because the constraint asserts a categorical ban on an entire technology class — it extracts the option to develop, test, and deploy autonomous systems from those who have invested in them. The suppression value (0.78) is high because enforcement depends on international agreement that constrains defection; states choosing to build LAWS face diplomatic consequences, sanctions, and legal jeopardy, and information about autonomous deployment is difficult to verify (creating reliance on trust and signaling). Theater is low (0.12) because the prohibition, if enforced, operates on substance (the weapon cannot be deployed) not performance (acceptance is not a matter of maintaining appearances). The measurement series shows extractiveness rising over 15 years as international pressure accumulates and technological capability spreads — states face increasing choice pressure between capability and compliance. Suppression rises correspondingly as treaty regimes tighten. Theater remains low throughout: the constraint is not performative; it either forbids the weapon or it does not.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of states with advanced autonomous systems, this reading appears as a categorical prohibition grounded in dignity — a rule that forecloses an entire technology without regard for performance. They experience it as maximally extractive: it forbids what they have invested in building. From the seat of anti-militarist civil society and states lacking autonomy, the same rule appears as a vindication of human dignity — a protection grounded in principle, not strategic disadvantage. The engine computes this divergence from the authored structural data: the payer seats have high directionality (d near 1.0, full targets); the beneficiary seats have low directionality (d near 0.0, full beneficiaries). The claim/metric independence means this story does NOT reconcile the seats' experience — it preserves the structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Military states with autonomous systems are the structural targets (d high, near 1.0): they bear the extraction directly — forgoing capability, destroying systems, accepting international legal jeopardy if they defect. Anti-militarist civil society and states lacking autonomy are structural beneficiaries (d low, near 0.0): they gain the prohibition without bearing development costs or strategic disadvantage. International courts and treaty bodies sit as analytical observers (d at 0.5, symmetric position): they enforce the rule but are not themselves extracted from. The directionality chain is straightforward: beneficiary + victim structure + active enforcement chain → clear d assignment.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy through sustained founding-problem alignment: the founding problem (preventing a race to autonomous killing and protecting human dignity in warfare) remains live, contested, and consequential. Military states have not abandoned the desire to develop LAWS; anti-militarist actors have not abandoned the pressure to ban them. The constraint persists because the contradiction persists. There is no sign of a dead founding problem animated only by institutional theater. Mandatrophy would emerge if: (a) LAWS technology became impossible or obsolete (founding problem dies), leaving the constraint as ceremonial residue, or (b) the international consensus shifted such that autonomous systems became universally accepted and the ban became decorative (the rule remains but enforcement atrophies). Neither has occurred as of the measurement interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_distinction_reading_contest,
    'Is the Martens Clause principle of humanity categorically incompatible with autonomous lethal weapons regardless of performance, or does it permit autonomy if technical outcomes match human distinction/proportionality?',
    'International court judgment on LAWS legality, or state practice through protocol amendments. This reading asserts a categorical axiom; sibling readings assert conditional or performance-based axioms. The contest is located at the foundational normative claim.',
    'If this reading''s axiom forecloses the outcomes_based_reading, LAWS are unlawful in all cases. If the readings coexist, the legal status depends on empirical performance assessment and party agreement — a different regime entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_distinction_reading_contest, conceptual, 'Whether human dignity prohibition is categorical or conditional on technical outcomes.').

omega_variable(
    human_agency_vs_categorical_dignity,
    'Does the Martens Clause ground the prohibition in irreducible HUMAN AGENCY (the human_agency_reading''s distinction) or in CATEGORICAL DIGNITY of persons (this reading''s distinction)?',
    'Textual exegesis of Martens Clause history and subsequent state ratification debates. The human_agency_reading argues the clause prohibits delegating judgment; this reading argues the clause prohibits the outcome (machine-decided killing) because it violates dignity per se, independent of whether the decision would have been identical if made by humans.',
    'If grounded in agency, a constraint on HOW decisions are made (delegated judgment). If grounded in dignity, a constraint on WHAT outcomes are permitted (machine killing is categorically impermissible). This reading instantiates the dignity grounding; the human_agency_reading instantiates the agency grounding. The two foundations produce different ε values and different beneficiary structures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(human_agency_vs_categorical_dignity, conceptual, 'Whether the Martens Clause prohibition is grounded in agency or dignity.').

omega_variable(
    enforcement_mechanism_ambiguity,
    'How would a categorical prohibition on LAWS be enforced internationally given the decentralized nature of state sovereignty and the dual-use technology problem?',
    'Analysis of existing arms control regimes (Chemical Weapons Convention, Bio Weapons Convention enforcement mechanisms); examination of whether detection and verification are technically feasible for autonomous code execution.',
    'If enforcement mechanisms are prohibitively weak, the constraint''s suppression value may be lower than authored — the rule exists but state compliance is voluntary and defection is undetectable. If robust verification exists, suppression remains high. This affects classification stability under empirical drift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_mechanism_ambiguity, empirical, 'Feasibility and robustness of international enforcement of LAWS prohibition.').

omega_variable(
    reading_sibling_coexistence_vs_foreclosure,
    'Can the categorical_prohibition_reading and the outcomes_based_reading both be held within the same international legal framework, or does the categorical axiom (dignity per se) logically foreclose the outcomes axiom (sufficient performance)?',
    'International negotiation and protocol drafting: if states adopt a legal regime permitting LAWS under performance conditions, this reading''s axiom is rejected in practice (status = overridden); if categorical prohibition is codified, the outcomes_based reading is rejected. The logical relationship (foreclosure vs. coexistence) is distinct from the political outcome.',
    'This affects the reading_relations declaration in cs_structure: if foreclosure is true, the engine routes this reading as logically excluding the sibling. If coexistence is true, both remain live positions despite contradiction, held by different coalitions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_sibling_coexistence_vs_foreclosure, conceptual, 'Whether categorical dignity prohibition and conditional performance-based authorization are logically foreclosed or politically coexisting.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__categorical_prohibition_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t0, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(ihl__tr_t3, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 3, 0.09).
narrative_ontology:measurement(ihl__tr_t6, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 6, 0.1).
narrative_ontology:measurement(ihl__tr_t9, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 9, 0.11).
narrative_ontology:measurement(ihl__tr_t12, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 12, 0.115).
narrative_ontology:measurement(ihl__tr_t15, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 15, 0.12).

% Extraction over time
narrative_ontology:measurement(ihl__be_t0, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 0, 0.81).
narrative_ontology:measurement(ihl__be_t3, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 3, 0.84).
narrative_ontology:measurement(ihl__be_t6, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 6, 0.87).
narrative_ontology:measurement(ihl__be_t9, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 9, 0.89).
narrative_ontology:measurement(ihl__be_t12, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 12, 0.91).
narrative_ontology:measurement(ihl__be_t15, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 15, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t0, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(ihl__su_t3, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 3, 0.67).
narrative_ontology:measurement(ihl__su_t6, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 6, 0.71).
narrative_ontology:measurement(ihl__su_t9, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 9, 0.74).
narrative_ontology:measurement(ihl__su_t12, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 12, 0.76).
narrative_ontology:measurement(ihl__su_t15, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 15, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__categorical_prohibition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ihl_distinction_proportionality__categorical_prohibition_reading, 0.18).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality__human_agency_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality__outcomes_based_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of kernel ihl_distinction_proportionality. Sibling readings (human_agency_reading and outcomes_based_reading) are separate constraint stories instantiating different normative interpretations of the same foundational commitment (Martens Clause principles). All three stories share the same referent (regulation of autonomous lethal weapons under IHL) but differ in their ε values, beneficiary/victim structures, and classifications because they ground the prohibition in different principles: categorical dignity (this story), irreducible agency (human_agency), and empirical outcomes sufficiency (outcomes_based). The three form a constraint family linked by network.affects_constraints. Decomposition rationale: each reading yields a structurally distinct ε because each reading's assessment of what the Martens Clause requires is different — and changing the reading changes what extraction is happening. This is not a measurement basis ambiguity; it is a kernel contest where the same standing commitment is read to instantiate three different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
