% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__outcomes_based_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_distinction_proportionality__outcomes_based_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ihl_distinction_proportionality__outcomes_based_reading
 *   human_readable: IHL Distinction/Proportionality via Outcomes-Based Autonomy Compliance
 *   domain: legal/military/technological
 *
 * SUMMARY:
 *   The outcomes-based reading of IHL's distinction and proportionality
 *   obligations holds that military use of autonomous weapons is lawful if
 *   the systems demonstrably achieve performance equal to or exceeding human
 *   operators in identifying targets and assessing civilian harm. The reading
 *   is technology-neutral: it does not forbid autonomous decision-making; it
 *   permits it when metrics justify it. This instantiates one of three
 *   contested readings of the kernel 'IHL obligations regarding
 *   distinction/proportionality in warfare.' The categorical prohibition
 *   reading forbids autonomous killing per se. The human-agency reading
 *   requires irreducible human judgment at the moment of lethal force. This
 *   reading, outcomes-based, asks whether metrics can substitute for judgment
 *   and answers affirmatively. The three readings coexist as live positions
 *   in different institutional and advocacy camps; none logically forecloses
 *   the others, though each influences the others' operating environment.
 *
 * KEY AGENTS:
 *   - Military doctrine adopters (beneficiary, institutional power): gain operational tempo by deploying certified autonomous systems without legal uncertainty.
 *   - Autonomous weapons manufacturers (beneficiary, powerful): capture a market by translating outcomes-based criteria into certification benchmarks.
 *   - Humanitarian law custodians (payer, excluded): lose interpretive authority as IHL compliance shifts from principled judgment to technical metric satisfaction.
 *   - Civilian populations in conflict zones (payer, powerless): depend on distinction/proportionality enforced with human judgment; risk aggregate statistical targeting under the outcomes-based model.
 *   - Performance certification bodies (agenda-setter, institutional): set and adjudicate technical benchmarks that operationalize the outcomes-based reading.
 *   - Categorical prohibition and human-agency advocates (excluded, organized): their principled objections are structurally outside the enforcement apparatus.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__outcomes_based_reading, 0.58).
domain_priors:suppression_score(ihl_distinction_proportionality__outcomes_based_reading, 0.62).
domain_priors:theater_ratio(ihl_distinction_proportionality__outcomes_based_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__outcomes_based_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__outcomes_based_reading, "IHL Distinction/Proportionality via Outcomes-Based Autonomy Compliance").
narrative_ontology:topic_domain(ihl_distinction_proportionality__outcomes_based_reading, "legal/military/technological").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__outcomes_based_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__outcomes_based_reading, '3deeb3fe-59ba-4a58-9b0c-a02d2f9ff59b').
narrative_ontology:cs_kernel_codification('3deeb3fe-59ba-4a58-9b0c-a02d2f9ff59b', formalized).
narrative_ontology:cs_authority_grounding('3deeb3fe-59ba-4a58-9b0c-a02d2f9ff59b', lineage).
narrative_ontology:cs_interpretation_layer_present('3deeb3fe-59ba-4a58-9b0c-a02d2f9ff59b').
narrative_ontology:cs_reading_relation('3deeb3fe-59ba-4a58-9b0c-a02d2f9ff59b', ihl_distinction_proportionality__categorical_prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('3deeb3fe-59ba-4a58-9b0c-a02d2f9ff59b', ihl_distinction_proportionality__human_agency_reading, coexists_with).
narrative_ontology:cs_axiom('3deeb3fe-59ba-4a58-9b0c-a02d2f9ff59b', foundational, outcomes_equivalence_suffices_for_compliance).
narrative_ontology:cs_axiom_status(outcomes_equivalence_suffices_for_compliance, holdable).
narrative_ontology:cs_axiom_grounding('3deeb3fe-59ba-4a58-9b0c-a02d2f9ff59b', outcomes_equivalence_suffices_for_compliance, instrumental).
narrative_ontology:cs_axiom('3deeb3fe-59ba-4a58-9b0c-a02d2f9ff59b', foundational, technical_neutrality_of_ihl_obligations).
narrative_ontology:cs_axiom_status(technical_neutrality_of_ihl_obligations, holdable).
narrative_ontology:cs_axiom_grounding('3deeb3fe-59ba-4a58-9b0c-a02d2f9ff59b', technical_neutrality_of_ihl_obligations, conventional).
narrative_ontology:cs_reference_frame('3deeb3fe-59ba-4a58-9b0c-a02d2f9ff59b', ihl_technology_neutral_compliance).
narrative_ontology:cs_drift_state('3deeb3fe-59ba-4a58-9b0c-a02d2f9ff59b', contemporary_autonomous_weapons_deployment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3deeb3fe-59ba-4a58-9b0c-a02d2f9ff59b', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, military_operational_efficiency).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, autonomous_weapons_manufacturers).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, humanitarian_law_interpretation_authority).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, civilian_populations_in_conflict_zones).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, military_doctrine_adopters).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, humanitarian_law_custodians).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, civilian_populations_conflict_zones).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain operational tempo advantages and reduced casualties among combatants by deploying autonomous systems certified as meeting distinction/proportionality outcomes thresholds. Can plan campaigns knowing legal compliance is a function of measurable technical performance rather than interpretive judgment.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, military_doctrine_adopters, beneficiary,
    institutional, generational, constrained, global).

% Capture a market for autonomous warfare systems by translating the outcomes-based reading into certification criteria. Once performance benchmarks are set, manufacturers compete on meeting them; the reading legitimizes the product class and creates demand from militaries seeking legal cover.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, autonomous_weapons_manufacturers, beneficiary,
    powerful, generational, mobile, global).

% Bear interpretive authority loss as the outcomes-based reading shifts law-compliance from principled judgment by qualified human operators to metric satisfaction by machines. Their role in IHL application is displaced by technical evaluation; their objections (that the reading breaks the Martens Clause's humanity principle) are structurally excluded from the certification process.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, humanitarian_law_custodians, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__outcomes_based_reading, humanitarian_law_custodians, excluded).

% Depend on distinction and proportionality being enforced with irreducible human judgment at the moment of lethal decision. If the outcomes-based reading permits deployment of systems meeting only aggregate statistical targets (e.g., error rate below human baseline), civilians face risk of legal strikes on locations with ambiguous civilian status, calculated at the system level rather than adjudicated per incident.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, civilian_populations_conflict_zones, payer,
    powerless, immediate, trapped, global).

% Set and adjudicate the technical benchmarks against which autonomous systems are measured for IHL compliance. Define what 'equal to or exceeding human operator performance' means operationally: which scenarios, metrics, baseline human populations, statistical confidence levels. Their decisions are both technical and normative — they embed the outcomes-based reading into enforced criteria.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, performance_certification_bodies, agenda_setter,
    institutional, generational, analytical, global).

% Argue that the outcomes-based reading violates the Martens Clause and customary IHL principles of human dignity and irreducible moral agency. They are excluded from certification governance; their principled objections (machine killing is prohibited per se, regardless of technical performance) are treated as regulatory opposition rather than legal inputs.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, categorical_prohibition_advocates, excluded,
    organized, generational, constrained, global).

% Maintain that IHL's distinction and proportionality obligations require human moral judgment at the moment of force application. They see the outcomes-based reading as instrumentalizing human decision-making — reducing it to a baseline performance metric rather than recognizing it as an irreducible principle. Structurally excluded from the enforcement and certification process.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, human_agency_preservationists, excluded,
    organized, generational, constrained, global).

% Witness the contest between the three readings. ICRC, IHL treaty bodies, and customary law interpreters hold interpretive authority over what IHL requires, but the outcomes-based reading operates by displacing that authority into technical performance domains. They observe, sometimes object, but the reading's adoption by major military powers creates facts on the ground.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, international_humanitarian_law_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ihl_distinction_proportionality__outcomes_based_reading, autonomous_weapons_manufacturers).
narrative_ontology:fixing_cost_class(ihl_distinction_proportionality__outcomes_based_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a uniform, technology-neutral standard for evaluating whether autonomous weapons systems satisfy IHL's core obligations (distinction between combatants and civilians, proportionality between military advantage and civilian harm). Solves the coordination problem of permitting innovation in weapons technology without creating a regulatory vacuum — law attaches to measurable outcomes, not to weapon design choices.
% TRANSFER_FUNCTION: Transfers interpretive authority over IHL compliance from international humanitarian law custodians and human commanders to technical certification bodies and manufacturers. Transfers legal risk from military adopters (who can now claim systems meeting certified benchmarks are lawful) to civilian populations (who depend on those benchmarks being set and enforced with irreducible human judgment).
% ABSENT_VOICES: Categorical prohibition advocates and human-agency preservationists are structurally excluded: their principled objections (machine killing violates the Martens Clause; human judgment is irreducible to performance metrics) are not seated in the certification process. Civilian populations most affected by autonomous targeting have no formal voice in benchmark-setting; their participation is mediated through humanitarian law bodies, which the outcomes-based reading partially displaces.
% DISAPPEARANCE_RATIONALE: If the outcomes-based reading and its certification apparatus vanished, militaries would face legal uncertainty about autonomous weapons: no clear performance standard would justify deployment, and IHL's distinction/proportionality obligations would revert to requiring human judgment at the moment of force application. Weapons development would slow or redirect toward human-supervised systems; existing deployments would face legal challenge. The market for autonomous weapons would collapse without regulatory legitimation.
% FOUNDING_PROBLEM: Rapid advances in autonomous systems and machine learning created urgent pressure to clarify whether IHL permits their military use. Doctrine confusion and regulatory gaps threatened to produce unlawful deployments or legal challenges to lawful ones. The outcomes-based reading was constructed to answer: 'Yes, if performance is measurable and meets the human baseline.'
% FOUNDING_PROBLEM_CORROBORATION: Military adopters and manufacturers affirm the founding problem remains live (ongoing capability development requires legal clarity). Humanitarian law bodies contest the founding problem diagnosis: they argue the problem is not 'what performance standard justifies autonomy' but 'whether autonomous killing is permissible in principle.' Academic research and NGO testimony from outside the military-manufacturer alliance support the contested reading.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__outcomes_based_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__outcomes_based_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__outcomes_based_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(ihl_distinction_proportionality__outcomes_based_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__outcomes_based_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_distinction_proportionality__outcomes_based_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ihl_distinction_proportionality__outcomes_based_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because the outcomes-based reading produces real coordination benefit (clarity on when autonomy is lawful) alongside extraction from humanitarian authority and civilian safety margins. Suppression is substantial (0.62) because the reading's persistence requires actively excluding the categorical prohibition and human-agency arguments from certification governance — those objections must be treated as regulatory opposition rather than legal inputs. Theater is moderate (0.41): the measurement process (testing systems against human baseline) is functionally real, but it increasingly serves to justify deployments that embody a normative choice (outcomes over principles) rather than to adjudicate whether that choice satisfies IHL. The measurement series show extractiveness rising and plateauing (initial uncertainty, then stabilization as certification bodies establish standard benchmarks and adopters deploy systems meeting them), suppression holding steady (the excluded voices remain excluded once the apparatus is established), and theater slightly rising (as the normative nature of the benchmarks becomes clearer, more of the certification process performs legitimacy rather than verifies compliance).
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (military adopters, manufacturers) compute the constraint as rope — genuine coordination on legal clarity. The payer seats (humanitarian custodians, civilians) compute it as snare — extraction of interpretive authority and safety margins under cover of technical neutrality. The agenda-setter seat (certification bodies) holds itself as observer, but is structurally committed to producing benchmarks the reading requires. The engine computes each seat's type from the power, exit, beneficiary/victim, and enforcement data; the claimed type (tangled_rope) reflects the structure: genuine coordination (clarity on autonomous deployment) AND asymmetric extraction (authority loss, risk shift to powerless populations) AND active enforcement (suppressing contrary readings).
 *
 * DIRECTIONALITY LOGIC:
 *   Military adopters and manufacturers are beneficiaries (d near 0.2): they gain operational efficiency and market opportunity without bearing the interpretive or safety costs. Humanitarian custodians are victims (d near 0.75): their authority is displaced and they have constrained exit (they cannot simply abandon IHL, but their voice is structurally excluded from the enforcement apparatus). Civilians are victims (d near 0.9): they are powerless, trapped in conflict zones, and bear the risk of aggregate statistical targeting; their exit is identity_locked (they cannot leave their civil status). Performance certification bodies are the agenda-setter (d near 0.5 for their structural position as rule-makers, but with powerful bias toward legitimizing the reading they are set up to instantiate).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT exhibit mandatrophy in the classical sense (founding problem dead, arrangement persisting). The founding problem (legal clarity on autonomous weapons) remains contested but live — military adoption is real and growing, and humanitarian bodies continue to debate the legality. However, the constraint does show a forking pattern: the outcomes-based reading began as one answer to the founding problem; as military adoption proceeded, it became institutionalized as THE answer, transforming the founding problem from 'what does IHL permit?' to 'does our system meet the certified threshold?' The reading's persistence depends on certification bodies continuing to produce benchmarks that military adopters can meet — a real coordination problem. But the constraint's stability also depends on suppressing the categorical prohibition and human-agency readings, which remain normatively coherent objections. This is tangled rope, not piton: the coordination function is real (clarity on legal deployment), but its persistence requires active suppression of competing legal interpretations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performance_baseline_ambiguity,
    'What constitutes ''equal to or exceeding human operator performance'' for distinction and proportionality in actual combat conditions?',
    'Specification of which human operators (elite forces, average combatants, command staff), which scenarios (conventional urban warfare, asymmetric conflict, night operations), which error metrics (false positive rate, discrimination threshold, collateral damage estimates), and which statistical confidence levels (single-incident parity or aggregate equivalence) define the threshold.',
    'A loose specification (aggregate statistics, select scenarios) permits more aggressive autonomy deployment and benefits manufacturers. A tight specification (per-incident human-equivalent judgment) approaches the human-agency reading''s requirements. The performance baseline is the reading''s core operation; its ambiguity is where much of the extractive power lies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(performance_baseline_ambiguity, conceptual, 'Specification of the technical threshold that operationalizes ''equal performance'' remains contested and in practice embeds normative choices.').

omega_variable(
    certification_authority_legitimacy,
    'Who legitimately sets and adjudicates the performance benchmarks, and by what process are excluded voices (categorical prohibition advocates, humanitarian bodies with principled objections) included or systematically excluded?',
    'Examination of certification body composition, decision processes, stakeholder input mechanisms, and appeals procedures. Comparison with parallel regulatory domains (pharmaceutical approval, nuclear safety) to assess whether humanitarian and principled-objection inputs are structurally available.',
    'If certification is dominated by military and manufacturer interests with humanitarian voices only advisory or excluded, the reading''s persistence depends on suppression. If humanitarian bodies hold veto or binding input, the reading''s operation would shift toward human-agency constraints. The legitimacy of the constraint depends heavily on how much suppression is institutional versus consensual.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(certification_authority_legitimacy, empirical, 'Whether the outcomes-based reading''s enforcement apparatus is legitimate as currently constituted or depends on suppressing competing legal interpretations.').

omega_variable(
    measurement_failure_risk_allocation,
    'If an autonomous system is certified as meeting the outcomes-based threshold, and then commits a clear violation of distinction or proportionality in actual combat, who bears the liability and how is the reading''s authority affected?',
    'Case law analysis of actual alleged autonomous weapons violations; examination of liability assignment mechanisms in military doctrine and international law; test of whether the reading''s legitimacy survives demonstration that the performance threshold was insufficient.',
    'If military adopters bear liability, the reading becomes a true coordination mechanism (shared risk). If liability is displaced to certification bodies (creating moral hazard) or to victims (denying redress), the constraint shows its extractive character. The resolution determines whether the reading can survive operational failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_failure_risk_allocation, empirical, 'Liability and accountability mechanisms reveal whether outcomes-based certification produces genuine shared responsibility or displaces risk to powerless populations.').

omega_variable(
    martens_clause_compatibility,
    'Can the outcomes-based reading''s technical performance standard satisfy the Martens Clause requirement that all conduct remain ''under the protection and authority of the principles of international law derived from established custom, from the principles of humanity and from dictates of public conscience''?',
    'Legal analysis of whether aggregate performance parity between machines and humans qualifies as applying ''principles of humanity'' and respecting ''public conscience.'' Empirical assessment of whether public conscience globally accepts outcomes-based autonomous killing or whether principled objection persists.',
    'If the outcomes-based reading can be reconciled with Martens Clause principles, it is genuinely lawful and the constraint is rope-like. If it cannot, the constraint''s legal foundation is contested and it persists through suppression of principled objection. This is the core legal contestation between the readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(martens_clause_compatibility, conceptual, 'Whether outcomes-based autonomy is compatible with foundational IHL principles or violates them despite meeting technical performance targets.').

omega_variable(
    reading_foreclosure_via_adoption,
    'As military powers adopt outcomes-based certified autonomous weapons, does widespread deployment functionally foreclose the categorical prohibition and human-agency readings by creating facts on the ground and sunk costs?',
    'Observation of whether adoption of outcomes-based systems makes reversal to human-judgment-only requirements politically impossible (weapons stockpiles, doctrine lock-in, industrial investment) or whether principled reversal remains live.',
    'If adoption forecloses the other readings functionally (not logically, but practically), the outcomes-based reading becomes de facto IHL standard. If adoption coexists with continued principled dispute, the three readings remain live. This determines the long-term trajectory of the constraint and whether it becomes the sole legitimate reading or remains contested.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_via_adoption, preference, 'Whether operational adoption of outcomes-based autonomy forecloses the other readings through institutional lock-in or whether principled objection and human-agency requirements remain live policy options.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__outcomes_based_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t0, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(ihl__tr_t0, projected).
narrative_ontology:measurement(ihl__tr_t5, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(ihl__tr_t5, projected).
narrative_ontology:measurement(ihl__tr_t10, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement_basis(ihl__tr_t10, observed).
narrative_ontology:measurement(ihl__tr_t15, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(ihl__tr_t15, observed).
narrative_ontology:measurement(ihl__tr_t20, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(ihl__tr_t20, observed).
narrative_ontology:measurement(ihl__tr_t25, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(ihl__tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(ihl__be_t0, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(ihl__be_t0, projected).
narrative_ontology:measurement(ihl__be_t5, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement_basis(ihl__be_t5, projected).
narrative_ontology:measurement(ihl__be_t10, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(ihl__be_t10, observed).
narrative_ontology:measurement(ihl__be_t15, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 15, 0.56).
narrative_ontology:measurement_basis(ihl__be_t15, observed).
narrative_ontology:measurement(ihl__be_t20, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement_basis(ihl__be_t20, observed).
narrative_ontology:measurement(ihl__be_t25, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement_basis(ihl__be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t0, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(ihl__su_t0, projected).
narrative_ontology:measurement(ihl__su_t5, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement_basis(ihl__su_t5, projected).
narrative_ontology:measurement(ihl__su_t10, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 10, 0.59).
narrative_ontology:measurement_basis(ihl__su_t10, observed).
narrative_ontology:measurement(ihl__su_t15, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 15, 0.61).
narrative_ontology:measurement_basis(ihl__su_t15, observed).
narrative_ontology:measurement(ihl__su_t20, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement_basis(ihl__su_t20, observed).
narrative_ontology:measurement(ihl__su_t25, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement_basis(ihl__su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, categorical_prohibition_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, human_agency_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality__categorical_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality__human_agency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel 'IHL distinction/proportionality obligations in autonomous warfare.' All three readings share the same domain (international humanitarian law, military ethics) and the same underlying legal text (IHL rules on distinction and proportionality, the Martens Clause), but they instantiate different constraints because they embed different answers to 'what does IHL require of autonomous systems.' The ε values, beneficiary/victim structures, and enforcement mechanisms differ substantially across readings. This reading (outcomes-based) permits autonomy when measurable performance justifies it. The categorical-prohibition reading forbids autonomy per se. The human-agency reading requires irreducible human judgment. Each reading is a structurally distinct constraint; the family structure enables tracking how the same legal domain produces different constraints under different interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ihl_distinction_proportionality__outcomes_based_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
