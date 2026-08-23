% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__human_agency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_distinction_proportionality__human_agency_reading, []).

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
 *   constraint_id: ihl_distinction_proportionality__human_agency_reading
 *   human_readable: IHL Distinction/Proportionality — Human Agency Reading
 *   domain: international_humanitarian_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   This constraint story captures the human agency reading of IHL's
 *   distinction and proportionality obligations — the position that
 *   irreducible human moral judgment is required at the moment of lethal
 *   force application, grounded in the Martens Clause principles of humanity.
 *   The reading renders fully autonomous weapons systems (LAWS) categorically
 *   unlawful while authorizing human-supervised autonomy. The constraint has
 *   high extractiveness (ε=0.78) because it suppresses an entire
 *   technological trajectory (fully autonomous targeting) and redirects
 *   massive R&D investment, transferring operational advantages from military
 *   commands to the interpretive authority. Suppression is high (0.82)
 *   because the constraint's persistence depends on active enforcement
 *   through treaty compliance mechanisms, customary law development, and
 *   diplomatic pressure — not voluntary adherence. The measurement series
 *   (2000-2025) shows rising extraction and suppression as autonomous
 *   capabilities advance but the legal constraint holds firm, creating
 *   widening tension between technical possibility and legal permission.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__human_agency_reading, 0.78).
domain_priors:suppression_score(ihl_distinction_proportionality__human_agency_reading, 0.82).
domain_priors:theater_ratio(ihl_distinction_proportionality__human_agency_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__human_agency_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__human_agency_reading, "IHL Distinction/Proportionality — Human Agency Reading").
narrative_ontology:topic_domain(ihl_distinction_proportionality__human_agency_reading, "international_humanitarian_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__human_agency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__human_agency_reading, '71e7239a-f69f-4aba-ab04-af7a6f341902').
narrative_ontology:cs_kernel_codification('71e7239a-f69f-4aba-ab04-af7a6f341902', fixed_text).
narrative_ontology:cs_authority_grounding('71e7239a-f69f-4aba-ab04-af7a6f341902', lineage).
narrative_ontology:cs_interpretation_layer_present('71e7239a-f69f-4aba-ab04-af7a6f341902').
narrative_ontology:cs_reading_relation('71e7239a-f69f-4aba-ab04-af7a6f341902', ihl_distinction_proportionality__categorical_prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('71e7239a-f69f-4aba-ab04-af7a6f341902', ihl_distinction_proportionality__outcomes_based_reading, influences).
narrative_ontology:cs_axiom('71e7239a-f69f-4aba-ab04-af7a6f341902', foundational, irreducible_human_moral_judgment_required).
narrative_ontology:cs_axiom_status(irreducible_human_moral_judgment_required, holdable).
narrative_ontology:cs_axiom_grounding('71e7239a-f69f-4aba-ab04-af7a6f341902', irreducible_human_moral_judgment_required, deontological).
narrative_ontology:cs_axiom('71e7239a-f69f-4aba-ab04-af7a6f341902', foundational, martens_clause_prohibits_delegation_to_machines).
narrative_ontology:cs_axiom_status(martens_clause_prohibits_delegation_to_machines, holdable).
narrative_ontology:cs_axiom_grounding('71e7239a-f69f-4aba-ab04-af7a6f341902', martens_clause_prohibits_delegation_to_machines, deontological).
narrative_ontology:cs_reference_frame('71e7239a-f69f-4aba-ab04-af7a6f341902', martens_clause_humanity_standard).
narrative_ontology:cs_drift_state('71e7239a-f69f-4aba-ab04-af7a6f341902', autonomous_weapons_debate_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('71e7239a-f69f-4aba-ab04-af7a6f341902', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, icrc_interpretive_authority).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, ihl_interpretive_community).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, humanitarian_civil_society).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, civilian_populations_in_conflict).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, military_operational_efficiency).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, autonomous_weapons_developers).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, state_military_commands).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, state_military_commands).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__human_agency_reading, martens_clause_principles_of_humanity).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__human_agency_reading, distinction_obligation).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__human_agency_reading, proportionality_obligation).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__human_agency_reading, irreducible_human_moral_judgment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Serves as the primary guardian and interpreter of IHL. Issues authoritative commentaries, drives customary law studies, and shapes the legal framework governing autonomous weapons. Maintains institutional centrality by defining what counts as compliance with distinction and proportionality. Collects legitimacy and operational authority from being the recognized interpreter.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, icrc_interpretive_authority, agenda_setter,
    institutional, generational, arbitrage, global).

% International lawyers, military legal advisors, and academics who build careers and authority within the IHL interpretive framework. Their professional standing depends on the centrality of human judgment in legal analysis. They benefit from the constraint's enforcement through institutional positions, consultancies, and epistemic authority.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, ihl_interpretive_community, beneficiary,
    organized, biographical, constrained, global).

% NGOs and advocacy organizations (Human Rights Watch, Article 36, ICRAC) that campaign for human control over lethal force. They gain moral authority, funding, and policy influence from the constraint. Their advocacy infrastructure is built around the human agency frame.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, humanitarian_civil_society, beneficiary,
    organized, biographical, mobile, global).

% The ultimate intended beneficiaries of distinction and proportionality protections. They bear the consequences when the constraint fails but have no voice in its interpretation or enforcement. Their protection is the constraint's stated coordination function.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, civilian_populations_in_conflict, beneficiary,
    powerless, immediate, trapped, local).

% State military commands and defense establishments that seek speed, scale, and precision advantages from autonomous systems. They bear the costs of foregone capabilities, compliance overhead, and strategic disadvantage relative to adversaries who may not comply. Exit means treaty withdrawal or non-compliance — both politically costly.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, military_operational_efficiency, payer,
    institutional, biographical, constrained, global).

% Defense contractors and technology firms developing LAWS. They face suppressed markets, redirected R&D investment, and regulatory uncertainty. Their exit options are pivoting to non-lethal autonomy, dual-use civilian applications, or selling to non-compliant states — all constrained by the same legal framework.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, autonomous_weapons_developers, payer,
    powerful, biographical, constrained, global).

% National military hierarchies that both bear compliance costs and benefit from the legal clarity and legitimacy the constraint provides. They pay in operational flexibility but collect in legal defensibility, alliance interoperability, and domestic political cover. Dual-positioned: constrained payer on autonomy, beneficiary on legal framework.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, state_military_commands, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__human_agency_reading, state_military_commands, beneficiary).

% Academic researchers analyzing the ethical, legal, and technical dimensions of autonomous weapons. They observe the constraint's operation without directly collecting rents or bearing its costs. Their work informs but does not drive the interpretive authority.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, technology_ethics_scholars, observer,
    moderate, biographical, analytical, global).

% State representatives in UN GGE on LAWS, CCW meetings, and treaty negotiations. They observe and negotiate the constraint's evolution but their institutional role is process management rather than substantive interpretation. They see the full structural picture across readings.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, arms_control_diplomats, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solving the collective action problem of limiting unnecessary suffering in warfare by establishing legally binding standards for distinction (between combatants and civilians) and proportionality (between military advantage and civilian harm) that all parties must respect, enforced through treaty law and customary international law.
% TRANSFER_FUNCTION: Moves operational autonomy, speed, and efficiency from military commands and weapons developers to the IHL interpretive authority (ICRC and associated community) via the legal requirement that a human make the final targeting decision. The transfer is legitimated as civilian protection but functionally concentrates interpretive power.
% ABSENT_VOICES: Military AI developers and defense innovation units who argue human-machine teaming outperforms human-only decisions; states actively developing LAWS (US, Russia, Israel, China) whose military legal advisors often favor the outcomes-based reading; some IHL scholars who contend the law governs outcomes not means. These voices are structurally excluded from the ICRC's interpretive process and the humanitarian NGO coalition that amplifies the human agency frame.
% DISAPPEARANCE_RATIONALE: If the human agency requirement vanished overnight, fully autonomous targeting would become legally permissible under IHL. Major military powers would accelerate LAWS deployment within 2-5 years. Defense procurement would restructure around algorithmic targeting. The ICRC's interpretive centrality would collapse. Civilian protection would depend entirely on technical performance metrics rather than legal judgment. The entire arms control architecture for autonomous weapons would dissolve.
% FOUNDING_PROBLEM: The problem of ensuring moral responsibility and legal accountability for lethal decisions in increasingly industrialized and technologically mediated warfare, where distance and automation threaten to sever the link between human agency and killing.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC's 1987 Commentaries and 2005 Customary Law Study attest the founding problem is live. However, the 2012-2024 UN GGE debates reveal fundamental contestation: outcomes-based reading proponents (supported by several major military powers' legal advisors) argue technology has changed the problem's parameters. No neutral third party corroborates either side — the dispute maps onto geopolitical alignments. The Martens Clause drafting history (1899/1907) is invoked by both sides.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__human_agency_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__human_agency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__human_agency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ihl_distinction_proportionality__human_agency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__human_agency_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__human_agency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_distinction_proportionality__human_agency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ihl_distinction_proportionality__human_agency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed type is tangled_rope because the constraint has a genuine coordination function (distinction/proportionality are real collective action problems in warfare — without them, civilian harm escalates uncontrollably) AND asymmetric extraction (military operational efficiency and weapons developers bear concentrated costs while ICRC and the interpretive community collect concentrated interpretive authority and legitimacy). The coordination function is not cover — civilian protection is measurably improved by legal constraints on targeting. But the extraction is real and asymmetric: the human agency requirement is the specific mechanism that preserves ICRC centrality. Theater ratio is low-moderate (0.28) because the legal review and compliance machinery is functionally necessary, though a growing share of diplomatic activity performs compliance without constraining major powers' development programs.
 *
 * PERSPECTIVAL GAP:
 *   From the ICRC/interpreter seat, the constraint is genuine coordination solving a real humanitarian problem — the human judgment requirement is the solution. From the military command seat, the same structure operates as enforced extraction — they lose capabilities that could reduce collateral damage (per their own testing) because the law forbids the means, not the outcome. From the civilian seat, the constraint is existential protection with no voice in its calibration. The engine computes these per-seat divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   ICRC and the interpretive community are structural beneficiaries (d ≈ 0.15) — they collect authority, legitimacy, and institutional centrality from the constraint. Civilian populations are beneficiaries with trapped exit (d ≈ 0.25) — they gain protection but cannot exit the conflict zones where the constraint operates. Military commands and weapons developers are payers (d ≈ 0.85) — they bear the costs of suppressed autonomy and redirected investment with constrained exit (treaty withdrawal is politically prohibitive). State military commands are dual-positioned: they pay in operational flexibility but collect in legal defensibility and alliance interoperability. The directionality derivation from beneficiary/victim declarations plus exit options produces this gradient; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (moral responsibility for lethal decisions in technologically mediated warfare) is contested, not dead. Autonomous systems proponents argue the problem has changed: AI can now process distinction/proportionality faster and more consistently than humans under combat conditions. Human agency proponents argue the problem is structural: moral judgment cannot be delegated regardless of performance. The constraint has not atrophied into a piton — it is actively enforced and its interpretation is the live center of the LAWS debate. But mandatrophy risk is real: if the outcomes-based reading gains treaty-level acceptance, the human agency reading becomes a scaffold whose transition (to performance-based standards) has been blocked by interpretive inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the human_agency_reading a distinct constraint from its siblings, or a rhetorical position within a single constraint?',
    'Apply the ε-invariance test: if measuring the constraint via ''human judgment required'' yields high ε (suppresses LAWS) but measuring via ''civilian harm outcomes'' yields low ε (some LAWS reduce harm), then ε changes with the observable — this indicates multiple constraints, not one. The kernel_id/reading_id decomposition in the manifest confirms structural separation.',
    'If readings are distinct constraints, each gets its own ε, stakeholders, and classification. If they are one constraint with measurement ambiguity, the framework''s ε-invariance principle is violated. The decomposition into three stories linked by network.affects_constraints is the correct modeling choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s label covers one constraint or three structurally distinct ones').

omega_variable(
    coordination_extraction_boundary,
    'Is the human judgment requirement structurally necessary for the coordination function (civilian protection), or does it serve primarily to preserve ICRC interpretive centrality?',
    'Natural experiment: compare civilian harm outcomes in conflicts where human-supervised autonomy operates vs. fully autonomous operations (where they occur despite the constraint) vs. human-only targeting. If supervised autonomy achieves equal or better outcomes than human-only, the human judgment requirement extracts without coordination gain.',
    'If the requirement is structurally necessary, the constraint is a genuine tangled rope (coordination + extraction). If it is institutionally preservative, the coordination story is cover and the constraint trends toward snare. The ε-invariance principle demands this distinction be resolved by observable outcomes, not rhetorical framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the human agency requirement is a genuine coordination mechanism or institutional preservation').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the constraint''s suppression of LAWS structural (treaty law, diplomatic pressure) or internalized (military legal advisors self-censoring, doctrinal path dependence)?',
    'Track LAWS development trajectories in states that are party to relevant treaties vs. non-parties. If suppression persists in non-party states, internalized normative pressure is significant. If non-parties develop freely, suppression is primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the legal machinery suggests — the constraint operates through professional identity and doctrinal inertia, not just formal enforcement. This affects theater_ratio interpretation and mandatrophy assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in military organizations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__human_agency_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl_human_agency_tr_t0, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ihl_human_agency_tr_t5, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(ihl_human_agency_tr_t10, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(ihl_human_agency_tr_t15, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement(ihl_human_agency_tr_t20, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(ihl_human_agency_tr_t25, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(ihl_human_agency_be_t0, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ihl_human_agency_be_t5, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(ihl_human_agency_be_t10, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(ihl_human_agency_be_t15, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(ihl_human_agency_be_t20, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 20, 0.74).
narrative_ontology:measurement(ihl_human_agency_be_t25, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 25, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(ihl_human_agency_su_t0, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(ihl_human_agency_su_t5, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(ihl_human_agency_su_t10, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(ihl_human_agency_su_t15, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(ihl_human_agency_su_t20, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(ihl_human_agency_su_t25, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 25, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__human_agency_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ihl_distinction_proportionality__human_agency_reading, 0.12).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, laws_regulation_treaty).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, icrc_interpretive_authority).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, martens_clause_customary_law).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality__categorical_prohibition_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality__outcomes_based_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the ihl_distinction_proportionality kernel family. The three readings (human_agency_reading, categorical_prohibition_reading, outcomes_based_reading) have different ε values and different victim/beneficiary structures. human_agency_reading ε=0.78 (high extraction from military efficiency); categorical_prohibition_reading ε≈0.65 (broader suppression but less targeted extraction); outcomes_based_reading ε≈0.25 (low extraction, performance-based coordination). They are linked by network.affects_constraints in both directions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
