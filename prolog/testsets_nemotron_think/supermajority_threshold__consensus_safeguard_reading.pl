% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__consensus_safeguard_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supermajority_threshold__consensus_safeguard_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: supermajority_threshold__consensus_safeguard_reading
 *   human_readable: Supermajority Threshold as Consensus Safeguard
 *   domain: constitutional_theory/political_economy/institutional_design
 *
 * SUMMARY:
 *   This constraint story captures the 'consensus_safeguard_reading' of the
 *   supermajority threshold kernel. The reading presents the threshold as a
 *   democratic quality filter — a natural requirement of legitimate
 *   constitutionalism that ensures changes reflect deep, persistent consensus
 *   rather than transient majoritarian passion. The constraint operates as a
 *   high amendment barrier that produces stability with diffuse beneficiaries
 *   (citizens, minority groups, constitutional institutions) and no specific
 *   victim set in normal operation; victims emerge only when the threshold
 *   blocks a reform majority. The reading legitimates the barrier as
 *   intrinsic to democratic legitimacy, not as a contingent institutional
 *   choice.
 *
 * KEY AGENTS:
 *   - citizens: Primary beneficiaries (organized/generational/constrained) — gain constitutional stability
 *   - minority_groups: Primary beneficiaries (moderate/generational/constrained) — gain protection against majoritarian override
 *   - constitutional_institutions: Beneficiaries and agenda_setters (institutional/generational/analytical) — administer and benefit from threshold stability
 *   - reform_majorities: Potential payers (powerful/biographical/constrained) — bear costs when threshold blocks change
 *   - amendment_proponents: Potential payers (moderate/biographical/constrained) — bear campaign costs that threshold may nullify
 *   - constitutional_court: Agenda_setter (institutional/generational/analytical) — authoritatively enforces threshold
 *   - political_scientists: Observers (analytical/civilizational/analytical) — analyze constraint from external vantage
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__consensus_safeguard_reading, 0.15).
domain_priors:suppression_score(supermajority_threshold__consensus_safeguard_reading, 0.6).
domain_priors:theater_ratio(supermajority_threshold__consensus_safeguard_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__consensus_safeguard_reading, mountain).
narrative_ontology:human_readable(supermajority_threshold__consensus_safeguard_reading, "Supermajority Threshold as Consensus Safeguard").
narrative_ontology:topic_domain(supermajority_threshold__consensus_safeguard_reading, "constitutional_theory/political_economy/institutional_design").

domain_priors:requires_active_enforcement(supermajority_threshold__consensus_safeguard_reading).
domain_priors:emerges_naturally(supermajority_threshold__consensus_safeguard_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__consensus_safeguard_reading, '24a344f9-c475-4d9f-adf3-335a7dca7bb2').
narrative_ontology:cs_kernel_codification('24a344f9-c475-4d9f-adf3-335a7dca7bb2', formalized).
narrative_ontology:cs_authority_grounding('24a344f9-c475-4d9f-adf3-335a7dca7bb2', lineage).
narrative_ontology:cs_interpretation_layer_present('24a344f9-c475-4d9f-adf3-335a7dca7bb2').
narrative_ontology:cs_reading_relation('24a344f9-c475-4d9f-adf3-335a7dca7bb2', supermajority_threshold__minoritarian_veto_reading, coexists_with).
narrative_ontology:cs_reading_relation('24a344f9-c475-4d9f-adf3-335a7dca7bb2', supermajority_threshold__adaptive_gradient_reading, forecloses).
narrative_ontology:cs_axiom('24a344f9-c475-4d9f-adf3-335a7dca7bb2', foundational, constitutional_change_requires_deep_consensus).
narrative_ontology:cs_axiom_status(constitutional_change_requires_deep_consensus, holdable).
narrative_ontology:cs_axiom_grounding('24a344f9-c475-4d9f-adf3-335a7dca7bb2', constitutional_change_requires_deep_consensus, deontological).
narrative_ontology:cs_axiom('24a344f9-c475-4d9f-adf3-335a7dca7bb2', secondary, transient_majorities_lack_constituting_authority).
narrative_ontology:cs_axiom_status(transient_majorities_lack_constituting_authority, holdable).
narrative_ontology:cs_axiom_grounding('24a344f9-c475-4d9f-adf3-335a7dca7bb2', transient_majorities_lack_constituting_authority, deontological).
narrative_ontology:cs_reference_frame('24a344f9-c475-4d9f-adf3-335a7dca7bb2', fixed_supermajority_consensus_requirement).
narrative_ontology:cs_drift_state('24a344f9-c475-4d9f-adf3-335a7dca7bb2', contemporary_polarization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('24a344f9-c475-4d9f-adf3-335a7dca7bb2', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__consensus_safeguard_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__consensus_safeguard_reading, citizens).
narrative_ontology:constraint_beneficiary(supermajority_threshold__consensus_safeguard_reading, minority_groups).
narrative_ontology:constraint_beneficiary(supermajority_threshold__consensus_safeguard_reading, constitutional_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(supermajority_threshold__consensus_safeguard_reading, reform_majorities).
narrative_ontology:constraint_victim(supermajority_threshold__consensus_safeguard_reading, amendment_proponents).
narrative_ontology:constraint_vindicates(supermajority_threshold__consensus_safeguard_reading, constitutional_continuity_doctrine).
narrative_ontology:constraint_vindicates(supermajority_threshold__consensus_safeguard_reading, deep_consensus_requirement).
narrative_ontology:constraint_vindicates(supermajority_threshold__consensus_safeguard_reading, transient_majority_illegitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a stable constitutional order that prevents abrupt shifts in fundamental rights and governance structures. The threshold protects the constitutional framework within which ordinary politics operates. Exit from the constitutional order is practically impossible for individuals.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, citizens, beneficiary,
    organized, generational, constrained, national).

% Gain protection against majoritarian measures that could strip rights or alter structural protections. The supermajority requirement means their core interests cannot be overridden by a transient simple majority. Their exit options are limited to emigration or constitutional challenge.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, minority_groups, beneficiary,
    moderate, generational, constrained, national).

% Courts and amendment bodies administer the threshold. They benefit from the institutional stability the threshold provides, which secures their own legitimacy and operational continuity. They set the agenda for what counts as a valid amendment proposal.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, constitutional_institutions, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(supermajority_threshold__consensus_safeguard_reading, constitutional_institutions, agenda_setter).

% Political coalitions that command simple majorities but not supermajorities. When they seek constitutional change, the threshold blocks them — they bear the cost of the constraint's suppression function. Their exit is constrained: they must either build broader consensus, wait for electoral realignment, or pursue extra-constitutional paths.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, reform_majorities, payer,
    powerful, biographical, constrained, national).

% Actors proposing specific constitutional amendments. They invest resources in campaigns that fail if the supermajority threshold is not met. The threshold raises the cost of constitutional entrepreneurship. Exit means abandoning the amendment or pursuing statutory instead of constitutional change.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, amendment_proponents, payer,
    moderate, biographical, constrained, national).

% Authoritatively interprets and enforces the supermajority requirement — certifying vote counts, reviewing amendment procedures, adjudicating challenges. Its legitimacy depends on the threshold being treated as a binding constitutional rule rather than a political guideline.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, constitutional_court, agenda_setter,
    institutional, generational, analytical, national).

% Study the threshold's effects on constitutional stability, democratic responsiveness, and minority protection. They observe the constraint from outside the political contest, providing comparative and theoretical analysis that informs but does not determine its operation.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, political_scientists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(supermajority_threshold__consensus_safeguard_reading, diffuse).
narrative_ontology:fixing_cost_class(supermajority_threshold__consensus_safeguard_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of constitutional stability: ensures that fundamental law changes only when a broad, persistent consensus exists, preventing transient majorities from imposing irreversible changes on minorities and future generations.
% TRANSFER_FUNCTION: Transfers agenda-setting power from simple majorities to supermajority coalitions. The constraint moves the authority to amend the constitution from the electorate's momentary majority to a more demanding consensus threshold, effectively transferring veto power to any bloc that can sustain the blocking minority.
% ABSENT_VOICES: Future generations who inherit the constitutional order but cannot participate in its amendment; disenfranchised populations excluded from the supermajority calculus; would-be reformers in systems where the threshold is practically unattainable due to polarization. They are structurally absent from the amendment process itself.
% DISAPPEARANCE_RATIONALE: If the supermajority threshold vanished overnight, constitutional amendment would revert to simple majority rule. This would enable rapid constitutional change by transient majorities, potentially destabilizing rights protections, federal arrangements, and institutional checks. The constitutional order would reorganize around majoritarian amendment dynamics.
% FOUNDING_PROBLEM: The founding problem was the instability of early constitutional regimes where simple majority amendment allowed each new legislative majority to rewrite fundamental law, producing constitutional churn that undermined rights, federal compacts, and intergenerational trust.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by constitutional historians (e.g., Ackerman on constitutional moments, Elkins-Ginsburg-Melton on constitutional endurance) and comparative constitutional scholars outside the beneficiary institutions. The status is contested: originalist scholars argue the problem persists; living constitutionalists argue modern democratic safeguards have reduced the risk; political scientists note that polarization has created a new problem of amendment impossibility.
narrative_ontology:disappearance_verdict(supermajority_threshold__consensus_safeguard_reading, world_rearranges).
narrative_ontology:founding_problem_status(supermajority_threshold__consensus_safeguard_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__consensus_safeguard_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(supermajority_threshold__consensus_safeguard_reading, 'none', 1).
narrative_ontology:epsilon_provenance(supermajority_threshold__consensus_safeguard_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supermajority_threshold__consensus_safeguard_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, ExtMetricName, E),
    domain_priors:suppression_score(supermajority_threshold__consensus_safeguard_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(supermajority_threshold__consensus_safeguard_reading),
    narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(supermajority_threshold__consensus_safeguard_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The reading claims mountain (natural democratic law) with emerges_naturally=true. Metrics show low extractiveness (0.15) because the threshold does not extract resources in normal operation — its function is suppressive, not extractive. Suppression is moderate-high (0.60) because the threshold actively blocks simple majority amendment. Theater is low (0.10) because the constraint genuinely performs its coordination function. Accessibility collapse is very high (0.90) because simple majority amendment is structurally foreclosed. Resistance is low (0.20) because the threshold enjoys broad legitimacy as a constitutional design principle. The claim/metric gap is deliberate: the reading claims mountain while metrics describe a constructed constraint with active suppression.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (citizens, minorities, institutions) experience the constraint as mountain-like — a natural democratic requirement that protects them. The payer seats (reform majorities, amendment proponents) experience it as a suppressive barrier — a constraint that extracts their political agency when they command simple but not supermajorities. The engine computes this divergence from the structural data: beneficiaries have low directionality (d near 0), payers have high directionality (d near 1) when blocking occurs. The agenda_setter (court) sits near symmetric — it administers the constraint but also depends on its legitimacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (citizens, minorities, institutions) are structural beneficiaries: the constraint subsidizes their interest in stability. Their exit is constrained but they do not seek exit — the constraint serves them. Payers (reform majorities, proponents) are structural targets only when blocking occurs: the constraint extracts their amendment agenda. In normal operation (no amendment proposed), directionality is near-symmetric for all. The constitutional court as agenda_setter has analytical exit and institutional power — it neither benefits nor pays in the ordinary sense but holds the enforcement authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (constitutional instability under simple majority amendment) is contested as live/dead. If dead, the threshold persists as a piton (inertial maintenance). If live, it remains a rope (active coordination). The contested status prevents mandatrophy mislabeling: the threshold is not pure extraction (snare) because it has genuine coordination function and diffuse beneficiaries; it is not pure coordination (rope) because it actively suppresses majority will and creates contingent victims. The mountain claim with beneficiaries triggers FSM evaluation — the engine must determine whether the natural-law framing is a false summit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_threshold,
    'Is the supermajority threshold a natural requirement of democratic legitimacy (as this reading claims), or a contingent institutional choice that could be otherwise?',
    'Comparative constitutional analysis: if all stable democracies converge on supermajority requirements for constitutional change, this supports natural-law framing; if thresholds vary widely (60%, 66%, 75%, referendum+majority) without clear legitimacy differences, this supports constructed-choice framing.',
    'If natural law, the threshold is a genuine mountain with near-zero extractiveness. If constructed, it is a rope or tangled_rope with non-zero extraction from blocked majorities, and its specific threshold level is a policy choice subject to democratic revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_threshold, conceptual, 'Whether the threshold''s democratic legitimacy is intrinsic or instrumental').

omega_variable(
    kernel_reading_identity,
    'Does this reading instantiate a distinct constraint from its siblings, or are they merely different evaluations of the same constraint?',
    'Structural divergence test: if the three readings produce different beneficiary/victim structures, different ε values, and different classification outcomes when run through the engine independently, they are distinct constraints. If they differ only in commentary, they are one constraint with multiple perspectives.',
    'If distinct constraints, each must be authored separately with its own ε, stakeholders, and classification, linked via network.affects_constraints. If one constraint, the kernel frame is analytical only and the readings are perspectival slices.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether kernel readings map to distinct constraints in the DR framework').

omega_variable(
    blocking_minority_as_victim_or_beneficiary,
    'When the threshold blocks a reform majority, is the blocking minority a beneficiary (protected by the constraint) or a victim (empowered to veto against democratic will)?',
    'Case study of blocked amendments: analyze whether the blocking coalition represents vulnerable minorities seeking protection or entrenched interests preserving privilege. Track whether the same minority groups consistently benefit from the threshold across multiple amendment cycles.',
    'If blocking minorities are systematically vulnerable groups, the threshold functions as protection (beneficiary structure). If they are systematically privileged groups, the threshold functions as entrenchment (victim structure for the broader polity). This determines whether victims[] should be populated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(blocking_minority_as_victim_or_beneficiary, empirical, 'Structural position of the blocking minority in the constraint''s operation').

omega_variable(
    reading_relation_to_adaptive_gradient,
    'Does the consensus_safeguard_reading''s premise (fixed natural requirement) logically foreclose the adaptive_gradient_reading''s premise (calibratable instrument), or do they coexist?',
    'Logical analysis of the two premises: can a single constitutional framework treat the threshold as both a fixed natural law and an evidence-based calibration target? If the framework must choose one epistemic stance, they foreclose; if different actors can hold different stances simultaneously, they coexist.',
    'If forecloses, the consensus reading''s axioms contradict the adaptive reading''s axioms — the engine will compute foreclosure. If coexists_with, both readings remain live in the constraint family with influences edges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relation_to_adaptive_gradient, conceptual, 'Logical relationship between consensus and adaptive readings of the threshold').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__consensus_safeguard_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supermajority_consensus_tr_t0, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(supermajority_consensus_tr_t10, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement(supermajority_consensus_tr_t20, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(supermajority_consensus_tr_t30, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement(supermajority_consensus_tr_t40, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(supermajority_consensus_tr_t50, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(supermajority_consensus_be_t0, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(supermajority_consensus_be_t10, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 10, 0.1).
narrative_ontology:measurement(supermajority_consensus_be_t20, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 20, 0.12).
narrative_ontology:measurement(supermajority_consensus_be_t30, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 30, 0.14).
narrative_ontology:measurement(supermajority_consensus_be_t40, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(supermajority_consensus_be_t50, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 50, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(supermajority_consensus_su_t0, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(supermajority_consensus_su_t10, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(supermajority_consensus_su_t20, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(supermajority_consensus_su_t30, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(supermajority_consensus_su_t40, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(supermajority_consensus_su_t50, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 50, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__consensus_safeguard_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(supermajority_threshold__consensus_safeguard_reading, 0.08).
narrative_ontology:affects_constraint(supermajority_threshold__consensus_safeguard_reading, supermajority_threshold__minoritarian_veto_reading).
narrative_ontology:affects_constraint(supermajority_threshold__consensus_safeguard_reading, supermajority_threshold__adaptive_gradient_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the supermajority_threshold kernel family. The consensus_safeguard_reading claims mountain with diffuse beneficiaries; the minoritarian_veto_reading claims snare/tangled_rope with concentrated victims; the adaptive_gradient_reading claims rope/scaffold with calibration beneficiaries. All three share the kernel_id 'supermajority_threshold' and are linked via affects_constraints. The ε values differ substantially: consensus reading ε≈0.15 (quality filter), veto reading ε≈0.65 (blockage extraction), adaptive reading ε≈0.25 (calibration cost).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
