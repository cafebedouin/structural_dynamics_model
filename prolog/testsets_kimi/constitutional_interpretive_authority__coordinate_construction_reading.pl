% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__coordinate_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__coordinate_construction_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: constitutional_interpretive_authority__coordinate_construction_reading
 *   human_readable: Coordinate Construction Reading of Constitutional Interpretive Authority
 *   domain: constitutional law/political theory/jurisprudence
 *
 * SUMMARY:
 *   This constraint instantiates the coordinate construction reading of the
 *   constitutional interpretive authority kernel. Under this reading, no
 *   single branchâlegislative, executive, or judicialâpossesses final
 *   interpretive authority over constitutional meaning. Instead, the
 *   constitution is constructed through ongoing inter-branch dialogue and
 *   political contestation, with disputes resolved through mechanisms such as
 *   amendment, appointment, and budget control rather than singular
 *   adjudication. This reading stands in direct contest with judicial
 *   supremacy (courts as final arbiters) and parliamentary supremacy
 *   (legislature as final arbiter). The constraint carries genuine
 *   coordination valueâpreventing tyranny of any single branch and enabling
 *   democratic adaptationâbut imposes asymmetric costs on actors who
 *   require stable, counter-majoritarian rights protections. The claim/metric
 *   independence is maintained: the constraint is claimed as tangled_rope
 *   because it couples real coordination with identifiable extraction, while
 *   the metrics are authored to describe its actual operation without tuning
 *   toward that claim.
 *
 * KEY AGENTS:
 *   - legislative_branch: Primary beneficiary/agenda-setter (institutional/constrained) â gains autonomy from judicial subordination and participates in constitutional construction.
 *   - executive_branch: Primary beneficiary/agenda-setter (institutional/constrained) â retains independent interpretive authority over executive powers.
 *   - judiciary: Primary target (institutional/constrained) â bears loss of final interpretive authority and faces political resistance to its rulings.
 *   - minority_rights_advocates: Secondary target (moderate/constrained) â bear costs of interpretive instability and lack of a final rights-protective arbiter.
 *   - governing_coalitions: Secondary beneficiary (powerful/mobile) â capture majoritarian flexibility to implement constitutional preferences through political contestation.
 *   - constitutional_scholars: Analytical observer (analytical/analytical) â evaluates the theory-practice gap and normative trade-offs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__coordinate_construction_reading, 0.62).
domain_priors:suppression_score(constitutional_interpretive_authority__coordinate_construction_reading, 0.55).
domain_priors:theater_ratio(constitutional_interpretive_authority__coordinate_construction_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__coordinate_construction_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__coordinate_construction_reading, "Coordinate Construction Reading of Constitutional Interpretive Authority").
narrative_ontology:topic_domain(constitutional_interpretive_authority__coordinate_construction_reading, "constitutional law/political theory/jurisprudence").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__coordinate_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__coordinate_construction_reading, 'c32de3fc-46c0-4a47-908c-2aafd02785c4').
narrative_ontology:cs_kernel_codification('c32de3fc-46c0-4a47-908c-2aafd02785c4', formalized).
narrative_ontology:cs_authority_grounding('c32de3fc-46c0-4a47-908c-2aafd02785c4', distributed).
narrative_ontology:cs_reading_relation('c32de3fc-46c0-4a47-908c-2aafd02785c4', constitutional_interpretive_authority__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('c32de3fc-46c0-4a47-908c-2aafd02785c4', constitutional_interpretive_authority__parliamentary_supremacy_reading, forecloses).
narrative_ontology:cs_axiom('c32de3fc-46c0-4a47-908c-2aafd02785c4', foundational, no_sole_interpreter_principle).
narrative_ontology:cs_axiom_status(no_sole_interpreter_principle, holdable).
narrative_ontology:cs_axiom_grounding('c32de3fc-46c0-4a47-908c-2aafd02785c4', no_sole_interpreter_principle, deontological).
narrative_ontology:cs_axiom('c32de3fc-46c0-4a47-908c-2aafd02785c4', secondary, political_contestation_constitutive).
narrative_ontology:cs_axiom_status(political_contestation_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('c32de3fc-46c0-4a47-908c-2aafd02785c4', political_contestation_constitutive, conventional).
narrative_ontology:cs_reference_frame('c32de3fc-46c0-4a47-908c-2aafd02785c4', interbranch_coordinate_equality).
narrative_ontology:cs_drift_state('c32de3fc-46c0-4a47-908c-2aafd02785c4', contemporary_judicial_supremacy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c32de3fc-46c0-4a47-908c-2aafd02785c4', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__coordinate_construction_reading, legislative_branch).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__coordinate_construction_reading, executive_branch).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__coordinate_construction_reading, governing_coalitions).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__coordinate_construction_reading, judiciary).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__coordinate_construction_reading, minority_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts independent constitutional interpretation of its own powers and limits; uses legislation, appointment authority, and budget control to resist judicial encroachment and advance its constitutional vision; benefits from not being subordinate to a final judicial veto.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, legislative_branch, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__coordinate_construction_reading, legislative_branch, beneficiary).

% Claims autonomous interpretive authority over executive powers and constitutional execution; resists judicial and legislative oversight through political mechanisms, appointments, and administrative action; benefits from coordinate construction by retaining independent constitutional agency.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, executive_branch, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__coordinate_construction_reading, executive_branch, beneficiary).

% Issues constitutional interpretations within its sphere but lacks final authority; faces political resistance, non-compliance, or budgetary pressure from other branches; bears the cost of reduced interpretive finality despite retaining operational independence.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, judiciary, payer,
    institutional, generational, constrained, national).

% Seek stable constitutional protections for minority groups through adjudication; bear costs when interpretive instability allows transient majorities to override rights protections via legislative or executive action; lack a final arbiter to settle constitutional meaning in their favor.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, minority_rights_advocates, payer,
    moderate, generational, constrained, national).

% Transient electoral majorities controlling legislative and executive branches; benefit from dispersed authority by implementing constitutional interpretations through political contestation without permanent judicial obstruction.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, governing_coalitions, beneficiary,
    powerful, biographical, mobile, national).

% Analyze and debate separation of powers and interpretive methodologies; observe gaps between coordinate construction theory and institutional practice; provide normative frameworks that influence but do not control branch behavior.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_interpretive_authority__coordinate_construction_reading, governing_coalitions).
narrative_ontology:fixing_cost_class(constitutional_interpretive_authority__coordinate_construction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Disperses interpretive authority across coordinate branches to prevent concentration of constitutional meaning in a single institution, enabling adaptive governance and self-correcting inter-branch dialogue.
% TRANSFER_FUNCTION: Moves interpretive finality away from any single branch into ongoing political contestation; transfers stability and rights-certainty from minorities and the judiciary to transient governing coalitions that prevail through electoral and appointment politics.
% ABSENT_VOICES: Constitutional scholars and advocates for judicial supremacy who view concentrated judicial review as essential to rights protection; minority communities dependent on counter-majoritarian judicial review; comparative constitutional lawyers from systems with explicit constitutional courts.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, one branch would likely seize final interpretive authorityâeither courts asserting judicial supremacy or the legislature claiming parliamentary sovereigntyâfundamentally reorganizing separation-of-powers dynamics and altering the predictability of rights protections.
% FOUNDING_PROBLEM: How to prevent tyranny of any single branch while maintaining constitutional government; how to reconcile democratic self-governance with constitutional limits without empowering one permanent institution to override all others.
% FOUNDING_PROBLEM_CORROBORATION: Historical constitutional designers and Federalist authors attest to the anti-monarchical, anti-concentration intent from outside the contemporary benefiting branches. Contemporary political scientists and legal historians corroborate the dispersal design. However, constitutional rights advocates and comparative scholars attest that minority protection against majoritarian oppression was an equally central founding concern and may be undermined by dispersed authority.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__coordinate_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__coordinate_construction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__coordinate_construction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_interpretive_authority__coordinate_construction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__coordinate_construction_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__coordinate_construction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_interpretive_authority__coordinate_construction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_interpretive_authority__coordinate_construction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-high because the constraint systematically transfers interpretive finality from minorities and the judiciary to transient political majorities, extracting stability and predictable rights-protection. Suppression (0.55) is moderate: the constraint suppresses alternatives like judicial supremacy and settled constitutional meaning not through direct coercion but through institutional resistance, appointment politics, and budgetary pressure. Theater ratio (0.40) captures the performative aspect of inter-branch 'dialogue'âhearings, signing statements, and symbolic resistance that exceed pure coordination. Accessibility collapse (0.45) reflects that alternatives (a final arbiter) are partially but not fully closed; judicial supremacy remains a live intellectual position. Resistance (0.58) is moderate-high because the judiciary and rights communities actively resist loss of final authority. Temporal measurements share a single grid (0â50) to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The legislative and executive seats experience the constraint as genuine coordination that preserves democratic self-governance and inter-branch equality. The judiciary experiences it as institutional demotionâthe loss of a supremacy that many jurists view as essential to constitutional fidelity. Minority rights advocates experience it as exposure to majoritarian override, where the absence of a final interpreter leaves rights contingent on political fortune. The engine computes this divergence from the structural data: beneficiary institutions with constrained exit sit nearer the beneficiary end than victim groups with identical exit because beneficiary declarations modulate directionality downward, while victim declarations push it upward.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations for legislative_branch, executive_branch, and governing_coalitions derive low directionality (d toward 0.0), reducing effective extraction for these seats. Victim declarations for judiciary and minority_rights_advocates derive high directionality (d toward 1.0), amplifying effective extraction. The governing_coalitions seat has mobile exit (electoral turnover), which would normally lower d, but the beneficiary declaration dominates because the seat structurally captures majoritarian gains. The judiciary has constrained exit and victim status, placing it near the full-target end. Constitutional_scholars sit at the analytical pole with neutral directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling in both directions. Without the victim structureâif only the inter-branch coordination were visibleâthe constraint might compute as rope. But the presence of identifiable victims (minorities losing stable rights protections, judiciary losing finality) and asymmetric extraction blocks that misclassification. Conversely, without the genuine coordination functionâif one focused only on rights instabilityâthe constraint might appear as snare. The active enforcement requirement and the real tyranny-prevention problem (dispersing authority to prevent single-branch dominance) anchor the coordination component, producing the tangled_rope hybrid. If the coordination function atrophied entirely and only majoritarian extraction remained, the constraint would drift toward snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordinate_collapse_to_majoritarianism,
    'Does coordinate construction in practice collapse into de facto parliamentary or executive supremacy because legislative majorities control appointments and budgets?',
    'Empirical analysis of inter-branch conflicts in coordinate-construction regimes, measuring whether legislative or executive power consistently dominates judicial interpretation over time.',
    'If legislative dominance prevails, the constraint is functionally majoritarian supremacy rather than genuine coordination, raising extractiveness and shifting classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordinate_collapse_to_majoritarianism, empirical, 'Whether dispersed authority collapses into de facto single-branch dominance').

omega_variable(
    instability_asymmetric_costs,
    'Does the interpretive instability produced by coordinate construction impose asymmetric costs on politically disadvantaged groups?',
    'Comparative rights-protection metrics across jurisdictions with coordinate construction versus judicial supremacy, tracking minority rights outcomes and enforcement variance over time.',
    'If instability systematically disadvantages minorities, the victim structure is deeper than the coordination function suggests, supporting higher extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instability_asymmetric_costs, empirical, 'Whether interpretive instability asymmetrically burdens minorities').

omega_variable(
    kernel_reading_sibling_foreclosure,
    'Is the logical foreclosure of judicial and parliamentary supremacy by coordinate construction maintained in practice, or do hybrid institutional positions exist that blend readings?',
    'Jurisprudential analysis of institutional rhetoric and practice to identify whether any branch simultaneously claims coordinate equality and final authority over specific domains.',
    'If hybrid positions are coherent and common, the forecloses relations to siblings may need reclassification as influences or coexists_with, altering the kernel''s network topology.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_sibling_foreclosure, conceptual, 'Whether hybrid readings blur the foreclosure relations between kernel siblings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__coordinate_construction_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coordinate_construction_tr_t0, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(coordinate_construction_tr_t10, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(coordinate_construction_tr_t20, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(coordinate_construction_tr_t30, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement(coordinate_construction_tr_t40, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 40, 0.36).
narrative_ontology:measurement(coordinate_construction_tr_t50, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(coordinate_construction_be_t0, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(coordinate_construction_be_t10, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(coordinate_construction_be_t20, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(coordinate_construction_be_t30, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 30, 0.54).
narrative_ontology:measurement(coordinate_construction_be_t40, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(coordinate_construction_be_t50, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(coordinate_construction_su_t0, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(coordinate_construction_su_t10, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement(coordinate_construction_su_t20, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(coordinate_construction_su_t30, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(coordinate_construction_su_t40, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(coordinate_construction_su_t50, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_interpretive_authority__parliamentary_supremacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the constitutional_interpretive_authority kernel, decomposed from the colloquial label 'constitutional interpretive authority' into three structurally distinct claims: coordinate construction (dispersed authority), judicial supremacy (court-final), and parliamentary supremacy (legislature-final). Each reading has a distinct beneficiary/victim structure, epsilon, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
