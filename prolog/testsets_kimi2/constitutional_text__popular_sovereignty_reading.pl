% ============================================================================
% CONSTRAINT STORY: constitutional_text__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__popular_sovereignty_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: constitutional_text__popular_sovereignty_reading
 *   human_readable: Constitutional Text: Popular Sovereignty Reading
 *   domain: constitutional_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the popular sovereignty reading of the
 *   constitutional text kernel. Under this reading, constitutional authority
 *   derives not from institutional fiat but from the constituent power of the
 *   demos. Courts and legislatures are interpretive agents only; final
 *   authority resides with the people, exercisable through amendment,
 *   convention, or revolutionary action. The constraint coordinates
 *   democratic legitimacy by tying constitutional law to popular consent, but
 *   it asymmetrically extracts institutional stability and legal expertise by
 *   denying finality to judicial and parliamentary interpretation.
 *
 * KEY AGENTS:
 *   - democratic_citizenry: Primary beneficiary (organized/constrained) â receives democratic legitimacy and retains ultimate constituent authority
 *   - judicial_branch: Primary target (institutional/constrained) â bears loss of interpretive finality and institutional certainty
 *   - legislative_branch: Primary target (institutional/constrained) â bears loss of parliamentary supremacy
 *   - constitutional_experts: Secondary target (moderate/constrained) â bears devaluation of technical interpretive expertise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__popular_sovereignty_reading, 0.55).
domain_priors:suppression_score(constitutional_text__popular_sovereignty_reading, 0.5).
domain_priors:theater_ratio(constitutional_text__popular_sovereignty_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__popular_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__popular_sovereignty_reading, "Constitutional Text: Popular Sovereignty Reading").
narrative_ontology:topic_domain(constitutional_text__popular_sovereignty_reading, "constitutional_theory").

domain_priors:requires_active_enforcement(constitutional_text__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__popular_sovereignty_reading, 'd6ad2b94-d85b-402d-a4a2-45131e3289c8').
narrative_ontology:cs_kernel_codification('d6ad2b94-d85b-402d-a4a2-45131e3289c8', fixed_text).
narrative_ontology:cs_authority_grounding('d6ad2b94-d85b-402d-a4a2-45131e3289c8', distributed).
narrative_ontology:cs_reading_relation('d6ad2b94-d85b-402d-a4a2-45131e3289c8', constitutional_text__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('d6ad2b94-d85b-402d-a4a2-45131e3289c8', constitutional_text__legislative_sovereignty_reading, forecloses).
narrative_ontology:cs_axiom('d6ad2b94-d85b-402d-a4a2-45131e3289c8', foundational, demos_retains_constituent_power).
narrative_ontology:cs_axiom_status(demos_retains_constituent_power, holdable).
narrative_ontology:cs_axiom_grounding('d6ad2b94-d85b-402d-a4a2-45131e3289c8', demos_retains_constituent_power, deontological).
narrative_ontology:cs_axiom('d6ad2b94-d85b-402d-a4a2-45131e3289c8', foundational, institutional_subordination_to_popular_will).
narrative_ontology:cs_axiom_status(institutional_subordination_to_popular_will, holdable).
narrative_ontology:cs_axiom_grounding('d6ad2b94-d85b-402d-a4a2-45131e3289c8', institutional_subordination_to_popular_will, deontological).
narrative_ontology:cs_reference_frame('d6ad2b94-d85b-402d-a4a2-45131e3289c8', popular_constituent_authority).
narrative_ontology:cs_drift_state('d6ad2b94-d85b-402d-a4a2-45131e3289c8', contemporary_institutional_consolidation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d6ad2b94-d85b-402d-a4a2-45131e3289c8', '').
narrative_ontology:cs_kernel_id(constitutional_text__popular_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, democratic_citizenry).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, judicial_branch).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, legislative_branch).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, constitutional_experts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The diffuse body of citizens whose collective will is claimed as the ultimate source of constitutional authority. They retain theoretical capacity to amend, convene, or revoke constitutional arrangements. Their exit from the constitutional order is difficult (emigration, renunciation), but their voice is the legitimating ground of the text.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, democratic_citizenry, beneficiary,
    organized, generational, constrained, national).

% Courts interpret constitutional text but are denied final interpretive authority under this reading. Their decisions remain subordinate to popular amendment or constitutional convention. This subordination erodes the certainty and finality that underpin judicial institutional power.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, judicial_branch, payer,
    institutional, biographical, constrained, national).

% Parliament enacts ordinary law and participates in constitutional politics, yet is explicitly denied supremacy. Its claim to represent the people is treated as derivative and contingent rather than ultimate, limiting its autonomy in constitutional design.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, legislative_branch, payer,
    institutional, biographical, constrained, national).

% Legal academics, drafters, and interpretive professionals whose authority depends on stable, expertise-mediated constitutional meaning. When constitutional significance is tied to unpredictable popular mobilization, the value of technical interpretive skill is destabilized.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, constitutional_experts, payer,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Grounds constitutional legitimacy in ongoing democratic consent rather than institutional fiat, ensuring that fundamental law remains traceable to the governed and providing a mechanism for systemic renewal when institutional paths ossify.
% TRANSFER_FUNCTION: Moves interpretive finality and constitutional supremacy from courts and legislatures to extra-institutional popular expressionâamendment, convention, or revolutionary actionâtransferring institutional stability into democratic liquidity.
% ABSENT_VOICES: Sitting judges, legislative leaders, and legal professionals committed to judicial or parliamentary supremacy are formally present in discourse but structurally overridden; their objections are recorded as resistance rather than as authoritativeå¦å³.
% DISAPPEARANCE_RATIONALE: If the popular sovereignty principle vanished, courts and legislatures would immediately claim the vacant authority; institutionalists would celebrate restored stability, while democratic theorists would mourn the loss of constituent accountability.
% FOUNDING_PROBLEM: The threat of tyranny by unaccountable institutionsâensuring that constitutional law does not become a self-perpetuating elite interpretive project detached from the will of the governed.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and comparative democratic theorists outside the benefiting citizenry attest that institutional capture remains a live threat in many jurisdictions; sitting judges and legislators (the paying parties) dispute this, asserting that electoral accountability already suffices, but independent comparative research corroborates the live-problem reading in captured and transitional democracies.
narrative_ontology:disappearance_verdict(constitutional_text__popular_sovereignty_reading, contested).
narrative_ontology:founding_problem_status(constitutional_text__popular_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__popular_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_text__popular_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__popular_sovereignty_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__popular_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text__popular_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) is moderate-to-high because institutional stability and interpretive finality are genuinely depleted by subordinating all authority to popular expression. Suppression (0.50) is moderate because the constraint must actively suppress judicial and legislative supremacy claims to maintain its authority structure. Theater ratio (0.30) is moderate-low because popular sovereignty represents a genuine normative commitment rather than mere performance, though populist invocations introduce theatrical elements. Accessibility collapse (0.65) is moderate: alternatives such as judicial supremacy remain visible but are normatively delegitimized within this reading. Resistance (0.70) is high because institutional actors actively resist the loss of finality.
 *
 * PERSPECTIVAL GAP:
 *   From the democratic-citizenry seat, the constraint is legitimate self-rule that prevents institutional tyranny; from the judicial and legislative seats, it is a destabilizing denial of interpretive finality that invites constitutional uncertainty. The engine computes this divergence from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The democratic citizenry is the declared beneficiary (low directionality), receiving democratic legitimacy and retaining constituent authority. The judicial and legislative branches are declared payers (high directionality), bearing the loss of supremacy and finality. Constitutional experts are secondary payers (moderate-to-high directionality) through the devaluation of their expertise. Directionality is structurally derived from beneficiary and victim declarations without override.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resists mandatrophy mislabeling because it openly admits both a coordination function (democratic legitimacy, constituent accountability) and an extraction function (institutional destabilization, expertise devaluation). If the coordination function were absent, the constraint would be a snare destabilizing institutions for no democratic gain; if the extraction were absent, it would be a rope of pure democratic legitimacy. The Tangled Rope classification captures the hybrid accurately.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constituent_power_exercise,
    'Is constituent power structurally exercisable by the demos, or is it a legitimating fiction that masks factional or elite capture of popular rhetoric?',
    'Historical case studies of successful constitutional conventions or amendments driven by genuine popular mobilization versus elite-orchestrated campaigns.',
    'If capture is typical, the beneficiary is not the demos but mobilizing elites, shifting classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constituent_power_exercise, empirical, 'Whether popular sovereignty is genuinely exercisable or captured by factions').

omega_variable(
    institutional_stability_tradeoff,
    'Does the subordination of courts and legislatures to popular sovereignty produce net democratic coordination, or does the resulting instability undermine the constitutional order it claims to legitimize?',
    'Comparative constitutional stability metrics in jurisdictions with strong popular sovereignty traditions versus strong institutional supremacy.',
    'If instability dominates, the coordination function is weaker than claimed, raising effective extraction and potentially shifting classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_stability_tradeoff, empirical, 'Whether institutional subordination yields net coordination or net destabilization').

omega_variable(
    foreclosure_boundary,
    'Does the popular sovereignty reading logically foreclose institutional supremacy readings, or can both be held as live options in a layered sovereignty framework?',
    'Conceptual analysis of whether ''ultimate'' popular authority is compatible with ''final'' institutional authority in a single constitutional framework.',
    'If coexistence is possible, reading_relations should be coexists_with rather than forecloses, altering CS pattern classification and network coupling.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(foreclosure_boundary, conceptual, 'Under-determination of foreclosure versus coexistence with sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__popular_sovereignty_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__popular_sovereignty_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cons_tr_t20, constitutional_text__popular_sovereignty_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(cons_tr_t40, constitutional_text__popular_sovereignty_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(cons_tr_t60, constitutional_text__popular_sovereignty_reading, theater_ratio, 60, 0.24).
narrative_ontology:measurement(cons_tr_t80, constitutional_text__popular_sovereignty_reading, theater_ratio, 80, 0.27).
narrative_ontology:measurement(cons_tr_t100, constitutional_text__popular_sovereignty_reading, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__popular_sovereignty_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(cons_be_t20, constitutional_text__popular_sovereignty_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(cons_be_t40, constitutional_text__popular_sovereignty_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(cons_be_t60, constitutional_text__popular_sovereignty_reading, base_extractiveness, 60, 0.46).
narrative_ontology:measurement(cons_be_t80, constitutional_text__popular_sovereignty_reading, base_extractiveness, 80, 0.51).
narrative_ontology:measurement(cons_be_t100, constitutional_text__popular_sovereignty_reading, base_extractiveness, 100, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__popular_sovereignty_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(cons_su_t20, constitutional_text__popular_sovereignty_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(cons_su_t40, constitutional_text__popular_sovereignty_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(cons_su_t60, constitutional_text__popular_sovereignty_reading, suppression_requirement, 60, 0.58).
narrative_ontology:measurement(cons_su_t80, constitutional_text__popular_sovereignty_reading, suppression_requirement, 80, 0.64).
narrative_ontology:measurement(cons_su_t100, constitutional_text__popular_sovereignty_reading, suppression_requirement, 100, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, legislative_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the constitutional_text kernel, decomposed per the epsilon-invariance principle. The judicial_supremacy_reading and legislative_sovereignty_reading are structurally distinct siblings with different epsilon values, stakeholder configurations, and axiom sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
