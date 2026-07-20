% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__parliamentary_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__parliamentary_constraint_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: fifth_republic_constitution__parliamentary_constraint_reading
 *   human_readable: Fifth Republic Parliamentary Constraint on Executive Power
 *   domain: constitutional_law/political_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the parliamentary_constraint_reading
 *   of the French Fifth Republic constitution kernel. Under this reading, the
 *   President and executive branch function as a coordinated executive
 *   requiring legislative authorization for policy implementation. The
 *   legislative majority is the primary beneficiary of this constraint,
 *   gaining constitutional leverage over the executive through the confidence
 *   procedure and legislative domain. The executive branch enters the victim
 *   set when the Assembly withholds confidence or blocks legislation. This
 *   reading competes with the hyper_presidential_reading (executive as direct
 *   sovereign) and the cohabitation_equilibrium_reading (dual executive
 *   requiring negotiation). The constraint is authored as a low-extraction
 *   tangled rope: it carries a genuine democratic coordination function while
 *   asymmetrically extracting autonomy from the executive in favor of the
 *   legislative majority.
 *
 * KEY AGENTS:
 *   - legislative_majority (institutional/beneficiary) â captures leverage through confidence and authorization requirements
 *   - executive_branch (institutional/target) â bears the cost of reduced unilateral autonomy
 *   - constitutional_council (institutional/analytical) â interprets and enforces the executive-legislative boundary
 *   - french_public (organized/beneficiary) â receives democratic constraint benefits diffusely
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__parliamentary_constraint_reading, 0.3).
domain_priors:suppression_score(fifth_republic_constitution__parliamentary_constraint_reading, 0.45).
domain_priors:theater_ratio(fifth_republic_constitution__parliamentary_constraint_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__parliamentary_constraint_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__parliamentary_constraint_reading, "Fifth Republic Parliamentary Constraint on Executive Power").
narrative_ontology:topic_domain(fifth_republic_constitution__parliamentary_constraint_reading, "constitutional_law/political_systems").

domain_priors:requires_active_enforcement(fifth_republic_constitution__parliamentary_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__parliamentary_constraint_reading, '9a189ee5-ad0a-424a-96c2-9b318d94d80b').
narrative_ontology:cs_kernel_codification('9a189ee5-ad0a-424a-96c2-9b318d94d80b', formalized).
narrative_ontology:cs_authority_grounding('9a189ee5-ad0a-424a-96c2-9b318d94d80b', lineage).
narrative_ontology:cs_interpretation_layer_present('9a189ee5-ad0a-424a-96c2-9b318d94d80b').
narrative_ontology:cs_reading_relation('9a189ee5-ad0a-424a-96c2-9b318d94d80b', fifth_republic_constitution__hyper_presidential_reading, coexists_with).
narrative_ontology:cs_reading_relation('9a189ee5-ad0a-424a-96c2-9b318d94d80b', fifth_republic_constitution__cohabitation_equilibrium_reading, influences).
narrative_ontology:cs_axiom('9a189ee5-ad0a-424a-96c2-9b318d94d80b', foundational, executive_subordinate_to_legislative_confidence).
narrative_ontology:cs_axiom_status(executive_subordinate_to_legislative_confidence, holdable).
narrative_ontology:cs_axiom_grounding('9a189ee5-ad0a-424a-96c2-9b318d94d80b', executive_subordinate_to_legislative_confidence, conventional).
narrative_ontology:cs_axiom('9a189ee5-ad0a-424a-96c2-9b318d94d80b', foundational, legislative_majority_as_policy_gatekeeper).
narrative_ontology:cs_axiom_status(legislative_majority_as_policy_gatekeeper, holdable).
narrative_ontology:cs_axiom_grounding('9a189ee5-ad0a-424a-96c2-9b318d94d80b', legislative_majority_as_policy_gatekeeper, conventional).
narrative_ontology:cs_reference_frame('9a189ee5-ad0a-424a-96c2-9b318d94d80b', legislative_authorization_framework).
narrative_ontology:cs_drift_state('9a189ee5-ad0a-424a-96c2-9b318d94d80b', contemporary_presidential_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9a189ee5-ad0a-424a-96c2-9b318d94d80b', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, legislative_majority).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, french_public).
narrative_ontology:constraint_victim(fifth_republic_constitution__parliamentary_constraint_reading, executive_branch).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the National Assembly and thereby the confidence procedure. Can force the government to resign or block legislation proposed by the executive. Benefits from the constitutional requirement that the executive must secure assembly approval for major policy and budget.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, legislative_majority, beneficiary,
    institutional, biographical, constrained, national).

% The President and Government must secure legislative authorization for laws and budget. During cohabitation or when lacking majority, the executive is blocked and must negotiate or concede. Bears the cost of reduced unilateral autonomy; exit from the constraint effectively requires exit from office.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, executive_branch, payer,
    institutional, biographical, identity_locked, national).

% Reviews legislation and executive actions for constitutionality. Interprets the boundary between executive decree authority and legislative domain. Its decisions determine whether the parliamentary constraint is enforceable.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, constitutional_council, observer,
    institutional, generational, analytical, national).

% Elects both the President and the legislative majority. Benefits from the institutional check when it prevents executive overreach, but does not directly control the constraint's operation.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, french_public, beneficiary,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fifth_republic_constitution__parliamentary_constraint_reading, legislative_majority).
narrative_ontology:fixing_cost_class(fifth_republic_constitution__parliamentary_constraint_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the exercise of executive power with democratic legislative authorization, ensuring that major policy changes require the backing of an elected legislative majority.
% TRANSFER_FUNCTION: Moves policy initiative and implementation autonomy from the executive branch to the legislative majority; the executive must trade concessions or confidence for legislative passage.
% ABSENT_VOICES: Political minorities in the legislature whose policy preferences are overridden when the majority blocks executive initiatives; executives and parties who would prefer a hyper-presidential reading and find their unilateral options foreclosed.
% DISAPPEARANCE_RATIONALE: If the requirement for legislative authorization disappeared, the executive would govern by decree on domestic policy, the assembly would lose its primary leverage, and the French political system would shift toward a hyper-presidential model.
% FOUNDING_PROBLEM: To prevent executive unilateralism and ensure that the government of the Republic remains accountable to the elected legislative assembly, following concerns about both Fourth Republic paralysis and authoritarian drift.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians acknowledge the 1958 constitution was drafted to cure Fourth Republic instability, but jurists and comparative constitutional scholars outside the benefiting legislative majority attest that the text encodes significant legislative checks, supporting a mixed reading. De Gaulle's own camp emphasized strong executive leadership, while constitutional jurists emphasized Article 20 and Article 49 as creating reciprocal constraint.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__parliamentary_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__parliamentary_constraint_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__parliamentary_constraint_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fifth_republic_constitution__parliamentary_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__parliamentary_constraint_reading, 0.3, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__parliamentary_constraint_reading_tests).
:- end_tests(fifth_republic_constitution__parliamentary_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is set low (0.30) per the structural delta: the executive loses autonomy but is not subject to resource extraction. The constraint is tangled rope because it requires active enforcement (Constitutional Council review, assembly confidence votes) and exhibits asymmetric extraction (legislative majority benefits, executive pays). Suppression (0.45) reflects the constitutional and political barriers to executive bypass. Theater ratio (0.25) is modest: the constraint is functional during cohabitation but becomes performative during unified majorities when the executive effectively ignores legislative constraint. Accessibility collapse (0.60) is moderate: alternatives like hyper-presidential decree authority exist in the text but are partially collapsed by jurisprudence. Resistance (0.50) reflects the executive's persistent efforts to expand decree authority and avoid confidence votes.
 *
 * PERSPECTIVAL GAP:
 *   The legislative majority seat should compute as beneficiary (low directionality, low effective extraction) because the constraint subsidizes its bargaining position. The executive branch seat should compute as target (high directionality, amplified effective extraction) because the constraint specifically limits its autonomy. The french_public sits near symmetric: they benefit from democratic accountability but do not capture the extracted leverage. The divergence between executive and legislative seats is the core measurement of this constraint's asymmetric structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (legislative_majority, french_public) derive directionality toward the beneficiary end for those seats. Victim declaration (executive_branch) derives directionality toward the target end. The executive branch is institutional in global power but is specifically identity_locked in this constraint because the President's political authority is fused with the constitutional office; exit from the constraint means exit from the role itself. The legislative majority is constrained by electoral and constitutional limits but is not identity_locked to the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the parliamentary constraint as pure extraction (snare) by preserving its genuine coordination function: democratic accountability and prevention of unilateral executive rule. Conversely, it prevents mislabeling it as pure coordination (rope) by acknowledging the asymmetric cost borne by the executive and the active enforcement required. The low extractiveness metric honestly reflects that the cost is autonomy loss rather than resource transfer, but the structural asymmetry is real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_text_ambiguity,
    'Does the constitutional text of the Fifth Republic inherently structure executive subordination to legislative confidence, or does it enable hyper-presidential practice that renders the parliamentary constraint ornamental?',
    'Comparative analysis of constitutional council decisions across cohabitation and unified majority periods, tracking whether legislative authorization requirements are consistently enforced or selectively applied.',
    'If the text is genuinely parliamentary, the constraint is a low-extraction tangled rope; if ornamental, the constraint computes as a piton or theater-heavy scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_text_ambiguity, conceptual, 'Ambiguity in the constitutional kernel between presidential and parliamentary readings').

omega_variable(
    cohabitation_cycle_effect,
    'Does the constraint''s extractiveness vary inherently with electoral cycles, or is the constraint stable while political context modulates its enforcement?',
    'Cross-temporal measurement of executive autonomy metrics during cohabitation vs. unified majority governments.',
    'If cyclical, the constraint''s base_extractiveness should be measured at enforcement peaks and troughs separately; if stable, the political cycle is external noise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cohabitation_cycle_effect, empirical, 'Whether cyclical political context modulates constraint enforcement').

omega_variable(
    kernel_reading_contestation,
    'Is this constraint a genuine feature of the constitutional text, or a retrospective reading that overstates parliamentary constraint relative to the hyper-presidential architecture?',
    'Historical analysis of the 1958 constitutional debates and comparative analysis with the sibling hyper_presidential_reading''s textual evidence.',
    'If the parliamentary reading is a retrospective construct, the constraint''s epsilon is lower than authored and the classification may shift toward piton or rope; if textually grounded, the tangled rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Uncertainty about whether the parliamentary constraint is kernel-inherent or reading-imposed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__parliamentary_constraint_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t0, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fift_tr_t10, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(fift_tr_t20, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(fift_tr_t30, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(fift_tr_t40, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(fift_tr_t50, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 50, 0.25).
narrative_ontology:measurement(fift_tr_t60, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 60, 0.25).

% Extraction over time
narrative_ontology:measurement(fift_be_t0, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(fift_be_t10, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 10, 0.25).
narrative_ontology:measurement(fift_be_t20, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 20, 0.32).
narrative_ontology:measurement(fift_be_t30, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 30, 0.28).
narrative_ontology:measurement(fift_be_t40, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 40, 0.26).
narrative_ontology:measurement(fift_be_t50, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 50, 0.28).
narrative_ontology:measurement(fift_be_t60, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 60, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t0, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(fift_su_t10, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(fift_su_t20, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(fift_su_t30, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(fift_su_t40, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(fift_su_t50, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 50, 0.42).
narrative_ontology:measurement(fift_su_t60, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 60, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__parliamentary_constraint_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, hyper_presidential_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, cohabitation_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the fifth_republic_constitution kernel, decomposed from the hyper_presidential and cohabitation_equilibrium readings per the epsilon-invariance principle. Each reading instantiates a structurally distinct constraint with different beneficiary/victim structures and epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
