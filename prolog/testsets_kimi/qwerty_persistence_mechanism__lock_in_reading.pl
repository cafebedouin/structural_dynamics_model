% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__lock_in_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__lock_in_reading, []).

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
 *   constraint_id: qwerty_persistence_mechanism__lock_in_reading
 *   human_readable: QWERTY Persistence via Path-Dependent Lock-In
 *   domain: economic_history/technology_studies/path_dependence
 *
 * SUMMARY:
 *   This constraint instantiates the lock_in_reading of the
 *   qwerty_persistence_mechanism kernel. It models the QWERTY keyboard layout
 *   as a degraded coordination standard that persists through path-dependent
 *   inertia rather than active extraction or current technical merit. The
 *   layout solved a genuine typewriter-engineering problem in the 1880s, but
 *   the problem died with mechanical typing. What remains is institutional
 *   inertia: platform defaults, educational curricula, and hardware form
 *   factors that re-enact a obsolete standard. The reading asserts no
 *   concentrated beneficiary captures rent from this persistence; the costs
 *   are diffuse productivity and ergonomic losses borne by typists
 *   collectively. Sibling readings differ structurally:
 *   naturalization_reading claims QWERTY is genuinely adequate, and
 *   beneficiary_extraction_reading identifies incumbents who actively
 *   maintain it to protect sunk investments.
 *
 * KEY AGENTS:
 *   - platform_vendors: Agenda setter (organized/constrained) â they control default software layouts and could change them but do not because of user-retraining risk.
 *   - general_typists: Primary payer (powerless/constrained) â they bear the diffuse ergonomic and speed cost of the suboptimal standard.
 *   - alternative_layout_advocates: Excluded voice (moderate/trapped) â they promote superior layouts but are locked out by network effects.
 *   - economic_historians: Analytical observer (analytical/analytical) â they document the path dependence without being bound by the constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__lock_in_reading, 0.45).
domain_priors:suppression_score(qwerty_persistence_mechanism__lock_in_reading, 0.35).
domain_priors:theater_ratio(qwerty_persistence_mechanism__lock_in_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__lock_in_reading, piton).
narrative_ontology:human_readable(qwerty_persistence_mechanism__lock_in_reading, "QWERTY Persistence via Path-Dependent Lock-In").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__lock_in_reading, "economic_history/technology_studies/path_dependence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__lock_in_reading, '05db1a99-9ea6-4255-a1cf-e31106539e05').
narrative_ontology:cs_kernel_codification('05db1a99-9ea6-4255-a1cf-e31106539e05', fixed_text).
narrative_ontology:cs_authority_grounding('05db1a99-9ea6-4255-a1cf-e31106539e05', practice).
narrative_ontology:cs_reading_relation('05db1a99-9ea6-4255-a1cf-e31106539e05', qwerty_persistence_mechanism__naturalization_reading, coexists_with).
narrative_ontology:cs_reading_relation('05db1a99-9ea6-4255-a1cf-e31106539e05', qwerty_persistence_mechanism__beneficiary_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('05db1a99-9ea6-4255-a1cf-e31106539e05', foundational, path_dependent_inefficiency_generates_reform_pressure).
narrative_ontology:cs_axiom_status(path_dependent_inefficiency_generates_reform_pressure, holdable).
narrative_ontology:cs_axiom_grounding('05db1a99-9ea6-4255-a1cf-e31106539e05', path_dependent_inefficiency_generates_reform_pressure, instrumental).
narrative_ontology:cs_axiom('05db1a99-9ea6-4255-a1cf-e31106539e05', foundational, diffuse_suboptimality_without_extraction_lacks_legitimacy).
narrative_ontology:cs_axiom_status(diffuse_suboptimality_without_extraction_lacks_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('05db1a99-9ea6-4255-a1cf-e31106539e05', diffuse_suboptimality_without_extraction_lacks_legitimacy, deontological).
narrative_ontology:cs_reference_frame('05db1a99-9ea6-4255-a1cf-e31106539e05', path_dependent_standardization_equilibrium).
narrative_ontology:cs_drift_state('05db1a99-9ea6-4255-a1cf-e31106539e05', contemporary_digital_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('05db1a99-9ea6-4255-a1cf-e31106539e05', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, general_typists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the default keyboard layout in operating systems and firmware. They could ship alternative layouts but do not because user retraining risk and support costs outweigh the efficiency gains. They do not capture surplus from QWERTY specifically; keyboard layout is incidental to their core business.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, platform_vendors, agenda_setter,
    organized, generational, constrained, global).

% Learn QWERTY as the default input method and bear the diffuse cost of lower typing speed and inferior ergonomics compared to alternative layouts. Individual switching is possible but costly due to relearning time and loss of interoperability with shared devices and workplace norms.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, general_typists, payer,
    powerless, biographical, constrained, global).

% Promote optimized keyboard layouts such as Dvorak or Colemak on ergonomic and efficiency grounds. They are structurally excluded from mainstream adoption because network effects and educational lock-in prevent collective switching, not because their arguments are refuted.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, alternative_layout_advocates, excluded,
    moderate, biographical, trapped, global).

% Analyze the QWERTY case as a canonical example of path dependence and coordination failure. They observe the divergence between private switching costs and social optimality without being bound by the constraint.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, economic_historians, observer,
    analytical, civilizational, analytical, global).

narrative_ontology:fixing_cost_class(qwerty_persistence_mechanism__lock_in_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single compatible keyboard input standard across all devices and users, eliminating the need to relearn layout when switching machines or sharing equipment.
% TRANSFER_FUNCTION: Imposes a diffuse opportunity cost on typists in the form of foregone typing speed and ergonomic efficiency; no concentrated agent receives the transfer.
% ABSENT_VOICES: Alternative keyboard layout designers and ergonomic reformers are absent from standard-setting; their exclusion is structural (no viable market entry due to network effects) rather than active suppression.
% DISAPPEARANCE_RATIONALE: If QWERTY vanished overnight, hardware defaults, software keyboards, and typing pedagogy would rapidly restandardize; the temporary unlock of collective action would permit migration to more efficient layouts before a new equilibrium locked in.
% FOUNDING_PROBLEM: Preventing mechanical typebar jams by spatially separating frequently paired letters on early typewriters.
% FOUNDING_PROBLEM_CORROBORATION: Technology historians and mechanical engineers attest that the jamming problem was real for late-19th-century typewriters but was solved by other mechanical improvements and is irrelevant to electronic keyboards; these sources are outside the set of actors with a stake in maintaining the layout.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__lock_in_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__lock_in_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__lock_in_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence_mechanism__lock_in_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__lock_in_reading, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__lock_in_reading_tests).
:- end_tests(qwerty_persistence_mechanism__lock_in_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.45 because the aggregate productivity loss from using a suboptimal layout across billions of daily typing hours is real, even though no agent captures it. Suppression is moderate (0.35): alternatives are not banned but are structurally marginalized by educational lock-in and hardware defaults. Theater ratio is high (0.70) because the original typewriter-jam rationale is dead, and contemporary maintenance of QWERTY is almost entirely performative (typing classes, 'standard' keyboard molds, cultural norm). Accessibility collapse is 0.65: once a user invests in QWERTY muscle memory and acquires QWERTY hardware, switching layouts imposes high relearning and interoperability costs. Resistance is low (0.20) because the costs are diffuse and individually too small to provoke organized opposition. The measurement grid uses a single shared timeline (1880â2020) to prevent misaligned substitution.
 *
 * PERSPECTIVAL GAP:
 *   Typists experience the constraint as a biographical sunk cost and minor daily friction; they do not experience it as extraction because no one is taking anything from them. Platform vendors experience no cost and only downside risk from deviating. Economic historians see the full social-cost divergence that individual agents cannot perceive. The engine should compute very different directionalities for the payer seat (high d, diffuse but real extraction) and the agenda-setter seat (low d, constrained but not benefiting).
 *
 * DIRECTIONALITY LOGIC:
 *   General typists are the structural targets: they pay through foregone productivity and ergonomics, and their exit is constrained by network effects and relearning costs (d near target end). Platform vendors are near symmetric to slightly beneficiary: they do not profit from QWERTY but incur risk from switching, giving them a mild structural subsidy in staying put (d near middle). Alternative layout advocates are excluded and trapped, experiencing the constraint as pure external imposition. No directionality overrides are needed because the structural derivation matches the actual relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpreventing typebar jamsâwas solved by technology change long ago. The constraint persists not because it solves a live problem but because the cost of collective switching exceeds private benefits. This prevents misclassification as a Rope (the coordination function is atrophied) and as a Snare (there is no active beneficiary defending the arrangement for rent). The mandatrophy is resolved: the problem is dead, the arrangement is inertial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lock_in_vs_beneficiary_extraction,
    'Does QWERTY persistence stem purely from decentralized path dependence, or do concentrated beneficiaries (manufacturers, training institutions) actively suppress alternatives?',
    'Trace the funding and governance of keyboard standardization bodies; examine whether alternative-layout keyboard production is actively discouraged by incumbent firms beyond ordinary market risk.',
    'If active suppression by beneficiaries is found, this reading merges into the beneficiary_extraction reading and the constraint reclassifies as tangled_rope or snare; if purely decentralized inertia, the piton classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lock_in_vs_beneficiary_extraction, conceptual, 'Whether the lock-in reading''s no-extraction claim survives against the beneficiary extraction sibling').

omega_variable(
    adequacy_vs_suboptimality,
    'Is QWERTY genuinely adequate for modern typing tasks, rendering the ''suboptimality'' claim moot?',
    'Meta-analysis of ergonomic studies comparing QWERTY to alternative layouts on speed, accuracy, and injury metrics.',
    'If QWERTY is adequate, the naturalization_reading is supported and the constraint is closer to a benign rope; if substantially inferior, the lock-in reading is corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adequacy_vs_suboptimality, empirical, 'Whether QWERTY''s technical adequacy falsifies the suboptimality premise').

omega_variable(
    collective_action_mechanism,
    'Could a centralized or platform-coordinated switch to a superior layout overcome the path dependence, and if so, what mechanism would unlock the collective action?',
    'Natural experiments from jurisdictions or platforms that have attempted layout migration (e.g., software-keyboard dual-layout trials).',
    'If collective switching is demonstrably unlockable, the constraint is a transient coordination failure rather than a deep piton; if switching remains stuck even with coordination, the inertial classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collective_action_mechanism, empirical, 'Whether the lock-in is technically reversible through coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__lock_in_reading, 0, 140).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwerty_lockin_tr_t0, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(qwerty_lockin_tr_t20, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(qwerty_lockin_tr_t50, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 50, 0.45).
narrative_ontology:measurement(qwerty_lockin_tr_t80, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 80, 0.6).
narrative_ontology:measurement(qwerty_lockin_tr_t110, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 110, 0.7).
narrative_ontology:measurement(qwerty_lockin_tr_t140, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 140, 0.75).

% Extraction over time
narrative_ontology:measurement(qwerty_lockin_be_t0, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(qwerty_lockin_be_t20, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 20, 0.2).
narrative_ontology:measurement(qwerty_lockin_be_t50, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 50, 0.3).
narrative_ontology:measurement(qwerty_lockin_be_t80, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 80, 0.38).
narrative_ontology:measurement(qwerty_lockin_be_t110, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 110, 0.42).
narrative_ontology:measurement(qwerty_lockin_be_t140, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 140, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(qwerty_lockin_su_t0, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(qwerty_lockin_su_t20, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 20, 0.25).
narrative_ontology:measurement(qwerty_lockin_su_t50, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 50, 0.4).
narrative_ontology:measurement(qwerty_lockin_su_t80, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 80, 0.45).
narrative_ontology:measurement(qwerty_lockin_su_t110, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 110, 0.4).
narrative_ontology:measurement(qwerty_lockin_su_t140, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 140, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(qwerty_persistence_mechanism__lock_in_reading, naturalization_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__lock_in_reading, beneficiary_extraction_reading).

% DUAL FORMULATION NOTE:
% The qwerty_persistence_mechanism kernel decomposes into three structurally distinct constraints because the natural-language label 'QWERTY persistence' conflates claims with different epsilon values, beneficiary structures, and enforcement mechanisms. This reading (lock_in_reading) models diffuse inertial persistence without concentrated extraction; the sibling readings model genuine adequacy and active extraction respectively.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
