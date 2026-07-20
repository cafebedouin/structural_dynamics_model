% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__beneficiary_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__beneficiary_extraction_reading, []).

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
 *   constraint_id: qwerty_persistence_mechanism__beneficiary_extraction_reading
 *   human_readable: QWERTY Persistence via Incumbent Beneficiary Extraction
 *   domain: economic_history/technology_studies/path_dependence
 *
 * SUMMARY:
 *   This constraint story instantiates the beneficiary_extraction reading of
 *   the qwerty_persistence_mechanism kernel. It treats the persistence of the
 *   QWERTY keyboard layout not as a mountain of technical inevitability, nor
 *   as a passive coordination failure (lock-in), but as an actively
 *   maintained arrangement in which incumbent manufacturers and training
 *   institutions captured returns by suppressing alternatives and inflating
 *   switching costs. The constraint is the institutional complex that
 *   enforces QWERTY exclusivity beyond the founding coordination problem's
 *   lifespan.
 *
 * KEY AGENTS:
 *   - typewriter_manufacturers: Primary agenda-setter (powerful/mobile) â enforces the constraint to protect sunk investment and market power
 *   - typing_schools: Primary beneficiary (moderate/constrained) â collects rents from standardized training lock-in
 *   - typists: Primary payer (powerless/constrained) â bears productivity and wage costs of the enforced standard
 *   - alternative_layout_inventors: Excluded party (moderate/trapped) â structurally barred from market entry by incumbent control of adoption channels
 *   - economic_historians: Analytical observer (analytical/analytical) â adjudicates evidentiary claims about active maintenance vs. passive path dependence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.75).
domain_priors:suppression_score(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.72).
domain_priors:theater_ratio(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__beneficiary_extraction_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__beneficiary_extraction_reading, "QWERTY Persistence via Incumbent Beneficiary Extraction").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__beneficiary_extraction_reading, "economic_history/technology_studies/path_dependence").

domain_priors:requires_active_enforcement(qwerty_persistence_mechanism__beneficiary_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__beneficiary_extraction_reading, '8d62b6c1-0991-4eb6-b18f-6e26a54a0f97').
narrative_ontology:cs_kernel_codification('8d62b6c1-0991-4eb6-b18f-6e26a54a0f97', fixed_text).
narrative_ontology:cs_authority_grounding('8d62b6c1-0991-4eb6-b18f-6e26a54a0f97', extraction).
narrative_ontology:cs_interpretation_layer_present('8d62b6c1-0991-4eb6-b18f-6e26a54a0f97').
narrative_ontology:cs_reading_relation('8d62b6c1-0991-4eb6-b18f-6e26a54a0f97', qwerty_persistence_mechanism__naturalization_reading, influences).
narrative_ontology:cs_reading_relation('8d62b6c1-0991-4eb6-b18f-6e26a54a0f97', qwerty_persistence_mechanism__lock_in_reading, coexists_with).
narrative_ontology:cs_axiom('8d62b6c1-0991-4eb6-b18f-6e26a54a0f97', foundational, incumbent_maintenance_causally_decisive).
narrative_ontology:cs_axiom_status(incumbent_maintenance_causally_decisive, holdable).
narrative_ontology:cs_axiom_grounding('8d62b6c1-0991-4eb6-b18f-6e26a54a0f97', incumbent_maintenance_causally_decisive, empirically_contingent).
narrative_ontology:cs_axiom('8d62b6c1-0991-4eb6-b18f-6e26a54a0f97', foundational, artificial_switching_cost_inflation).
narrative_ontology:cs_axiom_status(artificial_switching_cost_inflation, holdable).
narrative_ontology:cs_axiom_grounding('8d62b6c1-0991-4eb6-b18f-6e26a54a0f97', artificial_switching_cost_inflation, empirically_contingent).
narrative_ontology:cs_reference_frame('8d62b6c1-0991-4eb6-b18f-6e26a54a0f97', incumbent_controlled_standard).
narrative_ontology:cs_drift_state('8d62b6c1-0991-4eb6-b18f-6e26a54a0f97', post_mass_adoption_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8d62b6c1-0991-4eb6-b18f-6e26a54a0f97', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, typewriter_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, typing_schools).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, typists).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, alternative_layout_inventors).
narrative_ontology:constraint_vindicates(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_advantage_theory).
narrative_ontology:constraint_vindicates(qwerty_persistence_mechanism__beneficiary_extraction_reading, switching_cost_extraction_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dominated the mechanical typing market from the late 19th century; invested heavily in QWERTY-specific tooling, training pipelines, and brand recognition. Actively resisted alternative layouts through marketing, bundling with typing instruction, and control of replacement parts standards. Could have retooled but chose to defend existing sunk-cost advantages and market share, receiving the concentrated gains from standard lock-in.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, typewriter_manufacturers, agenda_setter,
    powerful, generational, mobile, national).

% Built curricula, certification, and instructor expertise entirely around the QWERTY layout. Benefited from a guaranteed stream of students who needed standardized credentials to enter the clerical labor market. Switching to an alternative layout would have rendered their sunk instructional capital obsolete and broken their placement relationships with employers.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, typing_schools, beneficiary,
    moderate, biographical, constrained, national).

% Absorbed the costs of a suboptimal layout through lower productivity, repetitive strain, and suppressed wages in a labor market where QWERTY proficiency was treated as a generic skill. Retraining to an alternative layout was personally costly and offered no labor-market reward because employers standardized on QWERTY.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, typists, payer,
    powerless, biographical, constrained, national).

% Developed demonstrably more efficient keyboard arrangements. Were denied access to manufacturing partnerships, typing-school curricula, and government procurement channels because incumbents controlled the adoption infrastructure. Their exclusion was structural, not a result of consumer choice.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, alternative_layout_inventors, excluded,
    moderate, biographical, trapped, national).

% Analyze whether QWERTY dominance reflects efficient choice, passive path dependence, or active incumbent suppression. Their empirical work on typing speeds, manufacturer archives, and patent litigation records provides the evidentiary basis for distinguishing the kernel's competing readings.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, economic_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_mechanism__beneficiary_extraction_reading, typewriter_manufacturers).
narrative_ontology:fixing_cost_class(qwerty_persistence_mechanism__beneficiary_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solved the genuine collective-action problem of incompatible keyboard layouts in the early mechanical typing industry, enabling interoperability of machines, portability of labor skills, and economies of scale in training.
% TRANSFER_FUNCTION: Moves wealth and market power from typists (through suppressed productivity and wages) and from alternative inventors (through blocked market entry) to incumbent typewriter manufacturers and typing schools, by inflating switching costs and foreclosing alternative standards.
% ABSENT_VOICES: Alternative keyboard inventors and efficiency engineers were excluded from standard-setting bodies, manufacturer design committees, and government procurement offices; their technical evidence was dismissed or buried by incumbent-controlled institutions.
% DISAPPEARANCE_RATIONALE: If the active maintenance of QWERTY by incumbents vanishedâif manufacturers had retooled, schools had retrained, and procurement had opened to alternativesâthe keyboard standard would have fragmented or shifted toward more efficient layouts, the typing labor market would have repriced skills, and the distribution of rents in the office-equipment sector would have reorganized.
% FOUNDING_PROBLEM: The absence of a common keyboard layout in the 1870s-1880s created a coordination failure: typists could not move between machines, manufacturers could not achieve training externalities, and the market for mechanical writing was fragmented.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians from outside the incumbent beneficiary set (e.g., Paul David, Stanley Liebowitz, Stephen Margolis) attest that the initial coordination problem was real but was solved by the 1920s; subsequent persistence is not explained by the founding problem's continued existence.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__beneficiary_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__beneficiary_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__beneficiary_extraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence_mechanism__beneficiary_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.75, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__beneficiary_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence_mechanism__beneficiary_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence_mechanism__beneficiary_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75) is high because the constraint systematically transfers surplus from typists and blocked inventors to incumbents through an artificially maintained standard. Suppression (0.72) is high because persistence required active exclusion of alternatives from manufacturing, education, and procurement channels. Theater_ratio (0.45) captures the extent to which 'standardization' rhetoric served as a coordinating cover for extraction. Accessibility_collapse (0.78) reflects how completely alternatives disappeared from viable market access once the incumbent complex was entrenched. Resistance (0.48) is moderate because alternative layouts periodically surfaced (Dvorak, etc.) but were defeated by institutional blockades rather than market testing.
 *
 * PERSPECTIVAL GAP:
 *   From the incumbent manufacturer's seat, QWERTY persistence is legitimate coordination: they built the market, invested in the ecosystem, and are entitled to returns on standardization. From the typist's seat, the same structure is an imposed inefficiency that suppresses wages and causes injury. From the excluded inventor's seat, it is an active suppression of innovation. The engine computes these divergent seat types from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Typewriter manufacturers and typing schools are structural beneficiaries: they collect rents from the constraint's operation and have low directionality (d near 0.0). Typists are structural targets: they bear the costs of an enforced suboptimal standard with high directionality (d near 1.0). Alternative layout inventors are excluded targets whose exclusion is the enforcement mechanism itself (d near 1.0). Economic historians sit at analytical distance.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification prevents two errors: (1) mislabeling the constraint as a pure Rope by requiring identifiable victims and active enforcementâstandardization genuinely solved a founding coordination problem, but the continued enforcement beyond its necessity creates extraction; (2) mislabeling it as a pure Snare by requiring a genuine coordination functionâthe interoperability and training externalities were real, so the constraint is not extraction all the way down. The metrics are authored to capture this hybridity independently of the claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_maintenance_vs_passive_lockin,
    'Does historical evidence show active incumbent suppression of alternatives (lobbying, contractual tying, procurement capture), or does QWERTY persistence reflect passive network-effects lock-in?',
    'Archival analysis of manufacturer correspondence, typing-school contracts, and government procurement records from 1890-1950 to identify explicit suppression campaigns versus mere market tipping.',
    'If maintenance was active, the Tangled Rope classification with high suppression holds. If purely passive, the constraint shifts toward the lock_in_reading (lower suppression, lower extraction, higher theater as post-hoc rationalization).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_maintenance_vs_passive_lockin, empirical, 'Whether QWERTY persistence was actively engineered or passively tipped.').

omega_variable(
    qwerty_kernel_reading_contest,
    'This constraint is the beneficiary_extraction reading of the qwerty_persistence_mechanism kernel. The sibling naturalization_reading treats persistence as evidence of QWERTY adequacy, and the lock_in_reading treats it as passive coordination failure. Which reading governs the structural classification?',
    'Cross-reading corroboration: compare the empirical predictions of each reading (e.g., whether alternative layouts were given fair market tests, whether incumbent profit margins correlate with standard maintenance) and identify which reading''s axioms remain holdable under the evidence.',
    'If the naturalization reading is correct, the constraint is a Rope or Mountain. If the lock_in reading is correct, it is a Tangled Rope with lower active enforcement. The current reading''s classification as Tangled Rope with high active extraction depends on incumbent agency being causally decisive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qwerty_kernel_reading_contest, conceptual, 'Structural ambiguity between active extraction, passive lock-in, and natural adequacy readings of the same kernel.').

omega_variable(
    founding_problem_obsolescence,
    'Had the initial coordination problem (incompatible keyboard layouts) been solved by alternative means or alternative layouts by the 1920s-1930s, or was QWERTY the only viable solution?',
    'Technical comparison of contemporary alternative layouts against QWERTY on 1920s-1940s typewriter engineering constraints; counterfactual analysis of market structure with open standards.',
    'If alternatives were technically viable and coordination could have been achieved without QWERTY-specific lock-in, the founding_problem_status is dead and the current persistence is extractive. If QWERTY was uniquely capable, the coordination function remains live, supporting a lower extraction classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the founding coordination problem required QWERTY specifically or could have been solved by alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(qwer_tr_t10, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(qwer_tr_t20, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(qwer_tr_t30, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(qwer_tr_t40, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 40, 0.33).
narrative_ontology:measurement(qwer_tr_t50, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 50, 0.38).
narrative_ontology:measurement(qwer_tr_t60, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(qwer_tr_t70, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 70, 0.43).
narrative_ontology:measurement(qwer_tr_t80, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 80, 0.45).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(qwer_be_t10, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 10, 0.2).
narrative_ontology:measurement(qwer_be_t20, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(qwer_be_t30, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(qwer_be_t40, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(qwer_be_t50, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(qwer_be_t60, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 60, 0.71).
narrative_ontology:measurement(qwer_be_t70, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 70, 0.73).
narrative_ontology:measurement(qwer_be_t80, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 80, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t0, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(qwer_su_t10, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(qwer_su_t20, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(qwer_su_t30, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(qwer_su_t40, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(qwer_su_t50, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 50, 0.7).
narrative_ontology:measurement(qwer_su_t60, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 60, 0.73).
narrative_ontology:measurement(qwer_su_t70, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 70, 0.75).
narrative_ontology:measurement(qwer_su_t80, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 80, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__beneficiary_extraction_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__beneficiary_extraction_reading, lock_in_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__beneficiary_extraction_reading, naturalization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the qwerty_persistence_mechanism kernel. The natural-language label 'QWERTY persistence' conflates three structurally distinct claims: active beneficiary extraction (this file), passive path-dependent lock-in (lock_in_reading), and naturalized market adequacy (naturalization_reading). Each has a different epsilon, victim/beneficiary structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
