% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__real_catastrophe_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_validity__real_catastrophe_only, []).

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
 *   constraint_id: competence_exercise_validity__real_catastrophe_only
 *   human_readable: Real Catastrophe as Sole Competence Validator
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   In safety-critical industries, a pervasive epistemic norm holds that
 *   genuine competence can be exercised and validated only through real
 *   catastrophic events, rendering simulation and drills structurally
 *   insufficient. This constraint story instantiates the
 *   real_catastrophe_only reading of the competence_exercise_validity kernel.
 *   Under this reading, safety institutions systematically devalue
 *   simulation-based validation, transferring the burden of proof to rare
 *   existential failures and allowing competence decay to hide behind system
 *   redundancy and favorable safety records. The arrangement is claimed as a
 *   hard-won empirical truth about human performance under existential
 *   stakes; the authored metrics treat it as an actively enforced extraction
 *   mechanism that externalizes risk to operators and the public while
 *   concentrating epistemic authority in a narrow class of
 *   catastrophe-experienced professionals.
 *
 * KEY AGENTS:
 *   - post_crisis_authority: Primary beneficiary (powerful/mobile) â collects status, consulting authority, and epistemic monopoly from the devaluation of simulation.
 *   - safety_executive_leadership: Agenda setter and secondary beneficiary (institutional/arbitrage) â administers the belief system, defunds simulation, captures cost avoidance.
 *   - frontline_operators: Primary target (moderate/constrained) â works with unvalidated competence, bears immediate error consequence.
 *   - public_at_risk: Secondary target (powerless/trapped) â carries catastrophic tail risk, excluded from safety discourse.
 *   - simulation_researchers: Excluded voice (moderate/constrained) â would advocate for drill-based validation, structurally marginalized.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__real_catastrophe_only, 0.72).
domain_priors:suppression_score(competence_exercise_validity__real_catastrophe_only, 0.68).
domain_priors:theater_ratio(competence_exercise_validity__real_catastrophe_only, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, extractiveness, 0.72).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__real_catastrophe_only, snare).
narrative_ontology:human_readable(competence_exercise_validity__real_catastrophe_only, "Real Catastrophe as Sole Competence Validator").
narrative_ontology:topic_domain(competence_exercise_validity__real_catastrophe_only, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_validity__real_catastrophe_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__real_catastrophe_only, 'ae1dcf58-8f91-4fac-97b9-933bcf2c7927').
narrative_ontology:cs_kernel_codification('ae1dcf58-8f91-4fac-97b9-933bcf2c7927', implicit).
narrative_ontology:cs_authority_grounding('ae1dcf58-8f91-4fac-97b9-933bcf2c7927', practice).
narrative_ontology:cs_reading_relation('ae1dcf58-8f91-4fac-97b9-933bcf2c7927', competence_exercise_validity__simulation_as_proxy, forecloses).
narrative_ontology:cs_reading_relation('ae1dcf58-8f91-4fac-97b9-933bcf2c7927', competence_exercise_validity__continuous_refresh_hybrid, influences).
narrative_ontology:cs_axiom('ae1dcf58-8f91-4fac-97b9-933bcf2c7927', foundational, existential_stakes_irreducible).
narrative_ontology:cs_axiom_status(existential_stakes_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('ae1dcf58-8f91-4fac-97b9-933bcf2c7927', existential_stakes_irreducible, empirically_contingent).
narrative_ontology:cs_axiom('ae1dcf58-8f91-4fac-97b9-933bcf2c7927', foundational, simulation_deceives_confidence).
narrative_ontology:cs_axiom_status(simulation_deceives_confidence, holdable).
narrative_ontology:cs_axiom_grounding('ae1dcf58-8f91-4fac-97b9-933bcf2c7927', simulation_deceives_confidence, empirically_contingent).
narrative_ontology:cs_reference_frame('ae1dcf58-8f91-4fac-97b9-933bcf2c7927', catastrophe_validated_competence).
narrative_ontology:cs_drift_state('ae1dcf58-8f91-4fac-97b9-933bcf2c7927', contemporary_simulation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ae1dcf58-8f91-4fac-97b9-933bcf2c7927', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(competence_exercise_validity__real_catastrophe_only, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__real_catastrophe_only, post_crisis_authority).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__real_catastrophe_only, safety_executive_leadership).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, frontline_operators).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, public_at_risk).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Veteran safety practitioners and crisis managers whose authority derives directly from having navigated real catastrophes. They command premium consulting fees, regulatory advisory roles, and institutional prestige that simulation-trained professionals cannot match. Their market value and epistemic standing depend on the continued devaluation of simulation-based credentials.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, post_crisis_authority, beneficiary,
    powerful, biographical, mobile, national).

% Senior executives who set organizational safety policy and training budgets. They administratively enforce the real-catastrophe standard by defunding advanced simulation programs and justifying competence gaps as irreducible. They benefit indirectly through reduced operational expenditure and simplified liability postures.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, safety_executive_leadership, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__real_catastrophe_only, safety_executive_leadership, beneficiary).

% Operators in safety-critical roles whose individual competence is formally unvalidated because the organization accepts that only catastrophe can truly test it. They bear the immediate consequence of latent errors and work within systems where their own readiness is assumed adequate only after failure.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, frontline_operators, payer,
    moderate, immediate, constrained, national).

% Populations living near industrial facilities or depending on safety-critical infrastructure. They carry the catastrophic downside of unvalidated operator competence and cannot opt out of the risk landscape. They are not invited to safety-culture debates.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, public_at_risk, payer,
    powerless, biographical, trapped, national).

% Researchers and practitioners developing high-fidelity simulation and continuous drill methodologies. They are structurally excluded from safety-standards committees and funding councils dominated by catastrophe-experienced authorities, and their work is dismissed as academically interesting but operationally irrelevant.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, simulation_researchers, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_validity__real_catastrophe_only, diffuse).
narrative_ontology:fixing_cost_class(competence_exercise_validity__real_catastrophe_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The claimed coordination function is to prevent dangerous overconfidence in procedural preparation by asserting that genuine competence requires existential stakes, thereby justifying investment in system redundancy and maintaining institutional humility about the limits of rehearsal.
% TRANSFER_FUNCTION: Transfers the burden of safety validation from continuous institutional practice to rare catastrophic events, and transfers catastrophic risk from institutional decision-makers to frontline operators and the surrounding public.
% ABSENT_VOICES: Simulation researchers and continuous-drill advocates who would argue that competence can be maintained through structured, high-fidelity rehearsal; they are excluded from safety-standards committees and funding councils dominated by catastrophe-experienced authorities.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, safety institutions would reorganize validation budgets toward simulation and continuous drill programs; the epistemic monopoly of catastrophe-experienced authorities would dissolve; safety culture would shift to demonstrated procedural readiness rather than post-hoc crisis narrative; liability frameworks would demand proactive validation evidence.
% FOUNDING_PROBLEM: Early industrial safety lacked reliable methods to stress-test human performance under novel failure modes; actual disasters were often the only source of actionable feedback on organizational competence.
% FOUNDING_PROBLEM_CORROBORATION: Historians of safety engineering and organizational sociology attest the founding context of limited simulation technology. Contemporary high-reliability organization scholars, aviation psychology researchers, and independent nuclear-safety reviewers attest that high-fidelity simulation now exists and the founding problem is solved. No corroboration from outside the benefiting parties supports the continued categorical exclusion of simulation.
narrative_ontology:disappearance_verdict(competence_exercise_validity__real_catastrophe_only, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_validity__real_catastrophe_only, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__real_catastrophe_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_exercise_validity__real_catastrophe_only, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_validity__real_catastrophe_only, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_validity__real_catastrophe_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_validity__real_catastrophe_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_validity__real_catastrophe_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint systematically substitutes catastrophe-dependent validation for continuous competence verification, allowing institutions to avoid the cost of rigorous simulation while externalizing catastrophic risk. Suppression (0.68) is high because the constraint's persistence depends on actively marginalizing simulation research and discrediting drill-based advocates within safety culture. Theater ratio (0.45) reflects moderate performative maintenance: post-crisis debriefs and 'lessons learned' rituals often serve to reassert the primacy of catastrophe narrative rather than to generate actionable procedural reform. Accessibility collapse (0.62) is substantial because once the real-catastrophe frame is adopted, simulation alternatives are dismissed categorically rather than evaluated empirically. Resistance (0.45) is moderate: simulation researchers and some high-reliability-organization scholars contest the frame, but are outgunned institutionally.
 *
 * PERSPECTIVAL GAP:
 *   From the post-crisis authority seat, the constraint is a rueful empirical law of human nature â the humility to know that you cannot know until tested by fire. From the frontline operator and public seats, the same structure is an abdication of institutional duty to validate readiness, masking decay behind redundancy and luck. The agenda-setter seat experiences it as a budgetary and liability convenience. The engine computes this divergence from the structural data â beneficiary/victim roles, power differentials, and exit options â without requiring claim reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   Post-crisis authorities are beneficiaries with mobile exit (they can sell expertise across institutions), so their directionality sits near the subsidy end. Safety executives are agenda-setters with arbitrage-grade exit (can move to other industries), also near the beneficiary end. Frontline operators are payers with constrained exit (skill specificity, employment dependence), placing them near the full-target end. The public is payer with trapped exit (geographic and infrastructural lock-in), yielding the highest effective extraction. Simulation researchers are excluded with constrained exit; though not directly paying, their exclusion is the suppression mechanism that protects the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â absence of reliable stress-testing methods in early industrial safety â is dead. High-fidelity simulation and continuous drill methodologies now exist and are deployed in some domains (aviation, nuclear). The constraint persists beyond its founding problem, satisfying the R5 genealogy criterion for mandatrophy. However, the presence of concentrated beneficiaries (post-crisis authority figures capturing status and economic rents) and active agenda-setters (executives avoiding validation costs) indicates the constraint is not merely inertial; it is actively maintained for extraction. This prevents misclassifying a living snare as a dead piton. The dead founding problem plus world_rearranges disappearance verdict triggers the capture/zombie flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the real_catastrophe_only reading represent a genuine epistemic limit or an institutionalized excuse for inadequate validation investment?',
    'Comparative outcome analysis across organizations that heavily invest in simulation versus those that do not, controlling for industry and scale; if simulation-heavy organizations show superior or equivalent safety outcomes, the reading is empirically undermined.',
    'If undermined empirically, reclassify from claimed epistemic limit to snare or piton; if supported, toward mountain or rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Empirical test of the kernel''s contested readings via safety outcomes').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (career penalties for simulation advocates) or internalized (safety professionals genuinely believe drills are futile)?',
    'Post-exit belief trajectory: if safety professionals who leave high-catastrophe-culture organizations adopt simulation-positive views, suppression is structural; if they retain the belief, it is internalized.',
    'If internalized, effective suppression exceeds structural measure and constraint is more deeply anchored; if structural, reform is cheaper.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    gain_capturer_identity,
    'Do post-crisis authority figures and institutions capture measurable gains (status, funding, policy influence) from the real-catastrophe-only stance, or are the gains entirely diffuse cost avoidance?',
    'Sociometric and budget-tracing analysis of safety institutions: map speaking fees, consulting contracts, and regulatory advisory roles to catastrophe-experienced individuals versus simulation-trained professionals.',
    'If concentrated capturer exists, confirms snare/tangled_rope extraction structure; if fully diffuse, supports piton or rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gain_capturer_identity, empirical, 'Whether extraction accrues to identifiable seats or remains diffuse').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__real_catastrophe_only, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_validity__real_catastrophe_only, theater_ratio, 0, 0.25).
narrative_ontology:measurement(comp_tr_t10, competence_exercise_validity__real_catastrophe_only, theater_ratio, 10, 0.35).
narrative_ontology:measurement(comp_tr_t20, competence_exercise_validity__real_catastrophe_only, theater_ratio, 20, 0.42).
narrative_ontology:measurement(comp_tr_t30, competence_exercise_validity__real_catastrophe_only, theater_ratio, 30, 0.48).
narrative_ontology:measurement(comp_tr_t40, competence_exercise_validity__real_catastrophe_only, theater_ratio, 40, 0.52).
narrative_ontology:measurement(comp_tr_t50, competence_exercise_validity__real_catastrophe_only, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(comp_be_t10, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(comp_be_t20, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(comp_be_t30, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(comp_be_t40, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(comp_be_t50, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 50, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(comp_su_t10, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(comp_su_t20, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(comp_su_t30, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(comp_su_t40, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 40, 0.64).
narrative_ontology:measurement(comp_su_t50, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(competence_exercise_validity__real_catastrophe_only, competence_exercise_validity__simulation_as_proxy).
narrative_ontology:affects_constraint(competence_exercise_validity__real_catastrophe_only, competence_exercise_validity__continuous_refresh_hybrid).

% DUAL FORMULATION NOTE:
% The kernel 'competence_exercise_validity' decomposes into three structurally distinct constraints per the epsilon-invariance principle. This reading (real_catastrophe_only) treats simulation as categorically insufficient, while sibling readings treat simulation as proxy or necessary hybrid component. Each reading carries distinct epsilon, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
