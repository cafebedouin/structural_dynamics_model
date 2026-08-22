% ============================================================================
% CONSTRAINT STORY: personhood_boundary__potential_based_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personhood_boundary__potential_based_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: personhood_boundary__potential_based_reading
 *   human_readable: Personhood Boundary — Potential-for-Rational-Agency Reading
 *   domain: moral_philosophy/historical_ethics/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the personhood_boundary kernel:
 *   the potential_based_reading, on which moral standing attaches to beings
 *   with the potential for rational agency, so that severely disabled infants
 *   judged to lack that potential may fall outside the protected class. Per
 *   the epsilon-invariance principle, the sibling readings
 *   (birth_threshold_reading, fitness_contingent_reading) are separate
 *   constraints in separate files with different victim sets; nothing about
 *   the contest is averaged into this story. The epsilon referent is fixed:
 *   the standing arrangement under contest is the potential-governed
 *   allocation of moral standing itself — the practice of extending
 *   protection via developmental potential and licensing exclusion judgments
 *   at the impaired margin — never some idealized alternative arrangement.
 *   KEY AGENTS (by structural relationship): typical_newborns: protected
 *   class (powerless/trapped) — standing secured by potential despite present
 *   incapacity; severely_disabled_infants: primary target (powerless/trapped)
 *   — excluded from standing by diagnosis, unable to object or exit;
 *   guardian_medical_decision_authorities: agenda setter and partial
 *   beneficiary (powerful/constrained) — hold both the exclusion judgment and
 *   the treatment decision; clinical_ethics_committees: secondary agenda
 *   setter (institutional/constrained) — administer and standardize the
 *   judgment process; health_systems_and_insurers: indirect beneficiary
 *   (institutional/arbitrage) — accrue avoided-care costs and shape the
 *   decision environment; disability_rights_advocates: excluded objectors
 *   (organized/mobile) — contest the criterion from outside the clinical
 *   frame; courts_and_legislatures: analytical observer
 *   (institutional/analytical) — bound the arrangement case by case and could
 *   replace the criterion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__potential_based_reading, 0.65).
domain_priors:suppression_score(personhood_boundary__potential_based_reading, 0.6).
domain_priors:theater_ratio(personhood_boundary__potential_based_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__potential_based_reading, tangled_rope).
narrative_ontology:human_readable(personhood_boundary__potential_based_reading, "Personhood Boundary — Potential-for-Rational-Agency Reading").
narrative_ontology:topic_domain(personhood_boundary__potential_based_reading, "moral_philosophy/historical_ethics/commitment_systems").

domain_priors:requires_active_enforcement(personhood_boundary__potential_based_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__potential_based_reading, 'bfa9ab35-e963-432b-b82b-2c1ce3a99273').
narrative_ontology:cs_kernel_codification('bfa9ab35-e963-432b-b82b-2c1ce3a99273', distributed).
narrative_ontology:cs_authority_grounding('bfa9ab35-e963-432b-b82b-2c1ce3a99273', distributed).
narrative_ontology:cs_reading_relation('bfa9ab35-e963-432b-b82b-2c1ce3a99273', personhood_boundary__birth_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('bfa9ab35-e963-432b-b82b-2c1ce3a99273', personhood_boundary__fitness_contingent_reading, forecloses).
narrative_ontology:cs_axiom('bfa9ab35-e963-432b-b82b-2c1ce3a99273', foundational, potential_grounds_moral_standing).
narrative_ontology:cs_axiom_status(potential_grounds_moral_standing, holdable).
narrative_ontology:cs_axiom_grounding('bfa9ab35-e963-432b-b82b-2c1ce3a99273', potential_grounds_moral_standing, deontological).
narrative_ontology:cs_axiom('bfa9ab35-e963-432b-b82b-2c1ce3a99273', foundational, absence_of_relevant_potential_permits_exclusion).
narrative_ontology:cs_axiom_status(absence_of_relevant_potential_permits_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('bfa9ab35-e963-432b-b82b-2c1ce3a99273', absence_of_relevant_potential_permits_exclusion, instrumental).
narrative_ontology:cs_reference_frame('bfa9ab35-e963-432b-b82b-2c1ce3a99273', potential_rational_nature_standard).
narrative_ontology:cs_drift_state('bfa9ab35-e963-432b-b82b-2c1ce3a99273', contemporary_disability_rights_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('bfa9ab35-e963-432b-b82b-2c1ce3a99273', '').
narrative_ontology:cs_kernel_id(personhood_boundary__potential_based_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, typical_newborns).
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, guardian_medical_decision_authorities).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, severely_disabled_infants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, health_systems_and_insurers).
narrative_ontology:constraint_vindicates(personhood_boundary__potential_based_reading, potentiality_principle).
narrative_ontology:constraint_vindicates(personhood_boundary__potential_based_reading, rational_nature_dignity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are born without yet exercising language, planning, or self-awareness. Under this reading their standing does not depend on present capacities: possession of the developmental potential for rational agency is enough, so they enter the protected class at birth alongside older children and adults. They cannot act, object, or leave any arrangement made about them; everything that reaches them arrives through others' acceptance of the criterion.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, typical_newborns, beneficiary,
    powerless, biographical, trapped, global).

% Have profound impairments that clinicians judge incompatible with ever developing the capacities the criterion names. Under this reading they fall outside the protected class: their interests may be weighed against family, medical, and social goods without the side-constraint that rights provide. They cannot object, consent, or exit; their position is fixed by diagnosis at the beginning of life.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, severely_disabled_infants, payer,
    powerless, biographical, trapped, global).

% Parents together with neonatologists hold near-discretionary authority over life-sustaining treatment for impaired newborns, exercised within hospital protocols and subject to episodic court review. The reading hands them the exclusion judgment itself: they identify which infants the criterion covers. They also carry the duties, grief, and second-guessing that follow such judgments, and cannot simply walk away from a case once involved.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, guardian_medical_decision_authorities, agenda_setter,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__potential_based_reading, guardian_medical_decision_authorities, beneficiary).

% Administer the judgment process: convene when treatment questions arise, document reasoning, and standardize application of the criterion across cases. Their authority and caseload grow with each case the framework recognizes; they answer to hospital administration and professional bodies rather than to the infants the judgments concern.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, clinical_ethics_committees, agenda_setter,
    institutional, generational, constrained, national).

% Bear the cost of prolonged neonatal intensive care and accrue savings when treatment is withheld on quality-of-life grounds. They shape the decision environment through bed availability, coverage rules, and staffing pressure without appearing in any individual deliberation, and can shift costs across regions and payers.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, health_systems_and_insurers, beneficiary,
    institutional, biographical, arbitrage, national).

% Organize against quality-of-life exclusions of any human, arguing the criterion revives discredited classifications and endangers everyone living with disabilities. They testify, litigate, and publish, but sit outside the clinical rooms where individual judgments are made; within the framework their objections are treated as misunderstanding what the criterion is for.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, disability_rights_advocates, excluded,
    organized, biographical, mobile, national).

% Set the outer limits of parental and medical authority — reviewing withholding decisions, occasionally restricting them, occasionally authorizing formal protocols. They observe the arrangement from above, intervene case by case, and alone hold the power to replace the governing criterion outright.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, courts_and_legislatures, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(personhood_boundary__potential_based_reading, guardian_medical_decision_authorities).
narrative_ontology:fixing_cost_class(personhood_boundary__potential_based_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a single administrable test for who counts as a bearer of moral standing in neonatal care, extending protection to all humans with developmental potential for rational agency — including every typical newborn — and giving clinicians and families a shared rule for the hardest treatment decisions.
% TRANSFER_FUNCTION: Moves moral standing and treatment authority: standing is withheld from severely impaired infants judged to lack relevant potential, and the resulting decision power over their care accrues to parents, physicians, and hospital committees; protection and discretion flow away from the diagnosed class toward the judging seats.
% ABSENT_VOICES: The excluded infants themselves are structurally absent — they cannot object, retain counsel, or appeal. Disability rights advocates speak only from outside the clinical setting, and their contributions are classified within the framework as misunderstandings of the criterion's purpose. Future adults who would have grown from these infants are absent by construction.
% DISAPPEARANCE_RATIONALE: If the potential-based criterion vanished overnight, every jurisdiction would fall back on one of its siblings: under a birth threshold the excluded class disappears entirely and all impaired newborns gain unconditional standing, shifting decisions to futility and best-interest frames; under a demonstrated-fitness test typical newborns lose guaranteed standing. Treatment protocols, court dockets, and the protected class itself would reorganize around whichever replacement held.
% FOUNDING_PROBLEM: Draw a principled line for moral standing that neither demands actual rational exercise (which would strip standing from every infant) nor grants it indiscriminately (which forecloses mercy and triage at the extreme margins of impairment) — a line running from Aristotle through scholastic and Enlightenment thought, made urgent by neonatology's new ability to sustain profoundly impaired infants indefinitely.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties: disability studies scholarship documents the problem's persistence from the position of those the criterion exposes; mainstream bioethics journals continue dedicated debates; clinical bodies (national pediatrics associations, the Groningen protocol literature) and court records show jurisdictions still unable to close the question. No seat outside the dispute claims the problem is settled.
narrative_ontology:disappearance_verdict(personhood_boundary__potential_based_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__potential_based_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__potential_based_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(personhood_boundary__potential_based_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__potential_based_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__potential_based_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(personhood_boundary__potential_based_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(personhood_boundary__potential_based_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.65 at interval end) because the arrangement withdraws the strongest available protection — rights-grade standing — from a class that cannot resist, and concentrates the resulting discretion in the judging seats; it is bounded by the criterion's genuine protective reach over everyone else and by residual safeguards (best-interest review, pain-relief norms, episodic judicial oversight). Suppression (0.60) is structural rather than coercive-police: the target class is silenced by incapacity, and external objectors are absorbed as category errors rather than answered. Theater (0.30) reflects a real but increasingly ceremonial ethics-consultation layer: the criterion guides actual decisions, while a growing share of committee activity legitimates outcomes already shaped by resource pressure. The series run on one shared time grid (t=0..50, six points, all three metrics at every point). The trajectories are cyclical rather than monotonic: neonatal technology ratchets the impaired-surviving population upward (rising extraction and enforcement through t=20), disability-rights procedural wins (Baby Doe-era regulation, anti-discrimination law) impose a dip around t=30, and protocol formalization (explicit national end-of-life frameworks) re-intensifies the machinery thereafter. The oscillation is partly an extraction mechanism in itself — each relaxation legitimizes the framework, enabling the next ratchet — and the end-state scalars in base_properties were measured at the final phase of the cycle.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting seats the arrangement is careful, humane triage governed by a principled test; from the excluded class's structural position — voiced only by their advocates — it is the licensed withdrawal of protection from those least able to resist. Families of typical newborns experience the criterion purely as a shield; parents of impaired infants experience the same instrument as a burden of terrible discretion; health systems experience it as cost relief. The engine computes these divergent per-seat classifications from the structural data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   typical_newborns sit near the full-beneficiary end: the criterion subsidizes them with standing they could not otherwise claim at their stage of development. severely_disabled_infants sit at the full-target end: trapped by diagnosis, bearing the entire cost of exclusion, with no exit anywhere in the possibility space. guardian_medical_decision_authorities derive direct benefit (decision authority accrues to them — they are the receipt seat for the arrangement's principal gain) tempered by the duties and scrutiny the role carries. health_systems_and_insurers benefit indirectly through avoided costs while exerting background pressure on judgments. disability_rights_advocates absorb the suppression of their objections from outside the frame. The arrangement operates at national-to-global scope with verification of individual judgments difficult, which amplifies effective extraction on the trapped target class; suppression itself is authored unscaled, as a raw structural property.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem remains live, so no obsolescence declaration is warranted: the arrangement still performs the work it was built for. The classification guards both failure modes — reading the arrangement as pure coordination would erase the asymmetric victim class that bears its costs; reading it as pure extraction would erase the genuine protective function for typical newborns and the real triage paralysis it resolves. The mismatch consumer should watch the founding_problem_status x disappearance_verdict pair: both currently read live/world_rearranges, consistent with an actively enforced hybrid rather than a zombie mandate. If the founding problem were ever resolved (e.g., by consensus replacement of the criterion), the residual enforcement machinery would decay toward performance, and the theater trajectory would be the leading indicator.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epsilon_seat_indexing,
    'Does the excluded class bear extraction at all, given this reading denies that entities lacking potential for rational agency possess standing capable of being violated?',
    'Author epsilon from the external structural seat (the position of the excluded and their advocates, who bear the withdrawal of protection) versus the reading''s internal endorsing seat (which holds no wrong occurs because no right is violated), and compare the resulting classifications over the fixed referent.',
    'The internal seat drives epsilon toward zero and the arrangement toward a defended coordination mechanism; the external seat sustains high epsilon and pushes computed types toward the extractive end. The divergence between seats is itself the diagnostic signal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_seat_indexing, conceptual, 'Reading-indexed epsilon ambiguity over a fixed referent: whose lights measure the excluded class''s loss.').

omega_variable(
    potential_operationalization_drift,
    'Can ''potential for rational agency'' be assessed prospectively without collapsing into demonstrated-fitness testing?',
    'Compare the actual criteria used in clinical exclusion judgments against prospective markers of rational-agency potential; if judgments rest on presently observable capacities and responsiveness, the reading operates in practice as its fitness sibling.',
    'Operational convergence would merge this reading''s victim-set boundary with fitness_contingent_reading''s, shrinking the protected class further and invalidating the claimed structural delta between the two readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(potential_operationalization_drift, empirical, 'Whether the potential criterion is operationally distinct from fitness testing in clinical use.').

omega_variable(
    judgment_authority_capture,
    'Do parental and medical exclusion judgments track the infant''s welfare, or institutional and resource pressure?',
    'Audit treatment-withholding decisions against resource-context variables (bed availability, cost of continued care, insurer posture) statistically independent of prognosis severity.',
    'Demonstrated capture would push the arrangement from a hybrid toward pure extraction, with health_systems_and_insurers revealed as receivers rather than incidental beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judgment_authority_capture, empirical, 'Welfare-tracking versus resource-capture in exclusion judgments.').

omega_variable(
    kernel_sibling_delta,
    'How would the sibling readings of the personhood_boundary kernel restructure this constraint''s victim set and authority allocation?',
    'Instantiate birth_threshold_reading (universal post-birth standing — the excluded class empties, decisions shift to futility and best-interest frames) and fitness_contingent_reading (demonstrated fitness required — typical newborns join the exposed class) as separate constraint stories and compare victim sets, epsilon, and per-seat classifications.',
    'Victim-set boundaries and judgment-authority allocations move wholesale between readings; epsilon and computed classification follow. The kernel-level contest cannot be resolved inside any one story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_sibling_delta, conceptual, 'Committer structure: this constraint is one of three readings of the personhood_boundary kernel; the structural delta lives in the criterion fixing the boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__potential_based_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, personhood_boundary__potential_based_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(pers_tr_t0, observed).
narrative_ontology:measurement(pers_tr_t10, personhood_boundary__potential_based_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(pers_tr_t10, observed).
narrative_ontology:measurement(pers_tr_t20, personhood_boundary__potential_based_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement_basis(pers_tr_t20, observed).
narrative_ontology:measurement(pers_tr_t30, personhood_boundary__potential_based_reading, theater_ratio, 30, 0.33).
narrative_ontology:measurement_basis(pers_tr_t30, observed).
narrative_ontology:measurement(pers_tr_t40, personhood_boundary__potential_based_reading, theater_ratio, 40, 0.31).
narrative_ontology:measurement_basis(pers_tr_t40, observed).
narrative_ontology:measurement(pers_tr_t50, personhood_boundary__potential_based_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement_basis(pers_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, personhood_boundary__potential_based_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(pers_be_t0, observed).
narrative_ontology:measurement(pers_be_t10, personhood_boundary__potential_based_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(pers_be_t10, observed).
narrative_ontology:measurement(pers_be_t20, personhood_boundary__potential_based_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement_basis(pers_be_t20, observed).
narrative_ontology:measurement(pers_be_t30, personhood_boundary__potential_based_reading, base_extractiveness, 30, 0.56).
narrative_ontology:measurement_basis(pers_be_t30, observed).
narrative_ontology:measurement(pers_be_t40, personhood_boundary__potential_based_reading, base_extractiveness, 40, 0.61).
narrative_ontology:measurement_basis(pers_be_t40, observed).
narrative_ontology:measurement(pers_be_t50, personhood_boundary__potential_based_reading, base_extractiveness, 50, 0.65).
narrative_ontology:measurement_basis(pers_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t0, personhood_boundary__potential_based_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(pers_su_t0, observed).
narrative_ontology:measurement(pers_su_t10, personhood_boundary__potential_based_reading, suppression_requirement, 10, 0.57).
narrative_ontology:measurement_basis(pers_su_t10, observed).
narrative_ontology:measurement(pers_su_t20, personhood_boundary__potential_based_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement_basis(pers_su_t20, observed).
narrative_ontology:measurement(pers_su_t30, personhood_boundary__potential_based_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement_basis(pers_su_t30, observed).
narrative_ontology:measurement(pers_su_t40, personhood_boundary__potential_based_reading, suppression_requirement, 40, 0.63).
narrative_ontology:measurement_basis(pers_su_t40, observed).
narrative_ontology:measurement(pers_su_t50, personhood_boundary__potential_based_reading, suppression_requirement, 50, 0.6).
narrative_ontology:measurement_basis(pers_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__potential_based_reading, identity_coordination).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, birth_threshold_reading).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, fitness_contingent_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the personhood boundary' decomposes into three structurally distinct constraints — birth_threshold_reading, fitness_contingent_reading, and this potential_based_reading — each with its own victim set, epsilon, and stakeholders, per the epsilon-invariance principle. They form a constraint family linked via affects_constraints. The birth threshold reading is the legally dominant member and shapes the operating environment of the other two; this reading and the fitness reading compete directly over the same marginal cases (impaired infants), while differing sharply over typical newborns. Relations from this reading to both siblings are forecloses: the potential criterion as a core premise contradicts the birth threshold's unconditional grant on the impaired-infant class and the fitness test's demonstration requirement on typical newborns, so no single framework holds this reading's core alongside either rival core.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
