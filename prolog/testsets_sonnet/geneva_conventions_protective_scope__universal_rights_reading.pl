% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__universal_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__universal_rights_reading, []).

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
 *   constraint_id: geneva_conventions_protective_scope__universal_rights_reading
 *   human_readable: Universal Rights Reading of Geneva Protective Scope (Common Article 3 + IHRL Floor)
 *   domain: international_humanitarian_law/legal_theory/armed_conflict
 *
 * SUMMARY:
 *   This story instantiates the universal_rights_reading of the Geneva
 *   Conventions' protective scope kernel: Common Article 3, read together
 *   with international human rights law, is understood to establish a
 *   non-derogable minimum floor of humane treatment applicable to every
 *   person affected by armed conflict, regardless of whether they meet the
 *   Article 4 criteria for combatant status. This is a distinct constraint
 *   from the state_centric_reading (which gates protection on uniformed,
 *   responsible-command status) and the hybrid_proportionality_reading (which
 *   scales protection by conflict-type classification). The three readings
 *   share a textual kernel — the Conventions and their commentaries — but
 *   diverge sharply on who counts as a protected person and how much
 *   operational latitude state militaries retain. This reading raises ε
 *   specifically on state military operations because it forecloses the
 *   classification-based exclusions that the state-centric reading permits,
 *   extending the beneficiary set to unprivileged belligerents and captured
 *   non-state fighters who would otherwise fall into a legal gap.
 *
 * KEY AGENTS:
 *   - civilian_populations: primary beneficiary (powerless/trapped) — receive the universal floor as their only protection
 *   - captured_non_state_fighters: primary beneficiary (powerless/trapped) — gain protection they would lack under a narrower reading
 *   - state_military_operational_command: primary payer (institutional/constrained) — bears the operational restriction
 *   - international_courts_and_tribunals: agenda_setter (institutional/analytical) — elaborates and enforces the doctrinal content
 *   - human_rights_ngos: agenda_setter/observer (organized/mobile) — advocates for and documents the reading's application
 *   - adversary_non_state_armed_groups: excluded (moderate/constrained) — benefit but rarely participate in defining the floor
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__universal_rights_reading, 0.58).
domain_priors:suppression_score(geneva_conventions_protective_scope__universal_rights_reading, 0.47).
domain_priors:theater_ratio(geneva_conventions_protective_scope__universal_rights_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__universal_rights_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__universal_rights_reading, "Universal Rights Reading of Geneva Protective Scope (Common Article 3 + IHRL Floor)").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__universal_rights_reading, "international_humanitarian_law/legal_theory/armed_conflict").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__universal_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__universal_rights_reading, 'bf65a580-8583-42d3-a404-f00a577ef51c').
narrative_ontology:cs_kernel_codification('bf65a580-8583-42d3-a404-f00a577ef51c', fixed_text).
narrative_ontology:cs_authority_grounding('bf65a580-8583-42d3-a404-f00a577ef51c', practice).
narrative_ontology:cs_interpretation_layer_present('bf65a580-8583-42d3-a404-f00a577ef51c').
narrative_ontology:cs_reading_relation('bf65a580-8583-42d3-a404-f00a577ef51c', geneva_conventions_protective_scope__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('bf65a580-8583-42d3-a404-f00a577ef51c', geneva_conventions_protective_scope__hybrid_proportionality_reading, influences).
narrative_ontology:cs_axiom('bf65a580-8583-42d3-a404-f00a577ef51c', foundational, protection_attaches_to_personhood_not_status).
narrative_ontology:cs_axiom_status(protection_attaches_to_personhood_not_status, holdable).
narrative_ontology:cs_axiom_grounding('bf65a580-8583-42d3-a404-f00a577ef51c', protection_attaches_to_personhood_not_status, deontological).
narrative_ontology:cs_axiom('bf65a580-8583-42d3-a404-f00a577ef51c', foundational, non_derogable_floor_applies_irrespective_of_reciprocity).
narrative_ontology:cs_axiom_status(non_derogable_floor_applies_irrespective_of_reciprocity, holdable).
narrative_ontology:cs_axiom_grounding('bf65a580-8583-42d3-a404-f00a577ef51c', non_derogable_floor_applies_irrespective_of_reciprocity, deontological).
narrative_ontology:cs_reference_frame('bf65a580-8583-42d3-a404-f00a577ef51c', common_article_3_customary_floor).
narrative_ontology:cs_drift_state('bf65a580-8583-42d3-a404-f00a577ef51c', post_9_11_counterterrorism_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('bf65a580-8583-42d3-a404-f00a577ef51c', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, civilian_populations).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, captured_non_state_fighters).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, unprivileged_belligerents).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, detained_persons).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, state_military_operational_command).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, counterterrorism_detention_operators).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__universal_rights_reading, human_dignity_universality_doctrine).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__universal_rights_reading, non_derogable_minimum_standards_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live inside active or adjacent conflict zones with no combatant status and no capacity to influence targeting or detention decisions made about them. The universal floor is the only protection available to them regardless of who controls the territory; without it they depend entirely on the discretion of whichever armed actor holds power over them.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, civilian_populations, beneficiary,
    powerless, immediate, trapped, global).

% Members of non-state armed groups who, under the state-centric reading, would fall entirely outside treaty protection as unprivileged belligerents. Under this reading, Common Article 3 and IHRL guarantee them a floor of humane treatment, fair trial guarantees, and protection from torture regardless of their lack of uniform or responsible-command status. They have no capacity to negotiate their own legal status; the reading is the only thing standing between them and unregulated treatment.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, captured_non_state_fighters, beneficiary,
    powerless, biographical, trapped, national).

% Individuals who take up arms without satisfying Article 4 criteria (no fixed insignia, no responsible command structure recognized by an opposing state). Under this reading they retain a non-derogable protective floor rather than falling into a legal gap; their treatment is governed by universal minimum standards rather than by their classification as lawful or unlawful combatants.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, unprivileged_belligerents, beneficiary,
    powerless, biographical, trapped, national).

% Persons held by state or non-state forces in the context of armed conflict, including those in extrajudicial or informal detention facilities. The universal floor constrains interrogation methods and detention conditions regardless of the detaining authority's classification of the detainee, giving them recourse to minimum-standard claims even outside formal POW status.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, detained_persons, beneficiary,
    powerless, biographical, trapped, national).

% Plans and executes targeting, detention, and interrogation operations against adversaries who often deliberately avoid Article 4 criteria (no uniforms, embedding among civilians). Under this reading, the same protective floor applies regardless of the adversary's compliance with the laws of war, which commanders argue removes the reciprocal incentive structure the Conventions were built on and constrains operational flexibility against actors who do not observe equivalent restraint. Exit is constrained: withdrawal from treaty obligations is politically and legally costly, and doctrine, training, and rules of engagement must be built around the universal floor.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, state_military_operational_command, payer,
    institutional, generational, constrained, global).

% Operate detention and interrogation facilities for captured non-state actors in asymmetric conflicts. This reading requires them to apply Common Article 3 and IHRL floor protections to detainees who, under the state-centric reading, would not qualify for treaty protection at all, foreclosing interrogation techniques and indefinite-detention practices that a narrower reading would permit.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, counterterrorism_detention_operators, payer,
    institutional, biographical, constrained, national).

% Bodies such as the ICTY, ICRC customary law studies, and human rights treaty bodies have progressively read Common Article 3 and IHRL together to extend and enforce the universal floor. They administer and elaborate the doctrine through jurisprudence, advisory opinions, and monitoring mechanisms, effectively setting the operative content of the reading even though they lack direct enforcement power over state militaries.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, international_courts_and_tribunals, agenda_setter,
    institutional, civilizational, analytical, global).

% Document violations, litigate before international bodies, and campaign to entrench the universal-floor reading as the governing interpretation. They have no coercive power but shape which reading gains normative traction through advocacy, reporting, and strategic litigation.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, human_rights_ngos, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__universal_rights_reading, human_rights_ngos, observer).

% Armed groups that benefit from the universal floor's protections when captured, but are rarely party to the treaty-making or interpretive process that defines the floor's content, and are not bound by symmetric obligations enforceable against them in the same institutional venues. Their voice enters mainly through advocacy proxies, not direct participation.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, adversary_non_state_armed_groups, excluded,
    moderate, immediate, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, non-derogable minimum standard of humane treatment applicable to every person affected by armed conflict, closing the legal gap that would otherwise leave unprivileged or unclassifiable persons with no protection at all — coordinating expectations across state militaries, non-state actors, courts, and humanitarian organizations around one floor rather than a patchwork of contested classifications.
% TRANSFER_FUNCTION: Moves operational flexibility and interrogation/detention latitude away from state military and security services and toward captured, detained, or affected persons who would otherwise fall outside Article 4's combatant-status gate; also moves interpretive authority toward international courts and human rights bodies rather than resting solely with state military legal counsel.
% ABSENT_VOICES: Adversary non-state armed groups who receive the floor's protections rarely participate in the treaty interpretation or enforcement bodies that define its content; the reading is elaborated primarily by courts, IGOs, and NGOs speaking on behalf of affected persons rather than by those persons or groups themselves.
% DISAPPEARANCE_RATIONALE: If the universal-rights reading were abandoned in favor of a purely state-centric reading, unprivileged belligerents and captured non-state fighters would lose treaty-based protection entirely, detention and interrogation practices toward them would no longer be constrained by the Common Article 3 floor, and international courts would lose the doctrinal basis for many of their asymmetric-conflict rulings. Military doctrine, detention policy, and human rights litigation strategy would all reorganize around the narrower gate.
% FOUNDING_PROBLEM: The classical laws of war assumed conflicts between symmetric state armies with uniformed, responsible-command combatants; this left persons in non-international armed conflicts, irregular fighters, and civilians caught in asymmetric warfare with no clear legal protection, exposed to the discretion of whichever force held power over them.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC's customary international humanitarian law study and ICTY jurisprudence (e.g., Tadić) attest that the protection gap for non-international and asymmetric conflicts remains a live structural problem, independent of state military self-assessment. State military legal advisors, however, largely attest that the problem has been substantially addressed by existing detention review mechanisms and that the universal floor now functions primarily as an operational constraint rather than a filled gap — this is a genuinely contested corroboration rather than unanimous outside attestation.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__universal_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__universal_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__universal_rights_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_protective_scope__universal_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__universal_rights_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__universal_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__universal_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__universal_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the real transfer of operational and legal flexibility from state military command to persons who would otherwise be excluded from protection — this is substantial but not maximal because the floor is genuinely minimal (Common Article 3 sets a low bar: humane treatment, no torture, fair trial guarantees) rather than a full POW-status regime. Suppression (0.47) is moderate: the reading is enforced through litigation, customary law citation, and diplomatic pressure rather than direct coercive machinery, though the ICTY/ICC apparatus and treaty-body reporting create real compliance pressure. Theater ratio (0.28) is modest-low: most of the apparatus around this reading (tribunal jurisprudence, ICRC commentary, detention review boards) performs genuine adjudicative work, though a meaningful share of state compliance reporting is documentary rather than substantive. Accessibility collapse (0.40) is deliberately moderate-low — the state-centric and hybrid readings remain live, cited, and adopted by some states, so alternatives to this reading have not collapsed; this is a live jurisprudential contest, not a settled monopoly. Resistance (0.72) is high because state military legal establishments actively contest this reading's application to non-uniformed adversaries, particularly in counterterrorism contexts, making this one of the most contested interpretive fault lines in contemporary IHL.
 *
 * DIRECTIONALITY LOGIC:
 *   Civilian populations, captured non-state fighters, unprivileged belligerents, and detained persons all sit near the full-beneficiary end: the reading extends a protective floor to them that a narrower state-centric reading would deny, and they have essentially no capacity to exit the conflict zones or detention systems that expose them to risk. State military operational command and counterterrorism detention operators sit toward the target end: the reading constrains their targeting, detention, and interrogation latitude, and their exit options are constrained rather than mobile — withdrawal from treaty commitments carries severe diplomatic and legal costs, so in practice they absorb the constraint rather than escaping it. International courts and human rights NGOs are agenda-setters who administer and expand the doctrine without bearing its operational costs directly, which is why they are marked institutional/organized rather than payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protection gaps for non-uniformed, irregular, or civilian persons in asymmetric conflict — remains empirically live per ICRC customary law studies and ICTY jurisprudence, which is corroboration from outside the direct beneficiary class (courts and legal scholarship, not the protected persons themselves). This blocks a mandatrophy read: the classification as tangled_rope rather than pure snare or pure rope is justified because the coordination function (closing the protection gap) is genuinely still needed, but it operates through active enforcement (tribunal jurisprudence, treaty-body pressure) that imposes real, contested costs on state military operations. Were the founding problem to become genuinely dead — e.g., if state and non-state actors converged on symmetric compliance making classification irrelevant — the same structure would look more like inertial theater than active coordination/extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocity_versus_universality,
    'Does extending Geneva protections to persons who do not themselves observe the laws of war (e.g., irregular fighters who target civilians) undermine the reciprocal incentive structure the Conventions were originally built on, or does universal application better serve the humanitarian object and purpose regardless of the adversary''s conduct?',
    'Comparative analysis of state compliance rates and battlefield conduct in conflicts where the universal-rights reading has been judicially applied (e.g., post-Tadić ICTY jurisprudence) versus conflicts governed by narrower state-centric practice, tracking whether adversary conduct converges toward or diverges from IHL compliance over time.',
    'If universal application is shown to erode reciprocal compliance incentives without increasing overall protection, this would strengthen the case for the hybrid or state-centric readings; if protection outcomes improve or remain stable, it strengthens the universal-rights reading''s coordination claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reciprocity_versus_universality, empirical, 'Whether universal application undermines or preserves the reciprocity logic underlying IHL compliance.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the universal_rights_reading the textually compelled interpretation of Common Article 3 and its relationship to IHRL, or is it one contestable interpretive choice among the state-centric and hybrid readings, each defensible from the treaty text and travaux préparatoires?',
    'Systematic review of ICJ advisory opinions, ICTY/ICC jurisprudence, and state practice/opinio juris to determine whether customary international law has converged on the universal floor or remains genuinely contested across jurisdictions.',
    'If customary law has converged on the universal reading, its classification as merely one contested reading understates its authority; if state practice remains genuinely divided (as much evidence suggests, given persistent state objections in counterterrorism contexts), the three-reading kernel structure is the accurate representation and no single reading should be treated as settled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the universal-rights reading is legally compelled or one contested reading among several defensible ones.').

omega_variable(
    operational_flexibility_cost_measurement,
    'How much genuine military operational capability is lost by extending the protective floor to unprivileged belligerents, versus how much of the claimed cost is exaggerated by military institutions resistant to external constraint?',
    'Independent military-effectiveness studies comparing operations conducted under strict universal-floor compliance versus operations where narrower classification-based exclusions were applied, controlling for conflict intensity and adversary tactics.',
    'If the operational cost is small relative to institutional resistance, the extractiveness score for state military command may be overstated; if the cost is substantial and well-documented, it validates treating state military command as a genuine payer bearing significant transferred cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(operational_flexibility_cost_measurement, empirical, 'Whether the operational cost claimed by state militaries is real or largely institutional resistance to constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__universal_rights_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1949, 0.15).
narrative_ontology:measurement(gene_tr_t1977, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1977, 0.18).
narrative_ontology:measurement(gene_tr_t1995, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(gene_tr_t2004, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 2004, 0.24).
narrative_ontology:measurement(gene_tr_t2012, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 2012, 0.27).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1949, 0.22).
narrative_ontology:measurement(gene_be_t1977, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1977, 0.3).
narrative_ontology:measurement(gene_be_t1995, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1995, 0.4).
narrative_ontology:measurement(gene_be_t2004, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 2004, 0.5).
narrative_ontology:measurement(gene_be_t2012, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 2012, 0.55).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1949, 0.3).
narrative_ontology:measurement(gene_su_t1977, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1977, 0.34).
narrative_ontology:measurement(gene_su_t1995, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1995, 0.38).
narrative_ontology:measurement(gene_su_t2004, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 2004, 0.44).
narrative_ontology:measurement(gene_su_t2012, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 2012, 0.46).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 2024, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__universal_rights_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_protective_scope__universal_rights_reading, 0.12).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, state_centric_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, hybrid_proportionality_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the geneva_conventions_protective_scope kernel. state_centric_reading and hybrid_proportionality_reading are siblings, not competing measurements of the same constraint — each has its own stable ε, its own beneficiary/victim structure, and its own classification. The universal_rights_reading raises ε on state military operations relative to the state-centric reading (by expanding the protected class) and treats the floor as non-contingent relative to the hybrid reading (by rejecting conflict-type-based tiering).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
