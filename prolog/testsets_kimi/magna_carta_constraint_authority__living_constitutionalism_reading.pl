% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__living_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__living_constitutionalism_reading, []).

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
 *   constraint_id: magna_carta_constraint_authority__living_constitutionalism_reading
 *   human_readable: Magna Carta Living Constitutionalism: Inherited Due Process Binding Rulers
 *   domain: constitutional/legal/political_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the living constitutionalism reading
 *   of the Magna Carta kernel: the claim that the 1215 charter established an
 *   inherited due process and lawful restraint that binds all subsequent
 *   rulers through juridical precedent and evolutionary interpretation. Under
 *   this reading, the crown and executive are structurally constrained by a
 *   common law inheritance that expands through judicial interpretation,
 *   while subjects gain a procedural shield against arbitrary power. The
 *   constraint is claimed as ropeâgenuine coordination around inherited
 *   restraintâwith low-to-moderate extractiveness falling on royal
 *   prerogative.
 *
 * KEY AGENTS:
 *   - common_law_subjects (beneficiary/moderate/constrained): Subjects who receive due process protections against arbitrary rule.
 *   - crown_executive (payer/powerful/constrained): The royal and later executive authority whose arbitrary discretion is bounded by lawful process.
 *   - common_law_judiciary (agenda_setter/institutional/analytical): Courts that administer evolutionary interpretation and keep the precedent alive across centuries.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__living_constitutionalism_reading, 0.35).
domain_priors:suppression_score(magna_carta_constraint_authority__living_constitutionalism_reading, 0.3).
domain_priors:theater_ratio(magna_carta_constraint_authority__living_constitutionalism_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__living_constitutionalism_reading, rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__living_constitutionalism_reading, "Magna Carta Living Constitutionalism: Inherited Due Process Binding Rulers").
narrative_ontology:topic_domain(magna_carta_constraint_authority__living_constitutionalism_reading, "constitutional/legal/political_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__living_constitutionalism_reading, '41f25aba-4533-4bc6-83bb-38aa28737a10').
narrative_ontology:cs_kernel_codification('41f25aba-4533-4bc6-83bb-38aa28737a10', fixed_text).
narrative_ontology:cs_authority_grounding('41f25aba-4533-4bc6-83bb-38aa28737a10', lineage).
narrative_ontology:cs_interpretation_layer_present('41f25aba-4533-4bc6-83bb-38aa28737a10').
narrative_ontology:cs_reading_relation('41f25aba-4533-4bc6-83bb-38aa28737a10', magna_carta_constraint_authority__feudal_obsolescence_reading, forecloses).
narrative_ontology:cs_reading_relation('41f25aba-4533-4bc6-83bb-38aa28737a10', magna_carta_constraint_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('41f25aba-4533-4bc6-83bb-38aa28737a10', foundational, inherited_due_process_binds_successors).
narrative_ontology:cs_axiom_status(inherited_due_process_binds_successors, holdable).
narrative_ontology:cs_axiom_grounding('41f25aba-4533-4bc6-83bb-38aa28737a10', inherited_due_process_binds_successors, conventional).
narrative_ontology:cs_axiom('41f25aba-4533-4bc6-83bb-38aa28737a10', foundational, subjects_hold_process_rights_against_rulers).
narrative_ontology:cs_axiom_status(subjects_hold_process_rights_against_rulers, holdable).
narrative_ontology:cs_axiom_grounding('41f25aba-4533-4bc6-83bb-38aa28737a10', subjects_hold_process_rights_against_rulers, deontological).
narrative_ontology:cs_reference_frame('41f25aba-4533-4bc6-83bb-38aa28737a10', inherited_restraint_framework).
narrative_ontology:cs_drift_state('41f25aba-4533-4bc6-83bb-38aa28737a10', contemporary_parliamentary_supremacy_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('41f25aba-4533-4bc6-83bb-38aa28737a10', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, common_law_subjects).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__living_constitutionalism_reading, crown_executive).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive procedural protections against arbitrary imprisonment, seizure of property, and punishment. Their security in person and estate depends on the crown being required to act through recognized legal process. Exit would require leaving the jurisdiction or placing themselves outside the law, both of which are costly and dangerous.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, common_law_subjects, beneficiary,
    moderate, generational, constrained, national).

% Retains sovereignty and governance authority but loses arbitrary discretion over the lives and property of subjects. Must rule through established legal procedure and acknowledged custom. Cannot exit the constraint without forfeiting the legitimacy that succession and coronation depend upon; the framework is fused with the ideology of lawful kingship.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, crown_executive, payer,
    powerful, generational, constrained, national).

% Administers the evolutionary interpretation of the charter through precedent, gradually refining the scope of due process and lawful restraint. Their professional identity and authority are constituted by continuity with the common law tradition, making exit from the interpretive framework equivalent to abandoning their office.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, common_law_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates governance by establishing that rulers are bound by law and procedure, creating predictable constraints on arbitrary power that enable long-term political and economic planning by subjects.
% TRANSFER_FUNCTION: Transfers the authority to punish, seize property, or govern arbitrarily from unchecked royal discretion to bounded legal process; subjects receive procedural protections and predictability in exchange for acknowledging the lawful ruler's legitimacy.
% ABSENT_VOICES: Non-baronial feudal tenants, women, colonial populations, and later statutory positivists were excluded from the charter's protections and from the jurisprudential framework that evolved from it; they would question whether inherited due process was universally applied or merely a class-bound baronial compact dressed in universal language.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, executive and royal prerogative would re-expand, subjects would lose procedural shields against arbitrary detention and seizure, and the common law tradition of lawful restraint would fracture into ad hoc royal discretion, fundamentally rearranging the constitutional order.
% FOUNDING_PROBLEM: Medieval governance under King John suffered from arbitrary royal power: lands seized without trial, imprisonment without cause, and taxation without consent, creating a coordination failure where property and person were insecure.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary chroniclers and baronial record attest the arbitrary rule problem, but from within the benefiting baronial class. Modern constitutional historians such as J.C. Holt and later statutory positivists outside the common law beneficiary tradition contest whether the charter addressed a universal governance problem or merely redistributed feudal privileges among elites.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__living_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__living_constitutionalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__living_constitutionalism_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_constraint_authority__living_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__living_constitutionalism_reading, 0.35, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__living_constitutionalism_reading_tests).
:- end_tests(magna_carta_constraint_authority__living_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.35) because the primary effect is protective coordination for subjects; the cost to the crown is real but is the loss of arbitrary power rather than a transfer of resources. Suppression is moderate-low (0.30) because the constraint suppresses arbitrary executive action but does not suppress the crown's legitimate governing function or the subjects' alternatives. Theater ratio rises slowly to 0.32 because over centuries the charter acquired ceremonial veneration that sometimes outpaced its functional role, though the judicial enforcement remained real. Accessibility collapse is moderate-high (0.60) because once the due process framework is understood, arbitrary rule ceases to be a accessible alternative for legitimate governance. Resistance is moderate (0.40) reflecting historical royal resistance, notably in the century after sealing.
 *
 * PERSPECTIVAL GAP:
 *   The crown executive seat and the common law subjects seat compute very differently: from the crown's position the constraint is a loss of prerogative and a cost of rule; from the subjects' position it is a protective shield and a condition of legitimate governance. The judiciary seat sits near symmetric, administering the constraint without bearing its costs or receiving its protective benefits directly. The engine computes this divergence from the structural beneficiary-victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Common law subjects are declared beneficiaries (directionality near the beneficiary end: the constraint subsidizes their security). Crown executive is declared victim (directionality near the target end: the constraint extracts arbitrary power from this agent). The judiciary is agenda_setter with analytical exit, sitting near the middle. The effective extraction is scaled from the base 0.35 upward for the crown and downward for subjects, producing the per-seat asymmetry that the rope classification must accommodate as a coordination cost borne by the power holder.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as pure extraction by requiring a genuine coordination function (subjects gain predictable legal process) and low base extractiveness. If the base extractiveness were tuned high and the coordination function were absent, the engine would compute snare or tangled_rope despite any claim. The authored claim of rope is structurally grounded in the protective benefit to subjects and the absence of a concentrated capturer of rents; the crown's loss of discretion is a coordination cost, not a captured extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does Magna Carta possess trans-historical binding authority through evolutionary precedent, or is its modern constraint function a juridical construction projected backward onto a feudal compact?',
    'Historical analysis of judicial citation practices and constitutional treatises from 1215 to the present to determine whether courts before the 17th century treated the charter as binding on sovereigns or as a contingent feudal bargain.',
    'If authority is retroactively constructed by later jurists, this reading overstates coordination and understates extraction by the juridical class; the constraint type may shift from rope to tangled_rope or piton. If genuine inheritance, the rope classification is sustained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the living constitutionalism reading projects modern authority onto medieval text.').

omega_variable(
    executive_discretion_cost,
    'Is the loss of arbitrary executive discretion a genuine cost (extraction from rulers) or merely the elimination of a prior extraction mechanism?',
    'Comparative constitutional history examining whether executive efficiency and public welfare declined due to due process constraints, or whether arbitrary rule was primarily extractive of subjects.',
    'If the cost to the crown is actually the closing of an extraction channel, the constraint is more purely rope; if it imposes genuine coordination costs on governance, base extractiveness is higher than the protective framing suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(executive_discretion_cost, conceptual, 'Ambiguity in whether executive restraint extracts from rulers or removes their extraction tool.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__living_constitutionalism_reading, 0, 810).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magna_carta_lc_tr_t0, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(magna_carta_lc_tr_t162, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 162, 0.14).
narrative_ontology:measurement(magna_carta_lc_tr_t324, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 324, 0.19).
narrative_ontology:measurement(magna_carta_lc_tr_t486, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 486, 0.24).
narrative_ontology:measurement(magna_carta_lc_tr_t648, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 648, 0.28).
narrative_ontology:measurement(magna_carta_lc_tr_t810, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 810, 0.32).

% Extraction over time
narrative_ontology:measurement(magna_carta_lc_be_t0, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(magna_carta_lc_be_t162, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 162, 0.27).
narrative_ontology:measurement(magna_carta_lc_be_t324, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 324, 0.29).
narrative_ontology:measurement(magna_carta_lc_be_t486, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 486, 0.31).
narrative_ontology:measurement(magna_carta_lc_be_t648, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 648, 0.33).
narrative_ontology:measurement(magna_carta_lc_be_t810, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 810, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(magna_carta_lc_su_t0, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(magna_carta_lc_su_t162, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 162, 0.24).
narrative_ontology:measurement(magna_carta_lc_su_t324, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 324, 0.28).
narrative_ontology:measurement(magna_carta_lc_su_t486, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 486, 0.32).
narrative_ontology:measurement(magna_carta_lc_su_t648, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 648, 0.36).
narrative_ontology:measurement(magna_carta_lc_su_t810, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 810, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__living_constitutionalism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, feudal_obsolescence_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% The Magna Carta kernel decomposes into three structurally distinct constraints because the colloquial label 'Magna Carta authority' conflates a dead feudal compact, a living common law precedent, and a parliamentary statute. Each reading carries a different epsilon, beneficiary/victim structure, and classification. This file models the living constitutionalism reading only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
