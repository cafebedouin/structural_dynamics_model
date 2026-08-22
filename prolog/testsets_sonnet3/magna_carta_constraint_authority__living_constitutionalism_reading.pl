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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: magna_carta_constraint_authority__living_constitutionalism_reading
 *   human_readable: Magna Carta as Living Constitutional Restraint (Due Process Lineage Reading)
 *   domain: constitutional_history/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates the living-constitutionalism reading of the
 *   Magna Carta kernel: the claim that the 1215 charter established a
 *   due-process restraint on sovereign/executive power that persists today
 *   not by continuous textual force but through an unbroken juridical lineage
 *   of precedent and reinterpretation. Under this reading, courts are the
 *   primary custodians and extenders of the restraint, subjects and citizens
 *   are its beneficiaries, and royal prerogative and executive discretion are
 *   the parties whose latitude is structurally curtailed. This is a genuine
 *   coordination reading (rope) rather than an extraction reading: the
 *   restraint solves a real commitment problem (how does a sovereign credibly
 *   bind future sovereigns against arbitrary seizure) with comparatively low
 *   extractiveness, since it constrains power rather than redistributing
 *   rents from a captive population. It is distinguished from the sibling
 *   feudal_obsolescence_reading, which denies any binding continuity past the
 *   13th century, and from the sibling parliamentary_sovereignty_reading,
 *   which agrees continuity exists but relocates ultimate authority to
 *   Parliament's statute-making power rather than to an autonomous juridical
 *   lineage.
 *
 * KEY AGENTS:
 *   - subjects_and_citizens: primary beneficiary (powerless/trapped) — protected by due process shield
 *   - common_law_judiciary: agenda_setter/beneficiary (institutional/analytical) — administers and extends the restraint through precedent
 *   - royal_prerogative_powers: primary target (powerful/constrained) — bears the restraint on discretionary action
 *   - executive_discretion_holders: secondary target (institutional/constrained) — modern inheritor of the restrained prerogative
 *   - parliament_and_legislature: excluded voice (institutional/constrained) — sidelined as ultimate authority-source under this reading
 *   - legal_historians: analytical observer — assesses historical accuracy of the claimed unbroken lineage
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__living_constitutionalism_reading, 0.28).
domain_priors:suppression_score(magna_carta_constraint_authority__living_constitutionalism_reading, 0.32).
domain_priors:theater_ratio(magna_carta_constraint_authority__living_constitutionalism_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__living_constitutionalism_reading, rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__living_constitutionalism_reading, "Magna Carta as Living Constitutional Restraint (Due Process Lineage Reading)").
narrative_ontology:topic_domain(magna_carta_constraint_authority__living_constitutionalism_reading, "constitutional_history/legal_philosophy/political_theory").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__living_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__living_constitutionalism_reading, 'f00dedcf-d87f-4d6f-a2fa-283c68de7aeb').
narrative_ontology:cs_kernel_codification('f00dedcf-d87f-4d6f-a2fa-283c68de7aeb', fixed_text).
narrative_ontology:cs_authority_grounding('f00dedcf-d87f-4d6f-a2fa-283c68de7aeb', lineage).
narrative_ontology:cs_interpretation_layer_present('f00dedcf-d87f-4d6f-a2fa-283c68de7aeb').
narrative_ontology:cs_reading_relation('f00dedcf-d87f-4d6f-a2fa-283c68de7aeb', magna_carta_constraint_authority__feudal_obsolescence_reading, forecloses).
narrative_ontology:cs_reading_relation('f00dedcf-d87f-4d6f-a2fa-283c68de7aeb', magna_carta_constraint_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('f00dedcf-d87f-4d6f-a2fa-283c68de7aeb', foundational, restraint_authority_is_juridically_self_perpetuating).
narrative_ontology:cs_axiom_status(restraint_authority_is_juridically_self_perpetuating, holdable).
narrative_ontology:cs_axiom_grounding('f00dedcf-d87f-4d6f-a2fa-283c68de7aeb', restraint_authority_is_juridically_self_perpetuating, conventional).
narrative_ontology:cs_axiom('f00dedcf-d87f-4d6f-a2fa-283c68de7aeb', secondary, due_process_principle_transcends_originating_grievance).
narrative_ontology:cs_axiom_status(due_process_principle_transcends_originating_grievance, holdable).
narrative_ontology:cs_axiom_grounding('f00dedcf-d87f-4d6f-a2fa-283c68de7aeb', due_process_principle_transcends_originating_grievance, instrumental).
narrative_ontology:cs_reference_frame('f00dedcf-d87f-4d6f-a2fa-283c68de7aeb', baronial_restraint_settlement_1215).
narrative_ontology:cs_drift_state('f00dedcf-d87f-4d6f-a2fa-283c68de7aeb', contemporary_judicial_review_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f00dedcf-d87f-4d6f-a2fa-283c68de7aeb', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, subjects_and_citizens).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, common_law_judiciary).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, constitutional_courts).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__living_constitutionalism_reading, royal_prerogative_powers).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__living_constitutionalism_reading, executive_discretion_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live under a legal order in which the crown or executive may not seize liberty, property, or due process without lawful judgment. This reading treats that shield as an inherited entitlement traceable through unbroken juridical precedent from 1215 onward, not as a grant the state may retract at will. The subject has no exit from the polity itself but benefits from the constraint whenever the executive seeks to act against them without process.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, subjects_and_citizens, beneficiary,
    powerless, civilizational, trapped, national).

% Interprets, extends, and applies the inherited restraint through case law, treating Magna Carta's due process clauses as a living charter whose meaning evolves through precedent rather than static textual limitation. The judiciary administers the constraint, cites it to check executive overreach, and its institutional authority is itself substantially derived from being the custodian of this interpretive lineage.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, common_law_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__living_constitutionalism_reading, common_law_judiciary, beneficiary).

% In jurisdictions descended from or influenced by English common law, courts invoke the Magna Carta lineage as foundational precedent for due process guarantees, extending its logic into modern constitutional review. Their authority to check other branches is partly grounded in this claimed unbroken inheritance.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, constitutional_courts, beneficiary,
    institutional, civilizational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__living_constitutionalism_reading, constitutional_courts, agenda_setter).

% The monarchy (and by extension the executive that inherited much of its discretionary authority) is barred from certain unilateral actions against persons and property that would otherwise be available to an unconstrained sovereign. Under this reading, that bar is not a contingent political settlement but a binding restraint accumulated through eight centuries of precedent — the crown cannot simply legislate it away by assertion of will.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, royal_prerogative_powers, payer,
    powerful, generational, constrained, national).

% Modern executive branches (ministries, security services, administrative agencies) inherit the same juridical restraint whenever they seek to detain, seize, or penalize without due process. They experience the constraint as a binding limit on emergency or expedient action, enforceable through judicial review grounded in the Magna Carta lineage, regardless of the executive's own preferences about efficiency or discretion.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, executive_discretion_holders, payer,
    institutional, generational, constrained, national).

% Under this reading, Parliament is a downstream inheritor and enforcer of the restraint rather than its ultimate master — it did not create the due process principle and, in the living-constitutionalism frame, cannot simply legislate it out of existence without disrupting a deeper juridical inheritance. This displaces Parliament's own preferred self-understanding (developed in the sibling parliamentary_sovereignty_reading), so its voice on the ultimate source of authority is structurally sidelined here.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, parliament_and_legislature, excluded,
    institutional, generational, constrained, national).

% Study the actual textual and institutional continuity between 1215 and the present, assessing whether the claimed unbroken lineage is historically accurate or a retrospective juridical narrative constructed to legitimate later constitutional developments.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_constraint_authority__living_constitutionalism_reading, diffuse).
narrative_ontology:fixing_cost_class(magna_carta_constraint_authority__living_constitutionalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a durable, precedent-based check on arbitrary executive/sovereign action: by treating due process restraint as inherited rather than freshly negotiated each generation, the arrangement solves the problem of a ruler credibly binding future rulers (and their own future selves) against unilateral seizure of liberty or property.
% TRANSFER_FUNCTION: Moves discretionary latitude away from whoever holds executive or sovereign power at any given moment, and vests it as a standing procedural entitlement running to subjects/citizens, administered and enforced by courts who gain interpretive authority in the process.
% ABSENT_VOICES: Parliament's own claim to be the ultimate source of constraint-revising authority is structurally excluded from this reading's account of where authority originates; parliamentary sovereignty theorists would object that treating the restraint as juridically self-perpetuating understates Parliament's capacity to override or repeal charter-derived doctrine by statute.
% DISAPPEARANCE_RATIONALE: If the inherited-restraint doctrine were rejected by courts and rulers overnight, judicial review grounded in Magna Carta lineage would lose its foundational citation, executive and prerogative actors would face materially fewer precedent-based checks on discretionary seizure, and constitutional courts elsewhere that cite the lineage as authority for due process protections would need alternative grounding — a substantial rearrangement of doctrinal architecture, not merely rhetorical loss.
% FOUNDING_PROBLEM: In 1215, the founding problem was a specific baronial revolt against King John's arbitrary taxation, seizure, and denial of justice; the barons sought enforceable limits on royal discretion. The living-constitutionalism reading holds that the underlying problem — arbitrary sovereign/executive power against the person — recurs in every generation and that the charter's restraint principle remains a live solution to it, refreshed through precedent rather than replaced.
% FOUNDING_PROBLEM_CORROBORATION: Common law judges and constitutional scholars sympathetic to this reading attest the restraint principle remains actively invoked and functionally live in ongoing due process litigation. Legal historians outside the judiciary's own institutional interest are more divided: many attest the specific 1215 grievances (feudal relief, scutage, forest law) are long dead, and that what persists is a reinterpreted symbolic doctrine rather than the literal founding arrangement — corroboration for 'liveness' is strongest for the general due-process principle and weakest for continuity with the actual 13th-century text.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__living_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__living_constitutionalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__living_constitutionalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_constraint_authority__living_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__living_constitutionalism_reading, 0.28, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low-to-moderate (0.28 by 2025) because this reading's core claim is that the arrangement restrains power rather than extracts from a subject population — the transfer runs from discretionary executive latitude toward procedural protection for ordinary people, which is the coordination story, not a rent-extraction story. Suppression is moderate and declining slightly over the interval (0.5 to 0.32): early enforcement of baronial restraint against King John required real coercive leverage (the security clause's twenty-five barons), while later centuries increasingly relied on institutionalized judicial review rather than raw threat of force. Theater ratio rises modestly (0.1 to 0.22) reflecting the growing ceremonial invocation of Magna Carta in modern legal rhetoric relative to its operative doctrinal content — courts and politicians cite it symbolically more than they derive novel holdings from it, but the underlying due-process function remains substantially real, keeping theater well below dominance.
 *
 * DIRECTIONALITY LOGIC:
 *   Subjects and citizens are declared beneficiaries with derived low directionality: the constraint subsidizes them by shielding against arbitrary state action, even though their personal exit options are trapped (they cannot leave the polity to escape the constraint's absence, but that trappedness cuts toward benefiting fully from the shield rather than being extracted from). Royal prerogative and executive discretion holders are declared victims/payers: the constraint removes latitude they would otherwise possess, and their exit options are constrained (an executive cannot simply exit the constitutional order to escape review). The judiciary sits as both agenda_setter and beneficiary because administering the restraint is also how it accumulates and exercises its own institutional authority — this is a case where enforcing a constraint and profiting from enforcing it are the same act, though the profit is authority rather than rents.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this as pure extraction (snare) by recognizing the genuine coordination function: a credible, precedent-anchored restraint on arbitrary power is a real solution to a real commitment problem, not merely cover for judicial rent-seeking. It equally prevents mislabeling it as costless natural law (mountain) by requiring active enforcement (requires_active_enforcement: true) and naming victims (prerogative and executive power) who are structurally worse off than they would be absent the restraint. The founding_problem_status is marked contested rather than dead or live because the specific 1215 grievances are extinct while the generalized due-process problem the reading claims descends from them remains actively litigated — this mismatch (dead specific problem, live general principle, doctrine persists) is exactly the R5 genealogy signal the framework is built to surface rather than resolve by assertion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    juridical_continuity_vs_reinvention,
    'Is the due-process restraint genuinely continuous from 1215, or is ''unbroken juridical lineage'' a retrospective legitimating narrative constructed largely in the 17th century (Coke) and 19th-20th centuries to justify independently-arising judicial review powers?',
    'Detailed doctrinal history tracing actual citation chains and holdings from 1215 through major common law due-process cases; comparison against periods where Magna Carta clauses went uncited for centuries (much of the 14th-16th centuries) to test whether ''continuity'' is real or retrofitted.',
    'If the lineage is substantially reinvented rather than continuous, this reading''s central claim (binding force via inherited precedent) weakens considerably, pushing the constraint''s structural type toward something closer to constructed doctrine dressed as natural inheritance — a false-summit-adjacent pattern even without formal mountain claim, since the coordination story rests on a naturalized historical claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(juridical_continuity_vs_reinvention, empirical, 'Whether the claimed unbroken juridical lineage from 1215 to present is historically accurate or a later constructed narrative.').

omega_variable(
    kernel_reading_authority_location,
    'Where does binding authority over the restraint actually reside — in an autonomous juridical tradition that no single body can revoke (this reading), in Parliament''s statute-making power (sibling parliamentary_sovereignty_reading), or nowhere binding at all past 1215 (sibling feudal_obsolescence_reading)?',
    'This is a live constitutional-theory dispute rather than an empirically resolvable question; resolution mechanism is normative/doctrinal argument and, ultimately, which framework a given court or polity''s constitutional practice actually follows over time (revealed institutional practice as evidence, not proof).',
    'Adopting this reading versus a sibling changes which body is treated as the true agenda_setter and which body is treated as excluded or subordinate; it directly determines whether Parliament''s repeal power over charter-descended doctrine is treated as legitimate exercise of ultimate authority or as an act this reading would characterize as encroaching on an inherited restraint it does not have final power to revise.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_authority_location, conceptual, 'The committer-level disagreement between the three kernel readings over where ultimate constraint-revising authority is located.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__living_constitutionalism_reading, 1215, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1215, 0.1).
narrative_ontology:measurement(magn_tr_t1400, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1400, 0.12).
narrative_ontology:measurement(magn_tr_t1689, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1689, 0.14).
narrative_ontology:measurement(magn_tr_t1900, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1900, 0.17).
narrative_ontology:measurement(magn_tr_t1970, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1970, 0.19).
narrative_ontology:measurement(magn_tr_t2025, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1215, 0.15).
narrative_ontology:measurement(magn_be_t1400, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1400, 0.18).
narrative_ontology:measurement(magn_be_t1689, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1689, 0.2).
narrative_ontology:measurement(magn_be_t1900, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1900, 0.22).
narrative_ontology:measurement(magn_be_t1970, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1970, 0.25).
narrative_ontology:measurement(magn_be_t2025, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 2025, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1215, 0.5).
narrative_ontology:measurement(magn_su_t1400, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1400, 0.42).
narrative_ontology:measurement(magn_su_t1689, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1689, 0.38).
narrative_ontology:measurement(magn_su_t1900, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1900, 0.35).
narrative_ontology:measurement(magn_su_t1970, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1970, 0.33).
narrative_ontology:measurement(magn_su_t2025, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 2025, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__living_constitutionalism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, feudal_obsolescence_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the magna_carta_constraint_authority kernel, each authored as a separate story per the ε-invariance principle: feudal_obsolescence_reading treats the charter as spent 13th-century baronial settlement with no modern binding force; living_constitutionalism_reading (this story) treats it as a self-perpetuating juridical restraint tradition binding all subsequent rulers; parliamentary_sovereignty_reading treats the restraint as real but fully absorbed into and revisable by parliamentary statute. All three share the same historical text as their referent object but diverge sharply in claimed_type, beneficiary/victim structure, and extractiveness because they diverge on where binding authority actually sits — exactly the committer-frame disagreement the omega variables in this file document.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
