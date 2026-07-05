% ============================================================================
% CONSTRAINT STORY: udhr_article_3__negative_liberty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__negative_liberty_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: udhr_article_3__negative_liberty_reading
 *   human_readable: UDHR Article 3 — Negative Liberty Reading (Security as Freedom From State Violence)
 *   domain: constitutional_law/human_rights/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the negative-liberty reading of UDHR Article 3:
 *   the state is prohibited from depriving anyone of life or liberty except
 *   through narrow procedural justice, and 'security' is defined as freedom
 *   from state violence specifically. This reading grounds capital punishment
 *   abolition arguments, restrictive doctrine on state agents' use of lethal
 *   force, and expansive due process protections before any state
 *   deprivation. It is a distinct constraint from the positive-entitlement
 *   reading (which would obligate the state to provide material conditions
 *   for life and security) and the procedural-hybrid reading (which stops at
 *   guaranteeing due process mechanics without resolving the substantive
 *   liberty/welfare contest). The three readings are not the same constraint
 *   measured differently — they have different beneficiary/victim structures,
 *   different epsilon profiles, and different institutional homes. This file
 *   addresses only the negative-liberty reading.
 *
 * KEY AGENTS:
 *   - criminal_defendants: primary beneficiary (powerless/trapped) — the reading's protections operate directly on their cases
 *   - death_row_prisoners: primary beneficiary (powerless/trapped) — abolition arguments under this reading determine survival
 *   - civil_liberties_litigators: agenda-setter (organized/mobile) — builds and defends the doctrine through litigation
 *   - law_enforcement_agencies: primary payer (institutional/constrained) — operational latitude curtailed
 *   - crime_victims_advocacy_groups: primary payer (moderate/constrained) — bears externalized risk from restrictive doctrine
 *   - communities_facing_organized_violence: primary payer (powerless/trapped) — forgoes collective security measures the reading treats as secondary to the state-violence paradigm
 *   - positive_entitlement_advocates: excluded voice — structurally outside a reading that defines security as freedom-from-state-violence only
 *   - international_human_rights_bodies: analytical observer — tracks reading selection across jurisdictions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__negative_liberty_reading, 0.61).
domain_priors:suppression_score(udhr_article_3__negative_liberty_reading, 0.52).
domain_priors:theater_ratio(udhr_article_3__negative_liberty_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__negative_liberty_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__negative_liberty_reading, "UDHR Article 3 — Negative Liberty Reading (Security as Freedom From State Violence)").
narrative_ontology:topic_domain(udhr_article_3__negative_liberty_reading, "constitutional_law/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__negative_liberty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__negative_liberty_reading, '9b1477af-91e8-4ccb-9dcd-021df27ea74d').
narrative_ontology:cs_kernel_codification('9b1477af-91e8-4ccb-9dcd-021df27ea74d', fixed_text).
narrative_ontology:cs_authority_grounding('9b1477af-91e8-4ccb-9dcd-021df27ea74d', practice).
narrative_ontology:cs_interpretation_layer_present('9b1477af-91e8-4ccb-9dcd-021df27ea74d').
narrative_ontology:cs_reading_relation('9b1477af-91e8-4ccb-9dcd-021df27ea74d', udhr_article_3__positive_entitlement_reading, coexists_with).
narrative_ontology:cs_reading_relation('9b1477af-91e8-4ccb-9dcd-021df27ea74d', udhr_article_3__procedural_hybrid_reading, influences).
narrative_ontology:cs_axiom('9b1477af-91e8-4ccb-9dcd-021df27ea74d', foundational, security_is_freedom_from_state_violence_only).
narrative_ontology:cs_axiom_status(security_is_freedom_from_state_violence_only, holdable).
narrative_ontology:cs_axiom_grounding('9b1477af-91e8-4ccb-9dcd-021df27ea74d', security_is_freedom_from_state_violence_only, conventional).
narrative_ontology:cs_axiom('9b1477af-91e8-4ccb-9dcd-021df27ea74d', foundational, narrow_procedural_justice_is_the_sole_legitimate_gate_on_deprivation).
narrative_ontology:cs_axiom_status(narrow_procedural_justice_is_the_sole_legitimate_gate_on_deprivation, holdable).
narrative_ontology:cs_axiom_grounding('9b1477af-91e8-4ccb-9dcd-021df27ea74d', narrow_procedural_justice_is_the_sole_legitimate_gate_on_deprivation, deontological).
narrative_ontology:cs_reference_frame('9b1477af-91e8-4ccb-9dcd-021df27ea74d', post_totalitarian_state_violence_restraint).
narrative_ontology:cs_drift_state('9b1477af-91e8-4ccb-9dcd-021df27ea74d', contemporary_organized_violence_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9b1477af-91e8-4ccb-9dcd-021df27ea74d', '').
narrative_ontology:cs_kernel_id(udhr_article_3__negative_liberty_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, criminal_defendants).
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, death_row_prisoners).
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, civil_liberties_litigators).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, crime_victims_advocacy_groups).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, law_enforcement_agencies).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, communities_facing_organized_violence).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face the state's power to deprive them of life or liberty. Under this reading, they benefit from narrow procedural gates on capital punishment, restrictive self-defense doctrine applied to state agents, and expansive due process before any deprivation. Cannot exit the jurisdiction that holds them; their only leverage is the doctrine itself.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, criminal_defendants, beneficiary,
    powerless, biographical, trapped, national).

% Are the direct beneficiaries of capital punishment abolition arguments grounded in this reading of Article 3. The reading's expansive due process requirements and restrictive framing of legitimate state killing directly determine whether they live or die.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, death_row_prisoners, beneficiary,
    powerless, immediate, trapped, national).

% Bring and win cases establishing narrow procedural justice as the only permissible ground for state deprivation of life/liberty. They administer the doctrine's expansion through litigation strategy, amicus briefs, and precedent-building; their institutional survival and reputation are tied to the negative-liberty framing prevailing over rival readings.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, civil_liberties_litigators, agenda_setter,
    organized, generational, mobile, continental).

% Argue that the reading's restrictive self-defense doctrine and expansive due process protections for the accused externalize risk onto victims and potential victims. They bear the cost when procedural constraints they see as excessive delay or block state action against people who have harmed them; their exit option is limited to political mobilization for legislative counter-reform.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, crime_victims_advocacy_groups, payer,
    moderate, biographical, constrained, national).

% Operate under doctrine that treats nearly every use of lethal or restrictive force as presumptively suspect, requiring narrow procedural justification. They experience this as extraction of operational latitude — cases dismissed, uses of force litigated, restrictive self-defense standards applied retroactively to their conduct. They cannot exit the constitutional order they are embedded in; their only recourse is legislative or judicial pushback.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, law_enforcement_agencies, payer,
    institutional, immediate, constrained, national).

% Live in areas where organized violence (gangs, cartels, insurgency) is a daily threat. Under this reading, collective security measures that would otherwise be mobilized against such threats are constrained by the doctrine's insistence that state violence, not private or organized violence, is the paradigm harm Article 3 addresses. They bear the cost of forgone state protection without being able to relocate or opt out of the jurisdiction's constitutional framework.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, communities_facing_organized_violence, payer,
    powerless, biographical, trapped, local).

% Must design criminal justice, policing, and security policy within the narrow procedural constraints this reading imposes. They also administer and enforce the doctrine through courts and legislatures, giving them a dual position: constrained by the reading's demands while also being the institutional apparatus through which it operates.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, state_governments, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__negative_liberty_reading, state_governments, agenda_setter).

% Argue that focusing Article 3 exclusively on freedom from state violence ignores material deprivation, poverty, and structural harm as equally real threats to life and security. They are structurally absent from courts and doctrine built on the negative-liberty framing; their objection is that this reading forecloses welfare-based security claims by definition, not by argument.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, positive_entitlement_advocates, excluded,
    organized, generational, constrained, global).

% Monitor state compliance with Article 3 across multiple readings, issue interpretive guidance, and adjudicate individual complaints. They observe how different domestic legal systems select among the negative-liberty, positive-entitlement, and procedural-hybrid readings and can influence which reading gains ascendancy through comparative jurisprudence.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, international_human_rights_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_article_3__negative_liberty_reading, diffuse).
narrative_ontology:fixing_cost_class(udhr_article_3__negative_liberty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared, litigable standard restraining state actors from arbitrary killing, detention, and deprivation of liberty — coordinating expectations between citizens and the state about when lethal or restrictive force is permissible, and giving courts a workable, narrow test to apply.
% TRANSFER_FUNCTION: Moves discretionary latitude away from state security and law enforcement apparatuses toward individuals facing state action (defendants, detainees, condemned prisoners), and correspondingly shifts risk of harm from those individuals onto crime victims, threatened communities, and the operational capacity of law enforcement.
% ABSENT_VOICES: Positive-entitlement advocates who would read Article 3 as requiring material provision are structurally excluded from doctrine built on this reading — their claims are not weighed and rejected on the merits within this framework, they are simply outside its scope. Victims of organized, non-state violence in high-crime communities are also underrepresented in the litigation forums where this reading is elaborated.
% DISAPPEARANCE_RATIONALE: If the negative-liberty reading disappeared, capital punishment abolition arguments grounded in Article 3 would lose their strongest textual anchor, restrictive self-defense doctrine applied to state agents would need alternative justification, and due process protections against state deprivation could be renegotiated toward either a positive-entitlement or procedural-hybrid framework — criminal justice systems, use-of-force law, and death penalty jurisprudence would all shift.
% FOUNDING_PROBLEM: The UDHR's drafters, writing after state-perpetrated mass killing, arbitrary detention, and extrajudicial execution under totalitarian regimes, sought to constrain state power to kill or imprison its own citizens without narrow, reviewable justification.
% FOUNDING_PROBLEM_CORROBORATION: Civil liberties litigators and courts applying this reading attest the founding problem (unconstrained state violence) remains live, citing ongoing extrajudicial killings and arbitrary detention globally. Crime victims advocacy groups and law enforcement agencies, from outside the beneficiary set, attest that in many jurisdictions the operative threat to life and security now comes predominantly from organized non-state violence, and that a reading fixated on state action alone has drifted from addressing the security threats actually faced by vulnerable communities.
narrative_ontology:disappearance_verdict(udhr_article_3__negative_liberty_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__negative_liberty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__negative_liberty_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(udhr_article_3__negative_liberty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__negative_liberty_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__negative_liberty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_article_3__negative_liberty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_article_3__negative_liberty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.61) reflects that the negative-liberty reading, when it prevails, imposes real costs on crime victims, law enforcement, and threatened communities by treating state violence as the paradigm harm and channeling doctrinal energy toward constraining state action rather than addressing organized non-state violence. Suppression (0.52) is moderate: the reading is maintained through active litigation, precedent, and constitutional courts rather than brute coercion, but it does foreclose competing framings (positive-entitlement claims) from being heard on their own terms within the same doctrinal space. Accessibility collapse (0.42) is only moderate because rival readings (positive-entitlement, procedural-hybrid) remain live and contested in other jurisdictions and forums — the negative-liberty reading has not achieved global doctrinal monopoly. Resistance (0.71) is high: crime victims' groups, law enforcement, and communities facing organized violence actively contest the doctrine's restrictive scope, and this resistance is a genuine structural feature, not noise. Theater ratio (0.28) is low-moderate: the coordination function (constraining arbitrary state killing) is largely real, though some due-process formalism has become procedural without substantive teeth in weak-rule-of-law states.
 *
 * PERSPECTIVAL GAP:
 *   From the civil liberties litigator seat, this reading is coordination — a hard-won constraint on state power built through decades of precedent, protecting against a real and recurring threat. From the law enforcement and crime victims seats, the same doctrine registers as extraction: latitude removed, risk shifted, protection withheld from people harmed by organized non-state violence while procedural solicitude is extended to those who harmed them. The engine should compute these as different seat-level types from the same structural data — the tangled_rope claim is meant to hold both readings simultaneously as the correct structural description, not resolve which seat is 'right.'
 *
 * DIRECTIONALITY LOGIC:
 *   Criminal defendants and death row prisoners sit near the full-beneficiary end: the reading exists specifically to constrain state action against them, and they have essentially no exit from the jurisdiction that holds them, which under this reading cuts toward benefit rather than harm because the constraint is what protects them. Law enforcement agencies and state governments sit toward the target end: institutionally powerful but constrained by doctrine they did not choose and cannot easily exit. Crime victims' advocacy groups and communities facing organized violence are victims in a different sense — not targeted by the constraint directly, but bearing displaced risk because collective security measures against organized violence are deprioritized relative to the state-violence paradigm. Civil liberties litigators are the agenda-setters: organized, mobile, and structurally positioned to expand the doctrine through litigation, giving them the profile of an administering beneficiary group even though they are not personally at risk of state deprivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unconstrained state violence against citizens — remains partially live (extrajudicial killing and arbitrary detention persist in many jurisdictions), which argues against mandatrophy. But the corroboration from outside the beneficiary set (crime victims' groups, law enforcement) suggests the reading's exclusive focus on state violence has drifted from addressing where security threats have moved in many contexts — toward organized non-state violence. This is not full mandatrophy (the founding problem is contested-live, not dead) but it is exactly the kind of status where founding_problem_status=contested combined with the doctrine's continued rigid application produces the diagnostic tension the six-questions battery exists to surface. Classifying this as tangled_rope rather than snare or rope prevents mislabeling: it is not pure extraction (the coordination function against state violence is real and historically grounded) and it is not pure rope (there are genuine victims bearing displaced risk through the same structure that protects defendants).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_violence_vs_material_deprivation_as_security_threat,
    'Is ''security'' under Article 3 correctly understood as freedom from state violence specifically, or does the negative-liberty reading arbitrarily narrow a term the drafters intended more broadly?',
    'Comparative analysis of UDHR drafting history (travaux préparatoires) and subsequent treaty body interpretation (ICCPR Article 6/9 general comments) to establish whether ''security of person'' was drafted with a state-action limitation in mind or left deliberately open.',
    'If drafting history supports a broader reading, the negative-liberty reading''s exclusive focus on state violence is a constructed narrowing that benefits civil liberties litigation strategy rather than a natural reading of the text — strengthening the case that this reading''s high epsilon reflects genuine extraction from victims of non-state violence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_violence_vs_material_deprivation_as_security_threat, conceptual, 'Whether the negative-liberty reading''s state-action limitation on ''security'' is textually warranted or a strategic narrowing.').

omega_variable(
    kernel_reading_displacement,
    'Where the negative-liberty reading prevails in a jurisdiction''s doctrine, does it merely coexist with the positive-entitlement and procedural-hybrid readings, or does it functionally foreclose them by capturing the interpretive space Article 3 litigation occupies?',
    'Track whether jurisdictions with strong negative-liberty Article 3 jurisprudence also develop parallel positive-entitlement doctrine (e.g., through separate social-rights provisions) or whether the negative-liberty reading''s dominance correlates with the absence of any live positive-entitlement claims in that jurisdiction''s courts.',
    'If negative-liberty dominance correlates with positive-entitlement doctrine remaining undeveloped in the same jurisdictions, that supports treating the relationship as influences (crowding out resources/legitimacy for the sibling reading) rather than mere coexistence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_displacement, empirical, 'Whether negative-liberty doctrinal dominance structurally crowds out positive-entitlement claims within the same legal system.').

omega_variable(
    collective_security_measure_suppression_mechanism,
    'Is the diffuse cost borne by communities facing organized violence a direct consequence of the negative-liberty reading''s doctrine, or a consequence of separate policy choices about policing resources that happen to coincide with this doctrinal framework?',
    'Case studies comparing jurisdictions with similarly high organized-violence threat levels but different Article-3-analog doctrinal postures, controlling for policing budget and strategy.',
    'If the victimization is primarily doctrinal (restrictive self-defense/use-of-force standards directly impede effective collective security response), the tangled_rope classification with communities_facing_organized_violence as victims is well-grounded. If primarily a resourcing artifact, the victim declaration may overstate this reading''s causal contribution to that group''s harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_security_measure_suppression_mechanism, empirical, 'Whether harm to organized-violence-affected communities traces causally to this reading''s doctrine or to independent resourcing decisions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__negative_liberty_reading, 1948, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t1948, udhr_article_3__negative_liberty_reading, theater_ratio, 1948, 0.12).
narrative_ontology:measurement(udhr_tr_t1966, udhr_article_3__negative_liberty_reading, theater_ratio, 1966, 0.15).
narrative_ontology:measurement(udhr_tr_t1984, udhr_article_3__negative_liberty_reading, theater_ratio, 1984, 0.18).
narrative_ontology:measurement(udhr_tr_t2000, udhr_article_3__negative_liberty_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(udhr_tr_t2012, udhr_article_3__negative_liberty_reading, theater_ratio, 2012, 0.25).
narrative_ontology:measurement(udhr_tr_t2025, udhr_article_3__negative_liberty_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(udhr_be_t1948, udhr_article_3__negative_liberty_reading, base_extractiveness, 1948, 0.35).
narrative_ontology:measurement(udhr_be_t1966, udhr_article_3__negative_liberty_reading, base_extractiveness, 1966, 0.42).
narrative_ontology:measurement(udhr_be_t1984, udhr_article_3__negative_liberty_reading, base_extractiveness, 1984, 0.48).
narrative_ontology:measurement(udhr_be_t2000, udhr_article_3__negative_liberty_reading, base_extractiveness, 2000, 0.54).
narrative_ontology:measurement(udhr_be_t2012, udhr_article_3__negative_liberty_reading, base_extractiveness, 2012, 0.58).
narrative_ontology:measurement(udhr_be_t2025, udhr_article_3__negative_liberty_reading, base_extractiveness, 2025, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t1948, udhr_article_3__negative_liberty_reading, suppression_requirement, 1948, 0.3).
narrative_ontology:measurement(udhr_su_t1966, udhr_article_3__negative_liberty_reading, suppression_requirement, 1966, 0.36).
narrative_ontology:measurement(udhr_su_t1984, udhr_article_3__negative_liberty_reading, suppression_requirement, 1984, 0.41).
narrative_ontology:measurement(udhr_su_t2000, udhr_article_3__negative_liberty_reading, suppression_requirement, 2000, 0.46).
narrative_ontology:measurement(udhr_su_t2012, udhr_article_3__negative_liberty_reading, suppression_requirement, 2012, 0.49).
narrative_ontology:measurement(udhr_su_t2025, udhr_article_3__negative_liberty_reading, suppression_requirement, 2025, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__negative_liberty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(udhr_article_3__negative_liberty_reading, 0.12).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, positive_entitlement_reading).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, procedural_hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language concept 'UDHR Article 3' per the ε-invariance principle. negative_liberty_reading (this file, ε=0.61, tangled_rope) reads Article 3 as a restraint on state deprivation of life/liberty via narrow procedural justice; positive_entitlement_reading (separate file) reads the same text as obligating material provision for life and security, with a different beneficiary/victim structure (state as provider vs. state as restrained actor) and a different ε profile driven by redistributive extraction rather than doctrinal-restriction extraction; procedural_hybrid_reading (separate file) reads Article 3 as guaranteeing due-process mechanics without resolving the substantive contest, likely yielding lower ε as a narrower, more consensus procedural claim. All three link to each other via affects_constraints because they share a textual kernel and doctrinal contest over the same clause; strengthening one reading in a jurisdiction's jurisprudence structurally pressures the others' available interpretive space and resources.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
