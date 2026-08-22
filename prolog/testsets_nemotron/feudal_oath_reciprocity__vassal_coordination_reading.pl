% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__vassal_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_oath_reciprocity__vassal_coordination_reading, []).

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
 *   constraint_id: feudal_oath_reciprocity__vassal_coordination_reading
 *   human_readable: Feudal Oath Reciprocity (Vassal Coordination Reading)
 *   domain: medieval_political_economy
 *
 * SUMMARY:
 *   This constraint story captures the vassal coordination reading of the
 *   feudal oath: a written charter fixes reciprocal obligations (service for
 *   tenure) with mutual enforceability. The charter text is the constraint —
 *   it bounds what the lord can demand and what the vassal must render.
 *   Extraction is low because the terms are specific and bilateral;
 *   suppression is low because enforcement runs through peer courts and the
 *   vassal's own court, not a monopolistic apparatus. The reading presents
 *   the oath as a genuine coordination mechanism (rope) that solves the
 *   credible commitment problem of feudal society. This is one reading of the
 *   contested kernel 'feudal_oath_reciprocity'; sibling readings emphasize
 *   extraction (lord_extraction_reading) or sacramental limitation
 *   (ecclesiastical_mediation_reading).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__vassal_coordination_reading, 0.18).
domain_priors:suppression_score(feudal_oath_reciprocity__vassal_coordination_reading, 0.22).
domain_priors:theater_ratio(feudal_oath_reciprocity__vassal_coordination_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__vassal_coordination_reading, rope).
narrative_ontology:human_readable(feudal_oath_reciprocity__vassal_coordination_reading, "Feudal Oath Reciprocity (Vassal Coordination Reading)").
narrative_ontology:topic_domain(feudal_oath_reciprocity__vassal_coordination_reading, "medieval_political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__vassal_coordination_reading, 'e1b20bd0-c047-4e21-af23-46eea872ab75').
narrative_ontology:cs_kernel_codification('e1b20bd0-c047-4e21-af23-46eea872ab75', formalized).
narrative_ontology:cs_authority_grounding('e1b20bd0-c047-4e21-af23-46eea872ab75', practice).
narrative_ontology:cs_interpretation_layer_present('e1b20bd0-c047-4e21-af23-46eea872ab75').
narrative_ontology:cs_reading_relation('e1b20bd0-c047-4e21-af23-46eea872ab75', feudal_oath_reciprocity__lord_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('e1b20bd0-c047-4e21-af23-46eea872ab75', feudal_oath_reciprocity__ecclesiastical_mediation_reading, influences).
narrative_ontology:cs_axiom('e1b20bd0-c047-4e21-af23-46eea872ab75', foundational, charter_text_binds_both_parties_equally).
narrative_ontology:cs_axiom_status(charter_text_binds_both_parties_equally, holdable).
narrative_ontology:cs_axiom_grounding('e1b20bd0-c047-4e21-af23-46eea872ab75', charter_text_binds_both_parties_equally, conventional).
narrative_ontology:cs_axiom('e1b20bd0-c047-4e21-af23-46eea872ab75', foundational, fixed_service_quota_limits_lordly_extraction).
narrative_ontology:cs_axiom_status(fixed_service_quota_limits_lordly_extraction, holdable).
narrative_ontology:cs_axiom_grounding('e1b20bd0-c047-4e21-af23-46eea872ab75', fixed_service_quota_limits_lordly_extraction, conventional).
narrative_ontology:cs_reference_frame('e1b20bd0-c047-4e21-af23-46eea872ab75', charter_reciprocity_norm).
narrative_ontology:cs_drift_state('e1b20bd0-c047-4e21-af23-46eea872ab75', post_edwardian_parliament, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e1b20bd0-c047-4e21-af23-46eea872ab75', '2026-07-25T14:32:11Z').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, vassals).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, lords).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, tenant_peasantry).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__vassal_coordination_reading, vassals).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__vassal_coordination_reading, mutual_obligation_doctrine).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__vassal_coordination_reading, charter_enforceability_principle).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__vassal_coordination_reading, reciprocity_as_governance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bound to provide specified military service and counsel in exchange for defined land tenure and protection. The charter text fixes the service quota (e.g., 40 days/year), inheritance rules, and limits on arbitrary tallage. They can appeal to the lord's court or peers if terms are breached, but exit means forfeiting the fief — a biographical investment.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, vassals, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__vassal_coordination_reading, vassals, payer).

% Receive defined service and incident reliefs (marriage aid, ransom, knighting fees) fixed by charter. They administer the local court where disputes are heard. Their exit is effectively arbitrage-grade: they can commute service to scutage, sell wardships, or alienate portions of the honor without losing the structural position. The charter stabilizes their revenue against vaguer customary claims.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, lords, beneficiary,
    powerful, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__vassal_coordination_reading, lords, agenda_setter).

% Not party to the oath directly, but the charter's fixed service terms bound the lord's capacity to extract labor from them for the lord's own wars. Predictable service means predictable demesne management. Exit is constrained: they are bound to the manor but the charter limits the lord's ability to convert their customary dues into arbitrary war levies.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, tenant_peasantry, beneficiary,
    moderate, biographical, constrained, local).

% Monitor oath-taking as a sacramental act; intervene when perjury or usury allegations arise. They do not collect from the feudal exchange but their jurisdiction over oaths creates a parallel enforcement layer. Their seat is analytical: they observe whether the charter's terms conform to canon law on just price and just war.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, ecclesiastical_courts, observer,
    institutional, civilizational, analytical, continental).

% Registers and authenticates charters; the written record becomes the reference for dispute resolution. Over time, the chancery's standardized forms shape what 'reciprocity' means across honors. They are observers of the constraint's operation but agenda-setters for the textual form that locks it in.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, royal_chancery, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__vassal_coordination_reading, royal_chancery, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the credible commitment problem between a lord who needs reliable military service and vassals who need secure tenure: the charter text fixes the quantum of service and the protections against arbitrary exaction, making the exchange repeatable without continuous renegotiation or violence.
% TRANSFER_FUNCTION: Moves a defined bundle of military service, counsel, and incident payments from vassal to lord; moves defined land tenure, protection, and judicial access from lord to vassal. The quantities are fixed in the charter — not a revenue share, not a tax, but a specific enumerated obligation.
% ABSENT_VOICES: Unfree tenants and serfs on the demesne are not party to the charter; they would object to the lord's ability to redirect their labor to meet his charter service quota, but they have no standing in the feudal court. Merchant communities in chartered towns are also excluded — their freedoms are negotiated separately and the feudal charter does not bind them.
% DISAPPEARANCE_RATIONALE: If the charter-text enforcement vanished overnight, service terms would revert to oral custom and lordly discretion. Vassals would face unpredictable tallage and wardship demands; lords would lose the ability to enforce service quotas against recalcitrant vassals. The predictable military levy that underpins royal campaign planning would dissolve into ad hoc negotiation.
% FOUNDING_PROBLEM: Post-Carolingian fragmentation created a vacuum where lords held land by force and vassals held by sufferance. The charter oath was built to convert violent possession into a documented, bounded, mutually enforceable exchange — replacing 'might makes right' with 'right makes might' for the warrior aristocracy.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary chroniclers (Orderic Vitalis, Galbert of Bruges) attest the charter form spread because it reduced private warfare between lords and vassals. Royal ordinances (e.g., Henry I's Coronation Charter, Magna Carta's antecedents) corroborate that the reciprocal form was recognized as a stability mechanism by the crown itself — not merely by the benefiting parties.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__vassal_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__vassal_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__vassal_coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(feudal_oath_reciprocity__vassal_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__vassal_coordination_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_oath_reciprocity__vassal_coordination_reading_tests).
:- end_tests(feudal_oath_reciprocity__vassal_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Metrics are stable across the interval: the charter form spreads but its core reciprocity logic does not intensify extraction. Theater rises slightly as chancery forms standardize and ceremonial homage accumulates, but the functional core (fixed service for fixed tenure) remains. Suppression requirement is modest — the constraint persists because both parties prefer the charter's predictability to the violence of the alternative, not because exit is blocked. Accessibility collapse is moderate: customary alternatives existed but the charter's textual fixation made them legibly inferior for dispute resolution. Resistance is low from vassals and lords (both benefit) but present from royal authority which sees chartered immunities as fragmentation.
 *
 * PERSPECTIVAL GAP:
 *   From the vassal seat, the charter is a shield against arbitrary lordship. From the lord seat, it is a floor guaranteeing minimum service. From the peasant seat, it is a ceiling on the lord's war demands. From the royal seat, it is a barrier to direct crown-vassal ties. The engine will compute these divergences from the structural data — the authored metrics describe the constraint's operation, not any single seat's experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Vassals and lords are mutual beneficiaries with secondary payer roles — each gives and receives a defined bundle. The charter's bilateral fixation means neither seat is a structural target. Tenant peasantry benefits incidentarily from the lord's constrained extraction capacity. Ecclesiastical courts and royal chancery are analytical observers; the chancery also shapes the textual form. No seat has d near 1.0; the constraint is symmetric by design.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (converting violent possession into documented exchange) remains live throughout the interval: even as royal justice expands, the charter remains the primary vehicle for lord-vassal coordination. No mandatrophy — the constraint's function has not atrophied; it has been supplemented but not replaced. The theater ratio's slow rise reflects ceremonial accretion, not functional decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_vassal_coordination,
    'Does the vassal coordination reading accurately capture the feudal oath''s structure, or does it idealize a relationship that was extractive in practice?',
    'Comparative analysis of charter texts vs. litigation records (placita) across multiple honors: if vassals successfully enforce charter terms against lords at rates comparable to lords enforcing service, the coordination reading holds. If enforcement is asymmetrical, the extraction reading gains ground.',
    'If coordination is bilateral, the rope classification stands. If enforcement is systematically asymmetrical (lords enforce, vassals cannot), the constraint reclassifies toward tangled_rope or snare under the lord_extraction_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_vassal_coordination, empirical, 'Whether the charter''s mutual enforceability was real or rhetorical.').

omega_variable(
    charter_text_vs_customary_practice,
    'Is the charter text the operative constraint, or does customary practice (unwritten, flexible) do the actual coordination work with the charter as mere evidence?',
    'Diplomatic analysis of charter formulae vs. court rolls: if disputes are resolved by reference to charter terms, the text is operative. If courts invoke ''custom of the honor'' overriding charter terms, the text is secondary.',
    'If custom governs, the constraint''s ε is lower (custom is more flexible, less extractive) but its suppression is also lower (custom adapts). The rope classification may still hold but with different metric justifications.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(charter_text_vs_customary_practice, conceptual, 'Whether the written charter or unwritten custom is the binding constraint.').

omega_variable(
    cs_framing_vassal_coordination,
    'Is the feudal oath''s kernel best framed as a bilateral charter contract (this reading''s frame) or as a hierarchical status relationship grounded in the lord''s sovereign authority (the lord_extraction_reading''s frame)?',
    'Analyze the diplomatic formulae: does the charter language emphasize ''conventio'' (agreement) and ''fidelitas'' (mutual fidelity) or ''dominium'' and ''servitium'' (lordship and service)? The framing that matches the dominant formulae is the kernel''s primary codification.',
    'If bilateral contract frame dominates, the vassal coordination reading captures the kernel''s authority_grounding as ''practice'' (mutual practice generates the norm). If hierarchical frame dominates, the kernel''s authority_grounding is ''lineage'' (lord''s authority derives from grant) or ''extraction'' (lord''s authority prevents revision). This changes the cs_structure classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_vassal_coordination, conceptual, 'Commitment-system framing ambiguity: bilateral contract vs. hierarchical status as the kernel''s codification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__vassal_coordination_reading, 1050, 1300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feudal_oath_vassal_tr_t1050, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1050, 0.08).
narrative_ontology:measurement(feudal_oath_vassal_tr_t1100, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1100, 0.09).
narrative_ontology:measurement(feudal_oath_vassal_tr_t1150, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1150, 0.1).
narrative_ontology:measurement(feudal_oath_vassal_tr_t1200, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1200, 0.11).
narrative_ontology:measurement(feudal_oath_vassal_tr_t1250, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1250, 0.12).
narrative_ontology:measurement(feudal_oath_vassal_tr_t1300, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1300, 0.12).

% Extraction over time
narrative_ontology:measurement(feudal_oath_vassal_be_t1050, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1050, 0.15).
narrative_ontology:measurement(feudal_oath_vassal_be_t1100, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1100, 0.16).
narrative_ontology:measurement(feudal_oath_vassal_be_t1150, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1150, 0.17).
narrative_ontology:measurement(feudal_oath_vassal_be_t1200, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1200, 0.17).
narrative_ontology:measurement(feudal_oath_vassal_be_t1250, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1250, 0.18).
narrative_ontology:measurement(feudal_oath_vassal_be_t1300, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1300, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(feudal_oath_vassal_su_t1050, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 1050, 0.18).
narrative_ontology:measurement(feudal_oath_vassal_su_t1100, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 1100, 0.2).
narrative_ontology:measurement(feudal_oath_vassal_su_t1150, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 1150, 0.21).
narrative_ontology:measurement(feudal_oath_vassal_su_t1200, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 1200, 0.22).
narrative_ontology:measurement(feudal_oath_vassal_su_t1250, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 1250, 0.22).
narrative_ontology:measurement(feudal_oath_vassal_su_t1300, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 1300, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__vassal_coordination_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(feudal_oath_reciprocity__vassal_coordination_reading, 0.12).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity__lord_extraction_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity__ecclesiastical_mediation_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, royal_justice_expansion).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, scutage_commutation).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, charter_standardization_chancery).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the feudal oath into three structurally distinct readings: vassal coordination (rope, bilateral charter), lord extraction (tangled_rope, unilateral authorization), ecclesiastical mediation (scaffold, sacramental limitation with sunset in secularization). Each has different ε, different beneficiary/victim structure, and different cs_structure. The vassal coordination reading is the upstream coordination claim; the lord extraction reading cites the same charters as evidence of authorization. The ecclesiastical reading cites the same oaths as sacramental acts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
