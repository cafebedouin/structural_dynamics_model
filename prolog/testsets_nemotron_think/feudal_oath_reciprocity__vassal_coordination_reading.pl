% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__vassal_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Feudal Oath Reciprocal Obligations (Vassal Coordination Reading)
 *   domain: historical/legal/political
 *
 * SUMMARY:
 *   This constraint story captures the vassal_coordination_reading of the
 *   feudal oath kernel: the charter text (e.g., Magna Carta, local
 *   coutumiers, homage charters) establishes fixed, bounded, mutually
 *   enforceable obligations between vassal and lord. The reading treats the
 *   oath as a coordination device — a Rope — not an extraction license. Both
 *   parties are beneficiaries: vassals get heritable tenure and legal
 *   redress; lords get reliable levies and counsel. No structural victim
 *   exists within the vassal-lord dyad (serfs are excluded, not victims of
 *   this specific constraint). The claimed_type is rope; metrics reflect low
 *   extractiveness, low suppression, low theater — the charter courts
 *   functioned as genuine coordination enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__vassal_coordination_reading, 0.18).
domain_priors:suppression_score(feudal_oath_reciprocity__vassal_coordination_reading, 0.12).
domain_priors:theater_ratio(feudal_oath_reciprocity__vassal_coordination_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__vassal_coordination_reading, rope).
narrative_ontology:human_readable(feudal_oath_reciprocity__vassal_coordination_reading, "Feudal Oath Reciprocal Obligations (Vassal Coordination Reading)").
narrative_ontology:topic_domain(feudal_oath_reciprocity__vassal_coordination_reading, "historical/legal/political").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__vassal_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__vassal_coordination_reading, '123a0eaa-f852-4057-9d97-131d5cfe6933').
narrative_ontology:cs_kernel_codification('123a0eaa-f852-4057-9d97-131d5cfe6933', fixed_text).
narrative_ontology:cs_authority_grounding('123a0eaa-f852-4057-9d97-131d5cfe6933', lineage).
narrative_ontology:cs_interpretation_layer_present('123a0eaa-f852-4057-9d97-131d5cfe6933').
narrative_ontology:cs_reading_relation('123a0eaa-f852-4057-9d97-131d5cfe6933', feudal_oath_reciprocity__lord_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('123a0eaa-f852-4057-9d97-131d5cfe6933', feudal_oath_reciprocity__ecclesiastical_mediation_reading, influences).
narrative_ontology:cs_axiom('123a0eaa-f852-4057-9d97-131d5cfe6933', foundational, charter_reciprocity_binding).
narrative_ontology:cs_axiom_status(charter_reciprocity_binding, holdable).
narrative_ontology:cs_axiom_grounding('123a0eaa-f852-4057-9d97-131d5cfe6933', charter_reciprocity_binding, conventional).
narrative_ontology:cs_axiom('123a0eaa-f852-4057-9d97-131d5cfe6933', foundational, vassal_enforceability_against_lord).
narrative_ontology:cs_axiom_status(vassal_enforceability_against_lord, holdable).
narrative_ontology:cs_axiom_grounding('123a0eaa-f852-4057-9d97-131d5cfe6933', vassal_enforceability_against_lord, conventional).
narrative_ontology:cs_reference_frame('123a0eaa-f852-4057-9d97-131d5cfe6933', feudal_charter_law_framework).
narrative_ontology:cs_drift_state('123a0eaa-f852-4057-9d97-131d5cfe6933', post_centralized_state_emergence, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('123a0eaa-f852-4057-9d97-131d5cfe6933', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, vassals).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, lords).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__vassal_coordination_reading, vassals).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__vassal_coordination_reading, charter_law_binding).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__vassal_coordination_reading, reciprocal_obligation_doctrine).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__vassal_coordination_reading, mutual_enforceability_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold land tenure (fief) in exchange for defined military service (typically 40 days/year) and counsel. Can petition feudal courts when lords exceed charter bounds (excessive service demands, arbitrary dispossession). Exit is constrained: abandoning the fief means loss of status and livelihood; appeal to king is possible but distant and uncertain.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, vassals, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__vassal_coordination_reading, vassals, payer).

% Grant fiefs and receive defined service. Administer feudal courts that hear vassal complaints. Benefit from predictable military levies and counsel. Can alienate or subdivide fiefs within charter limits. Exit is mobile: can commute service to scutage, sell wardships, or negotiate terms with vassals; the charter framework is a tool they wield, not a cage.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, lords, agenda_setter,
    powerful, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__vassal_coordination_reading, lords, beneficiary).

% Ultimate feudal overlord; charters theoretically derive from royal concession. Intervenes in great disputes (e.g., Magna Carta moments) but day-to-day enforcement is local. Sees the charter system as a coordination layer that stabilizes the military aristocracy — useful while it lasts, replaceable when central bureaucracy matures.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, king_emperor, observer,
    institutional, generational, analytical, continental).

% Claims moral oversight of oaths (sacramental character) and runs ecclesiastical courts that hear perjury cases. Does not administer feudal tenure but can excommunicate oath-breakers. Its reading (ecclesiastical_mediation) overlaps but is not identical to the charter reading.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, church, observer,
    institutional, civilizational, analytical, continental).

% Work the land that fiefs comprise. Bear the economic extraction (rents, labor services) that funds the vassal-lord exchange. Have no standing in feudal courts, no voice in charter negotiation, and no exit — flight is punished as fugitive serfdom. Their objection would be to the entire feudal extraction stack, not to the charter's reciprocity per se.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, serfs_peasants, excluded,
    powerless, immediate, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates military defense and land tenure in a landscape without centralized state capacity: vassals provide defined military service and counsel; lords provide defined land tenure and protection. The charter text fixes the terms so neither side can unilaterally escalate.
% TRANSFER_FUNCTION: Moves military service (40 days/year standard) and counsel from vassals to lords; moves land tenure (heritable fief) and protection from lords to vassals. The charter bounds both flows — no open-ended extraction.
% ABSENT_VOICES: Serfs and peasants who fund the system through agrarian surplus but have no standing in charter courts. Ecclesiastical authorities who claim a higher moral bound (Christian charity) on the same oaths. Both would object to different aspects: serfs to the extraction stack, church to the purely secular enforceability.
% DISAPPEARANCE_RATIONALE: If charter-enforced reciprocity vanished overnight, the vassal-lord military-tenure nexus would lose its coordination mechanism. Lords would revert to arbitrary extraction; vassals would withhold service or seek alternative patrons. The feudal military system would fracture until a new coordination layer (royal bureaucracy, mercenary contracts, or allodial consolidation) emerged.
% FOUNDING_PROBLEM: Coordinating heavy cavalry defense and land tenure across fragmented polities after Carolingian collapse, where no central state could pay standing armies or enforce contracts.
% FOUNDING_PROBLEM_CORROBORATION: Medievalist consensus (e.g., Duby, Reynolds, Bartlett) outside feudal beneficiary lineages: the charter system solved a 10th–11th century coordination vacuum; by 1300 royal taxation, mercenary companies, and Roman law revival had obsoleted its founding problem. The arrangement persisted as inert form (piton drift) after the problem died.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__vassal_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__vassal_coordination_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__vassal_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
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
 *   Extractiveness stays low (0.12→0.18) because the charter bounds are specific (40 days, defined reliefs, inheritance rules) and courts enforce them against both parties. Suppression is low (0.10→0.12) because enforcement is bilateral — vassals sue lords successfully in feudal courts (e.g., Curia Regis rolls). Theater rises slightly (0.05→0.10) as royal courts begin absorbing feudal jurisdiction, making some charter proceedings performative. Accessibility_collapse is moderate (0.55): alternatives (allodial hold, commutation to scutage, appeal to king) exist but are costly. Resistance is low (0.22) because the system works for its principals — the coordination function is live through 1200.
 *
 * PERSPECTIVAL GAP:
 *   The vassal seat and lord seat should compute similarly (both near-symmetric coordination beneficiaries). The serf seat (excluded) would compute as high-extraction if the manorial constraint were in view — but this story isolates the charter-reciprocity layer. The engine's per-seat classification will show rope for vassal/lord, while a combined feudal-system story would show snare for serfs. This seat divergence is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Vassals and lords are mutual beneficiaries (d ≈ 0.4–0.5): each gains more from the coordination than they pay in constraint compliance. The king is an analytical observer (d ≈ 0.0). The church is an analytical observer with a competing reading. Serfs are excluded — they bear the agrarian extraction that funds the feudal layer, but that extraction is a separate constraint (manorialism), not this charter-reciprocity constraint. The engine will compute per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordinating cavalry defense without a state) died by 1300. The charter framework persisted as piton drift — theatrical maintenance of feudal forms (parlements, hereditary offices) while royal taxation and mercenaries did the real coordination work. The mandatrophy is resolved: the arrangement's coordination function is dead; its persistence was inertial/theatrical. This reading captures the live coordination phase (1000–1200), not the zombie phase.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (vassal_coordination_reading) of the contested kernel feudal_oath_reciprocity. What structural elements do the sibling readings (lord_extraction_reading, ecclesiastical_mediation_reading) change, and how would those changes reclassify the constraint?',
    'Compare the three readings'' beneficiary/victim structures, enforcement mechanisms, and epsilon referents. The lord_extraction_reading adds victims (vassals as payers) and raises ε; the ecclesiastical_mediation_reading adds a moral authority layer that suppresses extraction but introduces ecclesiastical courts as parallel enforcement.',
    'If lord_extraction_reading is the empirically dominant reading for a given region/period, the constraint reclassifies from rope to snare/tangled_rope. If ecclesiastical_mediation_reading dominates, suppression rises (ecclesiastical courts) but extraction falls (charity bounds). The kernel''s classification is reading-indexed, not kernel-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Commitment-system committer structure: one kernel, three readings, three constraints.').

omega_variable(
    mutual_enforceability_historical_reality,
    'Did vassals actually enforce charter bounds against lords in practice, or was ''mutual enforceability'' a juridical fiction masking lord dominance?',
    'Quantitative analysis of feudal court records (Curia Regis, Parlement rolls, coutumier verdicts): frequency of vassal-initiated suits against lords, success rates, and remedy enforcement. Compare to lord-initiated suits.',
    'If vassal enforcement was rare/ineffective, the coordination function is illusory — the constraint is a false-summit rope (actually tangled_rope or snare). If enforcement was real and bilateral, the rope classification holds for the live period.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mutual_enforceability_historical_reality, empirical, 'Whether the charter''s mutual enforceability was operational or theoretical.').

omega_variable(
    suppression_mechanism_ambiguity_feudal,
    'Is the measured suppression (0.12) structural (feudal court access barriers, appeal costs) or internalized (vassals accepting lord dominance as natural order)?',
    'Post-exit suppression trajectory: examine vassals who successfully appealed to royal courts or migrated to allodial regions — did they continue to self-limit claims, or did they fully exercise charter rights? If internalized suppression persisted after structural barriers fell, the effective suppression is higher.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — vassals carry the suppression with them. This would raise the computed χ for vassals and could shift classification toward tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity_feudal, empirical, 'Structural vs. internalized suppression in the vassal-lord charter relationship.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__vassal_coordination_reading, 1000, 1300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t1000, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1000, 0.05).
narrative_ontology:measurement(feud_tr_t1050, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1050, 0.06).
narrative_ontology:measurement(feud_tr_t1100, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1100, 0.07).
narrative_ontology:measurement(feud_tr_t1150, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1150, 0.08).
narrative_ontology:measurement(feud_tr_t1200, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1200, 0.09).
narrative_ontology:measurement(feud_tr_t1250, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1250, 0.1).
narrative_ontology:measurement(feud_tr_t1300, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1300, 0.1).

% Extraction over time
narrative_ontology:measurement(feud_be_t1000, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1000, 0.12).
narrative_ontology:measurement(feud_be_t1050, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1050, 0.14).
narrative_ontology:measurement(feud_be_t1100, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1100, 0.15).
narrative_ontology:measurement(feud_be_t1150, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1150, 0.16).
narrative_ontology:measurement(feud_be_t1200, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1200, 0.17).
narrative_ontology:measurement(feud_be_t1250, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1250, 0.18).
narrative_ontology:measurement(feud_be_t1300, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1300, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t1000, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 1000, 0.1).
narrative_ontology:measurement(feud_su_t1050, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 1050, 0.11).
narrative_ontology:measurement(feud_su_t1100, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 1100, 0.11).
narrative_ontology:measurement(feud_su_t1150, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 1150, 0.12).
narrative_ontology:measurement(feud_su_t1200, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 1200, 0.12).
narrative_ontology:measurement(feud_su_t1250, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 1250, 0.12).
narrative_ontology:measurement(feud_su_t1300, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 1300, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__vassal_coordination_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(feudal_oath_reciprocity__vassal_coordination_reading, 0.15).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, feudal_manorial_extraction).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, royal_taxation_bureaucracy).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, mercenary_contract_system).

% DUAL FORMULATION NOTE:
% This is the vassal_coordination_reading of kernel feudal_oath_reciprocity. The lord_extraction_reading and ecclesiastical_mediation_reading are sibling constraints. All three share the kernel_id but have distinct ε, beneficiaries/victims, and claimed_type. This reading asserts rope (low ε, mutual beneficiaries); lord_extraction_reading asserts snare/tangled_rope (high ε, vassals as victims); ecclesiastical_mediation_reading asserts scaffold/rope (moderate ε, church as agenda_setter).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
