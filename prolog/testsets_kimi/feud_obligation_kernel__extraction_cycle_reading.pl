% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__extraction_cycle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__extraction_cycle_reading, []).

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
 *   constraint_id: feud_obligation_kernel__extraction_cycle_reading
 *   human_readable: Blood-Feud Obligations as Destructive Extraction Cycle
 *   domain: legal_anthropology/medieval_history/comparative_political_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the extraction_cycle_reading of the
 *   feud_obligation_kernel. Under this reading, blood-feud obligations are
 *   not a self-enforcing coordination mechanism for stateless justice but a
 *   destructive extraction cycle that depletes productive capacity and
 *   prevents territorial consolidation. The primary beneficiaries are
 *   centralizing royal authorities, who gain fiscal and political advantage
 *   from fragmented regional kinship power. The primary victims are the
 *   feuding kinship groups themselves, locked into reciprocal violence by
 *   identity-fused obligation. The constraint is actively enforced through
 *   kinship-based social control, with high suppression of exits such as
 *   legal recourse, migration, or reconciliation.
 *
 * KEY AGENTS:
 *   - feuding_kinship_groups: Primary target (organized/identity_locked) â bear extraction via resource depletion, mortality, and foregone territorial consolidation.
 *   - royal_authority: Primary beneficiary (institutional/mobile) â captures fiscal and political advantage from the fragmented-polity condition.
 *   - modern_political_economist: Analytical observer â reinterprets the feud as state-formation apparatus.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, 0.78).
domain_priors:suppression_score(feud_obligation_kernel__extraction_cycle_reading, 0.85).
domain_priors:theater_ratio(feud_obligation_kernel__extraction_cycle_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__extraction_cycle_reading, snare).
narrative_ontology:human_readable(feud_obligation_kernel__extraction_cycle_reading, "Blood-Feud Obligations as Destructive Extraction Cycle").
narrative_ontology:topic_domain(feud_obligation_kernel__extraction_cycle_reading, "legal_anthropology/medieval_history/comparative_political_systems").

domain_priors:requires_active_enforcement(feud_obligation_kernel__extraction_cycle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__extraction_cycle_reading, '4e449f71-54b7-4ac7-a9bd-ca62ebdc1ad2').
narrative_ontology:cs_kernel_codification('4e449f71-54b7-4ac7-a9bd-ca62ebdc1ad2', distributed).
narrative_ontology:cs_authority_grounding('4e449f71-54b7-4ac7-a9bd-ca62ebdc1ad2', practice).
narrative_ontology:cs_interpretation_layer_present('4e449f71-54b7-4ac7-a9bd-ca62ebdc1ad2').
narrative_ontology:cs_reading_relation('4e449f71-54b7-4ac7-a9bd-ca62ebdc1ad2', feud_obligation_kernel__stateless_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('4e449f71-54b7-4ac7-a9bd-ca62ebdc1ad2', feud_obligation_kernel__christianized_pacification_reading, coexists_with).
narrative_ontology:cs_axiom('4e449f71-54b7-4ac7-a9bd-ca62ebdc1ad2', foundational, kinship_feud_as_extractive_apparatus).
narrative_ontology:cs_axiom_status(kinship_feud_as_extractive_apparatus, holdable).
narrative_ontology:cs_axiom_grounding('4e449f71-54b7-4ac7-a9bd-ca62ebdc1ad2', kinship_feud_as_extractive_apparatus, empirically_contingent).
narrative_ontology:cs_axiom('4e449f71-54b7-4ac7-a9bd-ca62ebdc1ad2', secondary, state_capacity_over_local_kinship_autonomy).
narrative_ontology:cs_axiom_status(state_capacity_over_local_kinship_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('4e449f71-54b7-4ac7-a9bd-ca62ebdc1ad2', state_capacity_over_local_kinship_autonomy, instrumental).
narrative_ontology:cs_reference_frame('4e449f71-54b7-4ac7-a9bd-ca62ebdc1ad2', kinship_reciprocity_norm).
narrative_ontology:cs_drift_state('4e449f71-54b7-4ac7-a9bd-ca62ebdc1ad2', high_medieval_state_formation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4e449f71-54b7-4ac7-a9bd-ca62ebdc1ad2', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, royal_authority).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, feuding_kinship_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bound by reciprocal obligation to avenge killings and slights, diverting labor, livestock, and manpower into cycles of retaliation rather than productive investment. Mortality and asset depletion are direct costs. Exit is identity-locked: renouncing the feud means renouncing kinship membership, which carries social death and often physical danger from both kin and rivals.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, feuding_kinship_groups, payer,
    organized, generational, identity_locked, regional).

% Derives fiscal and political benefit from the inability of regional kinship groups to consolidate independent territorial power or form cross-cutting alliances. The feud cycle fragments potential rivals and legitimizes the crown's claim to a monopoly over legitimate violence and tax extraction, without needing to provide costly centralized adjudication.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, royal_authority, beneficiary,
    institutional, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feud_obligation_kernel__extraction_cycle_reading, royal_authority).
narrative_ontology:fixing_cost_class(feud_obligation_kernel__extraction_cycle_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claimed coordination function: decentralized grievance resolution and deterrence in the absence of centralized enforcement. Operative function under this reading: channeling kinship labor and assets into sustained reciprocal violence that prevents regional territorial consolidation and secures royal fiscal-military dominance.
% TRANSFER_FUNCTION: Moves productive capacityâlivestock, labor, and livesâfrom feuding kinship groups into the structural advantage of royal authority, via the mechanism of internally enforced violence that precludes autonomous regional state-formation.
% ABSENT_VOICES: Non-combatant kinship membersâwomen, children, and the elderlyâwho bear depleted subsistence and security but are excluded from blood-price negotiations and vengeance councils; also peasant producers whose surplus is diverted to feud costs without voice in kinship or royal assemblies.
% DISAPPEARANCE_RATIONALE: If the obligation vanished, kinship groups would cease enforced reciprocity of violence, productive assets would return to subsistence and investment, regional territorial consolidation would become possible, and royal authority would lose the fragmented-polity conditions that underpin its monopoly claims over taxation and legitimate violence.
% FOUNDING_PROBLEM: The absence of centralized adjudication and enforcement in regions where kinship groups must manage grievances and violent conflict.
% FOUNDING_PROBLEM_CORROBORATION: Modern historical political economy and anthropological critiques from outside the benefiting parties attest that centralizing states leveraged fragmented violence rather than providing alternative justice; royal fiscal records and charter evidence corroborate that monarchs extracted surplus from kinship-depleted regions.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__extraction_cycle_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__extraction_cycle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__extraction_cycle_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(feud_obligation_kernel__extraction_cycle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__extraction_cycle_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the feud systematically converts productive capacity into violence, with mortality and asset destruction as direct transfers. Suppression is higher (0.85) because the constraint persists only by actively suppressing alternativesâroyal courts, ecclesiastical peacemaking, and individual exitâthrough identity-locking and social death. Theater_ratio is moderate (0.45) because the feud carries ritualized performative elements (oaths, ceremonial raids) that increasingly substitute for genuine grievance resolution as royal authority consolidates. Accessibility_collapse is high (0.82): once inside the kinship framework, non-violent alternatives are nearly inaccessible. Resistance is moderate (0.55): some kinship members resist participation, but identity-lock and enforcement keep individual defection rare.
 *
 * PERSPECTIVAL GAP:
 *   The royal_authority seat and the feuding_kinship_groups seat compute to divergent types. From the royal seat, the constraint appears as a favorable background conditionâlow extraction, even beneficialâthat legitimates central taxation and violence monopoly. From the kinship seat, the same constraint reads as high-extraction snare: costs are concentrated, exit is identity-locked, and the coordination story is experienced as coercive cover. The engine derives this divergence from the beneficiary/victim declarations and the exit asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Royal authority is declared beneficiary with mobile exit, placing its derived directionality near the full-beneficiary end; effective extraction is damped or inverted into subsidy. Feuding kinship groups are declared victims with identity_locked exit, placing directionality near the full-target end; effective extraction is amplified by scope and powerlessness. The asymmetry is structural, not perspectival: the same obligation extracts from one seat and subsidizes the other.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâdecentralized grievance resolution in the absence of centralized enforcementâis dead under this reading. Royal authority now possesses the institutional capacity to adjudicate disputes but refrains because the feud's persistence is more extractively valuable. The classification prevents mislabeling the constraint as a living scaffold or rope by requiring victim identification, active enforcement, and the absence of a sunset clause. The R5 mismatch (founding_problem_status: dead Ã disappearance_verdict: world_rearranges) flags the constraint as a zombie/snare rather than a coordination mechanism that has outlived its function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_nature_ambiguity,
    'Is the blood-feud obligation fundamentally a coordination mechanism for stateless justice, or an extractive apparatus for royal authority?',
    'Comparative analysis of feud intensity against state fiscal extraction records; if feud intensity correlates with royal tax yield and anti-consolidation outcomes, the extraction reading is supported; if it correlates with state absence and grievance resolution, the coordination reading is supported.',
    'Determines whether the constraint is classified as snare/extraction or as a hybrid coordination-extraction structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_nature_ambiguity, conceptual, 'Whether the kernel is intrinsically extractive or coordinative.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternatives structuralâroyal law precludes kinship exitâor internalized through kinship identity fusion?',
    'Post-pacification trajectory analysis: if feud obligations persist after royal courts offer accessible alternatives, suppression is partially internalized; if obligations drop immediately, suppression was structural.',
    'Internalized suppression raises effective extraction because targets carry the constraint after structural barriers are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression in kinship obligations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__extraction_cycle_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_extract_tr_t0, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(feud_extract_tr_t10, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(feud_extract_tr_t20, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(feud_extract_tr_t30, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 30, 0.39).
narrative_ontology:measurement(feud_extract_tr_t40, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(feud_extract_tr_t50, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(feud_extract_be_t0, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(feud_extract_be_t10, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(feud_extract_be_t20, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(feud_extract_be_t30, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 30, 0.7).
narrative_ontology:measurement(feud_extract_be_t40, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 40, 0.74).
narrative_ontology:measurement(feud_extract_be_t50, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 50, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(feud_extract_su_t0, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(feud_extract_su_t10, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 10, 0.73).
narrative_ontology:measurement(feud_extract_su_t20, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 20, 0.77).
narrative_ontology:measurement(feud_extract_su_t30, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 30, 0.8).
narrative_ontology:measurement(feud_extract_su_t40, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 40, 0.83).
narrative_ontology:measurement(feud_extract_su_t50, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 50, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
