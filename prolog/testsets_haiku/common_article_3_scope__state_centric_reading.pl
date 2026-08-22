% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__state_centric_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: common_article_3_scope__state_centric_reading
 *   human_readable: Common Article 3 Scope Threshold (State-Centric Reading)
 *   domain: legal/humanitarian
 *
 * SUMMARY:
 *   The state-centric reading of Common Article 3 scope anchors humanitarian
 *   protection to intensity and organization thresholds applied by the state
 *   itself. Under this reading, CA3 protections apply only to armed conflicts
 *   serious enough and organized enough to justify a separate legal regime;
 *   low-intensity violence, gang activity, and sporadic armed clashes are
 *   governed by domestic law enforcement standards instead. This reading
 *   preserves state sovereignty over conflict classification and maximizes
 *   governmental operational discretion. The ICRC and human rights bodies
 *   increasingly contest this interpretation, advocating for lower or
 *   eliminated thresholds, but the state-centric reading remains
 *   institutionalized in many national military doctrines and state practice.
 *
 * KEY AGENTS:
 *   - State military apparatus: interprets and applies the threshold; retains classification authority
 *   - Irregular combatants below threshold: excluded from CA3 protections; treated as law enforcement targets
 *   - Civilian victims in excluded conflicts: receive no humanitarian protection guarantees under this reading
 *   - Non-state armed groups: cannot claim CA3 status independent of state classification
 *   - ICRC monitors: observe and advise but lack binding authority over threshold determinations
 *   - International humanitarian law community: advocates for broader application but excluded from legal authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__state_centric_reading, 0.68).
domain_priors:suppression_score(common_article_3_scope__state_centric_reading, 0.79).
domain_priors:theater_ratio(common_article_3_scope__state_centric_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__state_centric_reading, "Common Article 3 Scope Threshold (State-Centric Reading)").
narrative_ontology:topic_domain(common_article_3_scope__state_centric_reading, "legal/humanitarian").

domain_priors:requires_active_enforcement(common_article_3_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__state_centric_reading, 'ad23da6d-fa1a-4bb6-a001-7d459335e3a1').
narrative_ontology:cs_kernel_codification('ad23da6d-fa1a-4bb6-a001-7d459335e3a1', fixed_text).
narrative_ontology:cs_authority_grounding('ad23da6d-fa1a-4bb6-a001-7d459335e3a1', extraction).
narrative_ontology:cs_interpretation_layer_present('ad23da6d-fa1a-4bb6-a001-7d459335e3a1').
narrative_ontology:cs_reading_relation('ad23da6d-fa1a-4bb6-a001-7d459335e3a1', common_article_3_scope__expansive_human_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('ad23da6d-fa1a-4bb6-a001-7d459335e3a1', common_article_3_scope__icrc_customary_reading, influences).
narrative_ontology:cs_axiom('ad23da6d-fa1a-4bb6-a001-7d459335e3a1', foundational, state_unilateral_threshold_authority).
narrative_ontology:cs_axiom_status(state_unilateral_threshold_authority, holdable).
narrative_ontology:cs_axiom_grounding('ad23da6d-fa1a-4bb6-a001-7d459335e3a1', state_unilateral_threshold_authority, instrumental).
narrative_ontology:cs_axiom('ad23da6d-fa1a-4bb6-a001-7d459335e3a1', secondary, intensity_organization_boundary_necessity).
narrative_ontology:cs_axiom_status(intensity_organization_boundary_necessity, holdable).
narrative_ontology:cs_axiom_grounding('ad23da6d-fa1a-4bb6-a001-7d459335e3a1', intensity_organization_boundary_necessity, deontological).
narrative_ontology:cs_reference_frame('ad23da6d-fa1a-4bb6-a001-7d459335e3a1', state_sovereign_conflict_classification).
narrative_ontology:cs_drift_state('ad23da6d-fa1a-4bb6-a001-7d459335e3a1', contemporary_human_rights_advocacy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ad23da6d-fa1a-4bb6-a001-7d459335e3a1', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__state_centric_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, state_military_apparatus).
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, governmental_actors).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, irregular_combatants_below_threshold).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, civilian_victims_in_excluded_conflicts).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, non_state_armed_groups).
narrative_ontology:constraint_vindicates(common_article_3_scope__state_centric_reading, state_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(common_article_3_scope__state_centric_reading, armed_conflict_vs_law_enforcement_distinction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates under CA3 constraints only when conflict intensity and organization meet the threshold. Below the threshold, state military personnel treat operations as law enforcement without humanitarian law protections. The state interprets and applies the threshold itself, determining which conflicts qualify. Retains full discretion to classify encounters as law enforcement rather than armed conflict.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, state_military_apparatus, agenda_setter,
    institutional, generational, trapped, national).

% Benefit from the threshold mechanism by maintaining operational flexibility in counterinsurgency, counter-terrorism, and policing. Governments can apply different rules and scrutiny levels to the same population depending on how the conflict is classified. The threshold preserves governmental authority over conflict classification without external verification.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, governmental_actors, beneficiary,
    institutional, generational, trapped, national).

% Engage in armed resistance or organized violence that states classify as below the intensity/organization threshold. Receive no CA3 protections; they are treated as law enforcement targets rather than combatants. No claim to prisoner-of-war status, medical care minimums, or distinction protections. Their exclusion from the victim set is the mechanism through which suppression operates.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, irregular_combatants_below_threshold, payer,
    powerless, immediate, trapped, local).

% Located in conflicts classified as below the threshold and receive no humanitarian protection guarantees. Civilian-military distinction, proportionality, and precaution obligations that CA3 mandates do not apply in these classified contexts. Their exclusion depends on the state's threshold determination.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, civilian_victims_in_excluded_conflicts, payer,
    powerless, immediate, trapped, local).

% Operate in conflicts the state may classify as below threshold. Some groups are militarily organized but excluded from CA3 because the state determines the conflict does not meet intensity criteria. Groups cannot independently claim CA3 status; the state's classification is determinative. Their identity as armed organizations provides no automatic legal standing.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, non_state_armed_groups, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__state_centric_reading, non_state_armed_groups, excluded).

% Monitors humanitarian compliance and advises on CA3 applicability. Under the state-centric reading, the ICRC's view on threshold application is advisory; states retain authority to determine scope. The monitoring function is included as theater: ICRC presence is cited to show humanitarian attention while states make final classification decisions.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, icrc_monitors, observer,
    organized, generational, mobile, national).

% International legal scholars, human rights organizations, and treaty bodies advocate for lower or eliminated thresholds. Their arguments are heard in treaty body reviews and academic forums but carry no binding authority in state practice. The state-centric reading excludes their threshold determinations from legal effect.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, international_humanitarian_law_community, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_article_3_scope__state_centric_reading, state_military_apparatus).
narrative_ontology:fixing_cost_class(common_article_3_scope__state_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a recognized boundary between armed conflict and law enforcement, enabling states to apply distinct legal regimes to different situations. The threshold attempts to identify conflicts serious enough to warrant international humanitarian law's more elaborate protections while preserving law enforcement authority over low-intensity situations.
% TRANSFER_FUNCTION: Transfers decision-making authority over humanitarian protection standards from international bodies and humanitarian doctrine to individual states. States gain operational discretion to exclude populations from CA3 protections by classifying their situations as below threshold. Irregular combatants and civilians in excluded conflicts bear the cost of reduced protections.
% ABSENT_VOICES: International humanitarian law advocates, ICRC legal positions arguing for broader CA3 application, human rights monitoring bodies that contest threshold determinations, and the irregular combatants and civilians themselves — whose status depends entirely on the state's classification and who have no formal voice in making that determination.
% DISAPPEARANCE_RATIONALE: If this threshold constraint vanished, CA3 would either apply to all organized armed violence (expansive reading) or be determined by customary international law evolution (ICRC reading). States would lose the authority to unilaterally exclude conflicts from humanitarian protections. Military operations would require justification under humanitarian standards in situations now classified as law enforcement.
% FOUNDING_PROBLEM: Distinguish armed conflict from law enforcement so that the international humanitarian law regime applies to the appropriate situations and does not over-regulate police or counter-terrorism operations that do not rise to armed conflict intensity.
% FOUNDING_PROBLEM_CORROBORATION: States attest the problem remains live: low-intensity insurgencies, gang violence, and localized armed clashes require legal clarity on applicability. Human rights organizations and the ICRC attest the founding problem is partly addressed but the state-centric solution overstates law enforcement and understates humanitarian obligations; international humanitarian law scholars document the doctrinal contestation across the three readings.
narrative_ontology:disappearance_verdict(common_article_3_scope__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__state_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(common_article_3_scope__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__state_centric_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_article_3_scope__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_article_3_scope__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderately high (0.68 at interval end) because the threshold mechanism systematically excludes populations from humanitarian protections in contexts states classify as law enforcement. Suppression is high (0.79) because maintaining the threshold requires active exclusion of irregular combatants and their advocates from the victim set and from threshold-determination authority. The constraint's suppression does not diminish over the interval because state practice and military doctrine embed the state-centric classification mechanism institutionally. Theater ratio is moderate (0.41) because the ICRC's advisory presence and humanitarian monitoring create appearance of international oversight while states retain unilateral classification authority; humanitarian legitimacy is preserved through institutional theater while operational discretion expands. The measurement grid captures the initial period of threshold contestation (years 0-12) and the subsequent plateau (years 18-26) where the state-centric reading becomes consolidated in doctrine despite ICRC and scholarly criticism.
 *
 * PERSPECTIVAL GAP:
 *   State military and governmental seats perceive the threshold as a legitimate coordination mechanism that preserves law-enforcement authority and avoids over-applying humanitarian constraints. Irregular combatants and civilians in excluded conflicts perceive the same mechanism as a suppressive filter that denies them protections based on unilateral state determination. The engine computes these divergent classifications from the same structural data: high suppression on the payer side, beneficiary extraction on the state side. The perspectival gap is not a measurement error—it reflects the asymmetric power to define what the constraint applies to.
 *
 * DIRECTIONALITY LOGIC:
 *   State military and governmental actors are beneficiaries (d near 0.0): they gain operational discretion and avoid constraint application. Their power is institutional and their exit is trapped (they cannot leave the constraint system they define). Irregular combatants and civilians in excluded conflicts are targets (d near 1.0): they bear the cost of reduced protections and have no say in threshold determination. Their power is powerless and their exit is trapped (they cannot change the state's classification). The ICRC and international humanitarian law community are partially excluded (their arguments are heard but overridden). The directionality derivation reflects structural asymmetry: the state defines the constraint's scope for others while exempting itself from broader humanitarian obligations.
 *
 * MANDATROPHY ANALYSIS:
 *   The state-centric reading exhibits mandatrophy trajectory. The founding problem—distinguishing armed conflict from law enforcement—remains live at the policy level, but the constraint's function has drifted toward preserving state operational discretion rather than solving the coordination problem. The ICRC's customary-law tracking and the expansive human rights reading both propose alternative threshold mechanisms, yet the state-centric reading persists because states collectively benefit from retaining classification authority. This is tangled rope (genuine coordination function + asymmetric extraction + active enforcement), not snare (the coordination problem is real), but the extraction component grows over time as military doctrine increasingly relies on threshold classification to conduct operations that would be restricted under humanitarian law. The classification mechanism is enforced not through explicit coercion but through institutional authority and the exclusion of non-state voices from determination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_determinability,
    'Can the intensity and organization thresholds be applied objectively, or do they inevitably embed discretionary judgment that states exploit?',
    'Comparative case analysis of threshold application in reported state practice and international court decisions. Track whether states apply consistent standards across similar conflicts or whether classification tracks strategic interest.',
    'If thresholds are objectively determinable, the state-centric reading preserves legitimate state authority. If application is systematically discretionary, the constraint functions as pure suppression (reclassifies toward snare). If mixed, the constraint is a hybrid where legitimate coordination rides on extractive classification authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_determinability, empirical, 'Whether threshold application is objective or discretionary.').

omega_variable(
    reading_foreclosure_possibility,
    'Can the state-centric reading and the expansive_human_rights_reading both be held within a single legal framework, or does the state-centric reading''s core claim (unilateral state determination) logically foreclose the expansive reading''s core claim (universal floor of protections)?',
    'Doctrinal analysis of treaty law and customary international law. Can a state simultaneously assert unilateral threshold authority AND accept binding humanitarian floors set externally? Or are these mutually exclusive commitments?',
    'If mutually exclusive (forecloses), the reading relations should change from coexists_with to forecloses. If compatible (coexists), the readings are truly held by different parties as live positions. The engine computes foreclosure from axiom contradiction; this omega clarifies whether the logical structure supports that computation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_possibility, conceptual, 'Whether state-centric and expansive readings logically foreclose each other or coexist as live positions.').

omega_variable(
    suppression_internalization,
    'Is the high measured suppression (0.79) structural (external barriers—states exclude populations from determining threshold) or internalized (irregular combatants accept that states have the authority to classify)?',
    'Post-threshold-change analysis: if a state adopts a lower threshold or international court imposes one, do populations previously excluded resume engagement in legal processes, or does acceptance of state authority persist despite the formal change?',
    'If structural, removing the threshold constraint would immediately restore suppressed voices. If internalized, suppression persists even after structural removal because populations have adopted the state''s authority frame. If both, the effective suppression is higher than the structural measure suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether measured suppression is structural or internalized in irregular combatants'' authority acceptance.').

omega_variable(
    kernel_codification_status,
    'Is the Common Article 3 kernel formalized (the text of CA3 itself) or does the kernel include the unwritten intensity/organization boundary that the three readings dispute?',
    'Determine the scope of the kernel: does it include only the published text of CA3, or does it include the customary international law surrounding scope? The answer affects whether the three readings are interpretations of a single fixed text or readings of a partially implicit kernel.',
    'If the kernel is text-only, the readings are interpretive disputes about unstated boundaries and the codification is formalized but incomplete. If the kernel is text + customary scope, the codification is distributed (no single authoritative source) and the readings are competing customary determinations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_codification_status, conceptual, 'Scope of the Common Article 3 kernel: text-only or text+customary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__state_centric_reading, 0, 26).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_article_3_scope__state_centric_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(comm_tr_t4, common_article_3_scope__state_centric_reading, theater_ratio, 4, 0.35).
narrative_ontology:measurement(comm_tr_t8, common_article_3_scope__state_centric_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement(comm_tr_t12, common_article_3_scope__state_centric_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement(comm_tr_t18, common_article_3_scope__state_centric_reading, theater_ratio, 18, 0.41).
narrative_ontology:measurement(comm_tr_t26, common_article_3_scope__state_centric_reading, theater_ratio, 26, 0.41).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_article_3_scope__state_centric_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(comm_be_t4, common_article_3_scope__state_centric_reading, base_extractiveness, 4, 0.62).
narrative_ontology:measurement(comm_be_t8, common_article_3_scope__state_centric_reading, base_extractiveness, 8, 0.65).
narrative_ontology:measurement(comm_be_t12, common_article_3_scope__state_centric_reading, base_extractiveness, 12, 0.67).
narrative_ontology:measurement(comm_be_t18, common_article_3_scope__state_centric_reading, base_extractiveness, 18, 0.69).
narrative_ontology:measurement(comm_be_t26, common_article_3_scope__state_centric_reading, base_extractiveness, 26, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_article_3_scope__state_centric_reading, suppression_requirement, 0, 0.71).
narrative_ontology:measurement(comm_su_t4, common_article_3_scope__state_centric_reading, suppression_requirement, 4, 0.74).
narrative_ontology:measurement(comm_su_t8, common_article_3_scope__state_centric_reading, suppression_requirement, 8, 0.76).
narrative_ontology:measurement(comm_su_t12, common_article_3_scope__state_centric_reading, suppression_requirement, 12, 0.78).
narrative_ontology:measurement(comm_su_t18, common_article_3_scope__state_centric_reading, suppression_requirement, 18, 0.79).
narrative_ontology:measurement(comm_su_t26, common_article_3_scope__state_centric_reading, suppression_requirement, 26, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__state_centric_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(common_article_3_scope__state_centric_reading, 0.12).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, common_article_3_scope__expansive_human_rights_reading).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, common_article_3_scope__icrc_customary_reading).

% DUAL FORMULATION NOTE:
% The Common Article 3 scope kernel decomposes into three readings with structurally distinct ε values and victim sets. The state-centric reading excludes irregular combatants below the intensity/organization threshold from protection, yielding high extractiveness (0.68). The expansive reading applies CA3 to all organized armed violence, yielding lower extractiveness and broader victim coverage. The ICRC reading delegates scope determination to customary international law evolution, yielding intermediate extractiveness depending on opinio juris trajectory. Each reading shares the CA3 text but instantiates different constraints by disagreeing on the scope-determining boundary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(common_article_3_scope__state_centric_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
