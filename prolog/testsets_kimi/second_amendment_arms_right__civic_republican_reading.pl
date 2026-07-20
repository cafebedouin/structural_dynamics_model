% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__civic_republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__civic_republican_reading, []).

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
 *   constraint_id: second_amendment_arms_right__civic_republican_reading
 *   human_readable: Second Amendment Civic Republican Reading
 *   domain: constitutional law / political philosophy
 *
 * SUMMARY:
 *   The civic republican reading of the Second Amendment holds that the right
 *   to keep and bear arms protects armed citizenship as a prerequisite for
 *   republican self-governance, positioning the individual neither as a
 *   purely autonomous rights-bearer nor as a subordinate of state militia
 *   authority. It generates a constraint on regulatory power that conditions
 *   arms-bearing on civic participation and militia-readiness, creating a
 *   hybrid structure with genuine coordination function (distributing defense
 *   responsibility across the citizenry) and asymmetric extraction
 *   (constraining local democratic majorities and concentrating public-safety
 *   costs on disarmed communities).
 *
 * KEY AGENTS:
 *   - citizen_militia_members: Primary beneficiary (moderate/identity_locked) â receive constitutional shelter and civic status
 *   - federal_judiciary: Agenda setter (institutional/analytical) â enforces the reading
 *   - local_governments: Primary payer (moderate/constrained) â lose regulatory autonomy
 *   - gun_violence_affected_communities: Secondary payer (powerless/trapped) â bear safety externalities
 *   - individual_rights_absolutists: Excluded voice (organized/constrained) â libertarian reading marginalized
 *   - constitutional_historians: Analytical observer (moderate/analytical) â supply epistemic substrate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__civic_republican_reading, 0.48).
domain_priors:suppression_score(second_amendment_arms_right__civic_republican_reading, 0.55).
domain_priors:theater_ratio(second_amendment_arms_right__civic_republican_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__civic_republican_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_arms_right__civic_republican_reading, "Second Amendment Civic Republican Reading").
narrative_ontology:topic_domain(second_amendment_arms_right__civic_republican_reading, "constitutional law / political philosophy").

domain_priors:requires_active_enforcement(second_amendment_arms_right__civic_republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__civic_republican_reading, 'c499d4fd-a9e1-4aa5-abca-98db0da3ef53').
narrative_ontology:cs_kernel_codification('c499d4fd-a9e1-4aa5-abca-98db0da3ef53', fixed_text).
narrative_ontology:cs_authority_grounding('c499d4fd-a9e1-4aa5-abca-98db0da3ef53', lineage).
narrative_ontology:cs_interpretation_layer_present('c499d4fd-a9e1-4aa5-abca-98db0da3ef53').
narrative_ontology:cs_reading_relation('c499d4fd-a9e1-4aa5-abca-98db0da3ef53', second_amendment_arms_right__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('c499d4fd-a9e1-4aa5-abca-98db0da3ef53', second_amendment_arms_right__collective_right_reading, coexists_with).
narrative_ontology:cs_axiom('c499d4fd-a9e1-4aa5-abca-98db0da3ef53', foundational, armed_citizenship_as_republican_prerequisite).
narrative_ontology:cs_axiom_status(armed_citizenship_as_republican_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('c499d4fd-a9e1-4aa5-abca-98db0da3ef53', armed_citizenship_as_republican_prerequisite, deontological).
narrative_ontology:cs_axiom('c499d4fd-a9e1-4aa5-abca-98db0da3ef53', foundational, civic_duty_of_arms_bearing).
narrative_ontology:cs_axiom_status(civic_duty_of_arms_bearing, holdable).
narrative_ontology:cs_axiom_grounding('c499d4fd-a9e1-4aa5-abca-98db0da3ef53', civic_duty_of_arms_bearing, conventional).
narrative_ontology:cs_reference_frame('c499d4fd-a9e1-4aa5-abca-98db0da3ef53', civic_republican_founding).
narrative_ontology:cs_drift_state('c499d4fd-a9e1-4aa5-abca-98db0da3ef53', post_heller_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c499d4fd-a9e1-4aa5-abca-98db0da3ef53', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, citizen_militia_members).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, local_governments).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, gun_violence_affected_communities).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__civic_republican_reading, civic_republican_governance_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__civic_republican_reading, armed_citizenship_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear arms under constitutional protection framed as civic duty to republic; participate in militia training and qualification regimes; their ownership is legitimized by service to collective self-governance rather than pure individual preference.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, citizen_militia_members, beneficiary,
    moderate, generational, identity_locked, national).

% Interprets the Second Amendment through civic republican historiography; enforces limits on federal and state regulation that would disarm the citizenry outside militia context; sets doctrinal boundaries between permissible training requirements and prohibitory bans.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, federal_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% Enact public safety ordinances that are preempted or struck down when courts adopt the civic republican reading; lose regulatory autonomy to restrict arms possession in their jurisdictions; must redirect enforcement toward qualification and training frameworks rather than prohibition.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, local_governments, payer,
    moderate, biographical, constrained, local).

% Bear the public health and safety externalities of widespread arms possession in their neighborhoods; lack effective exit from violence exposure because regulatory tools are constitutionally constrained; their policy preferences for disarmament are structurally blocked.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, gun_violence_affected_communities, payer,
    powerless, immediate, trapped, local).

% Advocate for a libertarian individual right untethered to militia service or civic duty; their reading is structurally excluded from the civic republican framework because the latter conditions the right on participation in republican self-governance.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, individual_rights_absolutists, excluded,
    organized, generational, constrained, national).

% Provide the historical record of civic republican thought and founding-era militia practice that the judiciary draws on; they do not collect from or pay into the constraint but supply the epistemic substrate for its interpretation.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, constitutional_historians, observer,
    moderate, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the arming and training of the citizen body so that a militia capable of republican self-defense can be mustered without standing-army dependency; solves the collective-action problem of military preparedness by distributing arms-bearing across the populace as civic obligation.
% TRANSFER_FUNCTION: Moves regulatory authority over arms from local democratic majorities to a constitutionally protected civic practice; transfers public safety costs from armed citizenry to violence-exposed communities while transferring political status and constitutional shelter to militia participants.
% ABSENT_VOICES: Individual rights absolutists who reject any civic duty framing, and collective-right statists who would limit arms-bearing to formal state militias under government command, are both present in broader discourse but structurally marginalized within the civic republican interpretive framework.
% DISAPPEARANCE_RATIONALE: If the civic republican reading vanished overnight, federal courts would lose the doctrinal framework that justifies training requirements and militia-linked ownership; local governments would regain broader regulatory authority; the armed citizenry's constitutional shelter would narrow to individual-right or collective-right logics, reorganizing gun policy around libertarian or statist premises.
% FOUNDING_PROBLEM: The founding generation sought to avoid a large standing army as a threat to republican liberty while ensuring collective security, by relying on an armed citizenry mustered in militias.
% FOUNDING_PROBLEM_CORROBORATION: Civic republican historians and some constitutional scholars attest the problem is historically grounded; gun violence researchers and public health authorities attest the problem context has transformed such that militia-based security is anachronistic, and the arrangement now serves a different function; no neutral party attests unanimity on status.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__civic_republican_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__civic_republican_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__civic_republican_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_arms_right__civic_republican_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__civic_republican_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__civic_republican_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_arms_right__civic_republican_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_arms_right__civic_republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48) because the constraint extracts regulatory autonomy and imposes safety externalities, but it also delivers a real coordination good (militia readiness). Suppression is moderate-high (0.55) because the constraint's persistence depends on judicial enforcement that preempts democratic alternatives; it is not a natural law. Theater is moderate (0.32): early scholarship was highly performative, but the reading acquired substantive doctrinal content before being partially displaced by Heller. Accessibility collapse (0.58) reflects that outright arms bans are collapsed as alternatives, but regulatory training and qualification frameworks remain available. Resistance (0.62) is substantial from gun-control advocates and local governments. The measurement series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (local governments, affected communities) experience the constraint as judicially enforced extraction that preempts their policy preferences, while the beneficiary seat experiences it as the restoration of republican civic duty. The agenda-setter seat (federal judiciary) experiences it as historically grounded interpretation. The engine computes this divergence from power and exit asymmetry: trapped, powerless communities face amplified effective extraction, while identity-locked militia members experience damped or inverted extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   citizen_militia_members are structural beneficiaries (low directionality) because the constraint subsidizes their arms-bearing by constitutionalizing it and shielding it from prohibition. local_governments and gun_violence_affected_communities are structural targets (high directionality) because the constraint extracts regulatory autonomy and imposes uncompensated safety costs. federal_judiciary sits near symmetric: it administers the constraint but does not personally collect or pay. individual_rights_absolutists are excluded, not targeted â their exit is constrained by the judicial monopoly on constitutional meaning.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope rather than snare prevents misreading the genuine coordination function â militia preparedness and distributed defense â as purely cover for arms-lobby extraction. The presence of a real coordination function (citizen militia as substitute for standing army) and the dual beneficiary-duty structure keep it out of snare. Classifying it as rope rather than tangled_rope would ignore the identifiable victims (preempted localities, violence-exposed communities) and the active judicial enforcement required to maintain the asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    civic_republican_reading_contest,
    'Does the civic republican reading survive as a live constitutional interpretation, or has it been functionally displaced by the individual right reading?',
    'Judicial adoption rate: tracking whether federal courts cite civic republican reasoning versus individual liberty reasoning in Second Amendment jurisprudence.',
    'If displaced, this constraint''s effective epsilon shifts toward the individual right reading''s profile; if live, the moderate epsilon on training and qualification requirements remains structurally grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civic_republican_reading_contest, empirical, 'Whether civic republican reading remains live in adjudication').

omega_variable(
    coordination_extraction_boundary_militia,
    'Are training and qualification requirements genuine civic republican coordination (preparing citizens for militia service) or disguised extraction (selective disarmament of non-compliant populations)?',
    'Empirical analysis of qualification regimes: do they expand competent militia participation or functionally exclude marginalized groups from arms bearing?',
    'If exclusionary, the constraint shifts toward snare-like extraction; if genuinely competency-building, it remains tangled rope with low extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary_militia, empirical, 'Whether training requirements coordinate or extract').

omega_variable(
    militia_identity_modern_context,
    'Can the civic republican conception of militia-centered armed citizenship be operationalized in a modern military context where organized defense is state-professionalized?',
    'Comparative constitutional analysis of nations with civic republican traditions (e.g., Switzerland) versus the U.S. professional military context.',
    'If the militia concept is inoperable today, the civic republican reading becomes a historical anachronism (piton candidate); if operable, it sustains coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(militia_identity_modern_context, conceptual, 'Modern operability of militia-based civic republicanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__civic_republican_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_arms_right__civic_republican_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(seco_tr_t8, second_amendment_arms_right__civic_republican_reading, theater_ratio, 8, 0.4).
narrative_ontology:measurement(seco_tr_t16, second_amendment_arms_right__civic_republican_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(seco_tr_t24, second_amendment_arms_right__civic_republican_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement(seco_tr_t32, second_amendment_arms_right__civic_republican_reading, theater_ratio, 32, 0.31).
narrative_ontology:measurement(seco_tr_t40, second_amendment_arms_right__civic_republican_reading, theater_ratio, 40, 0.32).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(seco_be_t8, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(seco_be_t16, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(seco_be_t24, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 24, 0.44).
narrative_ontology:measurement(seco_be_t32, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 32, 0.46).
narrative_ontology:measurement(seco_be_t40, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 40, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(seco_su_t8, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(seco_su_t16, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(seco_su_t24, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(seco_su_t32, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 32, 0.55).
narrative_ontology:measurement(seco_su_t40, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__civic_republican_reading, identity_coordination).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, collective_right_reading).

% DUAL FORMULATION NOTE:
% The Second Amendment arms right kernel decomposes into three structurally distinct readings: civic_republican_reading (this file), individual_right_reading, and collective_right_reading. Each has distinct epsilon, beneficiaries, and victim structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
