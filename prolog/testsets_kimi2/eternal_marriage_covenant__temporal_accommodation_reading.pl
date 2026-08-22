% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__temporal_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__temporal_accommodation_reading, []).

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
 *   constraint_id: eternal_marriage_covenant__temporal_accommodation_reading
 *   human_readable: Eternal Marriage Covenant â Temporal Accommodation Reading
 *   domain: religious_law/political_theology/commitment_system
 *
 * SUMMARY:
 *   The 1890 Manifesto issued by the LDS Church suspended the practice of
 *   plural marriage without removing D&C 132 from canon or renouncing its
 *   eternal validity. This reading treats the Manifesto as a temporary
 *   accommodation to federal coercive pressure (the Edmunds-Tucker Act,
 *   threatened disincorporation, imprisonment of leaders), preserving the
 *   doctrinal kernel in a dormant state while prioritizing obedience to the
 *   law of the land. The constraint binds the institutional church and its
 *   membership to a split between eternal doctrine and temporal practice,
 *   generating a commitment-system dynamic where the kernel remains fixed but
 *   its authoritative interpretation is held in abeyance.
 *
 * KEY AGENTS:
 *   - lds_church_institution: Agenda-setter â issues the Manifesto and administers discipline; constrained by federal power but retains interpretive authority over the kernel.
 *   - practicing_believers: Payer â identity-locked to the doctrine but compelled to forsake practice; bear the theological and familial costs of the accommodation.
 *   - federal_government: Beneficiary â achieves legal compliance and territorial integration without continued occupation.
 *   - fundamentalist_dissenters: Excluded â hold the immutable commandment reading, continue practice, are expelled from the institutional conversation.
 *   - theological_historians: Observer â analytical seat examining the drift between doctrinal text and institutional practice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__temporal_accommodation_reading, 0.65).
domain_priors:suppression_score(eternal_marriage_covenant__temporal_accommodation_reading, 0.85).
domain_priors:theater_ratio(eternal_marriage_covenant__temporal_accommodation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__temporal_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__temporal_accommodation_reading, "Eternal Marriage Covenant â Temporal Accommodation Reading").
narrative_ontology:topic_domain(eternal_marriage_covenant__temporal_accommodation_reading, "religious_law/political_theology/commitment_system").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__temporal_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__temporal_accommodation_reading, '8b968e79-1493-4448-8a49-0d41b366d054').
narrative_ontology:cs_kernel_codification('8b968e79-1493-4448-8a49-0d41b366d054', fixed_text).
narrative_ontology:cs_authority_grounding('8b968e79-1493-4448-8a49-0d41b366d054', lineage).
narrative_ontology:cs_interpretation_layer_present('8b968e79-1493-4448-8a49-0d41b366d054').
narrative_ontology:cs_reading_relation('8b968e79-1493-4448-8a49-0d41b366d054', eternal_marriage_covenant__immutable_commandment_reading, coexists_with).
narrative_ontology:cs_reading_relation('8b968e79-1493-4448-8a49-0d41b366d054', eternal_marriage_covenant__prophetic_override_reading, influences).
narrative_ontology:cs_axiom('8b968e79-1493-4448-8a49-0d41b366d054', foundational, eternal_principle_temporarily_suspended_by_civil_law).
narrative_ontology:cs_axiom_status(eternal_principle_temporarily_suspended_by_civil_law, holdable).
narrative_ontology:cs_axiom_grounding('8b968e79-1493-4448-8a49-0d41b366d054', eternal_principle_temporarily_suspended_by_civil_law, theological).
narrative_ontology:cs_axiom('8b968e79-1493-4448-8a49-0d41b366d054', foundational, doctrine_validity_independent_of_practice).
narrative_ontology:cs_axiom_status(doctrine_validity_independent_of_practice, holdable).
narrative_ontology:cs_axiom_grounding('8b968e79-1493-4448-8a49-0d41b366d054', doctrine_validity_independent_of_practice, deontological).
narrative_ontology:cs_reference_frame('8b968e79-1493-4448-8a49-0d41b366d054', eternal_principle_active_practice).
narrative_ontology:cs_drift_state('8b968e79-1493-4448-8a49-0d41b366d054', post_manifesto_federal_pressure_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('8b968e79-1493-4448-8a49-0d41b366d054', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, lds_church_institution).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, federal_government).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, practicing_believers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Through the First Presidency and Quorum of the Twelve, issued the 1890 Manifesto suspending plural marriage practice. Retains D&C 132 as canonical and eternal while directing compliance with federal law to avoid institutional destruction and secure statehood.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, lds_church_institution, agenda_setter,
    institutional, civilizational, constrained, national).

% Theologically committed to plural marriage as a requirement for exaltation, instructed to abandon public and private practice. Must either suppress a core religious practice or break with the institutional church and risk excommunication and federal prosecution.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, practicing_believers, payer,
    moderate, generational, identity_locked, national).

% Secures compliance with anti-polygamy law through church self-enforcement rather than continued federal military or judicial intervention. Achieves territorial integration and legitimizes Utah statehood.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, federal_government, beneficiary,
    institutional, generational, mobile, national).

% Reject the Manifesto as apostasy and continue plural marriage in defiance of both federal law and church discipline. Excommunicated and prosecuted; their perspective is absent from the institutional accommodation.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, fundamentalist_dissenters, excluded,
    powerless, biographical, trapped, local).

% Study the split between doctrinal permanence and practical suspension as an instance of political theology and commitment-system drift under coercive pressure.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, theological_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the survival of the LDS Church as an institution within the United States legal and political system by resolving the immediate conflict between a practice-oriented doctrinal command and federal criminal law.
% TRANSFER_FUNCTION: Moves the public and private practice of plural marriage from active believers into dormancy, transferring compliance and legitimacy from the doctrinal imperative to the federal legal order and institutional church discipline.
% ABSENT_VOICES: Fundamentalist dissenters who continue to hold the immutable commandment reading are excluded from the institutional dialogue; they would argue that the accommodation is apostasy. Posthumous practitioners and plural families prior to 1890 had no voice in the Manifesto's issuance.
% DISAPPEARANCE_RATIONALE: If the temporal accommodation vanished overnight â meaning the church either fully restored plural marriage practice or formally renounced the doctrine â the institutional structure of the LDS Church, its relationship with the U.S. federal government, and the theological self-understanding of its membership would undergo significant reorganization. The current equilibrium depends on the ambiguity.
% FOUNDING_PROBLEM: The LDS Church faced federal disincorporation, seizure of property, and imprisonment of leaders under the Edmunds-Tucker Act and related anti-polygamy legislation, threatening institutional destruction unless plural marriage practice ceased.
% FOUNDING_PROBLEM_CORROBORATION: Federal legislative record and historical scholarship outside the church confirm the enforcement threat ended with Utah statehood in 1896 and the gradual decline of federal prosecution. Church historians and non-Mormon political historians agree the existential threat is no longer live; the arrangement persists beyond its founding crisis.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__temporal_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__temporal_accommodation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__temporal_accommodation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(eternal_marriage_covenant__temporal_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__temporal_accommodation_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__temporal_accommodation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__temporal_accommodation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__temporal_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   High initial suppression (0.85) reflects the combined federal and ecclesiastical enforcement at the founding moment. Extractiveness is substantial (0.65) because the constraint requires believers to suppress a practice they regard as salvific, creating an identity-locked cost. Theater ratio peaks around the Second Manifesto and Reed Smoot hearings (1904â1906) as public disavowal became more performative relative to private belief. Over the interval, base_extractiveness declines modestly as generational replacement reduces the pool of believers with lived attachment to the practice, though the doctrinal dormancy persists.
 *
 * PERSPECTIVAL GAP:
 *   The institutional seat (church leadership) experiences the constraint as a necessary survival mechanism â a painful but justified coordination with state power. The practicing believer seat experiences it as an extraction of religious obligation under duress: they must choose between their theological identity and institutional membership. The federal seat experiences it as successful compliance. The excluded fundamentalist seat experiences it as apostasy. These divergences are structurally determined by power and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The church institution sits near the beneficiary end (d low): it gains survival and property retention. The federal government also sits near the beneficiary end (d low): it gains compliance without cost. Practicing believers sit near the target end (d high): they bear the cost of suppressed practice and identity tension. Fundamentalist dissenters sit at the extreme target end (trapped/identity_locked). The engine will compute high effective extraction for the believer and dissenter seats, low or negative extraction for the institutional and federal seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â existential federal threat â was resolved by 1896 (Utah statehood) and was certainly dead by the 1930s. Yet the constraint persists: the doctrine remains canonized but unpracticed, and the church continues to discipline members who attempt restoration. This is a classic mandatrophy pattern. The classification as tangled_rope captures the genuine coordination (survival) that was its original function, while the temporal measurements show declining extraction and suppression as the threat fades. The persistence after the threat's death is what would push a piton classification if viewed solely from the endpoint; the claim of tangled_rope preserves the historical structure, and the measurements allow the engine to detect the drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    temporal_accommodation_stability,
    'Does the temporal accommodation reading remain structurally stable under conditions of removed federal pressure, or does prolonged dormancy functionally collapse into either the prophetic override reading (de facto abolition) or the immutable commandment reading (underground restoration)?',
    'Historical analysis of post-1950 church discourse and practice: if no restoration occurs despite political feasibility, the reading drifts toward prophetic override; if restoration movements gain institutional traction, it drifts toward immutable commandment.',
    'Determines whether the constraint is a genuine tangled rope with live coordination or a piton/snare where the coordination function has atrophied.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_accommodation_stability, conceptual, 'Stability of the temporal accommodation reading against its sibling readings over time').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of plural marriage practice maintained primarily by federal legal coercion, church disciplinary coercion, or internalized identity fusion among members?',
    'Comparative analysis of jurisdictions without federal prohibition and post-exit member trajectories; if suppression persists after legal threat removal, reclassify as partially internalized.',
    'If internalized, effective extraction exceeds structural suppression measure; the constraint persists even if legal threat is removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    doctrine_dormancy_naturality,
    'Is the doctrinal dormancy a natural feature of theological development or a constructed constraint benefiting institutional survival?',
    'Comparative political theology examining how churches under similar existential threat treat contested doctrines â whether they formally renounce, reaffirm, or suspend without renunciation.',
    'If constructed primarily for survival, the coordination story is cover and the constraint leans snare; if genuine theological development, the coordination function is primary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrine_dormancy_naturality, conceptual, 'Whether doctrinal dormancy is natural theological evolution or institutional construction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__temporal_accommodation_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t0, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(eter_tr_t10, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 10, 0.5).
narrative_ontology:measurement(eter_tr_t20, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 20, 0.7).
narrative_ontology:measurement(eter_tr_t30, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 30, 0.65).
narrative_ontology:measurement(eter_tr_t40, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 40, 0.55).
narrative_ontology:measurement(eter_tr_t50, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 50, 0.45).
narrative_ontology:measurement(eter_tr_t60, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 60, 0.35).

% Extraction over time
narrative_ontology:measurement(eter_be_t0, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(eter_be_t10, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(eter_be_t20, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 20, 0.72).
narrative_ontology:measurement(eter_be_t30, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(eter_be_t40, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(eter_be_t50, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 50, 0.5).
narrative_ontology:measurement(eter_be_t60, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 60, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t0, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(eter_su_t10, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(eter_su_t20, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 20, 0.9).
narrative_ontology:measurement(eter_su_t30, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 30, 0.85).
narrative_ontology:measurement(eter_su_t40, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(eter_su_t50, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 50, 0.5).
narrative_ontology:measurement(eter_su_t60, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 60, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__temporal_accommodation_reading, identity_coordination).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, immutable_commandment_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, prophetic_override_reading).

% DUAL FORMULATION NOTE:
% The eternal_marriage_covenant kernel decomposes into three structurally distinct constraints: immutable_commandment_reading (treats the doctrine as eternally active regardless of civil law), prophetic_override_reading (treats the Manifesto as a revelatory supersession), and temporal_accommodation_reading (this file, treating the Manifesto as a temporary legal accommodation). Each has a distinct epsilon, beneficiary/victim structure, and directional logic. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
