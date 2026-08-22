% ============================================================================
% CONSTRAINT STORY: aneyoshi_land_use_prohibition__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_land_use_prohibition__commemorative_husk_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: aneyoshi_land_use_prohibition__commemorative_husk_reading
 *   human_readable: Aneyoshi Tsunami Stone Commemorative Husk
 *   domain: disaster_anthropology/commitment_systems/temporal_institutional_analysis
 *
 * SUMMARY:
 *   The Aneyoshi tsunami stone in Iwate Prefecture, Japan, warns 'Do not
 *   build below this point.' In this reading, the inscription has decayed
 *   from an operational land-use prohibition into a commemorative monument
 *   without behavioral force. Development interests treat the line as
 *   non-binding, constructing residential and commercial property in the zone
 *   below the stone. Future residents bear the catastrophic risk. The
 *   constraint is the institutionalized huskâthe stone maintained as
 *   heritage while its protective function has atrophied. The claimed type of
 *   Piton captures the inertial, theatrical nature of the remaining
 *   institution, even though the structural data (beneficiaries, victims,
 *   rising extraction) create tension with the pure Piton profile. That
 *   tension is the signal the engine is designed to detect.
 *
 * KEY AGENTS:
 *   - Municipal government: agenda_setter (institutional/constrained) â administers heritage designation and land-use permits.
 *   - Real estate developers: primary beneficiary (powerful/mobile) â profit from construction below the line.
 *   - Future residents below the line: primary payer (powerless/trapped) â bear tsunami risk without recourse.
 *   - Disaster anthropologist: observer (analytical/analytical) â documents the decay trajectory.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.72).
domain_priors:suppression_score(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.35).
domain_priors:theater_ratio(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.88).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 0.88).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_land_use_prohibition__commemorative_husk_reading, piton).
narrative_ontology:human_readable(aneyoshi_land_use_prohibition__commemorative_husk_reading, "Aneyoshi Tsunami Stone Commemorative Husk").
narrative_ontology:topic_domain(aneyoshi_land_use_prohibition__commemorative_husk_reading, "disaster_anthropology/commitment_systems/temporal_institutional_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_land_use_prohibition__commemorative_husk_reading, '685c9525-79ba-441a-ab9b-c79aac06c76b').
narrative_ontology:cs_kernel_codification('685c9525-79ba-441a-ab9b-c79aac06c76b', fixed_text).
narrative_ontology:cs_authority_grounding('685c9525-79ba-441a-ab9b-c79aac06c76b', practice).
narrative_ontology:cs_interpretation_layer_present('685c9525-79ba-441a-ab9b-c79aac06c76b').
narrative_ontology:cs_reading_relation('685c9525-79ba-441a-ab9b-c79aac06c76b', aneyoshi_land_use_prohibition__behavioral_competence_reading, forecloses).
narrative_ontology:cs_axiom('685c9525-79ba-441a-ab9b-c79aac06c76b', foundational, prohibition_has_no_binding_force).
narrative_ontology:cs_axiom_status(prohibition_has_no_binding_force, holdable).
narrative_ontology:cs_axiom_grounding('685c9525-79ba-441a-ab9b-c79aac06c76b', prohibition_has_no_binding_force, empirically_contingent).
narrative_ontology:cs_axiom('685c9525-79ba-441a-ab9b-c79aac06c76b', foundational, commemorative_preservation_as_sufficient_duty).
narrative_ontology:cs_axiom_status(commemorative_preservation_as_sufficient_duty, holdable).
narrative_ontology:cs_axiom_grounding('685c9525-79ba-441a-ab9b-c79aac06c76b', commemorative_preservation_as_sufficient_duty, conventional).
narrative_ontology:cs_reference_frame('685c9525-79ba-441a-ab9b-c79aac06c76b', tsunami_safe_settlement_pattern).
narrative_ontology:cs_drift_state('685c9525-79ba-441a-ab9b-c79aac06c76b', contemporary_development_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('685c9525-79ba-441a-ab9b-c79aac06c76b', '').
narrative_ontology:cs_kernel_id(aneyoshi_land_use_prohibition__commemorative_husk_reading, aneyoshi_land_use_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, real_estate_developers).
narrative_ontology:constraint_victim(aneyoshi_land_use_prohibition__commemorative_husk_reading, future_residents_below_line).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Construct and sell residential and commercial properties in the zone below the historical tsunami inundation line, profiting from the absence of enforced land-use restrictions while the stone is treated as a non-binding memorial.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, real_estate_developers, beneficiary,
    powerful, biographical, mobile, regional).

% Maintains the stone as a registered cultural heritage asset and tourist attraction, manages land-use zoning, and issues building permits below the inundation line while officially preserving the monument for historical education.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, municipal_government, agenda_setter,
    institutional, generational, constrained, local).

% Purchase or rent housing in the developed zone below the stone's line, often unaware of the historical prohibition or unable to afford safer alternatives, bearing the full catastrophic tsunami risk that the original rule was designed to prevent.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, future_residents_below_line, payer,
    powerless, biographical, trapped, local).

% Documents the institutional decay of the stone from an operational warning to a commemorative monument, analyzing how collective memory transforms into symbolic performance that no longer governs behavior.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, disaster_anthropologist, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_land_use_prohibition__commemorative_husk_reading, real_estate_developers).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves intergenerational memory of tsunami inundation limits through a durable physical monument that anchors collective historical consciousness.
% TRANSFER_FUNCTION: Transfers tsunami risk from developers and municipal tax bases to future occupants of properties constructed below the historical line, while transferring cultural prestige to heritage maintenance authorities.
% ABSENT_VOICES: Future residents are absent from land-use decisions; disaster risk engineers and elderly villagers who remember the prohibition as binding are sidelined by the heritage framing.
% DISAPPEARANCE_RATIONALE: If the stone vanished, real estate development would likely continue unabated given the lack of other enforcement mechanisms, but the loss of the symbol might eventually erode the legitimacy of building below the line. Heritage advocates would mourn; developers would not notice.
% FOUNDING_PROBLEM: Preventing tsunami fatalities by creating a socially enforced land-use boundary that prohibited construction below the maximum observed inundation line.
% FOUNDING_PROBLEM_CORROBORATION: Village oral histories and prefectural disaster records from the 1896 Meiji Sanriku and 1933 Showa Sanriku tsunamis attest the stone was erected as a warning and land-use rule. Contemporary municipal planning documents and tourism brochures corroborate the shift to heritage framing; no external disaster-risk authority attests the founding protective function remains active.
narrative_ontology:disappearance_verdict(aneyoshi_land_use_prohibition__commemorative_husk_reading, contested).
narrative_ontology:founding_problem_status(aneyoshi_land_use_prohibition__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_land_use_prohibition__commemorative_husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(aneyoshi_land_use_prohibition__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_land_use_prohibition__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(aneyoshi_land_use_prohibition__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Theater_ratio is very high (0.88) because the stone is almost pure performance: it is maintained, photographed, and interpreted, but it governs no behavior. Base_extractiveness rises to 0.72 because the husk arrangement enables extensive development that transfers catastrophic risk to future occupants. Suppression is moderate-low (0.35): the original rule is no longer enforced, but the heritage framing suppresses revival by coding any return to enforcement as disrespect for history. Resistance is low (0.15) because there is little organized opposition to the memorialization framing. The measurement series share a single time grid to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The municipal government experiences the constraint as benign heritage preservation and tourism revenue. The developer experiences it as a green light for profitable construction. The future resident experiences it as an unacknowledged risk transfer that only becomes visible in catastrophe. The anthropologist sees the full decay trajectory from protective rule to commemorative theater. The engine should compute divergent seat types from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Real estate developers are structural beneficiaries of the governance vacuum (d near the beneficiary end). Future residents are structural targets (d near the full-target end). Municipal government sits near symmetric: it gains heritage status and tax revenue while avoiding the political cost of restrictive zoning, but it also bears reputational risk if a tsunami strikes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâtsunami protection via land-use restrictionâis dead. The arrangement persists as a Piton because removing the stone would violate cultural taboo, while reviving its enforcement would impose immediate economic costs on developers and the municipal tax base. The mandatrophy is resolved in the sense that the original mandate is obsolete, but the institution has not been retired; instead, it has been repurposed as heritage theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the stone currently possess behavioral force as a land-use rule, or has it decayed to commemorative symbol?',
    'Ethnographic observation of permitting decisions, construction activity, and resident compliance in the zone below the line.',
    'If the stone is behaviorally enforced, this reading''s epsilon and classification are wrong; if not, the sibling reading is wrong.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Empirical resolution of the kernel contest between live rule and dead memorial').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of the original prohibition structural (permitting regime, legal zoning) or internalized (community belief that the stone is only a memorial)?',
    'Post-policy shock observation: if a sudden legal revival of the prohibition meets compliance, suppression was structural; if it meets cultural resistance, suppression was partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest, and revival becomes harder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression of the original rule').

omega_variable(
    naturalness_of_decay,
    'Is the decay of the prohibition an inevitable cultural forgetting, or an actively constructed outcome serving development interests?',
    'Archival analysis of municipal council minutes, planning commission records, and campaign finance linking development lobbying to the heritage reframing.',
    'If actively constructed, the constraint is better classified as Snare or Tangled Rope; if inevitable, Piton is the more accurate claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(naturalness_of_decay, conceptual, 'Whether institutional decay was engineered or inertial').

omega_variable(
    cs_framing_underdetermination,
    'Should the constraint be framed as the physical stone inscription (fixed_text) or the distributed social consensus that it is non-binding (distributed practice)?',
    'Comparative analysis across villages with similar stones: where the text is identical but enforcement differs, the operative constraint is the social consensus, not the stone.',
    'If the social consensus is the true constraint, the kernel is distributed rather than fixed_text, changing the authority_grounding and coupling analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Alternative CS framing of the operative constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aneyoshi_husk_tr_t0, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(aneyoshi_husk_tr_t15, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement(aneyoshi_husk_tr_t30, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(aneyoshi_husk_tr_t45, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 45, 0.68).
narrative_ontology:measurement(aneyoshi_husk_tr_t60, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 60, 0.8).
narrative_ontology:measurement(aneyoshi_husk_tr_t78, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 78, 0.88).

% Extraction over time
narrative_ontology:measurement(aneyoshi_husk_be_t0, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(aneyoshi_husk_be_t15, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 15, 0.24).
narrative_ontology:measurement(aneyoshi_husk_be_t30, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(aneyoshi_husk_be_t45, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 45, 0.52).
narrative_ontology:measurement(aneyoshi_husk_be_t60, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 60, 0.64).
narrative_ontology:measurement(aneyoshi_husk_be_t78, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 78, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(aneyoshi_husk_su_t0, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(aneyoshi_husk_su_t15, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 15, 0.45).
narrative_ontology:measurement(aneyoshi_husk_su_t30, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 30, 0.35).
narrative_ontology:measurement(aneyoshi_husk_su_t45, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 45, 0.25).
narrative_ontology:measurement(aneyoshi_husk_su_t60, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 60, 0.18).
narrative_ontology:measurement(aneyoshi_husk_su_t78, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 78, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__commemorative_husk_reading, behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% The aneyoshi_land_use_prohibition kernel decomposes into two readings: behavioral_competence_reading (live rule) and commemorative_husk_reading (dead memorial). They share the same physical stone but instantiate different constraints with different epsilon values, beneficiary structures, and enforcement profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
