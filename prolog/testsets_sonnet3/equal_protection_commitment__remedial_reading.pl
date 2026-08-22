% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_commitment__remedial_reading, []).

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
 *   constraint_id: equal_protection_commitment__remedial_reading
 *   human_readable: Equal Protection as Anti-Subordination: Remedial Race-Consciousness Reading
 *   domain: constitutional_law/political_philosophy/social_policy
 *
 * SUMMARY:
 *   This story instantiates the remedial reading of the equal protection
 *   kernel: equal protection is read to forbid the perpetuation of a racial
 *   caste system and to affirmatively permit — not merely tolerate —
 *   race-conscious state action designed to dismantle the residue of that
 *   subordination. This is one of three structurally distinct constraints
 *   sharing the equal protection kernel (the others being the colorblind
 *   reading and the diversity/compelling-interest reading); each reading
 *   produces a different beneficiary/victim structure and a different epsilon
 *   from the same constitutional text. Under this reading specifically,
 *   historically subordinated groups and the state actors implementing
 *   remedies sit in the beneficiary position, while individuals denied a
 *   specific allocation because of a race-conscious program sit in the victim
 *   position. ε is authored high (0.52) because the beneficiary/victim
 *   structure genuinely inverts depending on which individual applicant's
 *   cycle is examined — this is not measurement noise but the structural
 *   signature of a remedial regime that reallocates scarce, zero-sum goods.
 *
 * KEY AGENTS:
 *   - historically_subordinated_racial_groups: beneficiary of remedial classification (organized/constrained)
 *   - state_actors_implementing_remedial_programs: agenda_setter authorized to use racial classification instrumentally (institutional/analytical)
 *   - historically_privileged_applicants_denied_preference: payer bearing individual cost for group-level historical remedy (moderate/constrained)
 *   - individual_nonminority_competitors_in_zero_sum_allocations: payer with no time-horizon flexibility, single irreversible allocation event (powerless/trapped)
 *   - reviewing_courts: analytical observer adjudicating which kernel reading controls a given program
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__remedial_reading, 0.52).
domain_priors:suppression_score(equal_protection_commitment__remedial_reading, 0.48).
domain_priors:theater_ratio(equal_protection_commitment__remedial_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__remedial_reading, "Equal Protection as Anti-Subordination: Remedial Race-Consciousness Reading").
narrative_ontology:topic_domain(equal_protection_commitment__remedial_reading, "constitutional_law/political_philosophy/social_policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__remedial_reading, '6d002506-6936-47bb-a894-610b65f80f85').
narrative_ontology:cs_kernel_codification('6d002506-6936-47bb-a894-610b65f80f85', fixed_text).
narrative_ontology:cs_authority_grounding('6d002506-6936-47bb-a894-610b65f80f85', lineage).
narrative_ontology:cs_interpretation_layer_present('6d002506-6936-47bb-a894-610b65f80f85').
narrative_ontology:cs_reading_relation('6d002506-6936-47bb-a894-610b65f80f85', equal_protection_commitment__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('6d002506-6936-47bb-a894-610b65f80f85', equal_protection_commitment__diversity_reading, coexists_with).
narrative_ontology:cs_axiom('6d002506-6936-47bb-a894-610b65f80f85', foundational, anti_caste_principle_permits_remedial_classification).
narrative_ontology:cs_axiom_status(anti_caste_principle_permits_remedial_classification, holdable).
narrative_ontology:cs_axiom_grounding('6d002506-6936-47bb-a894-610b65f80f85', anti_caste_principle_permits_remedial_classification, deontological).
narrative_ontology:cs_axiom('6d002506-6936-47bb-a894-610b65f80f85', foundational, historical_subordination_creates_present_remedial_obligation).
narrative_ontology:cs_axiom_status(historical_subordination_creates_present_remedial_obligation, holdable).
narrative_ontology:cs_axiom_grounding('6d002506-6936-47bb-a894-610b65f80f85', historical_subordination_creates_present_remedial_obligation, empirically_contingent).
narrative_ontology:cs_reference_frame('6d002506-6936-47bb-a894-610b65f80f85', reconstruction_era_anti_subordination_purpose).
narrative_ontology:cs_drift_state('6d002506-6936-47bb-a894-610b65f80f85', post_sffa_2023_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('6d002506-6936-47bb-a894-610b65f80f85', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__remedial_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, historically_subordinated_racial_groups).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, state_actors_implementing_remedial_programs).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, civil_rights_enforcement_agencies).
narrative_ontology:constraint_victim(equal_protection_commitment__remedial_reading, historically_privileged_applicants_denied_preference).
narrative_ontology:constraint_victim(equal_protection_commitment__remedial_reading, individual_nonminority_competitors_in_zero_sum_allocations).
narrative_ontology:constraint_vindicates(equal_protection_commitment__remedial_reading, anti_caste_principle).
narrative_ontology:constraint_vindicates(equal_protection_commitment__remedial_reading, substantive_equality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups whose access to education, employment, and capital was structured for generations by explicit legal subordination. Under this reading, they receive standing to be the object of remedial, race-conscious programs — set-asides, targeted admissions criteria, disparity studies — designed to dismantle the residue of that subordination. Exit from the underlying condition (accumulated disadvantage) is not available by individual choice; the remedial apparatus is one of few levers responsive to group-level historical injury.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, historically_subordinated_racial_groups, beneficiary,
    organized, generational, constrained, national).

% Legislatures, agencies, and universities that design and administer race-conscious remedies — affirmative action plans, minority contracting set-asides, majority-minority districting. Under this reading they are affirmatively authorized (not merely tolerated) to use racial classification instrumentally, provided the program is tied to documented past discrimination and is not more burdensome than necessary. They bear the institutional risk of litigation if a reviewing court applies a different reading of the same kernel.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, state_actors_implementing_remedial_programs, agenda_setter,
    institutional, generational, analytical, national).

% Agencies (e.g., civil rights divisions, EEOC-type bodies) whose enforcement mandate and institutional relevance depend on the remedial reading remaining doctrinally live. They investigate disparate treatment claims, defend remedial statutes, and gain resources and jurisdiction when courts credit anti-subordination as a compelling interest.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, civil_rights_enforcement_agencies, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_commitment__remedial_reading, civil_rights_enforcement_agencies, agenda_setter).

% Individual applicants for a school seat, contract, or job who are personally denied that specific allocation because a remedial program weighted race in favor of another applicant. From this seat, an individual bears a concrete, personal cost for a historical injury they did not personally inflict; their only recourse is litigation challenging the specific program's tailoring, which is slow and uncertain under this reading's own doctrinal test.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, historically_privileged_applicants_denied_preference, payer,
    moderate, biographical, constrained, national).

% Applicants in a single competitive cycle (one admissions class, one contract cycle) where the remedial classification directly reallocates a scarce slot. Unlike the broader privileged-applicant category, these individuals have no time-horizon flexibility — the allocation is a single irreversible event — and no meaningful exit: reapplying elsewhere does not undo the specific loss.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, individual_nonminority_competitors_in_zero_sum_allocations, payer,
    powerless, immediate, trapped, regional).

% Federal and state courts adjudicating whether a given remedial program survives scrutiny. They do not benefit or pay directly but determine, case by case, whether this reading or a sibling reading of the equal protection kernel controls the outcome — their doctrinal choices are the site where the kernel contest is actually resolved for a given program.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, reviewing_courts, observer,
    institutional, civilizational, analytical, national).

% Litigants, scholars, and jurists who hold that any racial classification by the state is presumptively unconstitutional regardless of remedial purpose. They are not parties inside this reading's operation — this story concerns the remedial reading's own internal structure — but their doctrine is the primary competing framework that would, if adopted by a reviewing court, dissolve the remedial reading's authorization entirely.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, colorblind_reading_advocates, excluded,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_commitment__remedial_reading, diffuse).
narrative_ontology:fixing_cost_class(equal_protection_commitment__remedial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a collective response to accumulated, group-level harm from historical de jure and de facto subordination that individual-level, race-neutral remedies cannot reach — pooling remedial authority in state actors so that disparate group outcomes traceable to past discrimination can be addressed at the level (group) at which the injury occurred.
% TRANSFER_FUNCTION: Moves specific, scarce allocations (admissions seats, contracts, districted political power) from individuals who would have received them under a race-neutral rule to individuals from groups the state has determined were subordinated by prior state action, justified as offsetting an unremedied historical transfer running the other direction.
% ABSENT_VOICES: Individual applicants denied a specific seat or contract rarely have a forum to contest the historical premise underlying the remedy itself (only its tailoring); colorblind_reading advocates are structurally excluded from this reading's own operation since accepting their premise would dissolve the remedial authorization being exercised.
% DISAPPEARANCE_RATIONALE: If this reading disappeared overnight (i.e., no reading of equal protection authorized race-conscious remedial measures), affirmative action programs, minority set-asides, and remedial districting would lose their constitutional foundation nationwide; state actors would need to redesign programs on strictly race-neutral, proxy-based criteria (income, geography, first-generation status), producing materially different beneficiary populations and reopening decades of settled institutional practice.
% FOUNDING_PROBLEM: Formal legal equality (the end of de jure segregation) left in place accumulated, structural racial subordination — in wealth, education access, political representation, and institutional capture — that a purely prospective, race-neutral rule would leave permanently unremedied because the harm was itself racially targeted and compounding.
% FOUNDING_PROBLEM_CORROBORATION: Empirical labor-market and education-access studies from outside civil rights enforcement agencies (independent economists and sociologists studying racial wealth and opportunity gaps) corroborate that the founding problem — compounding, group-differentiated disadvantage traceable to state action — remains measurably live in several domains. Colorblind_reading advocates and some historically privileged payers dispute that the ORIGINAL founding problem (de jure caste) still describes the present situation closely enough to justify continued race-conscious remedy, arguing the doctrine has drifted from remedy toward permanent group-preference; this dispute is the live fault line and is not resolved by any single corroborating source.
narrative_ontology:disappearance_verdict(equal_protection_commitment__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__remedial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__remedial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_commitment__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__remedial_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_commitment__remedial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_commitment__remedial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_commitment__remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) is authored moderate-high and rising slightly over the interval as remedial programs have proliferated and become more granular (disparity-study-justified set-asides, holistic-review racial weighting) since the doctrine's initial articulation, without a correspondingly rising suppression apparatus — suppression is authored as gently DECLINING (0.62 to 0.48) because courts have progressively narrowed the doctrinal space (tighter tailoring requirements, sunset expectations, strict scrutiny discipline) even as the underlying remedial commitment persists, meaning less raw coercive latitude is available to administer the same commitment over time. Theater ratio rises modestly (0.10 to 0.22) reflecting a documented trend toward diversity-statement and disparity-study formalism that substitutes procedural box-checking for demonstrated nexus to specific past discrimination in some implementations, without yet dominating the constraint's operation.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (state actors) and the beneficiary-group seat experience this as coordination: a state finally exercising its power to correct a documented historical wrong. The payer seats — particularly the single-cycle competitor with no time-horizon flexibility — experience the identical structure as an involuntary, individually-borne transfer justified by a historical account they had no part in and cannot contest through the normal channel (they can only contest tailoring, not the underlying premise). This is the seat divergence the tangled_rope classification is built to hold: both readings of the SAME structural operation are correct from their respective positions, and the engine computes the divergence rather than requiring an author's adjudication.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically subordinated groups and implementing state actors derive low d (near-beneficiary) because the remedial reading's authorization runs directly to them — they are its intended object and administrator, respectively. Individual denied applicants derive high d (near-target) because the same classification that empowers the beneficiary group directly reallocates AWAY from them a specific, identifiable good, in a single transaction with no arbitrage or exit available within that allocation cycle. The single-cycle competitor is authored with directionality nearer full-target than the broader privileged-applicant category because 'constrained' exit still implies some longer-run adaptability (different school, different year) that 'trapped' single-cycle competitors lack entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (de jure caste subordination) is authored as contested rather than dead or clearly live: independent empirical corroboration supports that compounding structural disadvantage persists in measurable form, which prevents this reading from being dismissed as a pure zombie mandate; but the reading's own internal doctrine (strict scrutiny, narrow tailoring, sunset expectations articulated by reviewing courts) reflects an acknowledgment that the ORIGINAL problem (explicit legal caste) is substantially resolved even if compounding effects are not, which is precisely why courts increasingly demand documented present-tense nexus rather than general historical narrative. This prevents mislabeling the whole doctrine as either pure coordination (ignoring the real cost imposed on payer seats) or pure extraction (ignoring the corroborated persistence of the founding problem in some form).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remedy_scope_temporal_boundary,
    'Is the remedial reading''s authorization bounded by an implicit sunset (once the specific historical injury is substantially remedied) or does anti-caste concern extend indefinitely to any measurable disparate group outcome regardless of causal proximity to the original subordination?',
    'Track whether courts and legislatures applying the remedial reading increasingly require documented, present-tense causal nexus to specific past discriminatory acts (narrowing) versus accepting generalized societal disparity as sufficient justification (widening) across the doctrine''s case history.',
    'A bounded reading pushes the constraint toward scaffold (temporary remedial support with an eventual sunset); an unbounded reading sustains tangled_rope indefinitely, since the coordination/extraction hybrid never resolves toward either pure coordination or termination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedy_scope_temporal_boundary, conceptual, 'Whether the remedial reading has an implicit temporal boundary tied to remedying a specific historical injury.').

omega_variable(
    beneficiary_victim_inversion_under_observer_position,
    'Does the beneficiary/victim assignment structurally invert depending on which individual''s allocation cycle is examined, such that the SAME remedial program is simultaneously coordination (from the group-historical view) and extraction (from the single-cycle denied-applicant view)?',
    'Compare aggregate group-level outcome data (which would show net coordination benefit across cohorts) against individual case records of denied applicants (which would show concentrated, personal cost) for a specific remedial program over multiple admissions/allocation cycles.',
    'If the inversion is confirmed structurally (not merely rhetorically), it supports authoring this constraint as tangled_rope rather than pure rope or pure snare — genuine coordination function coexisting with genuine, non-incidental extraction from an identifiable payer class, which is exactly the high-ε signature this story authors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_victim_inversion_under_observer_position, empirical, 'Whether beneficiary and victim positions genuinely invert by observer position rather than merely appearing to.').

omega_variable(
    kernel_framing_alternative_the_state_actor_as_kernel_vs_the_text_as_kernel,
    'Should the kernel here be read as the constitutional TEXT (the Equal Protection Clause, fixed since 1868/1964) or as the accumulated JUDICIAL DOCTRINE interpreting it (which has itself shifted materially across Brown, Bakke, Grutter, Fisher, SFFA)? These are two coherent framings: text-as-kernel treats doctrinal shifts as interpretation drift around a fixed anchor; doctrine-as-kernel treats the doctrine itself as the object under contest, with the text functioning mainly as a legitimating reference.',
    'Examine whether reviewing courts, when adopting a new reading, purport to be discovering the text''s original meaning (supporting text-as-kernel) or explicitly revising precedent in light of changed social understanding (supporting doctrine-as-kernel).',
    'Text-as-kernel framing would classify recent doctrinal narrowing (e.g., SFFA) as drift AWAY from a stable remedial-reading anchor (supporting an authority_erosion drift_state); doctrine-as-kernel framing would treat the same event as the kernel itself relocating, which would instead register as codification_collapse. This story adopts text-as-kernel per the manifest''s framing (kernel = ''equal_protection_commitment'' as a persisting textual commitment), but the alternative is live and would change the drift_state direction authored below.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_alternative_the_state_actor_as_kernel_vs_the_text_as_kernel, conceptual, 'Alternative framing of the kernel as fixed text versus as the accumulated, shifting judicial doctrine interpreting that text.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__remedial_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_commitment__remedial_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(equa_tr_t10, equal_protection_commitment__remedial_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(equa_tr_t20, equal_protection_commitment__remedial_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(equa_tr_t30, equal_protection_commitment__remedial_reading, theater_ratio, 30, 0.17).
narrative_ontology:measurement(equa_tr_t40, equal_protection_commitment__remedial_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement(equa_tr_t50, equal_protection_commitment__remedial_reading, theater_ratio, 50, 0.21).
narrative_ontology:measurement(equa_tr_t60, equal_protection_commitment__remedial_reading, theater_ratio, 60, 0.22).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_commitment__remedial_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(equa_be_t10, equal_protection_commitment__remedial_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement(equa_be_t20, equal_protection_commitment__remedial_reading, base_extractiveness, 20, 0.47).
narrative_ontology:measurement(equa_be_t30, equal_protection_commitment__remedial_reading, base_extractiveness, 30, 0.49).
narrative_ontology:measurement(equa_be_t40, equal_protection_commitment__remedial_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(equa_be_t50, equal_protection_commitment__remedial_reading, base_extractiveness, 50, 0.51).
narrative_ontology:measurement(equa_be_t60, equal_protection_commitment__remedial_reading, base_extractiveness, 60, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_commitment__remedial_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(equa_su_t10, equal_protection_commitment__remedial_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(equa_su_t20, equal_protection_commitment__remedial_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(equa_su_t30, equal_protection_commitment__remedial_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(equa_su_t40, equal_protection_commitment__remedial_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(equa_su_t50, equal_protection_commitment__remedial_reading, suppression_requirement, 50, 0.49).
narrative_ontology:measurement(equa_su_t60, equal_protection_commitment__remedial_reading, suppression_requirement, 60, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__remedial_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_commitment__remedial_reading, equal_protection_commitment__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__remedial_reading, equal_protection_commitment__diversity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'equal protection and race-conscious action' per the ε-invariance principle. equal_protection_commitment__colorblind_reading holds that ANY state racial classification is presumptively unconstitutional (near-mountain framing from that reading's own premises, low authorized ε since no remedial exception exists). equal_protection_commitment__diversity_reading permits race as one factor for compelling educational-diversity interest, not historical remedy (moderate ε, narrower beneficiary class limited to educational-diversity contexts). This remedial reading authors the highest ε of the three (0.52) because its beneficiary/victim structure is the most directly redistributive and inverts most sharply by observer position. All three share the same constitutional text as their contested kernel but instantiate structurally distinct constraints with distinct beneficiary/victim sets, distinct enforcement postures, and distinct persistence dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
