% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__proportionality_balancing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__proportionality_balancing, []).

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
 *   constraint_id: humane_treatment_standard__proportionality_balancing
 *   human_readable: Common Article 3 Proportionality-Balancing Reading of Humane Treatment
 *   domain: international_humanitarian_law/state_security/human_rights
 *
 * SUMMARY:
 *   This story instantiates the proportionality-balancing reading of the
 *   Common Article 3 humane-treatment kernel: neither an absolute prohibition
 *   on coercive treatment nor unlimited security discretion, but a
 *   case-by-case judicial weighing of dignity against asserted necessity.
 *   Under this reading, courts become gatekeepers and treatment
 *   permissibility is decided after the fact, technique by technique, case by
 *   case. The coordination function is real — genuinely hard cases exist
 *   where categorical rules would either paralyze legitimate security
 *   operations or license unlimited abuse — but the same open texture that
 *   solves hard cases also gives the security apparatus room to authorize
 *   increasingly coercive practice under a proportionality label, litigate it
 *   for years, and impose the interim harm on detainees who have no seat at
 *   the balancing table. This is a distinct constraint from the sibling
 *   readings (absolute_prohibition, contextual_necessity), which have their
 *   own ε and are authored as separate stories linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - detaining_state_security_apparatus: primary beneficiary and co-agenda-setter — designs and defends proportionality-compliant protocols
 *   - reviewing_courts_and_tribunals: primary agenda-setter — adjudicates proportionality case by case, gains institutional discretion
 *   - detainees_subject_to_coercive_interrogation: primary target — bears treatment before any proportionality ruling exists
 *   - detainee_families_seeking_accountability: secondary target — bears the cost of individualized, precedent-thin litigation
 *   - human_rights_monitoring_bodies: excluded voice — documents patterns but has no binding role in the balance
 *   - international_humanitarian_law_scholars: analytical observer across the kernel's three readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__proportionality_balancing, 0.51).
domain_priors:suppression_score(humane_treatment_standard__proportionality_balancing, 0.58).
domain_priors:theater_ratio(humane_treatment_standard__proportionality_balancing, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, extractiveness, 0.51).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__proportionality_balancing, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__proportionality_balancing, "Common Article 3 Proportionality-Balancing Reading of Humane Treatment").
narrative_ontology:topic_domain(humane_treatment_standard__proportionality_balancing, "international_humanitarian_law/state_security/human_rights").

domain_priors:requires_active_enforcement(humane_treatment_standard__proportionality_balancing).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__proportionality_balancing, '3cfe3ea3-496e-4ab6-b0ad-82ffb53a8fb8').
narrative_ontology:cs_kernel_codification('3cfe3ea3-496e-4ab6-b0ad-82ffb53a8fb8', fixed_text).
narrative_ontology:cs_authority_grounding('3cfe3ea3-496e-4ab6-b0ad-82ffb53a8fb8', lineage).
narrative_ontology:cs_interpretation_layer_present('3cfe3ea3-496e-4ab6-b0ad-82ffb53a8fb8').
narrative_ontology:cs_reading_relation('3cfe3ea3-496e-4ab6-b0ad-82ffb53a8fb8', humane_treatment_standard__absolute_prohibition, forecloses).
narrative_ontology:cs_reading_relation('3cfe3ea3-496e-4ab6-b0ad-82ffb53a8fb8', humane_treatment_standard__contextual_necessity, influences).
narrative_ontology:cs_axiom('3cfe3ea3-496e-4ab6-b0ad-82ffb53a8fb8', foundational, dignity_and_security_are_commensurable_and_weighable).
narrative_ontology:cs_axiom_status(dignity_and_security_are_commensurable_and_weighable, holdable).
narrative_ontology:cs_axiom_grounding('3cfe3ea3-496e-4ab6-b0ad-82ffb53a8fb8', dignity_and_security_are_commensurable_and_weighable, conventional).
narrative_ontology:cs_axiom('3cfe3ea3-496e-4ab6-b0ad-82ffb53a8fb8', secondary, judicial_case_by_case_review_is_the_legitimate_situs_of_permissibility).
narrative_ontology:cs_axiom_status(judicial_case_by_case_review_is_the_legitimate_situs_of_permissibility, holdable).
narrative_ontology:cs_axiom_grounding('3cfe3ea3-496e-4ab6-b0ad-82ffb53a8fb8', judicial_case_by_case_review_is_the_legitimate_situs_of_permissibility, instrumental).
narrative_ontology:cs_reference_frame('3cfe3ea3-496e-4ab6-b0ad-82ffb53a8fb8', geneva_conventions_common_article_three_floor).
narrative_ontology:cs_drift_state('3cfe3ea3-496e-4ab6-b0ad-82ffb53a8fb8', post_war_on_terror_litigation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3cfe3ea3-496e-4ab6-b0ad-82ffb53a8fb8', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__proportionality_balancing, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, detaining_state_security_apparatus).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, reviewing_courts_and_tribunals).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, interrogators_operating_under_legal_cover).
narrative_ontology:constraint_victim(humane_treatment_standard__proportionality_balancing, detainees_subject_to_coercive_interrogation).
narrative_ontology:constraint_victim(humane_treatment_standard__proportionality_balancing, detainee_families_seeking_accountability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs interrogation and detention protocols under the balancing standard, and litigates their compatibility with Common Article 3 when challenged. Because the standard requires only proportionality rather than a bright line, the apparatus can authorize increasingly coercive techniques by framing them as proportionate to an asserted threat, then defend those authorizations in litigation it has resources to sustain over years.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, detaining_state_security_apparatus, beneficiary,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__proportionality_balancing, detaining_state_security_apparatus, agenda_setter).

% Adjudicate whether specific treatment was proportionate to the security interest claimed, on a case-by-case basis. They gain expanded institutional authority and discretion under this reading — every contested technique becomes a matter for judicial balancing rather than a settled prohibition, which increases the courts' gatekeeping role and their control over the doctrine's substance.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, reviewing_courts_and_tribunals, agenda_setter,
    institutional, generational, analytical, national).

% Conduct interrogations relying on institutional legal guidance about what counts as proportionate. They benefit from ambiguity that shields good-faith reliance on authorized techniques from individual liability, but bear career and legal risk if a court later finds a specific technique disproportionate after the fact.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, interrogators_operating_under_legal_cover, beneficiary,
    moderate, immediate, constrained, national).

% Experience whatever treatment the detaining authority has determined to be proportionate, often without contemporaneous access to counsel or independent review. The balancing standard means no specific technique is categorically off the table until a court rules on it years later, if at all; by the time proportionality is adjudicated, the treatment has already occurred.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, detainees_subject_to_coercive_interrogation, payer,
    powerless, immediate, trapped, national).

% Seek redress or acknowledgment for treatment of detained relatives. Case-by-case balancing means each claim must be individually litigated against a shifting security-necessity justification, with no categorical precedent guaranteeing relief even where treatment closely resembles conduct found disproportionate in prior cases.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, detainee_families_seeking_accountability, payer,
    powerless, biographical, trapped, national).

% Document patterns of coercive treatment and argue for bright-line prohibitions, but have no binding role in the proportionality determination itself — their findings are treated as evidence to be weighed, not as dispositive of the balance, and are frequently excluded or discounted in classified proceedings.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, human_rights_monitoring_bodies, excluded,
    organized, generational, constrained, global).

% Analyze how the balancing standard functions across jurisdictions, comparing outcomes under this reading to the absolute-prohibition and contextual-necessity readings, without a stake in any particular case's outcome.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, international_humanitarian_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(humane_treatment_standard__proportionality_balancing, diffuse).
narrative_ontology:fixing_cost_class(humane_treatment_standard__proportionality_balancing, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for adjudicating hard cases where security claims and dignity claims genuinely conflict, allowing courts to weigh context (threat level, intelligence value, alternatives available) rather than forcing either automatic prohibition or automatic permission.
% TRANSFER_FUNCTION: Moves the power to define 'humane treatment' from a fixed textual floor to case-by-case judicial and administrative discretion; in practice this moves risk of harm onto detainees during the period before any court rules, and moves interpretive authority toward courts and the security apparatus that litigates before them.
% ABSENT_VOICES: Detainees themselves rarely testify or participate meaningfully in the proportionality determination affecting their own treatment; human rights monitoring bodies are treated as external commentators rather than parties to the balance. Their exclusion means the balancing test is conducted primarily between the state's asserted security interest and a judicially-imagined dignity interest, not a real adversarial contest.
% DISAPPEARANCE_RATIONALE: If the proportionality-balancing reading disappeared and one of its siblings prevailed instead, detention practice would shift sharply: under absolute prohibition, entire categories of interrogation technique would become per se unlawful regardless of asserted necessity, ending the case-by-case litigation industry; under contextual necessity, the same techniques would be authorized more freely without judicial gatekeeping. Courts, security agencies, and detainees would all operate under a materially different regime.
% FOUNDING_PROBLEM: Common Article 3 needed to be applied across an enormous range of detention circumstances (declared and undeclared conflicts, mixed civilian-combatant populations, intelligence-gathering contexts) without either freezing security operations entirely or licensing unlimited coercion; proportionality balancing was adopted by some courts and drafters as the mechanism to avoid both extremes.
% FOUNDING_PROBLEM_CORROBORATION: Judges and government legal advisors attest the founding problem remains live — genuine hard cases requiring contextual judgment do arise. Independent human rights monitors and several international law scholars outside the security apparatus argue the 'genuine hard case' framing has become a vehicle for normalizing techniques that would fail under a bright-line reading, and that the balancing framework's open texture is now exploited more than it is needed.
narrative_ontology:disappearance_verdict(humane_treatment_standard__proportionality_balancing, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__proportionality_balancing, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__proportionality_balancing, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(humane_treatment_standard__proportionality_balancing, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__proportionality_balancing, 0.51, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__proportionality_balancing_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(humane_treatment_standard__proportionality_balancing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(humane_treatment_standard__proportionality_balancing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.51) sits at a genuine midpoint reflecting the tangled character of this reading: real coordination value exists (courts do resolve legitimately hard cases) alongside real extraction (the same discretion lets the security apparatus authorize coercive techniques and defend them through protracted litigation while detainees bear the interim harm). Suppression (0.58) is moderate-high because the enforcement mechanism is judicial review with long timelines and classified proceedings, which structurally limits detainee access to timely relief. Theater ratio (0.44) and its rising trajectory reflect increasing use of the proportionality framework as a defensive litigation posture rather than a genuine contemporaneous constraint on interrogation decisions — the balancing test is increasingly invoked after techniques are already in use, rather than before. Accessibility collapse (0.42) is moderate: unlike an absolute prohibition, alternatives (categorical bans on specific techniques) remain conceptually available and are litigated for, but courts' case-by-case posture makes durable precedent difficult to establish, functionally narrowing what detainees can rely on. Resistance (0.62) is comparatively high because human rights bodies, scholars, and some judges actively contest the balancing framework's drift toward permissiveness.
 *
 * PERSPECTIVAL GAP:
 *   From the security apparatus and courts' seats, this reading is a coordination achievement — a workable middle path avoiding both paralysis and lawlessness, and one that expands each institution's own authority. From the detainee and family seats, the same standard operates as an extraction mechanism: no technique is categorically forbidden until adjudicated, which structurally means the harm occurs first and vindication (if it comes) arrives years later against a moving evidentiary target. The engine should compute a tangled_rope from the courts'/security seats and something closer to snare from the detainee seat — that divergence is the point of authoring the story this way rather than reconciling it.
 *
 * DIRECTIONALITY LOGIC:
 *   Detaining state security apparatus and interrogators are declared beneficiaries: the balancing standard gives them room to authorize contested techniques and litigate them from institutional strength, so directionality should sit near the beneficiary end. Reviewing courts are beneficiaries of expanded institutional discretion even though they are nominally neutral arbiters — the reading vindicates judicial case-by-case competence as a doctrine, which is why that proposition (not an actor) is listed separately under vindicated_propositions. Detainees and their families are declared victims: trapped exit, immediate/biographical time horizon, and no meaningful voice in the proportionality determination affecting them — directionality should sit near the full-target end. Human rights monitoring bodies are excluded rather than victimized directly; their exclusion is evidentiary (documented but non-dispositive), which is why they are marked excluded, not payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (avoiding both paralysis and unlimited coercion across enormously varied detention contexts) may still be genuinely live in some hard cases, which is why founding_problem_status is authored as contested rather than dead. The mandatrophy risk is that the balancing framework, originally justified by the existence of genuinely hard cases, has been generalized into a default posture that treats nearly every contested technique as requiring individualized adjudication — including techniques with settled treatment under customary international law. The classification as tangled_rope (rather than snare) preserves the possibility that the coordination function is still doing real work in the residual hard cases, while flagging that the same structure now carries substantial extraction that a pure-coordination rope would not.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_hard_cases_vs_normalized_coercion,
    'How much of the proportionality-balancing framework''s operation addresses genuinely hard, novel cases versus normalizing techniques that would fail under a bright-line prohibition?',
    'Longitudinal case-coding of proportionality rulings: classify each ruling as (a) addressing a genuinely novel context-dependent question or (b) re-litigating a technique with settled customary-law status under a proportionality label. A rising share of (b) over time would support the mandatrophy reading.',
    'If (b) dominates, the tangled_rope classification understates extraction and the constraint functions closer to a snare wearing coordination language; if (a) dominates, the coordination function is more substantial than the authored metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_hard_cases_vs_normalized_coercion, empirical, 'Whether balancing cases are genuinely novel or re-litigate settled prohibitions.').

omega_variable(
    detainee_voice_structural_absence,
    'Is the detainee''s structural absence from the proportionality determination itself (no timely participation, no binding evidentiary weight for monitoring bodies) a fixable procedural defect or is it constitutive of how this reading necessarily operates?',
    'Comparative study of jurisdictions that have introduced contemporaneous independent monitoring with binding evidentiary weight under a balancing framework, versus those without, tracking whether extraction (measured by adverse-treatment rates surviving judicial review) changes.',
    'If detainee absence is fixable without abandoning balancing, the reading could evolve toward genuine rope; if the absence is structurally necessary to preserve security-apparatus litigation advantage, it supports classifying the reading as durably tangled or worse.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(detainee_voice_structural_absence, conceptual, 'Whether the excluded-voice problem is a fixable defect or structurally constitutive of this reading.').

omega_variable(
    kernel_reading_selection_pressure,
    'Which institutional actors have selected the proportionality-balancing reading over its siblings (absolute_prohibition, contextual_necessity), and does that selection itself indicate capture?',
    'Trace which reading dominant courts and government legal offices have adopted over time and correlate with litigation posture and outcomes for the security apparatus versus detainees.',
    'If the reading was substantially advanced by state legal advisors specifically because it preserves more discretion than absolute prohibition while offering more legal cover than unconstrained necessity, that supports treating the reading''s adoption itself as part of the extraction structure, not a neutral doctrinal evolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, conceptual, 'Whether institutional selection of this reading over its siblings reflects genuine doctrinal merit or strategic advantage-seeking.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__proportionality_balancing, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, humane_treatment_standard__proportionality_balancing, theater_ratio, 0, 0.2).
narrative_ontology:measurement(huma_tr_t4, humane_treatment_standard__proportionality_balancing, theater_ratio, 4, 0.25).
narrative_ontology:measurement(huma_tr_t8, humane_treatment_standard__proportionality_balancing, theater_ratio, 8, 0.3).
narrative_ontology:measurement(huma_tr_t12, humane_treatment_standard__proportionality_balancing, theater_ratio, 12, 0.34).
narrative_ontology:measurement(huma_tr_t16, humane_treatment_standard__proportionality_balancing, theater_ratio, 16, 0.38).
narrative_ontology:measurement(huma_tr_t20, humane_treatment_standard__proportionality_balancing, theater_ratio, 20, 0.41).
narrative_ontology:measurement(huma_tr_t24, humane_treatment_standard__proportionality_balancing, theater_ratio, 24, 0.44).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, humane_treatment_standard__proportionality_balancing, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(huma_be_t4, humane_treatment_standard__proportionality_balancing, base_extractiveness, 4, 0.39).
narrative_ontology:measurement(huma_be_t8, humane_treatment_standard__proportionality_balancing, base_extractiveness, 8, 0.43).
narrative_ontology:measurement(huma_be_t12, humane_treatment_standard__proportionality_balancing, base_extractiveness, 12, 0.46).
narrative_ontology:measurement(huma_be_t16, humane_treatment_standard__proportionality_balancing, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(huma_be_t20, humane_treatment_standard__proportionality_balancing, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(huma_be_t24, humane_treatment_standard__proportionality_balancing, base_extractiveness, 24, 0.51).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, humane_treatment_standard__proportionality_balancing, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(huma_su_t4, humane_treatment_standard__proportionality_balancing, suppression_requirement, 4, 0.45).
narrative_ontology:measurement(huma_su_t8, humane_treatment_standard__proportionality_balancing, suppression_requirement, 8, 0.49).
narrative_ontology:measurement(huma_su_t12, humane_treatment_standard__proportionality_balancing, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(huma_su_t16, humane_treatment_standard__proportionality_balancing, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(huma_su_t20, humane_treatment_standard__proportionality_balancing, suppression_requirement, 20, 0.57).
narrative_ontology:measurement(huma_su_t24, humane_treatment_standard__proportionality_balancing, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__proportionality_balancing, enforcement_mechanism).
narrative_ontology:affects_constraint(humane_treatment_standard__proportionality_balancing, humane_treatment_standard__absolute_prohibition).
narrative_ontology:affects_constraint(humane_treatment_standard__proportionality_balancing, humane_treatment_standard__contextual_necessity).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the humane_treatment_standard kernel decomposed per the ε-invariance principle: absolute_prohibition (non-derogable minimum, ε near-zero extraction from its own lights, near-mountain-like textual claim), contextual_necessity (permissive, no judicial gate, highest ε of the three), and this proportionality_balancing reading (moderate ε, tangled_rope, courts as gatekeepers). Each reading has its own beneficiary/victim structure and its own stable ε; they are linked, not merged, because measuring 'humane treatment permissibility' one way (textual floor) versus another way (case-by-case judicial weighing) versus a third way (security-imperative override) yields three structurally distinct constraints, not one constraint under three observables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
