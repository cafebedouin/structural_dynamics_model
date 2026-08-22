% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__security_maximization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__security_maximization_reading, []).

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
 *   constraint_id: geneva_conventions_1949__security_maximization_reading
 *   human_readable: Security-Maximization Reading of the Geneva Conventions (Operational-Necessity Suspension Doctrine)
 *   domain: international_law/security_policy/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the security-maximization reading of the Geneva
 *   Conventions kernel: the position that Convention protections are
 *   peacetime aspirations properly suspended when operational necessity in
 *   asymmetric, irregular warfare demands it. Under this reading the
 *   'unlawful combatant' category expands to deny POW status and habeas
 *   review, civilian immunity degrades through human-shields doctrine and
 *   expanded collateral-damage tolerance, detention becomes indefinite and
 *   executively administered, and coercive interrogation is relabeled as
 *   falling short of torture. This is a distinct constraint from the
 *   humanitarian_ceiling_reading (which holds the Conventions bind regardless
 *   of adversary conduct) and the conditional_reciprocity_reading (which ties
 *   the degree of protection to the adversary's own compliance) — each of
 *   those is a separate story with its own epsilon, beneficiary/victim
 *   structure, and classification, linked here via
 *   network.affects_constraints. This story's epsilon (0.81) describes only
 *   the security-maximization arrangement as it actually operates: high
 *   extraction concentrated on detainees, captured irregulars, and civilians,
 *   running through an enforcement apparatus the executive itself controls
 *   and self-certifies.
 *
 * KEY AGENTS:
 *   - detaining_state_executive: primary agenda-setter and structural beneficiary — sets the necessity classification and controls review
 *   - counterterrorism_intelligence_apparatus: primary functional beneficiary — collects the interrogation/detention leverage the suspension enables
 *   - private_detention_contractors: secondary beneficiary — revenue depends on population remaining outside POW/civilian administrative tracks
 *   - unlawful_combatant_detainees: primary target — bears indefinite detention without POW status or habeas review
 *   - civilian_populations_in_conflict_zones: primary target — bears degraded targeting immunity
 *   - captured_irregular_fighters: primary target — denied reciprocal protection categories by definitional fiat
 *   - international_humanitarian_law_bodies: analytical/excluded observer — asserts the ceiling reading applies but has no enforcement leverage over this reading's operation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, 0.81).
domain_priors:suppression_score(geneva_conventions_1949__security_maximization_reading, 0.87).
domain_priors:theater_ratio(geneva_conventions_1949__security_maximization_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__security_maximization_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__security_maximization_reading, "Security-Maximization Reading of the Geneva Conventions (Operational-Necessity Suspension Doctrine)").
narrative_ontology:topic_domain(geneva_conventions_1949__security_maximization_reading, "international_law/security_policy/political_philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__security_maximization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__security_maximization_reading, '7666969a-e50d-401c-b036-2e599449ed9c').
narrative_ontology:cs_kernel_codification('7666969a-e50d-401c-b036-2e599449ed9c', fixed_text).
narrative_ontology:cs_authority_grounding('7666969a-e50d-401c-b036-2e599449ed9c', extraction).
narrative_ontology:cs_interpretation_layer_present('7666969a-e50d-401c-b036-2e599449ed9c').
narrative_ontology:cs_reading_relation('7666969a-e50d-401c-b036-2e599449ed9c', geneva_conventions_1949__humanitarian_ceiling_reading, forecloses).
narrative_ontology:cs_reading_relation('7666969a-e50d-401c-b036-2e599449ed9c', geneva_conventions_1949__conditional_reciprocity_reading, influences).
narrative_ontology:cs_axiom('7666969a-e50d-401c-b036-2e599449ed9c', foundational, operational_necessity_supersedes_treaty_floor).
narrative_ontology:cs_axiom_status(operational_necessity_supersedes_treaty_floor, holdable).
narrative_ontology:cs_axiom_grounding('7666969a-e50d-401c-b036-2e599449ed9c', operational_necessity_supersedes_treaty_floor, instrumental).
narrative_ontology:cs_axiom('7666969a-e50d-401c-b036-2e599449ed9c', foundational, executive_sole_arbiter_of_combatant_status).
narrative_ontology:cs_axiom_status(executive_sole_arbiter_of_combatant_status, holdable).
narrative_ontology:cs_axiom_grounding('7666969a-e50d-401c-b036-2e599449ed9c', executive_sole_arbiter_of_combatant_status, conventional).
narrative_ontology:cs_reference_frame('7666969a-e50d-401c-b036-2e599449ed9c', executive_necessity_determination_primacy).
narrative_ontology:cs_drift_state('7666969a-e50d-401c-b036-2e599449ed9c', post_9_11_asymmetric_conflict_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('7666969a-e50d-401c-b036-2e599449ed9c', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, detaining_state_executive).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, counterterrorism_intelligence_apparatus).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, private_detention_contractors).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, unlawful_combatant_detainees).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, civilian_populations_in_conflict_zones).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, captured_irregular_fighters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, field_commanders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declares the category of 'unlawful combatant' and the operational threshold for suspending Convention protections, authorizes indefinite detention and coercive interrogation programs, and controls the classification review process that determines who receives any hearing at all. Faces no binding external check on the necessity determination it makes about itself.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, detaining_state_executive, agenda_setter,
    institutional, generational, arbitrage, global).

% Gains direct access to interrogation methods and indefinite custody that would be barred under the humanitarian-ceiling reading. Interrogation output and detention leverage are treated as security goods; the apparatus reports to the executive that classified this reading as necessary and has no incentive to certify the necessity has expired.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, counterterrorism_intelligence_apparatus, beneficiary,
    institutional, immediate, arbitrage, global).

% Operate detention facilities and interrogation-support services under contracts that exist because the suspension regime removed ordinary prisoner-of-war administrative requirements. Revenue and continuity depend on the detention population remaining outside the POW/civilian tracks that would trigger release timelines.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, private_detention_contractors, beneficiary,
    organized, biographical, mobile, national).

% Held without POW status, without habeas corpus, and without a fixed release date because the classification denies them the Convention's status categories. Cannot contest the necessity determination that placed them there; formal review, where it exists, is administered by the same authority that made the classification.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, unlawful_combatant_detainees, payer,
    powerless, biographical, trapped, global).

% Bear degraded immunity from attack because 'human shields' doctrine and expanded collateral-damage tolerance shift the burden of avoiding civilian harm away from the attacking force and onto the adversary's alleged co-location with combatants. Have no standing before the body that authorized the doctrine and no reliable after-the-fact accounting.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, civilian_populations_in_conflict_zones, payer,
    powerless, immediate, trapped, regional).

% Fall outside the POW category by definition of the doctrine (failure to wear distinguishing insignia, lack of a responsible command structure recognized by the detaining state), which removes the reciprocal protections the conditional-reciprocity reading would still extend contingent on their own conduct. Have essentially no forum in which to dispute the category assignment.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, captured_irregular_fighters, payer,
    powerless, biographical, trapped, regional).

% Make real-time targeting and detention decisions under rules of engagement loosened by the operational-necessity doctrine. Benefit from wider discretion but also bear personal legal exposure if a later tribunal (domestic court-martial, international body) revisits a necessity call the doctrine told them was theirs to make.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, field_commanders, agenda_setter,
    organized, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__security_maximization_reading, field_commanders, payer).

% ICRC, treaty-body monitors, and international courts assert the Conventions bind regardless of adversary conduct or operational tempo. Their findings are treated by the detaining state as advisory at most; they have no enforcement mechanism against a state invoking this reading and are not party to the internal necessity determination.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, international_humanitarian_law_bodies, excluded,
    institutional, generational, analytical, global).

% Periodically asked to review individual detention or interrogation-method claims. Some assert limited habeas jurisdiction; the executive's necessity classification narrows what they can review and the state resists disclosure that would let courts test the classification against the facts.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, domestic_courts_reviewing_detention, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_1949__security_maximization_reading, counterterrorism_intelligence_apparatus).
narrative_ontology:fixing_cost_class(geneva_conventions_1949__security_maximization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the state's security apparatus around a single operational premise — that in asymmetric, irregular conflict the enemy's non-adherence to formal combatant markers and command structures makes conventional Convention categories unworkable, so protections are suspended in favor of executive-determined necessity. This lets intelligence, detention, and targeting functions move fast without case-by-case legal clearance.
% TRANSFER_FUNCTION: Moves procedural protection, physical safety, and legal standing away from detainees, captured irregulars, and civilians in contested zones, and moves interrogation access, detention flexibility, and unreviewed operational discretion toward the executive, the intelligence apparatus, and contracted detention operators.
% ABSENT_VOICES: International humanitarian law bodies (ICRC, treaty monitors, international courts) assert the ceiling applies regardless of reciprocity or operational tempo, but they sit outside the classification process entirely. Detainees themselves are structurally absent from the necessity determination that defines their status.
% DISAPPEARANCE_RATIONALE: If the security-maximization reading disappeared overnight — if courts and political actors uniformly adopted the humanitarian-ceiling or conditional-reciprocity readings instead — the detention population would need individualized status review, indefinite holds without charge would become legally exposed, coercive interrogation programs would need to be dismantled or relabeled, and targeting doctrine around civilian co-location would tighten. Entire contractor and intelligence-processing arrangements built on the suspension premise would need to restructure or close.
% FOUNDING_PROBLEM: Early-21st-century irregular conflicts involved adversaries who did not wear uniforms, did not maintain conventional command hierarchies, and sometimes deliberately embedded among civilians — states argued the 1949 Convention categories (POW, protected civilian) were drafted for state-on-state war and left a real gap for classifying and handling fighters who fit neither category cleanly.
% FOUNDING_PROBLEM_CORROBORATION: The detaining executive and intelligence apparatus attest the categorization gap is still live and operationally necessary. Outside the benefiting parties, international humanitarian law bodies and a substantial body of international and domestic judicial opinion attest the gap is real but does not license wholesale suspension — the ICRC's position and multiple domestic court rulings hold that the Conventions' minimum protections (common Article 3, due process for detention) were drafted precisely to cover exactly this ambiguous-status case, meaning the 'aspiration that yields' framing is itself contested by the treaty's own drafting history, not merely by adversaries of the state.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__security_maximization_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__security_maximization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__security_maximization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_1949__security_maximization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__security_maximization_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__security_maximization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__security_maximization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81) and rising over the interval because the doctrine's core move — treating Convention categories as suspendable by unilateral executive necessity determination — concentrates cost on detainees and civilians while the classifying authority bears none of the cost of misclassification. Suppression is authored even higher (0.87) because the arrangement's persistence depends on actively foreclosing external review: denying habeas corpus, resisting disclosure to domestic courts, and disregarding ICRC/treaty-body findings. Theater ratio rises moderately (0.20 to 0.42) as the doctrine matures institutionally — periodic tribunal review processes and administrative status boards are established, but they operate inside the same classification the executive controls, so an increasing share of the apparatus performs review without altering outcomes. All three series share one time grid (T=0,4,8,12,16,20,24) as required.
 *
 * PERSPECTIVAL GAP:
 *   From the detaining executive's seat, this reading is a necessary operational adaptation to an adversary that will not play by conventional-war rules — a coordination mechanism letting the security apparatus function without case-by-case paralysis. From the detainee, captured-irregular, and civilian-population seats, the identical structure operates as unreviewable extraction: loss of status, loss of forum, loss of a fixed endpoint to custody. The engine computes these as different seat-level classifications from the same structural data; the divergence is exactly the phenomenon a security-maximization reading needs the corpus to register, not an inconsistency to resolve.
 *
 * DIRECTIONALITY LOGIC:
 *   The detaining executive and intelligence apparatus sit near the full-beneficiary end: they set the classification, control the review process, and capture the operational benefit (unimpeded detention, interrogation, targeting discretion). Private detention contractors sit as secondary beneficiaries whose revenue is downstream of the same suspension. Detainees, captured irregulars, and civilians sit near the full-target end: trapped exit options, no standing in the classification process, and the cost of the necessity determination falls entirely on them. Field commanders are dual-positioned — they gain operational discretion (beneficiary-adjacent) but also bear personal legal exposure if later tribunals revisit their necessity calls (payer-adjacent), which is why they carry a secondary payer role rather than a directionality override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (categorizing fighters who fit neither POW nor civilian categories cleanly) was real at the doctrine's inception, which is why founding_problem_status is authored 'contested' rather than 'dead' — the classification gap has not vanished. But the security-maximization reading's specific response — wholesale suspension rather than the narrower Common Article 3 minimum the treaty's own drafting history anticipated for exactly this ambiguity — is where mandatrophy risk concentrates: the mandate (handle the classification gap) has been used to justify a scope of suspension (indefinite detention, coercive interrogation, degraded civilian immunity) far exceeding what the gap itself requires. Classifying this as tangled_rope rather than snare preserves the fact that a genuine coordination problem exists at the root, while requiring beneficiaries, victims, and active enforcement all be named — which the schema gate itself enforces — keeps the extractive layer from being laundered as pure coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_maximization_kernel_reading,
    'This constraint is one reading (security_maximization_reading) of the contested Geneva Conventions 1949 kernel. The sibling readings are humanitarian_ceiling_reading (Conventions bind as absolute minimums regardless of reciprocity or operational tempo) and conditional_reciprocity_reading (protections scale with the adversary''s own compliance). Which reading correctly characterizes the treaty''s binding force in irregular conflict?',
    'This is not resolvable by further data internal to this story — it is a live jurisprudential and political dispute across international courts, the ICRC, and state practice. Each reading is authored as its own constraint story with its own epsilon and stakeholder structure; resolution would require either a convergent international legal consensus (unlikely given decades of state practice divergence) or a dominant hegemon''s practice becoming customary law by default.',
    'If the humanitarian_ceiling_reading is adopted as controlling law, this constraint''s entire operational premise (that necessity suspends the floor) becomes legally void and the arrangement would need to be reclassified as unlawful extraction rather than a live doctrinal position. If conditional_reciprocity_reading is adopted, this reading''s protections-suspension would be permissible only where adversary non-compliance is individually demonstrated, sharply narrowing the population this constraint currently classifies as unlawful combatants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_maximization_kernel_reading, conceptual, 'Committer-frame ambiguity: which of the three kernel readings (security_maximization, humanitarian_ceiling, conditional_reciprocity) is the legally controlling interpretation of the 1949 Conventions in irregular conflict.').

omega_variable(
    necessity_self_certification,
    'Is the executive''s operational-necessity determination a genuine empirical judgment about battlefield conditions, or is it a self-certifying legal construct that expands whenever the executive finds expansion convenient?',
    'Comparative analysis of necessity determinations across conflicts and administrations: if the scope of ''unlawful combatant'' and detention duration correlates with actual battlefield conditions rather than with political or bureaucratic convenience, the empirical-judgment reading is supported; if scope tracks institutional interest independent of battlefield facts, the self-certifying reading is supported.',
    'If self-certifying, this reading''s extraction is understated by the current epsilon because the suspension has no natural stopping condition — it would tend toward snare rather than tangled_rope over a longer interval as the coordination-function core (the classification gap) is progressively outstripped by extractive scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_self_certification, empirical, 'Whether the necessity determination tracks real battlefield conditions or institutional convenience.').

omega_variable(
    human_shields_doctrine_causal_attribution,
    'When civilian casualties occur in contexts where the detaining/attacking state invokes human-shields doctrine, is the doctrine accurately attributing responsibility to the adversary''s tactics, or is it functioning as a liability-shifting mechanism regardless of actual co-location facts?',
    'Independent forensic investigation of specific strikes, comparing the state''s human-shields attribution against on-the-ground evidence of actual combatant presence and warning/precaution measures taken.',
    'If the doctrine systematically over-attributes to adversary tactics beyond what evidence supports, the civilian-immunity degradation this reading authorizes is extractive beyond even the necessity claim''s own terms; if attribution is generally accurate, the degradation is a genuine (if harsh) consequence of adversary conduct under the conditional-reciprocity logic this reading borrows from.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(human_shields_doctrine_causal_attribution, empirical, 'Whether human-shields attribution tracks battlefield fact or functions as post-hoc liability shifting.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__security_maximization_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, geneva_conventions_1949__security_maximization_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(gene_tr_t4, geneva_conventions_1949__security_maximization_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(gene_tr_t8, geneva_conventions_1949__security_maximization_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(gene_tr_t12, geneva_conventions_1949__security_maximization_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(gene_tr_t16, geneva_conventions_1949__security_maximization_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(gene_tr_t20, geneva_conventions_1949__security_maximization_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(gene_tr_t24, geneva_conventions_1949__security_maximization_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(gene_be_t4, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 4, 0.66).
narrative_ontology:measurement(gene_be_t8, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 8, 0.72).
narrative_ontology:measurement(gene_be_t12, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 12, 0.76).
narrative_ontology:measurement(gene_be_t16, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 16, 0.79).
narrative_ontology:measurement(gene_be_t20, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(gene_be_t24, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 24, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(gene_su_t4, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 4, 0.72).
narrative_ontology:measurement(gene_su_t8, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 8, 0.78).
narrative_ontology:measurement(gene_su_t12, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 12, 0.82).
narrative_ontology:measurement(gene_su_t16, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 16, 0.85).
narrative_ontology:measurement(gene_su_t20, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 20, 0.86).
narrative_ontology:measurement(gene_su_t24, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 24, 0.87).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__security_maximization_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, humanitarian_ceiling_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, conditional_reciprocity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints decomposing the natural-language concept 'the Geneva Conventions' as applied to irregular/asymmetric conflict. The kernel (geneva_conventions_1949) is a single treaty text read three structurally distinct ways: security_maximization_reading (this story — minimal binding force, necessity-based suspension, epsilon=0.81, tangled_rope), humanitarian_ceiling_reading (absolute minimums regardless of reciprocity, expected low epsilon, likely rope or mountain-adjacent from the treaty-drafters' seat), and conditional_reciprocity_reading (protections scale with adversary compliance, expected mid-range epsilon, likely tangled_rope with a narrower victim set than this reading). Each reading has its own beneficiary/victim structure and its own epsilon because the readings make different factual and normative claims about what the treaty requires — they are not the same constraint measured three ways, per the epsilon-invariance principle. All three should link to each other via affects_constraints to preserve the family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
