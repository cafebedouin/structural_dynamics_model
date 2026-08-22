% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__unable_unwilling_doctrine_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__unable_unwilling_doctrine_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: article_51_self_defense__unable_unwilling_doctrine_reading
 *   human_readable: Unable/Unwilling Host-State Doctrine of Self-Defense (Article 51 Reading)
 *   domain: international_law/security_studies/constitutional_interpretation
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested Article 51 kernel:
 *   the 'unable or unwilling' doctrine, which permits a state to use force
 *   against a non-state actor inside another state's territory when that host
 *   state has failed to suppress the threat itself. This reading is
 *   deliberately moderate — narrower than the expansive preventive reading
 *   (which would authorize force absent any completed attack) but broader
 *   than the narrow armed-attack reading (which requires state attribution
 *   for the original attack). The doctrine emerged prominently in state
 *   practice after 2001 and has been invoked with increasing frequency and
 *   geographic reach since, most visibly in cross-border strikes against
 *   non-state armed groups in weak or contested states. The extractiveness
 *   and enforcement metrics authored here describe the standing arrangement
 *   as it has come to operate — not the narrow-armed-attack alternative and
 *   not the preventive alternative, which are separate constraints (separate
 *   files) linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - intervening_states_with_counterterrorism_mandates: primary agenda-setter and beneficiary — determines the threshold and acts on it unilaterally
 *   - host_states_with_bypassed_sovereignty: primary target — sovereignty is bypassed on the intervening state's own determination
 *   - civilian_populations_in_host_territory: bear the immediate physical costs without voice
 *   - non_state_armed_groups: the nominal object of the doctrine, structurally excluded from the legal contest
 *   - un_security_council: nominal collective-authorization body the doctrine is invoked to route around
 *   - international_law_scholars_and_icj: analytical observers assessing whether state practice has crystallized the doctrine into custom
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__unable_unwilling_doctrine_reading, 0.61).
domain_priors:suppression_score(article_51_self_defense__unable_unwilling_doctrine_reading, 0.58).
domain_priors:theater_ratio(article_51_self_defense__unable_unwilling_doctrine_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__unable_unwilling_doctrine_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__unable_unwilling_doctrine_reading, "Unable/Unwilling Host-State Doctrine of Self-Defense (Article 51 Reading)").
narrative_ontology:topic_domain(article_51_self_defense__unable_unwilling_doctrine_reading, "international_law/security_studies/constitutional_interpretation").

domain_priors:requires_active_enforcement(article_51_self_defense__unable_unwilling_doctrine_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__unable_unwilling_doctrine_reading, '32ba8456-2741-48b3-86cf-911f37b47368').
narrative_ontology:cs_kernel_codification('32ba8456-2741-48b3-86cf-911f37b47368', fixed_text).
narrative_ontology:cs_authority_grounding('32ba8456-2741-48b3-86cf-911f37b47368', distributed).
narrative_ontology:cs_reading_relation('32ba8456-2741-48b3-86cf-911f37b47368', article_51_self_defense__narrow_armed_attack_reading, forecloses).
narrative_ontology:cs_reading_relation('32ba8456-2741-48b3-86cf-911f37b47368', article_51_self_defense__expansive_preventive_reading, influences).
narrative_ontology:cs_axiom('32ba8456-2741-48b3-86cf-911f37b47368', foundational, host_state_failure_substitutes_for_attribution).
narrative_ontology:cs_axiom_status(host_state_failure_substitutes_for_attribution, holdable).
narrative_ontology:cs_axiom_grounding('32ba8456-2741-48b3-86cf-911f37b47368', host_state_failure_substitutes_for_attribution, conventional).
narrative_ontology:cs_axiom('32ba8456-2741-48b3-86cf-911f37b47368', foundational, completed_attack_required_but_state_responsibility_not_required).
narrative_ontology:cs_axiom_status(completed_attack_required_but_state_responsibility_not_required, holdable).
narrative_ontology:cs_axiom_grounding('32ba8456-2741-48b3-86cf-911f37b47368', completed_attack_required_but_state_responsibility_not_required, empirically_contingent).
narrative_ontology:cs_reference_frame('32ba8456-2741-48b3-86cf-911f37b47368', post_9_11_state_practice_baseline).
narrative_ontology:cs_drift_state('32ba8456-2741-48b3-86cf-911f37b47368', contemporary_drone_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('32ba8456-2741-48b3-86cf-911f37b47368', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_counterterrorism_mandates).
narrative_ontology:constraint_beneficiary(article_51_self_defense__unable_unwilling_doctrine_reading, targeted_state_domestic_security_apparatus).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, host_states_with_bypassed_sovereignty).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, civilian_populations_in_host_territory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invoke the doctrine to justify airstrikes, drone campaigns, or ground incursions into another state's territory when a non-state armed group operating there has attacked them and the host government has not suppressed it. They control the evidentiary threshold for 'unwilling or unable,' select the timing and scope of response, and bear no binding external adjudication before acting.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_counterterrorism_mandates, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_counterterrorism_mandates, beneficiary).

% Typically weaker or fragile states whose territorial integrity is breached by a stronger state's unilateral determination that they failed to police non-state actors within their borders. They may dispute the 'unwilling or unable' finding, but have no forum with binding authority over the intervening state and limited capacity to resist militarily or diplomatically.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, host_states_with_bypassed_sovereignty, payer,
    moderate, biographical, constrained, national).

% Live in the areas where the non-state actor operates and where the intervening state's strikes land. They bear collateral harm from the response without having participated in the underlying attack and without any voice in whether the doctrine's threshold was correctly applied to their government.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, civilian_populations_in_host_territory, payer,
    powerless, immediate, trapped, local).

% The actors whose attacks trigger the doctrine. They operate across the host state's territory, often relocating in response to strikes, and are not parties to the international-law contest over the doctrine's legitimacy even though the entire structure exists to address them.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, non_state_armed_groups, excluded,
    organized, biographical, mobile, regional).

% Nominally the body charged with authorizing collective responses to threats to peace, but the unable/unwilling doctrine is invoked precisely to bypass the need for Security Council authorization, since Article 51 self-defense is self-executing pending Council action. The Council can condemn or ratify after the fact but rarely constrains the initial determination.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, un_security_council, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__unable_unwilling_doctrine_reading, un_security_council, excluded).

% Assess state practice and opinio juris to determine whether the doctrine has crystallized into customary international law or remains a contested unilateral assertion. Their rulings and scholarship shape legitimacy but bind no state that has not consented to jurisdiction.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, international_law_scholars_and_icj, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_counterterrorism_mandates).
narrative_ontology:fixing_cost_class(article_51_self_defense__unable_unwilling_doctrine_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal pathway for states to respond to genuine cross-border non-state actor attacks (terrorism, armed insurgency) when the territorial host state lacks the capacity or the will to neutralize the threat itself, filling a gap left by the narrow armed-attack reading's requirement of state attribution.
% TRANSFER_FUNCTION: Moves the practical authority to determine when force is justified in another state's territory from that host state (and from collective UN Security Council authorization) to the unilateral judgment of the intervening state; costs of the resulting military action (casualties, destroyed infrastructure, sovereignty violation) fall on the host state and its civilian population.
% ABSENT_VOICES: The host state's own assessment of its capacity and willingness is rarely treated as dispositive; affected civilians have no forum at all; smaller and non-aligned states as a bloc have repeatedly objected in UN debates that the doctrine is applied asymmetrically against weaker states while never invoked against powerful ones, but this objection has not altered the doctrine's operation by dominant military powers.
% DISAPPEARANCE_RATIONALE: If the unable/unwilling doctrine were repudiated overnight, states with counterterrorism mandates would lose their primary legal justification for cross-border strikes against non-state actors in non-consenting host states; they would need to fall back on host-state consent, Security Council authorization, or the narrower armed-attack/attribution standard, substantially reducing the frequency and geographic reach of unilateral counterterrorism operations.
% FOUNDING_PROBLEM: Traditional Article 51 doctrine, built around state-to-state armed attack, left an apparent gap when non-state actors launched attacks from within a state that either could not or would not suppress them (post-9/11 Afghanistan being the paradigm case), leaving victim states seemingly unable to respond lawfully to a real security threat.
% FOUNDING_PROBLEM_CORROBORATION: A minority of international law scholars and several non-aligned states attest that the underlying security gap remains genuinely live in cases of true state collapse; a larger body of scholarship, ICJ dicta (Armed Activities on the Territory of the Congo), and statements from host states and their allies attest that the doctrine has been stretched well past collapsed-state scenarios into a general license for unilateral force against politically disfavored governments, with the founding problem serving increasingly as post-hoc justification.
narrative_ontology:disappearance_verdict(article_51_self_defense__unable_unwilling_doctrine_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__unable_unwilling_doctrine_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__unable_unwilling_doctrine_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_51_self_defense__unable_unwilling_doctrine_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__unable_unwilling_doctrine_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__unable_unwilling_doctrine_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__unable_unwilling_doctrine_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__unable_unwilling_doctrine_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.61) reflects that the doctrine's benefit (a lawful basis for a real, historically demonstrated security gap) is real but has been progressively extended by state practice to situations well beyond the paradigm collapsed-state case, shifting the balance toward unilateral extraction of sovereignty from weaker states. Suppression (0.58) captures that host states and civilian populations have no binding forum to contest an intervening state's factual determination of 'unwilling or unable' — the determination is essentially self-certifying. Theater ratio (0.40) and its upward drift reflect that post-hoc legal justifications (state department legal opinions, doctrine papers) have grown as a share of the activity relative to genuine case-by-case necessity assessment, without the underlying enforcement machinery becoming more constrained.
 *
 * PERSPECTIVAL GAP:
 *   From the intervening state's seat, this reading is the only legally coherent response to a demonstrated security failure by the host state — coordination, not extraction. From the host state's and civilian population's seats, the same structure is a unilateral determination made by a more powerful actor with no meaningful opportunity to contest it before harm occurs. The engine computes these as different seat-level types from the same structural data; the divergence is the finding, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Intervening states sit near the beneficiary end: they set the threshold, act on their own assessment, and bear minimal binding accountability (arbitrage-grade exit from any adverse legal consequence). Host states and civilian populations sit near the target end: sovereignty and physical safety are the things extracted, and their exit options are constrained-to-trapped respectively. Non-state armed groups are excluded rather than positioned on the beneficiary/victim axis proper — they are the doctrine's occasion, not its party.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a genuine post-9/11 gap where victim states faced non-state attacks from ungoverned or complicit territory) was real and arguably remains live in true state-collapse scenarios. But because the doctrine's threshold ('unwilling or unable') is self-certified by the intervening state with no binding review, the arrangement is structurally exposed to being invoked in situations far from the founding paradigm — a mandatrophy risk the tangled_rope classification is built to flag: genuine coordination function (filling a real security gap) coexisting with asymmetric extraction (unilateral sovereignty bypass) sustained by active enforcement (military capacity, diplomatic cover) rather than consent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unable_unwilling_evidentiary_threshold,
    'What evidentiary standard, if any, should govern a state''s determination that a host state is ''unwilling or unable'' to suppress a non-state actor threat, and who adjudicates disputes over whether that standard was met?',
    'Systematic comparison of invocations (Afghanistan 2001, Syria post-2014, Pakistan drone campaign) against a codified standard, ideally reviewed by an independent international body rather than self-certified by the intervening state.',
    'A binding external evidentiary standard would shift the doctrine toward genuine coordination (rope-like); continued self-certification without review sustains the tangled_rope/extraction reading and risks drift toward the expansive preventive reading in practice even where the moderate reading is claimed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unable_unwilling_evidentiary_threshold, empirical, 'Whether the doctrine''s core threshold is externally verifiable or self-certified.').

omega_variable(
    doctrine_customary_status_ambiguity,
    'Has the unable/unwilling doctrine crystallized into customary international law through consistent state practice and opinio juris, or does it remain a contested unilateral assertion by a subset of powerful states?',
    'ICJ adjudication of a contentious case squarely presenting the question, or a comprehensive survey of state practice and explicit government legal positions (not just conduct) across the full UN membership, not only the states that invoke the doctrine.',
    'If customary status is established, the doctrine''s legitimacy and coordination function strengthen considerably; if it remains contested and geographically concentrated among militarily powerful states, the tangled_rope classification''s extraction component is reinforced and the doctrine looks more like great-power privilege than settled law.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrine_customary_status_ambiguity, conceptual, 'Whether the reading reflects settled custom or an unresolved unilateral claim.').

omega_variable(
    reading_selection_under_determination,
    'Given that state practice cited in support of the unable/unwilling doctrine often overlaps with practice that could equally be characterized as expansive/preventive (e.g., strikes justified partly by imminence of future attacks), is the moderate reading''s boundary against the preventive reading actually stable in application, or does it collapse under scrutiny of specific cases?',
    'Case-by-case reconstruction of the stated legal justification versus the actual factual predicate (completed attack vs. anticipated future attack) for a representative sample of invocations.',
    'If the boundary collapses in practice, this reading and the expansive_preventive_reading may not be as structurally distinct in operation as they are in doctrine, which would require re-examining whether they are truly separate constraints or a single constraint with contested self-description.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_under_determination, conceptual, 'Whether the moderate reading''s line against the preventive reading holds up against actual state practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__unable_unwilling_doctrine_reading, 2001, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t2001, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2001, 0.2).
narrative_ontology:measurement(arti_tr_t2005, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2005, 0.26).
narrative_ontology:measurement(arti_tr_t2009, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2009, 0.31).
narrative_ontology:measurement(arti_tr_t2014, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2014, 0.35).
narrative_ontology:measurement(arti_tr_t2019, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2019, 0.38).
narrative_ontology:measurement(arti_tr_t2024, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(arti_be_t2001, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2001, 0.42).
narrative_ontology:measurement(arti_be_t2005, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2005, 0.48).
narrative_ontology:measurement(arti_be_t2009, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2009, 0.53).
narrative_ontology:measurement(arti_be_t2014, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2014, 0.57).
narrative_ontology:measurement(arti_be_t2019, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2019, 0.6).
narrative_ontology:measurement(arti_be_t2024, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2024, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t2001, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2001, 0.35).
narrative_ontology:measurement(arti_su_t2005, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2005, 0.42).
narrative_ontology:measurement(arti_su_t2009, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2009, 0.47).
narrative_ontology:measurement(arti_su_t2014, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2014, 0.52).
narrative_ontology:measurement(arti_su_t2019, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2019, 0.56).
narrative_ontology:measurement(arti_su_t2024, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__unable_unwilling_doctrine_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_51_self_defense__unable_unwilling_doctrine_reading, 0.1).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, narrow_armed_attack_reading).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, expansive_preventive_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the article_51_self_defense kernel. narrow_armed_attack_reading forecloses this reading's core unilateral-determination premise by requiring binding state attribution; expansive_preventive_reading is influenced by this reading in the sense that successful normalization of the unable/unwilling threshold lowers the practical and rhetorical barrier to preventive invocations, without this reading endorsing that extension. Each reading carries its own independently authored ε, beneficiaries, and victims; do not average or reconcile ε across the three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
