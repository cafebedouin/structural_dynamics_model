% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__dropping_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__dropping_reading, []).

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
 *   constraint_id: total_war_reachability_boundary__dropping_reading
 *   human_readable: Total War Reachability Boundary (Dropping Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   Total war between great powers has dropped in probability since the early
 *   Cold War but remains structurally reachable through nuclear arsenals and
 *   strategic doctrines. Under the dropping_reading of the
 *   total_war_reachability_boundary kernel, deterrence operates as a
 *   coordination equilibrium (rope) rather than an immutable physical law
 *   (mountain). Yet the arrangement asymmetrically benefits nuclear-armed
 *   states and their security clients while imposing existential risk on
 *   global civilian populations, producing a tangled_rope structure. Sibling
 *   readings dispute whether total war has become impossible
 *   (contraction_reading) or whether reachability is merely an atrophied,
 *   reversible condition (contingent_reachability_reading).
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states: agenda_setter/beneficiary (institutional power, constrained exit) â administer arsenals and gain credibility
 *   - extended_deterrence_clients: beneficiary (organized power, constrained exit) â receive security umbrella without indigenous arsenals
 *   - civilian_populations_under_threat: payer (powerless, trapped exit) â bear existential risk without agency
 *   - disarmament_advocacy_networks: excluded (organized power, constrained exit) â structurally marginalized in strategic discourse
 *   - strategic_studies_community: observer (analytical power, analytical exit) â frames and analyzes the equilibrium
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__dropping_reading, 0.58).
domain_priors:suppression_score(total_war_reachability_boundary__dropping_reading, 0.52).
domain_priors:theater_ratio(total_war_reachability_boundary__dropping_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, accessibility_collapse, 0.46).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__dropping_reading, tangled_rope).
narrative_ontology:human_readable(total_war_reachability_boundary__dropping_reading, "Total War Reachability Boundary (Dropping Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__dropping_reading, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_reachability_boundary__dropping_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__dropping_reading, '2c4d2be4-83bf-4684-85d6-080e3b1d0ecc').
narrative_ontology:cs_kernel_codification('2c4d2be4-83bf-4684-85d6-080e3b1d0ecc', distributed).
narrative_ontology:cs_authority_grounding('2c4d2be4-83bf-4684-85d6-080e3b1d0ecc', distributed).
narrative_ontology:cs_reading_relation('2c4d2be4-83bf-4684-85d6-080e3b1d0ecc', total_war_reachability_boundary__contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('2c4d2be4-83bf-4684-85d6-080e3b1d0ecc', total_war_reachability_boundary__contingent_reachability_reading, coexists_with).
narrative_ontology:cs_axiom('2c4d2be4-83bf-4684-85d6-080e3b1d0ecc', foundational, nuclear_deterrence_as_coordination_equilibrium).
narrative_ontology:cs_axiom_status(nuclear_deterrence_as_coordination_equilibrium, holdable).
narrative_ontology:cs_axiom_grounding('2c4d2be4-83bf-4684-85d6-080e3b1d0ecc', nuclear_deterrence_as_coordination_equilibrium, empirically_contingent).
narrative_ontology:cs_axiom('2c4d2be4-83bf-4684-85d6-080e3b1d0ecc', foundational, total_war_persistent_reachability).
narrative_ontology:cs_axiom_status(total_war_persistent_reachability, holdable).
narrative_ontology:cs_axiom_grounding('2c4d2be4-83bf-4684-85d6-080e3b1d0ecc', total_war_persistent_reachability, empirically_contingent).
narrative_ontology:cs_reference_frame('2c4d2be4-83bf-4684-85d6-080e3b1d0ecc', stable_deterrence_equilibrium).
narrative_ontology:cs_drift_state('2c4d2be4-83bf-4684-85d6-080e3b1d0ecc', contemporary_strategic_environment, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2c4d2be4-83bf-4684-85d6-080e3b1d0ecc', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, extended_deterrence_clients).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, civilian_populations_under_threat).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain nuclear arsenals, command-and-control infrastructure, and strategic doctrines that keep total war reachable. Set deterrence posture and escalation thresholds. Derive security, geopolitical leverage, and regime credibility from the credible threat of nuclear use. Cannot unilaterally disarm without risking relative vulnerability and alliance collapse.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__dropping_reading, nuclear_weapon_states, beneficiary).

% Receive security guarantees from nuclear-armed patrons, avoiding the cost and political burden of indigenous nuclear programs. Their national security is tied to the patron's deterrence credibility. Opting out would require either proliferating independently or accepting strategic vulnerability in a contested region.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, extended_deterrence_clients, beneficiary,
    organized, generational, constrained, continental).

% Live in cities and regions that are targeted by nuclear planning. Bear the existential risk of deterrence failure, accidental launch, or escalation without any corresponding voice in strategic decision-making. No individual or collective exit from the threat environment is available; the risk is atmospheric and territorial.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, civilian_populations_under_threat, payer,
    powerless, biographical, trapped, global).

% Advocate for nuclear abolition, alternative security architectures, and treaty-based disarmament. Structurally excluded from nuclear policy planning, strategic dialogues, and doctrinal reviews. Their participation would challenge the foundational premises of deterrence doctrine and the legitimacy of maintaining reachability.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, disarmament_advocacy_networks, excluded,
    organized, generational, constrained, global).

% Analyze deterrence stability, escalation dynamics, and war scenarios. Produce the intellectual frameworks that justify or critique the total-war reachability boundary. Do not directly bear costs or capture benefits; influence is discursive and operates through policy uptake, publication, and education of strategic elites.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, strategic_studies_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_reachability_boundary__dropping_reading, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(total_war_reachability_boundary__dropping_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preventing total war between nuclear-armed great powers by making the cost of all-out victory prohibitive through mutual threat of destruction; stabilizing great-power competition at levels below civilization-scale war.
% TRANSFER_FUNCTION: Moves security and geopolitical leverage from civilian populationsâwho bear the existential downside risk of deterrence failureâto nuclear-armed states and their allies, who gain deterrence credibility. Also moves economic resources from general taxation to arsenal maintenance, delivery systems, and strategic infrastructure.
% ABSENT_VOICES: Civilian populations residing in targeted areas are not represented in strategic discourse or crisis decision-making. Disarmament advocates and abolitionist diplomatic coalitions are structurally excluded from deterrence policy planning and doctrinal review.
% DISAPPEARANCE_RATIONALE: If total war became structurally unreachableâor if the deterrence equilibrium dissolvedâthe architecture of great-power relations, alliance systems, military spending, and strategic doctrine would fundamentally reorder. The current international hierarchy and extended deterrence relationships depend on the boundary being actively maintained.
% FOUNDING_PROBLEM: Preventing recurrence of total industrial war between great powers in an era of nuclear weapons capable of civilization-scale destruction, as witnessed in 1945.
% FOUNDING_PROBLEM_CORROBORATION: Cold War historians attest to the acute risk of US-Soviet total war in the early nuclear era. Contemporary peace researchers argue the founding problem has evolved rather than persisted, while strategic studies institutions defend continued necessity. The International Campaign to Abolish Nuclear Weapons and Global South diplomatic coalitions corroborate the historical reality of the founding problem while contesting that the current arrangement remains necessary.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__dropping_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__dropping_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__dropping_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_reachability_boundary__dropping_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__dropping_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__dropping_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_reachability_boundary__dropping_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_reachability_boundary__dropping_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate-high: the constraint diverts massive resources to arsenals and imposes irreducible existential risk on populations to produce security for states. Suppression (0.52) reflects the marginalization of disarmament alternatives and the ideological enforcement of deterrence orthodoxy. Theater_ratio (0.28) is low-moderate: much deterrence activity is functionally real, but signaling and posture reviews have performative dimensions. Accessibility_collapse (0.46) is moderate: alternatives like global zero are conceptually available but treated as strategically naive in policy discourse. Resistance (0.40) reflects persistent anti-nuclear movements and Global South dissatisfaction without structural leverage. Temporal measurements show a modest decline in extractiveness as war probability dropped, coupled with slowly rising theater and declining suppression requirement as the equilibrium institutionalized.
 *
 * PERSPECTIVAL GAP:
 *   From the nuclear_weapon_states seat, the constraint is a necessary coordination mechanism that prevents total war and stabilizes great-power relations. From the civilian_populations_under_threat seat, the same constraint is an existential threat imposed without consent or exit. Extended_deterrence_clients see security benefits; disarmament_advocacy_networks see a captured discourse. The engine computes these divergences from the structural asymmetry in power, exit options, and beneficiary/victim position, not from narrative framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear_weapon_states are structural beneficiaries (security, leverage, status) with constrained exit because unilateral disarmament invites vulnerability. Extended_deterrence_clients are beneficiaries (security without arsenals) with constrained exit because abandoning the umbrella requires costly realignment. Civilian_populations_under_threat are the victims: they bear the catastrophic downside risk of deterrence failure without agency in the arrangement, and their exit_options are trapped because the threat environment is global. Disarmament networks are excluded rather than coordinated; their exclusion is part of the suppression structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents a false summit by refusing to treat deterrence as a mountain (natural law). Deterrence is not gravity; it is a socially reproduced equilibrium requiring active maintenance of arsenals, doctrines, and crisis-management institutions. The tangled_rope classification captures that the coordination function (preventing total war) is genuine but inseparable from asymmetric extraction (populations under threat, resource diversion to arsenals). If the coordination function were cleanly separable from the victimization, it might rate as rope; if the war-prevention story were pure cover for extraction, it would rate as snare. The tangled_rope verdict reflects the structural entanglement of genuine coordination and genuine extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_sibling_delta,
    'This constraint is the dropping_reading of kernel total_war_reachability_boundary. The contraction_reading claims total war has left the feasible set entirely, while the contingent_reachability_reading treats current reachability as a technology-dependent piton. What structural classification would follow if either sibling reading were adopted?',
    'Cross-reading comparison of compiled constraint stories; evaluation of whether total-war capability is technically abolishable or only politically dormant.',
    'Adopting contraction_reading would dissolve this constraint (no reachability, no parties). Adopting contingent_reachability_reading would reclassify as piton (atrophied function with potential revival).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_sibling_delta, conceptual, 'Committee frame: structural delta from sibling readings of the same kernel').

omega_variable(
    deterrence_equilibrium_stability,
    'Is the deterrence equilibrium a self-sustaining coordination outcome that would persist even if great powers stopped investing in arsenal modernization, or does it require continuous costly investment to maintain credibility?',
    'Historical case analysis of deterrence breakdowns and non-use episodes; analysis of modernization spending relative to credibility judgments.',
    'If self-sustaining, the constraint trends toward rope. If it requires active costly maintenance that extracts from populations while benefiting state actors, tangled_rope is reinforced and mountain/rope classifications are falsified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_equilibrium_stability, empirical, 'Whether deterrence stability is passive or actively produced').

omega_variable(
    reachability_extraction_necessity,
    'Does the coordination function of preventing total war structurally require maintaining civilian populations as potential victims, or could the same non-use outcome be achieved without existential threat to populations?',
    'Theoretical analysis of counterforce-only postures and alternative security architectures; assessment of whether deterrence logic inseparably requires targeting populations.',
    'If population threat is inseparable, extraction is inherent to the coordination. If separable, current arrangement layers extractive victimization onto a cleaner coordination mechanism, increasing snare-like features.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reachability_extraction_necessity, conceptual, 'Whether deterrence coordination inherently requires population-level victimization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__dropping_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twrbd_tr_t0, total_war_reachability_boundary__dropping_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(twrbd_tr_t10, total_war_reachability_boundary__dropping_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(twrbd_tr_t20, total_war_reachability_boundary__dropping_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(twrbd_tr_t30, total_war_reachability_boundary__dropping_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(twrbd_tr_t40, total_war_reachability_boundary__dropping_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(twrbd_tr_t50, total_war_reachability_boundary__dropping_reading, theater_ratio, 50, 0.32).

% Extraction over time
narrative_ontology:measurement(twrbd_be_t0, total_war_reachability_boundary__dropping_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(twrbd_be_t10, total_war_reachability_boundary__dropping_reading, base_extractiveness, 10, 0.67).
narrative_ontology:measurement(twrbd_be_t20, total_war_reachability_boundary__dropping_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(twrbd_be_t30, total_war_reachability_boundary__dropping_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(twrbd_be_t40, total_war_reachability_boundary__dropping_reading, base_extractiveness, 40, 0.56).
narrative_ontology:measurement(twrbd_be_t50, total_war_reachability_boundary__dropping_reading, base_extractiveness, 50, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(twrbd_su_t0, total_war_reachability_boundary__dropping_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(twrbd_su_t10, total_war_reachability_boundary__dropping_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(twrbd_su_t20, total_war_reachability_boundary__dropping_reading, suppression_requirement, 20, 0.59).
narrative_ontology:measurement(twrbd_su_t30, total_war_reachability_boundary__dropping_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(twrbd_su_t40, total_war_reachability_boundary__dropping_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(twrbd_su_t50, total_war_reachability_boundary__dropping_reading, suppression_requirement, 50, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, contraction_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, contingent_reachability_reading).

% DUAL FORMULATION NOTE:
% One of three readings of the total_war_reachability_boundary kernel, decomposed because the natural-language label 'total war reachability' conflates structurally distinct claims about whether total war remains possible, impossible, or conditionally atrophied.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
