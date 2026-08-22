% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__state_centric_reading, []).

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
 *   constraint_id: combatant_status_definition__state_centric_reading
 *   human_readable: State-Centric Combatant Status Doctrine (Third Geneva Convention Article 4)
 *   domain: international_humanitarian_law/law_of_armed_conflict
 *
 * SUMMARY:
 *   This story authors the state-centric reading of the combatant status
 *   kernel: Article 4 of the Third Geneva Convention grants POW status only
 *   to fighters organized under formal state military structure meeting its
 *   enumerated criteria, categorically excluding non-state armed group
 *   members regardless of their organization, command discipline, or
 *   observance of the laws of war. This reading treats the state/non-state
 *   line as the operative test and does not evaluate the national-liberation
 *   reading (which would extend status to AP I Article 1(4) groups) or the
 *   functional-protection reading (which would make treatment
 *   status-independent under Common Article 3) — those are separate
 *   constraints, linked here by network edges, each with its own ε.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__state_centric_reading, 0.71).
domain_priors:suppression_score(combatant_status_definition__state_centric_reading, 0.78).
domain_priors:theater_ratio(combatant_status_definition__state_centric_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(combatant_status_definition__state_centric_reading, "State-Centric Combatant Status Doctrine (Third Geneva Convention Article 4)").
narrative_ontology:topic_domain(combatant_status_definition__state_centric_reading, "international_humanitarian_law/law_of_armed_conflict").

domain_priors:requires_active_enforcement(combatant_status_definition__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__state_centric_reading, 'e236bf49-f056-4196-b643-1f80382353b5').
narrative_ontology:cs_kernel_codification('e236bf49-f056-4196-b643-1f80382353b5', fixed_text).
narrative_ontology:cs_authority_grounding('e236bf49-f056-4196-b643-1f80382353b5', lineage).
narrative_ontology:cs_interpretation_layer_present('e236bf49-f056-4196-b643-1f80382353b5').
narrative_ontology:cs_reading_relation('e236bf49-f056-4196-b643-1f80382353b5', combatant_status_definition__national_liberation_reading, coexists_with).
narrative_ontology:cs_reading_relation('e236bf49-f056-4196-b643-1f80382353b5', combatant_status_definition__functional_protection_reading, influences).
narrative_ontology:cs_axiom('e236bf49-f056-4196-b643-1f80382353b5', foundational, combatant_status_requires_state_affiliation).
narrative_ontology:cs_axiom_status(combatant_status_requires_state_affiliation, holdable).
narrative_ontology:cs_axiom_grounding('e236bf49-f056-4196-b643-1f80382353b5', combatant_status_requires_state_affiliation, conventional).
narrative_ontology:cs_axiom('e236bf49-f056-4196-b643-1f80382353b5', secondary, organizational_form_not_conduct_determines_status).
narrative_ontology:cs_axiom_status(organizational_form_not_conduct_determines_status, holdable).
narrative_ontology:cs_axiom_grounding('e236bf49-f056-4196-b643-1f80382353b5', organizational_form_not_conduct_determines_status, conventional).
narrative_ontology:cs_reference_frame('e236bf49-f056-4196-b643-1f80382353b5', id_1949_state_military_formalism).
narrative_ontology:cs_drift_state('e236bf49-f056-4196-b643-1f80382353b5', post_ap1_decolonization_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e236bf49-f056-4196-b643-1f80382353b5', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__state_centric_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, state_militaries).
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, state_parties_to_geneva_conventions).
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, regular_armed_force_personnel).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, non_state_armed_group_fighters).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, irregular_resistance_combatants).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, captured_insurgents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, detaining_power_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Personnel meeting Article 4 criteria (uniform, command hierarchy, open carrying of arms, adherence to laws of war) receive automatic POW status upon capture: no prosecution for lawful acts of war, repatriation at conflict's end, protections against coercive interrogation. State parties negotiated and continue to interpret the treaty text, giving them control over both the rule and its authoritative reading.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, state_militaries, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__state_centric_reading, state_militaries, agenda_setter).

% Drafted, ratified, and continue to interpret Article 4 through diplomatic conferences, military manuals, and domestic implementing legislation. Control the formal amendment process and dominate the ICRC commentary process that shapes customary interpretation. Bear no direct cost from the categorical exclusion of non-state actors and gain a durable legal tool for delegitimizing armed challengers to incumbent governments.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, state_parties_to_geneva_conventions, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Fighters organized under a chain of command, sometimes wearing distinguishing insignia and carrying arms openly, are nonetheless categorically excluded from Article 4 unless they meet the additional strict criteria the state-centric reading applies narrowly. On capture they face prosecution under ordinary domestic criminal law (treason, terrorism, murder) for the same combat acts that would be lawful for a state soldier. They have no exit from the classification — it attaches by virtue of what organization they fought for, not what they did.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, non_state_armed_group_fighters, payer,
    powerless, biographical, trapped, national).

% Resistance fighters against occupation or colonial administration who lack formal state backing are read out of Article 4 protection under this reading regardless of the justice of their cause. Captured, they face the detaining power's domestic law rather than the law of armed conflict, and coercive interrogation and summary punishment are not treaty violations under this reading because no protected status attached in the first place.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, irregular_resistance_combatants, payer,
    powerless, biographical, trapped, national).

% Individuals already in detention experience the classification as the difference between a POW camp with Geneva-mandated conditions and indefinite domestic detention or capital prosecution. They have no agency over the classification; it was fixed by the structure of the group they belonged to before capture ever occurred.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, captured_insurgents, payer,
    powerless, immediate, trapped, local).

% Governments fighting internal insurgencies or occupying foreign territory benefit doubly: they retain full domestic prosecutorial discretion over captured non-state fighters while their own uniformed forces retain full POW protection if captured by a state adversary. The categorical exclusion is a strategic asset in asymmetric conflicts.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, detaining_power_governments, beneficiary,
    institutional, generational, arbitrage, national).

% Document detention conditions and advocate for at least Common Article 3 minimum treatment where full combatant status is denied, but have no enforcement power over the classification itself and must operate within whatever detaining powers permit.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, icrc_and_humanitarian_monitors, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(combatant_status_definition__state_centric_reading, state_militaries).
narrative_ontology:fixing_cost_class(combatant_status_definition__state_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a bright-line, administrable test for who qualifies for POW status, letting detaining powers and international monitors apply a single objective criterion (formal state military organization) rather than adjudicating the legitimacy of each armed group's cause case by case.
% TRANSFER_FUNCTION: Moves legal protection and immunity from prosecution away from non-state fighters and toward state militaries, converting the same category of combat conduct into either a lawful act of war (for state soldiers) or a domestic crime (for non-state fighters), independent of the conduct's actual conformity to the laws of war.
% ABSENT_VOICES: Non-state armed groups, resistance movements, and their captured fighters had no seat at the 1949 Geneva drafting table (dominated by state parties) and remain absent from the interpretive bodies (state military legal advisors, ICRC state-party consultations) that continue to shape the doctrine's application.
% DISAPPEARANCE_RATIONALE: If the state-centric reading disappeared and combatant status attached to conduct and organization rather than to state affiliation, prosecutorial practice toward captured non-state fighters would shift dramatically toward POW-style treatment, undermining a key legal tool states currently use to delegitimize internal and anti-colonial armed opposition; the entire architecture of counterinsurgency legal strategy would need to be rebuilt.
% FOUNDING_PROBLEM: In 1949, drafters sought to distinguish uniformed soldiers fighting under disciplined command from irregular fighters and spies, in order to preserve incentives for combatants to follow the laws of war and to prevent abuse of POW status by anyone claiming it.
% FOUNDING_PROBLEM_CORROBORATION: State military legal advisors and government delegations attest the distinction remains necessary to preserve discipline-incentive structures and prevent status abuse. Independent legal scholars, ICRC commentary (post-1977 Additional Protocols), and UN human rights bodies attest that AP I Article 1(4) and functional-protection jurisprudence already demonstrate the categorical state/non-state line is not necessary to achieve the drafters' stated aims, and that its persistence increasingly serves incumbent-government interests rather than the founding problem.
narrative_ontology:disappearance_verdict(combatant_status_definition__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__state_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(combatant_status_definition__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__state_centric_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(combatant_status_definition__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(combatant_status_definition__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.71 by interval end) because the same combat conduct is treated as lawful for one class of fighter and criminal for another, purely as a function of institutional affiliation rather than conduct or organization. Suppression is authored higher still (0.78) because maintaining the categorical exclusion requires active state cooperation across domestic courts, military tribunals, and diplomatic consensus at treaty-revision conferences — the line does not hold itself. Theater ratio is modest (0.28) because the coordination function (a bright-line test for detaining powers) is genuinely operative, not merely performed; the extraction rides on top of a real administrability function rather than replacing it. All three metrics are authored on one shared time grid across the post-1949 interval, reflecting how the state-centric reading hardened in application as it was invoked more aggressively against 20th-century decolonization insurgencies and later counterterrorism detentions.
 *
 * PERSPECTIVAL GAP:
 *   From the state military and state-party seat, Article 4 reads as coordination: a stable, symmetric, mutually beneficial rule every state accepts for its own soldiers' protection, which happens also to deny status to non-state actors as a side effect of a neutral administrability test. From the non-state fighter seat, the identical rule reads as enforced asymmetric extraction: the same combat conduct is legal for one party and criminal for the other, and the criterion sorting them was drafted and is interpreted exclusively by the parties who benefit from the sorting. The engine computes these as different seat-level classifications from the same structural data; the divergence is real, not an authoring artifact.
 *
 * DIRECTIONALITY LOGIC:
 *   State militaries and the state parties that drafted and continue to interpret Article 4 sit at the beneficiary end: they receive automatic protected status for their own personnel while retaining full prosecutorial discretion over captured non-state fighters — a genuinely asymmetric structural position, not merely a favorable one. Non-state armed group fighters, irregular resistance combatants, and captured insurgents sit at the target end: their exit options are trapped by construction, since the classification attaches to organizational affiliation fixed before capture, not to any choice available to the individual fighter post-capture. Detaining-power governments benefit doubly as both potential POW-status recipients (their own soldiers) and potential prosecutors (of captured non-state fighters), which is why they are listed as an independent beneficiary rather than folded into state_militaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (distinguishing disciplined uniformed combatants from spies and irregulars to preserve incentives for lawful conduct) remains partially live — states still have legitimate interest in distinguishing organized forces from unconstrained irregulars. But the founding problem's status is contested precisely because AP I Article 1(4) and functional-protection frameworks already demonstrate that the incentive-preservation goal can be achieved without the categorical state/non-state line — organization and command-responsibility criteria could apply to non-state groups directly. The persistence of the strict state-centric line past the point where equally administrable alternatives exist is the mandatrophy signal: a founding problem that could be solved more narrowly is instead solved by a categorical exclusion that also, not incidentally, preserves state monopoly on legitimate violence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_centric_line_necessity_vs_convenience,
    'Is the categorical state/non-state exclusion structurally necessary to preserve combatant-discipline incentives, or is it a convenient byproduct that primarily serves incumbent-government interests in delegitimizing internal and anti-colonial armed opposition?',
    'Comparative analysis of state practice under AP I Article 1(4) (which already extends conduct-based criteria to some non-state groups) versus practice under the strict Article 4 line: if discipline and laws-of-war compliance outcomes are comparable, the categorical exclusion is not necessary to the stated aim.',
    'If not necessary, the state-centric reading''s coordination justification collapses to a residual administrability convenience riding on top of a primarily extractive/delegitimizing function — strengthening the tangled_rope classification toward snare at the analytical seat. If necessary, the coordination function is more substantial than the extraction critique allows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_centric_line_necessity_vs_convenience, empirical, 'Whether the categorical exclusion is functionally required or merely convenient for incumbents.').

omega_variable(
    which_reading_is_the_kernel_committer_frame,
    'Is the state-centric reading the historically primary/default reading of Article 4 (with national-liberation and functional-protection readings as later contestations), or is it one contested reading among three co-equal readings with no privileged default?',
    'Track the sequence of authoritative interpretive acts: 1949 original ratification practice, 1977 AP I adoption and ratification rates, subsequent ICRC customary law study findings, and ICC/ICTY jurisprudence applying combatant-status tests.',
    'If state-centric is the historical default, the sibling readings are properly understood as revisionist pressure on an established kernel reading (supporting an ''influences'' relation from state-centric toward the siblings). If genuinely co-equal and contested from the outset, the relation is better characterized as pure coexistence with no directional pressure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_reading_is_the_kernel_committer_frame, conceptual, 'Whether this reading holds a privileged default position or is one of three genuinely co-equal contested readings.').

omega_variable(
    natural_law_vs_constructed_sovereignty_tool,
    'Does the state monopoly on legitimate violence that this reading vindicates reflect a natural feature of organized political order, or is it a constructed doctrine whose persistence is explained by the concentrated benefit it confers on incumbent state militaries and governments?',
    'Historical and comparative political theory analysis of pre-Westphalian and non-state-centric conflict regulation regimes, and assessment of whether alternative combatant-recognition regimes (organization/command-based rather than state-affiliation-based) produce comparably stable conflict-regulation outcomes.',
    'This constraint does not claim mountain status, so FSM does not apply directly — but the vindicated proposition (state_monopoly_on_legitimate_violence) risks being treated as natural fact in adjacent doctrinal discourse. Resolving this clarifies whether that adjacent naturalization is itself a false-summit pattern worth authoring as a separate constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_sovereignty_tool, conceptual, 'Whether the vindicated state-monopoly doctrine is natural political order or a constructed, beneficiary-serving arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__state_centric_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t0, combatant_status_definition__state_centric_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(comb_tr_t15, combatant_status_definition__state_centric_reading, theater_ratio, 15, 0.15).
narrative_ontology:measurement(comb_tr_t30, combatant_status_definition__state_centric_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(comb_tr_t45, combatant_status_definition__state_centric_reading, theater_ratio, 45, 0.22).
narrative_ontology:measurement(comb_tr_t60, combatant_status_definition__state_centric_reading, theater_ratio, 60, 0.25).
narrative_ontology:measurement(comb_tr_t75, combatant_status_definition__state_centric_reading, theater_ratio, 75, 0.28).

% Extraction over time
narrative_ontology:measurement(comb_be_t0, combatant_status_definition__state_centric_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(comb_be_t15, combatant_status_definition__state_centric_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(comb_be_t30, combatant_status_definition__state_centric_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(comb_be_t45, combatant_status_definition__state_centric_reading, base_extractiveness, 45, 0.66).
narrative_ontology:measurement(comb_be_t60, combatant_status_definition__state_centric_reading, base_extractiveness, 60, 0.69).
narrative_ontology:measurement(comb_be_t75, combatant_status_definition__state_centric_reading, base_extractiveness, 75, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t0, combatant_status_definition__state_centric_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(comb_su_t15, combatant_status_definition__state_centric_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement(comb_su_t30, combatant_status_definition__state_centric_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(comb_su_t45, combatant_status_definition__state_centric_reading, suppression_requirement, 45, 0.73).
narrative_ontology:measurement(comb_su_t60, combatant_status_definition__state_centric_reading, suppression_requirement, 60, 0.76).
narrative_ontology:measurement(comb_su_t75, combatant_status_definition__state_centric_reading, suppression_requirement, 75, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__state_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, combatant_status_definition__national_liberation_reading).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, combatant_status_definition__functional_protection_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints decomposing the natural-language concept 'combatant status under IHL' per the ε-invariance principle. The three readings share a kernel (Article 4 / the combatant status definition) but diverge structurally: state_centric_reading (this story) authors high ε for non-state fighters and low ε for state militaries under a categorical organizational test; national_liberation_reading authors a conduct/command-based extension test lowering ε for AP I Article 1(4) qualifying groups; functional_protection_reading authors near-zero ε variance across combatant status entirely, since Common Article 3 minimums are held to apply regardless of status. Each story carries its own beneficiary/victim structure and its own claimed_type; none averages across the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
