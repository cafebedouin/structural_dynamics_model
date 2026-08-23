% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__structural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__structural_contraction_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: nuclear_impossibility_kernel__structural_contraction_reading
 *   human_readable: Assured-Destruction War Exclusion (Structural-Contraction Reading)
 *   domain: international_security/strategic_studies
 *
 * SUMMARY:
 *   A thermonuclear arsenal with a secure second-strike force makes the
 *   outcome of a direct great-power war mutual annihilation; on this reading
 *   the conclusion is categorical — there is no rational path to victory
 *   because victory has been removed from the physically reachable set, not
 *   merely priced out of it. The standing arrangement under contest is the
 *   post-1945 order in which peer competitors abstain from direct war,
 *   sustained by continuously maintained assured-destruction forces; conflict
 *   did not disappear but was displaced onto proxy battlefields in the
 *   periphery, and the arrangement's costs — permanent strategic
 *   mobilization, an unremovable tail risk of inadvertent annihilation, the
 *   subordination of unprotected states — are borne diffusely while its
 *   dividends concentrate. This file instantiates one reading of the
 *   nuclear-impossibility kernel (see commentary.kernel_context); the sibling
 *   readings are separate constraints, not components of this one. The
 *   claimed type is mountain because this reading holds the exclusion to be a
 *   physical regularity that would hold regardless of enforcement or belief;
 *   the metrics are authored independently as descriptions of the
 *   arrangement's actual operation — including the maintenance transfers and
 *   peripheral displacement that cut against pure naturality — and the engine
 *   computes each seat's classification from the structural data.
 *   Beneficiaries are declared deliberately: the arrangement presents itself
 *   as natural law while identifiable actors collect from its maintenance,
 *   and the schema-required omega documents that natural-versus-constructed
 *   ambiguity.
 *
 * KEY AGENTS:
 *   - - nuclear_weapon_states: agenda-setter and beneficiary (powerful / identity_locked) — maintain the forces constituting the exclusion, collect the security subsidy, locked from both exit directions.
 *   - - strategic_defense_establishments: primary collecting seat (organized / identity_locked) — budgets, careers, and institutional purpose ride on permanent assurance maintenance.
 *   - - extended_deterrence_umbrella_states: protected clients (institutional / constrained) — receive great-power-war protection while forswearing independent arsenals.
 *   - - citizens_of_nuclear_states: dual beneficiary/payer (moderate / constrained) — hold the peace dividend while funding the forces and carrying an unremovable tail risk.
 *   - - nonaligned_unprotected_states: exposed payers (organized collectively / constrained) — outside every umbrella, hosting competition without protection or agenda power.
 *   - - proxy_war_host_populations: displaced-violence bearers (powerless / trapped) — absorb the armed conflicts the central exclusion displaces onto their territories.
 *   - - humanitarian_disarmament_coalition: excluded objectors (organized / trapped) — majority-of-states bloc behind the prohibition treaty, kept out by boycott.
 *   - - arms_control_institutions: analytical observer (institutional / analytical) — verify counts and manage margins around the standing exclusion.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__structural_contraction_reading, 0.22).
domain_priors:suppression_score(nuclear_impossibility_kernel__structural_contraction_reading, 0.3).
domain_priors:theater_ratio(nuclear_impossibility_kernel__structural_contraction_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__structural_contraction_reading, mountain).
narrative_ontology:human_readable(nuclear_impossibility_kernel__structural_contraction_reading, "Assured-Destruction War Exclusion (Structural-Contraction Reading)").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__structural_contraction_reading, "international_security/strategic_studies").

domain_priors:emerges_naturally(nuclear_impossibility_kernel__structural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__structural_contraction_reading, '3e060264-124c-4fb2-bf0a-4ddfa3e1eddc').
narrative_ontology:cs_kernel_codification('3e060264-124c-4fb2-bf0a-4ddfa3e1eddc', distributed).
narrative_ontology:cs_authority_grounding('3e060264-124c-4fb2-bf0a-4ddfa3e1eddc', distributed).
narrative_ontology:cs_reading_relation('3e060264-124c-4fb2-bf0a-4ddfa3e1eddc', nuclear_impossibility_kernel__rational_dropout_reading, forecloses).
narrative_ontology:cs_reading_relation('3e060264-124c-4fb2-bf0a-4ddfa3e1eddc', nuclear_impossibility_kernel__credibility_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('3e060264-124c-4fb2-bf0a-4ddfa3e1eddc', foundational, mutual_annihilation_physically_guaranteed).
narrative_ontology:cs_axiom_status(mutual_annihilation_physically_guaranteed, holdable).
narrative_ontology:cs_axiom_grounding('3e060264-124c-4fb2-bf0a-4ddfa3e1eddc', mutual_annihilation_physically_guaranteed, empirically_contingent).
narrative_ontology:cs_axiom('3e060264-124c-4fb2-bf0a-4ddfa3e1eddc', foundational, great_power_war_exits_reachable_set).
narrative_ontology:cs_axiom_status(great_power_war_exits_reachable_set, holdable).
narrative_ontology:cs_axiom_grounding('3e060264-124c-4fb2-bf0a-4ddfa3e1eddc', great_power_war_exits_reachable_set, empirically_contingent).
narrative_ontology:cs_reference_frame('3e060264-124c-4fb2-bf0a-4ddfa3e1eddc', war_categorically_excluded_from_reachable_set).
narrative_ontology:cs_drift_state('3e060264-124c-4fb2-bf0a-4ddfa3e1eddc', contemporary_counterforce_and_multipolarity, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('3e060264-124c-4fb2-bf0a-4ddfa3e1eddc', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, strategic_defense_establishments).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, extended_deterrence_umbrella_states).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, citizens_of_nuclear_states).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__structural_contraction_reading, nonaligned_unprotected_states).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__structural_contraction_reading, proxy_war_host_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__structural_contraction_reading, citizens_of_nuclear_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build, maintain, and doctrinally justify assured second-strike arsenals; their declaratory policies and force postures define what the exclusion consists of. They collect a standing security subsidy and status premium from the arrangement's operation. Exit is closed from both directions: dismantling the forces exposes them to conquest-level coercion, and employing them destroys them; great-power identity and treaty-recognized arsenal status are fused with possession.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_weapon_states, agenda_setter,
    powerful, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_weapon_states, beneficiary).

% Operate the missile fields, bomber wings, submarine fleets, and warning networks whose continued functioning keeps retaliation assured. Budgets, careers, promotions, and institutional purpose ride on the permanence of the assurance mission; the organizations have become their function, and repurposing or shrinking them is experienced internally as institutional death.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, strategic_defense_establishments, beneficiary,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__structural_contraction_reading, strategic_defense_establishments, agenda_setter).

% Receive protection against great-power attack under a patron's arsenal while hosting forward basing and forswearing arsenals of their own under the nonproliferation bargain. Leaving the umbrella means facing exposure alone or acquiring weapons, which invites sanction and isolation; staying means accepting dependency and hosting targets.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, extended_deterrence_umbrella_states, beneficiary,
    institutional, generational, constrained, continental).

% Live inside the longest great-power peace on record and receive that dividend without choosing it. They also fund the forces through taxation indefinitely and carry a tail risk of annihilation that relocating abroad cannot remove, since fallout and escalation ignore borders. Political influence over force posture is thin relative to the sums committed.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, citizens_of_nuclear_states, beneficiary,
    moderate, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__structural_contraction_reading, citizens_of_nuclear_states, payer).

% Sit outside every umbrella while superpower competition plays out across their regions. They bear exposure to crises and interventions they did not choose, with collective organizing (the nonaligned and Group-of-77 blocs) that shifted rhetoric more often than outcomes. Joining an umbrella means subordination; acquiring weapons means punishment; remaining outside means absorbing the risks.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, nonaligned_unprotected_states, payer,
    organized, generational, constrained, regional).

% Live on the terrain where the central powers' competition was displaced: Korea, Vietnam, Angola, Afghanistan and others were fought with outsiders' weapons, supplies, and vetoes, producing casualties, displacement, and destroyed infrastructure on their soil. They hold no seat in any council where the conflicts are sponsored or terminated, and flight from the fighting is partial and ruinous.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, proxy_war_host_populations, payer,
    powerless, biographical, trapped, local).

% A majority-of-states bloc plus survivor and civil-society movements that negotiated a treaty prohibiting nuclear weapons, arguing the arrangements' humanitarian consequences and lack of consent make them illegitimate. The arsenal holders boycotted the negotiating conference and lobby allied states against joining; the coalition has moral and numerical weight but no access to the councils where force postures are decided.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, humanitarian_disarmament_coalition, excluded,
    organized, generational, trapped, global).

% Verification bodies, treaty secretariats, and review-conference machinery that count warheads, inspect facilities, and convene the periodic stocktaking. They treat the underlying exclusion as the fixed background condition and manage margins around it: ceilings, telemetry, notification regimes. Their leverage rises and falls with the great powers' appetite for agreements.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, arms_control_institutions, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nuclear_impossibility_kernel__structural_contraction_reading, strategic_defense_establishments).
narrative_ontology:fixing_cost_class(nuclear_impossibility_kernel__structural_contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes coexistence between peer nuclear competitors by removing direct war from the mutually understood option set; substitutes negotiation, signaling, and peripheral competition for the decisive war that historically settled supremacy questions between great powers.
% TRANSFER_FUNCTION: Moves resources continuously from nuclear-state taxpayers to strategic forces and their industrial base as the standing price of keeping retaliation assured; imposes an unpriced annihilation tail risk on all humanity; displaced great-power violence onto proxy-host territories; delivers security and status subsidies to arsenal holders and their protected clients.
% ABSENT_VOICES: The majority of UN member states that backed the 2017 prohibition treaty never consented to the arrangement and were boycotted out of the process by the arsenal holders; hibakusha and downwind communities; the populations of proxy battlegrounds who absorbed the displaced violence; future generations carrying the tail risk. They sit outside the design councils in Washington, Moscow, London, Paris, and Beijing, and hold agenda power only in forums the arsenal holders decline to attend.
% DISAPPEARANCE_RATIONALE: If the exclusion vanished overnight — arsenals suddenly incapable — great-power war re-enters the option set, alliance umbrellas evaporate or convert to independent armament, strategic establishments lose mission and budgets, proxy-sponsorship logic inverts, and unprotected states scramble to hedge. The post-1945 security order is organized around the exclusion and would reorganize around its absence.
% FOUNDING_PROBLEM: After 1945 the founding problem was how rival great powers could avoid a war that would destroy both — a problem previously settled by fighting. The arrangement answers it not by agreement but by engineering: build forces that guarantee the other side's retaliation survives any first strike, so victory ceases to exist as a possible outcome.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by declassified crisis deliberations on all sides (the ExComm tapes, Soviet Politburo records), by former officials' memoirs — including architects who later condemned the arrangement's morality while attesting that it held — and by neutral and nonaligned states' diplomatic archives recording how they planned around a war everyone treated as unwinnable. The humanitarian-disarmament coalition corroborates a different status judgment: that the founding problem was superseded decades ago and the arrangement now outlives it — a live dispute about status, not about the problem's historical reality.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__structural_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__structural_contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__structural_contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nuclear_impossibility_kernel__structural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__structural_contraction_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__structural_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(nuclear_impossibility_kernel__structural_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(nuclear_impossibility_kernel__structural_contraction_reading),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(nuclear_impossibility_kernel__structural_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.22): the recurring costs are real but bounded — permanent strategic-force maintenance funded by nuclear-state taxpayers, an unremovable existential tail risk carried by everyone, and displacement of great-power violence onto peripheral populations — against the universal dividend of no third world war. Suppression is modest (0.30) and structural rather than physical: nobody is coerced into accepting the exclusion, which is self-evident once understood, but the alternatives to the arrangement (verified disarmament, independent arsenals for umbrella states) are institutionally marginalized through treaty gatekeeping, supplier-cartel export controls, and boycott of the prohibition process. Theater is low-moderate (0.25): declaratory signaling, exercise ritual, and civil-defense pageantry coexist with genuinely functional assurance forces. Accessibility collapse is very high (0.88): once secure second strike exists, war-fighting alternatives between peers collapse almost completely — there is no workaround to find, the signature this reading asserts. Resistance is low (0.18): no actor fights through the exclusion, and the abolition movement resisting the arrangement is weak and formally excluded. Suppression is authored as a raw structural property and left unscaled; only extractiveness rides directionality and scope in the engine's arithmetic. All three temporal series share one eight-point grid (1960–2026). The trajectories trace a U: crisis-era burdens ease through détente to a post-Cold-War minimum while theater peaks around 2000 — the constraint running on institutional autopilot, its closest approach to ceremonial maintenance — then burdens and enforcement rebuild as multipolar arsenals and modernization return. Suppression_requirement is tracked because enforcement capacity is genuinely dynamic here: the NPT-era regime was built up after 1968 and is now visibly eroding. The oscillation tracks external geopolitics, not intermittent reinforcement.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the agenda-setting seat (nuclear_weapon_states) the exclusion is an achievement they administer: the per-seat computation lands near the benign end because they collect the subsidy and set the terms. From the collecting seat (strategic_defense_establishments) it is mission and payroll. From citizens it is near-symmetric — dividend and bill arrive together. From unprotected states, and most sharply from proxy-host populations, the same arrangement registers as imposed exposure: costs with no seat at the table. The excluded coalition experiences it as an unconsented imposition maintained by boycott. Same-level differentiation: umbrella states and nonaligned states hold comparable nominal standing as sovereign states yet occupy opposite positions — the constraint-specific variable is alliance geography, not global power, which is why identical nominal rank yields opposite directionalities and exits.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the low-d seats: umbrella states are clean clients (d near 0.10); defense establishments are pure collectors (d near 0.05); nuclear_weapon_states are net beneficiaries but heavily self-costing, so an explicit override sets d to 0.28 — a role-only derivation would anchor them near 0.1 while they simultaneously fund the forces, host the reciprocal hostage populations, and carry their share of the tail risk. Victim declarations drive the high-d seats: proxy_war_host_populations approach the full-target end (powerless, trapped, absorbing the displaced violence); nonaligned_unprotected_states sit high (constrained, exposed, unprotected). citizens_of_nuclear_states carry an override to 0.48 because their dual flows nearly cancel: the peace dividend against indefinite maintenance taxes plus an unremovable tail risk. Overrides are confined to the two seats where declaration-plus-exit derivation would misread a dual-flow or heavily self-costing position.
 *
 * MANDATROPHY ANALYSIS:
 *   The mountain claim protects the analysis from one error and the declared beneficiaries from its mirror. Reading the arrangement as a pure protection racket run by defense establishments would erase its genuine and primary coordination function — peer coexistence without deciding supremacy by war — which is real and load-bearing. Conversely, leaving the naturality claim unchallenged would launder the maintenance transfers and the displaced peripheral violence as mere physics. On the genealogy interview the founding problem — how rival great powers survive each other — is still live, so no resolved-mandatrophy is declared. The post-1991 episode is the cautionary case recorded in the measurement series: when rivalry cooled, theater peaked and the arrangement drifted toward ceremonial maintenance until renewed multipolar competition restored functional urgency, showing the constraint's lived character is regime-dependent even where its physical basis is not.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constructed_vs_physical_origin,
    'Is the exclusion of great-power war a physical regularity that would hold regardless of anyone''s choices, or a constructed condition contingent on the continuing decision to build and maintain assured-destruction arsenals?',
    'Comparative assessment of verified-disarmament feasibility: if inspection technology and treaty architecture could credibly dismantle the arsenals, the constraint is contingent on policy; if no feasible path exists even in principle, the naturality claim stands.',
    'If constructed, the mountain claim fails and the arrangement reclassifies toward a maintained hybrid — coordinated stability financed by extraction — with the declared beneficiaries and payers doing real directional work; if physical, the beneficiary declarations are incidental and the mountain certification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_vs_physical_origin, conceptual, 'Natural law versus maintained artifact: whether the impossibility is physics or policy.').

omega_variable(
    kernel_reading_delta,
    'This story instantiates the structural_contraction_reading of nuclear_impossibility_kernel. Do the sibling readings change the constraint''s structure — does victory remain structurally available at dominated cost (rational_dropout_reading), or does the operative fragility live in threat credibility rather than the payoff set (credibility_paradox_reading)?',
    'Cross-reading structural comparison: test whether any site-expansion cell represents great-power war under this reading (none does — the reachable-set contraction), whether utility shifts could reopen war (the dropout reading says yes), and whether crisis behavior tracks payoff certainty or signaling doubt (the paradox reading).',
    'If rational_dropout is structurally right, this constraint is a reversible cost barrier rather than an impossibility and the mountain claim collapses toward a coordination or cost-barrier type; if credibility_paradox is right, the load-bearing element is psychological and the constraint''s stability varies with leadership perception rather than arsenal physics. This reading''s axioms assert the set-theoretic exclusion; the engine''s foreclosure computation runs against the siblings from these declarations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_delta, conceptual, 'Committer-frame routing: which reading of the nuclear-impossibility kernel is structurally operative.').

omega_variable(
    proxy_substitution_status,
    'Are the peripheral wars of the era substitutions — violence displaced by the central exclusion onto proxy territories — or independent conflicts the arrangement merely failed to prevent?',
    'Counterfactual and archival analysis of superpower arming, funding, and veto patterns in Korea, Vietnam, Angola, and Afghanistan: measure whether the conflicts'' scale, duration, and termination track sponsor decisions that only make sense given the central exclusion.',
    'If substitution, proxy casualties belong in the arrangement''s cost ledger and effective extraction rises materially, weakening the pure-naturality reading; if independent, the victim set shrinks toward taxpayers and tail-risk bearers and the mountain claim strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_substitution_status, empirical, 'Whether displaced proxy violence is a cost of the arrangement or exogenous to it.').

omega_variable(
    existential_tail_risk_netting,
    'Does the standing arrangement impose a net-negative bet on humanity — is the annualized probability of inadvertent or accidental annihilation (command-and-control failure, unauthorized launch, misread warning) large enough that the protection costs more than the wars it prevents?',
    'Declassified incident archives (the 1983 false-alarm watch, Able Archer, the Norwegian rocket incident), fault-tree analysis of command-and-control, and structured expert estimation of annualized use probability against the counterfactual frequency of great-power war.',
    'Materially changes epsilon: if tail risk dominates, the arrangement extracts from all humanity and the mountain claim fails outright; if negligible, the protection reading holds and the low-extraction profile is confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(existential_tail_risk_netting, empirical, 'Net accounting of the arrangement''s catastrophic tail risk against its prevented wars.').

omega_variable(
    second_strike_durability,
    'Does assured second strike remain physically robust under counterforce modernization — hypersonic delivery, conventional prompt strike, cyber attack on command-and-control, AI-enabled targeting — such that mutual annihilation stays guaranteed?',
    'Technical net assessment of retaliatory survivability under emerging counterforce capabilities, tracking first-strike incentive indicators such as vulnerability windows and adoption of launch-on-warning postures.',
    'Erosion of assurance converts the categorical exclusion back into a contested bargaining zone — the set-theoretic axiom weakens and classification migrates toward the rational-dropout structure; durable assurance confirms the mountain claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(second_strike_durability, empirical, 'Whether the physical foundation of the impossibility survives the current modernization wave.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__structural_contraction_reading, 1960, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t1960, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1960, 0.2).
narrative_ontology:measurement(nucl_tr_t1970, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1970, 0.22).
narrative_ontology:measurement(nucl_tr_t1980, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1980, 0.27).
narrative_ontology:measurement(nucl_tr_t1990, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1990, 0.31).
narrative_ontology:measurement(nucl_tr_t2000, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 2000, 0.34).
narrative_ontology:measurement(nucl_tr_t2010, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 2010, 0.32).
narrative_ontology:measurement(nucl_tr_t2020, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 2020, 0.28).
narrative_ontology:measurement(nucl_tr_t2026, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 2026, 0.25).

% Extraction over time
narrative_ontology:measurement(nucl_be_t1960, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1960, 0.34).
narrative_ontology:measurement(nucl_be_t1970, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1970, 0.29).
narrative_ontology:measurement(nucl_be_t1980, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1980, 0.27).
narrative_ontology:measurement(nucl_be_t1990, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1990, 0.23).
narrative_ontology:measurement(nucl_be_t2000, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 2000, 0.17).
narrative_ontology:measurement(nucl_be_t2010, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 2010, 0.15).
narrative_ontology:measurement(nucl_be_t2020, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 2020, 0.19).
narrative_ontology:measurement(nucl_be_t2026, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 2026, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t1960, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1960, 0.15).
narrative_ontology:measurement(nucl_su_t1970, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1970, 0.34).
narrative_ontology:measurement(nucl_su_t1980, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1980, 0.38).
narrative_ontology:measurement(nucl_su_t1990, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1990, 0.37).
narrative_ontology:measurement(nucl_su_t2000, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 2000, 0.43).
narrative_ontology:measurement(nucl_su_t2010, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 2010, 0.41).
narrative_ontology:measurement(nucl_su_t2020, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 2020, 0.35).
narrative_ontology:measurement(nucl_su_t2026, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 2026, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__structural_contraction_reading, global_infrastructure).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel__rational_dropout_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel__credibility_paradox_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the nuclear-impossibility kernel. The colloquial label 'the nuclear revolution' conflates three structurally distinct claims that this corpus holds apart: the structural-contraction claim (this story — war excluded from the reachable set, epsilon low, mountain-claimed), the rational-dropout claim (victory possible but dominated — a reversible utility barrier), and the credibility-paradox claim (fragility located in threat signaling). Epsilon differs sharply across the family: the contraction reading prices the arrangement near coordination cost, the dropout reading prices a controllable risk premium, and the paradox reading prices an unstable signaling equilibrium. Upstream-downstream structure: the physical destructiveness results feed all three, but this reading is cited as settled background by the other two; the edges here declare this story's influence on its siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nuclear_impossibility_kernel__structural_contraction_reading, powerful, 0.28).
constraint_indexing:directionality_override(nuclear_impossibility_kernel__structural_contraction_reading, moderate, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
