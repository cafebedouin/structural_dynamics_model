% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__deterrence_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__deterrence_equilibrium_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: total_war_possibility_space__deterrence_equilibrium_reading
 *   human_readable: Total War Deterrence via Mutual Vulnerability (Equilibrium Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   Under the deterrence_equilibrium reading, total war remains in the
 *   strategic possibility space—calculable, planned for, integrated into
 *   doctrine—but is continuously deterred by mutual vulnerability.
 *   Nuclear-armed great powers maintain arsenals, war-fighting doctrine, and
 *   escalation procedures to credibly signal that initiating total war would
 *   result in unacceptable retaliation. This reading contrasts with the
 *   nuclear_taboo reading (total war is normatively prohibited) and the
 *   space_contraction reading (total war is cognitively unavailable). The
 *   deterrence reading predicts active doctrine development, counterforce
 *   targeting refinement, and continuous investment in signaling credible
 *   escalation capability. Non-nuclear states and extended-deterrence allies
 *   benefit from reduced total-war risk but pay by accepting constraints on
 *   their own strategic autonomy. The constraint is tangled_rope because
 *   genuine coordination (mutual avoidance of extinction-level conflict) is
 *   entangled with asymmetric extraction (great powers retain escalation
 *   monopoly, smaller states lose it). The claim is independent of the
 *   metrics: this reading asserts that war remains reachable; the metrics
 *   measure how much extraction and suppression the constraint actually
 *   generates.
 *
 * KEY AGENTS:
 *   - Nuclear-armed great powers: agenda-setters and beneficiaries; maintain deterrent credibility through doctrine and capability signaling.
 *   - Extended-deterrence allies: beneficiaries receiving security guarantees; constrained exit (cannot credibly threaten umbrella withdrawal).
 *   - Non-nuclear states: payers and victims; lose escalation-to-total-war option; identity-locked into non-nuclear posture by proliferation enforcement and regime-change threat.
 *   - Regional hegemons without nuclear capability: payers; prevented from escalating conflicts to existential level by vulnerability to great-power intervention.
 *   - Civilians under umbrella: payer-beneficiary; protected from total war but hostage to deterrence stability.
 *   - Strategic planners and doctrine writers: agenda-setters; benefit from institutional investment in deterrence infrastructure.
 *   - Anti-nuclear movements: excluded; argue total war should be removed from strategic thinking, not deterred through vulnerability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__deterrence_equilibrium_reading, 0.68).
domain_priors:suppression_score(total_war_possibility_space__deterrence_equilibrium_reading, 0.71).
domain_priors:theater_ratio(total_war_possibility_space__deterrence_equilibrium_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__deterrence_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__deterrence_equilibrium_reading, "Total War Deterrence via Mutual Vulnerability (Equilibrium Reading)").
narrative_ontology:topic_domain(total_war_possibility_space__deterrence_equilibrium_reading, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_possibility_space__deterrence_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__deterrence_equilibrium_reading, 'e6c48771-dfe7-4cf3-a226-54f51a8c240f').
narrative_ontology:cs_kernel_codification('e6c48771-dfe7-4cf3-a226-54f51a8c240f', distributed).
narrative_ontology:cs_authority_grounding('e6c48771-dfe7-4cf3-a226-54f51a8c240f', extraction).
narrative_ontology:cs_interpretation_layer_present('e6c48771-dfe7-4cf3-a226-54f51a8c240f').
narrative_ontology:cs_reading_relation('e6c48771-dfe7-4cf3-a226-54f51a8c240f', total_war_possibility_space__nuclear_taboo_reading, coexists_with).
narrative_ontology:cs_reading_relation('e6c48771-dfe7-4cf3-a226-54f51a8c240f', total_war_possibility_space__space_contraction_reading, influences).
narrative_ontology:cs_axiom('e6c48771-dfe7-4cf3-a226-54f51a8c240f', foundational, total_war_remains_strategically_reachable).
narrative_ontology:cs_axiom_status(total_war_remains_strategically_reachable, holdable).
narrative_ontology:cs_axiom_grounding('e6c48771-dfe7-4cf3-a226-54f51a8c240f', total_war_remains_strategically_reachable, empirically_contingent).
narrative_ontology:cs_axiom('e6c48771-dfe7-4cf3-a226-54f51a8c240f', foundational, mutual_vulnerability_creates_rational_deterrence).
narrative_ontology:cs_axiom_status(mutual_vulnerability_creates_rational_deterrence, holdable).
narrative_ontology:cs_axiom_grounding('e6c48771-dfe7-4cf3-a226-54f51a8c240f', mutual_vulnerability_creates_rational_deterrence, instrumental).
narrative_ontology:cs_reference_frame('e6c48771-dfe7-4cf3-a226-54f51a8c240f', mutual_vulnerability_as_strategic_rationality).
narrative_ontology:cs_drift_state('e6c48771-dfe7-4cf3-a226-54f51a8c240f', contemporary_post_cold_war_proliferation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e6c48771-dfe7-4cf3-a226-54f51a8c240f', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_armed_great_powers).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, extended_deterrence_allies).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, non_nuclear_states).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, regional_hegemons_without_arsenal).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, civilian_populations_under_umbrella).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, civilian_populations_under_umbrella).
narrative_ontology:constraint_vindicates(total_war_possibility_space__deterrence_equilibrium_reading, strategic_stability_doctrine).
narrative_ontology:constraint_vindicates(total_war_possibility_space__deterrence_equilibrium_reading, mutual_assured_destruction_credibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess operational nuclear arsenals and the strategic doctrine framing total war as calculable deterrent cost. Continuously invest in war-fighting capability, doctrine refinement, and counterforce targeting postures to signal credible escalation threat. They benefit from the constraint by locking rivals into strategic caution without renouncing warfighting capacity. Their exit is trapped—possession creates the permanent vulnerability they rely on for deterrence; unilateral disarmament breaks the equilibrium they depend on.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_armed_great_powers, agenda_setter,
    institutional, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_armed_great_powers, beneficiary).

% Depend on nuclear umbrella pledges from great powers. They receive security guarantees against peer or superior conventional military challengers under the assumption that total war remains deterred by mutual vulnerability. Their exit is constrained: developing independent nuclear capability is expensive, proliferation-controlled, and diplomatically costly; remaining under umbrella means accepting the risk calculus others set.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, extended_deterrence_allies, beneficiary,
    organized, generational, constrained, regional).

% Subject to the strategic architecture built on nuclear deterrence without possessing escalation tools themselves. Their exit is identity-locked: acquiring nuclear capability triggers international sanctions, regime change pressure, and proliferation enforcement; remaining non-nuclear means accepting a permanent subordinate position in conflicts with nuclear-armed or nuclear-backed states. Their strategic choices are compressed to sub-total-war conflict modes.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, non_nuclear_states, payer,
    moderate, generational, identity_locked, regional).

% Possess conventional military dominance in their region but are structurally prevented from escalating to total war by the knowledge that an existential threat might invoke nuclear umbrella response from a great power. This constraint operates asymmetrically: they cannot threaten existential war against smaller neighbors without triggering nuclear-backed retaliation risk; smaller neighbors gain security from this asymmetry but remain subject to conventional dominance. Their exit is constrained—renouncing the regional hegemon role means catastrophic loss of influence and resources.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, regional_hegemons_without_arsenal, payer,
    powerful, biographical, constrained, regional).

% Live in territories covered by nuclear deterrence guarantees and benefit from the reduction in total-war risk between great powers. Simultaneously, they remain hostages to the deterrence equilibrium: they cannot credibly threaten defection without losing their protection, and they absorb any escalation risk if deterrence fails. Their identity is fused to the protected status (national identity under umbrella, childhood socialization to the guarantee structure); exit would mean relocation or regime change.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, civilian_populations_under_umbrella, payer,
    powerless, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__deterrence_equilibrium_reading, civilian_populations_under_umbrella, beneficiary).

% Maintain the theoretical and operational infrastructure of deterrence: counterforce targeting doctrine, escalation ladder theory, war-gaming of total-war scenarios, and continuous refinement of command-and-control procedures. They have mobility (can shift to civilian research, other militaries, or private sector) but are embedded in institutional career tracks that reward mastery of the deterrence framework. They benefit from the constraint's persistence because it justifies continued investment in their expertise and infrastructure.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, strategic_planners_and_military_doctrine_writers, agenda_setter,
    institutional, generational, mobile, global).

% Argue that nuclear deterrence is not a stable equilibrium but a ticking mechanism toward catastrophe; that the constraint is illusory because rationality assumptions can fail; that total war should be removed from strategic possibility space through nuclear disarmament rather than deterred through vulnerability. They are excluded from the decision-making structure that maintains the constraint (arsenal operations, strategic doctrine, great-power negotiation). Their absence from the enforcing seats enables the deterrence reading to persist unchallenged in operational planning.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, anti_nuclear_advocacy_movements, excluded,
    organized, generational, mobile, global).

% Monitor compliance with non-proliferation frameworks and inspect facilities. They observe the constraint's operation from a formal enforcement position but lack authority over great-power arsenals or doctrine. They take testimony about what the constraint means (deterrent or taboo or space contraction) but their role is bounded to non-proliferation verification, not to adjudicating the reading of the kernel.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, international_nuclear_governance_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_armed_great_powers).
narrative_ontology:fixing_cost_class(total_war_possibility_space__deterrence_equilibrium_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents mutual escalation to total war by establishing credible mutual vulnerability: each side's capacity to inflict unacceptable damage on the other creates a cost-benefit calculation that keeps war below the extinction threshold. The coordination problem solved is: how to maintain strategic competition without triggering all-out commitment to mutual destruction. The mechanism is continuous signaling of second-strike capability and willingness to escalate in tit-for-tat response.
% TRANSFER_FUNCTION: Moves strategic autonomy and escalation control from non-nuclear and non-aligned states to the nuclear-armed great powers. Non-nuclear states transfer their freedom to wage existential conflict (and to credibly threaten it) in exchange for protection against total war initiated by others. Extended-deterrence allies transfer sovereignty over their most extreme responses in exchange for security guarantees. The great powers retain the monopoly on calculable extinction-level threat.
% ABSENT_VOICES: Anti-nuclear advocacy movements, disarmament scholars, and states that believe total war can be removed from strategic possibility through norm-setting (not deterrence) are excluded from the strategic planning and doctrine-writing seats. They argue the constraint is a temporary and fragile equilibrium masquerading as stable strategy, but their arguments do not reach the audiences that make doctrine or arsenal decisions. Non-nuclear states attempting to acquire nuclear capability are structurally excluded by proliferation enforcement, which operates to maintain the constraint.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared—if mutual vulnerability ceased to deter, or if one side acquired a first-strike disarming capability—the strategic landscape would reorganize catastrophically. Doctrine writers would activate escalation procedures; regional hegemons would pursue existential conflict against neighbors; extended allies would either acquire independent arsenals or capitulate. The entire post-1945 architecture of managed great-power competition would shatter. The constraint's persistence is the foundation of the current international order.
% FOUNDING_PROBLEM: After 1945, nuclear weapons created the possibility of total war at civilizational scale. The founding problem was: how to maintain great-power competition and deterrence without triggering mutual annihilation? The deterrence reading's founding solution: establish mutual vulnerability as credible, calculate war costs as unacceptable, and maintain this equilibrium through continuous signaling and capability maintenance.
% FOUNDING_PROBLEM_CORROBORATION: Strategic planners, military strategists, and deterrence theorists (Schelling, Jervis, Waltz) attest the founding problem remains live—escalation risk persists and deterrence doctrine is actively maintained. Anti-nuclear scholars and disarmament advocates attest the founding problem is falsely framed: they argue total war should be removed from strategic thinking entirely, not deterred through vulnerability. States experiencing proliferation enforcement attest the constraint persists by active suppression of alternatives. No independent corroboration exists from outside the strategic establishment; the constraint's persistence is ratified by those who maintain it, observed with skepticism by those excluded from its defense.
narrative_ontology:disappearance_verdict(total_war_possibility_space__deterrence_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__deterrence_equilibrium_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__deterrence_equilibrium_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(total_war_possibility_space__deterrence_equilibrium_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__deterrence_equilibrium_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_possibility_space__deterrence_equilibrium_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_possibility_space__deterrence_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.41 to 0.68 over the interval because the constraint's enforcement machinery (deterrent signaling, escalation doctrine, proliferation control) must intensify as the equilibrium becomes contested. Early-interval extractiveness is lower because the Cold War deterrence consensus was less challenged. Late-interval extractiveness is higher because deterrence must be actively maintained against disarmament pressure, proliferation attempts, and doubts about escalation credibility. Suppression rises from 0.52 to 0.71 because maintaining the deterrence equilibrium requires active suppression of alternatives: nuclear acquisition attempts by non-nuclear states, escalation of regional conflicts above conventional thresholds, and challenges to great-power escalation doctrine. Theater_ratio (0.28 to 0.42) reflects growing performative elements: public signaling of deterrent credibility, war-gaming exercises staged for reassurance, and doctrine refinement that serves institutional continuity more than operational necessity. Accessibility_collapse (0.38) is moderate because alternatives (disarmament, regional nuclear capability, norm-based taboo) remain conceptually available even if politically suppressed. Resistance (0.61) is substantial because anti-nuclear movements, non-nuclear states, and scholars contest the deterrence reading itself.
 *
 * PERSPECTIVAL GAP:
 *   The great-power perspective experiences the constraint as coordination—mutual commitment to rational escalation calculation protects all from accident or miscalculation. The non-nuclear-state perspective experiences the constraint as forced subordination: loss of escalation options without agreement to that loss. The agenda-setter (strategic planners) perspective experiences the constraint as technical management of a dangerous equilibrium. The anti-nuclear perspective experiences the constraint as cover story for permanent war-readiness and proliferation control. These divergences are rooted in structural position: only the agenda-setters control escalation decisions; only the payers bear costs they did not choose. The engine computes d separately for each seat from power/exit/beneficiary-victim data; where perspectives diverge sharply, d should differentiate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear-armed great powers: d approaches 0.0 (full beneficiary) because they benefit from the constraint without paying its costs, set the agenda, and maintain trapped-exit status that locks rivals into vulnerability. Extended-deterrence allies: d approaches 0.3–0.4 (moderate beneficiary with constrained cost) because they benefit from reduced total-war risk but lose escalation autonomy and pay this cost continuously. Non-nuclear states: d approaches 0.8–0.9 (near full target) because they bear costs (constrained escalation, proliferation punishment, subordinate status) in an arrangement they did not negotiate and cannot exit without regime risk. Regional hegemons without arsenal: d approaches 0.7 (high target) because their escalation options are constrained and they bear the cost of restraint while great powers retain escalation monopoly. Civilians under umbrella: d approaches 0.5 (symmetric) because they benefit from reduced extinction risk but absorb escalation risk if deterrence fails and their identity is fused to the protected status, making exit identity-locked. Strategic planners: d approaches 0.2 (beneficiary) because institutional investment and career paths depend on constraint persistence. Anti-nuclear movements: d approaches 1.0 if counted as a seat (excluded, bearing suppression cost) but should not be counted as a stakeholder in this story because they are excluded from the decision structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The deterrence reading's mandate is: keep total war calculable but deterred through mutual vulnerability. This mandate is live—the constraint persists as long as doctrine writers maintain escalation procedures and great powers maintain arsenals. However, the mandate contains internal tension: if deterrence works perfectly, escalation doctrine becomes theater (maintained for signaling, not operational use). If deterrence fails, the mandate fails catastrophically. The rising theater_ratio (0.28 to 0.42) suggests mandatrophy may be emerging: a growing portion of the constraint's operation is performative (war-gaming, doctrine refinement, strategic signaling) rather than functional (actually planning for and executing deterrent response). The constraint is not yet mandatrophic (the mandate of mutual-vulnerability deterrence is still operational and contested), but the trend toward theater suggests that if the constraint persists long enough without an actual escalation crisis, it may calcify into Piton (maintaining deterrence doctrine as institutional theater rather than as live strategic response).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_stability_empirical_contingency,
    'Does the deterrence equilibrium remain stable if rationality assumptions fail, information asymmetries widen, or accidental systems trigger unauthorized escalation?',
    'Historical analysis of near-miss incidents (Cuban Missile Crisis, false alarms, command-and-control failures); game-theoretic models of bounded rationality; studies of decision-making under extreme time pressure and uncertainty.',
    'If deterrence stability depends on rationality assumptions that are empirically fragile, the constraint is reclassified as temporally contingent (less stable than authored, shorter time horizon). If near-miss analysis shows the system has self-correcting mechanisms, deterrence stability is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_stability_empirical_contingency, empirical, 'Whether the deterrence equilibrium''s stability is robust to rationality failure or contingent on assumptions that can break.').

omega_variable(
    reading_differentiation_kernel_scope,
    'What structural evidence distinguishes the deterrence_equilibrium reading from the nuclear_taboo reading and space_contraction reading? Is war really in planning space (deterrence), or has it been removed from thinkability (taboo/space_contraction)?',
    'Discourse analysis of military doctrine and strategic planning: What do doctrine writers say war looks like? Do they describe war as strategically possible but rationally deterred, or as normatively unthinkable, or as outside strategic imagination altogether? Do counterforce targeting plans exist? Are escalation ladders theorized? Textual analysis of what the strategic community treats as reachable.',
    'If doctrine writers describe total war as strategically calculable and doctrinally planned (counterforce, escalation response), the deterrence reading holds and is distinguished from taboo/space_contraction. If doctrine describes war as normatively prohibited independent of cost, the nuclear_taboo reading is more accurate. If doctrine describes war as cognitively unavailable (not entertained in planning), space_contraction is more accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_differentiation_kernel_scope, conceptual, 'Whether total war is in strategic planning space (deterrence), normatively prohibited (taboo), or cognitively removed (space contraction).').

omega_variable(
    suppression_mechanism_enforcement_vs_internalization,
    'Is the constraint''s suppression of total-war initiation maintained by external enforcement (active deterrent threat, command-and-control procedures, escalation doctrine) or by internalized norm (taboo, cultural shift, civilizational learning)?',
    'Policy experiments and natural experiments: Do states under weaker external deterrent threat still refrain from total war? Do states with stronger internal anti-war norms show lower escalation rates in conflicts? Do generations socialized post-Cold War show different risk tolerance for nuclear-backed escalation than Cold War generations?',
    'If suppression is primarily external enforcement, the constraint''s persistence depends on maintained deterrent credibility and capability. If suppression is primarily internalized, the constraint could persist even with degraded external enforcement. The distinction affects how robustly the constraint persists if deterrent capability were to decline.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_enforcement_vs_internalization, empirical, 'Whether suppression is maintained by external deterrent enforcement or internalized norm shift.').

omega_variable(
    beneficiary_identification_contradiction,
    'Do the declared beneficiaries (nuclear-armed great powers, extended-deterrence allies) actually benefit from the constraint, or do they benefit from the possibility of total war deterred by vulnerability—i.e., do they benefit from the kernel (total war possibility space) rather than from the constraint itself?',
    'Compare welfare under three scenarios: (1) total war impossible (space_contraction reading); (2) total war deterred by vulnerability (deterrence reading); (3) total war thinkable and calculable with low cost. Do great powers maximize welfare in scenario (2) or would they prefer scenario (1) or (3)? Do extended allies prefer being under vulnerable-based umbrella or under norm-based prohibition?',
    'If beneficiaries actually prefer the constraint to be read as taboo (not deterred), the stated beneficiary structure is misleading and the constraint''s persistence is driven by different interests. If beneficiaries genuinely prefer vulnerability-based deterrence, the beneficiary identification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identification_contradiction, preference, 'Whether beneficiaries truly benefit from vulnerability-based deterrence or would prefer alternative readings of the kernel.').

omega_variable(
    kernel_reading_contest_incompleteness,
    'Are the three declared sibling readings (deterrence_equilibrium, nuclear_taboo, space_contraction) exhaustive of the possible readings, or do other interpretations of the total-war possibility-space kernel exist?',
    'Systematic review of strategic studies literature, international relations theory, and nuclear policy discourse. Identify all distinct claims about what constrains total war and whether any cannot be mapped to one of the three readings.',
    'If other readings exist and are live in the strategic community, the constraint family is incomplete and additional constraint stories should be authored. If the three readings are exhaustive, the kernel family is complete.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_incompleteness, conceptual, 'Whether the three declared readings exhaust the interpretations of the total-war-possibility kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__deterrence_equilibrium_reading, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t0, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(tota_tr_t0, observed).
narrative_ontology:measurement(tota_tr_t8, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement_basis(tota_tr_t8, observed).
narrative_ontology:measurement(tota_tr_t16, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement_basis(tota_tr_t16, observed).
narrative_ontology:measurement(tota_tr_t24, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement_basis(tota_tr_t24, observed).
narrative_ontology:measurement(tota_tr_t32, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement_basis(tota_tr_t32, observed).

% Extraction over time
narrative_ontology:measurement(tota_be_t0, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 0, 0.41).
narrative_ontology:measurement_basis(tota_be_t0, observed).
narrative_ontology:measurement(tota_be_t8, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement_basis(tota_be_t8, observed).
narrative_ontology:measurement(tota_be_t16, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 16, 0.57).
narrative_ontology:measurement_basis(tota_be_t16, observed).
narrative_ontology:measurement(tota_be_t24, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 24, 0.63).
narrative_ontology:measurement_basis(tota_be_t24, observed).
narrative_ontology:measurement(tota_be_t32, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement_basis(tota_be_t32, observed).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t0, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(tota_su_t0, observed).
narrative_ontology:measurement(tota_su_t8, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement_basis(tota_su_t8, observed).
narrative_ontology:measurement(tota_su_t16, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 16, 0.64).
narrative_ontology:measurement_basis(tota_su_t16, observed).
narrative_ontology:measurement(tota_su_t24, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement_basis(tota_su_t24, observed).
narrative_ontology:measurement(tota_su_t32, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement_basis(tota_su_t32, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__deterrence_equilibrium_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_possibility_space__deterrence_equilibrium_reading, 0.14).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space__nuclear_taboo_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space__space_contraction_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_proliferation_enforcement_regime).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, extended_deterrence_alliance_structure).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'total_war_possibility_space'. The three readings (deterrence_equilibrium, nuclear_taboo, space_contraction) form a constraint family linked by network.affects_constraints. The deterrence_equilibrium reading is upstream of nuclear_taboo and space_contraction because it asserts the factual condition (war remains reachable) that the other readings interpret normatively or cognitively. The deterrence reading also affects proliferation enforcement and extended deterrence structures because they are operationalized to maintain the deterrence equilibrium.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_possibility_space__deterrence_equilibrium_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
