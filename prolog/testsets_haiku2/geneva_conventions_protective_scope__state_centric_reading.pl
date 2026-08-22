% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__state_centric_reading, []).

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
 *   constraint_id: geneva_conventions_protective_scope__state_centric_reading
 *   human_readable: State-Centric Geneva Protections: Uniformed Combatant Criterion
 *   domain: legal/international_humanitarian_law
 *
 * SUMMARY:
 *   This constraint instantiates the state-centric reading of the Geneva
 *   Conventions' protective scope. The constraint is: 'Geneva protections
 *   apply to uniformed combatants under responsible command meeting Article 4
 *   criteria; unprivileged belligerents fall outside treaty scope.' This
 *   reading privileges clarity and bright-line legal rules: a fighter either
 *   meets the Article 4 uniform/insignia/command/accountability criteria or
 *   does not; if not, the fighter is not entitled to POW status, medical
 *   protections, or prohibitions on prosecution for mere participation in
 *   hostilities. The reading benefits conventional state militaries fighting
 *   asymmetric conflicts by permitting targeting of non-uniformed fighters
 *   without extending combatant immunity protections. The constraint is one
 *   reading of a contested kernel (the Geneva Conventions' actual scope);
 *   sibling readings include the universal_rights_reading (which extends
 *   protections to all persons regardless of combatant status) and the
 *   hybrid_proportionality_reading (which scales protections by conflict type
 *   and proportionality analysis). This story authors the state-centric
 *   reading only—clean, ε-invariant, with its own beneficiary/victim
 *   structure—and routes the committer structure (kernel identity, sibling
 *   relationships, axiom status) to omega variables and cs_structure per
 *   Rules 1–4.
 *
 * KEY AGENTS:
 *   - Conventional state militaries: institutional power, arbitrage exit, agenda-setter role — interpret and enforce the uniformed criterion, benefit from exclusion of non-state actors from POW protections
 *   - Non-state armed groups: moderate power, identity-locked exit, payer role — excluded from combatant status, face targeting without immunity protections, structurally unable to meet uniform criterion without compromising organizational legitimacy
 *   - Unprivileged belligerents: powerless, trapped exit, payer role — individual fighters without formal rank/insignia/command accountability, targetable without restriction, denied POW status
 *   - Civilian populations in asymmetric conflict: powerless, constrained exit, dual role (payer + incidental beneficiary) — experience enforcement through increased targeting of non-uniformed fighters, bear collateral effects
 *   - International humanitarian law courts: institutional power, analytical exit, observer role — interpret and apply the criterion in war crimes proceedings, shape enforcement but lack power to set the fundamental standard
 *   - Human rights advocacy organizations: organized power, constrained exit, excluded role — argue for universal protections regardless of combatant status, excluded from standard-setting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__state_centric_reading, 0.68).
domain_priors:suppression_score(geneva_conventions_protective_scope__state_centric_reading, 0.72).
domain_priors:theater_ratio(geneva_conventions_protective_scope__state_centric_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__state_centric_reading, "State-Centric Geneva Protections: Uniformed Combatant Criterion").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__state_centric_reading, "legal/international_humanitarian_law").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__state_centric_reading, '8c469275-a1b0-4d2b-8385-7f1016aa4af8').
narrative_ontology:cs_kernel_codification('8c469275-a1b0-4d2b-8385-7f1016aa4af8', formalized).
narrative_ontology:cs_authority_grounding('8c469275-a1b0-4d2b-8385-7f1016aa4af8', lineage).
narrative_ontology:cs_interpretation_layer_present('8c469275-a1b0-4d2b-8385-7f1016aa4af8').
narrative_ontology:cs_reading_relation('8c469275-a1b0-4d2b-8385-7f1016aa4af8', geneva_conventions_protective_scope__universal_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c469275-a1b0-4d2b-8385-7f1016aa4af8', geneva_conventions_protective_scope__hybrid_proportionality_reading, influences).
narrative_ontology:cs_axiom('8c469275-a1b0-4d2b-8385-7f1016aa4af8', foundational, uniformed_combatant_threshold_necessary).
narrative_ontology:cs_axiom_status(uniformed_combatant_threshold_necessary, holdable).
narrative_ontology:cs_axiom_grounding('8c469275-a1b0-4d2b-8385-7f1016aa4af8', uniformed_combatant_threshold_necessary, conventional).
narrative_ontology:cs_axiom('8c469275-a1b0-4d2b-8385-7f1016aa4af8', foundational, state_military_accountability_via_command).
narrative_ontology:cs_axiom_status(state_military_accountability_via_command, holdable).
narrative_ontology:cs_axiom_grounding('8c469275-a1b0-4d2b-8385-7f1016aa4af8', state_military_accountability_via_command, deontological).
narrative_ontology:cs_reference_frame('8c469275-a1b0-4d2b-8385-7f1016aa4af8', uniformed_state_combatant_protection).
narrative_ontology:cs_drift_state('8c469275-a1b0-4d2b-8385-7f1016aa4af8', contemporary_asymmetric_warfare_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8c469275-a1b0-4d2b-8385-7f1016aa4af8', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, non_state_armed_groups).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, unprivileged_belligerents).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, civilian_populations_near_asymmetric_conflict).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, civilian_populations_near_asymmetric_conflict).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, state_treaty_signatories).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% State armed forces operate under the assumption that Geneva protections apply to uniformed combatants meeting Article 4 criteria. They set the enforcement standard through state practice, military doctrine, and legal interpretation. They benefit from a reading that permits targeting of non-uniformed fighters without obligation to extend combatant immunity. Their exit option is denunciation of the treaty or reinterpretation toward the universal reading, but both carry costs: denunciation invites diplomatic isolation; reinterpretation constrains operational freedom. Their real power lies in maintaining the state-centric interpretation through consistent state practice and military legal doctrine.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries, agenda_setter,
    institutional, generational, arbitrage, global).

% Organizations that do not meet the Article 4 uniformed-combatant criteria fall outside the treaty's protective scope. Members captured or killed are not afforded POW status, medical treatment protections, or trial guarantees. Exit from the constraint would require either abandoning the organization, formally hierarchizing and uniforming it (operationally impossible in many contexts), or accepting that the reading applies and bearing the costs. The identity lock is structural: formalization compromises the insurgent or resistance framing that legitimates the organization within its own political narrative and operational context.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, non_state_armed_groups, payer,
    moderate, biographical, identity_locked, regional).

% Individual fighters without fixed insignia, formal rank, or adequate command accountability are targetable without restriction under this reading. They may be killed in action without the legal protections of combatant status, prosecuted for participation in hostilities (unlawful combatancy) without courts-martial rights, and denied medical protections available to uniformed combatants. Exit is death, capture without protections, formal disengagement, or withdrawal from the conflict. The constraint offers no path to protected status for these individuals under the state-centric reading.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, unprivileged_belligerents, payer,
    powerless, immediate, trapped, local).

% Civilians in regions of asymmetric conflict experience enforcement of the uniformed criterion through increased targeting operations against non-uniformed fighters. The reading permits state forces to treat suspect populations as potential unprivileged belligerents if they offer material support to non-uniformed combatants. Civilians benefit incidentally from state military operations against non-state groups (reduced insurgent control) but bear costs through collateral effects and pressure to distinguish themselves from combatants in environments where distinction is deliberately erased. Exit options are blocked by geography and circumstance.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, civilian_populations_near_asymmetric_conflict, payer,
    powerless, immediate, constrained, local).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__state_centric_reading, civilian_populations_near_asymmetric_conflict, beneficiary).

% Courts interpreting this constraint (International Court of Justice, International Criminal Court, UN commissions of inquiry) evaluate whether fighters met Article 4 criteria and whether their targeting was lawful. They apply the uniformed-combatant criterion to assess whether state conduct violated international humanitarian law. Their interpretation shapes enforcement in practice, but the fundamental legal power to set the standard lies with state treaty parties and state practice rather than with courts.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, international_humanitarian_law_courts, observer,
    institutional, generational, analytical, global).

% Organizations advocating universal human rights protections regardless of combatant status are excluded from the constraint's core standard-setting. They argue the uniformed-combatant criterion creates a loophole permitting targeting of persons who retain fundamental human dignity. They lack institutional power to set the treaty interpretation but generate pressure through advocacy, litigation, and alternative legal frameworks (human rights law, universal jurisdiction). The constraint's enforcement machinery is designed to exclude their voice from the authoritative interpretation.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, human_rights_advocacy_organizations, excluded,
    organized, generational, constrained, global).

% State parties broadly benefit from a legal regime that protects their uniformed combatants while permitting operations against non-state actors outside the protective scope. They can exit by denunciation but rarely do so; they prefer to stay in the treaty while interpreting it narrowly (state-centric reading) to maximize operational flexibility in counterinsurgency and counterterrorism. Exit carries diplomatic and reputational costs; the constrained-mobile exit reflects that most states value treaty membership even while narrowly interpreting protections.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, state_treaty_signatories, beneficiary,
    institutional, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries).
narrative_ontology:fixing_cost_class(geneva_conventions_protective_scope__state_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a bright-line legal criterion (uniformed status, responsible command, fixed insignia, open arms) for combatant status and treaty protections. This solves the coordination problem of distinguishing lawful combatants (who may engage in lawful killing and are entitled to protection when captured) from civilians (who may not engage in killing and are entitled to protection from targeting) and from spies and saboteurs (who may be executed without trial). The criterion creates a clear, verifiable status that does not require subjective judgment about individual intent or context.
% TRANSFER_FUNCTION: Transfers legal protection, status, and due-process rights (POW treatment, prohibition on torture, medical care, trial as combatants rather than criminals) from non-uniformed fighters and unprivileged belligerents to state militaries meeting the Article 4 criteria. Transfers targeting freedom (the right to engage in combat without legal restriction, without liability for lawful killing, without obligation to extend protections) from state forces to non-state actors that do not meet the uniformed standard, permitting their targeting without formal protection or due process.
% ABSENT_VOICES: Non-state armed groups, human rights advocates, and civilians in asymmetric-conflict zones would object if present. They argue the uniformed criterion is operationally inapplicable to insurgent movements, creates a gap that exposes fighters to targeting without protection, and serves primarily to exclude non-state actors from benefits rather than to protect civilians. Non-state actors are structurally excluded from the treaty-interpretation process; the criterion is set by state parties, and non-state actors have no seat at the standard-setting table.
% DISAPPEARANCE_RATIONALE: If the state-centric reading disappeared, state military conduct in asymmetric conflicts would rearrange substantially. Targeting decisions would face different legal constraints (proportionality analysis, universal protections, or conflict-type scaling). Unprivileged belligerents would gain protections or their targeting would require justification beyond mere combatant status. Military legal reviews would shift from 'Does the fighter meet Article 4 criteria?' to 'What protections apply under the alternative reading?' and military rules of engagement would change accordingly. The constraint's enforcement apparatus—military legal reviews, targeting procedures, detention and trial protocols—would reorganize around the new reading.
% FOUNDING_PROBLEM: Early industrial warfare created a need to distinguish uniformed state combatants (who undertake lawful killing on behalf of the state and should retain protections even when captured) from civilian combatants and spies (who may be executed without trial). The Geneva Conventions codified the uniformed combatant as a status that permits people to engage in lawful killing while retaining legal protections—solving the principal-agent problem of making combatancy safe for those who undertake it on behalf of the state.
% FOUNDING_PROBLEM_CORROBORATION: State military establishments and international law scholars writing from a state-sovereignty framing attest the founding problem remains live: asymmetric warfare, insurgency, and terrorism involve fighters who deliberately reject the uniform/command/insignia criteria to blend with civilian populations, creating a coordination problem the state-centric reading solves (a bright-line criterion that does not require subjective judgment). Conversely, human rights organizations, humanitarian law scholars, and non-state armed-group advocates attest the founding problem is solved (asymmetric conflicts exist whether or not uniforms are worn; the criterion does not prevent conflict) and the constraint now primarily serves to extract operational freedom for states. The International Committee of the Red Cross—a neutral outside authority—has published interpretive guidance (ICRC Customary IHL Study, 2005) suggesting the state-centric reading is too narrow and Common Article 3 provides a universal floor for all conflicts, which corroborates the 'solved but persisting' verdict from the humanitarian perspective.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__state_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_protective_scope__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__state_centric_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.68 end-of-interval (2024) because the constraint systematically excludes a large class of fighters from protections, permitting targeting without combatant immunity protections—this extraction is structural and asymmetric: state militaries gain freedom; non-state fighters lose protections. The measurement series shows extraction rising from 0.42 (1949) to 0.68 (2024), tracking the growth of asymmetric warfare and state reliance on the state-centric reading to conduct counterinsurgency and counterterrorism operations without extending Geneva protections to non-uniformed fighters. Suppression is high (0.72) because the constraint's persistence depends on active enforcement—states must maintain rules of engagement that enforce the uniform criterion, courts must deny POW status to non-uniformed detainees, and the treaty interpretation machinery must resist pressure to expand protections. Theater is moderate (0.41, rising from 0.18 in 1949) because much of the enforcement activity now defends the operational exclusion rather than the original coordination function: states conduct extensive legal reviews and public statements defending the uniformed criterion, framing it as a humanitarian measure that protects civilians by creating clear combatant/civilian distinction—but the primary effect is operational freedom. Accessibility collapse is high (0.78): once the uniformed criterion is understood, non-state actors have few alternatives; they can either formalize into uniformed organizations (which often destroys their political legitimacy and operational mobility) or accept targeting without protections. Resistance is moderate (0.62): international humanitarian law scholars, human rights organizations, and some state legal advisors resist the state-centric reading, arguing for Common Article 3 universal floors; but states with military power have sufficient institutional control to maintain the reading despite resistance.
 *
 * PERSPECTIVAL GAP:
 *   The state-military seat and the non-state-armed-group seat compute fundamentally different constraint types from the same structural data. From the state military position: the constraint is rope-like or even mountain-like—a natural legal requirement that distinguishes combatants from civilians, reducing civilian casualties and creating clarity. From the non-state group position: the constraint is snare-like—it is a constructed legal rule that uses the language of protection and clarity to exclude non-state actors from the benefits of the treaty while permitting their targeting. The engine should compute tangled_rope from the structural data (genuine coordination benefit to state militaries + asymmetric extraction from non-state actors + active enforcement requirement). The author believes the structural data should not be tuned to produce this type; the type emerges from the declared beneficiary/victim asymmetry and the enforcement requirement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality diverges sharply between seats. For conventional state militaries (institutional, arbitrage exit), d ≈ 0.1 (strong beneficiary): they set the criterion, interpret it, and benefit from the operational freedom it grants. For non-state armed groups (moderate power, identity-locked exit), d ≈ 0.85 (near-full target): they are structurally excluded from beneficiary status by the criterion itself; their identity as insurgent or resistant organizations prevents formalization that would meet the uniform requirement; they bear the cost of targeting without immunity. For unprivileged belligerents (powerless, trapped exit), d ≈ 1.0 (full target): they have no exit and no negotiating power; the constraint operates on them as pure coercion. For civilian populations (powerless, constrained exit), d ≈ 0.65 (partial target): they experience collateral effects from enforcement and bear pressure to distinguish themselves from combatants, but they also benefit incidentally from state military operations against non-uniformed insurgents. International courts (institutional, analytical exit) sit at d ≈ 0.5 (symmetric): they apply the criterion impartially but lack power to change it. This directionality distribution is the signature of a tangled rope: one party (state militaries) benefits from genuine coordination (the bright-line combatant criterion) while other parties (non-state actors, unprivileged belligerents) bear asymmetric extraction (the loss of protections).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is whether the constraint still solves it. The founding problem was: how do we protect uniformed state combatants who engage in lawful killing while denying protection to spies, saboteurs, and non-accountable fighters? This problem was real in 1949 when professional state armies dominated warfare. By 2024, the problem is contested: asymmetric warfare involves non-state actors who often cannot or will not uniform, but this is not because the Geneva criterion is unclear—it is because non-state actors face different operational and political constraints. States argue the criterion still solves the problem (it distinguishes lawful from unlawful combatants, reducing civilian casualties). Human rights and humanitarian advocates argue the problem is solved by other means (combatants can be distinguished by behavior and context regardless of uniform) and the criterion now primarily serves to exclude actors from protections. The measurement series shows theater_ratio rising from 0.18 to 0.41, indicating that much of the enforcement effort is now theatrical (defending the operational exclusion) rather than functional (ensuring combatant protection). The mandatrophy verdict: this constraint is intermediate between live founding problem and dead founding problem—the founding problem is contested, the constraint persists because states with military power find it operationally useful, and the theater ratio suggests increasing performative maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    uniformed_criterion_necessity,
    'Is the Article 4 uniformed-combatant criterion structurally necessary for humanitarian law to function, or is it a proxy for a deeper distinction (accountability, command responsibility, civilian harm prevention) that could be assessed without the uniform requirement?',
    'Empirical analysis of conflicts with and without formal uniforms: do non-uniformed actors who maintain military discipline and accountability achieve similar protection levels under alternative legal regimes (proportionality analysis, contextual targeting rules)? Do uniformed actors without accountability engage in more civilian harm or no worse than non-uniformed actors under equal legal scrutiny?',
    'If the criterion is necessary (uniforms are the clearest signal of lawful combatancy), the constraint''s core premise is sound and the state-centric reading is justified. If the criterion is a proxy for deeper principles, then alternative readings (universal_rights_reading, hybrid_proportionality_reading) that protect non-uniformed actors meeting other accountability criteria would produce equivalent or better humanitarian outcomes. The classification would shift from tangled_rope toward snare if the criterion is purely extractive cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(uniformed_criterion_necessity, empirical, 'Whether the uniformed-combatant criterion is a necessary humanitarian requirement or a surrogate for deeper principles that could be applied without the uniform gate.').

omega_variable(
    kernel_reading_foreclosure,
    'Do the state-centric reading''s core premises logically foreclose the universal_rights_reading''s core premises within any single coherent legal framework, or do they remain genuinely coexistent positions held by different parties?',
    'Logical analysis of the axioms: the state-centric reading asserts ''uniform status is the threshold for combatant protection'' (categorical); the universal reading asserts ''all persons retain fundamental protections regardless of combatant status'' (categorical). Can both assertions coexist in a single legal system? Yes: a state could grant full POW protections to uniformed combatants AND extend Common Article 3 minimum protections (medical care, no torture, trial rights) to non-uniformed fighters. The readings are distinguishable by their categorical claims about the THRESHOLD, not by their claims about MINIMUM protections. Resolution would require establishing whether the readings are genuinely about different thresholds (coexistent) or whether one reading claims to exhaust the category (foreclosing the other).',
    'If the readings foreclose each other, one of them must be wrong; the constraint''s classification is determined by which reading is adopted. If the readings coexist, then the kernel contest is about which reading is MORE justified, not about which is logically sound. Coexistence supports the cs_structure.reading_relations classification as ''coexists_with'' (author''s current assessment); foreclosure would require reclassification to ''forecloses''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether the state-centric and universal-rights readings are logically incompatible or genuinely coexistent positions.').

omega_variable(
    asymmetric_warfare_causation,
    'Does the state-centric reading''s enforcement cause the growth of asymmetric warfare and non-state armed groups that reject the uniform criterion, or does the growth of asymmetric warfare create pressure for the state-centric reading as a tool to manage non-state actors?',
    'Historical analysis of the causal sequence: did states adopt the state-centric reading first and then non-state actors responded by de-uniforming? Or did non-state actors emerge in asymmetric form first and states subsequently invoked the state-centric reading to deny them protections? Temporal ordering and counterfactual analysis of what would happen if the reading were reversed or neutral.',
    'If the reading causes asymmetric tactics (enforcement incentivizes non-state actors to stay non-uniform to evade the rules), then the constraint is endogenous to the asymmetry it claims to manage—extraction becomes a mechanism that perpetuates the problem it claims to solve, supporting snare classification. If asymmetric warfare causes the state-centric reading (states adopt it as a rational response to pre-existing asymmetry), the constraint is an effect of the asymmetry, not a cause, and the classification holds as tangled_rope (coordination + asymmetric extraction driven by structural conditions, not by the constraint''s incentive structure).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(asymmetric_warfare_causation, empirical, 'Whether the state-centric reading''s enforcement structure caused or responded to the growth of asymmetric warfare.').

omega_variable(
    common_article_3_applicability,
    'Does Common Article 3 (which applies to non-international armed conflicts and establishes minimum protections for all persons regardless of combatant status) create a floor that undermines the state-centric reading''s exclusion of non-uniformed fighters, or is Common Article 3 compatible with the state-centric reading when applied at different conflict-classification levels?',
    'Jurisprudential analysis: courts (ICC, ICJ, national tribunals) have interpreted Common Article 3 as applying universally to all armed conflicts, creating a minimum floor of protections. The state-centric reading is compatible with Common Article 3 if the state-centric reading applies only to international armed conflicts (where full Geneva protections apply to uniformed combatants) while Common Article 3 applies to non-international armed conflicts and asymmetric warfare (where minimum protections apply to all). The resolution depends on whether courts accept this hierarchical interpretation or whether they read Common Article 3 as creating a universal floor that applies regardless of conflict classification.',
    'If Common Article 3 creates a universal floor, the state-centric reading''s exclusive focus on uniformed combatants is qualified by Article 3 minimums, and the constraint''s extractiveness from unprivileged belligerents is reduced (they retain minimum medical care, trial rights, no torture even without combatant status). This would lower measured extractiveness from 0.68 toward 0.45–0.55, shifting classification toward rope or balanced tangled_rope. If the state-centric reading and Common Article 3 are compatible at different conflict levels, the constraint''s extractiveness holds at 0.68 for asymmetric conflicts where the state-centric reading dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(common_article_3_applicability, conceptual, 'Whether Common Article 3 creates a universal floor that qualifies the state-centric reading''s exclusions or whether the readings are hierarchical and non-overlapping.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__state_centric_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 1949, 0.18).
narrative_ontology:measurement(gene_tr_t1977, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 1977, 0.24).
narrative_ontology:measurement(gene_tr_t1991, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 1991, 0.31).
narrative_ontology:measurement(gene_tr_t2001, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 2001, 0.37).
narrative_ontology:measurement(gene_tr_t2011, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 2011, 0.39).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 2024, 0.41).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 1949, 0.42).
narrative_ontology:measurement(gene_be_t1977, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 1977, 0.52).
narrative_ontology:measurement(gene_be_t1991, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 1991, 0.58).
narrative_ontology:measurement(gene_be_t2001, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 2001, 0.64).
narrative_ontology:measurement(gene_be_t2011, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 2011, 0.66).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 1949, 0.58).
narrative_ontology:measurement(gene_su_t1977, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 1977, 0.64).
narrative_ontology:measurement(gene_su_t1991, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 1991, 0.68).
narrative_ontology:measurement(gene_su_t2001, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 2001, 0.7).
narrative_ontology:measurement(gene_su_t2011, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 2011, 0.71).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__state_centric_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_protective_scope__state_centric_reading, 0.12).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope__universal_rights_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope__hybrid_proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'geneva_conventions_protective_scope.' The kernel is the Geneva Conventions' protective scope, which all parties dispute over but treat as authoritative. The state-centric reading narrows the victim set (excludes non-state actors from POW protections) and lowers ε on state military operations (permits targeting of unprivileged belligerents without combatant immunity). Sibling readings (universal_rights_reading, hybrid_proportionality_reading) make different structural claims about the same kernel and should be authored as separate constraint stories with their own ε values, beneficiary/victim structures, and omegas documenting where the readings diverge. The network linkage indicates these stories are part of the same constraint family—the kernel is contested, each reading instantiates a distinct constraint, and understanding the kernel contest requires reading all three stories together. Do not merge the readings into one story; each reading is a clean, ε-invariant constraint instantiated separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
