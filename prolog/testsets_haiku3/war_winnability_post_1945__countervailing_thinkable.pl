% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__countervailing_thinkable
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__countervailing_thinkable, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: war_winnability_post_1945__countervailing_thinkable
 *   human_readable: Nuclear Weapons Enable Limited War Winnability via Counterforce Targeting
 *   domain: strategic_studies/nuclear_deterrence/international_relations
 *
 * SUMMARY:
 *   This constraint instantiates the countervailing_thinkable reading of the
 *   contested kernel war_winnability_post_1945. The reading asserts that
 *   nuclear weapons have constrained but not eliminated the possibility of
 *   winning limited wars through selective counterforce targeting and
 *   superior technical execution. The military-strategic establishment,
 *   weapons laboratories, and political leadership with nuclear authority
 *   operationalize this reading in doctrines (counterforce strategy, flexible
 *   response, escalation control) and force structure decisions (continuous
 *   modernization of weapons systems, command-and-control infrastructure,
 *   targeting capabilities). The constraint remains live and operative: major
 *   nuclear powers continue planning for nuclear war-winning scenarios.
 *   Alternative readings (deterrence_unthinkable, which treats all nuclear
 *   conflict as categorically unwinnable; rhetorical_contraction, which bans
 *   public discussion of winnability while maintaining operational planning)
 *   exist as competing framings held by different communities and political
 *   constituencies. The claim/metric gap is intentional: the constraint is
 *   CLAIMED as tangled_rope (provides genuine coordination function for
 *   deterrence while extracting benefits through military-industrial
 *   complexity and doctrine maintenance) while metrics show substantial
 *   extraction, high suppression of alternative voices, and rising theater
 *   ratio (performative cost-benefit analysis accompanying the constraint's
 *   operation). The measurement series tracks 81 years of the constraint's
 *   operative life, showing initial accumulation of extractiveness and
 *   suppression as the constraint's institutional embedding deepened, then
 *   plateauing as the constraint reached mature institutional saturation.
 *
 * KEY AGENTS:
 *   - Military-strategic establishment: authors doctrine, maintains planning, justifies arsenals under winnability assumptions
 *   - Weapons laboratories and defense contractors: develop technologies to support counterforce capability and operational persistence under constraint
 *   - Political leadership with nuclear authority: must decide whether the constraint's assumptions remain coherent and whether to sustain or revise doctrine
 *   - Arms control regimes: pay the cost of delegitimization and non-compliance as winnability doctrine undermines their legitimacy
 *   - Civilian populations in target zones: bear the risk of living under counterforce targeting assumptions
 *   - Analytical and dissident voices: excluded from doctrine-setting authority but providing persistent challenge to the constraint's empirical and logical coherence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__countervailing_thinkable, 0.68).
domain_priors:suppression_score(war_winnability_post_1945__countervailing_thinkable, 0.71).
domain_priors:theater_ratio(war_winnability_post_1945__countervailing_thinkable, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, extractiveness, 0.68).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__countervailing_thinkable, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__countervailing_thinkable, "Nuclear Weapons Enable Limited War Winnability via Counterforce Targeting").
narrative_ontology:topic_domain(war_winnability_post_1945__countervailing_thinkable, "strategic_studies/nuclear_deterrence/international_relations").

domain_priors:requires_active_enforcement(war_winnability_post_1945__countervailing_thinkable).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__countervailing_thinkable, 'e1e2bbd9-d572-48bb-a257-0ca4d7148de9').
narrative_ontology:cs_kernel_codification('e1e2bbd9-d572-48bb-a257-0ca4d7148de9', fixed_text).
narrative_ontology:cs_authority_grounding('e1e2bbd9-d572-48bb-a257-0ca4d7148de9', extraction).
narrative_ontology:cs_interpretation_layer_present('e1e2bbd9-d572-48bb-a257-0ca4d7148de9').
narrative_ontology:cs_reading_relation('e1e2bbd9-d572-48bb-a257-0ca4d7148de9', war_winnability_post_1945__deterrence_unthinkable, coexists_with).
narrative_ontology:cs_reading_relation('e1e2bbd9-d572-48bb-a257-0ca4d7148de9', war_winnability_post_1945__rhetorical_contraction, influences).
narrative_ontology:cs_axiom('e1e2bbd9-d572-48bb-a257-0ca4d7148de9', foundational, limited_nuclear_victory_achievable).
narrative_ontology:cs_axiom_status(limited_nuclear_victory_achievable, holdable).
narrative_ontology:cs_axiom_grounding('e1e2bbd9-d572-48bb-a257-0ca4d7148de9', limited_nuclear_victory_achievable, empirically_contingent).
narrative_ontology:cs_axiom('e1e2bbd9-d572-48bb-a257-0ca4d7148de9', secondary, counterforce_targeting_enables_war_termination).
narrative_ontology:cs_axiom_status(counterforce_targeting_enables_war_termination, holdable).
narrative_ontology:cs_axiom_grounding('e1e2bbd9-d572-48bb-a257-0ca4d7148de9', counterforce_targeting_enables_war_termination, instrumental).
narrative_ontology:cs_reference_frame('e1e2bbd9-d572-48bb-a257-0ca4d7148de9', nuclear_weapons_as_instruments_of_statecraft).
narrative_ontology:cs_drift_state('e1e2bbd9-d572-48bb-a257-0ca4d7148de9', contemporary_command_reliability_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e1e2bbd9-d572-48bb-a257-0ca4d7148de9', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__countervailing_thinkable, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, strategic_doctrine_establishment).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, arms_control_regimes).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, civilian_populations_in_target_zones).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, rival_nuclear_powers).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, rival_nuclear_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains continuous funding, mission legitimacy, and strategic relevance by planning for nuclear war scenarios in which victory remains achievable through superior force application. The counterforce doctrine justifies continued weapons development, modernization programs, and strategic platform procurement. Benefits from the constraint's operative assumption that nuclear conflict can be won through selective targeting, which keeps operational planning and weapons systems development flowing.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex, agenda_setter).

% Develops and transmits war-fighting doctrines that assume nuclear exchange remains within the space of political struggle rather than existential boundary. Authors strategic concepts (counterforce, war termination, escalation control) that treat limited nuclear victory as a reachable goal. The doctrine provides both legitimacy for military strategy and intellectual scaffolding for deterrence arguments that remain credible only if some scenarios end in achievement of objectives short of mutual annihilation.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, strategic_doctrine_establishment, beneficiary,
    institutional, generational, mobile, global).

% Inherits and sustains nuclear weapons arsenals and the strategic plans that accompany them. Must decide whether to treat the arsenals as instruments of statecraft (which requires winnability assumptions to make deterrence threats credible) or as pure doomsday devices (which requires no plan and no expectation of advantage). The counterforce constraint holds this tension in the middle: weapons remain in operational status, planning continues, but victory is framed as limited and achievable through technical competence, not as total domination or capitulation.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, political_leadership_with_nuclear_authority, agenda_setter,
    institutional, biographical, trapped, global).

% Attempt to establish limits on nuclear weapons development, testing, and deployment. The constraint that winnability remains achievable directly undermines arms control arguments: if nuclear war is unwinnable, the case for abolition strengthens; if winnability persists, the case for accumulation and refinement persists instead. Arms control regimes pay the cost of delegitimization and non-compliance as military planners continue developing counterforce capabilities to maintain war-fighting optionality.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, arms_control_regimes, payer,
    organized, generational, constrained, global).

% Situated within or adjacent to military installations, command centers, weapons caches, and strategic infrastructure that are the object of counterforce targeting. The constraint that limited nuclear victory is achievable through selective strikes creates a category of acceptable collateral damage and residual radiation exposure. Their exit option is evacuation, which is expensive, disruptive, and incomplete. They bear the cost of living under the operational assumption that their location is a target in scenarios planners treat as winnable.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, civilian_populations_in_target_zones, payer,
    powerless, immediate, trapped, global).

% Argue that the winnability assumption is incoherent, that nuclear exchange cannot be limited or controlled, and that planning for victory amounts to planning for human extinction under optimistic assumptions. They are excluded from the constraint's operative framework: their objections are noted in policy discourse but are not permitted to override the doctrine's structural assumptions. Their exclusion is enforced by the authority structures (military, strategic establishment, political leadership) that have sunk legitimacy into the constraint itself.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, deterrence_sceptics_and_abolitionist_movements, excluded,
    moderate, biographical, constrained, global).

% Also rely on the constraint that winnability remains in reachable space: their own deterrence postures depend on the assumption that an adversary might attempt limited nuclear war, which justifies their counterforce arsenals and war plans. Each major nuclear power benefits from the shared assumption that victory is possible, because that assumption is what makes their own deterrent credible. But they are also payers: the arms competition to maintain technological superiority in counterforce capability is expensive and never concludes; the operational planning creates hair-trigger risks; the mutual assumption that someone might try to win creates persistent instability.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, rival_nuclear_powers, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__countervailing_thinkable, rival_nuclear_powers, payer).

% Examines the logical structure of the constraint and its policy implications from multiple disciplinary perspectives. Includes game theorists, historians, physicists, economists, and philosophers. Takes evidence from war planning documents, technical analyses of weapons effects, and historical accounts of nuclear command authority. Can assess the constraint's empirical premises and logical coherence.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, analytical_community_in_security_studies, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex).
narrative_ontology:fixing_cost_class(war_winnability_post_1945__countervailing_thinkable, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the strategic assumptions of major nuclear powers around a shared framework in which nuclear conflict remains winnable through superior force application and selective targeting. Creates a common language for deterrence arguments (credible threat requires winnability assumption), arms competition justification (we must maintain superiority), and military-strategic planning (victory is a reachable operational goal). This coordination allows militaries to speak to each other in the same framework without collapse into either capitulation rhetoric or mutual-annihilation rhetoric.
% TRANSFER_FUNCTION: Moves legitimacy, funding, and strategic relevance from arms control regimes and non-proliferation frameworks toward military-industrial capacity building and doctrine development. Transfers operational planning authority from civilian constraint to military technical judgment. Moves political risk (the existential danger of uncontrolled escalation) from explicitly-framed doom to implicitly-managed technical problem.
% ABSENT_VOICES: Physicists who study thermal and radiation effects; survivors of Hiroshima and Nagasaki; dissident military officers who have raised command-control failures; historians of accident and miscalculation; economists of opportunity cost; civilians in potential target zones. These voices would testify that winnability is incoherent under the physical realities of nuclear effect, that command systems have failed in peacetime and will fail under stress, that the cost-benefit ratio of military superiority in weapons systems is negative when the weapons cannot be used without mutual destruction. They are structurally excluded from the doctrine-setting process.
% DISAPPEARANCE_RATIONALE: If the operational constraint that winnability remains achievable through counterforce targeting were to vanish (replaced by the reading that nuclear war is categorically unwinnable, or by rhetorical contraction that bans planning), the entire architecture of nuclear deterrence, force structure justification, weapons-development roadmaps, and strategic doctrine would require reorganization. Arms control regimes would strengthen. Military budgets would face pressure. The strategic establishment would be forced to articulate a different rationale for nuclear retention (pure deterrent, not war-fighting tool). The weapons themselves would persist, but the legitimating narrative for continuous modernization would collapse.
% FOUNDING_PROBLEM: After 1945, major powers faced the problem of how to retain the ability to win wars under conditions of mutual nuclear-weapons capability. The founding problem is not whether to have nuclear weapons but whether to treat them as pure terror devices or as instruments that, with sufficient technical competence and operational control, could achieve political objectives in limited conflicts. The constraint emerged as a solution: nuclear weapons are powerful but not infinite; wars can be limited; victory through superior force application in counterforce exchange remains theoretically achievable.
% FOUNDING_PROBLEM_CORROBORATION: The military-strategic establishment and weapons laboratories attest the problem remains live: adversaries continue developing counterforce capabilities, command-control systems must assume some scenarios might escalate to limited nuclear exchange, and operational planning must account for that possibility. Arms control advocates and nuclear security physicists attest the founding problem has been solved in the negative: nuclear war cannot be limited or won; the illusion of winnability is the problem, not the solution. Declassified strategy documents from the 1950s–1980s (SIOP reviews, National Security Directives, Air Force doctrine) support the establishment's self-attestation; historical analysis of accident-and-near-miss incidents (Cuban Missile Crisis, false alarms, command failures) supports the sceptics' reading.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__countervailing_thinkable, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__countervailing_thinkable, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__countervailing_thinkable, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(war_winnability_post_1945__countervailing_thinkable, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__countervailing_thinkable, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__countervailing_thinkable_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__countervailing_thinkable, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_winnability_post_1945__countervailing_thinkable_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness begins low (0.38 in 1945) because the constraint is a nascent institutional fiction: few resources are committed, planning is primitive, and the winnability assumption is lightly held. It rises steeply through the 1950s–1970s (Cuban Missile Crisis, SIOP refinement, weapons modernization cycles) as military-strategic and political institutions invest in the constraint's legitimacy and operationalization. It plateaus around 0.65–0.68 from 1980 onward as the constraint reaches mature institutional embedding: the military establishment has adapted all its planning to assume winnability; weapons development roadmaps are locked in; the doctrine is self-reproducing. Suppression rises in parallel (from 0.44 to 0.71) because maintaining the constraint requires active exclusion of sceptics, suppression of accident reports and near-miss narratives, and institutional resistance to arms control proposals that would require abandoning the winnability assumption. Theater ratio rises more slowly (from 0.15 to 0.42) and plateaus earlier, indicating that by 2000, a substantial portion of the constraint's operative activity (strategic briefings, doctrine publications, force structure justifications) is performative—maintaining the intellectual coherence of the winnability assumption for audiences who are increasingly skeptical of its empirical foundation. The one shared time grid ensures every metric is authored at every examined point; cyclical patterns (Cuban Missile Crisis peak, détente relaxation, Cold War acceleration, post-Cold War plateau) are captured within the trajectory.
 *
 * PERSPECTIVAL GAP:
 *   The military-strategic establishment and agenda-setters compute the constraint as tangled_rope from their seat: there is genuine coordination (all major powers share the framework, allowing deterrence to function), and there is extraction (mission continuity, funding, authority to plan for war). From the arms control regimes' seat, the constraint computes as snare: there is no coordination benefit to them, only the cost of delegitimization and non-compliance as military planners ignore their proposals. From the analytical observer seat, the constraint computes as piton or degraded snare: the functional justification (coordinating deterrence) has atrophied as nuclear command-and-control systems have become more reliable, not less; what remains is performance of the winnability assumption to maintain institutional authority, not actual contingency planning. From the civilian populations' seat in target zones, the constraint computes as snare with identity-lock components: they are trapped by geography and identity (where they live is where military targets are located); suppression is both structural (they cannot easily move) and internalized (they have rationalized the risk as normal or inevitable).
 *
 * DIRECTIONALITY LOGIC:
 *   The military-industrial complex and strategic doctrine establishment are the structural beneficiaries: they collect mission continuity, funding, and institutional authority. Their directionality (d) is near the beneficiary end (0.1–0.25). Political leadership with nuclear authority is near the symmetric midpoint (d ≈ 0.5): they benefit from the doctrinal simplification and the deterrence framework, but they also bear the operational risk of command failure, accident, or escalation spiral. Arms control regimes have high d (0.75–0.9) because they bear the extraction cost (delegitimization, non-compliance) without collecting benefits. Civilian populations have the highest d (0.85–1.0): trapped by geography, facing existential risk from counterforce targeting, unable to exit. Rival nuclear powers have mixed directionality: they benefit from the shared assumption that winnability is possible (it makes their own deterrents credible), but they also pay the cost of continuous arms competition and hair-trigger risk (d ≈ 0.5–0.65). The analytical community is at d = 0.5 (observer): neither collecting benefits nor bearing costs, but providing the epistemic infrastructure that could destabilize the constraint if its findings contradicted the winnability assumption strongly enough.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to win wars under mutual nuclear capability) has arguably shifted from live to dead or contested by 2015–2026. The military establishment continues to assert the problem is live, but the empirical basis has eroded: command-and-control reliability has improved (reducing accident risk but also reducing the need for operational planning to account for chaos); weapons have proliferated beyond major powers (raising the cost of any limited exchange and making the winnability assumption less coherent); and climate and economic constraints have made extended nuclear conflict less survivable. The constraint persists despite mandatrophy because the military-industrial complex has sunk institutional identity into it: abandoning the winnability assumption would require reorganizing budgets, doctrines, and career paths. The constraint is now held by inertia, sunk cost, and institutional interest rather than by the coherence of its founding problem. The high theater_ratio (0.42) supports this assessment: a substantial portion of the constraint's activity is performative maintenance rather than genuine strategic planning.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    winnability_empirical_coherence,
    'Is the winnability assumption empirically coherent under actual conditions of nuclear-weapons effects, command-and-control reliability, and escalation dynamics?',
    'Physics-based modeling of thermal, radiation, and environmental effects from the NUKEMAP/Missilemap class; historical analysis of command-control failures under stress; game-theoretic analysis of escalation dynamics with imperfect information and fog of war.',
    'If winnability is incoherent under actual physics and command conditions, the constraint is a false-summit mountain held by institutional inertia — it should reclassify as piton or degraded snare. If winnability is coherent under narrow technical conditions (e.g., first-strike counterforce with perfect command), the constraint remains tangled_rope but with high opacity and high suppression of alternative premises.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(winnability_empirical_coherence, empirical, 'Whether limited nuclear victory is empirically achievable under actual weapons effects and command-control conditions.').

omega_variable(
    institutional_inertia_vs_live_function,
    'Does the constraint persist because it solves a genuine coordination problem for nuclear powers, or because military institutions have sunk identity and budgets into the winnability assumption and cannot coherently revise it?',
    'Comparative institutional analysis: examine why some nuclear powers (e.g., UK, France) maintain smaller deterrent forces without detailed war-fighting doctrine, while others (US, Russia) maintain massive counterforce arsenals and detailed escalation plans. If the coordination function is genuine, all nuclear powers should converge on similar doctrines and force structures; if institutional inertia is primary, variation should track historical path-dependency and bureaucratic incentive structure.',
    'If live function dominates, the constraint is tangled_rope as classified. If institutional inertia dominates, the constraint should reclassify as piton (performative maintenance of atrophied function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_inertia_vs_live_function, empirical, 'Whether the constraint solves a genuine coordination problem or persists by institutional inertia.').

omega_variable(
    kernel_reading_committer_contestation,
    'Which reading of the kernel war_winnability_post_1945 represents the actual operative constraint: countervailing_thinkable (winnability is achievable), deterrence_unthinkable (winnability is incoherent), or rhetorical_contraction (winnability is operationally planned but publicly unsayable)?',
    'Examine three types of evidence in parallel: (1) doctrine publications and strategic guidance (which reading do they enact?); (2) force structure and weapons development programs (which reading justifies the observed arsenal?); (3) public rhetoric and political discourse (which reading is permissible to state?). The operative constraint is the one that best predicts actual military behavior and budget allocation.',
    'If deterrence_unthinkable is operative, weapons are retained as pure terror and winnability planning is window-dressing; the constraint should reclassify and the measurement series should show rapid decline in theater_ratio post-Cold War. If rhetorical_contraction is operative, winnability is operationally real but publicly suppressed; the constraint continues but with high suppression and theater_ratio values. If countervailing_thinkable is operative, winnability is openly planned and doctrinally endorsed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_contestation, conceptual, 'Which reading of war_winnability_post_1945 is the actual operative constraint.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression of arms control voices and accident-acknowledgment structural (external barriers, institutional exclusion, classification systems) or internalized (strategic planners have internalized the winnability assumption as inevitable and unquestionable)?',
    'Exit-trajectory analysis: if military institutional insiders who leave their posts begin openly questioning the winnability assumption (whistleblower trajectory), suppression is structural. If they continue defending it even after institutional departure (identity-fusion trajectory), suppression is partly internalized.',
    'If structural, remedies would focus on institutional reform and transparency. If internalized, the suppression travels with individual identity; even institutional change would not dissolve it, and the constraint would persist through cultural transmission rather than active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression of winnability-sceptic voices is structural or internalized in military institutional identity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__countervailing_thinkable, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1945, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1945, 0.15).
narrative_ontology:measurement_basis(war__tr_t1945, observed).
narrative_ontology:measurement(war__tr_t1962, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1962, 0.28).
narrative_ontology:measurement_basis(war__tr_t1962, observed).
narrative_ontology:measurement(war__tr_t1980, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1980, 0.38).
narrative_ontology:measurement_basis(war__tr_t1980, observed).
narrative_ontology:measurement(war__tr_t2000, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 2000, 0.41).
narrative_ontology:measurement_basis(war__tr_t2000, observed).
narrative_ontology:measurement(war__tr_t2015, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 2015, 0.42).
narrative_ontology:measurement_basis(war__tr_t2015, observed).
narrative_ontology:measurement(war__tr_t2026, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 2026, 0.42).
narrative_ontology:measurement_basis(war__tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(war__be_t1945, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1945, 0.38).
narrative_ontology:measurement_basis(war__be_t1945, observed).
narrative_ontology:measurement(war__be_t1962, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1962, 0.52).
narrative_ontology:measurement_basis(war__be_t1962, observed).
narrative_ontology:measurement(war__be_t1980, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement_basis(war__be_t1980, observed).
narrative_ontology:measurement(war__be_t2000, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 2000, 0.66).
narrative_ontology:measurement_basis(war__be_t2000, observed).
narrative_ontology:measurement(war__be_t2015, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 2015, 0.68).
narrative_ontology:measurement_basis(war__be_t2015, observed).
narrative_ontology:measurement(war__be_t2026, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(war__be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1945, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1945, 0.44).
narrative_ontology:measurement_basis(war__su_t1945, observed).
narrative_ontology:measurement(war__su_t1962, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1962, 0.58).
narrative_ontology:measurement_basis(war__su_t1962, observed).
narrative_ontology:measurement(war__su_t1980, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1980, 0.68).
narrative_ontology:measurement_basis(war__su_t1980, observed).
narrative_ontology:measurement(war__su_t2000, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement_basis(war__su_t2000, observed).
narrative_ontology:measurement(war__su_t2015, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 2015, 0.71).
narrative_ontology:measurement_basis(war__su_t2015, observed).
narrative_ontology:measurement(war__su_t2026, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 2026, 0.71).
narrative_ontology:measurement_basis(war__su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__countervailing_thinkable, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_winnability_post_1945__countervailing_thinkable, 0.12).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, nuclear_command_and_control_reliability).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, arms_control_regime_legitimacy).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, escalation_control_doctrine).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, deterrence_stability_assumption).

% DUAL FORMULATION NOTE:
% This constraint is one reading (countervailing_thinkable) in a kernel family of three related constraints on war winnability post-1945. All three readings decompose the single ambiguous kernel into structurally distinct constraints with different ε values, beneficiary/victim structures, and operative assumptions. Sister reading constraints: (1) deterrence_unthinkable — nuclear weapons made great-power total war categorically unwinnable; (2) rhetorical_contraction — winnability became unsayable while remaining operationally planned. The three readings coexist across different institutional and political communities; they are not alternative descriptions of the same constraint, but rather three different constraints with different empirical referents and different beneficiary structures. Linked through network.affects_constraints to enable contention analysis and reading-choice identification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_winnability_post_1945__countervailing_thinkable, organized, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
