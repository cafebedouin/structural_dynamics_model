% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__dropping_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: total_war_reachability_boundary__dropping_reading
 *   human_readable: Deterrence Coordination Equilibrium Under Total War Reachability
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint instantiates the DROPPING READING of the contested kernel
 *   'total_war_reachability_boundary': total war dropped in probability but
 *   remains reachable; deterrence is a tangled_rope (coordination equilibrium
 *   with defection risk), not a mountain. The reading asserts that nuclear
 *   deterrence operates as a coordination game—rational powers choose not to
 *   escalate—rather than as a natural law or physical impossibility. Total
 *   war probability declined sharply after the Cold War ended, but
 *   reachability was never eliminated: the capacity and willingness to wage
 *   total war remain at hand, held in equilibrium by mutual vulnerability and
 *   credible second-strike forces. This reading differs fundamentally from
 *   the CONTRACTION READING (which holds that winnable total war left the
 *   feasible set entirely, making reachability a false appearance) and the
 *   CONTINGENT_REACHABILITY READING (which holds that reachability is purely
 *   technology-dependent and could reverse). The DROPPING READING locates the
 *   constraint in strategic choice, not technology or physical law.
 *
 * KEY AGENTS:
 *   - Nuclear weapon states: institutional agenda-setters maintaining deterrence credibility through force posture and doctrine
 *   - Non-nuclear allied states: powerful beneficiaries sheltering under extended deterrence umbrellas
 *   - Global civilian populations: powerless victims bearing existential risk and suppressed disarmament alternatives
 *   - Non-aligned states: constrained payers excluded from deterrence benefits but subject to its risks
 *   - Strategic doctrine intellectuals: organized agenda-setters designing deterrence theory and legitimacy
 *   - Disarmament advocates: excluded voices suppressed by institutional marginalization
 *   - Analytical observer: witnesses the constraint structure without participating in coordination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__dropping_reading, 0.68).
domain_priors:suppression_score(total_war_reachability_boundary__dropping_reading, 0.72).
domain_priors:theater_ratio(total_war_reachability_boundary__dropping_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__dropping_reading, tangled_rope).
narrative_ontology:human_readable(total_war_reachability_boundary__dropping_reading, "Deterrence Coordination Equilibrium Under Total War Reachability").
narrative_ontology:topic_domain(total_war_reachability_boundary__dropping_reading, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_reachability_boundary__dropping_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__dropping_reading, '591144a5-fdfd-4847-9d59-b38f3c014756').
narrative_ontology:cs_kernel_codification('591144a5-fdfd-4847-9d59-b38f3c014756', distributed).
narrative_ontology:cs_authority_grounding('591144a5-fdfd-4847-9d59-b38f3c014756', extraction).
narrative_ontology:cs_interpretation_layer_present('591144a5-fdfd-4847-9d59-b38f3c014756').
narrative_ontology:cs_reading_relation('591144a5-fdfd-4847-9d59-b38f3c014756', total_war_reachability_boundary__contraction_reading, influences).
narrative_ontology:cs_reading_relation('591144a5-fdfd-4847-9d59-b38f3c014756', total_war_reachability_boundary__contingent_reachability_reading, coexists_with).
narrative_ontology:cs_axiom('591144a5-fdfd-4847-9d59-b38f3c014756', foundational, deterrence_as_rational_coordination).
narrative_ontology:cs_axiom_status(deterrence_as_rational_coordination, holdable).
narrative_ontology:cs_axiom_grounding('591144a5-fdfd-4847-9d59-b38f3c014756', deterrence_as_rational_coordination, instrumental).
narrative_ontology:cs_axiom('591144a5-fdfd-4847-9d59-b38f3c014756', foundational, total_war_reachability_persistent).
narrative_ontology:cs_axiom_status(total_war_reachability_persistent, holdable).
narrative_ontology:cs_axiom_grounding('591144a5-fdfd-4847-9d59-b38f3c014756', total_war_reachability_persistent, empirically_contingent).
narrative_ontology:cs_reference_frame('591144a5-fdfd-4847-9d59-b38f3c014756', mutual_vulnerability_equilibrium).
narrative_ontology:cs_drift_state('591144a5-fdfd-4847-9d59-b38f3c014756', post_cold_war_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('591144a5-fdfd-4847-9d59-b38f3c014756', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, non_nuclear_allied_states).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, global_civilian_populations).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, non_aligned_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, strategic_doctrine_intellectuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain nuclear arsenals and credible second-strike postures. Set deterrence rules through declaratory policy, capability demonstrations, and threat signaling. Their security depends on other nuclear powers believing their willingness to escalate, which requires keeping total war reachable as a credible threat. Enforce the constraint through weapons maintenance, strategic doctrine refinement, periodic nuclear signaling (tests, exercises, doctrine updates), and through international nonproliferation regimes. Benefit from deterrence coordination because it prevents conventional wars from escalating into nuclear exchanges that would destroy them. Face trapped exit because abandoning nuclear forces would create security vulnerability and loss of strategic influence.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, nuclear_weapon_states, agenda_setter,
    institutional, civilizational, trapped, global).

% Shelter under extended nuclear umbrellas provided by allied nuclear powers. Gain security assurances and strategic autonomy in their regions because nuclear deterrent holds escalation threats at bay. Their exit from the alliance is constrained by regional security threats, economic interdependence, and the credibility costs of leaving a nuclear alliance. They benefit from deterrence without bearing direct costs of maintaining nuclear forces. Remain exposed to nuclear escalation risk if deterrence fails or if drawn into conflicts where their nuclear ally faces nuclear threats.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, non_nuclear_allied_states, beneficiary,
    powerful, generational, constrained, global).

% Bear the existential risk that deterrence fails and total war becomes actual. Their exit options are structurally zero: they cannot withdraw from the global deterrence game, cannot refuse to inhabit territories that are nuclear targets, cannot decline to accept the constraint. They pay through continuous anxiety, emergency preparedness infrastructure (fallout shelters, civil defense), economic resources diverted to military spending, and the ultimate cost—annihilation—if deterrence breaks down. The constraint extracts from them through suppressed alternatives (no legitimate institutional pathway to disarm) and structural vulnerability (they are counted as acceptable losses in deterrence calculations).
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, global_civilian_populations, payer,
    powerless, biographical, trapped, global).

% Remain outside nuclear alliance structures but remain subject to deterrence logic and its risks. Cannot access security benefits of nuclear protection but absorb risks of escalation spirals between aligned powers. Some face direct nuclear threats from aligned powers or rival nuclear states. Their attempts to build autonomous deterrents are actively suppressed through nonproliferation regimes and security guarantees enforced by the nuclear powers. They pay through vulnerability and through the suppression of their own deterrence options.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, non_aligned_states, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__dropping_reading, non_aligned_states, excluded).

% Design and legitimize deterrence doctrine. Their theoretical frameworks establish what counts as 'credible threat,' what levels of retaliation are rational, and how total war remains reachable despite declining probability. They participate in war games, policy advisory roles, academic discourse, and government strategy reviews. They benefit from deterrence system's continuation because it funds their research, maintains their institutional authority, and keeps them as recognized experts in matters of state security. They can exit by leaving the field or shifting research focus but do so at career and institutional cost. They actively enforce the constraint through doctrine refinement and through framing disarmament as destabilizing.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, strategic_doctrine_intellectuals, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__dropping_reading, strategic_doctrine_intellectuals, beneficiary).

% Argue that total war should not remain reachable, that the constraint should be dissolved through comprehensive nuclear disarmament, arms reduction treaties, and structural changes to international security. They are structurally excluded from the deterrence coordination game: their advocacy is treated as naive, destabilizing, or threatening by the agenda-setters and beneficiaries. They bear suppression through institutional marginalization (excluded from security policy circles), dismissal of their arguments as impractical, and active framing of disarmament advocacy as increasing nuclear risks. Their alternative framing (disarmament is stabilizing) is suppressed rather than contested on evidence.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, disarmament_advocates, excluded,
    moderate, generational, constrained, global).

% Analyzes the constraint from outside the coordination game. Examines whether deterrence truly solves the problem of preventing escalation or merely postpones it while increasing fragility. Assesses whether dropping probability without eliminating reachability creates stable equilibrium or unstable standoff. Witnesses whether the constraint's enforcement (weapons maintenance, doctrine signaling) remains genuinely functional or is increasingly theatrical. Does not participate in enforcing deterrence rules but reflects on their structural implications and sustainability.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_reachability_boundary__dropping_reading, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(total_war_reachability_boundary__dropping_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates nuclear weapon states' behavior to prevent escalation of major-power conflicts into nuclear exchanges. Establishes a shared understanding that first use of nuclear weapons invites unacceptable retaliation, creating mutual interest in keeping total war reachable as threat but improbable as action. Solves the problem of asymmetric escalation dominance: before nuclear weapons, conventional superiority could lead to total victory; with nuclear weapons, total war becomes mutually destructive, so both sides have shared interest in preventing escalation spirals.
% TRANSFER_FUNCTION: Moves security assurance from nuclear weapon states to their non-nuclear allies. Moves existential risk from nuclear powers to global civilian populations, non-aligned states, and future generations. Non-nuclear allies receive strategic protection and autonomy; they pay through indirect exposure to escalation. Nuclear powers receive deterrence credibility (others believe they will retaliate); they pay through the cost of maintaining credible forces and through the ever-present risk of accident or miscalculation. Civilians and non-aligned states are not consulted but are placed into the game as potential casualties whose vulnerability makes the deterrent credible.
% ABSENT_VOICES: Disarmament advocates, future generations whose existence depends on deterrence not failing, populations in regions targeted by nuclear scenarios, non-state actors with no nuclear capacity, and developing nations pursuing their own security interests are structurally excluded from the deterrence coordination game. They would argue that the arrangement treats total war as a permanent feature of international life rather than as a contingency to be eliminated. Their absence from the table is enforced by the doctrine itself: admitting them would introduce voices skeptical of deterrence and would require justifying why civilian vulnerability must be maintained for the sake of state security.
% DISAPPEARANCE_RATIONALE: If the constraint vanished—if total war became unreachable rather than merely improbable—international security would reorganize completely. Major powers would lose their ultimate threat, making conventional conflicts resume their historical escalation patterns. Alliance structures would dissolve as security assurances became worthless. Non-aligned states would face new opportunities to develop autonomous deterrents or pursue non-aligned realignment. The institutional structures built around deterrence (nuclear commands, strategic doctrine, arms control regimes) would lose their raison d'être. The world would not return to a pre-nuclear state but would reorganize around new assumptions about what constrains major war.
% FOUNDING_PROBLEM: Asymmetric escalation risk in major-power conflicts. Before nuclear weapons, wars between great powers could be won; the winner would absorb costs and claim territory or reparations. After nuclear weapons, any major-power conflict could escalate into mutual annihilation. This creates a new problem: how do rivals settle disputes if any escalation could be terminal? The deterrence constraint was built to solve this: make total war so costly for all parties that no rational actor initiates the escalation sequence. Establish that defection from peaceful settlement would trigger catastrophic retaliation.
% FOUNDING_PROBLEM_CORROBORATION: Strategic doctrine texts (Schelling, Jervis, Waltz) and Cold War historical analysis attest the founding problem was urgent in the 1950s-1980s: major-power war seemed possible and catastrophic. Current disarmament literature and diplomatic initiatives (Iran nuclear deal, New START negotiations, ICAN campaign) argue the problem is being managed but not solved: deterrence is a temporary equilibrium whose underlying instability is growing. Independent scholarship from security studies (outside the nuclear deterrence establishment) documents accident risks, command-and-control vulnerabilities, and the fragility of deterrence assumptions under new technologies (AI, cyber, hypersonics). This corroboration from outside the nuclear powers' official narrative suggests the founding problem persists in new forms rather than being resolved.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__dropping_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__dropping_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__dropping_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_reachability_boundary__dropping_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__dropping_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   The DROPPING READING classifies this as a TANGLED_ROPE because the constraint exhibits both genuine coordination (nuclear powers cooperate to prevent escalation spirals that would destroy them all) AND asymmetric extraction (civilians and non-aligned states bear existential risk without voice in the coordination game). Extractiveness is high (0.68 at interval end) because the arrangement transfers security to some actors and existential vulnerability to others, without compensating the victims or offering them exit. Suppression is higher still (0.72) because the constraint actively suppresses disarmament alternatives through nonproliferation regimes, doctrine that treats total war as permanent, and institutional marginalization of voices calling for disarmament. Theater is moderate (0.41): the constraint involves genuine strategic calculation and force maintenance, but a growing share of its enforcement is theatrical—doctrinal posturing, ceremonial weapons displays, threat signaling that sustains credibility rather than resolving underlying conflicts. The measurement series shows extractiveness declining modestly from 0.71 to 0.67 mid-interval (reflecting post-Cold War confidence that total war became less likely) before rebounding to 0.68 at interval end (reflecting renewed concern about great-power competition and stability), suggesting the constraint's form stabilized despite probability shifts. Suppression requirement showed similar V-shaped dynamics: declining confidence in disarmament (1990s-2000s) then increasing awareness that suppression must be actively maintained as proliferation and emerging powers challenge deterrence foundations.
 *
 * PERSPECTIVAL GAP:
 *   The nuclear weapon states and their allies experience this constraint as genuine coordination—a shared interest in preventing mutual annihilation that requires active maintenance. From this seat, deterrence is rational cooperation, not extraction. But from the seat of global civilian populations and non-aligned states, the same structure appears as coerced vulnerability: they are trapped in a game they did not consent to, cannot exit, and bear the ultimate cost if coordination breaks down. The engine computes these divergent seats from the structural data (powerless vs. institutional, trapped vs. arbitrage exit options, direct vs. diffuse vulnerability). This perspectival gap is the core of the TANGLED ROPE classification: the same constraint solves a genuine coordination problem for some parties while extracting catastrophic risk from others.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states carry directionality near the beneficiary end (d ≈ 0.15–0.25): they benefit from deterrence credibility, control the rules, face constrained exit (abandoning nuclear forces would undermine their security), and hold institutional power. Non-nuclear allies sit near symmetric (d ≈ 0.45–0.55): they gain security assurances but also bear indirect escalation risk; their exit is constrained by alliance structure but not completely trapped. Global civilian populations carry directionality near the full-target end (d ≈ 0.8–0.9): they are powerless, trapped (cannot exit inhabiting the Earth), bear existential vulnerability, and have no voice in the coordination. The constraint's enforcement machinery—weapons maintenance, doctrine, threat signaling, nonproliferation rules—falls disproportionately on extracting and suppressing from the victims' seat rather than on beneficiary redistribution, which amplifies their effective directionality upward.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids the false-summit trap that would classify deterrence as a MOUNTAIN (natural law). Deterrence is strategically contingent: it persists because rational actors choose to maintain it, not because it emerges from physics or logic alone. If actors changed their preferences or beliefs about retaliation, deterrence could collapse. The TANGLED_ROPE classification correctly captures that deterrence is a constructed coordination arrangement with active enforcement requirements (weapons maintenance, doctrinal signaling, alliance management, nonproliferation suppression). The reading also avoids the opposite trap of the CONTRACTION READING (that total war is no longer reachable). The DROPPING READING locates reachability as a persistent feature of the strategic landscape, even as probability declined: the capacity and willingness remain, held in equilibrium by credible second-strike forces and mutually assured vulnerability. This prevents mandatrophy—the arrangement hasn't outlived its founding function; deterrence still actively prevents major-power escalation. But it does suggest a boundary between the coordination function (genuine, persistent) and the extraction overlay (suppressed disarmament, uncompensated vulnerability).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_stability_fragility,
    'Is the deterrence equilibrium structurally stable, or does it rest on assumptions that are increasingly violated (command-and-control reliability, rationality, symmetry of interests)?',
    'Near-miss analysis (escalation incidents), modeling of accident scenarios, assessment of emerging technologies (AI, hypersonics, cyber weapons) that might undermine deterrence assumptions.',
    'If the equilibrium is fragile, the constraint should be reclassified as a SNARE (coerced by mutual vulnerability, not stable cooperation). If stable despite new technologies, the tangled_rope classification holds. If becoming more robust, the constraint trends toward ROPE (pure coordination with minimal extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_stability_fragility, empirical, 'Whether deterrence equilibrium remains plausibly stable given evolving technology and state interests.').

omega_variable(
    reachability_versus_probability_distinction,
    'Is the distinction between ''total war dropped in probability'' and ''remains reachable'' operationally meaningful, or does low probability converge on impossibility from a rational actor''s perspective?',
    'Examine strategic doctrine and force planning assumptions: if planners treat total war as genuinely reachable (maintaining second-strike forces, updating doctrine for new scenarios), reachability is a live strategic fact. If planning converges on treating it as vanishingly unlikely, the CONTRACTION_READING (reachability departed the feasible set) becomes more plausible.',
    'If reachability is merely probabilistic noise, the constraint should be reclassified toward CONTRACTION or ROPE. If reachability remains a structural feature of strategic planning and force design, the DROPPING READING holds and extraction (vulnerability to existential risk) remains real.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reachability_versus_probability_distinction, conceptual, 'Operational meaning of the reachability/probability distinction in strategic calculation.').

omega_variable(
    suppressed_disarmament_alternative,
    'What does it cost in terms of institutional power, policy autonomy, and security assurance for states to exit the deterrence arrangement and pursue comprehensive disarmament?',
    'Historical analysis of disarmament initiatives (NPT, CWC, CTBT outcomes), game-theoretic modeling of exit incentives, assessment of regional security dynamics under disarmament scenarios.',
    'If exit costs are prohibitive for all parties (security vacuum, power redistribution), the extraction is structural and the TANGLED_ROPE classification is robust. If disarmament paths are feasible but suppressed by preference rather than by structural constraint, the classification may underestimate the victims'' agency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppressed_disarmament_alternative, empirical, 'Cost structure of exiting deterrence coordination through comprehensive disarmament.').

omega_variable(
    reading_boundary_kernel_ambiguity,
    'Does the distinction between the DROPPING_READING and the CONTRACTION_READING rest on an empirically resolvable fact, or is it constitutive—does each reading define ''reachability'' and ''feasible set'' differently such that both readings are always locally coherent?',
    'Examine the readings'' definitions: if ''reachable'' means ''physically capable and strategically rational'' (DROPPING) vs. ''winnable without mutual destruction'' (CONTRACTION), the definitions are distinct and one fact can resolve the ambiguity. If ''reachable'' is framework-dependent and both readings define it consistently with their premises, the ambiguity is conceptual.',
    'If empirically resolvable, future evidence (technological breakthroughs, doctrine shifts, new strategic scenarios) can move the constraint between readings. If conceptual, the readings are permanently coexisting interpretations of the same kernel, and the engine should route to COEXISTS_WITH rather than logical foreclosure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_kernel_ambiguity, conceptual, 'Whether the DROPPING/CONTRACTION boundary is empirical or constitutive.').

omega_variable(
    suppression_mechanism_internalized_or_structural,
    'Is the suppression of disarmament alternatives structural (enforced by international regimes, alliance dependencies, military-industrial interests) or internalized (states have come to believe deterrence is necessary and disarmament is irrational)?',
    'Examine state behavior when external constraints relax: if states maintain deterrence commitment when legally free to disarm, suppression is internalized. If states pursue disarmament when external constraints ease (domestic political shifts, regional peace breakthroughs), suppression is structural.',
    'If structural, the constraint''s effective suppression (measured capacity to maintain coerced vulnerability) is durably high. If internalized, the suppression persists post-exit and victims carry it forward, raising effective extraction beyond what the constraint itself imposes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_or_structural, empirical, 'Whether suppression of disarmament is enforced externally or internalized by states.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__dropping_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t0, total_war_reachability_boundary__dropping_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(tota_tr_t0, observed).
narrative_ontology:measurement(tota_tr_t5, total_war_reachability_boundary__dropping_reading, theater_ratio, 5, 0.37).
narrative_ontology:measurement_basis(tota_tr_t5, observed).
narrative_ontology:measurement(tota_tr_t10, total_war_reachability_boundary__dropping_reading, theater_ratio, 10, 0.39).
narrative_ontology:measurement_basis(tota_tr_t10, observed).
narrative_ontology:measurement(tota_tr_t15, total_war_reachability_boundary__dropping_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(tota_tr_t15, observed).
narrative_ontology:measurement(tota_tr_t20, total_war_reachability_boundary__dropping_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(tota_tr_t20, observed).
narrative_ontology:measurement(tota_tr_t25, total_war_reachability_boundary__dropping_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(tota_tr_t25, observed).
narrative_ontology:measurement(tota_tr_t30, total_war_reachability_boundary__dropping_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(tota_tr_t30, observed).
narrative_ontology:measurement(tota_tr_t40, total_war_reachability_boundary__dropping_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(tota_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(tota_be_t0, total_war_reachability_boundary__dropping_reading, base_extractiveness, 0, 0.71).
narrative_ontology:measurement_basis(tota_be_t0, observed).
narrative_ontology:measurement(tota_be_t5, total_war_reachability_boundary__dropping_reading, base_extractiveness, 5, 0.69).
narrative_ontology:measurement_basis(tota_be_t5, observed).
narrative_ontology:measurement(tota_be_t10, total_war_reachability_boundary__dropping_reading, base_extractiveness, 10, 0.67).
narrative_ontology:measurement_basis(tota_be_t10, observed).
narrative_ontology:measurement(tota_be_t15, total_war_reachability_boundary__dropping_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(tota_be_t15, observed).
narrative_ontology:measurement(tota_be_t20, total_war_reachability_boundary__dropping_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(tota_be_t20, observed).
narrative_ontology:measurement(tota_be_t25, total_war_reachability_boundary__dropping_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(tota_be_t25, observed).
narrative_ontology:measurement(tota_be_t30, total_war_reachability_boundary__dropping_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(tota_be_t30, observed).
narrative_ontology:measurement(tota_be_t40, total_war_reachability_boundary__dropping_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(tota_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t0, total_war_reachability_boundary__dropping_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement_basis(tota_su_t0, observed).
narrative_ontology:measurement(tota_su_t5, total_war_reachability_boundary__dropping_reading, suppression_requirement, 5, 0.74).
narrative_ontology:measurement_basis(tota_su_t5, observed).
narrative_ontology:measurement(tota_su_t10, total_war_reachability_boundary__dropping_reading, suppression_requirement, 10, 0.73).
narrative_ontology:measurement_basis(tota_su_t10, observed).
narrative_ontology:measurement(tota_su_t15, total_war_reachability_boundary__dropping_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement_basis(tota_su_t15, observed).
narrative_ontology:measurement(tota_su_t20, total_war_reachability_boundary__dropping_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(tota_su_t20, observed).
narrative_ontology:measurement(tota_su_t25, total_war_reachability_boundary__dropping_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(tota_su_t25, observed).
narrative_ontology:measurement(tota_su_t30, total_war_reachability_boundary__dropping_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(tota_su_t30, observed).
narrative_ontology:measurement(tota_su_t40, total_war_reachability_boundary__dropping_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(tota_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__dropping_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(total_war_reachability_boundary__dropping_reading, 0.18).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary__contraction_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary__contingent_reachability_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, nonproliferation_regime).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, extended_deterrence_alliance_system).

% DUAL FORMULATION NOTE:
% The total_war_reachability_boundary kernel decomposes into three readings representing distinct structural claims about reachability and deterrence function. The DROPPING_READING (this file) treats deterrence as a tangled_rope coordination equilibrium with extraction overlay. The CONTRACTION_READING treats total war as physically impossible (mountain). The CONTINGENT_REACHABILITY_READING treats reachability as purely technology-dependent (piton or rope depending on tech state). All three readings share a referent (current strategic vulnerability) but instantiate different constraint types and operate under different axioms about what constrains total war. They are linked as constraint family members, not as competing measurements of a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_reachability_boundary__dropping_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
