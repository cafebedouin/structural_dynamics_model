% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__deterrence_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: total_war_possibility_space__deterrence_equilibrium_reading
 *   human_readable: Total-War Possibility Space - Deterrence Equilibrium Reading
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   Since the maturation of survivable second-strike forces in the early
 *   1960s, the nuclear-armed great powers have maintained a standing
 *   arrangement whose operation keeps total war reachable-but-uninitiated:
 *   war plans continuously revised, arsenals recapitalized on political
 *   rather than operational schedules, escalation ladders theorized and
 *   exercised, command systems hardened. That arrangement is this file's
 *   referent, assessed by the deterrence-equilibrium reading's own lights:
 *   the withholding mechanism is material cost-benefit calculation under
 *   mutual vulnerability, and the reading predicts exactly what the record
 *   shows - doctrine development continues, counterforce targeting persists,
 *   escalation ladders remain theorized, and capability investment never
 *   stops. The claim/metric split is deliberate: claimed_type states the
 *   structure I believe true (a hybrid that genuinely coordinates
 *   war-prevention while extracting maintenance rents); the metrics describe
 *   the arrangement's actual operation, independently. Sibling readings of
 *   the same kernel are separate constraints in separate files, linked
 *   through network.affects_constraints; this file does not average across
 *   them and does not hedge epsilon over them. KEY AGENTS (by structural
 *   relationship): - strategic_war_planning_establishments: agenda-setting
 *   seat (institutional/identity_locked) - writes the plans whose existence
 *   keeps total war operable - peer_adversary_strategic_commands: mirror
 *   agenda-setting seat - its maintained capability is what the first side's
 *   deterrent answers - nuclear_weapons_laboratories: beneficiary seat
 *   (institutional/constrained) - funded to keep the arsenal credible -
 *   defense_industrial_contractors: beneficiary and receipt seat
 *   (powerful/arbitrage) - receives the modernization flows -
 *   extended_deterrence_protectorates: dual beneficiary/payer seat
 *   (organized/constrained) - sheltered under the umbrella, hosts the
 *   weapons, carries target status - taxpaying_publics_nuclear_states: payer
 *   seat (moderate/trapped) - funds the maintenance with weak doctrinal voice
 *   - civilian_populations_in_targeting_scenarios: payer/beneficiary seat
 *   (powerless/trapped) - held at risk as the guarantee of retaliation,
 *   protected by the same fact - nuclear_abolition_advocates: excluded seat
 *   (organized/constrained) - would remove the possibility space entirely;
 *   outside the planning rooms - arms_control_negotiators: observer seat
 *   (institutional/analytical) - measures and manages drift without authority
 *   to redirect doctrine
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__deterrence_equilibrium_reading, 0.64).
domain_priors:suppression_score(total_war_possibility_space__deterrence_equilibrium_reading, 0.7).
domain_priors:theater_ratio(total_war_possibility_space__deterrence_equilibrium_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__deterrence_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__deterrence_equilibrium_reading, "Total-War Possibility Space - Deterrence Equilibrium Reading").
narrative_ontology:topic_domain(total_war_possibility_space__deterrence_equilibrium_reading, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_possibility_space__deterrence_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__deterrence_equilibrium_reading, '769c0620-de8f-4dd0-8ffb-72ba3bcbb851').
narrative_ontology:cs_kernel_codification('769c0620-de8f-4dd0-8ffb-72ba3bcbb851', formalized).
narrative_ontology:cs_authority_grounding('769c0620-de8f-4dd0-8ffb-72ba3bcbb851', expertise).
narrative_ontology:cs_interpretation_layer_present('769c0620-de8f-4dd0-8ffb-72ba3bcbb851').
narrative_ontology:cs_reading_relation('769c0620-de8f-4dd0-8ffb-72ba3bcbb851', total_war_possibility_space__space_contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('769c0620-de8f-4dd0-8ffb-72ba3bcbb851', total_war_possibility_space__nuclear_taboo_reading, coexists_with).
narrative_ontology:cs_axiom('769c0620-de8f-4dd0-8ffb-72ba3bcbb851', foundational, mutual_vulnerability_sustains_peace).
narrative_ontology:cs_axiom_status(mutual_vulnerability_sustains_peace, holdable).
narrative_ontology:cs_axiom_grounding('769c0620-de8f-4dd0-8ffb-72ba3bcbb851', mutual_vulnerability_sustains_peace, empirically_contingent).
narrative_ontology:cs_axiom('769c0620-de8f-4dd0-8ffb-72ba3bcbb851', secondary, credible_retaliation_requires_continuous_modernization).
narrative_ontology:cs_axiom_status(credible_retaliation_requires_continuous_modernization, holdable).
narrative_ontology:cs_axiom_grounding('769c0620-de8f-4dd0-8ffb-72ba3bcbb851', credible_retaliation_requires_continuous_modernization, instrumental).
narrative_ontology:cs_reference_frame('769c0620-de8f-4dd0-8ffb-72ba3bcbb851', calculated_mutual_vulnerability_equilibrium).
narrative_ontology:cs_drift_state('769c0620-de8f-4dd0-8ffb-72ba3bcbb851', contemporary_multipolar_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('769c0620-de8f-4dd0-8ffb-72ba3bcbb851', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, strategic_war_planning_establishments).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_weapons_laboratories).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, defense_industrial_contractors).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, extended_deterrence_protectorates).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, taxpaying_publics_nuclear_states).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, civilian_populations_in_targeting_scenarios).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_abolition_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, civilian_populations_in_targeting_scenarios).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, extended_deterrence_protectorates).
narrative_ontology:constraint_vindicates(total_war_possibility_space__deterrence_equilibrium_reading, rational_deterrence_theory).
narrative_ontology:constraint_vindicates(total_war_possibility_space__deterrence_equilibrium_reading, crisis_stability_doctrine).
narrative_ontology:constraint_vindicates(total_war_possibility_space__deterrence_equilibrium_reading, second_strike_credibility_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and continuously revises the war plans, targeting doctrine, and escalation ladders whose continued existence keeps total war an operable option. Staffs command centers, runs the exercises that demonstrate readiness, and certifies that retaliation would in fact execute. Budgets, promotion pipelines, and institutional prestige are tied to the mission's permanence; leaving the mission would mean dissolving the organization's reason to exist, which its members experience as professionally unthinkable.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, strategic_war_planning_establishments, agenda_setter,
    institutional, generational, identity_locked, global).

% The mirror establishment on the other side of the rivalry: maintains its own survivable forces, its own targeting doctrine, its own escalation theory. Its maintained capability is precisely what the first side's investment answers, and vice versa; each command cites the other's arsenal as the reason its own must persist. Together the two commands constitute the mutual-vulnerability structure neither could dismantle alone.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, peer_adversary_strategic_commands, agenda_setter,
    institutional, generational, identity_locked, global).

% Receive standing public funding for stockpile stewardship, warhead life-extension, and weapons science, justified by the requirement that the arsenal remain credible. Their work programs are scheduled by maintenance politics rather than by any war being fought. Repurposing their missions would require re-founding their scientific workforce around different problems, which past conversion attempts show is slow and partial.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_weapons_laboratories, beneficiary,
    institutional, generational, constrained, national).

% Build and upgrade the delivery systems, submarines, missiles, and command infrastructure under continuous multi-decade recapitalization cycles. Procurement revenue concentrates here; program timelines are calibrated to political budget cycles. Portfolios are diversified enough that individual firms could shift toward conventional markets, but the strategic programs are their largest and most durable revenue lines.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, defense_industrial_contractors, beneficiary,
    powerful, biographical, arbitrage, continental).

% Allies sheltered under another power's nuclear guarantee: they receive security against major attack without fielding their own arsenals, and in exchange host forward-deployed weapons, provide basing and early-warning participation, and accept placement on adversary target lists. Their defense planning is built around the umbrella; abandoning it would mean either independent nuclearization, with proliferation consequences, or accommodation with the adversary, both severe.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, extended_deterrence_protectorates, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__deterrence_equilibrium_reading, extended_deterrence_protectorates, payer).

% Fund the entire maintenance enterprise through general revenues, decade after decade, with little direct voice over doctrine or force size. They cannot exit the taxation or the territory, and the strategic programs are among the least auditable lines of the defense budget. They receive the war-prevention the arrangement delivers but do not set its terms.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, taxpaying_publics_nuclear_states, payer,
    moderate, biographical, trapped, national).

% Are written into the war plans of every nuclear power as the casualties that make retaliation credible: their exposure is the mechanism. The same exposure is why no total war comes. They have no seat anywhere in the planning process, appear in the documents only as effects, and cannot exit their geography. On both sides of every rivalry they are simultaneously the hostage and the protected.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, civilian_populations_in_targeting_scenarios, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__deterrence_equilibrium_reading, civilian_populations_in_targeting_scenarios, beneficiary).

% Campaign for eliminating the possibility of total war altogether, through treaty bans, humanitarian-initiative framing, and stigmatization campaigns. They hold conferences and pass conventions but are absent from the rooms where targeting doctrine and force posture are decided. The credibility logic of the arrangement defines their project as dangerous naivete, which is what keeps them outside.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_abolition_advocates, excluded,
    organized, generational, constrained, global).

% Diplomats and technical experts who negotiate ceilings, verification regimes, and crisis-communication protocols intended to stabilize the arrangement and slow its drift. They measure the arsenals, inspect the facilities, and document the treaty lapses, but hold no authority to redirect doctrine or force structure; their leverage ends where the war plans begin.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, arms_control_negotiators, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_possibility_space__deterrence_equilibrium_reading, defense_industrial_contractors).
narrative_ontology:fixing_cost_class(total_war_possibility_space__deterrence_equilibrium_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps rival nuclear-armed states out of total war by making initiation predictably self-destructive: each side's maintained second-strike capability converts the other's war option into a losing move, while crisis communication channels, escalation thresholds, and verification regimes coordinate expectations so that neither side acts on a believed first-strike advantage.
% TRANSFER_FUNCTION: Moves a continuous stream of fiscal resources from general taxpaying publics to weapons laboratories, delivery-system industries, and command infrastructure, sized to keep the retaliatory threat credible rather than to any war-fighting need; and moves existential risk onto civilian populations, whose exposure is the mechanism that makes the threat credible.
% ABSENT_VOICES: Abolition advocates and the civilian populations written into targeting plans have no seat where doctrine is drafted; non-nuclear host states have consultative but not deciding voices; the populations of adversary states appear in the plans only as targets. Unanimity inside the planning rooms arises partly because these seats were never in them.
% DISAPPEARANCE_RATIONALE: If mutual vulnerability failed overnight - arsenals vanished, or defenses became leak-proof - war plans would convert from deterrent signaling to executable options within months, alliance architectures built on extended deterrence would unravel or trigger proliferations, and the fiscal flows sustaining the weapons complex would redirect or collapse. The post-1945 absence of great-power total war depends on this arrangement persisting.
% FOUNDING_PROBLEM: How rival states capable of annihilating each other can coexist without either trusting the other's restraint or submitting to a world government - how to prevent total war between nuclear-armed adversaries while remaining adversaries.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside any single benefiting party: adversary governments' own published doctrines attest the problem is live, each citing the other's capability as the reason for its posture; declassified crisis archives (executive-committee recordings, general staff records) and the broad scholarly finding that great-power war has been absent since 1945 under conditions that previously produced it repeatedly; arms-control inspection bodies on both sides attest continued capability. No corroborating source outside the benefiting parties claims the founding problem is solved.
narrative_ontology:disappearance_verdict(total_war_possibility_space__deterrence_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__deterrence_equilibrium_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__deterrence_equilibrium_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_possibility_space__deterrence_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__deterrence_equilibrium_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.64 (interval end): the maintenance demand - recapitalized triads, warhead life-extension, command modernization - is decoupled from any war-fighting requirement, since no total war is being fought or planned to be fought; the flows are sized to credibility, which no operational need bounds from below. Suppression is 0.70: alternatives to the equilibrium (deep reductions, abolition, non-nuclear defense postures) are suppressed by the credibility logic itself, which defines any visible weakening as invitation, and the current treaty architecture is lapsing, narrowing exits further. Theater_ratio is 0.32: readiness, command-and-control, and exercise activity are functionally load-bearing, but a growing share of activity is signaling - arsenal sizes beyond any targeting requirement, parade-scale demonstration, declaratory documents aimed at audiences rather than operations. Accessibility_collapse is 0.48: understanding the equilibrium collapses unilateral-exit alternatives almost completely while leaving negotiated-management alternatives partially open, as the SALT/START/INF record shows. Resistance is 0.42: abolition campaigns meet the arrangement but have moved no governing seat. Coordination type is enforcement_mechanism: the arrangement governs rival conduct through managed threat rather than producing, allocating, or standardizing anything.
 *   
 *   CYCLICAL PATTERN: suppression_requirement oscillates with the exogenous geopolitical tension cycle - Cold War buildups and detente thaws, the post-1991 drawdown, the post-2010 revival - tracing one full down-up cycle across the thirteen-point grid. The oscillation is a side effect of external tension, not an intermittent-reinforcement extraction mechanism. Extractiveness dips with each drawdown but never returns to pre-buildup levels: capabilities, once built, are politically irreversible, so each cycle ratchets the floor upward. Theater_ratio spikes when the function atrophies fastest (the 1990s, when the apparatus shrank slower than its mission) and eases when real recapitalization resumes. All three series share one 13-point grid (t=0..60, five-year steps, approximately 1962-2022); the base_properties scalars are the t=60 endpoints, measured on the rising phase of the current cycle.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the war-planning establishments (identity_locked), the arrangement is their professional lifework and the acknowledged preventer of great-power war - the coordination function dominates their view and the maintenance costs read as necessity. From the taxpaying publics (trapped, weak voice), the same arrangement reads as a permanent fiscal claim sized by inter-service rivalry and contractor cycle-times rather than by any calculable requirement. From the protectorates (dual position), security subsidy and entrapment risk arrive in the same package. From the targeted civilian populations, the arrangement is experienced as hostagehood that is simultaneously the protection. The engine computes per-seat classifications from power, exit, and declared position; the divergence between the establishment seat and the payer seats is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: the establishments, laboratories, and contractors sit near the subsidized end - the arrangement's persistence is their budget justification, and the contractors additionally hold arbitrage-grade portfolio exit. The protectorates derive low d from their beneficiary declaration, tempered by their payer secondary role (target status, basing costs) and constrained exit, placing them nearer the middle than a pure beneficiary. Victim declarations drive high d: taxpayers (trapped, moderate power) and targeted populations (trapped, powerless, civilizational horizon) sit near the full-target end; the populations' incidental protection benefit does not offset their structural position as the mechanism's collateral. Abolition advocates carry high d through foreclosed-alternative costs. No directionality overrides were needed: the derivation from declared positions and exit options tracks the structural relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - how mutually capable adversaries coexist without trust or world government - is live, corroborated by adversary doctrines and the crisis archive; nothing here is a mandate outliving its function, so no mandatrophy resolution is declared, and the R5 mismatch consumer finds status=live paired with verdict=world_rearranges, a coherent pairing that raises no zombie flag. The classification work this story performs is boundary-keeping in both directions: against the pure-coordination reading (the coordination function is real, but the maintenance flows exceed any defensible coordination floor and alternatives are actively narrowed, so rope framing would launder the fiscal claim), and against the pure-extraction reading (the war-prevention function demonstrably operates - the post-1945 great-power silence is the coordinated output - so the coordination story is not cover). The hybrid category holds both facts without merging them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location_of_constraint,
    'Which reading of the total_war_possibility_space kernel correctly locates the operative constraint - material cost-benefit calculation under mutual vulnerability (this file), normative prohibition (nuclear_taboo_reading), or cognitive removal from the strategically thinkable (space_contraction_reading)?',
    'Comparative classification across the three linked stories plus discriminating archival evidence: cases where capability existed and calculation favored use but restraint held anyway (Korea 1950-53, the Vietnam nuclear-option debates) discriminate the taboo reading from this one; the doctrinal continuity of counterforce planning and escalation-ladder theorizing discriminates this reading from the contraction reading.',
    'If the taboo reading is correct, this arrangement''s extraction rides on a norm it did not create and its maintenance claims weaken accordingly; if the contraction reading is correct, the maintenance apparatus guards a possibility that no longer exists and the arrangement drifts toward inertial performance; if this reading is correct, the equilibrium is load-bearing and its costs are the price of the post-1945 great-power silence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location_of_constraint, conceptual, 'Kernel-level ambiguity over which mechanism withholds total war; this file instantiates only the deterrence-equilibrium answer.').

omega_variable(
    rational_actor_premise_soundness,
    'Does the reading''s foundational empirical premise hold - do leaders actually initiate or refrain through rational cost-benefit calculation under mutual vulnerability, given archival evidence that near-misses turned on misperception, malfunction, and individual refusal?',
    'Continued declassification of crisis decision records, systematic comparison of crisis-model predictions against revealed behavior, and engineering study of automation and compressed-decision-time failure modes.',
    'If the premise is unsound, the equilibrium is maintained against a failure mode its own theory cannot register; the suppression picture is understated because the binding risk is inadvertent rather than chosen, and the arrangement''s classification shifts toward a support structure that declares no transition and no sunset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rational_actor_premise_soundness, empirical, 'Whether the cost-benefit premise survives the crisis archive.').

omega_variable(
    coordination_floor_vs_rent_share,
    'How much of the measured extraction is the inherent coordination cost of a credible mutual-vulnerability system, and how much is rent captured above any defensible minimum deterrent?',
    'Independent costing of minimum-credible-deterrent postures (single-leg forces, reduced-yield stockpiles, finite target sets) against actual maintenance budgets across all nuclear powers.',
    'A wide excess confirms the hybrid reading with drift pressure toward pure extraction; a narrow excess would support reclassification toward pure coordination, with the remainder priced as the floor of war-prevention itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_floor_vs_rent_share, empirical, 'Splitting the maintenance bill into coordination floor and captured rent.').

omega_variable(
    multipolar_logic_transfer,
    'Does the equilibrium''s two-player calculative logic survive the shift to three or more peer nuclear competitors with entangled alliance commitments?',
    'Track crisis behavior and signaling among multiple simultaneous nuclear peers; model triadic and n-adic stability conditions against the bipolar baseline.',
    'Multipolarity raises miscalculation probability and amplifies effective extraction through scope, since verification grows harder as scale grows; if the logic fails to transfer, the coordination function degrades while the fiscal claim persists - the characteristic drift signature this corpus watches for.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(multipolar_logic_transfer, empirical, 'Whether bipolar-era stability logic transfers to the emerging multipolar configuration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__deterrence_equilibrium_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t0, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(tota_tr_t5, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 5, 0.17).
narrative_ontology:measurement(tota_tr_t10, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(tota_tr_t15, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(tota_tr_t20, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(tota_tr_t25, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement(tota_tr_t30, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 30, 0.33).
narrative_ontology:measurement(tota_tr_t35, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 35, 0.36).
narrative_ontology:measurement(tota_tr_t40, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 40, 0.34).
narrative_ontology:measurement(tota_tr_t45, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 45, 0.31).
narrative_ontology:measurement(tota_tr_t50, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 50, 0.29).
narrative_ontology:measurement(tota_tr_t55, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 55, 0.3).
narrative_ontology:measurement(tota_tr_t60, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 60, 0.32).

% Extraction over time
narrative_ontology:measurement(tota_be_t0, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(tota_be_t5, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 5, 0.56).
narrative_ontology:measurement(tota_be_t10, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(tota_be_t15, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 15, 0.57).
narrative_ontology:measurement(tota_be_t20, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(tota_be_t25, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(tota_be_t30, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(tota_be_t35, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 35, 0.38).
narrative_ontology:measurement(tota_be_t40, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 40, 0.41).
narrative_ontology:measurement(tota_be_t45, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 45, 0.46).
narrative_ontology:measurement(tota_be_t50, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 50, 0.52).
narrative_ontology:measurement(tota_be_t55, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 55, 0.58).
narrative_ontology:measurement(tota_be_t60, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 60, 0.64).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t0, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement(tota_su_t5, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 5, 0.8).
narrative_ontology:measurement(tota_su_t10, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(tota_su_t15, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(tota_su_t20, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 20, 0.76).
narrative_ontology:measurement(tota_su_t25, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(tota_su_t30, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 30, 0.4).
narrative_ontology:measurement(tota_su_t35, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 35, 0.32).
narrative_ontology:measurement(tota_su_t40, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 40, 0.38).
narrative_ontology:measurement(tota_su_t45, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 45, 0.44).
narrative_ontology:measurement(tota_su_t50, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 50, 0.52).
narrative_ontology:measurement(tota_su_t55, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 55, 0.6).
narrative_ontology:measurement(tota_su_t60, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 60, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__deterrence_equilibrium_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space__space_contraction_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space__nuclear_taboo_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_nonproliferation_regime).

% DUAL FORMULATION NOTE:
% The colloquial question 'why has total war not recurred?' decomposes into three structurally distinct constraints sharing one label, per the epsilon-invariance principle. This file authors epsilon for the maintained mutual-vulnerability arrangement as the deterrence-equilibrium reading assesses it: moderately-high extraction riding a real coordination function. space_contraction_reading authors epsilon for the claim that war left the strategically thinkable (its victims are strategic imagination and doctrinal honesty). nuclear_taboo_reading authors epsilon for the normative prohibition (its beneficiaries include norm entrepreneurs, not weapons complexes). Downstream coupling: this reading's material facts are cited as evidence by the taboo reading and challenged by the contraction reading; the equilibrium also underwrites the nonproliferation bargain, since umbrella states forgo arsenals in exchange for shelter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
