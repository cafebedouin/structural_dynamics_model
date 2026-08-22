% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__stateless_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__stateless_coordination_reading, []).

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
 *   constraint_id: feud_obligation_kernel__stateless_coordination_reading
 *   human_readable: Blood-Feud Obligation System (Stateless Coordination Reading)
 *   domain: legal_anthropology/comparative_political_systems
 *
 * SUMMARY:
 *   This story instantiates the stateless_coordination_reading of the
 *   feud_obligation_kernel: the claim that blood-feud obligations constitute
 *   a self-enforcing coordination mechanism providing justice and deterrence
 *   where centralized enforcement capacity is absent. On this reading the
 *   feud is the enforcement infrastructure of stateless society: kin-group
 *   collective liability makes every member's security a corporate concern,
 *   credible retaliation deters predation, and customary procedure (truce
 *   periods, safe conduct, proportional-response limits, compensation
 *   negotiation) keeps reciprocal violence from annihilating the groups it
 *   binds. The reading's structural signature is deliberate:
 *   feud-participating kin groups sit in the beneficiary set (they receive
 *   justice and deterrence), defectors sit in the victim set (honor loss,
 *   kinship expulsion), and alternative dispute mechanisms are suppressed
 *   only weakly — wergild compensation coexists with vengeance throughout.
 *   Claimed type and metrics are authored independently: the reading claims a
 *   coordination mechanism, while the authored metrics describe moderate
 *   extraction that accumulates over the interval as prestige inflation and
 *   escalation cycles load costs onto the enforcement arm and onto defectors.
 *   The engine computes per-seat classifications from the structural data;
 *   where a computed type diverges from the coordination claim, that
 *   divergence is the measurement this corpus exists to take. Sibling
 *   readings (extraction_cycle_reading, christianized_pacification_reading)
 *   are separate constraints with separate epsilons over the same historical
 *   material, linked through the network block.
 *
 * KEY AGENTS:
 *   - - feud_participant_kin_groups: Primary beneficiary (organized/constrained) — corporate descent groups that receive deterrence and a rule-governed path to redress
 *   - - feud_defectors: Primary target (powerless/trapped) — members who refuse vengeance obligations and bear honor loss and expulsion
 *   - - vengeance_bound_young_men: Cost-bearing enforcement arm (moderate/identity_locked) — execute raids and retaliations, bear the mortality risk
 *   - - wergild_mediators: Secondary beneficiary (moderate/mobile) — neutral kin who broker settlements and collect mediator gifts
 *   - - customary_law_custodians: Agenda-setter (moderate/mobile) — lawspeakers and elders who recite and transmit the procedural code
 *   - - ecclesiastical_peace_campaigners: Excluded challenger (institutional/mobile) — church authorities whose normative authority sits outside the feud's operative logic
 *   - - comparative_ethnographers: Analytical observer (analytical/analytical) — record the full structure including costs the participants discount
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__stateless_coordination_reading, 0.4).
domain_priors:suppression_score(feud_obligation_kernel__stateless_coordination_reading, 0.38).
domain_priors:theater_ratio(feud_obligation_kernel__stateless_coordination_reading, 0.21).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0.21).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__stateless_coordination_reading, rope).
narrative_ontology:human_readable(feud_obligation_kernel__stateless_coordination_reading, "Blood-Feud Obligation System (Stateless Coordination Reading)").
narrative_ontology:topic_domain(feud_obligation_kernel__stateless_coordination_reading, "legal_anthropology/comparative_political_systems").

domain_priors:requires_active_enforcement(feud_obligation_kernel__stateless_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__stateless_coordination_reading, 'd6f1b662-dcbd-492a-a977-6ef6992b8b09').
narrative_ontology:cs_kernel_codification('d6f1b662-dcbd-492a-a977-6ef6992b8b09', distributed).
narrative_ontology:cs_authority_grounding('d6f1b662-dcbd-492a-a977-6ef6992b8b09', practice).
narrative_ontology:cs_interpretation_layer_present('d6f1b662-dcbd-492a-a977-6ef6992b8b09').
narrative_ontology:cs_reading_relation('d6f1b662-dcbd-492a-a977-6ef6992b8b09', feud_obligation_kernel__extraction_cycle_reading, coexists_with).
narrative_ontology:cs_reading_relation('d6f1b662-dcbd-492a-a977-6ef6992b8b09', feud_obligation_kernel__christianized_pacification_reading, coexists_with).
narrative_ontology:cs_axiom('d6f1b662-dcbd-492a-a977-6ef6992b8b09', foundational, self_help_justice_legitimate_without_state).
narrative_ontology:cs_axiom_status(self_help_justice_legitimate_without_state, holdable).
narrative_ontology:cs_axiom_grounding('d6f1b662-dcbd-492a-a977-6ef6992b8b09', self_help_justice_legitimate_without_state, instrumental).
narrative_ontology:cs_axiom('d6f1b662-dcbd-492a-a977-6ef6992b8b09', secondary, deterrence_requires_credible_kin_retaliation).
narrative_ontology:cs_axiom_status(deterrence_requires_credible_kin_retaliation, holdable).
narrative_ontology:cs_axiom_grounding('d6f1b662-dcbd-492a-a977-6ef6992b8b09', deterrence_requires_credible_kin_retaliation, empirically_contingent).
narrative_ontology:cs_reference_frame('d6f1b662-dcbd-492a-a977-6ef6992b8b09', stateless_deterrence_equilibrium).
narrative_ontology:cs_drift_state('d6f1b662-dcbd-492a-a977-6ef6992b8b09', state_formation_and_church_pacification_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d6f1b662-dcbd-492a-a977-6ef6992b8b09', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, feud_participant_kin_groups).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, wergild_mediators).
narrative_ontology:constraint_victim(feud_obligation_kernel__stateless_coordination_reading, feud_defectors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, vengeance_bound_young_men).
narrative_ontology:constraint_victim(feud_obligation_kernel__stateless_coordination_reading, vengeance_bound_young_men).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Corporate descent groups bound by collective liability: they answer for their members' acts, pursue claims against offending groups, and in exchange receive deterrence against predation and a rule-governed path to redress when a member is killed. Leaving the arrangement would mean dissolving the kin corporation itself — surrendering the mutual defense that keeps the group viable — so participation is less a choice than a condition of existence.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, feud_participant_kin_groups, beneficiary,
    organized, generational, constrained, regional).

% Individuals — often younger sons, returned traders, or those drawn to religious life — who decline vengeance calls or accept compensation without group sanction. They lose honor standing, are marked as oath-breakers, and face expulsion from kin protection: the punishment strips them of the very security the arrangement exists to provide. There is nowhere inside the society to stand once the kin group withdraws its umbrella.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, feud_defectors, payer,
    powerless, biographical, trapped, regional).

% The men who execute raids, ambushes, and retaliatory killings. They bear the highest mortality risk in the arrangement and inherit debts incurred before their birth. Raised inside kin honor from childhood, their standing, marriage prospects, and sense of self are constituted through the vengeance duty; walking away would mean becoming nobody. They also sleep under the same protective umbrella their service maintains, which is why the duty never reads to them as simple loss.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, vengeance_bound_young_men, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__stateless_coordination_reading, vengeance_bound_young_men, beneficiary).

% Neutral kin groups and respected elders who broker compensation settlements: hosting negotiations, valuing the slain, arranging payment schedules and feast obligations. They collect mediator gifts and accumulate standing with every settlement they close. Their position depends on the arrangement continuing to generate disputes requiring brokerage; they hold no enforcement power of their own and can decline involvement.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, wergild_mediators, beneficiary,
    moderate, generational, mobile, regional).

% Lawspeakers, genealogists, and elders who recite and transmit the procedural code: proportionality limits, truce seasons, safe-conduct rules, compensation tariffs. They articulate what the arrangement permits and forbids, and their recitation shapes each new generation's sense of the duties. They command no armed force; their authority rests on memory, consensus, and the community's continued deference to precedent.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, customary_law_custodians, agenda_setter,
    moderate, generational, mobile, regional).

% Church authorities who condemn vengeance as a usurpation of divine prerogative, impose Peace and Truce provisions, offer sanctuary to fugitives from vengeance, and press penitential alternatives. Their normative authority sits outside the feud's operative logic — the arrangement's legitimacy never routes through ecclesiastical approval — yet they are materially present at its edge, brokering truces and collecting offerings for masses for the slain while campaigning for its abolition.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, ecclesiastical_peace_campaigners, excluded,
    institutional, generational, mobile, continental).

% Scholars recording and comparing feud systems across stateless and post-stateless societies. They see the full structure at once — the justice delivered, the deterrence achieved, the bodies spent, the defectors cast out — including the costs that participants inside the honor economy systematically discount.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, comparative_ethnographers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feud_obligation_kernel__stateless_coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(feud_obligation_kernel__stateless_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In societies lacking centralized enforcement, answers homicide through rule-governed reciprocal violence: kin-group collective liability makes every member's security a corporate concern, credible retaliation deters predation, and procedural custom (truce periods, safe conduct, proportional-response limits, compensation negotiation) channels conflict away from annihilation.
% TRANSFER_FUNCTION: Moves blood-debt obligations and compensation between kin groups — from the offender's kin to the victim's kin, discharged in vengeance or in silver; moves protection guarantees within alliance networks; moves honor and standing toward those who fulfill obligations and away from those who defect.
% ABSENT_VOICES: The slain cannot testify to whether their death purchased justice or merely escalated a cycle. Women in many feud societies transmitted claims and bore feud losses but rarely sat in the councils that declared them. Those who wished to exit the honor economy entirely — to farm, trade, or enter religious life without vengeance duties — had no seat; their preference surfaces only as defection and its sanctions.
% DISAPPEARANCE_RATIONALE: Overnight removal in a stateless society leaves homicide unanswered: predation becomes profitable for the strong, kin groups reorganize around private defense, strongmen, or migration, and the justice function re-emerges in cruder forms — banditry, armed patronage — until a substitute order consolidates. Every named seat loses something: the kin groups their security, the mediators their brokerage, the custodians their office, the young men their standing.
% FOUNDING_PROBLEM: How does a society with no state answer killing, protect its members, and deter the strong from preying on the weak?
% FOUNDING_PROBLEM_CORROBORATION: Comparative ethnography and legal history corroborate from outside any benefiting party: feud institutions appear independently across enforcement vacuums (Nuer segmentary lineages, the Icelandic free state, the Montenegrin and Albanian highlands) and recede as state courts arrive — the vacuum-feud correlation is documented by Evans-Pritchard, Black-Michaud, and the Icelandic saga corpus. The church's own pacification campaign presupposed the problem it condemned. No beneficiary attestation is relied upon.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__stateless_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__stateless_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__stateless_coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(feud_obligation_kernel__stateless_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__stateless_coordination_reading, 0.4, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__stateless_coordination_reading_tests).
:- end_tests(feud_obligation_kernel__stateless_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.40 at interval end) and rising: the feud delivers real justice and deterrence, but escalation cycles, prestige inflation around vengeance display, and the widening gap between obligation and willingness load growing costs onto the young men who fight and the defectors who refuse. Suppression is authored low-to-moderate (0.38): wergild compensation, sanctuary, and ecclesiastical peace operate alongside vengeance throughout, but group-enforced shame sanctions raise the price of choosing compensation alone. Theater is low (0.21): the system's outputs are concrete — settled blood-debts, answered killings — though ritual formalization (truce ceremonies, oath performances) adds a thin performative layer late in the interval. Accessibility collapse is low (0.30): understanding the feud system does not close off alternatives, because wergild and ecclesiastical routes remain usable. Resistance is moderate (0.35): defection, preference for compensation, and flight to sanctuary are chronic. All three tracked series share one time grid (points 0, 6, 12, 18, 24, 30) so no metric row borrows another's endpoints. Identity lock concentrates in the enforcement arm: young men raised inside kin honor have selves constituted through the vengeance duty — relational and institutional fusion, not mere career dependence; if wage markets, state courts, or religious careers offered alternative standing, the lock would break and the enforcement arm would dissolve first. Suppression splits between structural (loss of kin protection, group-administered shame sanction) and internalized (honor ideology); the split is carried as an omega rather than forced into the scalar.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same structure. From the kin-group seat the arrangement is a protection club it cannot safely leave: justice arrives, predation is deterred, and the dues are worth paying. From the defector seat the same arrangement is a sanction machine that confiscates honor and kinship for declining violence. From the young men's seat it is a duty roster that spends their bodies to purchase the group's security. From the custodian seat it is procedure — proportionality limits, truce seasons, compensation tariffs. From the ecclesiastical seat it is usurpation of a prerogative that belongs elsewhere. The engine computes these divergences from power, exit, and directional position; the authored coordination claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Kin groups and mediators sit near the beneficiary end: the arrangement subsidizes them with deterrence and settlement services, and their exit (dissolving the kin corporation itself) is costly enough to keep them invested rather than resentful. Defectors sit near the full-target end: the arrangement extracts honor, membership, and protection from precisely those who decline its duties. The young men occupy the middle with a dual position — they pay the highest physical costs and receive the same protective umbrella — which their secondary beneficiary role registers. Custodians draw status income from administering the code and sit mildly beneficiary-side. Ecclesiastical campaigners are not inside the directionality axis at all: their relationship to the arrangement is oppositional challenge, not extraction or subsidy. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already place each seat correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — answering killing without a state — is live wherever the arrangement operates, so no mandate has outlived its function and no mandatrophy is declared. Classification discipline cuts both ways here. Reading the feud as pure predation ignores its real output: settled blood-debts and deterred raiders are measurable goods the arrangement produces. Reading it as pure coordination ignores the accumulating series: extraction rises across the interval as prestige economics inflate the price of vengeance display and defector sanctions harden. The receipt surface sharpens the picture: gains land diffusely (no seat captures the arrangement's product as concentrated rent) while fixing is prohibitive (removing feud obligations without substitute enforcement exposes every seat to unchecked predation). Diffuse-plus-prohibitive patterns toward inertial residue, but the low theater ratio and live founding problem distinguish a working coordination order from theatrical maintenance; should state courts arrive and the feud persist as honor performance, theater_ratio would climb and decay toward vestige would become measurable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint instantiates one reading of the feud_obligation_kernel; do the stateless-coordination, extraction-cycle, and christianized-pacification readings describe one structure or three?',
    'Cross-case outcome comparison weighted by settlement rates, deterrence effects, and casualty trajectories across feud societies (Nuer, Icelandic commonwealth, Montenegrin and Albanian highlands).',
    'If the extraction-cycle reading dominates, epsilon rises sharply and the computed type shifts toward tangled_rope or snare; if the christianized-pacification reading dominates, the arrangement is illegitimate by fiat regardless of functional performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the feud kernel captures the dominant structure.').

omega_variable(
    feud_naturalness_ambiguity,
    'Is the feud obligation a near-universal emergent response to enforcement vacuums, recurring independently wherever centralized capacity is absent, or a culturally constructed institution sustained by honor ideology?',
    'Compare independent-invention cases for convergent structure versus divergent culturally specific elaboration; test whether feud form tracks enforcement-vacuum conditions or particular cultural transmission chains.',
    'If emergent, part of the measured cost is the irreducible price of order without a state; if constructed, the honor-economy overhead is discretionary and its costs weigh more heavily against the coordination claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feud_naturalness_ambiguity, empirical, 'Natural regularity versus cultural construction of feud obligation.').

omega_variable(
    defector_sanction_proportionality,
    'Are defector sanctions (honor loss, kinship expulsion) calibrated to what deterrence requires, or do they exceed what the coordination function needs?',
    'Compare deterrence quality and defection rates across feud societies with mild versus severe defection sanctions; locate the severity threshold beyond which added sanction buys no additional compliance.',
    'Sanctions exceeding deterrence needs indicate an extraction component riding the coordination function, shifting the computed type toward tangled_rope at the defector seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(defector_sanction_proportionality, empirical, 'Whether honor sanctions serve coordination or extraction.').

omega_variable(
    wergild_substitutability,
    'Can compensation fully substitute for blood vengeance, or is the compensation track parasitic on the credible threat of vengeance?',
    'Examine cases where wergild operated detached from feud capacity (tariff schedules imposed from above) versus embedded in living feud systems; compare settlement durability in each.',
    'Full substitutability would make the violent core contingent scaffolding removable by institutional design; parasitism would bind the coordination function to the violence permanently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wergild_substitutability, empirical, 'Whether peaceful compensation is separable from vengeance capacity.').

omega_variable(
    honor_suppression_internalization,
    'Is the residual pressure against wergild-only settlement structural (shame sanctions administered by the group, loss of protection) or internalized (honor identity that makes compensation feel intolerable)?',
    'Post-exit trajectories: individuals who leave kin structures and settle disputes peacefully retain functioning preferences where internalization was shallow; persistent distress and renewed vengeance-seeking indicate fused identity.',
    'Internalized pressure travels with the agent after exit, raising effective suppression above the structural measure and complicating any reform that merely removes group sanctions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(honor_suppression_internalization, empirical, 'Structural versus internalized honor suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__stateless_coordination_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_coord_reading_tr_t0, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(feud_coord_reading_tr_t6, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 6, 0.13).
narrative_ontology:measurement(feud_coord_reading_tr_t12, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 12, 0.15).
narrative_ontology:measurement(feud_coord_reading_tr_t18, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 18, 0.17).
narrative_ontology:measurement(feud_coord_reading_tr_t24, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 24, 0.19).
narrative_ontology:measurement(feud_coord_reading_tr_t30, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 30, 0.21).

% Extraction over time
narrative_ontology:measurement(feud_coord_reading_be_t0, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 0, 0.24).
narrative_ontology:measurement(feud_coord_reading_be_t6, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 6, 0.27).
narrative_ontology:measurement(feud_coord_reading_be_t12, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 12, 0.3).
narrative_ontology:measurement(feud_coord_reading_be_t18, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 18, 0.33).
narrative_ontology:measurement(feud_coord_reading_be_t24, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 24, 0.36).
narrative_ontology:measurement(feud_coord_reading_be_t30, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 30, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(feud_coord_reading_su_t0, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(feud_coord_reading_su_t6, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 6, 0.21).
narrative_ontology:measurement(feud_coord_reading_su_t12, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 12, 0.25).
narrative_ontology:measurement(feud_coord_reading_su_t18, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 18, 0.29).
narrative_ontology:measurement(feud_coord_reading_su_t24, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 24, 0.33).
narrative_ontology:measurement(feud_coord_reading_su_t30, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__stateless_coordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, extraction_cycle_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, christianized_pacification_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'blood feud' decomposes into at least three structurally distinct constraints — readings of feud_obligation_kernel: a self-enforcing coordination mechanism (this file), a destructive extraction cycle (extraction_cycle_reading), and a divinely-usurped violence regime (christianized_pacification_reading). Epsilon differs sharply across the readings over the same historical material, so each is authored separately and linked here. The upstream descriptive regularity (feud institutions track enforcement vacuums and recede as state courts arrive) lends evidential weight to whichever downstream reading a corpus weights; the coordination reading supplies the functional baseline against which the extraction reading measures waste and the pacification reading measures usurpation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
