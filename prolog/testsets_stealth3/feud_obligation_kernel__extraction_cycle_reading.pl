% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__extraction_cycle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__extraction_cycle_reading, []).

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
 *   constraint_id: feud_obligation_kernel__extraction_cycle_reading
 *   human_readable: Blood-Feud Obligation as Destructive Extraction Cycle
 *   domain: legal_anthropology/medieval_history/comparative_political_systems
 *
 * SUMMARY:
 *   In post-Roman and medieval Europe, the blood-feud obligation bound
 *   kindreds to avenge slain members or exact priced composition, making
 *   retaliation the operative justice system wherever central enforcement was
 *   absent or weak. This file instantiates the extraction_cycle_reading of
 *   the feud_obligation_kernel: the arrangement is modeled as a destructive
 *   extraction cycle consuming lives, labor, and wealth, whose persistence
 *   subsidizes royal authority (fines, legitimacy, taxable disorder),
 *   ecclesiastical institutions (compositions, sanctuary dues, moral
 *   capital), and broker magnates (settlement shares, followings). Per the
 *   epsilon-invariance decomposition, the sibling readings —
 *   stateless_coordination_reading and christianized_pacification_reading —
 *   are separate constraint files with their own epsilon, victim sets, and
 *   classifications, not positions argued inside this one. KEY AGENTS (by
 *   structural relationship): - feud_bound_kin_groups: Primary target
 *   (organized/identity_locked) — bear the vengeance duty corporately across
 *   generations - feud_obligated_male_kin: Primary target
 *   (moderate/identity_locked) — bear mortality and lost productive years -
 *   peasant_dependent_households: Collateral target (powerless/constrained) —
 *   provisioning and reprisal losses with no standing - royal_authority:
 *   Primary beneficiary (institutional/arbitrage) — converts feud disorder
 *   into fines and pacification legitimacy - ecclesiastical_institutions:
 *   Beneficiary (institutional/arbitrage) — compositions, sanctuary dues,
 *   moral capital - feud_broker_magnates: Beneficiary (powerful/mobile) —
 *   brokerage shares and enlarged followings - peace_inclined_kin_members:
 *   Excluded voice (moderate/identity_locked) — pro-settlement preference
 *   with no legitimate speech - comparative_anthropology_observers:
 *   Analytical observer — sees the full structure across societies
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, 0.82).
domain_priors:suppression_score(feud_obligation_kernel__extraction_cycle_reading, 0.78).
domain_priors:theater_ratio(feud_obligation_kernel__extraction_cycle_reading, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__extraction_cycle_reading, snare).
narrative_ontology:human_readable(feud_obligation_kernel__extraction_cycle_reading, "Blood-Feud Obligation as Destructive Extraction Cycle").
narrative_ontology:topic_domain(feud_obligation_kernel__extraction_cycle_reading, "legal_anthropology/medieval_history/comparative_political_systems").

domain_priors:requires_active_enforcement(feud_obligation_kernel__extraction_cycle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__extraction_cycle_reading, '4d6e68a9-783a-482e-8b25-abc480486331').
narrative_ontology:cs_kernel_codification('4d6e68a9-783a-482e-8b25-abc480486331', implicit).
narrative_ontology:cs_authority_grounding('4d6e68a9-783a-482e-8b25-abc480486331', practice).
narrative_ontology:cs_interpretation_layer_present('4d6e68a9-783a-482e-8b25-abc480486331').
narrative_ontology:cs_reading_relation('4d6e68a9-783a-482e-8b25-abc480486331', feud_obligation_kernel__stateless_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('4d6e68a9-783a-482e-8b25-abc480486331', feud_obligation_kernel__christianized_pacification_reading, influences).
narrative_ontology:cs_axiom('4d6e68a9-783a-482e-8b25-abc480486331', foundational, feud_net_destructive_to_productive_capacity).
narrative_ontology:cs_axiom_status(feud_net_destructive_to_productive_capacity, holdable).
narrative_ontology:cs_axiom_grounding('4d6e68a9-783a-482e-8b25-abc480486331', feud_net_destructive_to_productive_capacity, empirically_contingent).
narrative_ontology:cs_axiom('4d6e68a9-783a-482e-8b25-abc480486331', foundational, territorial_consolidation_requires_violence_monopoly).
narrative_ontology:cs_axiom_status(territorial_consolidation_requires_violence_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('4d6e68a9-783a-482e-8b25-abc480486331', territorial_consolidation_requires_violence_monopoly, instrumental).
narrative_ontology:cs_reference_frame('4d6e68a9-783a-482e-8b25-abc480486331', vengeance_extraction_economy).
narrative_ontology:cs_drift_state('4d6e68a9-783a-482e-8b25-abc480486331', post_revisionist_anthropology, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4d6e68a9-783a-482e-8b25-abc480486331', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, royal_authority).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, ecclesiastical_institutions).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, feud_broker_magnates).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, feud_bound_kin_groups).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, feud_obligated_male_kin).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, peasant_dependent_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collects fines, forfeitures, and composition dues that flow through royal courts whenever feuds are prosecuted or settled, and converts the disorder feuds produce into legitimacy: each outbreak makes the promise of imposed peace more valuable and the taxation that funds it easier to justify. Simultaneously builds the court and sheriff machinery that channels vengeance into payable claims. Leaving the game is not a live option here — the crown writes rules rather than playing inside them.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, royal_authority, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__extraction_cycle_reading, royal_authority, agenda_setter).

% Receives composition payments, penance dues, and endowments attached to feud settlements; sells sanctuary and safe-conduct to feud fugitives; accumulates moral authority by preaching a peace it charges to administer. Monasteries holding relics of murdered men sometimes cultivated cults whose offerings scaled with the fame of the vendetta that produced them.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, ecclesiastical_institutions, beneficiary,
    institutional, generational, arbitrage, continental).

% Local lords and chieftains who lead feud parties, host negotiations, and take a share of every settlement as brokerage. Each cycle enlarges their following, since victorious kinsmen owe service and defeated ones seek protection. They can relocate, marry across feud lines, or convert brokerage into judicial office; nothing binds them to any single vendetta.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, feud_broker_magnates, beneficiary,
    powerful, biographical, mobile, regional).

% Kindreds bound corporately by the vengeance duty: every killing of a member creates an obligation the whole group must execute or compound. They marshal armed retinues, pay wergeld, burn and see their harvests burned in return, and carry grudges across generations. Refusing vengeance dissolves the group's standing — allies defect, daughters go unmatched, rivals probe. Membership is inherited; there is no departure that leaves the lineage intact.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, feud_bound_kin_groups, payer,
    organized, generational, identity_locked, regional).

% The individual men who do the fighting, watching, and dying. They inherit enemies they never chose and spend their productive years armed, sleepless, and unavailable for farm or trade. Flight abandons kin to the enemy; staying risks the death the obligation exists to avenge. Honor is not a garment worn before the group but the substance of a man's name.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, feud_obligated_male_kin, payer,
    moderate, biographical, identity_locked, regional).

% Tenants and laborers on feud lands who are neither principals nor avengers. Raiding parties requisition their stores, burn their byres as reprisal, and press them as porters; their lords' absences leave fields untended. They cannot leave the land without losing tenancy, and they hold no standing in any settlement negotiation.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, peasant_dependent_households, payer,
    powerless, biographical, constrained, local).

% Kinsmen who privately favor taking or offering composition and ending the cycle. They have no legitimate voice: proposing settlement before satisfaction is read as cowardice, shames the lineage publicly, and can itself draw accusations of betrayal. Their preference surfaces only through brokers, at prices set by the very obligation they wish to escape.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, peace_inclined_kin_members, excluded,
    moderate, biographical, identity_locked, regional).

% Historians and legal anthropologists comparing feud systems across societies and periods — saga Iceland, the Kanun highlands, the Corsican vendetta, Carolingian and post-Carolingian Francia. They reconstruct the flows, test competing accounts against demographic and fiscal evidence, and hold no position inside any vendetta.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, comparative_anthropology_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feud_obligation_kernel__extraction_cycle_reading, royal_authority).
narrative_ontology:fixing_cost_class(feud_obligation_kernel__extraction_cycle_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In the absence of centralized enforcement, the feud obligation makes retaliation credible: every killing carries a predictable price, which deters casual violence and gives kin groups a self-help justice procedure where no other exists.
% TRANSFER_FUNCTION: Moves wealth (wergeld and composition payments, court fines, mediation fees), labor (armed retinues, watch-keeping, siege provisioning), and lives (combat deaths, hostage executions) from feud-bound kin groups and their dependents toward royal treasuries, ecclesiastical houses, broker magnates, and opposing kindreds.
% ABSENT_VOICES: Peace-inclined kinsmen have no legitimate voice — advocating settlement reads as cowardice and shames the lineage. Dependent peasants who provision raiding parties and shelter neither party cannot object anywhere. The dead, whose claims originate every cycle, are spoken for exclusively by the obligation that consumed them.
% DISAPPEARANCE_RATIONALE: If feud obligations vanished overnight, kin groups would lose their justice procedure and deterrent posture simultaneously; royal courts would lose fine revenue and the legitimacy dividend of promising peace; magnate followings built on feud brokerage would dissolve; and some substitute — royal courts, church tribunals, or private composition markets — would have to be assembled quickly to handle homicide, violently, across a century of transitions compressed into weeks.
% FOUNDING_PROBLEM: Homicide between kindreds in societies with no state enforcement: without a credible retaliation duty, killings would be costless and kin groups defenseless.
% FOUNDING_PROBLEM_CORROBORATION: No single attester spans the interval. From outside the beneficiary set: kin testimony preserved in saga and court record treats the security problem as real wherever royal courts were absent; frontier chronicles (Highland, Balkan, Caucasus) attest pockets where no substitute enforcement ever arrived; modern comparative anthropology corroborates the correlation — feud persists where state enforcement is absent and decays where it arrives. Royal justices and church peace statutes also attest the problem shrinking, but they sit inside the beneficiary set, which is precisely why the status is contested rather than dead.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__extraction_cycle_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__extraction_cycle_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__extraction_cycle_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(feud_obligation_kernel__extraction_cycle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__extraction_cycle_reading, 0.82, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Scores are authored from this reading's seat and are not tuned to any predicted verdict. Extractiveness 0.82: the cycle consumes combat deaths, hostage executions, burned harvests, wergeld transfers, and the armed unavailability of prime male labor across decades; the referent is the standing feud-obligation arrangement as this reading assesses it. Suppression 0.78 is authored raw and unscaled — it measures the coercive machinery keeping participants bound: kin sanction, ally defection, marriage-market exclusion, public shaming of peace-seekers; the engine owns any scaling. Theater 0.24: formal challenge declarations, oath ceremonies, and truce rituals perform real signaling work, though by the late interval heraldic defiance exchanges grow ornamental. Accessibility_collapse 0.62: alternatives (composition, arbitration, sanctuary, flight, conversion) exist and are used, but understanding the feud logic shows each alternative discounts the lineage's standing, so they collapse partially rather than fully. Resistance 0.58: the Peace and Truce of God, royal edicts, and mass composition-seeking met the system and were largely absorbed into it as channeled settlement. Assumptions stated openly: the interval models the western European feud arc from early post-Roman kin-law (600) to the late-medieval consolidation era (1400); measurement values are qualitative reconstructions from saga, charter, court-roll, and peace-statute evidence, not instrument readings. The three series share one seven-point grid. Suppression_requirement is authored because the story tracks enforcement-capacity change: as external pacification pressure rose, the kinship enforcement apparatus hardened internally, policing its own peace-seekers more intensely, so the series rises alongside base_extractiveness rather than staying flat. Coalition check: the classic remedy for powerless agents — coalition — is already exhausted here, since kin groups ARE coalitions, and the obligation structure blocks cross-kindred victim alliances (a pact with your attacker's enemies reads as betrayal and mints new feud lines). No cyclical dynamics are modeled; the interval shows monotone intensification, not oscillation.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the royal chair the arrangement presents as a revenue and legitimacy machine — close to a subsidized good the crown did not build; from the kin-group chairs the identical structure presents as a debt spiral where each settlement seeds the next killing; from the magnate chair it is a career ladder; from the peace-inclined member's position it is a silence enforced by shame. Identity-lock mechanics: the binding is relational (selfhood constituted through lineage membership) compounded by ideological fusion (an honor worldview in which exit is unthinkable rather than merely costly); if the honor frame broke — as court-centered status cultures eventually broke it in consolidated realms — exit opens and the computed severity at these seats falls. The structural-versus-internalized split of suppression is carried by omega suppression_structural_vs_internalized rather than resolved here.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. royal_authority and ecclesiastical_institutions sit at the beneficiary end (low d): the arrangement subsidizes them with fines, compositions, sanctuary dues, and legitimacy, and their arbitrage-grade exit means they bear almost none of its costs. feud_broker_magnates likewise sit low-d: they collect brokerage and followings and can move between vendettas. feud_bound_kin_groups and feud_obligated_male_kin sit near the full-target end (high d): they transfer wealth, labor, and lives, and identity_locked exit amplifies their effective extraction beyond what a mobile payer would bear. peasant_dependent_households are high-d targets with constrained exit — they pay without ever entering the feud as principals. peace_inclined_kin_members are structurally inside the victim set with identity_locked exit; their exclusion from speech does not lower their d. comparative_anthropology_observers take the analytical seat and feed no extraction arithmetic. No directionality overrides are authored: the derivation from declared roles plus exit options reproduces these relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — homicide between kindreds with no central enforcement — is authored contested: live in stateless margins (highlands, frontiers), dead in consolidated cores. Because status is contested rather than dead, the mismatch consumer finds no dead-plus-world_rearranges zombie flag, and mandatrophy_resolved is deliberately left undeclared: resolving it would mislabel the frontier systems where the founding problem still bites. The receipt surface blocks a piton misread: gain_flow names royal_authority (concentrated capture), and per the receipt-cell semantics a captured arrangement stays snare-flavored under either fixing_cost class — authored prohibitive, since replacing the feud required building an entire substitute enforcement apparatus across centuries, a cost no single reign could carry relative to its tenure. The classification thus guards against two opposite mislabels: reading the feud as pure coordination (the stateless sibling's risk) ignores the concentrated capture and suppressed exits; reading it as leaderless inertia ignores that identifiable seats collect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates the extraction_cycle_reading of the feud_obligation_kernel; how would the classification change if the same arrangement were read as the stateless_coordination_reading or the christianized_pacification_reading?',
    'Compile the two sibling stories and compare per-seat classifications, epsilon, and victim/beneficiary sets against this file; the deltas are the measurement.',
    'Under the stateless_coordination_reading, epsilon falls toward coordination-cost levels, feud participants migrate out of the victim set, and the type migrates toward rope; under the christianized_pacification_reading, the victim set extends to souls and divine order and the beneficiary set shifts toward ecclesiastical salvation-rents. Only the sibling files can settle which reading the corpus should weight.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame indexicality: one kernel, three readings, three constraints.').

omega_variable(
    net_destructiveness_dispute,
    'Is the feud system net-depleting of productive capacity and population, as this reading asserts, or net-ordering where state enforcement is absent?',
    'Demographic and economic reconstruction comparing feud-active regions with pacified comparators: settlement abandonment rates, tithe yields, and court-record mortality attributable to vengeance.',
    'If the system is net-ordering, this reading''s epsilon collapses toward the enforcement floor and the classification migrates toward rope or tangled_rope; the extraction_cycle_reading loses its foundational axiom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(net_destructiveness_dispute, empirical, 'Empirical contest over the reading''s core destructiveness claim.').

omega_variable(
    royal_net_position_ambiguity,
    'Does royal authority stand as net beneficiary of feud persistence (fines, legitimacy, taxable disorder) or net spender on its suppression, and does the balance flip across the interval?',
    'Fiscal reconstruction: feud-attributable court revenue and composition dues versus pacification expenditure (expeditions, garrisons, purchased settlements) by reign.',
    'If the crown is a net spender, its directionality rises toward target and the beneficiary declaration narrows to magnates and church; the snare reading survives but the capture seat moves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(royal_net_position_ambiguity, empirical, 'Whether the crown collects from the feud or pays to end it.').

omega_variable(
    suppression_structural_vs_internalized,
    'How much of the measured suppression is structural (kin sanction, ally defection, marriage-market exclusion) versus internalized (honor identity that persists when sanctions lapse)?',
    'Post-sanction-collapse trajectories: feud behavior in populations where kin enforcement capacity broke (plague depopulation, mass out-migration, forced resettlement) — if vengeance duty persisted without enforcement, the internalized share is large.',
    'Internalized suppression travels with the agent after exit, raising effective suppression above the structural measure and hardening the identity_locked exit classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Split of feud suppression between external sanction and fused honor identity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__extraction_cycle_reading, 600, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t600, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 600, 0.12).
narrative_ontology:measurement(feud_tr_t750, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 750, 0.15).
narrative_ontology:measurement(feud_tr_t900, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 900, 0.18).
narrative_ontology:measurement(feud_tr_t1050, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 1050, 0.2).
narrative_ontology:measurement(feud_tr_t1200, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 1200, 0.22).
narrative_ontology:measurement(feud_tr_t1300, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 1300, 0.23).
narrative_ontology:measurement(feud_tr_t1400, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 1400, 0.24).

% Extraction over time
narrative_ontology:measurement(feud_be_t600, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 600, 0.55).
narrative_ontology:measurement(feud_be_t750, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 750, 0.63).
narrative_ontology:measurement(feud_be_t900, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 900, 0.7).
narrative_ontology:measurement(feud_be_t1050, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 1050, 0.76).
narrative_ontology:measurement(feud_be_t1200, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 1200, 0.79).
narrative_ontology:measurement(feud_be_t1300, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 1300, 0.81).
narrative_ontology:measurement(feud_be_t1400, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 1400, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t600, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 600, 0.55).
narrative_ontology:measurement(feud_su_t750, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 750, 0.6).
narrative_ontology:measurement(feud_su_t900, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 900, 0.66).
narrative_ontology:measurement(feud_su_t1050, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 1050, 0.71).
narrative_ontology:measurement(feud_su_t1200, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 1200, 0.74).
narrative_ontology:measurement(feud_su_t1300, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 1300, 0.77).
narrative_ontology:measurement(feud_su_t1400, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 1400, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__extraction_cycle_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel__stateless_coordination_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel__christianized_pacification_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'blood-feud obligations' decomposes, per the epsilon-invariance principle, into at least three structurally distinct claims: a self-enforcing coordination mechanism (stateless_coordination_reading), a divine-law violation to be pacified (christianized_pacification_reading), and a destructive extraction cycle (this file). Each carries its own epsilon, victim set, and classification. The stateless account sits upstream in the family graph because both other readings cite it as the arrangement whose effects they evaluate. Every family member links at least one other; this file links both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
