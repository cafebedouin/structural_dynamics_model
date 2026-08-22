% ============================================================================
% CONSTRAINT STORY: reformation_composite__political_realignment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__political_realignment_reading, []).

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
 *   constraint_id: reformation_composite__political_realignment_reading
 *   human_readable: Cuius Regio Eius Religio as Sovereignty Instrument
 *   domain: historical_epistemology/political_economy
 *
 * SUMMARY:
 *   This story instantiates the political-realignment reading of the
 *   Reformation kernel: territorial princes within and adjacent to the Holy
 *   Roman Empire used religious differentiation instrumentally to break
 *   imperial and papal fiscal and jurisdictional authority, consolidating
 *   sovereign control over taxation, appointment, and legal jurisdiction
 *   under the banner of confessional choice. The primary observable is the
 *   cuius regio eius religio principle codified at the Peace of Augsburg
 *   (1555) and hardened through the Peace of Westphalia (1648): territorial
 *   rulers set the religion of their lands, and the resolution of religious
 *   war becomes simultaneously the resolution of a sovereignty contest. This
 *   is a distinct constraint from the theological-fragmentation reading
 *   (which locates the driving mechanism in incompatible soteriological
 *   commitments among believers and clergy) and the technological-mediation
 *   reading (which locates it in the printing press's capacity to scale local
 *   dissent). All three readings describe overlapping historical events but
 *   author different ε, different beneficiary/victim structures, and
 *   different primary observables — they are linked via
 *   network.affects_constraints, not merged into one story.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__political_realignment_reading, 0.68).
domain_priors:suppression_score(reformation_composite__political_realignment_reading, 0.72).
domain_priors:theater_ratio(reformation_composite__political_realignment_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__political_realignment_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__political_realignment_reading, "Cuius Regio Eius Religio as Sovereignty Instrument").
narrative_ontology:topic_domain(reformation_composite__political_realignment_reading, "historical_epistemology/political_economy").

domain_priors:requires_active_enforcement(reformation_composite__political_realignment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__political_realignment_reading, 'eb45accd-80c5-4ae8-a89b-078a916432af').
narrative_ontology:cs_kernel_codification('eb45accd-80c5-4ae8-a89b-078a916432af', distributed).
narrative_ontology:cs_authority_grounding('eb45accd-80c5-4ae8-a89b-078a916432af', extraction).
narrative_ontology:cs_interpretation_layer_present('eb45accd-80c5-4ae8-a89b-078a916432af').
narrative_ontology:cs_reading_relation('eb45accd-80c5-4ae8-a89b-078a916432af', reformation_composite__theological_fragmentation_reading, coexists_with).
narrative_ontology:cs_reading_relation('eb45accd-80c5-4ae8-a89b-078a916432af', reformation_composite__technological_mediation_reading, influences).
narrative_ontology:cs_axiom('eb45accd-80c5-4ae8-a89b-078a916432af', foundational, religious_identity_is_instrumentalizable_for_sovereignty).
narrative_ontology:cs_axiom_status(religious_identity_is_instrumentalizable_for_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('eb45accd-80c5-4ae8-a89b-078a916432af', religious_identity_is_instrumentalizable_for_sovereignty, empirically_contingent).
narrative_ontology:cs_axiom('eb45accd-80c5-4ae8-a89b-078a916432af', foundational, territorial_ruler_authority_supersedes_universal_ecclesiastical_jurisdiction).
narrative_ontology:cs_axiom_status(territorial_ruler_authority_supersedes_universal_ecclesiastical_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('eb45accd-80c5-4ae8-a89b-078a916432af', territorial_ruler_authority_supersedes_universal_ecclesiastical_jurisdiction, conventional).
narrative_ontology:cs_reference_frame('eb45accd-80c5-4ae8-a89b-078a916432af', universal_christendom_dual_authority).
narrative_ontology:cs_drift_state('eb45accd-80c5-4ae8-a89b-078a916432af', peace_of_westphalia_1648, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('eb45accd-80c5-4ae8-a89b-078a916432af', '').
narrative_ontology:cs_kernel_id(reformation_composite__political_realignment_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, territorial_princes).
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, emerging_nation_state_apparatus).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, holy_roman_emperor).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, papal_curia).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, religious_minorities_within_territories).
narrative_ontology:constraint_vindicates(reformation_composite__political_realignment_reading, territorial_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(reformation_composite__political_realignment_reading, cuius_regio_eius_religio_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% German princes and other territorial rulers adopt Lutheran or Reformed confessions to seize church lands, break imperial taxation and appointment authority, and consolidate direct rule over their subjects. They set the religious identity of their territory by decree and enforce it against dissenters, converting a theological dispute into a lever for consolidating sovereign power previously mediated through emperor and pope.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, territorial_princes, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(reformation_composite__political_realignment_reading, territorial_princes, beneficiary).

% Loses the capacity to compel religious uniformity across the Empire and, with it, a primary lever of centralized authority. Wars to suppress the princes' religious defections (Schmalkaldic War) fail to restore control; the Peace of Augsburg formalizes the loss by making territorial religious choice a legal principle rather than a violation to be punished.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, holy_roman_emperor, payer,
    institutional, civilizational, constrained, continental).

% Loses direct fiscal extraction (indulgences, benefices, annates) and appointment authority across entire territories that convert. Excommunication and doctrinal condemnation no longer function as effective enforcement once a secular ruler backs the dissenting church with military and legal power. Its authority claim survives rhetorically but its material reach across converted territories collapses.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, papal_curia, payer,
    institutional, civilizational, constrained, continental).

% Subjects whose confession does not match their prince's chosen religion face exile, conversion under duress, or persecution under the cuius regio eius religio settlement. The principle that resolved conflict between rulers offers them no protection; their only nominal remedy is the right to emigrate, which for the poor and rooted is not a real option.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, religious_minorities_within_territories, payer,
    powerless, biographical, trapped, local).

% The bureaucratic and legal machinery of the consolidating territorial state — tax collectors, court systems, state churches — expands to fill the administrative space vacated by imperial and papal authority. State-controlled religion becomes an instrument of population administration, marriage law, education, and loyalty enforcement that outlives the immediate theological dispute.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, emerging_nation_state_apparatus, beneficiary,
    institutional, civilizational, arbitrage, national).

% Read treaty language, territorial administrative records, and the timing of conversions relative to fiscal and military disputes with imperial authority to assess whether religious change tracks sovereignty assertion independent of genuine doctrinal conviction among the ruling class.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, diplomatic_historians, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_composite__political_realignment_reading, territorial_princes).
narrative_ontology:fixing_cost_class(reformation_composite__political_realignment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Cuius regio eius religio solves a real inter-state coordination problem: it ends decades of religious war between territories by giving each ruler exclusive authority to set confession within his own borders, removing religion as a casus belli between princes.
% TRANSFER_FUNCTION: Moves fiscal authority (church lands, tithes, annates), appointment power (bishoprics, benefices), and legal jurisdiction over marriage, education, and morals from the papal-imperial axis to territorial rulers and their administrative apparatus — at the cost of imposing the ruler's confession on all subjects regardless of their own belief.
% ABSENT_VOICES: Religious minorities within each territory had no seat at Augsburg or in the princely negotiations that produced the settlement; peasants and townspeople whose confession did not match their ruler's were governed by a peace made entirely among rulers, about rulers' prerogatives.
% DISAPPEARANCE_RATIONALE: If territorial religious sovereignty had not been secured, the fiscal and jurisdictional consolidation of early modern territorial states would have proceeded on a very different timeline and basis — imperial and papal claims on taxation, appointment, and legal jurisdiction would have persisted as live constraints on state formation rather than being resolved in the princes' favor.
% FOUNDING_PROBLEM: Territorial rulers within the Holy Roman Empire sought to break dependence on imperial taxation, military levies, and appointment authority, and on papal fiscal extraction and jurisdictional claims, in order to consolidate direct sovereign control over their populations and revenues.
% FOUNDING_PROBLEM_CORROBORATION: Fiscal and administrative historians studying territorial state formation (outside both the princely and papal traditions) document the systematic timing correlation between conversions and disputes over taxation, military levies, and appointment rights, corroborating the political-instrumental reading independent of confessional historiography from either Protestant or Catholic institutional descendants.
narrative_ontology:disappearance_verdict(reformation_composite__political_realignment_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__political_realignment_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__political_realignment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reformation_composite__political_realignment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__political_realignment_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__political_realignment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_composite__political_realignment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_composite__political_realignment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 at Luther's initial dissent to 0.68 by Westphalia as the political instrumentalization of religious identity matures from opportunistic land-grabs into an entrenched administrative apparatus of state churches, marriage courts, and loyalty enforcement. Suppression climbs even faster (0.30 to 0.72) because maintaining territorial religious uniformity against internal minorities requires escalating coercive machinery — expulsion, forced conversion, confiscation — that has little to do with theological conviction and everything to do with consolidating administrative control. Theater ratio grows moderately (0.15 to 0.40) as some genuine coordination function (ending inter-princely religious war) persists alongside a growing performative layer of confessional identity politics that serves state-building rather than belief.
 *
 * PERSPECTIVAL GAP:
 *   From the princely seat, the settlement reads as legitimate sovereignty assertion resolving decades of religious war — genuine coordination. From the imperial and papal seat, the identical structure reads as extraction of previously centralized authority under cover of doctrinal dispute. From the religious-minority seat, it reads as pure imposition with no coordination benefit at all, since their preferences were never solicited. The engine computes these divergent per-seat classifications from the same structural data; the political-realignment reading claims tangled_rope precisely because both a real coordination function (ending inter-state religious war) and asymmetric extraction (imperial/papal authority stripped, minorities persecuted) coexist in the same mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Territorial princes and the emerging state apparatus are structural beneficiaries: they acquire the fiscal, legal, and appointment authority formerly held by emperor and pope, and their exit options are arbitrage-grade (they can and did selectively invoke confession as leverage). The emperor and papal curia are structural victims: their authority claims persist rhetorically but their material extractive and jurisdictional reach collapses across converted territories — high directionality toward extraction from their seat. Religious minorities within territories are doubly victimized: trapped by exit options that are nominal (emigration) rather than real, and excluded entirely from the negotiations (Augsburg, Westphalia) that fixed their fate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — princely dependence on imperial and papal fiscal/jurisdictional authority — is dead by the terms of this reading: territorial sovereignty was achieved and consolidated well before 1648. Yet the state-church apparatus and confessional loyalty enforcement persisted long after the sovereignty question was settled, continuing to extract compliance from religious minorities under a coordination rationale (peace between territories) that no longer required minority suppression to sustain. This is the classic tangled_rope signature: real coordination function at founding, extraction that outlives the coordination need.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    instrumentality_vs_conviction,
    'Did territorial princes adopt Reformation theology primarily as an instrument for sovereignty assertion, or did genuine doctrinal conviction precede and independently motivate the political consolidation that followed?',
    'Comparative analysis of conversion timing against documented fiscal and jurisdictional disputes with imperial/papal authority; correlation between princes who converted and princes with the strongest pre-existing grievances against imperial taxation or papal appointment authority would support instrumentality; princes converting despite net political cost would weaken it.',
    'If conviction dominates, this reading''s beneficiary/victim structure and its tangled_rope classification collapse toward the theological_fragmentation_reading''s structure; if instrumentality dominates, the political-realignment reading''s high extraction and enforcement requirement are well-grounded as this story authors them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instrumentality_vs_conviction, empirical, 'Whether religious conversion by rulers was primarily instrumental or primarily sincere.').

omega_variable(
    counterfactual_state_formation_pace,
    'Would territorial sovereignty consolidation (tax authority, legal jurisdiction, appointment power) have proceeded at a similar pace and to a similar degree absent the religious vehicle, through purely secular/dynastic mechanisms already visible in the late medieval period?',
    'Comparative study of state formation trajectories in polities that did not undergo Reformation-driven religious differentiation (e.g., Catholic territorial consolidation in France, Spain) against Protestant territories, controlling for other variables.',
    'If secular state formation proceeded at comparable rates without the religious vehicle, the disappearance_verdict weakens toward world_unchanged for the specifically religious mechanism, since sovereignty consolidation would have found another route; if religious differentiation was load-bearing, disappearance_verdict of world_rearranges is well-supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_state_formation_pace, conceptual, 'Whether religious differentiation was a necessary or merely sufficient vehicle for the sovereignty consolidation this reading describes.').

omega_variable(
    framing_choice_kernel_versus_institution,
    'Is the correct unit of analysis the Reformation-as-kernel (a single contested historical claim read three ways) or should the political-realignment mechanism itself be decomposed further into distinct constraints for the Empire''s internal princely politics versus emerging nation-states outside the Empire (England, Scandinavia, the Netherlands) which had structurally different relationships to papal versus imperial authority?',
    'Compare whether ε and beneficiary/victim structure remain stable across the Empire-internal cases and the extra-imperial national cases; if they diverge substantially, a further decomposition is warranted under the ε-invariance principle.',
    'If the extra-imperial cases (e.g., Henrician England, which had no imperial authority to escape, only papal) show a substantially different ε or victim structure, this story should be split further; as authored here it treats the Empire''s princely politics as the paradigm case and the primary observable (cuius regio eius religio) is specifically an imperial-constitutional mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_choice_kernel_versus_institution, conceptual, 'Whether this single reading adequately covers all national contexts or requires further sub-decomposition by imperial versus non-imperial polities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__political_realignment_reading, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_composite__political_realignment_reading, theater_ratio, 1517, 0.15).
narrative_ontology:measurement(refo_tr_t1530, reformation_composite__political_realignment_reading, theater_ratio, 1530, 0.22).
narrative_ontology:measurement(refo_tr_t1546, reformation_composite__political_realignment_reading, theater_ratio, 1546, 0.3).
narrative_ontology:measurement(refo_tr_t1555, reformation_composite__political_realignment_reading, theater_ratio, 1555, 0.35).
narrative_ontology:measurement(refo_tr_t1600, reformation_composite__political_realignment_reading, theater_ratio, 1600, 0.38).
narrative_ontology:measurement(refo_tr_t1648, reformation_composite__political_realignment_reading, theater_ratio, 1648, 0.4).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_composite__political_realignment_reading, base_extractiveness, 1517, 0.35).
narrative_ontology:measurement(refo_be_t1530, reformation_composite__political_realignment_reading, base_extractiveness, 1530, 0.48).
narrative_ontology:measurement(refo_be_t1546, reformation_composite__political_realignment_reading, base_extractiveness, 1546, 0.58).
narrative_ontology:measurement(refo_be_t1555, reformation_composite__political_realignment_reading, base_extractiveness, 1555, 0.62).
narrative_ontology:measurement(refo_be_t1600, reformation_composite__political_realignment_reading, base_extractiveness, 1600, 0.65).
narrative_ontology:measurement(refo_be_t1648, reformation_composite__political_realignment_reading, base_extractiveness, 1648, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_composite__political_realignment_reading, suppression_requirement, 1517, 0.3).
narrative_ontology:measurement(refo_su_t1530, reformation_composite__political_realignment_reading, suppression_requirement, 1530, 0.45).
narrative_ontology:measurement(refo_su_t1546, reformation_composite__political_realignment_reading, suppression_requirement, 1546, 0.6).
narrative_ontology:measurement(refo_su_t1555, reformation_composite__political_realignment_reading, suppression_requirement, 1555, 0.65).
narrative_ontology:measurement(refo_su_t1600, reformation_composite__political_realignment_reading, suppression_requirement, 1600, 0.68).
narrative_ontology:measurement(refo_su_t1648, reformation_composite__political_realignment_reading, suppression_requirement, 1648, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__political_realignment_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(reformation_composite__political_realignment_reading, 0.12).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, reformation_composite__theological_fragmentation_reading).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, reformation_composite__technological_mediation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the reformation_composite kernel. theological_fragmentation_reading locates the driving mechanism in incompatible soteriological/ecclesiological commitments (a Rope or Tangled Rope depending on denominational enforcement structure); technological_mediation_reading locates it in printing-press-enabled scaling of dissent (likely a Rope — information coordination with some suppression around censorship). This reading (political_realignment_reading) authors the highest ε of the three because it identifies concrete institutional losers (emperor, papacy) and concrete institutional winners (territorial states) with an enforced settlement (Augsburg/Westphalia) transferring fiscal and jurisdictional authority. The three readings are not competing explanations to be averaged; each is a structurally distinct constraint with its own stable ε, per the ε-invariance principle, and each should be independently evaluated by the engine.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reformation_composite__political_realignment_reading, institutional, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
