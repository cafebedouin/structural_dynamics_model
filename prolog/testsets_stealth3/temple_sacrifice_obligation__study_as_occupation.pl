% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__study_as_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__study_as_occupation, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: temple_sacrifice_obligation__study_as_occupation
 *   human_readable: Study-as-Occupation Settlement of the Sacrificial Obligation
 *   domain: religious/halakhic_authority/commitment_systems
 *
 * SUMMARY:
 *   After the destruction of the Second Temple made the sacrificial
 *   commandments legally binding but physically unperformable, the rabbinic
 *   tradition settled on the ruling that sustained engagement with the laws
 *   of sacrifice lawfully occupies those obligations for as long as the
 *   Temple lies in ruins: the academies teach the sacrificial orders, the
 *   decisors certify that such study discharges the duty, and the community
 *   organizes its calendar around the practice. This file instantiates ONE
 *   reading of the contested kernel 'the sacrificial obligation in the
 *   Temple's absence', the study_as_occupation reading, and authors it as a
 *   clean, epsilon-invariant constraint: under this reading the obligation is
 *   currently discharged through study, so the standing arrangement leaves no
 *   violated party. The claimed type (rope) and the metric scores are
 *   authored independently: the claim states what I believe structurally true
 *   of the settlement; the metrics state what I believe descriptively true of
 *   its operation. Epsilon's referent is the standing study-as-occupation
 *   arrangement itself, assessed by this reading's own lights, never the
 *   arrangement a rival reading would install.
 *
 * KEY AGENTS:
 *   - rabbinic_interpretive_authority: Agenda-setter and principal beneficiary (institutional/identity_locked) — administers, teaches, and certifies the settlement; its self-account is fused with it
 *   - observant_jewish_communities: Beneficiary (organized/constrained) — lives under the settlement and receives a performable path to fulfillment
 *   - torah_scholars: Beneficiary (moderate/identity_locked) — performs the occupation vocationally; vocation and doctrine are fused
 *   - temple_restoration_activists: Excluded voice (organized/constrained) — objects from outside the adjudicating councils that study-as-sufficiency dulls restorative demand
 *   - comparative_religion_observers: Analytical observer (analytical/analytical) — outside check on the tradition's internal accounts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_occupation, 0.18).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_occupation, 0.15).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_occupation, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, extractiveness, 0.18).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_occupation, rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_occupation, "Study-as-Occupation Settlement of the Sacrificial Obligation").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_occupation, "religious/halakhic_authority/commitment_systems").

narrative_ontology:has_sunset_clause(temple_sacrifice_obligation__study_as_occupation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_occupation, '06162ea2-2f57-451d-8dc2-00886d75d650').
narrative_ontology:cs_kernel_codification('06162ea2-2f57-451d-8dc2-00886d75d650', fixed_text).
narrative_ontology:cs_authority_grounding('06162ea2-2f57-451d-8dc2-00886d75d650', lineage).
narrative_ontology:cs_interpretation_layer_present('06162ea2-2f57-451d-8dc2-00886d75d650').
narrative_ontology:cs_reading_relation('06162ea2-2f57-451d-8dc2-00886d75d650', temple_sacrifice_obligation__messianic_suspension, forecloses).
narrative_ontology:cs_reading_relation('06162ea2-2f57-451d-8dc2-00886d75d650', temple_sacrifice_obligation__study_as_archiving, forecloses).
narrative_ontology:cs_axiom('06162ea2-2f57-451d-8dc2-00886d75d650', foundational, torah_study_equals_sacrificial_service).
narrative_ontology:cs_axiom_status(torah_study_equals_sacrificial_service, holdable).
narrative_ontology:cs_axiom_grounding('06162ea2-2f57-451d-8dc2-00886d75d650', torah_study_equals_sacrificial_service, conventional).
narrative_ontology:cs_axiom('06162ea2-2f57-451d-8dc2-00886d75d650', foundational, commandments_remain_binding_without_performance_context).
narrative_ontology:cs_axiom_status(commandments_remain_binding_without_performance_context, holdable).
narrative_ontology:cs_axiom_grounding('06162ea2-2f57-451d-8dc2-00886d75d650', commandments_remain_binding_without_performance_context, deontological).
narrative_ontology:cs_reference_frame('06162ea2-2f57-451d-8dc2-00886d75d650', continuous_occupation_via_study).
narrative_ontology:cs_drift_state('06162ea2-2f57-451d-8dc2-00886d75d650', contemporary_restorationist_pressures, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('06162ea2-2f57-451d-8dc2-00886d75d650', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, observant_jewish_communities).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, rabbinic_interpretive_authority).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, torah_scholars).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, study_equivalence_doctrine).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, standing_character_of_commandments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Codifies and transmits the ruling that sustained engagement with the sacrificial corpus lawfully occupies the sacrificial obligations while the Temple lies in ruins: the academies schedule the tractates, the decisors answer whether and how study discharges the duty, and the interpretive chain certifies each generation's continuity. Administering the arrangement concentrates interpretive standing, students, and support; the tradition's self-account is bound up with having kept the obligation alive without an altar, so abandoning the ruling would unravel the institution's account of its own two millennia of work.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, rabbinic_interpretive_authority, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_obligation__study_as_occupation, rabbinic_interpretive_authority, beneficiary).

% Live under the arrangement: members count the commandments as observed-through-study rather than breached, follow study calendars that include the sacrificial orders, and support the institutions that teach them. Leaving would mean stepping outside observant life altogether, which is socially costly and rarely taken, or privately adopting a rival account of the obligation while continuing communal practice.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, observant_jewish_communities, beneficiary,
    organized, generational, constrained, global).

% Devote careers to mastering the sacrificial orders, a body of law with no practical application in the current state, and receive standing, livelihood, and religious merit for doing so. Under the arrangement this devotion is itself the discharge of the obligation, which fuses vocation and doctrine: the scholar's professional identity and religious standing rise and fall together with the ruling.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, torah_scholars, beneficiary,
    moderate, biographical, identity_locked, global).

% Groups that press for renewed actual service, preparing vessels, candidate priestly lines, and site access, and object that treating study as sufficient occupation domesticates the obligation's demand and dulls the drive to restore the altar. They sit outside the adjudicating councils: their publications circulate and their preparations proceed, but they do not sit in the decisor bodies whose rulings constitute the arrangement.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, temple_restoration_activists, excluded,
    organized, generational, constrained, regional).

% Historians and scholars of religion who track how post-destruction communities convert impossible rites into sustainable practice. They take no part in the arrangement and bear none of its costs; their analyses are the main outside check on the tradition's internal accounts of what its study practice achieves.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, comparative_religion_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_obligation__study_as_occupation, rabbinic_interpretive_authority).
narrative_ontology:fixing_cost_class(temple_sacrifice_obligation__study_as_occupation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective problem created by an unperformable commandment: with the altar gone, the sacrificial obligations remained binding but impossible for every member of the community at once. The arrangement routes the obligation into an act anyone can perform, sustained study of the sacrificial corpus, keeping each member in continuous lawful relation to the commandment system, keeping the legal corpus taught and alive rather than lapsed into dead letter, and giving the community a shared, certifiable account of its own observance.
% TRANSFER_FUNCTION: Moves interpretive labor and communal support toward the academies and scholars, who perform and certify the occupation on the tradition's behalf; moves religious legitimacy back outward, so that each member receives a performable path by which the commandments count as observed. The obligation itself is paid in the currency of study rather than in animals, priests, and altar service; what accumulates at the authority seat is standing, students, and resources concentrated by administering the arrangement.
% ABSENT_VOICES: Holders of rival accounts of the interim obligation, that it waits suspended or that study merely archives knowledge for restoration, would object that this arrangement overclaims what study achieves; they publish and teach in adjacent institutions but sit outside the decisor councils that certify fulfillment. Restorationist activists would object that treating study as sufficient occupation dulls the obligation's demand for actual renewal. Both voices are real and audible at the margins; neither is seated in the adjudicating bodies whose rulings constitute this arrangement.
% DISAPPEARANCE_RATIONALE: Withdraw the ruling overnight and the observant world wakes to binding commandments it cannot perform: the choices become mass technical breach, formal suspension declarations that restructure the commandment system, or an immediate turn to restoration politics, none of which current practice accommodates. Academy curricula lose the spine that organizes the sacrificial orders' place in study, and the community loses its working account of itself as fully observant. The rearrangement would reach every governed seat within a generation, which is the signature of load-bearing coordination rather than ornament.
% FOUNDING_PROBLEM: After the destruction of 70 CE the sacrificial commandments stayed legally binding while becoming physically impossible: no altar, no priesthood in service, no site. Left unresolved, every member of the community stood in permanent technical breach and the elaborate sacrificial legal corpus faced abandonment as unobservable dead letter. The arrangement was built to keep the obligation alive and accounted for, discharged in form through study, until the conditions of performance return.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: historians of rabbinic literature and of Second Temple Judaism document the post-destruction impossibility and the tradition's restructuring around it; holders of the rival readings of the same kernel attest from their own seats that the performance-impossibility problem remains live, disputing only what follows from it; secular academic scholarship on post-destruction Jewish law reaches the same assessment. No participating authority's self-attestation is relied upon for the problem's status.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__study_as_occupation, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__study_as_occupation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__study_as_occupation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_obligation__study_as_occupation, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__study_as_occupation, 0.18, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__study_as_occupation_tests).
:- end_tests(temple_sacrifice_obligation__study_as_occupation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.18): the settlement converts an unperformable obligation into a performable practice, so no participant bears an unmet debt; the residual score reflects the modest institutional rent accruing to the interpretive authority and the real opportunity cost of careers spent on practically inapplicable ritual law, sitting modestly above the inherent-cost floor of identity coordination. Suppression is low (0.15) and is authored as a raw structural property, NOT scaled by power or scope: the settlement persists by normative consensus and curricular habit rather than coercive machinery, and rival accounts of the obligation circulate openly at the margins. Theater ratio is low-to-moderate (0.20): the study is constitutive rather than decorative under this reading, but a growing share of engagement is calendrical and commemorative as practical application recedes further into the past. Accessibility collapse is moderate (0.40): understanding the settlement does not close off rival accounts, since the sibling readings remain live positions, so alternatives only partly collapse. Resistance is low (0.12): the settlement meets little active resistance inside the observant world. The measurement series run on ONE shared time grid (points 0, 6, 12, 18, 24, 30) so every tracked metric is authored at every examined time point; both series drift mildly upward, reflecting institutional consolidation and partial ritualization rather than any enforcement ratchet. Suppression_requirement series are deliberately omitted: the enforcement picture is static across the interval, with no enforcement machinery built up or decayed, so the scalar base_properties.suppression carries that fact.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the agenda-setter seat, the settlement is the tradition's signal achievement: an interpretive chain that kept a commandment system whole for two millennia without its altar; its exit is identity_locked because the institution's self-account is fused with having done this. From the scholar seat, the settlement fuses vocation with doctrine: study with no practical application is nonetheless the discharge of the obligation, so professional identity and religious merit coincide. From the ordinary community seat, the settlement is largely invisible infrastructure: members simply count as observant. The outside analytical seat sees a textbook case of a post-crisis community converting an impossible rite into sustainable practice. The excluded restorationist seat, which sits outside the settlement's governance, experiences the same structure as evasion, but that perception belongs to rival framings and is carried here only as commentary-grade absence, never as a classification override.
 *
 * DIRECTIONALITY LOGIC:
 *   Every governed seat declares beneficiary position and none declares victim position, so derived directionality sits near the beneficiary end for all of them: the community receives a performable path to fulfillment; scholars receive standing, livelihood, and merit; the authority receives interpretive centrality and support, with directionality nudged slightly above pure beneficiary by the maintenance burdens it carries in administering curricula, answering fulfillment questions, and certifying continuity. No seat bears asymmetric extraction, which is the structural signature the rope claim rests on. The excluded restorationist seat falls outside the derivation: it is not governed by the settlement, and its objection is recorded as absence rather than paid cost. Global spatial scope modestly amplifies whatever extraction the engine computes, but the authored epsilon already reflects the settled low level rather than presuming scope effects.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, binding commandments rendered unperformable by the Temple's destruction, is still live, so the settlement's mandate has not outlived its function and no mandatrophy is declared. The classification guards against two mislabels. Reading the settlement as a snare would require a victim set; under this reading none exists, because the obligation is discharged rather than deferred onto anyone. Reading it as a piton would require an atrophied function maintained theatrically; the study function is live and constitutive, and the theater ratio reflects only partial ritualization. The subtler risk is the reverse: the settlement's declared conditionality, its texts scope the ruling to the Temple's absence, could invite a scaffold reading. The mandatrophy lens shows why that fails: the arrangement has operated as steady-state coordination across the whole interval, its sunset condition is eschatological and unschedulable, and no institutional mechanism terminates it. That tension is carried as an omega rather than resolved by fiat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_status_trilemma_omega,
    'This constraint is the study_as_occupation reading of the kernel temple_sacrifice_obligation; what changes structurally if a sibling reading is adopted instead?',
    'Adoption tracing: if authorities shift to messianic_suspension, the obligation becomes an undischarged waiting debt and felt extraction rises across every seat with no fulfillment claim; if they shift to study_as_archiving, study becomes preparation rather than discharge, installing a deferred-performance structure with scaffold-like interim accounting. Track decisor statements and curriculum framing for which status the interim obligation carries.',
    'Suspension adoption would raise effective extraction for every governed seat and likely push classification toward snare-adjacent tension; archiving adoption would introduce a sunset-shaped interim structure and reclassify toward scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_status_trilemma_omega, conceptual, 'Kernel-reading indexicality: sibling readings assign different statuses to the same interim obligation.').

omega_variable(
    steady_state_vs_eschatological_sunset,
    'Is the settlement a steady-state coordination rope or transitional scaffolding awaiting an eschatological sunset it cannot schedule?',
    'Examine whether the tradition operationalizes restoration, with dated plans and practical preparation integrated into the settlement, or treats it as horizon-only; developments bearing on performability, such as red heifer candidates and site-access changes, offer a live test: if performability approaches and the settlement yields gracefully, scaffold; if it reinterprets to remain necessary, rope.',
    'Scaffold classification would attach sunset-gate dynamics and transitional justification; rope classification treats the arrangement as durable coordination whose declared conditionality has never functioned as a termination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(steady_state_vs_eschatological_sunset, conceptual, 'Rope-versus-scaffold ambiguity arising from the undeclared eschatological horizon.').

omega_variable(
    interpretive_rent_share,
    'How much of the interpretive authority''s standing depends specifically on the study-equivalence doctrine, as opposed to its broader halakhic functions?',
    'Counterfactual institutional analysis: estimate the share of academy enrollment, funding, and decisor attention attributable to the sacrificial orders and to the fulfillment claim, using curriculum allocations and responsa corpora.',
    'A high doctrinal share would indicate the authority draws its legitimacy substantially through this arrangement, pushing toward tangled_rope; a low share confirms the rope reading with incidental institutional benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_rent_share, empirical, 'Magnitude of authority rents tied to the study-equivalence doctrine.').

omega_variable(
    authority_identity_fusion_drift,
    'Is the interpretive authority''s commitment to the settlement identity-fused such that it cannot surface revision even if performance becomes possible again?',
    'Observe the structure''s response to performability-approaching events: does it metabolize them into the study frame, declaring study remains the essence, or yield to resumed performance? Persistent reframing under changed conditions indicates identity lock.',
    'Identity fusion would mean the arrangement persists by inertia if its founding context changes, a rope hardening toward piton-like maintenance; without fusion, the settlement would hand the obligation back cleanly at restoration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_identity_fusion_drift, empirical, 'Whether the authority seat can revise the settlement when its founding conditions change.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_occupation, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tso_study_occupation_tr_t0, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(tso_study_occupation_tr_t0, observed).
narrative_ontology:measurement(tso_study_occupation_tr_t6, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 6, 0.12).
narrative_ontology:measurement_basis(tso_study_occupation_tr_t6, observed).
narrative_ontology:measurement(tso_study_occupation_tr_t12, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 12, 0.14).
narrative_ontology:measurement_basis(tso_study_occupation_tr_t12, observed).
narrative_ontology:measurement(tso_study_occupation_tr_t18, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 18, 0.16).
narrative_ontology:measurement_basis(tso_study_occupation_tr_t18, observed).
narrative_ontology:measurement(tso_study_occupation_tr_t24, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 24, 0.18).
narrative_ontology:measurement_basis(tso_study_occupation_tr_t24, observed).
narrative_ontology:measurement(tso_study_occupation_tr_t30, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 30, 0.2).
narrative_ontology:measurement_basis(tso_study_occupation_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(tso_study_occupation_be_t0, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 0, 0.13).
narrative_ontology:measurement_basis(tso_study_occupation_be_t0, observed).
narrative_ontology:measurement(tso_study_occupation_be_t6, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 6, 0.15).
narrative_ontology:measurement_basis(tso_study_occupation_be_t6, observed).
narrative_ontology:measurement(tso_study_occupation_be_t12, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 12, 0.16).
narrative_ontology:measurement_basis(tso_study_occupation_be_t12, observed).
narrative_ontology:measurement(tso_study_occupation_be_t18, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 18, 0.17).
narrative_ontology:measurement_basis(tso_study_occupation_be_t18, observed).
narrative_ontology:measurement(tso_study_occupation_be_t24, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 24, 0.18).
narrative_ontology:measurement_basis(tso_study_occupation_be_t24, observed).
narrative_ontology:measurement(tso_study_occupation_be_t30, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 30, 0.18).
narrative_ontology:measurement_basis(tso_study_occupation_be_t30, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(temple_sacrifice_obligation__study_as_occupation, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__study_as_occupation, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation__messianic_suspension).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation__study_as_archiving).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'what happened to the sacrifice obligation after the destruction' covers three structurally distinct claims with different epsilon values, beneficiary structures, and failure modes. This file authors the study_as_occupation claim (study discharges the obligation; low epsilon, no victims, rope). The sibling files author messianic_suspension (an undischarged waiting debt; higher felt extraction across seats) and study_as_archiving (deferred-performance preparation; interim structure with scaffold-like dynamics). All three share the referent, the standing post-destruction arrangement of the sacrificial commandments, and differ only in the reading applied to it; family links are declared through network.affects_constraints in each file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
