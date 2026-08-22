% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__incoherence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__incoherence_reading, []).

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
 *   constraint_id: shinbutsu_ontological_commitment__incoherence_reading
 *   human_readable: Edo-Period Shinbutsu Institutional Incoherence
 *   domain: religious/historical/institutional
 *
 * SUMMARY:
 *   This constraint story instantiates the incoherence reading of the
 *   shinbutsu ontological commitment kernel. Under this reading, the
 *   Edo-period arrangement of shinbutsu-shugo did not rest on a stable
 *   honji-suijaku metaphysics or on a functional partition of domains, but
 *   rather on institutional tolerance for ontological ambiguity.
 *   Temple-shrine complexes performed both Buddhist and Shinto rites for the
 *   same parishioners without doctrinal resolution, while the Tokugawa bakufu
 *   managed religious institutions for political stability rather than
 *   theological coherence. The constraint is the standing arrangement of this
 *   tolerated incoherence itself.
 *
 * KEY AGENTS:
 *   - tokugawa_bakufu (agenda_setter, institutional/arbitrage): Administered the religious settlement through jisha bugyo without imposing ontological uniformity.
 *   - syncretic_temple_shrine_complexes (agenda_setter/beneficiary, organized/constrained): Performed dual rites and collected parish fees under the danka system.
 *   - danka_parishioners (payer, powerless/trapped): Legally required to support temples and participate in an ambiguous ritual nexus.
 *   - doctrinal_purists (payer, moderate/constrained): Marginalized clerics and scholars seeking orthodox consistency.
 *   - modern_historians (observer, analytical): Debate whether the record shows coherence, separation, or incoherence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__incoherence_reading, 0.58).
domain_priors:suppression_score(shinbutsu_ontological_commitment__incoherence_reading, 0.6).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__incoherence_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__incoherence_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__incoherence_reading, "Edo-Period Shinbutsu Institutional Incoherence").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__incoherence_reading, "religious/historical/institutional").

domain_priors:requires_active_enforcement(shinbutsu_ontological_commitment__incoherence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__incoherence_reading, '2a2e33a4-a1fc-4320-ac5a-3c512a786b56').
narrative_ontology:cs_kernel_codification('2a2e33a4-a1fc-4320-ac5a-3c512a786b56', distributed).
narrative_ontology:cs_authority_grounding('2a2e33a4-a1fc-4320-ac5a-3c512a786b56', distributed).
narrative_ontology:cs_reading_relation('2a2e33a4-a1fc-4320-ac5a-3c512a786b56', shinbutsu_ontological_commitment__syncretic_reading, forecloses).
narrative_ontology:cs_reading_relation('2a2e33a4-a1fc-4320-ac5a-3c512a786b56', shinbutsu_ontological_commitment__partition_reading, forecloses).
narrative_ontology:cs_axiom('2a2e33a4-a1fc-4320-ac5a-3c512a786b56', foundational, no_stable_ontological_commitment).
narrative_ontology:cs_axiom_status(no_stable_ontological_commitment, holdable).
narrative_ontology:cs_axiom_grounding('2a2e33a4-a1fc-4320-ac5a-3c512a786b56', no_stable_ontological_commitment, empirically_contingent).
narrative_ontology:cs_axiom('2a2e33a4-a1fc-4320-ac5a-3c512a786b56', foundational, institutional_incoherence_was_tolerated).
narrative_ontology:cs_axiom_status(institutional_incoherence_was_tolerated, holdable).
narrative_ontology:cs_axiom_grounding('2a2e33a4-a1fc-4320-ac5a-3c512a786b56', institutional_incoherence_was_tolerated, empirically_contingent).
narrative_ontology:cs_reference_frame('2a2e33a4-a1fc-4320-ac5a-3c512a786b56', edo_religious_settlement_practice).
narrative_ontology:cs_drift_state('2a2e33a4-a1fc-4320-ac5a-3c512a786b56', meiji_restoration, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('2a2e33a4-a1fc-4320-ac5a-3c512a786b56', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, syncretic_temple_shrine_complexes).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, tokugawa_bakufu).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, doctrinal_purists).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, danka_parishioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administered religious institutions through the jisha bugyo office without imposing ontological uniformity, prioritizing political stability and social control over doctrinal coherence. Tolerated and occasionally regulated temple-shrine relations to prevent either religious faction from accumulating independent political authority.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, tokugawa_bakufu, agenda_setter,
    institutional, generational, arbitrage, national).

% Operated combined ritual services including funerals, memorials, festivals, and talismans for local communities without resolving whether kami were manifestations of buddhas or independent ontological entities. Collected fees and maintained compulsory parish registries under the danka system while performing syncretic rites that obscured doctrinal boundaries.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, syncretic_temple_shrine_complexes, agenda_setter,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__incoherence_reading, syncretic_temple_shrine_complexes, beneficiary).

% Required by the danka system to register with and financially support a Buddhist temple regardless of personal belief. Participated in both Buddhist and Shinto rites through the same local institutional nexus without access to consistent doctrinal explanation of the relationship between the two traditions. Exit incurred legal penalties and social ostracism.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, danka_parishioners, payer,
    powerless, biographical, trapped, local).

% Buddhist clerics and Shinto scholars who insisted on orthodox doctrinal consistency were marginalized by an institutional order that treated assertive theology as politically threatening. Their writings circulated in private scholarly networks but did not shape mainstream temple-shrine practice or parish education.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, doctrinal_purists, payer,
    moderate, biographical, constrained, national).

% Analyze the Edo religious system through archival and ethnographic evidence, debating whether the record shows underlying metaphysical coherence, stable functional separation, or administrative indifference to ontology.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, modern_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_commitment__incoherence_reading, syncretic_temple_shrine_complexes).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_commitment__incoherence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allowed Buddhist temples and Shinto shrines to coexist and share parishioners without requiring doctrinal resolution or state adjudication of theological truth, solving the problem of potential inter-religious conflict under a unified political order.
% TRANSFER_FUNCTION: Moves material support in the form of fees, compulsory labor, and political compliance from danka parishioners to syncretic temple-shrine complexes, while moving political stability and social control from religious institutions to the Tokugawa state.
% ABSENT_VOICES: Doctrinal purists seeking Buddhist or Shinto orthodoxy were intellectually marginalized; Christian and heterodox communities were violently excluded from the religious frame entirely; Meiji modernizers who would later repudiate the arrangement were not yet in power during the mature Edo period.
% DISAPPEARANCE_RATIONALE: If the institutional incoherence vanished and was replaced by clear ontological separation or unified doctrine, the danka system would require immediate renegotiation, shrine-temple complexes would split along doctrinal lines, parishioner obligations would shift dramatically, and the Tokugawa political settlement would lose a key stabilizer of rural social order.
% FOUNDING_PROBLEM: Medieval and early modern Japan needed a religious settlement that could integrate powerful Buddhist institutional networks with indigenous kami worship without empowering any single doctrinal authority to challenge the secular political order.
% FOUNDING_PROBLEM_CORROBORATION: Modern historians outside the beneficiary institutions, including Kuroda Toshio and subsequent critical scholars, attest the founding problem was the management of religious political threat. Tokugawa-era jisha bugyo records corroborate the political motivation, though Edo beneficiary institutions naturally assert the arrangement served authentic spiritual needs.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__incoherence_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__incoherence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__incoherence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__incoherence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__incoherence_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__incoherence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_commitment__incoherence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_commitment__incoherence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58 at peak) reflects the extraction of material support and political compliance through institutions that performed spiritual authority without ontological foundation. Suppression (0.60 at peak) captures the active enforcement of the danka system and suppression of heterodox movements that maintained the frame within which incoherence could persist. Theater_ratio is high (0.72 at peak) because the performance of syncretic ritual and cosmological unity dominated over substantive doctrinal commitment. The end-state (1871) shows collapse as the Meiji state repudiates the frame. Accessibility_collapse is moderate (0.45) because pure Shinto or Buddhist alternatives existed intellectually but were institutionally unreachable for most. Resistance is low (0.25) because the incoherence was habitual rather than contested by organized movements.
 *
 * PERSPECTIVAL GAP:
 *   The bakufu and temple-shrine complexes experience the constraint as a functional coordination mechanism that preserves order and revenue; parishioners and purists experience it as an extraction of resources and doctrinal integrity without coherent spiritual return. The engine computes this divergence from the structural asymmetry in power and exit.
 *
 * DIRECTIONALITY LOGIC:
 *   The bakufu sits near the beneficiary end (low d) because the constraint subsidizes political stability and minimizes theological governance costs. Temple-shrine complexes sit at moderate beneficiary (low d) because they collect material flows. Parishioners sit near full target (high d) due to trapped exit and direct fee extraction. Doctrinal purists sit at high d because the constraint actively marginalizes their position. Modern historians sit at analytical (no directional extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâmanaging religious-political threat in the early modern stateâwas dead by the Meiji Restoration, yet the constraint persisted through the Edo period. Classifying it as tangled_rope rather than rope captures the coordination-extraction duality: the ambiguity genuinely coordinated peaceful coexistence, but it also enabled asymmetric extraction. A rope classification would miss the victimization of parishioners and purists; a snare classification would miss the genuine coordination function of avoiding doctrinal warfare. Piton is rejected because concentrated beneficiaries (temple-shrine complexes) actively profited and maintained the arrangement, and the Meiji transition shows it was changeable rather than purely inertialâthough the high theater_ratio captures its performative dimension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_sibling_delta,
    'This constraint instantiates the incoherence reading of the shinbutsu ontological commitment kernel; a syncretic reading would posit a genuine honji-suijaku metaphysics underlying the same institutions, while a partition reading would posit stable separate domains. Which structural account best fits the Edo-period archival record?',
    'Systematic review of Edo-period religious institutional records for evidence of internal metaphysical consistency versus administrative indifference to doctrine.',
    'Adopting the syncretic reading would reclassify the constraint as a commitment system with deontological grounding; adopting the partition reading would reclassify as dual-rope institutional separation; the incoherence reading produces tangled_rope due to coordination-through-ambiguity plus asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_sibling_delta, conceptual, 'Structural ambiguity between kernel readings and their empirical basis.').

omega_variable(
    meiji_beneficiary_ambiguity,
    'Did the Meiji state benefit from the ease of separation because the prior arrangement was genuinely incoherent, or did Meiji ideologues manufacture the incoherence narrative to legitimize State Shinto?',
    'Comparative analysis of pre-Meiji local parish records versus Meiji ideological documents for evidence of locally experienced coherence.',
    'If manufactured, the constraint''s instability and theater_ratio are overstated by retrospective projection; if genuine, the collapse sequence is endogenous to the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(meiji_beneficiary_ambiguity, empirical, 'Whether Meiji separation ease reflects genuine prior incoherence or manufactured narrative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__incoherence_reading, 1600, 1871).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t1600, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1600, 0.45).
narrative_ontology:measurement(shin_tr_t1675, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1675, 0.58).
narrative_ontology:measurement(shin_tr_t1750, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1750, 0.72).
narrative_ontology:measurement(shin_tr_t1800, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1800, 0.68).
narrative_ontology:measurement(shin_tr_t1850, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1850, 0.62).
narrative_ontology:measurement(shin_tr_t1871, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1871, 0.78).

% Extraction over time
narrative_ontology:measurement(shin_be_t1600, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1600, 0.42).
narrative_ontology:measurement(shin_be_t1675, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1675, 0.5).
narrative_ontology:measurement(shin_be_t1750, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1750, 0.58).
narrative_ontology:measurement(shin_be_t1800, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1800, 0.55).
narrative_ontology:measurement(shin_be_t1850, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1850, 0.48).
narrative_ontology:measurement(shin_be_t1871, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1871, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t1600, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1600, 0.5).
narrative_ontology:measurement(shin_su_t1675, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1675, 0.65).
narrative_ontology:measurement(shin_su_t1750, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1750, 0.6).
narrative_ontology:measurement(shin_su_t1800, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1800, 0.5).
narrative_ontology:measurement(shin_su_t1850, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1850, 0.4).
narrative_ontology:measurement(shin_su_t1871, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1871, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__incoherence_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_ontological_commitment__syncretic_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_ontological_commitment__partition_reading).

% DUAL FORMULATION NOTE:
% This constraint is the incoherence reading of the shinbutsu ontological commitment kernel. It decomposes from the colloquial label 'shinbutsu-shugo' along with the syncretic reading (honji-suijaku metaphysics) and partition reading (separate domain functionalism). Each reading carries a distinct epsilon and structural profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
