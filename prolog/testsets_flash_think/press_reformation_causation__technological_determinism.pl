% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__technological_determinism, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: press_reformation_causation__technological_determinism
 *   human_readable: Printing Press as Inevitable Cause of Reformation (Technological Determinism Reading)
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the 'technological determinism'
 *   reading of the printing press's role in the Reformation. It posits the
 *   printing press as an autonomous technological force that inevitably
 *   caused the Reformation by making censorship impossible and the widespread
 *   dissemination of vernacular scripture unavoidable. The constraint's
 *   operation is seen as a direct, unmediated consequence of the technology's
 *   inherent properties, rather than human agency or strategic deployment.
 *   The claim is 'mountain' because the technology's effect is presented as
 *   an unchangeable, natural law of media, but the presence of clear
 *   beneficiaries and victims will trigger False Summit Mountain detection.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__technological_determinism, 0.85).
domain_priors:suppression_score(press_reformation_causation__technological_determinism, 0.9).
domain_priors:theater_ratio(press_reformation_causation__technological_determinism, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, extractiveness, 0.85).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causation__technological_determinism, "Printing Press as Inevitable Cause of Reformation (Technological Determinism Reading)").
narrative_ontology:topic_domain(press_reformation_causation__technological_determinism, "history_of_technology/religious_history/media_studies").

domain_priors:emerges_naturally(press_reformation_causation__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__technological_determinism, '240198a0-ee2c-4b7a-9364-5a86ba192ec8').
narrative_ontology:cs_kernel_codification('240198a0-ee2c-4b7a-9364-5a86ba192ec8', implicit).
narrative_ontology:cs_authority_grounding('240198a0-ee2c-4b7a-9364-5a86ba192ec8', diffuse_epistemic).
narrative_ontology:cs_reading_relation('240198a0-ee2c-4b7a-9364-5a86ba192ec8', press_reformation_causation__strategic_deployment, forecloses).
narrative_ontology:cs_reading_relation('240198a0-ee2c-4b7a-9364-5a86ba192ec8', press_reformation_causation__mutual_shaping, forecloses).
narrative_ontology:cs_axiom('240198a0-ee2c-4b7a-9364-5a86ba192ec8', foundational, technology_as_exogenous_force).
narrative_ontology:cs_axiom_status(technology_as_exogenous_force, overridden).
narrative_ontology:cs_axiom_grounding('240198a0-ee2c-4b7a-9364-5a86ba192ec8', technology_as_exogenous_force, empirically_contingent).
narrative_ontology:cs_axiom('240198a0-ee2c-4b7a-9364-5a86ba192ec8', foundational, information_flow_unstoppable).
narrative_ontology:cs_axiom_status(information_flow_unstoppable, holdable).
narrative_ontology:cs_axiom_grounding('240198a0-ee2c-4b7a-9364-5a86ba192ec8', information_flow_unstoppable, empirically_contingent).
narrative_ontology:cs_reference_frame('240198a0-ee2c-4b7a-9364-5a86ba192ec8', technological_autonomy).
narrative_ontology:cs_drift_state('240198a0-ee2c-4b7a-9364-5a86ba192ec8', contemporary_historiography, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('240198a0-ee2c-4b7a-9364-5a86ba192ec8', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__technological_determinism, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, protestant_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, vernacular_readers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, printers).
narrative_ontology:constraint_victim(press_reformation_causation__technological_determinism, catholic_church).
narrative_ontology:constraint_victim(press_reformation_causation__technological_determinism, latin_literati).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The technology itself, viewed as an autonomous force with inherent properties that inevitably shaped society, rather than a neutral tool. It is the 'cause' in this deterministic reading.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, printing_press_technology, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(press_reformation_causation__technological_determinism, printing_press_technology).

% Benefited immensely from the press's capacity to rapidly disseminate their ideas and vernacular scriptures, bypassing traditional Church control. Their success is attributed to the press's inherent properties.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, protestant_reformers, beneficiary,
    powerful, biographical, mobile, continental).

% Suffered a profound loss of control over information, religious doctrine, and the interpretation of scripture. Its attempts at censorship were rendered futile by the press's inherent decentralizing power, leading to a decline in its authority and revenue.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, catholic_church, payer,
    institutional, generational, constrained, global).

% Gained unprecedented access to religious texts in their own languages, fostering individual interpretation and undermining the Latin-based authority of the clergy. This access was an inevitable outcome of the press.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, vernacular_readers, beneficiary,
    moderate, biographical, mobile, local).

% Lost their exclusive status as interpreters of sacred texts and custodians of knowledge, as the press democratized access to information. Their intellectual authority was eroded by the inevitable spread of vernacular literacy.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, latin_literati, payer,
    moderate, biographical, constrained, regional).

% Experienced significant economic opportunity and social influence by meeting the demand for printed materials, especially religious texts. Their commercial success was a direct consequence of the press's transformative power.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, printers, beneficiary,
    moderate, biographical, mobile, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The printing press, as a technology, inherently coordinated the rapid and widespread dissemination of information, creating a new standard for public discourse that bypassed traditional gatekeepers.
% TRANSFER_FUNCTION: Transferred control over religious discourse, knowledge production, and interpretive authority from the centralized Catholic Church to a decentralized network of printers, reformers, and vernacular readers.
% ABSENT_VOICES: Those who advocated for a more gradual, controlled reform within the Catholic Church, or those who believed that the press could be effectively managed by existing authorities, are absent from this deterministic narrative, as their efforts are deemed futile against the press's inherent power.
% DISAPPEARANCE_RATIONALE: If the printing press had never been invented, the conditions for the Reformation as it occurred would not have existed. Censorship would have remained effective, vernacular scripture would not have spread widely, and the rapid, decentralized dissemination of reformist ideas would have been impossible. The entire course of early modern European history would have been fundamentally different.
% FOUNDING_PROBLEM: The problem of information control and the Catholic Church's monopoly on the interpretation and dissemination of religious knowledge, which limited access to scripture and stifled dissent.
% FOUNDING_PROBLEM_CORROBORATION: Some historians of technology and early media theorists, particularly those aligned with McLuhan-esque perspectives, corroborate this view, emphasizing the inherent properties of the medium. However, many contemporary historians contest this deterministic framing.
narrative_ontology:disappearance_verdict(press_reformation_causation__technological_determinism, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__technological_determinism, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__technological_determinism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(press_reformation_causation__technological_determinism, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__technological_determinism, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__technological_determinism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(press_reformation_causation__technological_determinism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, ExtMetricName, E),
    domain_priors:suppression_score(press_reformation_causation__technological_determinism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(press_reformation_causation__technological_determinism),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(press_reformation_causation__technological_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the press's effect is seen as extracting power and authority from the old order (Catholic Church) and transferring it to the new (reformers, vernacular readers). Suppression is very high (0.90) because the press is viewed as inherently suppressing the Church's ability to control information, rendering its censorship efforts futile. Theater ratio is low (0.10) as the effect is presented as a direct, functional outcome of the technology, not a performative one. Accessibility collapse is near total (0.95) for the Church's prior monopoly on information, as the press fundamentally altered the information landscape. Resistance (0.70) from the Church was significant but ultimately ineffective against the 'inevitable' force of the press.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the 'printing_press_technology' (as an analytical observer of its own deterministic effect), its impact is a natural, unmediated force. From the 'protestant_reformers' seat, it is a liberating force. From the 'catholic_church' seat, it is an unstoppable, destructive force. The engine will compute these divergent classifications based on the structural roles and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   The printing press, as the 'causal agent' in this reading, acts as a structural force that benefits Protestant reformers, vernacular readers, and printers by enabling their agendas and increasing their access to information. Conversely, it acts as a structural target for the Catholic Church and Latin literati, from whom control, authority, and exclusive knowledge access are extracted. The directionality reflects this inherent, deterministic transfer of power.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_autonomy_vs_agency,
    'Is the printing press an autonomous technological force with inherent, deterministic effects, or is its impact mediated by human agency and strategic choices?',
    'Comparative historical analysis of other media technologies and their societal impacts, examining cases where similar technologies did not lead to similar outcomes due to differing social, political, or religious contexts.',
    'If human agency is found to be a significant mediator, the ''mountain'' claim for the press''s causal power would be reclassified, likely towards a ''rope'' or ''tangled_rope'' that facilitates, rather than determines, outcomes. This would shift the constraint''s core nature from an unchangeable force to a tool or platform.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_autonomy_vs_agency, conceptual, 'Ambiguity between technological determinism and human agency in shaping historical outcomes.').

omega_variable(
    censorship_impossibility_degree,
    'To what extent did the printing press truly make censorship ''impossible,'' as opposed to merely more difficult or requiring new forms of control?',
    'Empirical study of censorship attempts and successes in early modern Europe, including the effectiveness of indices of prohibited books, licensing systems, and state-sponsored propaganda.',
    'If censorship was merely made more difficult, the ''suppression'' metric would be lower, and the ''accessibility_collapse'' for the Church''s control would be less absolute, potentially shifting the constraint''s classification away from a pure ''mountain'' of inevitability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(censorship_impossibility_degree, empirical, 'The actual efficacy of censorship against the printing press.').

omega_variable(
    vernacular_scripture_inevitability,
    'Was the widespread adoption of vernacular scripture an inevitable outcome of the printing press, or a strategic choice by reformers that the press merely facilitated?',
    'Analysis of the theological and political motivations behind vernacular translations, and the varying rates of adoption across different regions and denominations, even with access to printing technology.',
    'If vernacular scripture was a strategic choice, the ''beneficiary'' status of reformers and readers would remain, but the ''inevitability'' aspect of the constraint would weaken, challenging the ''mountain'' claim and suggesting a more contingent, agency-driven process.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vernacular_scripture_inevitability, conceptual, 'The inevitability vs. strategic choice of vernacular scripture dissemination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__technological_determinism, 1450, 1550).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causation__technological_determinism, theater_ratio, 1450, 0.1).
narrative_ontology:measurement(pres_tr_t1470, press_reformation_causation__technological_determinism, theater_ratio, 1470, 0.1).
narrative_ontology:measurement(pres_tr_t1490, press_reformation_causation__technological_determinism, theater_ratio, 1490, 0.1).
narrative_ontology:measurement(pres_tr_t1510, press_reformation_causation__technological_determinism, theater_ratio, 1510, 0.1).
narrative_ontology:measurement(pres_tr_t1530, press_reformation_causation__technological_determinism, theater_ratio, 1530, 0.1).
narrative_ontology:measurement(pres_tr_t1550, press_reformation_causation__technological_determinism, theater_ratio, 1550, 0.1).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causation__technological_determinism, base_extractiveness, 1450, 0.6).
narrative_ontology:measurement(pres_be_t1470, press_reformation_causation__technological_determinism, base_extractiveness, 1470, 0.7).
narrative_ontology:measurement(pres_be_t1490, press_reformation_causation__technological_determinism, base_extractiveness, 1490, 0.78).
narrative_ontology:measurement(pres_be_t1510, press_reformation_causation__technological_determinism, base_extractiveness, 1510, 0.82).
narrative_ontology:measurement(pres_be_t1530, press_reformation_causation__technological_determinism, base_extractiveness, 1530, 0.84).
narrative_ontology:measurement(pres_be_t1550, press_reformation_causation__technological_determinism, base_extractiveness, 1550, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1450, press_reformation_causation__technological_determinism, suppression_requirement, 1450, 0.5).
narrative_ontology:measurement(pres_su_t1470, press_reformation_causation__technological_determinism, suppression_requirement, 1470, 0.65).
narrative_ontology:measurement(pres_su_t1490, press_reformation_causation__technological_determinism, suppression_requirement, 1490, 0.75).
narrative_ontology:measurement(pres_su_t1510, press_reformation_causation__technological_determinism, suppression_requirement, 1510, 0.85).
narrative_ontology:measurement(pres_su_t1530, press_reformation_causation__technological_determinism, suppression_requirement, 1530, 0.88).
narrative_ontology:measurement(pres_su_t1550, press_reformation_causation__technological_determinism, suppression_requirement, 1550, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__technological_determinism, information_standard).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, rise_of_nation_states).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, scientific_revolution).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__mutual_shaping).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'press_reformation_causation' kernel. This 'technological_determinism' reading emphasizes the press as an autonomous causal force, distinct from readings that focus on strategic deployment or mutual shaping.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
