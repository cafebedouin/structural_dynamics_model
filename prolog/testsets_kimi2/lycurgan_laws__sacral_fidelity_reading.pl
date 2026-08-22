% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__sacral_fidelity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__sacral_fidelity_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: lycurgan_laws__sacral_fidelity_reading
 *   human_readable: Lycurgan Laws: Sacral Fidelity Reading
 *   domain: political_philosophy/constitutional_theory/commitment_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the sacral fidelity reading of the
 *   lycurgan_laws kernel. In this reading, the Spartan constitutional order
 *   is treated as a fixed, divine ordinance delivered by Lycurgus through
 *   Apollo at Delphi. Immutability is not a design flaw but a sacred virtue;
 *   societal decline is attributed to citizen moral failure or external
 *   military pressure rather than systemic brittleness. The constraint
 *   governs a rigid hierarchy of Spartan citizens (homoioi), helots, and
 *   perioeci through active enforcement by ephors and social institutions.
 *   The reading presents the arrangement as natural law, but identifiable
 *   beneficiary and victim classes expose it to False Summit Mountain
 *   evaluation.
 *
 * KEY AGENTS:
 *   - ephors: Agenda-setter (institutional/constrained) â enforce the immutable code and declare war on helots annually
 *   - spartan_citizens: Primary beneficiary (organized/identity_locked) â receive land, political rights, and status maintained by helot subordination
 *   - helots: Primary target (powerless/trapped) â bear extraction through violent enslavement and agricultural labor
 *   - perioeci: Secondary target (moderate/constrained) â free non-citizens excluded from political benefit and pressed into auxiliary military service
 *   - external_historians: Analytical observer (analytical/analytical) â record and evaluate the system from outside the benefiting class
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__sacral_fidelity_reading, 0.78).
domain_priors:suppression_score(lycurgan_laws__sacral_fidelity_reading, 0.88).
domain_priors:theater_ratio(lycurgan_laws__sacral_fidelity_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__sacral_fidelity_reading, mountain).
narrative_ontology:human_readable(lycurgan_laws__sacral_fidelity_reading, "Lycurgan Laws: Sacral Fidelity Reading").
narrative_ontology:topic_domain(lycurgan_laws__sacral_fidelity_reading, "political_philosophy/constitutional_theory/commitment_systems").

domain_priors:requires_active_enforcement(lycurgan_laws__sacral_fidelity_reading).
domain_priors:emerges_naturally(lycurgan_laws__sacral_fidelity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__sacral_fidelity_reading, '2bcd60bf-f6a2-4a46-85bf-9176acef8fec').
narrative_ontology:cs_kernel_codification('2bcd60bf-f6a2-4a46-85bf-9176acef8fec', fixed_text).
narrative_ontology:cs_authority_grounding('2bcd60bf-f6a2-4a46-85bf-9176acef8fec', lineage).
narrative_ontology:cs_interpretation_layer_present('2bcd60bf-f6a2-4a46-85bf-9176acef8fec').
narrative_ontology:cs_reading_relation('2bcd60bf-f6a2-4a46-85bf-9176acef8fec', lycurgan_laws__demographic_trap_reading, forecloses).
narrative_ontology:cs_reading_relation('2bcd60bf-f6a2-4a46-85bf-9176acef8fec', lycurgan_laws__adaptive_fiction_reading, forecloses).
narrative_ontology:cs_axiom('2bcd60bf-f6a2-4a46-85bf-9176acef8fec', foundational, constitutional_immutability_is_divine_virtue).
narrative_ontology:cs_axiom_status(constitutional_immutability_is_divine_virtue, holdable).
narrative_ontology:cs_axiom_grounding('2bcd60bf-f6a2-4a46-85bf-9176acef8fec', constitutional_immutability_is_divine_virtue, theological).
narrative_ontology:cs_axiom('2bcd60bf-f6a2-4a46-85bf-9176acef8fec', foundational, spartan_decline_is_citizen_failure).
narrative_ontology:cs_axiom_status(spartan_decline_is_citizen_failure, holdable).
narrative_ontology:cs_axiom_grounding('2bcd60bf-f6a2-4a46-85bf-9176acef8fec', spartan_decline_is_citizen_failure, deontological).
narrative_ontology:cs_reference_frame('2bcd60bf-f6a2-4a46-85bf-9176acef8fec', divine_lycurgan_origin).
narrative_ontology:cs_drift_state('2bcd60bf-f6a2-4a46-85bf-9176acef8fec', classical_spartan_decline, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2bcd60bf-f6a2-4a46-85bf-9176acef8fec', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__sacral_fidelity_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, spartan_citizens).
narrative_ontology:constraint_victim(lycurgan_laws__sacral_fidelity_reading, helots).
narrative_ontology:constraint_victim(lycurgan_laws__sacral_fidelity_reading, perioeci).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elected magistrates responsible for enforcing the Lycurgan code, overseeing citizen conduct, and preserving constitutional immutability. They interpret the sacred laws and administer state terror against helots, but are themselves bound by the same code they enforce.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, ephors, agenda_setter,
    institutional, generational, constrained, national).

% Full citizens (homoioi) who hold land allotments worked by helots, participate in the assembly, and share in collective military supremacy. Their lives are strictly regulated by the agoge, syssitia, and dress codes; exit from this identity is socially and politically impossible.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, spartan_citizens, beneficiary,
    organized, generational, identity_locked, national).

% Enslaved agricultural population bound to Spartan-owned land. They perform the labor that sustains the citizen class and are subject to systematic state violence including the annual declaration of war by ephors and the Krypteia. No legal or geographic exit exists.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, helots, payer,
    powerless, immediate, trapped, local).

% Free non-citizen inhabitants engaged in trade, craft, and auxiliary military service. They lack political rights and are excluded from the land-helot system, yet are subordinate to Spartan citizens and required to serve Spartan military interests.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, perioeci, payer,
    moderate, biographical, constrained, regional).

% Later Greek and Roman historians such as Plutarch and Xenophon who document the Spartan system from outside. They evaluate its claims of divine origin and immutability against observed practice and demographic outcomes.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, external_historians, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lycurgan_laws__sacral_fidelity_reading, spartan_citizens).
narrative_ontology:fixing_cost_class(lycurgan_laws__sacral_fidelity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a rigid, hierarchical military society by enforcing shared austerity, collective education, and strict legal equality among citizens while suppressing economic differentiation and internal dissent.
% TRANSFER_FUNCTION: Transfers agricultural surplus and labor from helots to Spartan citizens; transfers political exclusion and military obligation from citizens and perioeci to the ephoral state apparatus.
% ABSENT_VOICES: Helots are structurally excluded from constitutional discourse; perioeci lack political representation; dissenting Spartan citizens advocating reform are suppressed by ephors and social shame; Athenian and Theban critics are external voices excluded from Spartan deliberation.
% DISAPPEARANCE_RATIONALE: The entire Spartan social, economic, and military order is predicated on the Lycurgan code. If the constraint vanished, the agoge, syssitia, land-helot nexus, and citizen hierarchy would collapse, forcing immediate reorganization of production, politics, and identity.
% FOUNDING_PROBLEM: Internal civil disorder, inequality, and vulnerability to tyranny in early Sparta; the laws were instituted to create a stable, enduring military polity.
% FOUNDING_PROBLEM_CORROBORATION: Ancient historians Plutarch and Xenophon, writing from outside the benefiting citizen class, record that Sparta's later decline involved wealth inequality and constitutional drift, suggesting the original problem was no longer solved by the code. Modern classical scholars corroborate that the Lycurgan system as described was likely a retrospective idealization masking adaptation and eventual obsolescence.
narrative_ontology:disappearance_verdict(lycurgan_laws__sacral_fidelity_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__sacral_fidelity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__sacral_fidelity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(lycurgan_laws__sacral_fidelity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__sacral_fidelity_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__sacral_fidelity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lycurgan_laws__sacral_fidelity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, ExtMetricName, E),
    domain_priors:suppression_score(lycurgan_laws__sacral_fidelity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(lycurgan_laws__sacral_fidelity_reading),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(lycurgan_laws__sacral_fidelity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the system transfers nearly all productive surplus and labor from helots to citizens while strictly regulating citizen life. Suppression is very high (0.88) because the constraint depends on state terror (Krypteia), social shame, and legal violence to maintain the fixed hierarchy. Theater ratio rises to 0.60 as the system ages, indicating increasing performative maintenance of 'Lycurgan' customs that no longer serve their original function. Accessibility collapse is high (0.85) because neither helots nor citizens have alternative social or legal frameworks within the Spartan domain. Resistance is moderate (0.60) reflecting chronic helot revolts and the eventual systemic failure that the reading attributes to vice rather than design.
 *
 * PERSPECTIVAL GAP:
 *   The ephor and citizen seats compute a low directionality (beneficiary/agenda-setter) and may classify the constraint as Mountain or Rope â genuine coordination preserving order. The helot seat computes a very high directionality (full target) and would classify it as Snare â pure extraction with no coordination benefit. The perioeci sit between, experiencing constrained exclusion. The engine captures this divergence from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Spartan citizens are declared beneficiaries because the laws grant them land, political participation, and status maintained by helot subordination. Helots and perioeci are declared victims because they bear the costs: helots through violent enslavement and labor extraction, perioeci through political exclusion and military subordination. The citizens' identity_locked exit amplifies their trapped condition despite their beneficiary status, creating a dual structural relationship the engine resolves through scope and power.
 *
 * MANDATROPHY ANALYSIS:
 *   The sacral reading claims the founding problem (internal disorder and tyranny) is solved permanently by divine law. The R5 genealogy interview records founding_problem_status as dead, because the system persisted long after its military and demographic conditions changed. This mismatch prevents classifying the constraint as mere coordination (Rope) or natural law (Mountain) without acknowledging the accumulated extraction and theatrical maintenance that characterize its later phase. The classification captures the drift from sacred coordination to inertial performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fsm_naturality_ambiguity,
    'Is the Lycurgan constitutional immutability a genuine divine natural law, or a constructed myth benefiting the Spartan citizen class?',
    'Archaeological and textual evidence of legal evolution or adaptation; detection of retroactive myth-making in the Plutarchan tradition.',
    'If constructed, reclassifies from mountain to tangled_rope or snare, exposing the extraction embedded in the ''natural'' order.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fsm_naturality_ambiguity, conceptual, 'Natural-law vs constructed ambiguity for FSM').

omega_variable(
    decline_attribution,
    'Was Spartan decline caused by external pressures and citizen vice (as this reading claims), or by the demographic and systemic brittleness of the unrevisable code?',
    'Demographic analysis of citizen numbers vs helot population; comparative institutional analysis of constitutional adaptability.',
    'If systemic brittleness, the sacral reading''s axiom of citizen-failure is falsified and the authority of the immutable kernel erodes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decline_attribution, empirical, 'Attribution of Spartan decline to external vs internal systemic causes').

omega_variable(
    sacral_kernel_contest,
    'Does the sacral fidelity reading represent a coherent historical Spartan self-understanding, or a retrospective Hellenistic-Roman idealization?',
    'Textual criticism of the Lycurgus biographical tradition; archaeological correlation with archaic Spartan material culture.',
    'If retrospective, the constraint''s referent may be an invented tradition, altering the epistemic status of its metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacral_kernel_contest, conceptual, 'Whether the sacral reading is historical belief or invented tradition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__sacral_fidelity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t0, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(lycu_tr_t20, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(lycu_tr_t40, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(lycu_tr_t60, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 60, 0.5).
narrative_ontology:measurement(lycu_tr_t80, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 80, 0.55).
narrative_ontology:measurement(lycu_tr_t100, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 100, 0.6).

% Extraction over time
narrative_ontology:measurement(lycu_be_t0, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(lycu_be_t20, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(lycu_be_t40, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(lycu_be_t60, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 60, 0.72).
narrative_ontology:measurement(lycu_be_t80, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 80, 0.75).
narrative_ontology:measurement(lycu_be_t100, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 100, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(lycu_su_t0, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(lycu_su_t20, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(lycu_su_t40, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 40, 0.8).
narrative_ontology:measurement(lycu_su_t60, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 60, 0.85).
narrative_ontology:measurement(lycu_su_t80, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 80, 0.85).
narrative_ontology:measurement(lycu_su_t100, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 100, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__sacral_fidelity_reading, identity_coordination).
narrative_ontology:affects_constraint(lycurgan_laws__sacral_fidelity_reading, demographic_trap_reading).
narrative_ontology:affects_constraint(lycurgan_laws__sacral_fidelity_reading, adaptive_fiction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the lycurgan_laws kernel, decomposed per the epsilon-invariance principle because the natural-language label 'Lycurgan laws' conflates structurally distinct claims: sacral immutability, demographic brittleness, and adaptive fiction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
