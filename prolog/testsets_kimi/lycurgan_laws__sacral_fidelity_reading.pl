% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__sacral_fidelity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Lycurgan Laws as Sacred Immutable Divine Ordinance
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   The sacral_fidelity_reading treats the Lycurgan constitutional order as a
 *   fixed divine ordinance delivered through the Delphic oracle and the
 *   lawgiver Lycurgus. It claims mountain status: the laws are naturalized as
 *   uncreated, immutable, and beneficial regardless of enforcement. The
 *   constraint governs Spartan citizenship, land tenure, the agoge, and helot
 *   status, all held in permanent fixity. This story authors the reading's
 *   own mountain claim while recording structural data that enables the
 *   engine to compute the divergence between that claim and the constraint's
 *   actual operation as an enforced, beneficiary-laden system of social
 *   control.
 *
 * KEY AGENTS:
 *   - spartan_citizen_class (beneficiary/organized/constrained): collect status and land stability from immutable order
 *   - helot_population (victim/powerless/trapped): bear the extracted labor preserved by unrevisable servitude
 *   - gerousia (agenda_setter/institutional/constrained): interpret and guard the sacred laws without altering them
 *   - ephors (agenda_setter/institutional/constrained): enforce absolute adherence, including deposition of deviant kings
 *   - spartan_reformists (excluded/moderate/constrained): kings and citizens who sought revision and were suppressed
 *   - perioikoi (payer/moderate/constrained): free non-citizens permanently excluded by immutable citizenship criteria
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__sacral_fidelity_reading, 0.48).
domain_priors:suppression_score(lycurgan_laws__sacral_fidelity_reading, 0.72).
domain_priors:theater_ratio(lycurgan_laws__sacral_fidelity_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__sacral_fidelity_reading, mountain).
narrative_ontology:human_readable(lycurgan_laws__sacral_fidelity_reading, "Lycurgan Laws as Sacred Immutable Divine Ordinance").
narrative_ontology:topic_domain(lycurgan_laws__sacral_fidelity_reading, "political/constitutional").

domain_priors:emerges_naturally(lycurgan_laws__sacral_fidelity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__sacral_fidelity_reading, '9440c70e-827e-47b9-b993-97ed5e6d7604').
narrative_ontology:cs_kernel_codification('9440c70e-827e-47b9-b993-97ed5e6d7604', fixed_text).
narrative_ontology:cs_authority_grounding('9440c70e-827e-47b9-b993-97ed5e6d7604', lineage).
narrative_ontology:cs_interpretation_layer_present('9440c70e-827e-47b9-b993-97ed5e6d7604').
narrative_ontology:cs_reading_relation('9440c70e-827e-47b9-b993-97ed5e6d7604', lycurgan_laws__adaptive_fiction_reading, forecloses).
narrative_ontology:cs_reading_relation('9440c70e-827e-47b9-b993-97ed5e6d7604', lycurgan_laws__demographic_trap_reading, coexists_with).
narrative_ontology:cs_axiom('9440c70e-827e-47b9-b993-97ed5e6d7604', foundational, divine_lycurgan_origin).
narrative_ontology:cs_axiom_status(divine_lycurgan_origin, holdable).
narrative_ontology:cs_axiom_grounding('9440c70e-827e-47b9-b993-97ed5e6d7604', divine_lycurgan_origin, theological).
narrative_ontology:cs_axiom('9440c70e-827e-47b9-b993-97ed5e6d7604', foundational, constitutional_immutability_as_virtue).
narrative_ontology:cs_axiom_status(constitutional_immutability_as_virtue, holdable).
narrative_ontology:cs_axiom_grounding('9440c70e-827e-47b9-b993-97ed5e6d7604', constitutional_immutability_as_virtue, deontological).
narrative_ontology:cs_reference_frame('9440c70e-827e-47b9-b993-97ed5e6d7604', divine_legislative_perfection).
narrative_ontology:cs_drift_state('9440c70e-827e-47b9-b993-97ed5e6d7604', spartan_imperial_collapse, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('9440c70e-827e-47b9-b993-97ed5e6d7604', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__sacral_fidelity_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, spartan_citizen_class).
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, gerousia).
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, ephors).
narrative_ontology:constraint_victim(lycurgan_laws__sacral_fidelity_reading, helot_population).
narrative_ontology:constraint_victim(lycurgan_laws__sacral_fidelity_reading, perioikoi).
narrative_ontology:constraint_victim(lycurgan_laws__sacral_fidelity_reading, spartan_reformists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Full Spartan citizens who draw land, status, and syssitia equality from the immutable order. The unchangeable laws guarantee their collective dominance over helots and preserve the agoge-military way of life. Exit from the citizen body means loss of honor, land, and political existence.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, spartan_citizen_class, beneficiary,
    organized, generational, constrained, national).

% Subjugated agricultural laborers bound to Spartan masters and the land. The immutable laws permanently fix their servitude, forbidding emancipation or status revision. Their labor surplus sustains the citizen class. Physical escape is possible only at extreme risk; legal exit does not exist.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, helot_population, payer,
    powerless, immediate, trapped, local).

% Council of elders that interprets and guards the laws of Lycurgus. They derive authority from their role as custodians of the divine ordinance, adjudicating disputes and qualifying candidates for office without ever altering the legal framework itself.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, gerousia, agenda_setter,
    institutional, generational, constrained, national).

% Five elected overseers who enforce adherence to the Lycurgan regime, inspect citizen households, and may depose kings for deviation from the sacred order. They wield great power but only within the fixed constitutional frame; they cannot initiate legal revision.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, ephors, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__sacral_fidelity_reading, ephors, beneficiary).

% Citizens and kings who argued for modifying property, currency, or citizenship laws to address demographic and economic strain. Their proposals were blocked by the religious prohibition against changing the divine ordinance and by ephoric enforcement of orthodoxy.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, spartan_reformists, excluded,
    moderate, biographical, constrained, national).

% Free non-citizen inhabitants of Laconia and Messenia who engaged in trade and craft production but were permanently excluded from political rights and land ownership by the immutable citizenship criteria. Their status could not be revised upward regardless of wealth or service.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, perioikoi, payer,
    moderate, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lycurgan_laws__sacral_fidelity_reading, spartan_citizen_class).
narrative_ontology:fixing_cost_class(lycurgan_laws__sacral_fidelity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents tyranny and preserves political equality among citizens by making the constitutional order immune to personal ambition, faction, and short-term demographic change; provides a fixed, predictable framework for military and social life.
% TRANSFER_FUNCTION: Moves political autonomy and revision capacity from individuals, kings, and reformers to the sacral-legal framework; transfers the surplus labor of helots and the political exclusion of perioikoi to the maintenance of the citizen-military order.
% ABSENT_VOICES: Helots, periokoi, and reform-minded Spartans including kings who sought to modify property or citizenship laws. They were excluded from deliberation by the religious prohibition on questioning the divine ordinance and by the ephors' power to suppress deviance.
% DISAPPEARANCE_RATIONALE: If the sacral immutability vanished, the constitutional basis for Spartan distinctiveness would collapse; property, citizenship, and helot status would become revisable, the gerousia would lose its anchor of authority, and the social order would reorganize around interest-based bargaining rather than sacred fixity.
% FOUNDING_PROBLEM: Post-Messenian conquest instability, inequality of wealth threatening civic strife, and the risk of tyranny or deviation from the military-societal order required a permanent, unchangeable constitutional framework.
% FOUNDING_PROBLEM_CORROBORATION: Xenophon and Plutarch attest the founding problem from within the Greek historiographical tradition, though both write centuries later and from philo-Spartan or ambivalent seats. Modern scholars contest whether the 'founding' was a single event or a retrospective construction. No contemporary non-beneficiary attestation exists; helots left no written record.
narrative_ontology:disappearance_verdict(lycurgan_laws__sacral_fidelity_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__sacral_fidelity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__sacral_fidelity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(lycurgan_laws__sacral_fidelity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__sacral_fidelity_reading, 0.48, 'kimi-k2.6', 'none', direct).

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
 *   The sacral immutability extracts heavily from subordinate classes (helots, periokoi) and from citizen autonomy, but the reading naturalizes this as divine order. Accessibility collapse is high because within the theological frame alternatives are literally unthinkable; resistance is moderate because the religious sanction internalizes compliance. Theater rises over time: as demographic and economic pressures mounted, enforcement of the sacral frame became increasingly performative and ritualized. The measurement grid is shared across all tracked metrics so no substitution artifacts arise.
 *
 * PERSPECTIVAL GAP:
 *   From the gerousia/ephors seat the constraint is sacred trust and constitutional bulwark; from the helot/reformist seat it is an unchangeable system of domination. The citizen class experiences it as both benefit and burden â the syssitia equality is genuine coordination among citizens, but purchased with the permanent exclusion of others. The engine will compute high directionality for trapped agents and low directionality for institutional beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (citizen class, gerousia, ephors) have low directionality because the constraint subsidizes their status and authority. Victims (helots, reformists, periokoi) have high directionality because the constraint extracts their labor, autonomy, or political possibility. The gerousia and ephors sit near symmetric despite their power because they are also bound by the immutability â they cannot revise, only interpret.
 *
 * MANDATROPHY ANALYSIS:
 *   The sacral reading resists mandatrophy by attributing Spartan decline to external pressures (Thebes, Macedon) and citizen vice rather than to the constitutional design. This deflection preserves the founding problem as live and the constraint as still necessary. The divergence between a claimed mountain and rising theater/extraction metrics is exactly the false-summit pattern: a constructed system defended as natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fsm_naturality_ambiguity,
    'Is the immutability of the Lycurgan laws a genuine feature of natural or divine order, or a retrospective human construction sacralized to freeze an advantageous social allocation?',
    'Archaeological and textual analysis of the laws'' historical development; evidence of retroactive attribution to Lycurgus and Delphi.',
    'If constructed, the mountain claim is a false summit and the constraint reclassifies toward tangled rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fsm_naturality_ambiguity, empirical, 'Whether the constraint is a genuine natural law or a constructed constraint benefiting identifiable agents.').

omega_variable(
    suppression_internalization,
    'Was compliance with the sacral immutability maintained primarily through external enforcement (ephors, gerousia) or through internalized religious belief among citizens?',
    'Comparative analysis of compliance patterns under weakened enforcement (e.g., during the Spartan diaspora) versus periods of strong institutional control.',
    'If internalized, effective suppression exceeds structural measures and the constraint''s extractive hold persists even when enforcement weakens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization, conceptual, 'Structural vs internalized suppression mechanism').

omega_variable(
    founding_problem_genuineness,
    'Did the founding problem (post-Messenian instability and tyranny risk) actually exist in the form described, or was it retrospectively constructed to legitimate the constitutional freeze?',
    'Archaeological evidence of pre-Lycurgan Sparta and comparative Greek constitutional history.',
    'If the founding problem was manufactured, the coordination rationale collapses and the constraint''s persistence is pure extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_genuineness, empirical, 'Whether the founding problem was genuine or retrospective justification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__sacral_fidelity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t0, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(lycu_tr_t5, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(lycu_tr_t10, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(lycu_tr_t15, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(lycu_tr_t20, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(lycu_tr_t25, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 25, 0.44).
narrative_ontology:measurement(lycu_tr_t30, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(lycu_tr_t35, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 35, 0.52).
narrative_ontology:measurement(lycu_tr_t40, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 40, 0.6).

% Extraction over time
narrative_ontology:measurement(lycu_be_t0, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lycu_be_t5, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(lycu_be_t10, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(lycu_be_t15, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 15, 0.46).
narrative_ontology:measurement(lycu_be_t20, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(lycu_be_t25, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(lycu_be_t30, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(lycu_be_t35, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 35, 0.58).
narrative_ontology:measurement(lycu_be_t40, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 40, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(lycu_su_t0, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(lycu_su_t5, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(lycu_su_t10, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(lycu_su_t15, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(lycu_su_t20, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(lycu_su_t25, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 25, 0.78).
narrative_ontology:measurement(lycu_su_t30, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(lycu_su_t35, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 35, 0.6).
narrative_ontology:measurement(lycu_su_t40, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 40, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(lycurgan_laws__sacral_fidelity_reading, lycurgan_laws__demographic_trap_reading).
narrative_ontology:affects_constraint(lycurgan_laws__sacral_fidelity_reading, lycurgan_laws__adaptive_fiction_reading).

% DUAL FORMULATION NOTE:
% The lycurgan_laws kernel decomposes into three structurally distinct constraints because the label 'Lycurgan laws' conflates sacral immutability claims, demographic-functional analyses, and fictional-adaptive readings. Each reading carries a different epsilon, beneficiary structure, and classification. This reading (sacral_fidelity) is upstream in the sense that it was the historically dominant self-understanding of the system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
