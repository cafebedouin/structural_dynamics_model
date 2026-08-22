% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__procedural_integrity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__procedural_integrity_reading, []).

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
 *   constraint_id: refugee_convention_text__procedural_integrity_reading
 *   human_readable: 1951 Refugee Convention — Procedural Integrity Reading
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   This story instantiates the procedural_integrity_reading of the 1951
 *   Refugee Convention kernel: the Convention is read as a process
 *   specification — fair, individualized assessment — rather than as either a
 *   substantive humanitarian floor (expansive_humanitarian_reading) or a
 *   minimal sovereignty check (restrictive_sovereignty_reading). Under this
 *   reading, the flexibility of the protection threshold is a feature, not a
 *   defect: states may define 'well-founded fear' and 'particular social
 *   group' narrowly or broadly, so long as whatever definition they use is
 *   applied through a fair individualized process. This decouples legitimacy
 *   from outcome, which is what generates the tangled-rope structure —
 *   genuine coordination (a verifiable compliance standard usable by courts
 *   without international adjudication) riding alongside asymmetric
 *   extraction (procedural compliance becomes achievable even where
 *   substantive access is functionally denied, e.g., offshore processing with
 *   nominal hearings). The rising extractiveness and suppression-requirement
 *   series reflect the documented drift from post-war individualized status
 *   determination toward mass expedited and offshore procedures that satisfy
 *   the letter of process while narrowing its practical reach.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__procedural_integrity_reading, 0.52).
domain_priors:suppression_score(refugee_convention_text__procedural_integrity_reading, 0.58).
domain_priors:theater_ratio(refugee_convention_text__procedural_integrity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__procedural_integrity_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__procedural_integrity_reading, "1951 Refugee Convention — Procedural Integrity Reading").
narrative_ontology:topic_domain(refugee_convention_text__procedural_integrity_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__procedural_integrity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__procedural_integrity_reading, 'eaf7a057-1dc2-4a67-adff-1edfc64d0a89').
narrative_ontology:cs_kernel_codification('eaf7a057-1dc2-4a67-adff-1edfc64d0a89', fixed_text).
narrative_ontology:cs_authority_grounding('eaf7a057-1dc2-4a67-adff-1edfc64d0a89', practice).
narrative_ontology:cs_interpretation_layer_present('eaf7a057-1dc2-4a67-adff-1edfc64d0a89').
narrative_ontology:cs_reading_relation('eaf7a057-1dc2-4a67-adff-1edfc64d0a89', refugee_convention_text__restrictive_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('eaf7a057-1dc2-4a67-adff-1edfc64d0a89', refugee_convention_text__expansive_humanitarian_reading, influences).
narrative_ontology:cs_axiom('eaf7a057-1dc2-4a67-adff-1edfc64d0a89', foundational, process_integrity_is_the_nonnegotiable_element).
narrative_ontology:cs_axiom_status(process_integrity_is_the_nonnegotiable_element, holdable).
narrative_ontology:cs_axiom_grounding('eaf7a057-1dc2-4a67-adff-1edfc64d0a89', process_integrity_is_the_nonnegotiable_element, conventional).
narrative_ontology:cs_axiom('eaf7a057-1dc2-4a67-adff-1edfc64d0a89', secondary, protection_threshold_is_state_adjustable).
narrative_ontology:cs_axiom_status(protection_threshold_is_state_adjustable, holdable).
narrative_ontology:cs_axiom_grounding('eaf7a057-1dc2-4a67-adff-1edfc64d0a89', protection_threshold_is_state_adjustable, instrumental).
narrative_ontology:cs_reference_frame('eaf7a057-1dc2-4a67-adff-1edfc64d0a89', individualized_status_determination_norm).
narrative_ontology:cs_drift_state('eaf7a057-1dc2-4a67-adff-1edfc64d0a89', contemporary_mass_processing_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('eaf7a057-1dc2-4a67-adff-1edfc64d0a89', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__procedural_integrity_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, receiving_states_asylum_bureaucracies).
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, procedural_law_practitioners).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, asylum_seekers_in_offshore_processing).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, unrepresented_claimants).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, claimants_in_expedited_removal_systems).
narrative_ontology:constraint_vindicates(refugee_convention_text__procedural_integrity_reading, due_process_as_sufficient_legitimacy_condition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and administers status-determination procedures — interview protocols, appeal tiers, evidentiary standards, timelines. Can satisfy the Convention's obligations entirely through procedural compliance (a hearing was held, a lawyer was offered, an appeal window existed) regardless of how many claims are substantively rejected. Gains legal cover and reduced adjudicatory liability by treating the Convention as a process specification rather than an outcome mandate.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, receiving_states_asylum_bureaucracies, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__procedural_integrity_reading, receiving_states_asylum_bureaucracies, beneficiary).

% Immigration lawyers, tribunal members, and compliance auditors whose professional function and billable relevance depend on the existence of elaborate procedural requirements. A procedure-centric reading of the Convention generates demand for their expertise regardless of whether outcomes improve for claimants.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, procedural_law_practitioners, beneficiary,
    organized, biographical, mobile, national).

% Held in third-country processing centers where formal hearing structures exist on paper but access to counsel, evidence-gathering, interpreters, and appeal is severely constrained by geography and detention. Under this reading, offshore processing is fully lawful provided the procedural steps are nominally present — the physical and informational barriers to actually using those steps are not counted against compliance.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, asylum_seekers_in_offshore_processing, payer,
    powerless, immediate, trapped, regional).

% Face full adversarial hearings without counsel, against government representatives, under rules of evidence they do not understand. The procedure was 'fair' in the sense of being open to all comers, but functionally unequal in outcome. Their only lever is procedural appeal — attacking process defects rather than substantive merits — which favors claimants who can afford to litigate procedure.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, unrepresented_claimants, payer,
    powerless, immediate, trapped, national).

% Processed under compressed timelines justified as 'streamlined procedure.' The individualized-assessment requirement is satisfied by a short interview; the claimant bears the cost of demonstrating why more time or evidence-gathering was needed, which is itself procedurally difficult under time pressure.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, claimants_in_expedited_removal_systems, payer,
    powerless, immediate, trapped, national).

% Monitors state compliance and issues guidance on what 'fair individualized assessment' requires, but has no binding enforcement power over states that adopt minimal procedural compliance. Its interpretive guidance is persuasive, not authoritative, so it can document procedural inadequacy without being able to compel remedy — effectively excluded from the enforcement loop even as it observes the whole structure.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, unhcr_supervisory_authority, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__procedural_integrity_reading, unhcr_supervisory_authority, excluded).

% Review whether state procedures met the process-integrity bar. Under this reading, judicial review is the primary enforcement mechanism — courts assess whether a fair process occurred, not whether the outcome was substantively correct, which structurally privileges procedural challenge over merits challenge.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, domestic_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(refugee_convention_text__procedural_integrity_reading, receiving_states_asylum_bureaucracies).
narrative_ontology:fixing_cost_class(refugee_convention_text__procedural_integrity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a verifiable, auditable standard — did a fair, individualized process occur — that domestic courts, UNHCR monitors, and states themselves can assess without requiring international bodies to relitigate the substantive merits of every asylum claim. This solves the real coordination problem of how a treaty with no supranational court can be checked at all.
% TRANSFER_FUNCTION: Moves legitimacy and legal insulation to states and adjudicators who can demonstrate procedural compliance, and moves the burden of demonstrating substantive merit onto claimants who must navigate that procedure — often without the resources, language access, or time the procedure formally assumes they have.
% ABSENT_VOICES: Asylum seekers themselves are structurally absent from the design of the procedures that govern them; offshore-detained claimants in particular have no practical channel to contest procedural adequacy from within the system that processes them. UNHCR raises these voices in monitoring reports but cannot compel remedy.
% DISAPPEARANCE_RATIONALE: States would argue their sovereign discretion over admission is unchanged if the procedural-integrity reading vanished — they would simply adopt a different justificatory frame (sovereignty-floor or humanitarian-mandate). Practitioners and UNHCR would argue that removing procedural-integrity review specifically would strip the only currently operative check on offshore and expedited-removal practices, since substantive merits review is rarely available as an alternative in the same fora.
% FOUNDING_PROBLEM: The 1951 Convention was drafted without a supranational court with compulsory jurisdiction over individual claims; states needed a way to demonstrate good-faith compliance that domestic courts and international monitors could actually verify without adjudicating the underlying persecution claim themselves.
% FOUNDING_PROBLEM_CORROBORATION: UNHCR executive committee conclusions and domestic judicial review bodies (outside the states being reviewed) attest that the verification problem remains live and that procedural review is currently the primary functioning check; academic administrative-law scholars and several national ombudsman offices corroborate that procedural compliance has, in specific jurisdictions, become decoupled from any meaningful substantive check, which the states administering the procedures do not themselves acknowledge as a problem.
narrative_ontology:disappearance_verdict(refugee_convention_text__procedural_integrity_reading, contested).
narrative_ontology:founding_problem_status(refugee_convention_text__procedural_integrity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__procedural_integrity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(refugee_convention_text__procedural_integrity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__procedural_integrity_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__procedural_integrity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(refugee_convention_text__procedural_integrity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(refugee_convention_text__procedural_integrity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) and suppression (0.58) are moderate-high but not extreme: the coordination function (a verifiable standard) is real and was the Convention's design solution to the absence of compulsory international jurisdiction, so this is not a pure snare. Theater ratio (0.42) is elevated and rising over the interval because an increasing share of 'procedural' activity — brief interviews, formal but practically inaccessible appeal windows — performs compliance without delivering the individualized assessment the standard nominally requires. Accessibility collapse (0.45) is moderate: alternatives (substantive merits review, humanitarian discretion) are narrowed but not eliminated by this reading, since it explicitly holds that process integrity is non-negotiable even while protection thresholds flex. Resistance (0.55) reflects active pushback from UNHCR, domestic courts in several jurisdictions, and advocacy litigation attacking procedural adequacy in offshore and expedited systems.
 *
 * DIRECTIONALITY LOGIC:
 *   Receiving-state bureaucracies and procedural-law practitioners sit near the beneficiary end: they gain legal insulation and professional relevance from a compliance standard measured in process steps rather than substantive outcomes. Asylum seekers in offshore processing, unrepresented claimants, and claimants in expedited removal sit near the target end: they are trapped, powerless, and immediate-horizon agents for whom the gap between nominal and functional process access is the entire lived difference in outcome. UNHCR and domestic courts are analytical/institutional observers with no direct extraction exposure, but UNHCR's practical exclusion from enforcement (persuasive-only authority) is itself a structural feature this reading depends on to remain flexible for states.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — verifying good-faith compliance without a supranational court — remains partially live (there is still no compulsory international adjudicator), which prevents a clean mandatrophy verdict. But the founding_problem_status is authored as contested because the specific gap this reading is now used to bridge (satisfying process while defeating practical access) was not the gap the standard was built to bridge in 1951, when individualized case-by-case determination was the norm and volumes were low. The mismatch between founding_problem_status=contested and disappearance_verdict=contested itself signals a structure worth monitoring for zombie-mandate drift rather than declaring it outright.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    process_outcome_decoupling,
    'Can procedural fairness be meaningfully assessed as satisfied when the practical conditions for using the procedure (counsel access, evidence-gathering time, geographic access) are absent, or does process integrity necessarily import a substantive access floor?',
    'Comparative empirical study of grant-rate and appeal-success divergence between represented and unrepresented claimants, and between onshore and offshore processing, holding formal procedural steps constant.',
    'If access conditions are found to be part of ''process integrity'' properly understood, this reading collapses much of its distance from the expansive_humanitarian_reading; if access conditions are genuinely severable from process fairness, the reading''s tangled-rope classification is robust as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(process_outcome_decoupling, conceptual, 'Whether procedural fairness requires a practical-access floor or is satisfiable by formal steps alone.').

omega_variable(
    reading_selection_provenance,
    'Which institutional actors actually select the procedural_integrity_reading over its siblings, and does that selection pattern itself indicate capture?',
    'Trace which reading is invoked in state legal defenses versus UNHCR guidance versus domestic court holdings across a sample of offshore-processing and expedited-removal litigation; a pattern where states consistently invoke procedural_integrity_reading specifically to defend practices UNHCR characterizes as substantively deficient would corroborate a capture reading.',
    'If states systematically select this reading precisely where substantive review would go against them, the reading''s tangled-rope coordination function is genuine but its selection pattern is itself extractive — a second-order finding not captured by ε alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_provenance, empirical, 'Whether selection of this reading over siblings correlates with defending otherwise-indefensible substantive outcomes.').

omega_variable(
    offshore_processing_naturalness,
    'Is offshore processing with full procedural guarantees a genuinely permissible instantiation of this reading, or does spatial displacement itself defeat the individualized-assessment requirement regardless of formal procedural completeness?',
    'Adjudicated case law and comparative-jurisdiction analysis of whether courts applying this reading have ever found offshore procedures per se inadequate independent of specific procedural defects.',
    'If courts under this reading have never invalidated offshore processing per se, the reading''s stated tolerance for offshore processing (given full guarantees) is confirmed as structurally load-bearing rather than aspirational; if courts have found spatial displacement itself defeats individualized assessment, the reading''s stated delta is narrower in practice than authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(offshore_processing_naturalness, empirical, 'Whether spatial displacement (offshore processing) is compatible with this reading''s own process-integrity standard in adjudicated practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__procedural_integrity_reading, 1951, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t1951, refugee_convention_text__procedural_integrity_reading, theater_ratio, 1951, 0.15).
narrative_ontology:measurement(refu_tr_t1980, refugee_convention_text__procedural_integrity_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(refu_tr_t2001, refugee_convention_text__procedural_integrity_reading, theater_ratio, 2001, 0.28).
narrative_ontology:measurement(refu_tr_t2013, refugee_convention_text__procedural_integrity_reading, theater_ratio, 2013, 0.35).
narrative_ontology:measurement(refu_tr_t2019, refugee_convention_text__procedural_integrity_reading, theater_ratio, 2019, 0.39).
narrative_ontology:measurement(refu_tr_t2025, refugee_convention_text__procedural_integrity_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(refu_be_t1951, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 1951, 0.28).
narrative_ontology:measurement(refu_be_t1980, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 1980, 0.33).
narrative_ontology:measurement(refu_be_t2001, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 2001, 0.4).
narrative_ontology:measurement(refu_be_t2013, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 2013, 0.46).
narrative_ontology:measurement(refu_be_t2019, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 2019, 0.49).
narrative_ontology:measurement(refu_be_t2025, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 2025, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t1951, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 1951, 0.3).
narrative_ontology:measurement(refu_su_t1980, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 1980, 0.35).
narrative_ontology:measurement(refu_su_t2001, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 2001, 0.44).
narrative_ontology:measurement(refu_su_t2013, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 2013, 0.5).
narrative_ontology:measurement(refu_su_t2019, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 2019, 0.55).
narrative_ontology:measurement(refu_su_t2025, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__procedural_integrity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(refugee_convention_text__procedural_integrity_reading, 0.1).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, restrictive_sovereignty_reading).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, expansive_humanitarian_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the refugee_convention_text kernel. procedural_integrity_reading occupies a structural middle position: it shares the restrictive_sovereignty_reading's tolerance for narrow substantive definitions but shares the expansive_humanitarian_reading's insistence that some element (here, process rather than substantive threshold) is non-negotiable. Each reading is authored with its own ε, its own beneficiary/victim structure, and its own claimed_type; none averages over the others. Network edges here mark contest-kernel siblinghood, not causal dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
