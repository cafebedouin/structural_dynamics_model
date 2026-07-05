% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__first_held_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__first_held_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: electronic_money_emergence__first_held_reading
 *   human_readable: First Institutional Holding as the Origin of Digital Money
 *   domain: economic_history/monetary_theory
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel about when
 *   digital money 'emerged.' Under the first_held_reading, emergence is
 *   treated as a discrete institutional event: the moment some identifiable
 *   institutional bearer held dematerialized currency in a form the
 *   regulatory and accounting apparatus could distinguish from physical
 *   notes. This reading has real coordination value — it gives regulators,
 *   clearing systems, and monetary statisticians a citable threshold for
 *   reserve rules and aggregate reporting — but it also concentrates
 *   legitimacy and historiographical authority on whichever institution
 *   happened to cross the ledger threshold first, and it renders
 *   cash-dependent and informal populations statistically invisible to the
 *   very narrative claiming to describe the birth of the money they don't
 *   use. This is a tangled_rope: genuine coordination function (a shared,
 *   auditable threshold for regulatory purposes) bundled with asymmetric
 *   extraction (first-mover legitimacy capture, late-adopter penalty, and
 *   exclusion of populations outside the ledger). It sits at moderate rather
 *   than high extraction because the coordination function is substantial and
 *   the extraction is more about narrative/legitimacy capture than direct
 *   financial rent.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__first_held_reading, 0.28).
domain_priors:suppression_score(electronic_money_emergence__first_held_reading, 0.22).
domain_priors:theater_ratio(electronic_money_emergence__first_held_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, resistance, 0.31).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__first_held_reading, tangled_rope).
narrative_ontology:human_readable(electronic_money_emergence__first_held_reading, "First Institutional Holding as the Origin of Digital Money").
narrative_ontology:topic_domain(electronic_money_emergence__first_held_reading, "economic_history/monetary_theory").

domain_priors:requires_active_enforcement(electronic_money_emergence__first_held_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__first_held_reading, 'feacb9dd-77e2-4592-ad1e-d91d1ae784d2').
narrative_ontology:cs_kernel_codification('feacb9dd-77e2-4592-ad1e-d91d1ae784d2', distributed).
narrative_ontology:cs_authority_grounding('feacb9dd-77e2-4592-ad1e-d91d1ae784d2', practice).
narrative_ontology:cs_interpretation_layer_present('feacb9dd-77e2-4592-ad1e-d91d1ae784d2').
narrative_ontology:cs_reading_relation('feacb9dd-77e2-4592-ad1e-d91d1ae784d2', electronic_money_emergence__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('feacb9dd-77e2-4592-ad1e-d91d1ae784d2', electronic_money_emergence__m4_m5_collapse_reading, influences).
narrative_ontology:cs_axiom('feacb9dd-77e2-4592-ad1e-d91d1ae784d2', foundational, institutional_ledger_holding_is_the_ontological_marker).
narrative_ontology:cs_axiom_status(institutional_ledger_holding_is_the_ontological_marker, holdable).
narrative_ontology:cs_axiom_grounding('feacb9dd-77e2-4592-ad1e-d91d1ae784d2', institutional_ledger_holding_is_the_ontological_marker, conventional).
narrative_ontology:cs_axiom('feacb9dd-77e2-4592-ad1e-d91d1ae784d2', secondary, discrete_dateable_events_are_epistemically_preferable_to_diffuse_processes).
narrative_ontology:cs_axiom_status(discrete_dateable_events_are_epistemically_preferable_to_diffuse_processes, holdable).
narrative_ontology:cs_axiom_grounding('feacb9dd-77e2-4592-ad1e-d91d1ae784d2', discrete_dateable_events_are_epistemically_preferable_to_diffuse_processes, instrumental).
narrative_ontology:cs_reference_frame('feacb9dd-77e2-4592-ad1e-d91d1ae784d2', regulatory_bright_line_threshold_framework).
narrative_ontology:cs_drift_state('feacb9dd-77e2-4592-ad1e-d91d1ae784d2', post_digital_payment_proliferation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('feacb9dd-77e2-4592-ad1e-d91d1ae784d2', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__first_held_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, clearing_banks_first_movers).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, central_bank_settlement_authority).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, monetary_historians_discrete_event_school).
narrative_ontology:constraint_victim(electronic_money_emergence__first_held_reading, cash_dependent_populations).
narrative_ontology:constraint_victim(electronic_money_emergence__first_held_reading, smaller_regional_banks_late_adopters).
narrative_ontology:constraint_victim(electronic_money_emergence__first_held_reading, informal_sector_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Held the first dematerialized ledger balances recognized as distinct from physical notes and used that priority to shape settlement rules, reserve treatment, and interbank protocols around their own infrastructure. Their claim to have been 'first' anchors regulatory legitimacy and gives them a durable seat at the standard-setting table.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, clearing_banks_first_movers, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(electronic_money_emergence__first_held_reading, clearing_banks_first_movers, agenda_setter).

% Declares the legal and regulatory threshold at which a bearer's holding of dematerialized currency counts as 'electronic money' for statistical and supervisory purposes. This declaration is what makes the discrete-event reading legible at all; the authority both administers and benefits from having a bright line to enforce.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, central_bank_settlement_authority, agenda_setter,
    institutional, civilizational, analytical, national).

% Builds academic and institutional careers on identifying and dating the specific ledger-holding event, treating it as a clean ontological transition. Benefits from the discrete-event framing being treated as settled rather than as one of three contested readings.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, monetary_historians_discrete_event_school, beneficiary,
    moderate, generational, mobile, global).

% Adopted dematerialized balance-holding after the first movers and consequently found themselves treated as laggards under a legitimacy structure that privileges chronological priority. Compliance costs and reputational deficits accrue to being 'later,' even though the underlying technical capability arrived within a narrow window.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, smaller_regional_banks_late_adopters, payer,
    moderate, biographical, constrained, regional).

% Excluded from the moment being dated as 'emergence' entirely — the first-held event happened inside institutional ledgers they never touched. As policy and infrastructure investment orient around the discrete-event narrative, physical-cash infrastructure atrophies, degrading their position without their having been party to the event that supposedly justifies the shift.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, cash_dependent_populations, payer,
    powerless, biographical, trapped, national).

% Operate largely outside the ledger systems whose first entries define the emergence event. Statistical and regulatory categories built around the discrete-event threshold render their economic activity invisible to the very system claiming to have measured the birth of digital money.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, informal_sector_workers, payer,
    powerless, biographical, trapped, national).

% Argue emergence happened earlier and diffusely — when the conceptual and technical possibility became socially thinkable — and are structurally excluded from the discrete-event framing's institutional legitimacy because their reading resists a bright-line date that regulators and historians can cite.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, becoming_thinkable_reading_proponents, excluded,
    moderate, generational, analytical, global).

% Argue the entire 'emergence' is a statistical artifact of how M4/M5 monetary aggregates were defined, not an ontological event. Excluded from the first-held framing's authority because their reading undermines the claim that there was any discrete event to date.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, m4_m5_collapse_reading_proponents, excluded,
    moderate, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishing a discrete, legally and statistically recognizable threshold for 'electronic money' lets regulators, central banks, and clearing systems coordinate reserve requirements, settlement finality rules, and monetary aggregate reporting around a shared reference point.
% TRANSFER_FUNCTION: Moves regulatory legitimacy, historical priority, and standard-setting authority toward the institution that held the first recognized dematerialized balance, and moves infrastructure investment and statistical visibility away from cash-dependent and informal-sector populations whose transactions never entered that ledger.
% ABSENT_VOICES: Proponents of the became_thinkable and m4_m5_collapse readings are not consulted in regulatory or historiographical accounts that treat the first-held event as settled fact; cash-dependent and informal-sector populations, whose economic lives the ledger event never touched, have no voice in a narrative built entirely from institutional ledger entries.
% DISAPPEARANCE_RATIONALE: If the first-held dating were abandoned as the canonical emergence marker, first-mover clearing banks would lose a specific claim to regulatory priority and historians would lose a clean periodization — the world of settlement rules and reserve categories built atop that date would need re-justification. But proponents of the other two readings argue the underlying monetary reality (statistical aggregates, actual payment behavior) would be unchanged, since the discrete-event dating was never load-bearing for the technology itself, only for the narrative and the institutions that cite it.
% FOUNDING_PROBLEM: Regulators and monetary statisticians needed a defensible, auditable threshold to distinguish 'electronic money' from physical currency for the purposes of reserve requirements, deposit insurance categorization, and monetary aggregate reporting (M1/M2/M3/M4).
% FOUNDING_PROBLEM_CORROBORATION: Central bank statisticians and first-mover clearing institutions attest the discrete threshold remains operationally necessary for reserve and reporting purposes. Independent monetary historians outside the discrete-event school, along with critics of the M4/M5 aggregate construction, attest that the 'first held' framing retrofits a clean origin story onto what was in practice a diffuse, decades-long technical and social transition — no source outside the institutions that benefit from the bright-line dating corroborates that a genuine singular event, rather than a convenient administrative marker, occurred.
narrative_ontology:disappearance_verdict(electronic_money_emergence__first_held_reading, contested).
narrative_ontology:founding_problem_status(electronic_money_emergence__first_held_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__first_held_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(electronic_money_emergence__first_held_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__first_held_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__first_held_reading_tests).
:- end_tests(electronic_money_emergence__first_held_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.28 at interval end) because the primary cost imposed is legitimacy and narrative capture rather than direct financial extraction — first movers gain standard-setting authority and historiographical priority, not a rent stream. Suppression is low-moderate (0.22) because the discrete-event framing is maintained through institutional citation practice and statistical convention rather than coercive enforcement, though central bank statistical categories do actively suppress alternative periodizations by making them administratively illegible. Theater ratio rises to 0.34 as the discrete-event narrative increasingly performs certainty about a specific date that the underlying historical record does not clearly support — anniversary commemorations and 'first electronic money' citations proliferate even as historians increasingly acknowledge the diffuse, contested nature of the actual transition.
 *
 * DIRECTIONALITY LOGIC:
 *   First-mover clearing banks and the central bank settlement authority sit near the beneficiary end: they gain regulatory legitimacy, standard-setting authority, and a citable origin story that serves ongoing institutional interests. The discrete-event historian school also benefits, having built interpretive careers on dating a specific transition. Late-adopter regional banks pay a reputational and compliance-timing cost for arriving after the recognized threshold. Cash-dependent populations and informal-sector workers pay the highest structural cost: they are trapped outside the ledger systems whose first entry defines 'emergence,' and infrastructure investment increasingly follows the discrete-event narrative away from the physical-cash rails they depend on.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a defensible statistical/regulatory threshold for reserve and reporting purposes — remains partially live (central banks still need SOME operational threshold), which is why founding_problem_status is 'contested' rather than 'dead.' The risk of mandatrophy here is narrower than typical: it is not that the coordination function has vanished, but that the SPECIFIC discrete-event framing has ossified into treating an administratively convenient marker as an ontological fact, displacing the two sibling readings that would treat emergence as either gradual (became_thinkable) or artifactual (m4_m5_collapse). Classifying this as tangled_rope rather than mountain or rope prevents the discrete-event narrative from being mistaken for settled natural history when it is one contested reading among three, maintained partly through institutional citation inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discrete_event_vs_gradual_diffusion,
    'Was there a genuine, identifiable discrete moment when digital money emerged, or is the ''first held'' framing an administratively convenient fiction imposed on a gradual, multi-decade diffusion process?',
    'Historical reconstruction of parallel institutional ledger systems across multiple jurisdictions in the relevant period; if multiple institutions independently crossed comparable technical thresholds within a narrow window with no clear causal priority, the discrete-event framing loses force relative to the became_thinkable_reading.',
    'If diffusion was genuinely gradual and multi-centered, the first_held_reading''s legitimacy-capture function (crediting one institution as ''first'') is exposed as largely arbitrary, strengthening the case that this constraint''s extractive component (first-mover legitimacy) rides on a constructed rather than discovered fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discrete_event_vs_gradual_diffusion, empirical, 'Whether a genuine discrete emergence event occurred or diffusion was gradual and multi-centered.').

omega_variable(
    measurement_artifact_vs_real_transition,
    'Did the M4/M5 statistical categorization create the appearance of an emergence event that has no independent ontological reality, per the m4_m5_collapse_reading?',
    'Compare pre-categorization institutional records (internal bank ledgers, correspondence, technical documentation) against the dates statistical aggregates began treating electronic balances as a distinct category; a mismatch would support the measurement-artifact reading.',
    'If the emergence event is substantially a statistical construction, the first_held_reading''s claim to describe an ''ontological transition'' is undermined, and its coordination value should be understood as purely administrative rather than descriptive of monetary reality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(measurement_artifact_vs_real_transition, conceptual, 'Whether the discrete emergence event is a real transition or a retroactive statistical artifact.').

omega_variable(
    beneficiary_capture_of_periodization,
    'Is the discrete-event periodization maintained because it serves genuine regulatory coordination needs, or because it serves the legitimacy interests of the institutions and historians who benefit from being able to cite a specific ''first'' event?',
    'Examine whether regulatory frameworks that abandoned bright-line discrete thresholds (in favor of continuous or threshold-free approaches) experienced measurable coordination failures, versus frameworks that retained them primarily for citation and legitimacy purposes.',
    'If coordination needs could be met without crediting a specific first-mover institution, the beneficiary structure identified in this story is closer to pure extraction of legitimacy than genuine coordination necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_of_periodization, conceptual, 'Whether the discrete threshold is coordination-necessary or a legitimacy-capture convenience.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__first_held_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_tr_t0, electronic_money_emergence__first_held_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(elec_tr_t8, electronic_money_emergence__first_held_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(elec_tr_t16, electronic_money_emergence__first_held_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(elec_tr_t24, electronic_money_emergence__first_held_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement(elec_tr_t32, electronic_money_emergence__first_held_reading, theater_ratio, 32, 0.31).
narrative_ontology:measurement(elec_tr_t40, electronic_money_emergence__first_held_reading, theater_ratio, 40, 0.34).

% Extraction over time
narrative_ontology:measurement(elec_be_t0, electronic_money_emergence__first_held_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(elec_be_t8, electronic_money_emergence__first_held_reading, base_extractiveness, 8, 0.17).
narrative_ontology:measurement(elec_be_t16, electronic_money_emergence__first_held_reading, base_extractiveness, 16, 0.21).
narrative_ontology:measurement(elec_be_t24, electronic_money_emergence__first_held_reading, base_extractiveness, 24, 0.24).
narrative_ontology:measurement(elec_be_t32, electronic_money_emergence__first_held_reading, base_extractiveness, 32, 0.26).
narrative_ontology:measurement(elec_be_t40, electronic_money_emergence__first_held_reading, base_extractiveness, 40, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(elec_su_t0, electronic_money_emergence__first_held_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(elec_su_t8, electronic_money_emergence__first_held_reading, suppression_requirement, 8, 0.13).
narrative_ontology:measurement(elec_su_t16, electronic_money_emergence__first_held_reading, suppression_requirement, 16, 0.16).
narrative_ontology:measurement(elec_su_t24, electronic_money_emergence__first_held_reading, suppression_requirement, 24, 0.18).
narrative_ontology:measurement(elec_su_t32, electronic_money_emergence__first_held_reading, suppression_requirement, 32, 0.2).
narrative_ontology:measurement(elec_su_t40, electronic_money_emergence__first_held_reading, suppression_requirement, 40, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__first_held_reading, information_standard).
narrative_ontology:boltzmann_floor_override(electronic_money_emergence__first_held_reading, 0.05).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, electronic_money_emergence__became_thinkable_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, electronic_money_emergence__m4_m5_collapse_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the electronic_money_emergence kernel. first_held_reading treats emergence as a discrete, dateable institutional event and is authored here as tangled_rope (genuine regulatory coordination value bundled with first-mover legitimacy capture and cash-population exclusion). became_thinkable_reading treats emergence as gradual and socially diffuse, prior to any institutional measurement — expected to show lower extraction and a weaker enforcement story since no institution can claim priority over a diffuse process. m4_m5_collapse_reading treats the entire emergence narrative as a statistical artifact of monetary aggregate categorization — expected to surface highest contestation of the coordination claim itself, since it argues the category was retroactively constructed rather than discovered. The three stories are not competing measurements of one constraint; each is ε-invariant on its own terms and linked here to preserve the kernel-contest structure without merging their classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
