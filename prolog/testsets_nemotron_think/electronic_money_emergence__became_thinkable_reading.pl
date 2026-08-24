% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__became_thinkable_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__became_thinkable_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: electronic_money_emergence__became_thinkable_reading
 *   human_readable: Conceptual Thinkability as Digital Money Emergence Criterion
 *   domain: economic_history/monetary_theory/technology_studies
 *
 * SUMMARY:
 *   This constraint story models the 'became thinkable' reading of the
 *   electronic_money_emergence kernel. The reading claims digital money
 *   emerged gradually when the conceptual possibility became technically and
 *   socially thinkable — a diffusion process preceding any institutional
 *   adoption by decades. The reading presents this as a natural
 *   conceptual-historical fact (Mountain claim), but it operates as a
 *   coordinating framework in monetary history that benefits the diffusionist
 *   scholarly tradition while marginalizing institutionalist and
 *   statistical-formalist alternatives. The extraction is epistemic: the
 *   framework gatekeeps what counts as legitimate periodization, transferring
 *   scholarly authority to its beneficiaries.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__became_thinkable_reading, 0.42).
domain_priors:suppression_score(electronic_money_emergence__became_thinkable_reading, 0.48).
domain_priors:theater_ratio(electronic_money_emergence__became_thinkable_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__became_thinkable_reading, mountain).
narrative_ontology:human_readable(electronic_money_emergence__became_thinkable_reading, "Conceptual Thinkability as Digital Money Emergence Criterion").
narrative_ontology:topic_domain(electronic_money_emergence__became_thinkable_reading, "economic_history/monetary_theory/technology_studies").

domain_priors:requires_active_enforcement(electronic_money_emergence__became_thinkable_reading).
domain_priors:emerges_naturally(electronic_money_emergence__became_thinkable_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__became_thinkable_reading, '1f144704-7ffa-4ade-a3cc-ac0defe293c2').
narrative_ontology:cs_kernel_codification('1f144704-7ffa-4ade-a3cc-ac0defe293c2', distributed).
narrative_ontology:cs_authority_grounding('1f144704-7ffa-4ade-a3cc-ac0defe293c2', practice).
narrative_ontology:cs_interpretation_layer_present('1f144704-7ffa-4ade-a3cc-ac0defe293c2').
narrative_ontology:cs_reading_relation('1f144704-7ffa-4ade-a3cc-ac0defe293c2', electronic_money_emergence__first_held_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f144704-7ffa-4ade-a3cc-ac0defe293c2', electronic_money_emergence__m4_m5_collapse_reading, influences).
narrative_ontology:cs_axiom('1f144704-7ffa-4ade-a3cc-ac0defe293c2', foundational, conceptual_priority_over_institutional_form).
narrative_ontology:cs_axiom_status(conceptual_priority_over_institutional_form, holdable).
narrative_ontology:cs_axiom_grounding('1f144704-7ffa-4ade-a3cc-ac0defe293c2', conceptual_priority_over_institutional_form, deontological).
narrative_ontology:cs_axiom('1f144704-7ffa-4ade-a3cc-ac0defe293c2', secondary, innovation_diffusion_precedes_measurement).
narrative_ontology:cs_axiom_status(innovation_diffusion_precedes_measurement, holdable).
narrative_ontology:cs_axiom_grounding('1f144704-7ffa-4ade-a3cc-ac0defe293c2', innovation_diffusion_precedes_measurement, empirically_contingent).
narrative_ontology:cs_reference_frame('1f144704-7ffa-4ade-a3cc-ac0defe293c2', pre_institutional_conceptual_emergence).
narrative_ontology:cs_drift_state('1f144704-7ffa-4ade-a3cc-ac0defe293c2', post_fintech_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1f144704-7ffa-4ade-a3cc-ac0defe293c2', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__became_thinkable_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__became_thinkable_reading, diffusionist_monetary_scholars).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__became_thinkable_reading, conceptual_history_tradition).
narrative_ontology:constraint_victim(electronic_money_emergence__became_thinkable_reading, institutionalist_monetary_scholars).
narrative_ontology:constraint_victim(electronic_money_emergence__became_thinkable_reading, statistical_formalists).
narrative_ontology:constraint_vindicates(electronic_money_emergence__became_thinkable_reading, conceptual_priority_over_institutional_form).
narrative_ontology:constraint_vindicates(electronic_money_emergence__became_thinkable_reading, innovation_diffusion_precedes_measurement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advance the 'became thinkable' reading through monographs, journal editorships, and doctoral training. They set the conceptual vocabulary for pre-institutional digital money emergence. Their careers are invested in this framework; exit means abandoning a research identity built over decades.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, diffusionist_monetary_scholars, agenda_setter,
    organized, generational, constrained, global).

% The broader intellectual tradition (history of technology, conceptual history, history of economic thought) that gains coherence when monetary innovation is framed as conceptual diffusion. They collect citation capital and disciplinary legitimacy without directly administering the monetary history subfield.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, conceptual_history_tradition, beneficiary,
    institutional, civilizational, arbitrage, global).

% Scholars whose framework locates emergence in the first institutional bearer holding dematerialized currency (first_held_reading). They bear the cost of engaging with a dominant paradigm that treats their institutional-adoption criterion as derivative. Their exit is constrained by the diffusionist framework's hold on top journals and funding panels.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, institutionalist_monetary_scholars, payer,
    organized, biographical, constrained, global).

% Proponents of the M4/M5 collapse reading who argue the category 'electronic money' is a statistical artifact. They pay in marginalization when central banks and the BIS adopt diffusionist periodization for historical data series. Their framework is treated as a measurement technicality rather than an ontological claim.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, statistical_formalists, payer,
    moderate, biographical, constrained, global).

% Custodians of the operational records that the first_held and M4/M5 readings rely on. Their expertise in ledger transitions and accounting changes is structurally excluded from the conceptual-history conversation. They would object that 'thinkability' leaves no documentary trace, but they lack a seat at the theoretical table.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, central_bank_archivists, excluded,
    institutional, generational, trapped, national).

% Sees the full structure: three readings contesting the same kernel, each with different beneficiary coalitions and different implications for how monetary history is periodized and taught.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, non-institutional criterion for periodizing the emergence of digital money, enabling cumulative research across monetary history, history of technology, and conceptual history without requiring consensus on specific institutional adoption dates.
% TRANSFER_FUNCTION: Moves scholarly authority, citation capital, journal space, and PhD supervision toward the conceptual-diffusion framework and away from institutional-adoption and statistical-artifact frameworks. The transfer is not monetary but epistemic: legitimacy accumulates to the reading that makes the broadest range of pre-institutional innovation visible.
% ABSENT_VOICES: Central bank archivists and institutional historians who prioritize formal adoption dates and ledger evidence. They are excluded because their evidentiary standard (documentary trace in official records) is treated as conceptually naive by the diffusionist framework.
% DISAPPEARANCE_RATIONALE: If the thinkability criterion vanished overnight, monetary history would revert to institutional-adoption dating (first central bank digital ledger, first commercial bank electronic transfer) or statistical-category dating (M4/M5 redefinition). The pre-history of digital money — the decades of conceptual work, failed experiments, and technical imagination — would lose its scholarly coherence as a distinct phase.
% FOUNDING_PROBLEM: Monetary history lacked a criterion for 'emergence' that could capture the long conceptual and technical gestation of digital money before any institution adopted it. The institutional-adoption criterion missed the innovation; the statistical criterion post-dated it. A conceptual-priority criterion was needed to make the pre-institutional phase visible.
% FOUNDING_PROBLEM_CORROBORATION: Historians of technology outside monetary theory (e.g., Joel Mokyr on the 'industrial enlightenment,' David Edgerton on 'use-centered' innovation) corroborate that conceptual diffusion precedes institutional adoption by decades across multiple technological domains. This is not self-asserted by monetary scholars.
narrative_ontology:disappearance_verdict(electronic_money_emergence__became_thinkable_reading, world_rearranges).
narrative_ontology:founding_problem_status(electronic_money_emergence__became_thinkable_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__became_thinkable_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(electronic_money_emergence__became_thinkable_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__became_thinkable_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__became_thinkable_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, ExtMetricName, E),
    domain_priors:suppression_score(electronic_money_emergence__became_thinkable_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(electronic_money_emergence__became_thinkable_reading),
    narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(electronic_money_emergence__became_thinkable_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects the framework's gatekeeping power over periodization — it extracts compliance from scholars who must engage its vocabulary. Suppression (0.48) is moderate: alternative readings persist but face barriers in top venues. Theater ratio (0.22) is low because the coordination function (shared periodization enabling cumulative research) is genuine. Accessibility collapse (0.78) is high: once the thinkability criterion is accepted, institutional-adoption and statistical-artifact criteria appear as category errors. Resistance (0.38) is moderate: the first_held and M4/M5 readings maintain active scholarly communities. The measurement series show extraction and suppression rising as the diffusionist framework consolidated in the 1990s-2000s.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, the constraint is a Mountain: the thinkability threshold is a natural conceptual fact, the coordination is genuine, the extraction is negligible. From the payer seats, it operates as a Tangled Rope: real coordination (shared periodization) coupled with asymmetric extraction (their frameworks must translate into diffusionist terms to be heard). The engine computes this divergence from the structural data; the claimed_type does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Diffusionist scholars are agenda_setters (d near beneficiary end) — they administer the framework. Conceptual history tradition is a beneficiary (d low) — it gains coherence without running the subfield. Institutionalist and statistical formalists are payers (d high) — their frameworks are marginalized, exit is constrained by the diffusionist hold on journals/funding. Central bank archivists are excluded (d not computed) — they hold the documentary evidence the other readings need but are structurally absent from the theoretical conversation. The analytical observer sits at d=0.5.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (capturing pre-institutional innovation) remains live — fintech and CBDC debates still need a pre-adoption emergence criterion. But the reading's mandate has expanded: it now governs periodization for the entire digital money pre-history, extracting compliance from scholars working on specific national or institutional cases where the thinkability criterion fits poorly. The mandatrophy is unresolved: the original coordination function is live, but the extraction has accumulated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the ''became thinkable'' criterion a natural conceptual boundary (Mountain) or a scholarly construction that benefits a specific coalition (Snare/Tangled Rope)?',
    'Cross-domain comparison: if conceptual-priority criteria independently emerge in multiple technological domains without scholarly coordination, the Mountain claim is strengthened. If the criterion is unique to monetary history and tracks the diffusionist coalition''s institutional power, the construction claim is strengthened.',
    'If Mountain, the reading''s ε is near zero and its coordination is pure. If constructed, the ε reflects epistemic rent extraction and the constraint reclassifies toward Tangled Rope or Snare depending on enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Natural-law vs. constructed status of the thinkability emergence criterion').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the marginalization of first_held and M4/M5 readings structural (peer review, funding, journal gatekeeping) or internalized (scholars self-censor because the diffusionist vocabulary has become the habitus of the field)?',
    'Post-exit suppression trajectory: track scholars who moved from institutionalist to diffusionist frameworks — if they report internalized suppression (self-censorship persisting after structural barriers lower), the internalized component is significant.',
    'If substantially internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression with them. This would increase χ for payer seats without changing base suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in scholarly paradigm marginalization').

omega_variable(
    measurement_lag_ontology,
    'Does the decades-long lag between conceptual innovation and institutional measurement reflect a real epistemic structure (concepts precede measurability) or a power asymmetry (measurers control the official record)?',
    'Counterfactual history: in domains where measurement institutions were weaker (e.g., early cryptography, pre-central-bank private money), did conceptual emergence still precede measurement by decades? If yes, the lag is structural; if no, it''s institutional power.',
    'If structural, the reading''s Mountain claim gains credibility and its low ε is justified. If power-asymmetry, the reading naturalizes an institutional effect, making it a false summit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_lag_ontology, conceptual, 'Ontological status of the concept-measurement lag in monetary innovation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__became_thinkable_reading, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eme_btr_tr_t1980, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1980, 0.08).
narrative_ontology:measurement(eme_btr_tr_t1990, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(eme_btr_tr_t2000, electronic_money_emergence__became_thinkable_reading, theater_ratio, 2000, 0.16).
narrative_ontology:measurement(eme_btr_tr_t2010, electronic_money_emergence__became_thinkable_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(eme_btr_tr_t2020, electronic_money_emergence__became_thinkable_reading, theater_ratio, 2020, 0.22).

% Extraction over time
narrative_ontology:measurement(eme_btr_be_t1980, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1980, 0.15).
narrative_ontology:measurement(eme_btr_be_t1990, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1990, 0.22).
narrative_ontology:measurement(eme_btr_be_t2000, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 2000, 0.31).
narrative_ontology:measurement(eme_btr_be_t2010, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 2010, 0.38).
narrative_ontology:measurement(eme_btr_be_t2020, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 2020, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(eme_btr_su_t1980, electronic_money_emergence__became_thinkable_reading, suppression_requirement, 1980, 0.25).
narrative_ontology:measurement(eme_btr_su_t1990, electronic_money_emergence__became_thinkable_reading, suppression_requirement, 1990, 0.33).
narrative_ontology:measurement(eme_btr_su_t2000, electronic_money_emergence__became_thinkable_reading, suppression_requirement, 2000, 0.41).
narrative_ontology:measurement(eme_btr_su_t2010, electronic_money_emergence__became_thinkable_reading, suppression_requirement, 2010, 0.45).
narrative_ontology:measurement(eme_btr_su_t2020, electronic_money_emergence__became_thinkable_reading, suppression_requirement, 2020, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__became_thinkable_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(electronic_money_emergence__became_thinkable_reading, 0.08).
narrative_ontology:affects_constraint(electronic_money_emergence__became_thinkable_reading, electronic_money_emergence__first_held_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__became_thinkable_reading, electronic_money_emergence__m4_m5_collapse_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the colloquial 'emergence of electronic money' into three structurally distinct claims with different ε values, different beneficiary coalitions, and different enforcement mechanisms. The became_thinkable_reading claims Mountain (natural conceptual diffusion) but operates with Tangled Rope dynamics (coordination + extraction). The first_held_reading claims Scaffold (institutional adoption as transitional coordination). The m4_m5_collapse_reading claims Snare (statistical category as retroactive extraction). All three are linked here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(electronic_money_emergence__became_thinkable_reading, institutional, 0.15).
constraint_indexing:directionality_override(electronic_money_emergence__became_thinkable_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
