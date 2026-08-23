% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__absolute_non_intervention
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__absolute_non_intervention, []).

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
 *   constraint_id: westphalia_sovereignty__absolute_non_intervention
 *   human_readable: Westphalian Absolute Non-Intervention Norm
 *   domain: international_law/political_theory/state_systems
 *
 * SUMMARY:
 *   The Westphalian absolute non-intervention norm declares that external
 *   interference in domestic affairs is per se illegitimate regardless of
 *   internal conduct. Originating as a settlement to end confessional war in
 *   1648, it became the constitutional principle of the modern state system.
 *   Over centuries, the norm expanded from a European diplomatic arrangement
 *   to a global rule shielding all recognized states — including those
 *   committing mass atrocities against their own populations. The constraint
 *   operates as a tangled rope: it coordinates genuine inter-state order
 *   (preventing intervention cascades, great power conflict) while extracting
 *   impunity for state elites who commit atrocities, with the cost borne by
 *   trapped populations. The coordination function is real but the extraction
 *   is asymmetric and substantial.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, 0.68).
domain_priors:suppression_score(westphalia_sovereignty__absolute_non_intervention, 0.72).
domain_priors:theater_ratio(westphalia_sovereignty__absolute_non_intervention, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, extractiveness, 0.68).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__absolute_non_intervention, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__absolute_non_intervention, "Westphalian Absolute Non-Intervention Norm").
narrative_ontology:topic_domain(westphalia_sovereignty__absolute_non_intervention, "international_law/political_theory/state_systems").

domain_priors:requires_active_enforcement(westphalia_sovereignty__absolute_non_intervention).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__absolute_non_intervention, 'dbdcec3b-a2f4-42ac-922a-cb31a7ac06bb').
narrative_ontology:cs_kernel_codification('dbdcec3b-a2f4-42ac-922a-cb31a7ac06bb', formalized).
narrative_ontology:cs_authority_grounding('dbdcec3b-a2f4-42ac-922a-cb31a7ac06bb', lineage).
narrative_ontology:cs_interpretation_layer_present('dbdcec3b-a2f4-42ac-922a-cb31a7ac06bb').
narrative_ontology:cs_reading_relation('dbdcec3b-a2f4-42ac-922a-cb31a7ac06bb', westphalia_sovereignty__conditional_responsibility, forecloses).
narrative_ontology:cs_reading_relation('dbdcec3b-a2f4-42ac-922a-cb31a7ac06bb', westphalia_sovereignty__graded_sovereignty, coexists_with).
narrative_ontology:cs_axiom('dbdcec3b-a2f4-42ac-922a-cb31a7ac06bb', foundational, territorial_inviolability_categorical).
narrative_ontology:cs_axiom_status(territorial_inviolability_categorical, holdable).
narrative_ontology:cs_axiom_grounding('dbdcec3b-a2f4-42ac-922a-cb31a7ac06bb', territorial_inviolability_categorical, conventional).
narrative_ontology:cs_axiom('dbdcec3b-a2f4-42ac-922a-cb31a7ac06bb', foundational, domestic_affairs_immune_from_external_judgment).
narrative_ontology:cs_axiom_status(domestic_affairs_immune_from_external_judgment, holdable).
narrative_ontology:cs_axiom_grounding('dbdcec3b-a2f4-42ac-922a-cb31a7ac06bb', domestic_affairs_immune_from_external_judgment, conventional).
narrative_ontology:cs_reference_frame('dbdcec3b-a2f4-42ac-922a-cb31a7ac06bb', westphalian_settlement_1648).
narrative_ontology:cs_drift_state('dbdcec3b-a2f4-42ac-922a-cb31a7ac06bb', contemporary_r2p_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('dbdcec3b-a2f4-42ac-922a-cb31a7ac06bb', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, state_elites_claiming_territorial_monopoly).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, authoritarian_regimes).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, great_powers_avoiding_precedent).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, populations_under_authoritarian_control).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, minorities_facing_state_persecution).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, civil_society_in_closed_states).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__absolute_non_intervention, territorial_integrity_principle).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__absolute_non_intervention, state_equality_doctrine).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__absolute_non_intervention, non_intervention_as_customary_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and enforce the non-intervention norm through UN Security Council vetoes, diplomatic recognition practices, and treaty interpretation. Collect political capital and regime survival from the barrier to external accountability. Can forum-shop between sovereignty claims and selective intervention when it serves their interests.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, state_elites_claiming_territorial_monopoly, agenda_setter,
    institutional, generational, arbitrage, global).

% Rely on absolute non-intervention to shield domestic repression from international consequences. The norm converts territorial control into impunity for internal atrocities. Exit from the constraint would require accepting external monitoring or regime change — structurally unavailable without losing power.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, authoritarian_regimes, beneficiary,
    powerful, biographical, constrained, national).

% Powerful states that violate the norm selectively (humanitarian interventions, regime change operations) but defend it structurally to avoid precedents that could be turned against them. They benefit from the norm's general force while carving exceptions through ad hoc coalitions. Their exit option is veto power and coalition-building capacity.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, great_powers_avoiding_precedent, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__absolute_non_intervention, great_powers_avoiding_precedent, agenda_setter).

% Bear the full cost of the constraint: repression, disappearances, starvation, cultural erasure continue without external remedy because the norm treats their suffering as a domestic matter. No meaningful exit — flight is criminalized or physically blocked, and international protection is foreclosed by the sovereignty barrier.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, populations_under_authoritarian_control, payer,
    powerless, biographical, trapped, local).

% Targeted groups (ethnic, religious, political) for whom the sovereignty shield means genocide, ethnic cleansing, or systematic discrimination proceed without effective international interruption. The constraint's coordination function (order among states) extracts their survival as the price.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, minorities_facing_state_persecution, payer,
    powerless, biographical, trapped, local).

% Activists, journalists, lawyers who cannot exit the relationship to their state without abandoning their professional identity and life's work. The constraint extracts their safety and efficacy — international solidarity is filtered through the sovereignty barrier, leaving them exposed to state retaliation while foreign NGOs cannot operate.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, civil_society_in_closed_states, payer,
    moderate, biographical, identity_locked, local).

% Produce the doctrinal architecture that sustains or challenges the norm. Their career incentives align with maintaining the interpretive structure (commentary, treaty drafting, court briefs) rather than resolving the extraction. They see the full pattern but their institutional role is to elaborate, not disrupt.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, international_legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% Operate in the gaps where sovereignty permits — disaster relief, technical assistance — but are structurally barred from protection mandates. They would argue for conditional access based on need; their exclusion is maintained by the same state consent regime the norm legitimates.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, humanitarian_ngos, excluded,
    organized, immediate, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable framework for inter-state order by establishing a clear, bright-line rule: territorial boundaries are final and internal affairs are not subject to external judgment. This prevents endless intervention cascades, great power wars over domestic governance, and the instrumentalization of human rights for regime change.
% TRANSFER_FUNCTION: Transfers the cost of domestic atrocities from the perpetrating state elites (who would face consequences, sanctions, or removal under a conditional norm) to the victim populations (who bear the violence, displacement, and death). The arrangement moves impunity upward and vulnerability downward.
% ABSENT_VOICES: The dead and disappeared in Syria, Xinjiang, Myanmar, North Korea, and historical cases (Cambodia, Rwanda pre-1994) — their voices are structurally excluded because the norm treats their suffering as internal to a sovereign's domain. Survivors and diaspora communities who would testify to the cost are filtered through state consent mechanisms for UN access.
% DISAPPEARANCE_RATIONALE: If absolute non-intervention vanished overnight, the UN system would need a new authorization framework for protection mandates; R2P would move from aspirational to operational; great powers would lose their primary legal shield for both restraint and selective intervention; authoritarian regimes would face immediate exposure to ICC referrals, targeted sanctions, and potential coalition action. The world would rearrange around a contested new legitimacy standard.
% FOUNDING_PROBLEM: The Thirty Years' War (1618-1648) demonstrated that confessional intervention by external powers into domestic religious affairs produced unlimited war. The Westphalian settlement established territorial sovereignty as the price of peace: princes would determine religion within their borders, and external powers would not intervene.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (Osiander, Teschke, Nexon) attest the founding problem was specifically confessional war in early modern Europe, not a universal principle for all internal conduct. IR constructivists (Krasner, Philpott) document the norm's evolution from a European settlement to a global template. R2P advocates (Evans, Welsh, Bellamy) attest the founding problem is substantially superseded by the atrocity-prevention imperative. No non-beneficiary source attests the original problem justifies the current categorical form.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__absolute_non_intervention, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__absolute_non_intervention, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__absolute_non_intervention, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(westphalia_sovereignty__absolute_non_intervention, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__absolute_non_intervention, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__absolute_non_intervention_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__absolute_non_intervention_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the substantial transfer: state elites gain regime survival and policy autonomy worth immense political capital; victims lose lives, liberty, and any external remedy. Suppression (0.72) is high because the norm's persistence depends on active enforcement — UNSC vetoes, non-recognition of secessionist entities, diplomatic pressure against intervention coalitions, and the institutional architecture of state consent. Theater ratio (0.25) is moderate: the order function is real but a growing share of enforcement activity protects the extraction (impunity) rather than the coordination (peace). Accessibility collapse (0.65) is significant — alternatives (R2P, conditional sovereignty, humanitarian intervention) exist intellectually but collapse operationally when veto-wielding states block them. Resistance (0.45) is moderate: R2P, ICC, targeted sanctions, and ad hoc coalitions (Kosovo, Libya) represent real but incomplete pushback.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (state elites, great powers), the constraint appears as essential coordination — the only barrier against a return to unlimited intervention wars. From the payer seats (victim populations, civil society), the same structure operates as a license for atrocity. The engine computes this divergence from the structural data: the same norm that prevents Iraq 2003 also shields Syria 2011-2024. The claimed type (tangled_rope) captures this duality; a pure rope claim would deny the extraction, a pure snare claim would deny the coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   State elites and authoritarian regimes are structural beneficiaries (d near 0.0-0.2): they collect regime survival and impunity directly from the norm's operation. Great powers are dual-positioned: beneficiaries of the general barrier (d ~0.15) but agenda-setters who selectively violate it (d varies by episode). Victim populations are full targets (d ~0.9-1.0): trapped, identity-locked, or powerless with no exit. Civil society in closed states is identity-locked (d ~0.75) — they cannot exit without abandoning their professional identity. International legal scholars are analytical observers (d=0.5 by definition). Humanitarian NGOs are excluded (not coordinated, not extracting, but structurally barred from protection roles).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (confessional war in 17th century Europe) is historically superseded, but the arrangement persists because it now serves a different function: protecting state elites from accountability. The coordination problem it originally solved is not the coordination problem it now solves. The norm has not been formally sunsetted; instead, its justification has been continuously reinterpreted (from religious peace to state equality to non-intervention to territorial integrity). This is classic mandatrophy: the mandate (prevent inter-state war) has outlived its original function, but the constraint persists by accumulating new justifications that serve the beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'Is the inter-state order function genuinely dependent on the categorical form, or could a conditional norm (R2P, atrocity threshold) provide equivalent coordination with less extraction?',
    'Counterfactual analysis of intervention cascades under conditional norms: if R2P-style thresholds had been operational since 1945, would great power conflict have increased? Historical comparison of intervention frequency/severity under League of Nations (conditional) vs UN (categorical with exceptions).',
    'If coordination survives conditionality, the categorical form is extractive overhead; if conditionality triggers cascade, the extraction is the price of order. Determines whether the constraint is tangled_rope (both real) or snare (coordination is cover).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the categorical form is necessary for the coordination function or serves extraction.').

omega_variable(
    victim_set_boundary,
    'Does the victim set include only populations under authoritarian control, or do populations in weak/failing states (where the state cannot protect them) also pay the extraction cost?',
    'Compare outcomes in authoritarian repression (Syria, Myanmar) vs state collapse (Somalia 1990s, DRC, Yemen) — in both, external protection is blocked or ineffective, but the mechanism differs (sovereignty shield vs capacity vacuum).',
    'If weak-state populations are also victims, the extraction base is wider than the authoritarian-only framing; the constraint extracts from state failure as well as state predation. Affects beneficiary/victim scope and coordination-extraction balance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_set_boundary, empirical, 'Whether state failure victims are extracted by the same constraint or a different one.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the absolute_non_intervention reading genuinely foreclose the conditional_responsibility reading within a single legal framework, or do they operate as competing but coexisting interpretive traditions?',
    'Analyze UNSC practice: does any resolution simultaneously invoke absolute sovereignty language AND authorize intervention under R2P? Examine ICJ jurisprudence for doctrinal coexistence vs mutual exclusion.',
    'If foreclosure holds, the kernel is a genuine binary; if coexistence holds, the kernel is a contested field where both readings operate simultaneously — affects reading_relations classification and axiom status assignment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Logical relationship between absolute and conditional sovereignty readings.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (UNSC vetoes, state consent regime, treaty architecture) or internalized (populations and civil society accepting that ''nothing can be done,'' NGOs self-censoring to maintain access)?',
    'Post-intervention suppression trajectory: in cases where intervention occurred (Kosovo, Libya), did local civil society suppression persist or diminish? Compare internalized suppression markers (self-censorship, fatalism) in never-intervened vs intervened contexts.',
    'If internalized, effective suppression is higher than structural measure — the constraint extracts compliance from victims themselves. If structural, suppression is vulnerable to institutional reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression in the sovereignty constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__absolute_non_intervention, 1648, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(westphalia_sovereignty__absolute_non_intervention_tr_t1648, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1648, 0.1).
narrative_ontology:measurement(westphalia_sovereignty__absolute_non_intervention_tr_t1815, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1815, 0.12).
narrative_ontology:measurement(westphalia_sovereignty__absolute_non_intervention_tr_t1919, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1919, 0.15).
narrative_ontology:measurement(westphalia_sovereignty__absolute_non_intervention_tr_t1945, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1945, 0.18).
narrative_ontology:measurement(westphalia_sovereignty__absolute_non_intervention_tr_t1991, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1991, 0.2).
narrative_ontology:measurement(westphalia_sovereignty__absolute_non_intervention_tr_t2001, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 2001, 0.22).
narrative_ontology:measurement(westphalia_sovereignty__absolute_non_intervention_tr_t2011, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 2011, 0.24).
narrative_ontology:measurement(westphalia_sovereignty__absolute_non_intervention_tr_t2024, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(westphalia_sovereignty__absolute_non_intervention_be_t1648, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1648, 0.35).
narrative_ontology:measurement(westphalia_sovereignty__absolute_non_intervention_be_t1815, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1815, 0.4).
narrative_ontology:measurement(westphalia_sovereignty__absolute_non_intervention_be_t1919, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1919, 0.45).
narrative_ontology:measurement(westphalia_sovereignty__absolute_non_intervention_be_t1945, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1945, 0.5).
narrative_ontology:measurement(westphalia_sovereignty__absolute_non_intervention_be_t1991, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1991, 0.58).
narrative_ontology:measurement(westphalia_sovereignty__absolute_non_intervention_be_t2001, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 2001, 0.62).
narrative_ontology:measurement(westphalia_sovereignty__absolute_non_intervention_be_t2011, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 2011, 0.66).
narrative_ontology:measurement(westphalia_sovereignty__absolute_non_intervention_be_t2024, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(westphalia_sovereignty__absolute_non_intervention_su_t1648, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1648, 0.55).
narrative_ontology:measurement(westphalia_sovereignty__absolute_non_intervention_su_t1815, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1815, 0.6).
narrative_ontology:measurement(westphalia_sovereignty__absolute_non_intervention_su_t1919, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1919, 0.62).
narrative_ontology:measurement(westphalia_sovereignty__absolute_non_intervention_su_t1945, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1945, 0.65).
narrative_ontology:measurement(westphalia_sovereignty__absolute_non_intervention_su_t1991, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1991, 0.68).
narrative_ontology:measurement(westphalia_sovereignty__absolute_non_intervention_su_t2001, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 2001, 0.7).
narrative_ontology:measurement(westphalia_sovereignty__absolute_non_intervention_su_t2011, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 2011, 0.71).
narrative_ontology:measurement(westphalia_sovereignty__absolute_non_intervention_su_t2024, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__absolute_non_intervention, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalia_sovereignty__absolute_non_intervention, 0.12).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty__conditional_responsibility).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty__graded_sovereignty).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, r2p_authorization_framework).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, icc_complementarity_regime).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, unsc_veto_power).

% DUAL FORMULATION NOTE:
% This constraint is the absolute_non_intervention reading of the westphalia_sovereignty kernel. The conditional_responsibility reading (R2P/atrocity threshold) and graded_sovereignty reading (capacity-calibrated intervention) are sibling constraints. The absolute reading provides the default legal framework from which the conditional reading must carve exceptions; the graded reading describes the de facto practice of powerful states that the absolute reading nominally prohibits.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalia_sovereignty__absolute_non_intervention, institutional, 0.15).
constraint_indexing:directionality_override(westphalia_sovereignty__absolute_non_intervention, powerful, 0.25).
constraint_indexing:directionality_override(westphalia_sovereignty__absolute_non_intervention, powerless, 0.95).
constraint_indexing:directionality_override(westphalia_sovereignty__absolute_non_intervention, moderate, 0.75).
constraint_indexing:directionality_override(westphalia_sovereignty__absolute_non_intervention, analytical, 0.5).
constraint_indexing:directionality_override(westphalia_sovereignty__absolute_non_intervention, organized, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
