% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__institutional_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-22
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__institutional_displacement_reading, []).

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
 *   constraint_id: dueling_disappearance_mechanism__institutional_displacement_reading
 *   human_readable: Dueling Displacement by Institutional Substitution (Dispute-Resolution Protocol)
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint story models the institutional displacement reading of
 *   dueling's decline: dueling did not disappear because it was banned or
 *   culturally condemned, but because courts, commercial law, and libel
 *   statutes offered a superior coordination mechanism for dispute
 *   resolution. The constraint is the *availability and functional dominance*
 *   of institutional alternatives. Dueling persists as a fringe option in
 *   institutional gaps (military honor, diplomatic ritual, subcultural
 *   enclaves) — but where institutions function, actors voluntarily
 *   substitute. No victim set exists because the substitution is
 *   welfare-improving for all participating parties. The constraint type
 *   remains rope throughout: a coordination protocol that persists because it
 *   works, not because it is enforced.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__institutional_displacement_reading, 0.12).
domain_priors:suppression_score(dueling_disappearance_mechanism__institutional_displacement_reading, 0.18).
domain_priors:theater_ratio(dueling_disappearance_mechanism__institutional_displacement_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__institutional_displacement_reading, rope).
narrative_ontology:human_readable(dueling_disappearance_mechanism__institutional_displacement_reading, "Dueling Displacement by Institutional Substitution (Dispute-Resolution Protocol)").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__institutional_displacement_reading, "historical_sociology/cultural_anthropology/legal_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__institutional_displacement_reading, '8f2d4148-c7b2-4a2d-af50-c3c66aa0da3f').
narrative_ontology:cs_kernel_codification('8f2d4148-c7b2-4a2d-af50-c3c66aa0da3f', distributed).
narrative_ontology:cs_authority_grounding('8f2d4148-c7b2-4a2d-af50-c3c66aa0da3f', practice).
narrative_ontology:cs_interpretation_layer_present('8f2d4148-c7b2-4a2d-af50-c3c66aa0da3f').
narrative_ontology:cs_reading_relation('8f2d4148-c7b2-4a2d-af50-c3c66aa0da3f', dueling_disappearance_mechanism__contraction_reading, influences).
narrative_ontology:cs_reading_relation('8f2d4148-c7b2-4a2d-af50-c3c66aa0da3f', dueling_disappearance_mechanism__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_axiom('8f2d4148-c7b2-4a2d-af50-c3c66aa0da3f', foundational, institutional_dispute_resolution_functionally_dominates_private_violence).
narrative_ontology:cs_axiom_status(institutional_dispute_resolution_functionally_dominates_private_violence, holdable).
narrative_ontology:cs_axiom_grounding('8f2d4148-c7b2-4a2d-af50-c3c66aa0da3f', institutional_dispute_resolution_functionally_dominates_private_violence, empirically_contingent).
narrative_ontology:cs_axiom('8f2d4148-c7b2-4a2d-af50-c3c66aa0da3f', secondary, dueling_persistence_in_gaps_is_voluntary_not_coerced).
narrative_ontology:cs_axiom_status(dueling_persistence_in_gaps_is_voluntary_not_coerced, holdable).
narrative_ontology:cs_axiom_grounding('8f2d4148-c7b2-4a2d-af50-c3c66aa0da3f', dueling_persistence_in_gaps_is_voluntary_not_coerced, empirically_contingent).
narrative_ontology:cs_reference_frame('8f2d4148-c7b2-4a2d-af50-c3c66aa0da3f', honor_culture_dispute_resolution_baseline).
narrative_ontology:cs_drift_state('8f2d4148-c7b2-4a2d-af50-c3c66aa0da3f', late_19th_century_institutional_maturity, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8f2d4148-c7b2-4a2d-af50-c3c66aa0da3f', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, merchants_bankers_commercial_actors).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, state_legal_institutions).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, middle_class_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__institutional_displacement_reading, aristocratic_officer_class).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__institutional_displacement_reading, formal_legal_dispute_resolution_superior_to_private_violence).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__institutional_displacement_reading, contract_enforcement_via_courts_reduces_transaction_costs).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__institutional_displacement_reading, libel_law_provides_reputational_recourse_without_bloodshed).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically used dueling to defend honor and settle disputes of status. As courts, banking, and libel law matured, this class found formal institutions increasingly accessible and effective for their dispute-resolution needs. The cost of dueling (risk of death, legal jeopardy, social censure) came to exceed the cost of litigation or arbitration. They voluntarily migrated to institutional channels because those channels delivered better outcomes at lower risk — not because they were coerced.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, aristocratic_officer_class, payer,
    organized, biographical, mobile, national).

% Required reliable, predictable dispute resolution for contracts, credit, and reputation. Courts and commercial law provided standardized procedures, precedent, and enforceable judgments — a coordination infrastructure that private violence could never match. They benefited directly from the displacement of dueling by formal institutions, which lowered transaction costs and enabled scalable commerce.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, merchants_bankers_commercial_actors, beneficiary,
    powerful, generational, arbitrage, national).

% Built and administered the court systems, commercial codes, and libel statutes that offered a superior coordination mechanism for dispute resolution. Their authority and legitimacy grew as they absorbed the dispute-resolution function formerly served by dueling. They actively developed these institutions but did not need to coercively suppress dueling — the substitution was driven by the institutions' superior functionality.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, state_legal_institutions, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__institutional_displacement_reading, state_legal_institutions, beneficiary).

% Gained access to legal recourse (courts for contracts and property, libel law for reputation) that was previously unavailable or reserved for elites. The displacement of dueling by institutional mechanisms democratized dispute resolution — they no longer needed physical prowess or aristocratic standing to defend their interests. Voluntary adopters of the new institutional options.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, middle_class_citizens, beneficiary,
    moderate, biographical, mobile, national).

% Analyze the historical transition from honor-based private violence to institutional dispute resolution. They observe the structural dynamics: functional substitution, voluntary migration, and the persistence of dueling as a residual option in institutional gaps (military, diplomatic, subcultural). Their seat computes the constraint's type from the structural data authored here.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, legal_historians_sociologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a socially recognized, low-cost, predictable protocol for resolving disputes of honor, contract, and reputation without private violence. Dueling served this function historically; courts, banking systems, and libel law progressively outcompeted it by offering greater reliability, lower risk, broader accessibility, and enforceable outcomes.
% TRANSFER_FUNCTION: Transfers dispute-resolution throughput from private violent encounters (dueling) to formal institutional channels (courts, arbitration, libel proceedings). The 'payment' is the abandonment of honor-violence as a dispute method; the 'receipt' is access to legal process, credit markets, and reputational protection. No extraction occurs — the transfer is voluntary because the institutional alternative dominates on cost, risk, and outcome quality.
% ABSENT_VOICES: No structurally excluded voices in this reading. The transition was driven by the superior functionality of institutions, not by suppressing a constituency. Residual dueling practitioners (military subcultures, diplomatic rituals, fringe honor communities) are not excluded — they remain free to duel where institutions do not reach, but choose not to where institutions function well.
% DISAPPEARANCE_RATIONALE: If the institutional displacement mechanism vanished — i.e., if courts, banking, and libel law ceased to provide effective dispute resolution — dueling (or similar private violence) would re-emerge as a coordination mechanism in the resulting vacuum. The world rearranges because the constraint IS the functional superiority of institutions; remove that superiority and the old mechanism returns.
% FOUNDING_PROBLEM: Pre-modern societies lacked reliable, accessible, impartial mechanisms for resolving disputes of honor, contract, and reputation. Dueling filled this gap as a costly but functional coordination protocol. The founding problem was the absence of better alternatives.
% FOUNDING_PROBLEM_CORROBORATION: Standard historical consensus (e.g., Stone 1965 on English courts, Graeber 2011 on credit institutions, Elias 1939/2000 on civilizing process) documents the functional superiority of institutional dispute resolution over private violence. The corroboration comes from institutional history, economic history, and sociological theory — all outside any beneficiary group of the dueling system itself.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__institutional_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__institutional_displacement_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__institutional_displacement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(dueling_disappearance_mechanism__institutional_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__institutional_displacement_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__institutional_displacement_reading_tests).
:- end_tests(dueling_disappearance_mechanism__institutional_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) and declining over the interval because the institutional alternative *reduces* the cost of dispute resolution relative to dueling — it extracts negative rent (provides subsidy). Suppression is low (0.18) and declining because the constraint does not rely on coercion to persist; it persists because it is the better option. Theater ratio is negligible (0.08) — the institutions perform their stated function (dispute resolution) effectively. Accessibility collapse is moderate (0.35) — dueling remains *legally* and *physically* possible but *functionally* inaccessible because better alternatives exist. Resistance is low (0.25) — the transition meets little opposition because the beneficiaries include nearly all dispute-resolution users. The measurement series shows monotonic improvement in institutional functionality and corresponding decline in dueling's relevance.
 *
 * PERSPECTIVAL GAP:
 *   The contraction_reading would compute a different seat divergence: from the honor-culture actor's seat, the displacement *looks* like extraction (loss of honor-capital, forced cultural assimilation) even if the institutional alternative is functionally superior. This reading (institutional_displacement) computes the payer seat as transitional, not structural — the cost of switching is a one-time transition cost, not ongoing extraction. The engine computes per-seat types from the structural data; this commentary explains why the seats diverge across readings of the same kernel.
 *
 * DIRECTIONALITY LOGIC:
 *   The aristocratic officer class are payers in the transitional sense: they bear the switching cost of adopting new institutional habits, but they are net beneficiaries because the new system lowers their dispute-resolution costs and risks. Merchants, state institutions, and middle-class citizens are clear beneficiaries — they gain access to superior coordination infrastructure. No agent is a victim. Directionality values derived from beneficiary declarations + exit options: aristocratic class (mobile exit, organized power) → d ~0.35 (mild target during transition, then beneficiary); merchants (arbitrage exit, powerful) → d ~0.05 (strong beneficiary); state institutions (analytical exit, institutional power) → d ~0.0 (agenda-setter beneficiary); middle class (mobile exit, moderate power) → d ~0.15 (beneficiary); observers (analytical) → d ~0.5.
 *
 * MANDATROPHY ANALYSIS:
 *   The dueling protocol's mandate (honor-based dispute resolution) was genuinely solved by institutional substitution — the founding problem is dead. The constraint (institutional dispute resolution) has no mandatrophy because it continues to solve live problems (contract enforcement, reputational protection, credit allocation). Dueling's residual persistence in gaps is not mandatrophy — it is the expected fringe of a coordination mechanism that has been functionally outcompeted but not prohibited.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_functionality_threshold,
    'At what level of institutional functionality does voluntary substitution become dominant over coercion or cultural change as the displacement mechanism?',
    'Comparative historical analysis of dueling decline across jurisdictions with varying court development, commercial law maturity, and libel statute timing (e.g., England vs. France vs. American South vs. Germany). Threshold identification via regression of dueling frequency on institutional quality indices.',
    'If a clear functionality threshold exists, the rope classification is robust — the constraint is purely coordination. If substitution occurs even with weak institutions, cultural or coercive factors dominate and the reading''s structural claim weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_functionality_threshold, empirical, 'Whether institutional superiority alone suffices to explain the displacement pattern.').

omega_variable(
    residual_dueling_coordination_value,
    'Does dueling retain any genuine coordination function in institutional gaps (military, diplomatic, subcultural), or is its persistence purely ceremonial/identity-performative?',
    'Ethnographic and historical study of contemporary dueling-adjacent practices (military honor codes, diplomatic protocol, fraternity rituals, underground fighting circuits). Test: do participants achieve dispute-resolution outcomes that institutions cannot provide in those contexts?',
    'If residual coordination value exists, the constraint family includes a genuine rope fragment (dueling_in_gaps). If purely performative, the residual is piton/theater — a different structural object.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(residual_dueling_coordination_value, empirical, 'Whether fringe dueling is functional coordination or theatrical residue.').

omega_variable(
    reading_relations_structural_delta,
    'Does the institutional_displacement_reading''s core premise (functional substitution) logically foreclose the contraction_reading''s core premise (cultural unthinkability), or do they coexist as explanations operating at different levels?',
    'Structural analysis of the two readings'' causal claims: if functional substitution *causes* cultural unthinkability (institutions work → honor culture becomes obsolete), they are sequential, not competing. If cultural change *enables* institutional adoption (dignity norms → demand for courts), they are reverse-sequential. If independent, they coexist.',
    'If forecloses: the kernel has a dominant reading. If coexists_with: the kernel is genuinely multi-causal. If influences: a causal chain exists. This determines the cs_structure.reading_relations values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relations_structural_delta, conceptual, 'Structural relationship between institutional displacement and cultural contraction readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__institutional_displacement_reading, 1650, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ddm_idr_tr_t1650, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1650, 0.25).
narrative_ontology:measurement(ddm_idr_tr_t1700, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1700, 0.2).
narrative_ontology:measurement(ddm_idr_tr_t1750, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1750, 0.15).
narrative_ontology:measurement(ddm_idr_tr_t1800, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(ddm_idr_tr_t1850, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1850, 0.08).
narrative_ontology:measurement(ddm_idr_tr_t1900, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1900, 0.08).

% Extraction over time
narrative_ontology:measurement(ddm_idr_be_t1650, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1650, 0.35).
narrative_ontology:measurement(ddm_idr_be_t1700, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1700, 0.28).
narrative_ontology:measurement(ddm_idr_be_t1750, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1750, 0.22).
narrative_ontology:measurement(ddm_idr_be_t1800, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1800, 0.15).
narrative_ontology:measurement(ddm_idr_be_t1850, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1850, 0.12).
narrative_ontology:measurement(ddm_idr_be_t1900, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1900, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(ddm_idr_su_t1650, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1650, 0.3).
narrative_ontology:measurement(ddm_idr_su_t1700, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1700, 0.25).
narrative_ontology:measurement(ddm_idr_su_t1750, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1750, 0.22).
narrative_ontology:measurement(ddm_idr_su_t1800, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1800, 0.18).
narrative_ontology:measurement(ddm_idr_su_t1850, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1850, 0.18).
narrative_ontology:measurement(ddm_idr_su_t1900, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1900, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__institutional_displacement_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(dueling_disappearance_mechanism__institutional_displacement_reading, 0.1).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism__contraction_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism__overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the dueling_disappearance_mechanism kernel. The institutional_displacement_reading models functional substitution by superior coordination institutions (rope, ε=0.12). The contraction_reading models cultural displacement of honor axioms (likely snare/tangled_rope, higher ε). The overdetermined_composite_reading models causal overdetermination (type depends on weight assignment). All three stories share the kernel_id and are linked via affects_constraints. The ε values differ structurally: this reading's ε is low because substitution is welfare-improving; the contraction reading's ε is higher because cultural displacement extracts from honor-culture adherents.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dueling_disappearance_mechanism__institutional_displacement_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
