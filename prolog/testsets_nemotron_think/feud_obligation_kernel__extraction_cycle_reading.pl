% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__extraction_cycle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__extraction_cycle_reading, []).

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
 *   constraint_id: feud_obligation_kernel__extraction_cycle_reading
 *   human_readable: Blood-Feud Obligation as Destructive Extraction Cycle
 *   domain: legal_anthropology/medieval_history/comparative_political_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the extraction_cycle_reading of the
 *   feud_obligation_kernel. The reading treats blood-feud obligations not as
 *   a genuine coordination mechanism (the stateless_coordination_reading) nor
 *   as a theological violation (the christianized_pacification_reading), but
 *   as a destructive extraction cycle: the obligation compels kin groups into
 *   retaliatory chains that deplete their productive capacity, preventing
 *   them from consolidating territorial power. This structural weakness
 *   legitimizes royal authority's monopoly on violence and tax extraction.
 *   The crown actively suppresses kinship-based enforcement (private war,
 *   dueling, blood money) while benefiting from the disorder the feuds
 *   create. The claimed type is tangled_rope because the constraint retains a
 *   residual coordination function (deterrence in stateless zones) but
 *   operates primarily as asymmetric extraction with active enforcement by
 *   royal authority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, 0.75).
domain_priors:suppression_score(feud_obligation_kernel__extraction_cycle_reading, 0.8).
domain_priors:theater_ratio(feud_obligation_kernel__extraction_cycle_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__extraction_cycle_reading, tangled_rope).
narrative_ontology:human_readable(feud_obligation_kernel__extraction_cycle_reading, "Blood-Feud Obligation as Destructive Extraction Cycle").
narrative_ontology:topic_domain(feud_obligation_kernel__extraction_cycle_reading, "legal_anthropology/medieval_history/comparative_political_systems").

domain_priors:requires_active_enforcement(feud_obligation_kernel__extraction_cycle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__extraction_cycle_reading, '27e64c3d-1ff2-410d-a274-15abf6b6a460').
narrative_ontology:cs_kernel_codification('27e64c3d-1ff2-410d-a274-15abf6b6a460', distributed).
narrative_ontology:cs_authority_grounding('27e64c3d-1ff2-410d-a274-15abf6b6a460', practice).
narrative_ontology:cs_interpretation_layer_present('27e64c3d-1ff2-410d-a274-15abf6b6a460').
narrative_ontology:cs_reading_relation('27e64c3d-1ff2-410d-a274-15abf6b6a460', feud_obligation_kernel__stateless_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('27e64c3d-1ff2-410d-a274-15abf6b6a460', feud_obligation_kernel__christianized_pacification_reading, coexists_with).
narrative_ontology:cs_axiom('27e64c3d-1ff2-410d-a274-15abf6b6a460', foundational, feud_obligations_operate_as_resource_extraction_cycle).
narrative_ontology:cs_axiom_status(feud_obligations_operate_as_resource_extraction_cycle, holdable).
narrative_ontology:cs_axiom_grounding('27e64c3d-1ff2-410d-a274-15abf6b6a460', feud_obligations_operate_as_resource_extraction_cycle, empirically_contingent).
narrative_ontology:cs_axiom('27e64c3d-1ff2-410d-a274-15abf6b6a460', secondary, royal_monopoly_on_violence_legitimizes_via_feud_suppression).
narrative_ontology:cs_axiom_status(royal_monopoly_on_violence_legitimizes_via_feud_suppression, holdable).
narrative_ontology:cs_axiom_grounding('27e64c3d-1ff2-410d-a274-15abf6b6a460', royal_monopoly_on_violence_legitimizes_via_feud_suppression, empirically_contingent).
narrative_ontology:cs_reference_frame('27e64c3d-1ff2-410d-a274-15abf6b6a460', customary_vengeance_economy).
narrative_ontology:cs_drift_state('27e64c3d-1ff2-410d-a274-15abf6b6a460', early_state_formation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('27e64c3d-1ff2-410d-a274-15abf6b6a460', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, royal_authority).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, urban_merchant_classes).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, feud_participants).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, kin_group_members).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__extraction_cycle_reading, state_monopoly_on_violence_legitimacy).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__extraction_cycle_reading, territorial_consolidation_requires_feud_suppression).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals and kin groups bound by customary obligation to retaliate for killings or insults. Each retaliation triggers counter-retaliation, depleting labor, capital, and lives. Exit requires abandoning kin identity and honor — effectively social death. The obligation is enforced by kin elders and community pressure; refusal brings ostracism.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, feud_participants, payer,
    organized, biographical, identity_locked, regional).

% Administer the feud obligation: declare when retaliation is required, negotiate settlements (blood money), and enforce compliance within the kin group. They derive status and authority from this role but are also trapped by it — failure to pursue feuds undermines their legitimacy. Some elders quietly seek royal intervention to escape the cycle.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, kin_group_elders, agenda_setter,
    organized, generational, constrained, regional).

% The crown benefits structurally from the feud cycle: it prevents kin groups from consolidating territorial power that could rival the state. The crown then suppresses kinship-based enforcement (dueling, private war) and replaces it with royal courts, legitimizing its monopoly on violence and extracting taxes from the pacified population. The crown does not directly run feuds but gains from their destructive persistence.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, royal_authority, beneficiary,
    institutional, generational, arbitrage, national).

% Merchants and town dwellers benefit from royal suppression of feuds because it secures trade routes, property rights, and predictable courts. They support royal authority fiscally and politically. They are not direct participants in feuds but gain from the constraint's attenuation.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, urban_merchant_classes, beneficiary,
    organized, biographical, mobile, regional).

% Church authorities condemn feuds as violating divine law (vengeance belongs to God). They promote peace oaths, truce of God movements, and penitential systems. In this reading's frame, they are excluded from the material extraction cycle — their theological frame is a sibling reading, not a structural actor within the extraction dynamic.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, ecclesiastical_authority, excluded,
    institutional, civilizational, analytical, continental).

% Analyzes the feud obligation as a structural arrangement: identifies the extraction cycle, the beneficiary-victim asymmetry, and the role of state formation in suppressing kinship enforcement while capturing the gains. Does not participate in or benefit from the constraint.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, historical_sociologist, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In stateless or weak-state regions, the feud obligation provides a decentralized deterrence and dispute-resolution mechanism: the threat of retaliation substitutes for absent courts and police, enforcing norms against killing and theft within and between kin groups.
% TRANSFER_FUNCTION: Moves productive capacity (labor, livestock, land, lives) from feud-participating kin groups into destruction and deadweight loss; the resulting weakness and disorder transfer legitimacy and fiscal extraction capacity to royal authority, which monopolizes violence and taxes the pacified population.
% ABSENT_VOICES: Women and non-combatant kin members (children, elderly) who bear mortality and impoverishment risks but have no voice in feud decisions; they are structurally excluded from the kin councils that declare and settle feuds. Also absent: the merchant and peasant populations who would prefer royal courts but lack organized representation in the feud system.
% DISAPPEARANCE_RATIONALE: If feud obligations vanished overnight, kin groups would lose their primary coercive leverage and deterrence mechanism in stateless zones — initially increasing vulnerability. But the structural extraction cycle would break: resources would no longer be depleted in retaliatory chains, royal courts would face less competition, and territorial consolidation would accelerate. The world rearranges toward state-centered order.
% FOUNDING_PROBLEM: In the absence of centralized enforcement, how can a society deter violence and enforce norms without a state? The feud obligation emerged as a self-help mechanism: kin groups internalize the cost of policing their own members and retaliating against outsiders.
% FOUNDING_PROBLEM_CORROBORATION: Medieval chroniclers (e.g., Nithard, Orderic Vitalis) document the feast cycle's destructive effects on peasant productivity and aristocratic consolidation. Modern historical sociologists (e.g., Tilly, Scott, Blaydes) corroborate that the founding problem — stateless order — was substantially solved by state formation, yet the feud obligation persisted as an extraction mechanism. No corroboration comes from kin group apologists; the 'honor culture' defense is endogenous to the benefiting elders.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__extraction_cycle_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__extraction_cycle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__extraction_cycle_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(feud_obligation_kernel__extraction_cycle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__extraction_cycle_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75) because the feud obligation systematically destroys value — lives, labor, capital — without proportional coordination benefit. Suppression is high (0.8) because royal authority must actively suppress kinship enforcement (banning private war, establishing royal courts, prosecuting feud participants) to maintain its monopoly; the constraint persists only because kin groups are identity-locked into the cycle. Theater ratio is moderate (0.4) — honor rituals and blood-money negotiations perform a coordination facade but increasingly mask pure extraction. Accessibility collapse is high (0.7) because once a kin group enters the feud logic, alternatives (royal courts, migration, conversion) are structurally closed. Resistance is moderate (0.5) — feud participants resist individual killings but cannot escape the systemic cycle without abandoning kin identity.
 *
 * PERSPECTIVAL GAP:
 *   The kin_group_elders seat experiences the constraint as agenda_setter with constrained exit — they administer the system but are trapped by it. The feud_participants seat experiences it as payer with identity_locked exit — they bear the costs and cannot leave without social death. The royal_authority seat experiences it as beneficiary with arbitrage exit — it extracts gains and can modulate enforcement intensity. The urban_merchant_classes seat experiences it as beneficiary with mobile exit — they gain from suppression and can relocate. The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Feud participants and kin group members are structural victims (payers): they lose resources and lives, have identity_locked exit, and face high suppression from both the feud cycle itself and royal counter-enforcement. Royal authority is the primary structural beneficiary: it gains monopoly on violence and tax base from the feuds' destructive persistence, with arbitrage-grade exit (it can choose suppression intensity). Kin group elders are dual-positioned: agenda_setters who administer the constraint but are constrained by it (secondary_role could be payer). Urban merchants are beneficiaries with mobile exit. Ecclesiastical authority is excluded — their theological frame is a sibling reading, not a structural position within this extraction cycle.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (stateless deterrence) is dead — state formation solved it centuries ago — yet the feud obligation persisted as an extraction mechanism. This is a classic mandatrophy case: the constraint's mandate outlived its function. The royal authority benefited from the delay in suppression (disorder legitimizes state-building), creating a perverse incentive to tolerate the extraction cycle while gradually capturing its gains. The classification prevents mislabeling this as pure coordination (rope) by documenting the asymmetric extraction (royal authority gains, kin groups lose) and the active enforcement required to maintain the royal monopoly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'How does the extraction_cycle_reading relate structurally to the other declared readings of the feud_obligation_kernel (stateless_coordination_reading, christianized_pacification_reading)?',
    'Map the structural delta for each reading: extraction_cycle_reading declares feud_participants as victims and royal_authority as beneficiaries with high suppression; stateless_coordination_reading declares feud_participants as beneficiaries with low suppression; christianized_pacification_reading declares divine_law as vindicated_proposition with ecclesiastical_authority as agenda_setter. The readings coexist as live analytical frames held by different communities (historical sociologists, legal anthropologists, theologians).',
    'If the readings foreclose each other, only one can be structurally true of the kernel. If they coexist, the kernel supports multiple valid constraint instantiations depending on the analytical frame. This determines whether the corpus treats them as a constraint family (linked via network.affects_constraints) or as competing claims requiring disambiguation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Structural relationship between sibling readings of the feud_obligation_kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the high suppression of kinship-based enforcement primarily structural (royal courts, bans on private war, state policing) or internalized (kin groups internalizing royal law, honor culture adapting to state norms)?',
    'Post-suppression trajectory analysis: in regions where royal authority collapsed (e.g., post-Roman Britain, Ottoman periphery), did feud obligations revive at pre-state intensity (structural suppression) or had the internalized norm decayed (internalized suppression)? Compare revival speed and form.',
    'If suppression is primarily internalized, the constraint''s effective suppression is higher than the structural measure suggests — kin groups carry the suppression with them after royal authority weakens. This would increase extractiveness (the cycle persists without external enforcement) and shift classification toward snare. If primarily structural, the constraint''s persistence depends on active royal enforcement, supporting tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the royal suppression of kinship enforcement.').

omega_variable(
    coordination_extraction_boundary,
    'At what point does the residual coordination function (deterrence in stateless zones) become negligible relative to the extraction cycle, such that the constraint should be classified as snare rather than tangled_rope?',
    'Counterfactual measurement: in regions/states where royal suppression was weak or absent (e.g., Albanian highlands, Scottish borders, Montenegrin tribes), measure the net productive capacity of kin groups engaged in feuds vs. those that adopted alternative coordination (blood money, mediation, church courts). If net capacity is negative even without royal suppression, coordination function is negligible.',
    'If coordination function is negligible throughout the interval, the claimed_type should be snare, not tangled_rope. The current tangled_rope claim rests on the stateless_coordination_reading''s premise being partially true in early periods. This omega documents the threshold uncertainty.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the coordination function is genuine or cover — the tangled_rope vs. snare boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__extraction_cycle_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_extraction_tr_t0, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(feud_extraction_tr_t20, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(feud_extraction_tr_t40, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(feud_extraction_tr_t60, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement(feud_extraction_tr_t80, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 80, 0.38).
narrative_ontology:measurement(feud_extraction_tr_t100, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(feud_extraction_be_t0, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(feud_extraction_be_t20, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(feud_extraction_be_t40, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(feud_extraction_be_t60, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(feud_extraction_be_t80, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 80, 0.72).
narrative_ontology:measurement(feud_extraction_be_t100, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 100, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(feud_extraction_su_t0, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(feud_extraction_su_t20, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(feud_extraction_su_t40, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(feud_extraction_su_t60, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 60, 0.7).
narrative_ontology:measurement(feud_extraction_su_t80, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 80, 0.77).
narrative_ontology:measurement(feud_extraction_su_t100, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 100, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__extraction_cycle_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(feud_obligation_kernel__extraction_cycle_reading, 0.08).
narrative_ontology:affects_constraint(feud_obligation_kernel__extraction_cycle_reading, royal_monopoly_on_violence).
narrative_ontology:affects_constraint(feud_obligation_kernel__extraction_cycle_reading, state_fiscal_extraction).
narrative_ontology:affects_constraint(feud_obligation_kernel__extraction_cycle_reading, ecclesiastical_pacification_movements).

% DUAL FORMULATION NOTE:
% This constraint is one member of the feud_obligation_kernel family. The stateless_coordination_reading (constraint_id: feud_obligation_kernel__stateless_coordination_reading) claims the same obligation operates as genuine coordination with negligible extraction. The christianized_pacification_reading (constraint_id: feud_obligation_kernel__christianized_pacification_reading) claims it violates divine law and requires theological suppression. The three readings share the kernel but instantiate different constraints with different ε, beneficiaries, and victims. This reading's ε (0.75) is substantially higher than the stateless_coordination_reading's expected ε (~0.15) because the referent is assessed by this reading's lights: the standing arrangement IS the extraction cycle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(feud_obligation_kernel__extraction_cycle_reading, organized, 0.85).
constraint_indexing:directionality_override(feud_obligation_kernel__extraction_cycle_reading, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
