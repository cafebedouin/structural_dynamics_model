% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__conditional_responsibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__conditional_responsibility, []).

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
 *   constraint_id: westphalia_sovereignty__conditional_responsibility
 *   human_readable: Conditional Sovereignty — Responsibility to Protect (R2P)
 *   domain: international_law/political_theory/state_systems
 *
 * SUMMARY:
 *   The Responsibility to Protect (R2P) doctrine, unanimously adopted at the
 *   2005 World Summit, redefines sovereignty as conditional on a state's
 *   protection of its population from four crimes: genocide, war crimes,
 *   ethnic cleansing, and crimes against humanity. When a state 'manifestly
 *   fails' to protect, the international community (through the UNSC) may
 *   authorize collective action including military intervention. This
 *   constraint story captures the conditional_responsibility reading of the
 *   westphalia_sovereignty kernel. The norm presents as coordination
 *   (atrocity prevention) but operates with asymmetric extraction: great
 *   powers gain intervention discretion via veto control, while
 *   atrocity-affected populations bear both atrocity costs and intervention
 *   collateral damage. The 2011 Libya intervention (UNSCR 1973) is the
 *   pivotal case — authorized as civilian protection, executed as regime
 *   change — driving theater_ratio up and exposing the
 *   extraction-coordination tension.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__conditional_responsibility, 0.68).
domain_priors:suppression_score(westphalia_sovereignty__conditional_responsibility, 0.45).
domain_priors:theater_ratio(westphalia_sovereignty__conditional_responsibility, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, extractiveness, 0.68).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__conditional_responsibility, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__conditional_responsibility, "Conditional Sovereignty — Responsibility to Protect (R2P)").
narrative_ontology:topic_domain(westphalia_sovereignty__conditional_responsibility, "international_law/political_theory/state_systems").

domain_priors:requires_active_enforcement(westphalia_sovereignty__conditional_responsibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__conditional_responsibility, '0fd353d3-aa6c-421d-834e-f6d0e03af731').
narrative_ontology:cs_kernel_codification('0fd353d3-aa6c-421d-834e-f6d0e03af731', formalized).
narrative_ontology:cs_authority_grounding('0fd353d3-aa6c-421d-834e-f6d0e03af731', lineage).
narrative_ontology:cs_interpretation_layer_present('0fd353d3-aa6c-421d-834e-f6d0e03af731').
narrative_ontology:cs_reading_relation('0fd353d3-aa6c-421d-834e-f6d0e03af731', westphalia_sovereignty__absolute_non_intervention, forecloses).
narrative_ontology:cs_reading_relation('0fd353d3-aa6c-421d-834e-f6d0e03af731', westphalia_sovereignty__graded_sovereignty, coexists_with).
narrative_ontology:cs_axiom('0fd353d3-aa6c-421d-834e-f6d0e03af731', foundational, sovereignty_entails_responsibility_to_protect).
narrative_ontology:cs_axiom_status(sovereignty_entails_responsibility_to_protect, holdable).
narrative_ontology:cs_axiom_grounding('0fd353d3-aa6c-421d-834e-f6d0e03af731', sovereignty_entails_responsibility_to_protect, conventional).
narrative_ontology:cs_axiom('0fd353d3-aa6c-421d-834e-f6d0e03af731', foundational, international_community_adjudicates_intervention_legitimacy).
narrative_ontology:cs_axiom_status(international_community_adjudicates_intervention_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('0fd353d3-aa6c-421d-834e-f6d0e03af731', international_community_adjudicates_intervention_legitimacy, conventional).
narrative_ontology:cs_reference_frame('0fd353d3-aa6c-421d-834e-f6d0e03af731', r2p_conditional_sovereignty_framework).
narrative_ontology:cs_drift_state('0fd353d3-aa6c-421d-834e-f6d0e03af731', contemporary_selective_enforcement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0fd353d3-aa6c-421d-834e-f6d0e03af731', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, global_governance_institutions).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, great_powers_p5).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, atrocity_affected_populations).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, target_states_sovereignty).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, sovereign_states_general).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__conditional_responsibility, sovereignty_entails_responsibility_to_protect).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__conditional_responsibility, international_community_adjudicates_intervention_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Populations experiencing or at risk of mass atrocities (genocide, war crimes, ethnic cleansing, crimes against humanity). They bear the primary costs of both the atrocities themselves and any subsequent intervention (collateral damage, displacement, political instability). No meaningful exit from the territory or the condition; their survival depends on external action they cannot control.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, atrocity_affected_populations, payer,
    powerless, immediate, trapped, local).

% Ad hoc or standing coalitions (NATO, regional organizations, 'coalitions of the willing') authorized by UNSC or claiming legitimacy under R2P. Gain legal-political authority to intervene militarily, access to target state territory/resources, and moral legitimacy otherwise unavailable. Can choose whether, when, and how to intervene — exit is arbitrage-grade (they initiate or decline).
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions, beneficiary,
    institutional, biographical, arbitrage, global).

% The sole body that can legally authorize Chapter VII enforcement action under the UN Charter. Adjudicates when R2P criteria are met, sets mandates, and can terminate mandates. Its P5 veto power means it controls whether the norm is activated. Does not bear intervention costs directly; its authority is the constraint's enforcement trigger.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, un_security_council, agenda_setter,
    institutional, generational, analytical, global).

% All UN member states cede absolute territorial inviolability in exchange for the collective security framework. They lose the Westphalian shield against external judgment of internal conduct but gain a normative structure that also protects them from arbitrary intervention. Exit is constrained — withdrawal from UN system carries massive diplomatic/economic costs.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, sovereign_states_general, payer,
    organized, biographical, constrained, national).

% Permanent UNSC members (US, UK, France, Russia, China) hold veto power over any authorization. They can block interventions against themselves or allies, and authorize interventions against adversaries — extracting strategic advantage from the same norm that constrains others. Their exit from the constraint's discipline is arbitrage-grade: they write the exceptions.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, great_powers_p5, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__conditional_responsibility, great_powers_p5, agenda_setter).

% States (primarily Global South, Non-Aligned Movement) that oppose R2P as a vehicle for great-power interventionism. They would object to the lowered intervention threshold and selective application but are structurally excluded from the adjudicative center (UNSC). Their sovereignty is the one most exposed to intervention; their voices are marginalized in authorization decisions.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, non_interventionist_states, excluded,
    organized, biographical, constrained, regional).

% Academics, jurists, and NGOs who interpret, critique, and document the norm's evolution. They do not collect rents or bear costs directly but shape the interpretive environment in which UNSC and states operate. Their exit is analytical — they can change frameworks without material penalty.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, international_legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of atrocity prevention by converting sovereignty from a shield into a conditional license: states that protect populations retain inviolability; states that perpetrate or permit mass atrocities forfeit it, triggering a (theoretically) collective international response rather than ad hoc unilateralism.
% TRANSFER_FUNCTION: Moves adjudicative authority over territorial integrity from the absolute sovereign to the international community (operationalized through UNSC). Moves the material costs and risks of intervention from the atrocity-affected population (who previously bore them alone) to intervening coalitions and the international system. Moves political discretion over intervention timing/scope to P5 great powers via veto.
% ABSENT_VOICES: Populations in atrocity situations where no intervention occurred (Syria post-2011, Myanmar/Rohingya, Xinjiang, Tigray, Sudan/Darfur) — they would object to the norm's selective non-application. Non-Aligned Movement states systematically excluded from UNSC authorization decisions — they would object to the great-power gatekeeping. Future generations who inherit the precedent of conditional sovereignty — not present to contest the long-term erosion of the non-intervention principle.
% DISAPPEARANCE_RATIONALE: If R2P/conditional sovereignty vanished overnight, the legal basis for UNSC-authorized humanitarian intervention (Libya 2011, Côte d'Ivoire 2011, Mali 2013, CAR 2013) would disappear. Intervention would revert to pure veto politics or unilateral 'humanitarian intervention' claims with no collective legitimacy framework. Atrocity prevention would lose its only universal normative hook; the UN system would lose its post-2005 doctrinal core.
% FOUNDING_PROBLEM: The 1994 Rwandan genocide and 1995 Srebrenica massacre occurred despite UN presence and the Genocide Convention. The international community had legal obligations but no operational framework to overcome the sovereignty barrier. Kofi Annan's 1999-2000 challenge — 'if humanitarian intervention is an unacceptable assault on sovereignty, how should we respond to a Rwanda, to a Srebrenica?' — crystallized the founding problem: how to reconcile sovereignty with the imperative to protect populations from mass atrocities.
% FOUNDING_PROBLEM_CORROBORATION: The ICISS 2001 report (Canadian government-sponsored, independent commission) and 2005 World Summit Outcome Document (unanimous UNGA adoption) corroborate the founding problem from outside the direct beneficiary set. However, the Non-Aligned Movement's 2009-2012 UNGA debates and the 2011 Libya intervention aftermath (where R2P was seen as regime-change cover) constitute counter-corroboration: the problem is contested as either 'still live' (atrocities continue) or 'solved in principle but hijacked in practice'.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__conditional_responsibility, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__conditional_responsibility, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__conditional_responsibility, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(westphalia_sovereignty__conditional_responsibility, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__conditional_responsibility, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__conditional_responsibility_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__conditional_responsibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__conditional_responsibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the norm's transfer of sovereign prerogative from target states to interveners, and the material extraction from affected populations (lives, displacement) when interventions occur. Suppression (0.45) is moderate — the constraint doesn't coercively suppress alternatives (diplomacy, sanctions, ICC referral remain available) but does foreclose the absolute non-intervention alternative. Theater_ratio (0.38) captures the gap between R2P's protective rhetoric and selective enforcement: Libya 2011 vs. Syria 2011-present, Myanmar 2017-present, where the same norm yields opposite outcomes based on P5 interests. Accessibility_collapse (0.48) is moderate — non-intervention, diplomatic pressure, and ICC referral remain live alternatives, but R2P has become the dominant framing. Resistance (0.75) is high: sustained opposition from NAM, BRICS, and sovereignty-absolutist states; repeated UNGA debates challenging selective application.
 *
 * PERSPECTIVAL GAP:
 *   From the atrocity-affected population seat, the constraint is experienced as a tangled rope at best (coordination promise, extraction reality) or a snare at worst (selective intervention that compounds harm). From the intervention coalition seat, it appears as a rope (genuine coordination enabling legitimate action). From the UNSC/P5 seat, it functions as a scaffold (transitional legitimacy for power projection) or piton (inertial framework maintained for veto leverage). The engine computes this seat divergence from the structural power/exit data authored above.
 *
 * DIRECTIONALITY LOGIC:
 *   Atrocity-affected populations are structural payers (trapped, powerless, immediate horizon) — they bear atrocity costs and intervention externalities with no exit. Humanitarian intervention coalitions and P5 great powers are structural beneficiaries (institutional/powerful, arbitrage exit) — they gain authorization authority, strategic discretion, and moral legitimacy. UNSC is agenda_setter (institutional, analytical exit) — it controls activation. Sovereign states general are payers (organized, constrained exit) — they cede absolute inviolability. Non-interventionist states are excluded (organized, constrained exit) — their sovereignty is most exposed, their voices absent from authorization. The directionality derivation from beneficiary/victim + exit options correctly places populations at the target end (high d) and interveners at the beneficiary end (low d).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Rwanda/Srebrenica regulatory gap) remains live — atrocities continue in Syria, Myanmar, Sudan, etc. But the arrangement has acquired mandatrophy: the norm persists as a legitimating vocabulary for great-power intervention discretion rather than as an operational atrocity-prevention mechanism. The theater_ratio rise post-2011 tracks this drift. The constraint is not 'resolved' — it is contested whether the coordination function can be decoupled from the extraction structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_conditional_responsibility,
    'How does the conditional_responsibility reading''s classification change if the kernel''s other readings (absolute_non_intervention, graded_sovereignty) are treated as live alternative framings rather than resolved?',
    'Run the classification engine on all three readings as separate constraint stories with their own ε, stakeholders, and metrics; compare seat divergences and network contamination via affects_constraints edges.',
    'If absolute_non_intervention classifies as mountain (natural law of non-intervention) and conditional_responsibility as tangled_rope, the kernel itself is a false summit candidate — the ''single Westphalian sovereignty'' label conceals structurally distinct constraints. The omega documents the committer-frame decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_conditional_responsibility, conceptual, 'Commitment-system framing under-determination: whether the kernel label ''Westphalian sovereignty'' conceals multiple constraints with divergent ε.').

omega_variable(
    selective_application_extraction,
    'Is the measured extractiveness (0.68) driven by the norm''s inherent structure or by its selective application by P5 great powers?',
    'Counterfactual: compare extractiveness in authorized interventions (Libya 2011, Côte d''Ivoire 2011) vs. non-intervention cases (Syria, Myanmar) where the norm''s protective promise was not activated. If extraction only appears in authorized cases, the norm itself may be lower-ε; the high ε reflects P5 gatekeeping.',
    'If extraction is gatekeeping-dependent, the constraint is a tangled_rope where the coordination function is real but capture is contingent. If extraction is inherent, the coordination story is cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_application_extraction, empirical, 'Whether high extractiveness is structural or contingent on great-power selectivity.').

omega_variable(
    intervention_effectiveness_population_outcomes,
    'Do R2P-authorized interventions actually improve population protection outcomes, or do they transfer harm from atrocity to intervention?',
    'Longitudinal study of civilian mortality, displacement, and political stability in R2P-intervention cases (Libya, Côte d''Ivoire, Mali, CAR) vs. matched non-intervention atrocity cases (Syria, Myanmar pre-2021).',
    'If interventions consistently reduce net harm, the coordination function is vindicated and ε reflects necessary cost. If interventions compound harm, the coordination story is cover and the constraint is snare-like from the population seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intervention_effectiveness_population_outcomes, empirical, 'Whether the coordination function (atrocity prevention) is empirically realized or aspirational.').

omega_variable(
    suppression_mechanism_non_intervention_foreclosure,
    'Is the suppression of the absolute_non_intervention alternative structural (UN Charter Chapter VII architecture) or internalized (states self-censor sovereignty claims due to R2P normative pressure)?',
    'Track UNGA voting patterns and state rhetoric on sovereignty over 2001-2024: if non-interventionist rhetoric persists but voting shifts, suppression is internalized; if voting and rhetoric both shift, suppression is structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure — the target (sovereign autonomy) carries the suppression internally. This would increase effective extraction for the sovereign_states_general seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_non_intervention_foreclosure, empirical, 'Structural vs. internalized suppression of the absolute sovereignty alternative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__conditional_responsibility, 2001, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t2001, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2001, 0.15).
narrative_ontology:measurement(west_tr_t2005, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2005, 0.22).
narrative_ontology:measurement(west_tr_t2011, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2011, 0.42).
narrative_ontology:measurement(west_tr_t2013, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2013, 0.38).
narrative_ontology:measurement(west_tr_t2017, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2017, 0.45).
narrative_ontology:measurement(west_tr_t2024, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(west_be_t2001, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2001, 0.35).
narrative_ontology:measurement(west_be_t2005, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2005, 0.45).
narrative_ontology:measurement(west_be_t2011, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2011, 0.62).
narrative_ontology:measurement(west_be_t2013, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2013, 0.58).
narrative_ontology:measurement(west_be_t2017, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2017, 0.65).
narrative_ontology:measurement(west_be_t2024, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t2001, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2001, 0.2).
narrative_ontology:measurement(west_su_t2005, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2005, 0.35).
narrative_ontology:measurement(west_su_t2011, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2011, 0.55).
narrative_ontology:measurement(west_su_t2013, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2013, 0.48).
narrative_ontology:measurement(west_su_t2017, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2017, 0.5).
narrative_ontology:measurement(west_su_t2024, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__conditional_responsibility, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalia_sovereignty__conditional_responsibility, 0.12).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, unsc_veto_power).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, icc_jurisdiction).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, humanitarian_aid_access).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty__absolute_non_intervention).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty__graded_sovereignty).

% DUAL FORMULATION NOTE:
% Part of the westphalia_sovereignty constraint family. This reading (conditional_responsibility) lowers the intervention threshold and vests adjudicative authority in UNSC. The absolute_non_intervention reading maintains the Westphalian shield (mountain-like ε). The graded_sovereignty reading treats sovereignty as capacity-scalar (tangled_rope with different beneficiary/victim structure). All three share the kernel but instantiate different constraints with different ε. Linkage via affects_constraints enables contamination analysis: if conditional_responsibility's theater_ratio rises, it may degrade the absolute_non_intervention reading's mountain certification by normalizing intervention.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalia_sovereignty__conditional_responsibility, institutional, 0.15).
constraint_indexing:directionality_override(westphalia_sovereignty__conditional_responsibility, powerful, 0.1).
constraint_indexing:directionality_override(westphalia_sovereignty__conditional_responsibility, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
