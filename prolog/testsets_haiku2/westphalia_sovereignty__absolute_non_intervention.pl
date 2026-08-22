% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__absolute_non_intervention
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: westphalia_sovereignty__absolute_non_intervention
 *   human_readable: Westphalian Sovereignty: Absolute Non-Intervention Doctrine
 *   domain: international_law/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested Westphalia
 *   kernel: the absolute non-intervention doctrine, in which external
 *   interference in domestic affairs is categorically illegitimate regardless
 *   of internal conduct. This is the reading most favorable to state elites
 *   (democratic and authoritarian alike) and least favorable to persecuted
 *   populations. The doctrine emerged from the 17th-century coordination
 *   problem of preventing mutual empire through mutual recognition of
 *   territorial inviolability. However, its persistence into the 21st century
 *   occurs in a vastly different context where the founding coordination
 *   problem is largely solved (mutual recognition is established) but
 *   atrocities and mass violence within states remain common. The constraint
 *   is claimed as tangled_rope (coordination + enforcement), but the authored
 *   metrics suggest extractiveness has grown as the founding coordination
 *   function has withered and the doctrine increasingly shields atrocities.
 *   The claim/metric gap is intentional and diagnostic: the engine will
 *   compute divergence that reveals whether the doctrine still performs real
 *   coordination or now functions primarily as cover for atrocities.
 *
 * KEY AGENTS:
 *   - state_elites (both democratic and authoritarian) — primary beneficiaries and agenda-setters
 *   - authoritarian regimes — direct beneficiaries, depend on doctrine for domestic monopoly on legitimate force
 *   - persecuted_populations — powerless, trapped, primary victims and payers
 *   - internal_dissidents — identity-locked, immediate time horizon, cannot exit without betraying their own cause
 *   - western_democracies — powerful beneficiaries with selective override capacity; agenda-setters who established the doctrine historically
 *   - humanitarian_advocates — excluded from governance; document and contest the doctrine
 *   - international legal order — observer; the institutional machinery through which the constraint is enforced
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, 0.81).
domain_priors:suppression_score(westphalia_sovereignty__absolute_non_intervention, 0.76).
domain_priors:theater_ratio(westphalia_sovereignty__absolute_non_intervention, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, extractiveness, 0.81).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__absolute_non_intervention, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__absolute_non_intervention, "Westphalian Sovereignty: Absolute Non-Intervention Doctrine").
narrative_ontology:topic_domain(westphalia_sovereignty__absolute_non_intervention, "international_law/political_theory").

domain_priors:requires_active_enforcement(westphalia_sovereignty__absolute_non_intervention).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__absolute_non_intervention, '18c69e9f-bfd3-42ee-b412-00804e854064').
narrative_ontology:cs_kernel_codification('18c69e9f-bfd3-42ee-b412-00804e854064', formalized).
narrative_ontology:cs_authority_grounding('18c69e9f-bfd3-42ee-b412-00804e854064', lineage).
narrative_ontology:cs_interpretation_layer_present('18c69e9f-bfd3-42ee-b412-00804e854064').
narrative_ontology:cs_reading_relation('18c69e9f-bfd3-42ee-b412-00804e854064', westphalia_sovereignty__conditional_responsibility, coexists_with).
narrative_ontology:cs_reading_relation('18c69e9f-bfd3-42ee-b412-00804e854064', westphalia_sovereignty__graded_sovereignty, coexists_with).
narrative_ontology:cs_axiom('18c69e9f-bfd3-42ee-b412-00804e854064', foundational, categorical_territorial_inviolability).
narrative_ontology:cs_axiom_status(categorical_territorial_inviolability, holdable).
narrative_ontology:cs_axiom_grounding('18c69e9f-bfd3-42ee-b412-00804e854064', categorical_territorial_inviolability, conventional).
narrative_ontology:cs_axiom('18c69e9f-bfd3-42ee-b412-00804e854064', foundational, non_interference_per_se_legitimate).
narrative_ontology:cs_axiom_status(non_interference_per_se_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('18c69e9f-bfd3-42ee-b412-00804e854064', non_interference_per_se_legitimate, deontological).
narrative_ontology:cs_reference_frame('18c69e9f-bfd3-42ee-b412-00804e854064', westphalian_mutual_recognition).
narrative_ontology:cs_drift_state('18c69e9f-bfd3-42ee-b412-00804e854064', contemporary_atrocity_era, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('18c69e9f-bfd3-42ee-b412-00804e854064', '2026-06-19T14:32:00Z').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, state_elites).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, authoritarian_regimes).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, persecuted_populations).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, internal_dissidents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, western_democracies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a norm that shields internal governance from external scrutiny and intervention. Can suppress internal dissent, manage resources without external pressure, and maintain monopolies on legitimate force within their territory. The absolute non-intervention reading provides legal cover for domestic atrocities by rendering them 'internal affairs.' Elites of both democratic and authoritarian states invoke the doctrine, though authoritarian regimes depend on it more heavily since democratic states face stronger internal constraints.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, state_elites, agenda_setter,
    institutional, generational, arbitrage, global).

% Directly depend on non-intervention norms to conduct systematic repression without facing external military, economic, or judicial consequences. Mass atrocities, ethnic cleansing, and political imprisonment remain shielded from intervention as 'sovereign prerogatives.' Their territorial control and monopoly on legitimate violence depend entirely on the doctrine's enforcement.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, authoritarian_regimes, beneficiary,
    institutional, biographical, trapped, national).

% Bear the direct costs of the non-intervention doctrine: systematic repression, ethnic violence, genocide, and mass displacement remain unaddressed by external actors. They are trapped geographically and have no exit option except refugee status or death. The constraint's persistence means their suffering is reframed as an internal matter beyond external remedy.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, persecuted_populations, payer,
    powerless, biographical, trapped, local).

% Political opposition, human rights activists, and resistance movements face suppression without threat of external intervention. Their identity as citizens of the state binds them to the territory under the doctrine; escape is understood as betrayal or treason. The non-intervention principle ensures their oppression remains within the jurisdiction of their oppressor.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, internal_dissidents, payer,
    powerless, immediate, identity_locked, national).

% Historically benefited from the non-intervention norm by establishing it as the governing principle of international law, then selectively invoking humanitarian exceptions and 'responsibility to protect' doctrines when strategic interests align. Their power allows them to maintain plausible deniability: they can claim the doctrine's neutrality while having the military and economic capacity to override it when convenient. The doctrine protects their own internal affairs from external pressure while reserving coercive power over weaker states.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, western_democracies, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__absolute_non_intervention, western_democracies, agenda_setter).

% International human rights organizations, NGOs, and transnational advocacy networks are systematically excluded from the sovereignty calculus. They document atrocities, demand intervention, and contest the doctrine's legitimacy, but lack the institutional standing or enforcement power to override it. Their exclusion from the governance framework ensures their moral arguments cannot compel action.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, humanitarian_advocates, excluded,
    moderate, biographical, constrained, global).

% The abstracted system of rules and norms that codifies the non-intervention principle in the UN Charter and customary international law. The constraint operates through this institutional machinery; its persistence depends on the legal order's enforcement capacity and the rhetorical alignment between the doctrine and the legitimacy claims of powerful states.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, international_legal_order, observer,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(westphalia_sovereignty__absolute_non_intervention, international_legal_order).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalia_sovereignty__absolute_non_intervention, state_elites).
narrative_ontology:fixing_cost_class(westphalia_sovereignty__absolute_non_intervention, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables a stable international system in which multiple sovereigns coexist without constant military intervention: if territorial inviolability were not recognized as a baseline principle, larger or more militarily capable states would have continuous license to override smaller states' internal governance, making stable long-term state existence impossible. The non-intervention norm solves the coordination problem of mutual defense against empire.
% TRANSFER_FUNCTION: Transfers authority over populations from international bodies and humanitarian coalitions to state elites. Moves the legitimacy to define justice, punishment, and remedies from external actors (courts, human rights bodies, foreign governments) to the state apparatus itself. Extracts compliance and silence from persecuted populations in exchange for the state's abstract 'right' to conduct internal affairs.
% ABSENT_VOICES: Persecuted populations and internal dissidents have no seat at the table where the doctrine is invoked and defended; their testimony about atrocities and repression is framed as an internal political matter, not grounds for doctrine revision. Humanitarian advocates document abuses but lack standing in the sovereignty framework. Rival interpretations of sovereignty (conditional responsibility, graded sovereignty) are excluded from the absolute reading's own legitimacy structure.
% DISAPPEARANCE_RATIONALE: If absolute non-intervention doctrine vanished, the international legal order would reorganize around humanitarian intervention rights, responsibility-to-protect principles, or conditional sovereignty frameworks. The global system's stability would shift: some authoritarian regimes would face military intervention or sanctions; humanitarian agencies and transnational courts would gain enforcement standing; borders would become less sacrosanct when internal atrocities cross explicit thresholds. State elites would lose a primary shield against external accountability.
% FOUNDING_PROBLEM: After the Wars of Religion in Europe (16th–17th centuries), the fundamental coordination problem was preventing constant military intervention by neighboring powers in domestic religious and political matters. The Treaty of Westphalia (1648) and subsequent international law established mutual recognition of territorial inviolability as the price of coexistence: if I recognize your right to settle your internal affairs without my interference, you recognize mine. The doctrine solved the problem of preventing empire by mutual agreement.
% FOUNDING_PROBLEM_CORROBORATION: International law scholars and state diplomats attest that mutual sovereignty prevents constant warfare and empire. However, humanitarian organizations, victims' advocates, and competing international law scholars attest that the problem has shifted: the founding coordination problem (preventing religious wars in Europe) is solved; the constraint now persists primarily to shield atrocities from intervention. The UN Charter's Preamble invokes human rights; competing charter articles invoke non-intervention. This structural contradiction is documented in legal scholarship outside benefiting parties (e.g., Evans, Anghie, Chimni on sovereignty and imperialism).
narrative_ontology:disappearance_verdict(westphalia_sovereignty__absolute_non_intervention, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__absolute_non_intervention, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__absolute_non_intervention, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(westphalia_sovereignty__absolute_non_intervention, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__absolute_non_intervention, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.81 at interval end) and rising because the doctrine increasingly shields atrocities rather than solving the founding coordination problem. The founding problem (preventing mutual interference and empire) was solved by establishing the doctrine; the constraint's persistence now serves to immunize state elites from accountability. Suppression is high (0.76) because the doctrine's enforcement requires silencing humanitarian calls for intervention and excluding persecuted populations from the international legal conversation. Theater is moderate (0.42) and rising because Western democracies increasingly invoke humanitarian exceptions and responsibility-to-protect doctrines while maintaining the absolute non-intervention principle as the baseline, creating theatrical consistency (the doctrine is stable in principle) while their practice becomes increasingly selective. Accessibility_collapse is moderate (0.68) because alternative interpretations exist (conditional_responsibility and graded_sovereignty readings) and are live in international legal discourse, yet the absolute reading maintains institutional dominance. Resistance is substantial (0.59) because humanitarian organizations, victim advocacy groups, and scholars mount real intellectual and institutional challenges to the doctrine; the doctrine is not accepted as natural law but actively defended.
 *
 * PERSPECTIVAL GAP:
 *   From the state-elite seat, the doctrine is genuine coordination: mutual recognition enables stable coexistence and prevents empire. From the persecuted-population seat, the doctrine is categorical exclusion: their suffering is defined out of the international conversation as an internal matter. From the humanitarian-advocate seat, the doctrine is strategically incoherent: Western democracies invoke it against rivals but override it for strategic interests, revealing it as a tool of selective power. From the international-legal-order seat, the doctrine is the foundation of the system's legitimacy and stability. These perspectives are not different readings of the same fact; they are structural positions in the constraint's operation. The engine computes per-seat classifications; the authored metrics describe the structure that produces these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   State elites (institutional power, arbitrage exit) are structural beneficiaries: d = 0.1–0.2. They set the agenda, collect the benefit of unaccountable domestic governance, and can exit by adopting alternative sovereignty readings whenever convenient. Authoritarian regimes (institutional power, trapped exit) are even more dependent beneficiaries: d = 0.05–0.15, since they lack the power to override the doctrine if it turned against them. Persecuted populations (powerless, trapped exit) are full targets: d = 0.90–1.0. They bear all costs, have zero ability to exit, and their interests are structurally excluded from the governance framework. Internal dissidents (powerless, identity_locked) are similarly full targets: d = 0.85–0.95. The identity_lock is crucial here: they cannot exit without violating their own political identity as citizens with rights to domestic reform. Western democracies occupy a dual position: as beneficiaries of the doctrine's original establishment (d = 0.2–0.3) but also as powerful enough to override it when convenient, giving them arbitrage-grade exit and effective d modulation downward via selective application. The engine's directionality derivation should capture this institutional power asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The foundational mandatrophy question: Did the Westphalian coordination problem (preventing mutual interference and empire) persist to the interval's end, or was it solved early and the doctrine continued for other reasons? If solved, the constraint is mandatrophic: coordination machinery (the non-intervention norm) persisting after its coordinating function is obsolete. The measurement series shows extractiveness rising and theater ratio rising while suppression plateaus — a pattern consistent with an originally-coordinating mechanism becoming increasingly extractive without the founding coordination problem updating. The rising extractiveness despite stable suppression suggests the constraint is hardening in its function (more thoroughly shielding atrocities) rather than simply being more heavily enforced. If the founding problem (mutual empire) is solved by the mere establishment of the doctrine and mutual recognition of sovereignty (which appears to have happened by ~1945 with the UN Charter), then everything after that point is mandatrophic extraction. The doctrine would be the quintessential example of a constraint that solved its problem so completely that its own success made it obsolete — but it persists because the beneficiaries (state elites) have invested in its institutional machinery and because revising it would require a coalition among states with no incentive to revise.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_death_vs_persistence,
    'This reading asserts that the Westphalian non-intervention doctrine solves the 17th-century coordination problem of preventing mutual empire. Is that founding problem still live, or was it solved decades ago and the doctrine now persists for different reasons?',
    'Historical institutional analysis: examine the doctrine''s actual function in post-WWII international relations. If invoked primarily to prevent mutual interference (founding problem), it remains coordinating. If invoked primarily to shield atrocities from intervention, the founding problem is obsolete and the constraint is extractive cover.',
    'If the founding problem is dead, the constraint reclassifies from tangled_rope (coordination + enforcement) toward snare (pure extraction with coordination cover). This is the core mandatrophy problem.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_death_vs_persistence, empirical, 'Whether the doctrine''s founding coordination problem persists or has been solved and superseded.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__absolute_non_intervention, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 0, 0.28).
narrative_ontology:measurement(west_tr_t12, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 12, 0.31).
narrative_ontology:measurement(west_tr_t25, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 25, 0.36).
narrative_ontology:measurement(west_tr_t37, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 37, 0.39).
narrative_ontology:measurement(west_tr_t50, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 50, 0.41).
narrative_ontology:measurement(west_tr_t62, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 62, 0.42).
narrative_ontology:measurement(west_tr_t75, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 75, 0.42).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(west_be_t12, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(west_be_t25, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 25, 0.74).
narrative_ontology:measurement(west_be_t37, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 37, 0.78).
narrative_ontology:measurement(west_be_t50, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 50, 0.79).
narrative_ontology:measurement(west_be_t62, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 62, 0.81).
narrative_ontology:measurement(west_be_t75, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 75, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 0, 0.64).
narrative_ontology:measurement(west_su_t12, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(west_su_t25, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(west_su_t37, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 37, 0.74).
narrative_ontology:measurement(west_su_t50, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 50, 0.75).
narrative_ontology:measurement(west_su_t62, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 62, 0.76).
narrative_ontology:measurement(west_su_t75, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 75, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__absolute_non_intervention, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalia_sovereignty__absolute_non_intervention, 0.12).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty__conditional_responsibility).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty__graded_sovereignty).

% DUAL FORMULATION NOTE:
% The Westphalia kernel constrains three distinct constraint stories, each instantiating a different reading. This story (absolute_non_intervention) asserts categorical territorial inviolability. The conditional_responsibility reading makes inviolability conditional on atrocity prevention. The graded_sovereignty reading treats inviolability as a scalar dependent on state capacity. All three stories share the kernel (the Westphalian principle of territorial sovereignty) but diverge on what that principle permits regarding intervention. They are linked via network.affects_constraints and documented in sibling cs_structure.reading_relations. The ε values differ: absolute_non_intervention ε=0.81 (high extraction because it categorically excludes intervention remedies); conditional_responsibility ε=0.42 (moderate extraction because atrocity exceptions reduce immunization); graded_sovereignty ε=0.55 (moderate-high extraction because the spectrum enables selective application by powerful states). The structural difference is not in the doctrine itself but in what the same doctrine permits regarding the exceptions to it. ε-invariance is preserved: each reading has one stable ε assessing the standing arrangement under that reading's lights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalia_sovereignty__absolute_non_intervention, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
