% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__withdrawal_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_1970__withdrawal_sovereignty_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: npt_treaty_1970__withdrawal_sovereignty_reading
 *   human_readable: NPT Article X Withdrawal Right as Legitimate Sovereign Option
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   The NPT (1970) contains Article X, which permits any party to withdraw
 *   upon deciding 'that extraordinary events, related to the subject matter
 *   of this Treaty, have jeopardized the supreme interests of its country.'
 *   The withdrawal_sovereignty_reading treats this as a genuine, exercisable
 *   sovereign right — not a dead letter, not a provision so hedged by
 *   customary law as to be inoperable. Under this reading, the NPT's
 *   obligations are contingent on the security environment: if a party's
 *   supreme interests are jeopardized, the treaty ceases to bind. This
 *   reading is held by threshold nuclear states (and their great-power
 *   patrons) who value the option to acquire nuclear weapons if the security
 *   environment deteriorates. The constraint coordinates nonproliferation
 *   (the NPT's overt function) while extracting from non-nuclear weapon
 *   states, who must accept a bargain that can be unilaterally revoked by the
 *   most security-threatened parties. The regime stability norm — the shared
 *   expectation that the NPT is a permanent, binding framework — is a
 *   structural victim: its erosion undermines the coordination function and
 *   leaves extraction unchecked.
 *
 * KEY AGENTS:
 *   - threshold_nuclear_states: Primary beneficiaries (powerful/identity_locked) — hold withdrawal option as strategic asset
 *   - great_power_patrons: Secondary beneficiaries (institutional/arbitrage) — use threshold states' withdrawal option as leverage
 *   - non_nuclear_weapon_states_parties: Primary victims (organized/constrained) — bear compliance costs while bargain is revocable
 *   - regime_stability_norm: Structural victim — erodes when withdrawal option is exercised or credibly threatened
 *   - nuclear_weapon_states_npt: Agenda setters (institutional/arbitrage) — administer the regime while benefiting from its asymmetry
 *   - international_observers_analytical: Observer seat (analytical/analytical) — track regime health
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__withdrawal_sovereignty_reading, 0.38).
domain_priors:suppression_score(npt_treaty_1970__withdrawal_sovereignty_reading, 0.42).
domain_priors:theater_ratio(npt_treaty_1970__withdrawal_sovereignty_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__withdrawal_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__withdrawal_sovereignty_reading, "NPT Article X Withdrawal Right as Legitimate Sovereign Option").
narrative_ontology:topic_domain(npt_treaty_1970__withdrawal_sovereignty_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__withdrawal_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__withdrawal_sovereignty_reading, 'c9c90522-fac8-49f1-bada-c3314e4a3217').
narrative_ontology:cs_kernel_codification('c9c90522-fac8-49f1-bada-c3314e4a3217', formalized).
narrative_ontology:cs_authority_grounding('c9c90522-fac8-49f1-bada-c3314e4a3217', lineage).
narrative_ontology:cs_interpretation_layer_present('c9c90522-fac8-49f1-bada-c3314e4a3217').
narrative_ontology:cs_reading_relation('c9c90522-fac8-49f1-bada-c3314e4a3217', npt_treaty_1970__oligopoly_enforcement_reading, coexists_with).
narrative_ontology:cs_reading_relation('c9c90522-fac8-49f1-bada-c3314e4a3217', npt_treaty_1970__reciprocal_disarmament_reading, coexists_with).
narrative_ontology:cs_axiom('c9c90522-fac8-49f1-bada-c3314e4a3217', foundational, article_x_as_exercisable_sovereign_right).
narrative_ontology:cs_axiom_status(article_x_as_exercisable_sovereign_right, holdable).
narrative_ontology:cs_axiom_grounding('c9c90522-fac8-49f1-bada-c3314e4a3217', article_x_as_exercisable_sovereign_right, conventional).
narrative_ontology:cs_axiom('c9c90522-fac8-49f1-bada-c3314e4a3217', foundational, treaty_obligations_contingent_on_security_environment).
narrative_ontology:cs_axiom_status(treaty_obligations_contingent_on_security_environment, holdable).
narrative_ontology:cs_axiom_grounding('c9c90522-fac8-49f1-bada-c3314e4a3217', treaty_obligations_contingent_on_security_environment, instrumental).
narrative_ontology:cs_reference_frame('c9c90522-fac8-49f1-bada-c3314e4a3217', npt_as_voluntary_bargain_with_sovereign_exit).
narrative_ontology:cs_drift_state('c9c90522-fac8-49f1-bada-c3314e4a3217', post_2003_north_korea_withdrawal, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c9c90522-fac8-49f1-bada-c3314e4a3217', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_nuclear_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, great_power_patrons).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, non_nuclear_weapon_states_parties).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, regime_stability_norm).
narrative_ontology:constraint_vindicates(npt_treaty_1970__withdrawal_sovereignty_reading, sovereign_withdrawal_right_under_article_x).
narrative_ontology:constraint_vindicates(npt_treaty_1970__withdrawal_sovereignty_reading, security_environment_conditionality_of_treaty_obligations).
narrative_ontology:constraint_vindicates(npt_treaty_1970__withdrawal_sovereignty_reading, nuclear_deterrence_as_legitimate_security_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States with latent nuclear capacity (enrichment, reprocessing, weaponization knowledge) that remain non-nuclear weapon state parties but treat Article X as a live strategic option. Their nuclear latency is their leverage; the withdrawal right makes that leverage credible. They gain option value (deterrence potential, diplomatic leverage) without paying weaponization costs. Exit from the constraint means exercising Article X — which is identity-locked because nuclear latency has become constitutive of their security doctrine and great-power alignment.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_nuclear_states, beneficiary,
    powerful, biographical, identity_locked, national).

% Nuclear weapon states (especially US, Russia, China) that extend security umbrellas to threshold states. They benefit when clients' withdrawal threats extract concessions from adversaries, while maintaining plausible deniability. They administer the nonproliferation regime (IAEA, UNSC, export controls) and can modulate enforcement. Their exit is arbitrage-grade: they can reinterpret or ignore regime constraints when strategic interests demand, as great-power prerogative.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, great_power_patrons, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__withdrawal_sovereignty_reading, great_power_patrons, agenda_setter).

% The 180+ NNWS parties that have foregone nuclear weapons and accepted IAEA safeguards. They bear the compliance costs (safeguards, export controls, foregone nuclear option) under a bargain where the most powerful parties can legally withdraw. Their exit is constrained: withdrawing triggers severe structural penalties (sanctions, loss of nuclear cooperation, security guarantee withdrawal) but staying means accepting a revocable deal. They are coordinated into the regime but extractively positioned.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, non_nuclear_weapon_states_parties, payer,
    organized, generational, constrained, global).

% The shared normative expectation that the NPT is a permanent, binding framework whose obligations are not contingent on individual security calculations. This norm is not an agent but a structural property of the regime. It is victimized when withdrawal threats or exercises erode the expectation of permanence, degrading the coordination function (states hedge, compliance becomes conditional, verification loses authority). Its 'exit' is analytical: the norm persists or collapses as a system property.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, regime_stability_norm, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(npt_treaty_1970__withdrawal_sovereignty_reading, regime_stability_norm).

% The five NPT-recognized nuclear weapon states (US, Russia, UK, France, China) that administer the regime through the UNSC, IAEA Board of Governors, and export control regimes (NSG, Zangger Committee). They benefit from the regime's asymmetry (they retain nuclear weapons; others cannot acquire them) but are notionally bound by Article VI. Under this reading, their Article VI obligation is also contingent on security environment — making them partial beneficiaries of the same contingency logic that benefits threshold states.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, nuclear_weapon_states_npt, agenda_setter,
    institutional, generational, arbitrage, global).

% Analytical actors (IAEA Secretariat, think tanks, academic researchers, NGOs) who monitor regime health, verify compliance, and document the gap between the NPT's proclaimed permanence and Article X's operational reality. They neither collect nor pay but their assessments shape the regime's legitimacy and the credibility of withdrawal threats.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, international_observers_analytical, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The NPT coordinates global nonproliferation by providing a single legal framework for verification (IAEA safeguards), technology transfer (Article IV), and security assurances (negative/positive). It solves the collective action problem of mutual nuclear restraint: without it, more states would hedge or weaponize, triggering cascades.
% TRANSFER_FUNCTION: Moves compliance costs (safeguards acceptance, foregone nuclear option, export control adherence) from non-nuclear weapon states to the regime, while moving option value (credible withdrawal threat, nuclear latency as leverage) to threshold states and their patrons. The transfer is not monetary but strategic: security assurance for restraint, maintained only while the security environment permits.
% ABSENT_VOICES: Future generations who inherit the nuclear risk environment shaped by today's withdrawal decisions; states that never joined the NPT (India, Pakistan, Israel, South Sudan) and thus have no voice in its interpretation but are affected by its stability; civil society movements for nuclear abolition whose framing of the NPT as a disarmament treaty is undermined by the withdrawal reading's contingency logic.
% DISAPPEARANCE_RATIONALE: If the withdrawal_sovereignty_reading vanished (i.e., Article X were interpreted as a dead letter or so hedged by customary law as to be inoperable), threshold states would lose their strategic option value, great-power patrons would lose a lever, and the regime would become more symmetric — but non-nuclear weapon states might also lose the assurance that the NPT is a voluntary bargain they can exit if betrayed. The world rearranges because the constraint's existence shapes the strategic calculus of every party.
% FOUNDING_PROBLEM: The NPT was built to prevent a world of 20+ nuclear weapon states (the 'nth country problem' projected in the 1960s). The withdrawal right (Article X) was the sovereign concession required to get states to join a treaty that permanently foreclosed their nuclear option — a guarantee that if the bargain failed them existentially, they were not trapped.
% FOUNDING_PROBLEM_CORROBORATION: The 1968 negotiating record (UNGA First Committee, Eighteen-Nation Disarmament Committee) corroborates that Article X was a sovereign insistence, not a regime design choice. The 1995 NPT Review Conference (indefinite extension decision) records the contestation: non-nuclear weapon states accepted indefinite extension only with 'principles and objectives' strengthening Article VI, precisely because they feared the bargain was becoming permanently asymmetric. The withdrawal_sovereignty_reading's claim that the founding problem is contested is corroborated by the Review Conference record, not merely asserted by beneficiaries.
narrative_ontology:disappearance_verdict(npt_treaty_1970__withdrawal_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__withdrawal_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__withdrawal_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(npt_treaty_1970__withdrawal_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__withdrawal_sovereignty_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_1970__withdrawal_sovereignty_reading_tests).
:- end_tests(npt_treaty_1970__withdrawal_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) reflects that the withdrawal option is not continuously exercised but structurally embedded — the threat of withdrawal is the extraction mechanism, not the act. Suppression (0.42) is moderate: non-nuclear weapon states comply partly because the regime delivers genuine coordination (security assurances, technology access) and partly because withdrawal threats trigger structural penalties. Theater ratio (0.28) captures the growing gap between the NPT's proclaimed permanence and the operational reality of Article X as a live option. The measurements show a clear trajectory: extraction and theater rise after the Cold War (1995) and especially after North Korea's 2003 withdrawal, as the regime's asymmetry becomes more visible. The reading is claimed as tangled_rope because it coordinates (nonproliferation, verification, technology transfer) AND extracts asymmetrically (threshold states hold option value; non-nuclear weapon states bear the cost of a revocable bargain). Active enforcement is required: the regime's verification machinery (IAEA safeguards) and the security assurance architecture (negative/positive security assurances) must actively suppress withdrawal incentives to maintain the coordination function.
 *
 * PERSPECTIVAL GAP:
 *   From the threshold state seat, the constraint is a sovereign insurance policy — the withdrawal right makes the NPT bearable, and its exercise is a legitimate response to existential threat. From the non-nuclear weapon state seat, the same structure is a trap: they complied in good faith, but the bargain's core symmetry (nonproliferation for disarmament) is broken when the most powerful parties treat their obligations as contingent. The engine computes this seat divergence from the declared power/exit/beneficiary structure; the claimed type (tangled_rope) reflects the author's structural reading, not a reconciliation of perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   Threshold nuclear states are structural beneficiaries (d ~ 0.15-0.25): they hold the withdrawal option as strategic leverage, gaining option value without paying the full cost of weaponization. Great power patrons are secondary beneficiaries (d ~ 0.2-0.3): they use client states' withdrawal threats as diplomatic leverage while maintaining plausible deniability. Non-nuclear weapon states are targets (d ~ 0.7-0.8): they bear compliance costs (foregone nuclear option, safeguards acceptance) under a bargain that can be unilaterally revoked by the most security-threatened parties. The regime_stability_norm is a structural victim (d ~ 0.9): its coherence is the direct casualty of withdrawal exercises. Nuclear weapon states sit near symmetric (d ~ 0.45-0.55): they administer the regime and benefit from its asymmetry but are bound by Article VI obligations (under other readings). The directionality derivation from beneficiary/victim declarations + exit options captures this gradient without overrides.
 *
 * MANDATROPHY ANALYSIS:
 *   The NPT was founded on a reciprocal bargain: non-nuclear weapon states forego nuclear weapons in exchange for access to peaceful nuclear technology and a binding commitment by nuclear weapon states to pursue disarmament (Article VI). Under the withdrawal_sovereignty_reading, the founding problem (preventing nuclear proliferation) is contested — some parties argue the problem is solved by the regime's existence, others that it persists and the regime is failing. The withdrawal right itself was not the founding problem's solution; it was a sovereign concession to get states to join. Its persistence as a live option, rather than a dead letter, means the constraint has not atrophied into a piton — it is actively maintained by threshold states as strategic optionality. The mandatrophy question is whether the regime's coordination function (nonproliferation) still justifies the asymmetric extraction (revocable bargain). This reading says yes — the coordination is real and the extraction is the price of universal participation. The sibling readings disagree.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the withdrawal_sovereignty_reading a distinct constraint with its own ε, or a perspectival slice of the NPT kernel?',
    'The ε-invariance test: if the extraction profile, beneficiary/victim structure, and classification remain stable under this reading''s own structural logic without reference to sibling readings, it is a distinct constraint. The fact that sibling readings produce different ε and different victims confirms distinctness.',
    'If confirmed as distinct, this reading is authored as a standalone constraint story with its own metrics and stakeholders, linked to siblings via network.affects_constraints. If treated as a perspectival slice, the metrics would be unstable and the framework would misclassify.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment to ε-invariance: this reading is a separate constraint, not a measurement variant of the kernel.').

omega_variable(
    withdrawal_threat_credibility_extraction,
    'Does the withdrawal right itself (as opposed to actual withdrawal) function as an extractive mechanism by allowing threshold states to hold compliance hostage?',
    'Trace concession patterns: do non-nuclear weapon states make disproportionate security assurances or technology transfers to states that credibly threaten Article X invocation? Compare North Korea (2003) and Iran (post-2018) concession landscapes against baseline.',
    'If credible, the withdrawal threat is not a dormant option but an active extraction lever — raising ε for this reading and strengthening the tangled_rope classification (coordination of nonproliferation + extraction via threat). If not, the reading overstates the structural function of Article X.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(withdrawal_threat_credibility_extraction, empirical, 'Whether the Article X threat operates as extractive leverage in practice.').

omega_variable(
    regime_norm_as_victim,
    'Can a normative standard (regime stability) be a victim in the same sense as an agent?',
    'The framework treats vindicated_propositions as non-agents that collect no rents. But regime_stability_norm is listed as a victim because its erosion has structural consequences: when the norm degrades, the coordination function collapses and extraction becomes unchecked. The omega records whether this analytical move is sound or whether regime stability should be modeled as a system property rather than a victim seat.',
    'If regime_stability_norm is a valid victim seat, the tangled_rope gate (coordination + asymmetric extraction) is satisfied with two victim categories. If invalid, the reading may need an additional agent victim (e.g., ''future_generations_entering_nuclear_risk'') to satisfy the gate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_norm_as_victim, conceptual, 'Ontological status of a normative standard as a victim of extraction.').

omega_variable(
    suppression_mechanism_article_x,
    'Is the suppression in this constraint structural (enforcement of non-withdrawal norms, security assurances that bind) or internalized (non-nuclear weapon states accept the bargain as legitimate)?',
    'Post-withdrawal trajectory analysis: if states that withdraw (or threaten to) face sustained structural penalties (sanctions, isolation, security guarantees withdrawn) rather than merely reputational costs, suppression is structural. If the primary cost is normative condemnation that fades, suppression is partly internalized.',
    'If internalized, the effective suppression experienced by non-nuclear weapon states is higher than the structural measure suggests — they carry the constraint''s legitimacy even when enforcement lapses. This affects directionality for the non_nuclear_weapon_states_parties seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_article_x, empirical, 'Structural vs. internalized suppression in the nonproliferation regime under the withdrawal reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__withdrawal_sovereignty_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(npt__tr_t1985, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(npt__tr_t1995, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(npt__tr_t2003, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 2003, 0.24).
narrative_ontology:measurement(npt__tr_t2015, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 2015, 0.26).
narrative_ontology:measurement(npt__tr_t2024, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 1970, 0.18).
narrative_ontology:measurement(npt__be_t1985, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 1985, 0.22).
narrative_ontology:measurement(npt__be_t1995, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 1995, 0.28).
narrative_ontology:measurement(npt__be_t2003, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 2003, 0.35).
narrative_ontology:measurement(npt__be_t2015, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 2015, 0.37).
narrative_ontology:measurement(npt__be_t2024, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 2024, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 1970, 0.35).
narrative_ontology:measurement(npt__su_t1985, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 1985, 0.38).
narrative_ontology:measurement(npt__su_t1995, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 1995, 0.4).
narrative_ontology:measurement(npt__su_t2003, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 2003, 0.43).
narrative_ontology:measurement(npt__su_t2015, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 2015, 0.41).
narrative_ontology:measurement(npt__su_t2024, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__withdrawal_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_1970__withdrawal_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970__oligopoly_enforcement_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970__reciprocal_disarmament_reading).

% DUAL FORMULATION NOTE:
% The npt_treaty_1970 kernel decomposes into three constraint stories with distinct ε and victim structures. This reading (withdrawal_sovereignty) has ε=0.38 and victims including regime_stability_norm. The oligopoly_enforcement reading has lower ε (~0.15) and treats non-nuclear weapon states as coordinated beneficiaries. The reciprocal_disarmament reading has higher ε (~0.55) and treats nuclear weapon states as victims of their own unfulfilled Article VI obligations. All three are required to model the NPT's contested structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
