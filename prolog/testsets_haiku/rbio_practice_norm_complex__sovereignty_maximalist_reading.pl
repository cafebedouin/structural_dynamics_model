% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__sovereignty_maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_sovereignty_maximalist, []).

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
 *   constraint_id: rbio_practice_norm_complex__sovereignty_maximalist_reading
 *   human_readable: RBIO Sovereignty-Maximalist Doctrine: Absolute State Sovereignty with Humanitarian Exception Framing
 *   domain: international_relations/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the sovereignty-maximalist reading of
 *   contested RBIO (Responsibility to Protect and International Order) norms.
 *   The reading asserts that state sovereignty is absolute and logically
 *   prior to all humanitarian claims; humanitarian intervention norms are
 *   delegitimized as pretexts for regime change by powerful states. Under
 *   this reading, the only legitimate intervention authority is self-defense;
 *   conditionality (human rights compliance, humanitarian access) is
 *   acceptable ONLY when the target state can exit without cost.
 *   Beneficiaries are authoritarian regimes and the institutional defenders
 *   of state sovereignty doctrine. Victims are populations trapped under
 *   repressive rule who lose access to external humanitarian recourse. The
 *   measurement series track extraction accumulation (from 0.58 to 0.72 over
 *   the interval), theater ratio increasing (regime-change language deployed
 *   more frequently relative to coordination function), and suppression
 *   intensity rising as enforcement of the doctrine requires more active
 *   suppression of humanitarian intervention rhetoric.
 *
 * KEY AGENTS:
 *   - Authoritarian regimes: benefit from sovereignty shield against intervention; extract protection in exchange for nominally accepting sovereignty principle
 *   - Populations under repressive rule: trapped; bear internal costs of repression unchecked by external intervention authority
 *   - Liberal institutional states (P5 veto holders): formally administer RBIO norms; benefit from doctrine's discretionary enforcement capacity
 *   - Sovereignty doctrine institutional defenders: diplomats, legal theorists, UN bureaucracy; benefit from role validation
 *   - Humanitarian advocates (excluded): would challenge the doctrine's framing of humanitarian norms as pretexts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.72).
domain_priors:suppression_score(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.78).
domain_priors:theater_ratio(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__sovereignty_maximalist_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__sovereignty_maximalist_reading, "RBIO Sovereignty-Maximalist Doctrine: Absolute State Sovereignty with Humanitarian Exception Framing").
narrative_ontology:topic_domain(rbio_practice_norm_complex__sovereignty_maximalist_reading, "international_relations/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__sovereignty_maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__sovereignty_maximalist_reading, '18907e61-9ea9-42de-91eb-f70028140548').
narrative_ontology:cs_kernel_codification('18907e61-9ea9-42de-91eb-f70028140548', fixed_text).
narrative_ontology:cs_authority_grounding('18907e61-9ea9-42de-91eb-f70028140548', extraction).
narrative_ontology:cs_interpretation_layer_present('18907e61-9ea9-42de-91eb-f70028140548').
narrative_ontology:cs_reading_relation('18907e61-9ea9-42de-91eb-f70028140548', rbio_practice_norm_complex__liberal_institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('18907e61-9ea9-42de-91eb-f70028140548', rbio_practice_norm_complex__hegemonic_extraction_reading, influences).
narrative_ontology:cs_axiom('18907e61-9ea9-42de-91eb-f70028140548', foundational, state_sovereignty_is_absolute).
narrative_ontology:cs_axiom_status(state_sovereignty_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('18907e61-9ea9-42de-91eb-f70028140548', state_sovereignty_is_absolute, deontological).
narrative_ontology:cs_axiom('18907e61-9ea9-42de-91eb-f70028140548', foundational, humanitarian_intervention_is_regime_change_pretext).
narrative_ontology:cs_axiom_status(humanitarian_intervention_is_regime_change_pretext, holdable).
narrative_ontology:cs_axiom_grounding('18907e61-9ea9-42de-91eb-f70028140548', humanitarian_intervention_is_regime_change_pretext, empirically_contingent).
narrative_ontology:cs_reference_frame('18907e61-9ea9-42de-91eb-f70028140548', westphalian_mutual_non_interference).
narrative_ontology:cs_drift_state('18907e61-9ea9-42de-91eb-f70028140548', contemporary_selective_intervention_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('18907e61-9ea9-42de-91eb-f70028140548', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, state_sovereignty_doctrine_institutional_defenders).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, populations_under_repressive_rule_without_exit).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, liberal_institutional_states).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, non_aligned_movement_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, non_aligned_movement_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claim absolute sovereignty to shield internal repression from external intervention. Invoke non-interference as protection against sanctions, humanitarian access, and regime-change operations. Benefit from the reading's framing of humanitarian norms as pretexts — the doctrine insulates their rule from scrutiny. Exit involves voluntary submission to external accountability, which their power position makes unattractive.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes, beneficiary,
    institutional, generational, arbitrage, global).

% Bear the internal cost of repression (violence, detention, resource extraction) shielded from external intervention by the sovereignty doctrine. Exit requires either fleeing the territory (at high cost) or waiting for internal change. The doctrine's framing of humanitarian norms as regime-change pretexts removes international legal grounds for protective intervention. They have no seat at the table where the doctrine is maintained.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, populations_under_repressive_rule_without_exit, payer,
    powerless, biographical, trapped, local).

% Maintain the formal institutional structures (UN, treaty systems) that house RBIO norms, but this reading sees them as frozen hegemons imposing a sovereignty doctrine that protects non-aligned authoritarian states from intervention while preserving their own capacity to intervene selectively when interests align. They administer the system's formal rules while the sovereignty-maximalist reading denies them legitimate intervention authority — creating a tension between their institutional role and their geopolitical interests.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, liberal_institutional_states, agenda_setter,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__sovereignty_maximalist_reading, liberal_institutional_states, beneficiary).

% Institutional actors (diplomats, legal theorists, UN bureaucracies) whose authority, career, and doctrine rests on state sovereignty as the organizing principle. They benefit from the reading's assertion that sovereignty is absolute and prior to humanitarian claims — it validates their institutional role as mediators of inter-state relations rather than enforcers of transnational norms. Exit from this reading would require reconstituting their institutional authority on different foundations.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, state_sovereignty_doctrine_institutional_defenders, beneficiary,
    institutional, generational, analytical, global).

% NGOs, human rights networks, and interventionist states would object to the sovereignty-maximalist reading's framing of humanitarian norms as regime-change pretexts. They argue humanitarian access and protection are independent of regime change and that absolute sovereignty is incompatible with human rights obligations. They are structurally excluded from the doctrine's framing — the reading defines humanitarian concern itself as inauthentic.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, humanitarian_intervention_advocates, excluded,
    moderate, biographical, constrained, global).

% Benefit from the absolute sovereignty doctrine as protection against powerful-state intervention, but also pay costs when internal repression goes unchecked and when humanitarian crises spill across borders (refugee flows, regional instability). Their exit is constrained: they cannot simply opt out of the doctrine without losing its protective shield, but remaining inside it exposes them to internal instability their own governments may not be able to manage.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, non_aligned_movement_states, beneficiary,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__sovereignty_maximalist_reading, non_aligned_movement_states, payer).

% States with veto power (P5 in UN Security Council) and institutional enforcers of RBIO norms. They administer the sovereignty doctrine and gate intervention authority. The reading sees them as strategically invoking sovereignty protection when it suits their interests (non-aligned regimes) and selectively setting it aside when regime change aligns with their geopolitical goals. Their position is to maintain the doctrine's formal absolute status while preserving the discretion to override it.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, enforcement_mechanism_actors, agenda_setter,
    institutional, generational, analytical, global).

% The formal institutional architecture (UN Charter, treaty systems, customary law) that houses and transmits RBIO norms. This reading treats it as a domain where the sovereignty-maximalist doctrine is institutionalized, but the reading does not hold it accountable — rather, it views the doctrine as the proper foundation of that system.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, international_legal_system, observer,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(rbio_practice_norm_complex__sovereignty_maximalist_reading, international_legal_system).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes).
narrative_ontology:fixing_cost_class(rbio_practice_norm_complex__sovereignty_maximalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes mutual non-interference as a rule of inter-state relations: solves the collective-action problem of unilateral intervention by strong states against weak states. Without the sovereignty doctrine, powerful states would constantly intervene in weaker states' affairs, destabilizing the whole system.
% TRANSFER_FUNCTION: Transfers protection from external intervention to states (especially authoritarian ones) in exchange for their acceptance of the sovereignty principle. The constraint also transfers intervention discretion to powerful states (P5 holders) — they formallymaintain the doctrine while preserving the capacity to override it when interests align.
% ABSENT_VOICES: Populations under repressive rule without exit have no representation in the forums where sovereignty doctrine is maintained. Humanitarian intervention advocates and human rights networks are structurally excluded — the doctrine defines humanitarian concern itself as a pretext for regime change. Weaker non-aligned states participate formally but face pressure (implicit and explicit) to defend the doctrine even when it shields their own governments' repression.
% DISAPPEARANCE_RATIONALE: If absolute sovereignty doctrine disappeared and were replaced by conditional intervention authority, regimes would lose their legal shield against humanitarian intervention, sanctions, and accountability mechanisms. Populations would gain potential access to external support. The balance of power in international relations would shift — weak states would lose their primary protection (non-interference), and strong states would lose their discretionary enforcement mechanism. The system would reorganize around either a new sovereignty doctrine (conditional on human rights compliance) or a competing framework (humanitarian primacy, transnational accountability).
% FOUNDING_PROBLEM: Prevention of great-power imperialism and colonialism: the Westphalian system was built to constrain the strongest states from unilaterally imposing rule on weaker territories. Absolute sovereignty doctrine was meant to protect the weak from the powerful.
% FOUNDING_PROBLEM_CORROBORATION: Sovereignty-doctrine defenders (authoritarian regimes, state diplomats, institutional traditionalists) attest the founding problem is still live — powerful states remain imperialist in intent and selective intervention remains a threat to non-aligned sovereignty. Liberal institutional and humanitarian advocates attest the founding problem is substantially solved (formal empires are gone, multilateral institutions exist) and the doctrine now primarily protects authoritarian regimes from accountability rather than protecting the weak from imperialism. Independent scholarship documents both selective intervention by powerful states AND the doctrine's use as a shield for internal repression — the status is genuinely contested.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__sovereignty_maximalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__sovereignty_maximalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__sovereignty_maximalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__sovereignty_maximalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.72, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the sovereignty-maximalist reading redistributes intervention authority away from powerless populations and toward state governments. Suppression is higher still (0.78) because active enforcement requires suppressing competing humanitarian-intervention narratives, delegitimizing transnational advocacy, and maintaining the doctrine's absolute status even when selective enforcement undermines it. Theater ratio is moderate (0.41) and rising: regime-change language is deployed routinely by powerful states (undermining the doctrine's formal absoluteness), yet the formal rule persists — the gap between stated doctrine (absolute sovereignty) and actual practice (selective intervention) creates theatrical performance. Accessibility collapse is high (0.68) because once the reading is accepted, alternatives (humanitarian intervention, conditional sovereignty, transnational accountability) appear illegitimate or impossible — the doctrine forecloses the conversation space itself. Resistance is substantial (0.62) from humanitarian advocates and weaker states whose internal crises generate pressure for intervention, but the institutional enforcement machinery (P5 veto, diplomatic consensus) suppresses active resistance at the formal level.
 *
 * PERSPECTIVAL GAP:
 *   The regime seat perceives the doctrine as a genuine coordination rule (mutual non-interference solves the collective-action problem). The trapped population seat perceives it as enforced extraction (they lose recourse). The liberal institutional state perceives it as a rule it formally maintains while selectively violating — the regime's protection is convenient when it aligns with their interests, expendable when it does not. From the humanitarian advocate seat (excluded), the doctrine is pure extraction: it shields repression and forecloses the moral language of intervention.
 *
 * DIRECTIONALITY LOGIC:
 *   Authoritarian regimes are beneficiaries (d ≈ 0.0): they collect sovereignty protection, experience no enforcement cost from this constraint (it insulates them), have arbitrage-level exit (invoke sovereignty when convenient, work around it when necessary). Trapped populations are targets (d ≈ 1.0): they bear all the cost (blocked intervention, lost recourse), are identity-locked (cannot escape being residents), trapped-exit. Liberal institutional states are complex: they formally defend the doctrine (d ≈ 0.4, moderate payer — they enforce it) but benefit from its discretionary application (d ≈ 0.2, partial beneficiary — they can override when interests align). The institutional defenders of sovereignty doctrine are beneficiaries (d ≈ 0.1): their authority, career, and institutional role rest on the doctrine being maintained as absolute.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading instantiates a tangled rope that is moving toward a snare: the founding coordination problem (preventing great-power imperialism) is substantially addressed (formal empires are gone, multilateral institutions exist), yet the doctrine persists and its primary function has shifted to protecting authoritarian regimes from accountability. The theater ratio rising (regime-change language increasingly deployed) while the doctrine maintains its formal absoluteness signals mandatrophy — the constraint's stated function (mutual non-interference) diverges from its actual function (providing a shield for selective intervention and shielding authoritarians from accountability). The measurement of rising theater ratio combined with rising extraction indicates the constraint is increasingly deployed performatively (state leaders invoke sovereignty while humanitarian crises signal its erosion). The constraint is not yet a pure piton (enforcement still requires active suppression of humanitarian rhetoric), but the trajectory shows extraction accumulation decoupled from coordination function — classic mandatrophy signal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    humanitarian_exception_legitimacy,
    'Are humanitarian exceptions to sovereignty doctrine genuine exceptions (authorized by legitimate multilateral process) or pretexts for regime change (cover for geopolitical interest)?',
    'Pattern analysis of intervention justifications (humanitarian stated, geopolitical actual); comparison of humanitarian crises where intervention occurred vs. did not occur; investigation of whether intervention aligned with prior geopolitical commitments.',
    'If humanitarian exceptions are genuine (decoupled from geopolitical interest), the sovereignty-maximalist reading''s framing of them as pretexts is false, and the doctrine is actually a hybrid restricting both humanitarian intervention AND regime change. If exceptions are consistently pretextual, the reading is structurally accurate and the constraint is a snare using humanitarian language as cover.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(humanitarian_exception_legitimacy, empirical, 'Whether humanitarian exceptions to sovereignty are legitimate or geopolitical cover.').

omega_variable(
    founding_problem_obsolescence,
    'Is the founding coordination problem (preventing great-power imperialism through mutual non-interference) still live, or has the constraint''s primary function shifted to protecting authoritarian regimes from accountability?',
    'Historical analysis of intervention patterns; documentation of which states invoke sovereignty protection (authoritarian regimes benefiting) vs. which states override it (powerful states doing so); comparison of pre- and post-Cold-War intervention frequency and justifications.',
    'If the founding problem is live, the constraint remains a genuine coordination solution. If it is dead/solved while extraction persists, the constraint has mandatrophy and is moving toward piton or snare. This determines whether the theater ratio''s rise indicates functional degradation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether sovereignty doctrine still solves its founding collective-action problem.').

omega_variable(
    absolute_sovereignty_vs_conditional_exit,
    'Can the sovereignty doctrine be truly absolute if enforcement depends on powerful states selectively overriding it, or is ''absolute sovereignty with discretionary exceptions'' a logical contradiction masking extraction?',
    'Formal logical analysis of the stated doctrine (absolute) vs. documented practice (selective); documentation of which states successfully invoke sovereignty (authoritarian) vs. which states successfully override it (powerful); test whether the doctrine is symmetrical or asymmetric across power levels.',
    'If absolute sovereignty is symmetrically enforced (all states equally protected and constrained), it is a genuine rule. If enforcement is asymmetric (powerful states override, weak states shield), the doctrine is a cover for differential extraction and should be reclassified as pure extraction (snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(absolute_sovereignty_vs_conditional_exit, empirical, 'Whether absolute sovereignty is symmetrically enforced or asymmetrically masks power differentials.').

omega_variable(
    reading_incompatibility_liberal_institutional,
    'Does the sovereignty-maximalist reading logically foreclose the liberal-institutional reading, or do they coexist as different parties'' live positions?',
    'Logical analysis: does accepting absolute sovereignty (maximalist) commit one to rejecting universal, consent-based, multilaterally revisable norms (liberal institutional)? Or can both framings be held simultaneously by different parties?',
    'If foreclosure holds, the readings are in a zero-sum contest and only one can be institutionally valid. If coexistence holds, they are competing framings held by different factions of an ongoing dispute. The relation type determines engine treatment of reading contamination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_incompatibility_liberal_institutional, conceptual, 'Logical relation between sovereignty-maximalist and liberal-institutional readings.').

omega_variable(
    identity_locked_vs_constrained_exit,
    'For trapped populations under repressive rule: is their immobility pure structural (economic, geographic, legal barriers) or partly internalized (identity fusion with territory, belief they deserve the treatment)?',
    'Post-exit trajectory analysis: if populations that escape the repressive regime continue to internalize suppression-consistent beliefs, internalization is present; if exit causes rapid perspective shift toward blaming the regime, suppression is primarily structural.',
    'If internalized, the effective suppression is higher than the structural measure suggests — the population carries the suppression with them. This would raise the constraint''s classification toward pure snare. If primarily structural, external intervention could meaningfully shift the population''s situation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_vs_constrained_exit, empirical, 'Whether suppression of trapped populations is structural or partly internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t0, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(rbio_tr_t5, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement(rbio_tr_t10, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement(rbio_tr_t15, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(rbio_tr_t25, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 25, 0.39).
narrative_ontology:measurement(rbio_tr_t40, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(rbio_be_t0, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(rbio_be_t5, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 5, 0.61).
narrative_ontology:measurement(rbio_be_t10, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(rbio_be_t15, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(rbio_be_t25, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 25, 0.71).
narrative_ontology:measurement(rbio_be_t40, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t0, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(rbio_su_t5, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(rbio_su_t10, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(rbio_su_t15, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 15, 0.74).
narrative_ontology:measurement(rbio_su_t25, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 25, 0.77).
narrative_ontology:measurement(rbio_su_t40, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__sovereignty_maximalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.12).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex__liberal_institutional_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex__hegemonic_extraction_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, humanitarian_intervention_norm_complex).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, p5_veto_system_collective_action).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the contested RBIO kernel (rbio_practice_norm_complex). The kernel itself is the stabilized commitment to managing international intervention authority. The sovereignty-maximalist reading asserts absolute state sovereignty as the legitimate foundation; the liberal-institutional reading asserts universal consent-based norms; the hegemonic-extraction reading asserts frozen hegemonic projects. Each reading instantiates a structurally distinct constraint (different ε, different beneficiary/victim, different type). The three readings are linked via network.affects_constraints to enable contamination analysis — if one reading's legitimacy erodes, it affects the others' institutional standing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rbio_practice_norm_complex__sovereignty_maximalist_reading, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
