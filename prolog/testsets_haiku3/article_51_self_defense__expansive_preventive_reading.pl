% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__expansive_preventive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__expansive_preventive_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: article_51_self_defense__expansive_preventive_reading
 *   human_readable: Article 51 Self-Defense: Expansive Preventive Reading
 *   domain: international_law/security_studies/constitutional_interpretation
 *
 * SUMMARY:
 *   Article 51 of the UN Charter permits self-defense when an armed attack
 *   occurs. The expansive preventive reading interprets this to permit
 *   preemptive or preventive force against non-state actors and emerging
 *   threats when necessity is demonstrated, with necessity adjudicated
 *   unilaterally by the acting state. This reading is one of three
 *   structurally distinct readings of the same constitutional-legal kernel:
 *   the narrow reading constrains self-defense to responses to actual or
 *   imminent armed attacks; the unable-unwilling doctrine reading splits the
 *   difference, permitting force against non-state actors only when they
 *   operate from a host state unwilling or unable to suppress them. This
 *   story instantiates the expansive reading's constraint structure. The
 *   kernel is the text of Article 51 and the international practice
 *   surrounding its interpretation; the readings diverge on what necessity
 *   means and who judges it.
 *
 * KEY AGENTS:
 *   - Militarily capable states: interpret Article 51 expansively, maintain unilateral necessity authority, benefit from reduced constraints on force initiation
 *   - Defense sector contractors: benefit from sustained high military operations under preventive-threat doctrine
 *   - Target-region populations: bear immediate costs of preemptive strikes; have no voice in necessity determination
 *   - Multilateral constraint authority (UN Security Council, international consensus mechanisms): authority is bypassed or rendered advisory
 *   - Narrow reading advocates (scholars, human rights orgs, non-aligned states): observe and contest the expansive reading's legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__expansive_preventive_reading, 0.82).
domain_priors:suppression_score(article_51_self_defense__expansive_preventive_reading, 0.76).
domain_priors:theater_ratio(article_51_self_defense__expansive_preventive_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__expansive_preventive_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__expansive_preventive_reading, "Article 51 Self-Defense: Expansive Preventive Reading").
narrative_ontology:topic_domain(article_51_self_defense__expansive_preventive_reading, "international_law/security_studies/constitutional_interpretation").

domain_priors:requires_active_enforcement(article_51_self_defense__expansive_preventive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__expansive_preventive_reading, '2dfce64e-704a-4462-bbd0-4d06d416faf9').
narrative_ontology:cs_kernel_codification('2dfce64e-704a-4462-bbd0-4d06d416faf9', formalized).
narrative_ontology:cs_authority_grounding('2dfce64e-704a-4462-bbd0-4d06d416faf9', extraction).
narrative_ontology:cs_interpretation_layer_present('2dfce64e-704a-4462-bbd0-4d06d416faf9').
narrative_ontology:cs_reading_relation('2dfce64e-704a-4462-bbd0-4d06d416faf9', article_51_self_defense__narrow_armed_attack_reading, forecloses).
narrative_ontology:cs_reading_relation('2dfce64e-704a-4462-bbd0-4d06d416faf9', article_51_self_defense__unable_unwilling_doctrine_reading, influences).
narrative_ontology:cs_axiom('2dfce64e-704a-4462-bbd0-4d06d416faf9', foundational, state_unilateral_necessity_authority).
narrative_ontology:cs_axiom_status(state_unilateral_necessity_authority, holdable).
narrative_ontology:cs_axiom_grounding('2dfce64e-704a-4462-bbd0-4d06d416faf9', state_unilateral_necessity_authority, deontological).
narrative_ontology:cs_axiom('2dfce64e-704a-4462-bbd0-4d06d416faf9', foundational, threat_prevention_justifies_force_initiation).
narrative_ontology:cs_axiom_status(threat_prevention_justifies_force_initiation, holdable).
narrative_ontology:cs_axiom_grounding('2dfce64e-704a-4462-bbd0-4d06d416faf9', threat_prevention_justifies_force_initiation, empirically_contingent).
narrative_ontology:cs_reference_frame('2dfce64e-704a-4462-bbd0-4d06d416faf9', unilateral_necessity_self_judged).
narrative_ontology:cs_drift_state('2dfce64e-704a-4462-bbd0-4d06d416faf9', contemporary_post_9_11_security_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2dfce64e-704a-4462-bbd0-4d06d416faf9', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__expansive_preventive_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, militarily_capable_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, defense_sector_contractors).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, target_region_populations).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, multilateral_constraint_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, non_aligned_and_weaker_states).
narrative_ontology:constraint_vindicates(article_51_self_defense__expansive_preventive_reading, state_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(article_51_self_defense__expansive_preventive_reading, unilateral_security_determination).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and apply Article 51 to permit preemptive or preventive use of force against non-state actors and emerging threats when they judge necessity is demonstrated. They set the terms of the constraint through military practice and legal argument. They benefit by avoiding the requirement to wait for attack materialization, by maintaining exclusive authority to judge necessity, and by expanding the scope of legitimate force initiation under the Article 51 label.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, militarily_capable_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__expansive_preventive_reading, militarily_capable_states, beneficiary).

% Benefit from sustained and expanded military operations justified by extended threat horizons and preventive doctrines. Their procurement volumes, contract pipelines, and long-term defense budgets are supported by the threat-anticipation framing that the expansive reading enables. They have no institutional stake in narrowing Article 51 interpretation.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, defense_sector_contractors, beneficiary,
    powerful, biographical, arbitrage, global).

% Regions where non-state actors operate or where emerging threats are perceived experience preemptive and preventive military strikes conducted by distant states. These populations bear the immediate costs — civilian casualties, infrastructure damage, displacement, psychological trauma — without having launched attacks and without having any voice in the necessity determination that precedes strikes. Their powerlessness and trapped exit (cannot escape geography or national boundaries) mean they bear costs with no mechanism for consent or remedy.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, target_region_populations, payer,
    powerless, immediate, trapped, regional).

% The UN Security Council, which holds formal authority to authorize military action, is bypassed or rendered advisory when militarily capable states claim necessity under the expansive reading. Their mandate to constrain force through collective authorization is subordinated to unilateral state judgment. They bear the institutional cost of eroded authority and reduced relevance to military decision-making, even as they remain nominally responsible for international peace and security.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, multilateral_constraint_authority, payer,
    institutional, generational, constrained, universal).

% International legal scholars, human rights organizations, non-aligned states, and some Western legal establishments argue for a narrow reading of Article 51 that constrains self-defense to responses to actual or imminent armed attacks. They articulate the structural concern that the expansive reading permits disguised wars. They cannot prevent the expansive reading's application but can challenge its legitimacy, invoke competing interpretations, and advocate through diplomatic and academic channels.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, narrow_reading_advocates, observer,
    institutional, generational, analytical, global).

% States without dominant military capability bear the costs of the expansive reading's application without the ability to invoke it. If they attempt to use the expansive reading to justify their own military operations, the militarily capable states often challenge the necessity determination (double standard). They benefit from multilateral constraint mechanisms but are constrained by the expansion of unilateral force authority, since weaker states cannot credibly claim necessary self-defense under the same loose standard.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, non_aligned_and_weaker_states, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__expansive_preventive_reading, non_aligned_and_weaker_states, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_51_self_defense__expansive_preventive_reading, militarily_capable_states).
narrative_ontology:fixing_cost_class(article_51_self_defense__expansive_preventive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Article 51 solves the problem of distinguishing lawful defensive force from aggressive war. Under this expansive reading, the coordination mechanism is weakened because the distinction becomes internal to the acting state's judgment rather than external to an objective international standard. The intended coordination function (community-validated self-defense) is decoupled from the actual function (state-validated threat response).
% TRANSFER_FUNCTION: Transfers unilateral authority over force initiation from multilateral (UN Security Council) to the unilateral (acting state necessity determination). Transfers immediate security costs (civilian casualties, infrastructure damage, displacement) from distant decision-makers to target populations. Transfers the authority to define 'threat' from international legal adjudication to military-strategic assessment by the acting state.
% ABSENT_VOICES: Non-state actors and populations in target regions have no formal seat in the necessity determination. International legal authorities outside the acting state (International Court of Justice, regional human rights courts, non-aligned state blocs) are excluded from meaningful constraint on the force decision. Target populations cannot contest the necessity judgment that precedes their targeting.
% DISAPPEARANCE_RATIONALE: If the expansive preventive reading disappeared and Article 51 reverted to requiring a prior armed attack or imminent attack meeting a high threshold of necessity, the global military architecture would reorganize: preemptive and preventive operations would require Security Council authorization or demonstrated threat materialization; military budgets would shift from indefinite threat-anticipation to demonstrable-threat response; regional conflicts would face higher procedural barriers before military force could be lawfully employed; multilateral mechanisms would regain relevance to force authorization. The constraint is not ornamental — its removal would change military practice significantly.
% FOUNDING_PROBLEM: Article 51 was drafted to solve the problem of permitting legitimate self-defense against invasion while preventing aggressive war disguised as response. The UN Charter framers sought to distinguish between lawful defensive force and unlawful preventive or aggressive war, codifying the right to defend while constraining the right to attack.
% FOUNDING_PROBLEM_CORROBORATION: Narrow reading advocates and international legal scholars attest that the founding problem is still live and unsolved — the expansive reading is being used to justify preventive wars while claiming Article 51 authority. The UN General Assembly has issued multiple resolutions expressing concern about preventive war and reaffirming that self-defense requires prior armed attack. However, militarily capable states attest that modern threats (terrorism, cyber, emerging biological threats) require expanded interpretation because they do not announce themselves as imminent armed attacks in the traditional sense. Neutral sources: The International Committee of the Red Cross, ICJ case law (Nicaragua v. United States; Legality of the Threat or Use of Nuclear Weapons), and foundational international legal scholarship (Brownlie, Gray, Cassese) document that the reading divergence is irresolved and that states have conflicting interpretations of the founding problem's status. No source outside the military-power establishment corroborates the claim that the founding problem is solved.
narrative_ontology:disappearance_verdict(article_51_self_defense__expansive_preventive_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__expansive_preventive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__expansive_preventive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_51_self_defense__expansive_preventive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__expansive_preventive_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__expansive_preventive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__expansive_preventive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__expansive_preventive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.68→0.82 over interval) because the constraint systematically transfers unilateral force authority from multilateral to unilateral decision-making, and necessity determination from objective international standard to state self-judgment. The rising trajectory reflects accumulating practice: early in the interval, the expansive reading was contested; by interval end, it is the de facto practice of major military powers. Suppression is high (0.76) because the constraint's persistence depends on preventing Security Council veto from constraining force, on marginalizing narrow-reading advocates, and on treating target populations' objections as irrelevant to necessity. Theater is moderate-rising (0.25→0.42) because states justify preventive strikes with elaborate threat narratives and necessity rhetoric, but the underlying dynamic is shifting from defense-justification to threat-anticipation-justification. The rising theater ratio indicates growing gap between presented function (defensive response) and actual function (unilateral force initiation under broad threat language). Accessibility collapse is low (0.38) because alternatives to accepting the expansive reading remain formally available — Security Council authorization, regional collective defense, diplomatic solutions, intelligence-based deterrence — but they are increasingly bypassed in practice. Resistance is high (0.71) because international legal scholars, human rights bodies, and non-aligned states actively contest this reading and invoke the narrow reading as the correct interpretation.
 *
 * PERSPECTIVAL GAP:
 *   The militarily capable state seat experiences the constraint as legitimate coordination — Article 51 properly extended to modern threats that don't announce themselves as imminent armed attacks. From this seat, the constraint is Rope: it solves the problem of protecting against terrorism and emerging threats while remaining within the UN framework (states invoke Article 51, not reject it outright). The target-region population seat experiences the same constraint as pure extraction — force initiated without their consent, justified by distant states' threat perception, with no remedy or appeal mechanism. The multilateral authority seat experiences the constraint as institutional displacement — their mandate to authorize force is transferred to unilateral judgment. The engine will compute these divergences from the structural data: the militarily capable state has high exit options (arbitrage — they can interpret Article 51 and enforce their interpretation unilaterally); the target populations are trapped (no veto, no exit); the UN authority is constrained (constrained exit, because states that don't respect Council authority still claim Article 51 legitimacy). These divergent exit structures will produce divergent directionalities and thus divergent computed types across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Militarily capable states are beneficiaries at d≈0.1 (collects unilateral authority, arbitrage exit). Defense contractors are beneficiaries at d≈0.15 (profit from sustained operations, arbitrage exit via contract flow). Target populations are victims at d≈0.95 (bear force costs, trapped exit, powerless power atom). Multilateral authority is victim-adjacent at d≈0.75 (authority is displaced, constrained exit, institutional power atom but constrained by state defection). The directionality profile confirms tangled rope: genuine coordination function (Article 51 solves a real problem of permitting legitimate defense) overlaid with asymmetric extraction (unilateral authority transfer, diffuse costs on target populations, suppression of alternatives through practice and rhetoric).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows early-stage mandatrophy symptoms: the founding problem (distinguishing defense from disguised war) remains live, but the constraint's evolution is shifting the meaning of Article 51 away from solving that problem toward enabling preventive war under the Article 51 label. The founding_problem_status is correctly authored as contested because narrow-reading advocates attest it is unsolved while expansive-reading advocates attest that modern threats require expanded interpretation. The measurement series showing rising extractiveness and theater ratio indicate the constraint is degrading toward pure extraction as the preventive-threat justification becomes more rhetorical and less tethered to the coordination function. However, because the underlying text (Article 51) is not being formally repealed, and states continue to invoke it as their legal authority, the mandatrophy is not yet terminal — the constraint remains a living text with contested interpretation rather than an abandoned commitment. This reading locks in high extraction while still claiming the Article 51 coordination function; the narrow reading represents a competing claim to the same coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_determination_authority,
    'Who has legitimate authority to determine whether necessity is demonstrated — the acting state unilaterally, the UN Security Council, international legal adjudication, or a distributed consensus of the international community?',
    'ICJ advisory opinion or binding ruling; formal amendment to Article 51 or codification of customary law through state practice convergence; Security Council resolution establishing standards for necessity evaluation.',
    'If authority is vested in the acting state, the constraint remains extractive and unilateral (current reading). If authority is transferred to multilateral adjudication, the constraint becomes constrained rope or snare remedied by procedural control. If authority is distributed, the constraint becomes negotiated-rope. The necessity determination is the crux that drives the reading''s extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(necessity_determination_authority, empirical, 'Locus of authority for adjudicating necessity under Article 51 self-defense').

omega_variable(
    preventive_vs_preemptive_distinction,
    'Is there a meaningful legal or structural distinction between preemptive force (against imminent attack) and preventive force (against emerging threats distant in time), or does the expansive reading collapse them into a single necessity criterion?',
    'State practice analysis and scholarly consensus on the preventive/preemptive boundary; ICJ case law distinguishing the standards; Security Council practice establishing when force is authorized versus condemned.',
    'If preemptive force is retained as a legal category distinct from preventive force, the constraint is narrower than currently authored (preventive would be excluded, preemptive would be permitted under narrower necessity). If the distinction collapses, the current extractiveness is structural and more difficult to constrain short of broader Article 51 reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preventive_vs_preemptive_distinction, empirical, 'Whether preemptive and preventive force are structurally or legally distinct categories').

omega_variable(
    non_state_actor_attribution_standard,
    'What level of state attribution or host-state involvement is required for non-state actor attacks to trigger Article 51 self-defense? Does the expansive reading require proof of attribution, or does attribution-assumption suffice?',
    'State practice in attributing non-state actor attacks; ICJ rulings on attribution standards; Security Council voting patterns on attribution in conflict scenarios.',
    'High attribution standards (proof of state complicity or control) narrow the expansive reading and favor the unable-unwilling doctrine reading. Low attribution standards (suspicion or historical association suffice) expand the reading and increase extractiveness because necessity determinations become less verifiable and more subject to acting-state judgment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(non_state_actor_attribution_standard, empirical, 'Evidentiary standard for attributing non-state actor attacks or emerging threats to a host state or state-like entity').

omega_variable(
    expansion_of_threat_category,
    'What counts as an ''emerging threat'' capable of triggering preventive self-defense? Does the expansive reading include cyber threats, biological threats, economic threats, or only kinetic/military threats?',
    'State practice in claiming self-defense against non-kinetic threats; ICJ interpretation of threat; Security Council precedent on expanded threat definitions.',
    'If ''emerging threat'' includes non-kinetic domains, the constraint expands dramatically and extractiveness rises further because any military-capable state can claim self-defense against a much broader category of events. If limited to kinetic threats, extractiveness is constrained by verifiability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expansion_of_threat_category, conceptual, 'The semantic boundary of ''emerging threat'' and whether the expansive reading extends to non-kinetic domains').

omega_variable(
    reading_vs_natural_law_status,
    'Is the expansive preventive reading a valid interpretation of Article 51''s text and the international legal system, or is it a contestable reading that misrepresents the kernel and benefits from institutional power to override narrower readings?',
    'International legal scholarship consensus; state practice convergence; ICJ precedent; UN General Assembly voting on resolutions defending or critiquing preventive war.',
    'If the expansive reading is a legitimate interpretation (one of several valid readings), the constraint remains in the tangled_rope category. If it is a false interpretation (a misreading that benefits military powers), it may reclassify as snare via false-summit detection when beneficiary presence and institutional enforcement are weighted against weak legal grounding.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_vs_natural_law_status, conceptual, 'Whether the expansive reading is a valid interpretation of the Article 51 kernel or an institutionally enforced misreading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__expansive_preventive_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_51_self_defense__expansive_preventive_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(arti_tr_t0, observed).
narrative_ontology:measurement(arti_tr_t5, article_51_self_defense__expansive_preventive_reading, theater_ratio, 5, 0.29).
narrative_ontology:measurement_basis(arti_tr_t5, observed).
narrative_ontology:measurement(arti_tr_t10, article_51_self_defense__expansive_preventive_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement_basis(arti_tr_t10, observed).
narrative_ontology:measurement(arti_tr_t15, article_51_self_defense__expansive_preventive_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(arti_tr_t15, observed).
narrative_ontology:measurement(arti_tr_t20, article_51_self_defense__expansive_preventive_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(arti_tr_t20, observed).
narrative_ontology:measurement(arti_tr_t25, article_51_self_defense__expansive_preventive_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(arti_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(arti_be_t0, observed).
narrative_ontology:measurement(arti_be_t5, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 5, 0.72).
narrative_ontology:measurement_basis(arti_be_t5, observed).
narrative_ontology:measurement(arti_be_t10, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 10, 0.76).
narrative_ontology:measurement_basis(arti_be_t10, observed).
narrative_ontology:measurement(arti_be_t15, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 15, 0.79).
narrative_ontology:measurement_basis(arti_be_t15, observed).
narrative_ontology:measurement(arti_be_t20, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 20, 0.81).
narrative_ontology:measurement_basis(arti_be_t20, observed).
narrative_ontology:measurement(arti_be_t25, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 25, 0.82).
narrative_ontology:measurement_basis(arti_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(arti_su_t0, observed).
narrative_ontology:measurement(arti_su_t5, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 5, 0.67).
narrative_ontology:measurement_basis(arti_su_t5, observed).
narrative_ontology:measurement(arti_su_t10, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 10, 0.71).
narrative_ontology:measurement_basis(arti_su_t10, observed).
narrative_ontology:measurement(arti_su_t15, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement_basis(arti_su_t15, observed).
narrative_ontology:measurement(arti_su_t20, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement_basis(arti_su_t20, observed).
narrative_ontology:measurement(arti_su_t25, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 25, 0.76).
narrative_ontology:measurement_basis(arti_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__expansive_preventive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_51_self_defense__expansive_preventive_reading, 0.18).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, article_51_self_defense__narrow_armed_attack_reading).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, article_51_self_defense__unable_unwilling_doctrine_reading).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, un_security_council_authorization_requirement).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, international_humanitarian_law_proportionality_constraint).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the article_51_self_defense kernel. The narrow_armed_attack_reading constrains self-defense to responses to actual armed attacks by states attributable under international law. The unable_unwilling_doctrine_reading permits force against non-state actors only when they operate from a host state unwilling or unable to suppress the threat. All three readings cite the same Article 51 text but produce different constraint structures with different ε values. The expansive reading is upstream to the unable_unwilling reading (the latter emerged partly as an attempt to compromise between the expansive and narrow readings). All three are linked via network.affects_constraints to establish their family relationship and mutual influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_51_self_defense__expansive_preventive_reading, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
