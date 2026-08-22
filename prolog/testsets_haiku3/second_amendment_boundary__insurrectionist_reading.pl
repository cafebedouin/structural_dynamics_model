% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__insurrectionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__insurrectionist_reading, []).

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
 *   constraint_id: second_amendment_boundary__insurrectionist_reading
 *   human_readable: Second Amendment Insurrectionist Reading: Armed Resistance Capacity Against Tyranny
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The insurrectionist reading of the Second Amendment holds that the right
 *   to keep and bear arms exists fundamentally to preserve popular armed
 *   capacity to resist tyrannical government. Under this interpretation,
 *   individual possession of military-grade weapons is constitutionally
 *   protected because the Founders intended an armed populace as a structural
 *   check on state power. This reading creates an asymmetric extraction
 *   structure: armed citizens gain constitutional legitimacy and expansive
 *   individual rights; the state security apparatus loses regulatory
 *   discretion; civilians in non-combatant roles inherit risk of hypothetical
 *   conflict without voice in the constitutional adjudication. The constraint
 *   is claimed as tangled_rope (coordination function + asymmetric extraction
 *   + active enforcement) while metrics show substantial extractiveness and
 *   suppression, the divergence that the engine measures.
 *
 * KEY AGENTS:
 *   - Armed citizens claiming constitutional insurrectionist doctrine (beneficiaries; identity-locked exit)
 *   - State security apparatus (payers; constrained by constitutional limits on arms control)
 *   - Civilians in conflict zones (payers; powerless, trapped exit)
 *   - Constitutional originalist judges (agenda-setters; enforce the insurrectionist reading)
 *   - Law enforcement (payers; operate under expanded threat profile)
 *   - Tyranny-prevention scholars (observers; provide empirical/logical scrutiny)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__insurrectionist_reading, 0.68).
domain_priors:suppression_score(second_amendment_boundary__insurrectionist_reading, 0.71).
domain_priors:theater_ratio(second_amendment_boundary__insurrectionist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__insurrectionist_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__insurrectionist_reading, "Second Amendment Insurrectionist Reading: Armed Resistance Capacity Against Tyranny").
narrative_ontology:topic_domain(second_amendment_boundary__insurrectionist_reading, "constitutional/political").

domain_priors:requires_active_enforcement(second_amendment_boundary__insurrectionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__insurrectionist_reading, '22f08b16-4537-410b-916e-f57ce47d9a6c').
narrative_ontology:cs_kernel_codification('22f08b16-4537-410b-916e-f57ce47d9a6c', fixed_text).
narrative_ontology:cs_authority_grounding('22f08b16-4537-410b-916e-f57ce47d9a6c', lineage).
narrative_ontology:cs_interpretation_layer_present('22f08b16-4537-410b-916e-f57ce47d9a6c').
narrative_ontology:cs_reading_relation('22f08b16-4537-410b-916e-f57ce47d9a6c', second_amendment_boundary__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('22f08b16-4537-410b-916e-f57ce47d9a6c', second_amendment_boundary__militia_conditioned_reading, forecloses).
narrative_ontology:cs_axiom('22f08b16-4537-410b-916e-f57ce47d9a6c', foundational, armed_populace_structural_tyranny_check).
narrative_ontology:cs_axiom_status(armed_populace_structural_tyranny_check, holdable).
narrative_ontology:cs_axiom_grounding('22f08b16-4537-410b-916e-f57ce47d9a6c', armed_populace_structural_tyranny_check, deontological).
narrative_ontology:cs_axiom('22f08b16-4537-410b-916e-f57ce47d9a6c', foundational, military_grade_arms_constitutionally_protected).
narrative_ontology:cs_axiom_status(military_grade_arms_constitutionally_protected, holdable).
narrative_ontology:cs_axiom_grounding('22f08b16-4537-410b-916e-f57ce47d9a6c', military_grade_arms_constitutionally_protected, empirically_contingent).
narrative_ontology:cs_reference_frame('22f08b16-4537-410b-916e-f57ce47d9a6c', popular_armed_capacity_as_constitutional_guarantee).
narrative_ontology:cs_drift_state('22f08b16-4537-410b-916e-f57ce47d9a6c', contemporary_post_empirical_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('22f08b16-4537-410b-916e-f57ce47d9a6c', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__insurrectionist_reading, armed_citizens_claiming_deterrent_legitimacy).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, state_security_apparatus).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, civilians_in_conflict_zones).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, law_enforcement_personnel).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret the Second Amendment as guaranteeing access to military-grade arms as insurance against state tyranny. Their reading claims that widespread armed populaces deter governmental overreach and preserve the ability to overthrow tyrannical regimes. The constraint's enforcement (judicial recognition of expansive individual rights) validates their constitutional reading and legitimates resistance narratives. Exit from this interpretation would require abandoning a foundational identity claim about citizenship and constitutional duty.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, armed_citizens_claiming_deterrent_legitimacy, beneficiary,
    powerful, civilizational, identity_locked, national).

% Operates under the constraint that comprehensive firearms regulation faces constitutional barriers framed around insurrectionist doctrine. The insurrectionist reading shifts the burden of justification onto state actors seeking to restrict arms — reframed as potential tyranny precursors. This reduces the state's discretion in security policy and arms control, forcing arguments around why disarmament would NOT be tyrannical rather than whether restrictions serve public safety. The state cannot exit the constitutional framework without sovereign authority loss.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, state_security_apparatus, payer,
    institutional, civilizational, constrained, national).

% Bear the costs of hypothetical armed insurrection: crossfire risk, destabilization of civilian infrastructure, and the absence of predictable law enforcement during conflict. They are not parties to the constitutional reading but become bearers of its operational consequences if insurrectionist premises lead to actual armed resistance. They have no mechanism to exit their geographic location or consent to the risk.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, civilians_in_conflict_zones, payer,
    powerless, biographical, trapped, local).

% Operate under rules of engagement shaped by the insurrectionist reading: armed populace access to military-grade arms increases threat profile and operational risk. Training, tactics, and threat assessment must account for wider lethality in civilian hands. The constraint reduces their ability to de-escalate through disarmament proposals and frames weapons-control as constitutional violation.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, law_enforcement_personnel, payer,
    organized, biographical, constrained, local).

% Set and enforce the interpretation that the Second Amendment protects individual right to possess military-grade arms as a check on tyranny. They adjudicate disputes over what arms are covered, what regulations are permissible, and whether state disarmament efforts violate the insurrectionist premise. Their rulings make or unmake the constraint's enforceability.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, constitutional_originalist_judges, agenda_setter,
    institutional, civilizational, constrained, national).

% Analyze whether the insurrectionist reading's factual and normative premises hold: whether armed populaces historically deter tyranny, whether modern military technology makes civilian insurrection viable, whether the constraint actually advances the stated goal. They offer empirical and logical scrutiny but do not directly enforce or benefit from the constraint.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, tyranny_prevention_doctrine_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_boundary__insurrectionist_reading, armed_citizens_claiming_deterrent_legitimacy).
narrative_ontology:fixing_cost_class(second_amendment_boundary__insurrectionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a shared constitutional understanding that armed populaces serve as a structural check on governmental power — solving the coordination problem of how to enshrine resistance capacity in law without enabling random violence.
% TRANSFER_FUNCTION: Shifts interpretive authority from state regulatory discretion to individual constitutional claimants: individuals gain legal space to possess military-grade arms; state security apparatus loses the freedom to disarm without constitutional justification; civilians in potential conflict zones inherit risk without consent.
% ABSENT_VOICES: Civilians in non-combatant roles who would bear the costs of hypothetical insurrection are structurally excluded from the constitutional reading (they are not parties to Second Amendment adjudication). Gun violence victims and families of shooting victims who question whether insurrectionist doctrine increases access to lethal force are not seated in constitutional debates at the same authority level as armed citizens and judges.
% DISAPPEARANCE_RATIONALE: If the insurrectionist reading were abandoned and the militia-conditioned interpretation took its place, state firearms regulation would expand substantially; civilians would lose the constitutional shield they claim for military-grade possession; law enforcement and security apparatus would gain regulatory authority; the armed resistance capacity premise would no longer justify expansive individual rights. The constitutional, political, and security landscape would reorganize.
% FOUNDING_PROBLEM: The Founders confronted standing armies and state monopolies on force; they sought to preserve popular capacity for armed resistance against tyrannical government as a constitutional matter.
% FOUNDING_PROBLEM_CORROBORATION: The insurrectionist reading cites Federalist Papers and Founders' statements about resistance to tyranny. Critics (originalist and living-constitution scholars alike) dispute whether the insurrectionist premise was the Founders' primary concern versus militia organization for common defense, and whether the premise applies to modern military technology. The founding problem's continued relevance is contested across disciplinary and ideological lines; no external corroboration from parties outside the armed-citizens coalition definitively settles it.
narrative_ontology:disappearance_verdict(second_amendment_boundary__insurrectionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__insurrectionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__insurrectionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_boundary__insurrectionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__insurrectionist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__insurrectionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_boundary__insurrectionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_boundary__insurrectionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness climbs from 0.45 to 0.68 over the interval, reflecting the expanding scope of military-grade arms recognized as protected under the insurrectionist premise. Early interval: insurrectionist doctrine is a minority reading, enforcement is contested, access gains are partial. Later interval: major constitutional decisions (e.g., DC v. Heller's expansion of individual rights) cement the reading's authority; state regulatory capacity erodes; the constraint's extractive character solidifies. Theater ratio remains moderate (0.28→0.42): the constraint performs constitutional legitimacy through doctrine, but a growing share of its operation defends armed-access claims rather than the stated tyranny-prevention function. Suppression requirement climbs as well (0.58→0.71): active suppression of rival readings (militia-conditioned interpretation) and of state disarmament efforts increases with the constraint's enforcement strength. Measurements are one shared time grid; every metric is authored at every point.
 *
 * PERSPECTIVAL GAP:
 *   From the armed-citizens seat, the insurrectionist reading is a genuine coordination solution: it provides constitutional certainty and legitimates deterrent capacity without requiring actual insurrection. From the state security apparatus seat, the same constraint is extractive and destabilizing: it forecloses comprehensive regulation that would otherwise be permissible and frames disarmament as a constitutional violation. The engine computes this divergence from power + exit + beneficiary/victim declarations; the authored claim (tangled_rope) does not adjudicate it. Law enforcement and civilians in conflict zones experience the constraint as pure extraction: they bear costs (expanded threat profile, hypothetical conflict risk) without benefiting from coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Armed citizens are near the beneficiary end (d ~0.2): they gain expanded constitutional rights and legitimacy; their exit is identity-locked (abandoning insurrectionist identity is unthinkable in this frame). State security apparatus is a high-d payer (d ~0.85): they lose regulatory discretion and must justify disarmament as constitutional rather than pragmatic; their constrained exit means they absorb the enforcement burden. Civilians and law enforcement are trapped/organized payers with d values of 0.8+ (they bear costs without benefit). Constitutional judges sit as agenda-setters near the beneficiary end (they author the reading's authority).
 *
 * MANDATROPHY ANALYSIS:
 *   The insurrectionist reading displays mandatrophy risk. The founding problem (preservation of armed resistance against tyranny) was live when standing armies posed existential risk to republican government. In the modern era, the founding problem's status is contested: gun violence, mass shootings, and the empirical questionability of civilian insurrection viability against modern military technology have shifted how parties evaluate the founding problem's relevance. Yet the constraint persists and expands (base_extractiveness climbing). The theater_ratio rise (0.28→0.42) indicates growing performative maintenance: the constraint's rhetorical defense increasingly emphasizes constitutional legitimacy and original intent rather than demonstrable tyranny-prevention function. The measurement series tracks how extraction accumulates while its founding rationale atrophies — a classic mandatrophy signature. The engine's mandatrophy detection would compare founding_problem_status (contested) + disappearance_verdict (world_rearranges) against the computed type; this divergence flags the constraint for empirical review.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_currency_contested,
    'Is the preservation of armed resistance against tyranny a live founding problem in the modern era, or has it atrophied into a historical artifact?',
    'Empirical assessment: Does an armed populace demonstrably deter modern state tyranny? Can civilian insurrection succeed against contemporary military technology? What is the counterfactual tyranny risk absent armed populace? Comparative study of democracies with and without insurrectionist doctrines.',
    'If the founding problem is live, the insurrectionist reading''s mandate is grounded; if atrophied, the constraint displays mandatrophy and the theater_ratio climb indicates performative enforcement rather than functional coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_currency_contested, empirical, 'Whether armed insurrection remains a viable or necessary constitutional check on tyranny.').

omega_variable(
    coordination_vs_extraction_boundary,
    'What portion of the measured extractiveness serves a genuine tyranny-prevention coordination function, and what portion is extraction riding on that narrative?',
    'Counterfactual analysis: could the state provide the coordination function (preserving armed capacity against tyranny) through non-extractive means (narrow permits for demonstrated resistance training, community militia structures)? If yes, the extraction is separable from coordination.',
    'If separable, the constraint would be reclassified as a snare wearing tangled_rope clothing. If inseparable, the extractiveness is the price of the coordination function itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, conceptual, 'Whether the insurrectionist reading''s extraction is intrinsic to its coordination function or parasitic on it.').

omega_variable(
    interpretation_kernel_stability,
    'Is the insurrectionist reading a stable interpretation of the Second Amendment''s text, or does it require continuous hermeneutic work to defend against textually-grounded rival readings?',
    'Textual analysis: How far do originalist scholars agree on the founding intent? What linguistic evidence supports insurrectionist vs. militia-conditioned readings? Does the interpretive consensus shift with political/judicial composition?',
    'If the reading requires continuous hermeneutic defense, it is interpretation_layer_present: true and authority_grounding: lineage. If the text is stable under the reading, kernel_codification approaches fixed_text.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretation_kernel_stability, conceptual, 'Textual stability of the insurrectionist reading against hermeneutic rivals.').

omega_variable(
    civilian_risk_internalization,
    'Do civilians in hypothetical conflict zones internalize the risk imposed by the insurrectionist reading (suppress resistance to it because they accept its framing), or is suppression structural (they have no legal recourse)?',
    'Post-conflict analysis: After armed incidents involving insurrectionist-motivated actors, do affected civilians support or oppose the insurrectionist reading''s expansion? Is opposition absent because they lack voice (structural) or because they have internalized the doctrine (internalized)?',
    'If internalized, effective suppression is higher than the structural measure suggests. If structural, the constraint''s persistent enforcement depends on excluding civilian voice from constitutional adjudication.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civilian_risk_internalization, empirical, 'Whether civilian suppression in the constraint is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__insurrectionist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_boundary__insurrectionist_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(seco_tr_t8, second_amendment_boundary__insurrectionist_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(seco_tr_t16, second_amendment_boundary__insurrectionist_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(seco_tr_t25, second_amendment_boundary__insurrectionist_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(seco_tr_t37, second_amendment_boundary__insurrectionist_reading, theater_ratio, 37, 0.42).
narrative_ontology:measurement(seco_tr_t50, second_amendment_boundary__insurrectionist_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(seco_be_t8, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(seco_be_t16, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 16, 0.59).
narrative_ontology:measurement(seco_be_t25, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 25, 0.65).
narrative_ontology:measurement(seco_be_t37, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 37, 0.68).
narrative_ontology:measurement(seco_be_t50, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(seco_su_t8, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement(seco_su_t16, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 16, 0.67).
narrative_ontology:measurement(seco_su_t25, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(seco_su_t37, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 37, 0.71).
narrative_ontology:measurement(seco_su_t50, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 50, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__insurrectionist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_boundary__insurrectionist_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary__militia_conditioned_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, state_disarmament_as_tyranny_precursor).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, civilian_insurrection_viability).

% DUAL FORMULATION NOTE:
% The second_amendment_boundary kernel decomposes into three constraint stories: individual_right_reading (emphasizes operative clause, treats militia reference as motivational), insurrectionist_reading (this story — anchors rights in resistance doctrine), and militia_conditioned_reading (treats prefatory clause as limiting). Each reading instantiates a different constraint with different ε, victim sets, and beneficiaries. The readings coexist in constitutional discourse but differ on whether military-grade arms access is a constitutional right (individual and insurrectionist) or subject to comprehensive regulation (militia_conditioned). This story chains to downstream constraints about whether armed insurrection is viable and whether state disarmament efforts constitute tyranny precursors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_boundary__insurrectionist_reading, powerless, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
