% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__security_maximization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__security_maximization_reading, []).

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
 *   constraint_id: geneva_conventions_1949__security_maximization_reading
 *   human_readable: Geneva Conventions 1949: Security Maximization Reading
 *   domain: international/legal/security
 *
 * SUMMARY:
 *   The security-maximization reading of the 1949 Geneva Conventions
 *   interprets humanitarian protections as peacetime aspirations that must
 *   yield to operational necessity in asymmetric conflict. This reading
 *   expands the 'unlawful combatant' category to deny POW status and habeas
 *   corpus, normalizes indefinite detention without trial, applies 'human
 *   shields' doctrine to degrade civilian immunity, and legitimizes coercive
 *   interrogation as non-torture. The reading emerged prominently after 2001
 *   as states adapted international humanitarian law to irregular warfare. It
 *   transfers protection status and legal immunity from detained persons and
 *   civilians to state security apparatus, enabling extensive extraction of
 *   operational flexibility while suppressing legal and institutional
 *   constraints on detention, interrogation, and targeting. The claim/metric
 *   gap is deliberate: this reading claims to be a legitimate adaptation of
 *   the Conventions; the authored metrics describe a substantially extractive
 *   constraint whose persistence depends on suppressing legal review,
 *   humanitarian monitoring, and public transparency about detention and
 *   interrogation practices.
 *
 * KEY AGENTS:
 *   - State security apparatus: institutional power, agenda-setter, controls detention/interrogation/targeting interpretation and implementation
 *   - Irregular combatants: powerless, trapped, denied combatant status and POW protections
 *   - Detained persons without classification: powerless, trapped, held in legal limbo without trial or habeas corpus
 *   - Civilian populations in conflict zones: powerless, constrained, subjected to degraded immunity via 'human shields' doctrine
 *   - Humanitarian organizations: organized, excluded, barred from inspecting detention facilities or verifying compliance
 *   - International legal bodies: institutional, observer, positioned to evaluate reading consistency but systematically delegitimized
 *   - Liberal democratic publics: moderate power, beneficiary/excluded duality—benefit from security framing but excluded from detailed knowledge of practices
 *   - Non-compliant irregular adversaries: powerful locally, trapped, create the condition cited to justify indefinite suspension
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, 0.81).
domain_priors:suppression_score(geneva_conventions_1949__security_maximization_reading, 0.88).
domain_priors:theater_ratio(geneva_conventions_1949__security_maximization_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__security_maximization_reading, snare).
narrative_ontology:human_readable(geneva_conventions_1949__security_maximization_reading, "Geneva Conventions 1949: Security Maximization Reading").
narrative_ontology:topic_domain(geneva_conventions_1949__security_maximization_reading, "international/legal/security").

domain_priors:requires_active_enforcement(geneva_conventions_1949__security_maximization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__security_maximization_reading, 'd5815642-88fa-4593-a1bc-d57dae0b6f49').
narrative_ontology:cs_kernel_codification('d5815642-88fa-4593-a1bc-d57dae0b6f49', fixed_text).
narrative_ontology:cs_authority_grounding('d5815642-88fa-4593-a1bc-d57dae0b6f49', extraction).
narrative_ontology:cs_interpretation_layer_present('d5815642-88fa-4593-a1bc-d57dae0b6f49').
narrative_ontology:cs_reading_relation('d5815642-88fa-4593-a1bc-d57dae0b6f49', geneva_conventions_1949__humanitarian_ceiling_reading, forecloses).
narrative_ontology:cs_reading_relation('d5815642-88fa-4593-a1bc-d57dae0b6f49', geneva_conventions_1949__conditional_reciprocity_reading, coexists_with).
narrative_ontology:cs_axiom('d5815642-88fa-4593-a1bc-d57dae0b6f49', foundational, operational_necessity_overrides_humanitarian_floor).
narrative_ontology:cs_axiom_status(operational_necessity_overrides_humanitarian_floor, holdable).
narrative_ontology:cs_axiom_grounding('d5815642-88fa-4593-a1bc-d57dae0b6f49', operational_necessity_overrides_humanitarian_floor, instrumental).
narrative_ontology:cs_axiom('d5815642-88fa-4593-a1bc-d57dae0b6f49', foundational, asymmetric_warfare_requires_suspended_protections).
narrative_ontology:cs_axiom_status(asymmetric_warfare_requires_suspended_protections, holdable).
narrative_ontology:cs_axiom_grounding('d5815642-88fa-4593-a1bc-d57dae0b6f49', asymmetric_warfare_requires_suspended_protections, empirically_contingent).
narrative_ontology:cs_reference_frame('d5815642-88fa-4593-a1bc-d57dae0b6f49', state_survival_security_imperative).
narrative_ontology:cs_drift_state('d5815642-88fa-4593-a1bc-d57dae0b6f49', contemporary_institutionalized_indefinite_detention, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d5815642-88fa-4593-a1bc-d57dae0b6f49', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, state_security_apparatus).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, irregular_combatants).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, detained_persons_without_classification).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, civilian_populations_in_conflict_zones).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, civilian_populations_in_conflict_zones).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, liberal_democratic_publics).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__security_maximization_reading, state_survival_overrides_humanitarian_constraint).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__security_maximization_reading, asymmetric_threat_justifies_asymmetric_response).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and implements the Conventions through the security-maximization reading: expands unlawful combatant category to deny POW status and habeas corpus, normalizes indefinite detention without trial, applies 'human shields' doctrine to degrade civilian immunity, and normalizes coercive interrogation as non-torture. Claims operational necessity justifies these suspensions when facing irregular adversaries. Controls the machinery of detention, interrogation, classification, and targeting.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, state_security_apparatus, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Denied combatant status and POW protections under the security-maximization reading. Subject to indefinite detention without trial, coercive interrogation, and targeting without distinction from civilians. Their capture does not trigger the protections of Geneva III; their legal status is deliberately indeterminate to maximize state flexibility.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, irregular_combatants, payer,
    powerless, immediate, trapped, local).

% Held in indefinite detention without trial, without access to legal review or habeas corpus protections. The security-maximization reading permits states to hold persons in legal limbo—neither prosecuted nor released, neither classified as POW nor civilian—on the basis of security threat assessment alone. No independent judicial review required under this reading.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, detained_persons_without_classification, payer,
    powerless, immediate, trapped, local).

% Subjected to degraded protections under the security-maximization reading's application of 'human shields' doctrine and collateral damage acceptance thresholds. Dual role: superficially benefit from state security efforts against irregular adversaries, but bear the actual cost through attacks justified by proximity to suspected combatants or use of civilian areas for military purposes. Their immunity is conditional on demonstrable absence of support for insurgents—a condition that shifts the burden of proof away from the targeting state.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, civilian_populations_in_conflict_zones, payer,
    powerless, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__security_maximization_reading, civilian_populations_in_conflict_zones, beneficiary).

% Excluded from the core decision-making about detention, interrogation, and targeting under this reading. Their capacity to inspect facilities, interview detainees, and verify compliance is systematically limited or denied on security grounds. They would object to indefinite detention and coercive interrogation on humanitarian grounds if their voice were heeded; instead, they are treated as potential security threats if they advocate for stricter protections.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, humanitarian_organizations, excluded,
    organized, biographical, constrained, global).

% Positioned to evaluate whether the security-maximization reading is consistent with the text of the Conventions and customary international law. They hold analytical seats—no direct extraction or benefit, but significant interpretive authority over which reading prevails in specific cases. The security apparatus seeks to delegitimize or marginalize their role when it contradicts the security-maximization frame.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, international_legal_bodies, observer,
    institutional, generational, analytical, global).

% Positioned to benefit from state security measures against irregular adversaries, but excluded from detailed knowledge of detention, interrogation, and targeting practices justified under the security-maximization reading. The reading's operational necessity framing is presented as non-negotiable; public debate about whether suspensions are proportional or necessary is treated as a luxury the security imperative does not permit.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, liberal_democratic_publics, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__security_maximization_reading, liberal_democratic_publics, excluded).

% Positioned to respond to the security-maximization reading's suspension of protections. Their non-compliance with the Conventions (if documented) is cited as justification for the reading's application; their continued use of irregular tactics creates the condition the security apparatus uses to maintain suspensions indefinitely.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, non_compliant_irregular_adversaries, observer,
    powerful, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_1949__security_maximization_reading, state_security_apparatus).
narrative_ontology:fixing_cost_class(geneva_conventions_1949__security_maximization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Conventions themselves coordinate humanitarian restraint across state actors—a public goods problem: any state that unilaterally observes them while others do not faces competitive disadvantage. The security-maximization reading dissolves this coordination problem by permitting unilateral suspension whenever state security is invoked, replacing reciprocal restraint with operational necessity.
% TRANSFER_FUNCTION: Transfers protection status (and thus legal immunity from targeting, detention, interrogation) from irregular combatants and civilians in conflict zones to the state security apparatus. The apparatus gains discretion over detention, interrogation method, targeting rules, and classification—constraints that would otherwise limit these powers are suspended. The cost is borne by those denied combatant status, held without trial, subjected to coercive interrogation, or targeted via 'human shields' doctrine.
% ABSENT_VOICES: Detained persons without access to counsel, humanitarian monitors barred from facilities, civilian populations subject to 'human shields' targeting doctrine and collateral damage acceptance—these groups would object if they had a seat at the table, but the security-maximization reading positions them as threats to operational security and thus excludes them from the interpretive process itself.
% DISAPPEARANCE_RATIONALE: If the security-maximization reading disappeared—if states reverted to the humanitarian_ceiling_reading or conditional_reciprocity_reading—the machinery of indefinite detention without trial, coercive interrogation, and degraded civilian immunity would have to be dismantled or legitimized through other frameworks. Captured irregular combatants would have access to legal review, POW status and habeas corpus, or due-process protections. The state security apparatus would lose the unilateral interpretive authority it currently exercises over Conventions compliance.
% FOUNDING_PROBLEM: The Conventions were drafted in 1949 on the assumption of symmetrical warfare between state militaries with similar capabilities and compliance incentives. Irregular, asymmetric conflict emerged (insurgency, terrorism, proxy warfare) where adversaries lack uniforms, fixed bases, or incentive to comply. States faced a dilemma: apply Conventions designed for symmetrical warfare to asymmetrical conflict and lose operational flexibility, or reinterpret the Conventions to permit necessary flexibility.
% FOUNDING_PROBLEM_CORROBORATION: The state security apparatus attests the founding problem is live and acute—asymmetric threats justify flexibility that the Conventions' 1949 framers could not have anticipated. Humanitarian organizations, international legal scholars outside the security establishment, and human-rights advocates attest the founding problem is an exaggeration used to justify systematic suspension of protections; they argue the Conventions are adaptable without abandoning humanitarian floors. Legislative testimony and academic literature from non-state-aligned sources (International Committee of the Red Cross, academic international law) support the 'exaggeration for expedience' reading.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__security_maximization_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__security_maximization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__security_maximization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_1949__security_maximization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__security_maximization_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__security_maximization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__security_maximization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81 at interval end) because the security-maximization reading concentrates discretion over detention classification, interrogation method, and targeting in the state security apparatus, removing constraints that would otherwise apply. Suppression is higher still (0.88) because the reading's persistence depends on suppressing legal review, humanitarian access, public disclosure of detention practices, and institutional challenges to state classification decisions. Theater ratio is substantial (0.62) because the security-maximization reading increasingly maintains the appearance of Conventions compliance while systematically reinterpreting key protections—the 'operational necessity' language is performative, masking the expansion of discretion. Accessibility collapse is moderate-high (0.72) because alternatives to the security-maximization reading exist (humanitarian ceiling, conditional reciprocity) but are systematically delegitimized as naive about asymmetric threats. Resistance is moderate (0.58) because humanitarian organizations, international legal scholars, and some state actors resist, but the resistance lacks enforcement power against states wielding the security-maximization frame. The measurement series track the reading's entrenchment from 2001 (post-9/11 emergence) through 2024: extractiveness and suppression rise sharply through 2011 as indefinite detention and coercive interrogation become normalized, then plateau as the reading stabilizes as operational practice. Theater ratio tracks the increasing gap between the reading's humanitarian veneer and its actual operation.
 *
 * PERSPECTIVAL GAP:
 *   From the state security apparatus seat, the security-maximization reading is a rational adaptation of the Conventions to asymmetric threat; protections designed for symmetrical warfare are constraints on necessary flexibility. From the irregular combatants' and detained persons' seat, the reading is a mask for systematic extraction of legal status, protection, and due process. From humanitarian organizations' seat, the reading is operationally dishonest—it claims necessity where alternatives exist and have been empirically tested (conditional reciprocity or humanitarian ceiling readings produce comparable security outcomes without sacrificing detainee protections). These divergences flow from the structural data: the security apparatus holds institutional power and sets the classification agenda; detained persons hold no power and are trapped. The engine computes these per-seat divergences from power, exit options, and beneficiary/victim declarations—the security-maximization reading appears legitimate from institutional seats but extractive from powerless seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The state security apparatus sits at d ≈ 0.0–0.2 (full beneficiary): it gains discretionary authority, removes constraints on detention and interrogation, and faces no penalty for suspensions framed as operational necessity. Irregular combatants sit at d ≈ 0.95–1.0 (full target): denied POW status, held without trial, subjected to coercive interrogation, with no legal remedy. Detained persons without classification sit at d ≈ 0.92 (near-full target): trapped in indefinite detention with no habeas corpus or legal review. Civilian populations in conflict zones sit at d ≈ 0.6–0.7 (asymmetric but with ambiguity): they superficially benefit from state security operations against irregular adversaries, but bear the actual cost through degraded immunity and 'human shields' doctrine application; the directionality is high because their situation is defined by the constraint without their consent. Humanitarian organizations sit at d ≈ 0.65 (target in the interpretive sense): excluded from decision-making, their monitoring capacity is suppressed, their advocacy is delegitimized. International legal bodies sit at d ≈ 0.55 (moderately asymmetric): they retain analytical authority but face systematic marginalization when they contradict the security-maximization frame. Liberal democratic publics sit at d ≈ 0.35 (weak beneficiary with suppression): they benefit from framed security narrative but are excluded from detailed knowledge of practices, creating suppressed resistance.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem for the Conventions was reciprocal restraint in symmetrical interstate warfare. The security-maximization reading reframes the founding problem as asymmetric threat—a problem the 1949 framers did not anticipate. This reframing is crucial: if the founding problem remains reciprocal restraint, the security-maximization reading violates it by suspending restraint unilaterally; if the founding problem has shifted to asymmetric threat, the reading addresses it directly. The 'contested' status in six_questions.founding_problem_status reflects this uncertainty. The measurement series show extractiveness rising 80% over two decades, theater ratio rising 148% over the same period—the constraint is becoming MORE extractive and MORE performative, not less. This trajectory suggests the founding problem (whatever it is) is not being solved; instead, the security-maximization reading is entrenching itself as operational practice independent of whether asymmetric threats are actually being mitigated. A Mandatrophy case: if the founding problem was asymmetric threat mitigation, the plateau in extractiveness after 2017 would be concerning—the security apparatus continues extracting authority after the threat environment has (arguably) stabilized or shifted. The rising theater ratio suggests the reading is increasingly maintained through interpretive performance rather than demonstrated operational effectiveness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_necessity_definition_ambiguity,
    'What constitutes ''operational necessity'' sufficient to suspend Conventions protections? Is the threshold determined by the security apparatus unilaterally, negotiated internationally, or subject to judicial review?',
    'Comparative legal analysis of how states operationalize ''necessity'' across different conflict contexts; examination of whether judicial review (international or domestic) has ever overturned a state''s ''operational necessity'' determination.',
    'If the threshold is unilaterally determined by states and never overturned, the security-maximization reading permits indefinite suspension and is effectively a snare. If the threshold is externally constrained (international negotiation, judicial review), the reading remains extractive but with measurable limits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_necessity_definition_ambiguity, empirical, 'Whether ''operational necessity'' is a genuine constraint on state discretion or a unilateral interpretive power.').

omega_variable(
    asymmetric_threat_vs_1949_framing_problem,
    'Is the founding problem genuinely that the 1949 Conventions were designed for symmetrical warfare and cannot be applied to asymmetric conflict without collapse? Or is this an exaggerated framing used to justify suspensions that serve other interests (cost reduction, interrogation flexibility, intelligence collection)?',
    'Empirical analysis of whether humanitarian-ceiling and conditional-reciprocity readings have been operationalized successfully in asymmetric conflicts; examination of whether states claiming operational necessity have documented specific operational failures from Conventions compliance.',
    'If the Conventions are demonstrably adaptable to asymmetric conflict without suspension (evidence from conditional-reciprocity or humanitarian-ceiling implementations), the founding problem is exaggerated and the security-maximization reading is primarily extractive. If adaptation has failed, the reading addresses a real structural limit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetric_threat_vs_1949_framing_problem, empirical, 'Whether asymmetric threat is a genuine design limit of the Conventions or a cover story for extraction.').

omega_variable(
    indefinite_detention_security_claim,
    'Does indefinite detention without trial actually produce better security outcomes (intelligence, threat mitigation, prevention of future attacks) than detention with habeas corpus review? Or is indefinite detention maintained because it is cheaper, simpler administratively, and maintains larger discretionary population under state control?',
    'Comparative analysis of security outcomes in conflict contexts using indefinite detention vs. detention with judicial review; examination of whether indefinite detainees produce actionable intelligence at higher rates than those held with legal protections.',
    'If indefinite detention produces demonstrably better security outcomes, the security-maximization reading addresses an operational necessity. If outcomes are comparable or indefinite detention performs worse, the reading is primarily extraction under security cover.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indefinite_detention_security_claim, empirical, 'Whether indefinite detention''s security justification corresponds to empirical operational effectiveness.').

omega_variable(
    reading_foreclosure_structure,
    'Does the security-maximization reading''s core premise (operational necessity overrides humanitarian protection) logically foreclose the humanitarian-ceiling reading''s core premise (absolute humanitarian minimums), or do both readings coexist as competing interpretations that states choose between?',
    'Analysis of whether a state adopting the security-maximization reading is logically committed to rejecting humanitarian-ceiling premises, or whether the two framings remain live options for different states or different phases of conflict.',
    'If foreclosure is genuine (not possible to hold both premises simultaneously within one legal framework), the readings are in genuine conflict. If both remain live options, they coexist as different parties'' interpretations of the same text.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_structure, conceptual, 'Whether the security-maximization and humanitarian-ceiling readings are logically contradictory or pragmatically competing.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the high suppression value (0.88) driven by structural exclusion of humanitarian monitors and detained persons from legal review (structural suppression)? Or is it increasingly driven by detained persons'' cognitive/psychological break from legal advocacy capacity after extended indefinite detention (internalized suppression)?',
    'Post-release study of indefinite-detention survivors: do suppression effects persist after exit from state custody and legal protection regain accessibility? If suppression persists, reclassify as partially internalized.',
    'If internalized, the effective suppression is higher than the structural measurement suggests because the target carries the suppression with them after exit and cannot effectively use legal remedies even when they become available.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression mechanism in indefinite detention regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__security_maximization_reading, 2001, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t2001, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2001, 0.25).
narrative_ontology:measurement_basis(gene_tr_t2001, observed).
narrative_ontology:measurement(gene_tr_t2006, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2006, 0.38).
narrative_ontology:measurement_basis(gene_tr_t2006, observed).
narrative_ontology:measurement(gene_tr_t2011, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2011, 0.5).
narrative_ontology:measurement_basis(gene_tr_t2011, observed).
narrative_ontology:measurement(gene_tr_t2017, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2017, 0.58).
narrative_ontology:measurement_basis(gene_tr_t2017, observed).
narrative_ontology:measurement(gene_tr_t2021, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2021, 0.61).
narrative_ontology:measurement_basis(gene_tr_t2021, observed).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2024, 0.62).
narrative_ontology:measurement_basis(gene_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(gene_be_t2001, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2001, 0.45).
narrative_ontology:measurement_basis(gene_be_t2001, observed).
narrative_ontology:measurement(gene_be_t2006, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2006, 0.62).
narrative_ontology:measurement_basis(gene_be_t2006, observed).
narrative_ontology:measurement(gene_be_t2011, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2011, 0.71).
narrative_ontology:measurement_basis(gene_be_t2011, observed).
narrative_ontology:measurement(gene_be_t2017, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2017, 0.78).
narrative_ontology:measurement_basis(gene_be_t2017, observed).
narrative_ontology:measurement(gene_be_t2021, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2021, 0.8).
narrative_ontology:measurement_basis(gene_be_t2021, observed).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2024, 0.81).
narrative_ontology:measurement_basis(gene_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t2001, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2001, 0.52).
narrative_ontology:measurement_basis(gene_su_t2001, observed).
narrative_ontology:measurement(gene_su_t2006, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2006, 0.68).
narrative_ontology:measurement_basis(gene_su_t2006, observed).
narrative_ontology:measurement(gene_su_t2011, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2011, 0.78).
narrative_ontology:measurement_basis(gene_su_t2011, observed).
narrative_ontology:measurement(gene_su_t2017, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2017, 0.85).
narrative_ontology:measurement_basis(gene_su_t2017, observed).
narrative_ontology:measurement(gene_su_t2021, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2021, 0.87).
narrative_ontology:measurement_basis(gene_su_t2021, observed).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2024, 0.88).
narrative_ontology:measurement_basis(gene_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__security_maximization_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_1949__security_maximization_reading, 0.18).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, conditional_reciprocity_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, humanitarian_ceiling_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, international_humanitarian_law_domestic_enforcement).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, coercive_interrogation_normalization).

% DUAL FORMULATION NOTE:
% This reading is one of three structurally distinct interpretations of the Geneva Conventions 1949 kernel. All three readings share the same textual referent (the 1949 Conventions) but instantiate different ε values, beneficiary/victim structures, and classification types. The security-maximization reading (this file) instantiates high extraction (0.81), systematic suppression (0.88), and snare classification. The humanitarian-ceiling reading instantiates lower extraction (~0.15–0.25), minimal suppression (~0.10), and rope/mountain classification. The conditional-reciprocity reading instantiates moderate extraction (~0.45–0.55), conditional suppression (~0.40–0.60), and tangled-rope classification. Each reading is a valid constraint story with its own ε-invariance, beneficiary/victim declarations, and stakeholder surface. They are linked via affects_constraints to represent the constraint family structure and enable comparison of how different readings of the same kernel produce different structural classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_1949__security_maximization_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
