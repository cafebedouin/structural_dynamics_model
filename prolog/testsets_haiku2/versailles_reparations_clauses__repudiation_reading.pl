% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__repudiation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_versailles_reparations_clauses__repudiation_reading, []).

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
 *   constraint_id: versailles_reparations_clauses__repudiation_reading
 *   human_readable: Versailles Reparations Clauses (Repudiation Reading)
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   The Treaty of Versailles (1919) imposed reparations and territorial
 *   cessions on Germany under the threat of military occupation and continued
 *   blockade. This constraint story instantiates the REPUDIATION READING: the
 *   treaty's signature was coerced, therefore the obligation to pay is
 *   illegitimate, and Germany bears no binding duty beyond token gestures.
 *   This reading contests the authority of the treaty itself and denies the
 *   credibility of the entire reparations regime. The claim is NOT that
 *   reparations are too high or that Germany's capacity is limited (those are
 *   sibling readings); the repudiation reading argues the obligation is void
 *   ab initio. Extractiveness is very high (0.88 at interval end) because the
 *   reading frames the constraint as pure coercion producing no legitimate
 *   coordination benefit. Suppression is high because enforcement depends on
 *   military threat and the suppression of German refusal to honor an
 *   illegitimate obligation.
 *
 * KEY AGENTS:
 *   - German state: victim and payer under the repudiation reading; forced to sign under duress; trapped in an obligation framed as illegitimate
 *   - German population: the domestic base for repudiation sentiment; bears the extraction costs; identity increasingly fused with the grievance of imposed debt
 *   - Allied creditor powers: beneficiaries and agenda-setters; enforce the obligation through occupation and threat; the repudiation reading casts them as extractors using military power
 *   - Successor German governments: inherit the obligation and the repudiation sentiment; eventually reject it unilaterally (Nazi rearmament)
 *   - International legal community: analyzes whether duress voids treaties; debates the legitimacy of victor-imposed obligations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__repudiation_reading, 0.88).
domain_priors:suppression_score(versailles_reparations_clauses__repudiation_reading, 0.79).
domain_priors:theater_ratio(versailles_reparations_clauses__repudiation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, resistance, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__repudiation_reading, snare).
narrative_ontology:human_readable(versailles_reparations_clauses__repudiation_reading, "Versailles Reparations Clauses (Repudiation Reading)").
narrative_ontology:topic_domain(versailles_reparations_clauses__repudiation_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__repudiation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__repudiation_reading, '4140862c-e0d1-4788-85b7-91437de537fd').
narrative_ontology:cs_kernel_codification('4140862c-e0d1-4788-85b7-91437de537fd', fixed_text).
narrative_ontology:cs_authority_grounding('4140862c-e0d1-4788-85b7-91437de537fd', extraction).
narrative_ontology:cs_interpretation_layer_present('4140862c-e0d1-4788-85b7-91437de537fd').
narrative_ontology:cs_reading_relation('4140862c-e0d1-4788-85b7-91437de537fd', versailles_reparations_clauses__punitive_liability_reading, forecloses).
narrative_ontology:cs_reading_relation('4140862c-e0d1-4788-85b7-91437de537fd', versailles_reparations_clauses__limited_responsibility_reading, coexists_with).
narrative_ontology:cs_axiom('4140862c-e0d1-4788-85b7-91437de537fd', foundational, duress_voids_treaty_obligation).
narrative_ontology:cs_axiom_status(duress_voids_treaty_obligation, holdable).
narrative_ontology:cs_axiom_grounding('4140862c-e0d1-4788-85b7-91437de537fd', duress_voids_treaty_obligation, deontological).
narrative_ontology:cs_axiom('4140862c-e0d1-4788-85b7-91437de537fd', foundational, sovereign_equality_principle).
narrative_ontology:cs_axiom_status(sovereign_equality_principle, holdable).
narrative_ontology:cs_axiom_grounding('4140862c-e0d1-4788-85b7-91437de537fd', sovereign_equality_principle, deontological).
narrative_ontology:cs_reference_frame('4140862c-e0d1-4788-85b7-91437de537fd', sovereign_consent_doctrine).
narrative_ontology:cs_drift_state('4140862c-e0d1-4788-85b7-91437de537fd', weimar_era_end, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('4140862c-e0d1-4788-85b7-91437de537fd', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, allied_creditor_powers).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, german_state).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, german_population).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, successor_german_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Forced to sign the Treaty of Versailles under threat of military occupation and continued blockade. Obligated to transfer enormous wealth to Allied creditors over decades, ceding territory, industrial capacity, and colonies. The repudiation reading holds that the duress context makes the signature illegitimate and the payment obligation void—yet enforcement mechanisms prevent exit without military defiance or unilateral renunciation that invites retaliation.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_state, payer,
    powerful, generational, trapped, national).

% Bears the domestic cost of reparations through taxation, inflation, reduced public services, and economic austerity. National identity becomes fused with the grievance of an imposed, illegitimate debt—the repudiation reading frames their suffering as extraction under duress imposed on a sovereign people who had no legitimate choice.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_population, payer,
    powerless, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__repudiation_reading, german_population, payer).

% Collect reparations payments and enforce them through occupation, asset seizure, and threat of military intervention. They claim the payments are just recompense for war costs inflicted by German aggression; the repudiation reading argues they extract via coercive enforcement of an illegitimate obligation, using superior military position to suppress German refusal.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, allied_creditor_powers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__repudiation_reading, allied_creditor_powers, agenda_setter).

% Observe the enforcement regime and are affected by German economic collapse and political instability. They witness whether coercive debt enforcement destabilizes the international order or whether legitimate creditor claims justify the terms.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, neutral_states, observer,
    moderate, biographical, mobile, global).

% Inherit the obligation and the domestic grievance. Later Weimar and Nazi administrations use the repudiation reading to mobilize opposition to the treaty, eventually rejecting obligations unilaterally. The reading becomes politically foundational to German resistance and rearmament.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, successor_german_governments, payer,
    powerful, generational, constrained, national).

% Debates whether treaties signed under duress are binding, whether victors can impose unlimited obligations on defeated powers, and whether the doctrine of international law permits or condemns the Versailles enforcement regime. The reading tests foundational principles of treaty legitimacy.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, international_legal_community, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(versailles_reparations_clauses__repudiation_reading, allied_creditor_powers).
narrative_ontology:fixing_cost_class(versailles_reparations_clauses__repudiation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. The repudiation reading denies any coordination function; Versailles is framed as pure extraction under coercion, not as a mechanism solving a genuine collective problem. There is no coordination to defend.
% TRANSFER_FUNCTION: Moves capital, commodities, territory, and sovereignty from Germany to Allied creditors—enforced through occupation, asset seizure, and threat of military retaliation against refusal. The mechanism is extractive not because the cost is high, but because the obligation itself is illegitimate under duress.
% ABSENT_VOICES: German negotiators were given a signed treaty with a take-it-or-leave-it ultimatum; their objections to the duress and the terms were not negotiated but overridden. The repudiation reading treats the German state's refusal to consent as the authentic voice suppressed by the coercive enforcement regime.
% DISAPPEARANCE_RATIONALE: If the reparations obligation disappeared, Germany would retain its territories, industrial capacity, and sovereign fiscal autonomy—it would not need to rearm defensively or destabilize Europe to escape the debt trap. Allied war-cost recovery would fail, forcing a renegotiation of postwar financial settlement. The international order would realign around a different power distribution.
% FOUNDING_PROBLEM: The founding problem framed by the repudiation reading is not a problem to be solved by Versailles, but rather the problem Versailles creates: how a victor imposes obligations on a defeated enemy without consent. The 'problem' the treaty addresses (from the Allied perspective) is the need to extract resources and security guarantees; the repudiation reading denies this is a legitimate problem at all.
% FOUNDING_PROBLEM_CORROBORATION: Allied statesmen (Clemenceau, Lloyd George) attest the treaty's purpose was to extract security and compensation; German legal scholars and statesmen (the Weimar governments, later Nazi ideology) attested the treaty was illegitimate coercion. No neutral party outside both beneficiary and victim sets attests a shared founding problem—the problem itself is what is contested. International legal scholars document the duress context; consensus does not exist on whether duress voids the treaty.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__repudiation_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__repudiation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__repudiation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(versailles_reparations_clauses__repudiation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__repudiation_reading, 0.88, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(versailles_reparations_clauses__repudiation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(versailles_reparations_clauses__repudiation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(versailles_reparations_clauses__repudiation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The repudiation reading frames Versailles as extraction because: (1) extractiveness is high — the obligation transfers enormous wealth and territory from a defeated power to victors, with no reciprocal benefit to Germany; (2) suppression is high — refusal to pay invites military retaliation, and the reading itself is suppressed in Allied discourse which frames the treaty as legitimate settlement; (3) theater is moderate and rising — early enforcement is direct (occupation, asset seizure), but over time enforcement requires performative legitimacy (legal procedures, League of Nations involvement) to sustain compliance; (4) accessibility_collapse is high (0.72) because the trapped exit (military retaliation against refusal) is the only alternative, foreclosing negotiation; (5) resistance is high (0.81) because the reading mobilizes widespread German opposition and eventually drives rearmament and treaty repudiation. The measurement series traces extractiveness rising from 0.78 at t=0 (initial imposition) to 0.88 at t=21 (plateau under Weimar), as enforcement hardens and German resentment deepens. Suppression rises similarly as the enforcement machinery matures and German refusal becomes more organized.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (Allied powers) and the payer seat (Germany) compute radically different types from the same structural data. From the Allied perspective, Versailles is legitimate punishment and security settlement — a rope or tangled_rope framing, solving the coordination problem of postwar order. From the German seat, the same terms are illegitimate extraction — a snare framing, suppressing refusal via military threat. The divergence is not about which metrics are true, but about whether the obligation is binding at all. The repudiation reading privileges the German seat's assessment: the duress context voids the treaty, making the constraint purely extractive regardless of how the Allies frame it.
 *
 * DIRECTIONALITY LOGIC:
 *   German state and population are targets (d near 1.0): they are the identified victims whose sovereignty and resources are extracted. The constraint's persistence depends on their continued compliance despite the repudiation reading's assertion that they have no legitimate obligation. Allied powers are beneficiaries (d near 0.0): they collect the transfer. The directionality is straightforward because the repudiation reading treats the entire framework as illegitimate coercion, not as mixed coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question is: does this constraint's founding purpose still stand? The repudiation reading denies the founding purpose was ever legitimate — the 'problem' the treaty solved (Allied security and compensation extraction) is itself illegitimate from the German perspective. By t=21, the founding problem is dead by the repudiation reading's lights (Germany is rearmament-focused, the treaty's security logic has failed), but the constraint persists via suppression. This is a classic mandatrophy signature: the original justification (preventing future German aggression through reparations and military limitation) has been overtaken by the resentment the reparations themselves generate, which ultimately drives German rearmament and treaty repudiation. The constraint becomes a mechanism for producing the very outcome it was meant to prevent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    duress_doctrine_legitimacy,
    'Does international law recognize that treaties signed under military duress are void ab initio, or are victors permitted to impose obligations on defeated powers as an exercise of legitimate state power?',
    'Historical and comparative international law analysis of duress doctrine; examination of whether duress-voidance is treated as a principle of pacta sunt servanda or an exception with limits; post-WWII jurisprudence on imposed treaties and reparations.',
    'If duress voids treaties, the repudiation reading is structurally sound and the entire reparations regime lacks legitimacy. If duress is not a recognized exception, the repudiation reading''s core premise collapses and the constraint becomes a negotiation over legitimate terms, not illegitimate coercion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(duress_doctrine_legitimacy, conceptual, 'Whether duress voids international treaties or is not a recognized exception to pacta sunt servanda.').

omega_variable(
    sovereignty_preservation_intent,
    'Did the Allied powers genuinely intend to preserve German sovereignty and capacity for recovery, or did they intend indefinite extraction and subordination masked by treaty language?',
    'Historical analysis of Allied strategic intent (documents, memoirs, diplomatic records); examination of whether the reparations schedule was framed as temporary transition or permanent subordination; analysis of whether later debt forgiveness and economic aid represent acknowledgment of overreach or pragmatic acceptance of German recovery as necessary to Allied security.',
    'Evidence of predatory intent would support the extraction framing; evidence of genuine capacity-limiting intent would support the limited_responsibility reading and complicate the repudiation reading''s claim of pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_preservation_intent, empirical, 'Whether Allied intent was predatory extraction or legitimate security settlement with bounded obligations.').

omega_variable(
    alternative_treaty_availability,
    'Under the duress context (military threat, continued blockade), did Germany have any real alternative to signing, or was refusal an impossible choice that negates consent?',
    'Counterfactual historical analysis: what would have happened had Germany refused to sign? Would occupation have proceeded, or would negotiation have occurred? Examination of whether the Allied ultimatum provided a meaningful choice or foreclosed refusal entirely.',
    'If refusal was foreclosed, the duress claim is strongest and the repudiation reading''s legitimacy argument is reinforced. If Germany had negotiation options, duress is weaker and the treaty begins to look like a harsh but legitimate settlement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_treaty_availability, empirical, 'Whether the duress context foreclosed meaningful alternatives or negotiation was possible.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.79) structural (military threat, occupation machinery, asset seizure) or internalized (German population accepts the obligation as legitimate despite hating the terms)?',
    'Analysis of German compliance: does the population comply under threat or under acceptance of legitimacy? Post-exit trajectory (what happens to German resentment after repudiation and rearmament?) indicates whether suppression was structural or internalized.',
    'If suppression is purely structural, the constraint depends entirely on military enforcement; if partly internalized, some German populations have incorporated the obligation as legitimate. The measured suppression reflects structural enforcement (occupation, threat, asset seizure), but the repudiation reading''s political power suggests the suppression is being overcome by the internal rejection of the obligation''s legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression is structural enforcement or partly internalized acceptance of legitimacy.').

omega_variable(
    reading_commission_ambiguity,
    'Is this constraint one reading of a genuinely contested kernel (three parties holding different readings of the same treaty), or are the readings after-the-fact interpretive frames imposed by outside observers, not authentic to the parties'' own sense of legitimacy?',
    'Historical evidence of whether German, Allied, and neutral parties explicitly adopted these readings as their own legitimacy claims, or whether the readings are analytical reconstructions by historians and legal scholars.',
    'If the readings are authentic to the parties (German leaders explicitly claimed duress-voidance, Allies explicitly claimed punitive legitimacy, neutrals explicitly claimed capacity-limiting legitimacy), the kernel framing is correct. If the readings are analytical retroductions, the kernel framing misattributes agency and the constraint is better understood as a single imposed arrangement with contested interpretation post-hoc.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_commission_ambiguity, conceptual, 'Whether the three readings are authentic to the parties'' own legitimacy claims or analytical reconstructions by outside observers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__repudiation_reading, 0, 21).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t0, versailles_reparations_clauses__repudiation_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(vers_tr_t3, versailles_reparations_clauses__repudiation_reading, theater_ratio, 3, 0.31).
narrative_ontology:measurement(vers_tr_t6, versailles_reparations_clauses__repudiation_reading, theater_ratio, 6, 0.34).
narrative_ontology:measurement(vers_tr_t9, versailles_reparations_clauses__repudiation_reading, theater_ratio, 9, 0.37).
narrative_ontology:measurement(vers_tr_t12, versailles_reparations_clauses__repudiation_reading, theater_ratio, 12, 0.39).
narrative_ontology:measurement(vers_tr_t15, versailles_reparations_clauses__repudiation_reading, theater_ratio, 15, 0.41).
narrative_ontology:measurement(vers_tr_t18, versailles_reparations_clauses__repudiation_reading, theater_ratio, 18, 0.42).
narrative_ontology:measurement(vers_tr_t21, versailles_reparations_clauses__repudiation_reading, theater_ratio, 21, 0.42).

% Extraction over time
narrative_ontology:measurement(vers_be_t0, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(vers_be_t3, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 3, 0.81).
narrative_ontology:measurement(vers_be_t6, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 6, 0.84).
narrative_ontology:measurement(vers_be_t9, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 9, 0.86).
narrative_ontology:measurement(vers_be_t12, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 12, 0.87).
narrative_ontology:measurement(vers_be_t15, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 15, 0.88).
narrative_ontology:measurement(vers_be_t18, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 18, 0.88).
narrative_ontology:measurement(vers_be_t21, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 21, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t0, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(vers_su_t3, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 3, 0.71).
narrative_ontology:measurement(vers_su_t6, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 6, 0.74).
narrative_ontology:measurement(vers_su_t9, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 9, 0.76).
narrative_ontology:measurement(vers_su_t12, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 12, 0.77).
narrative_ontology:measurement(vers_su_t15, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 15, 0.78).
narrative_ontology:measurement(vers_su_t18, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 18, 0.79).
narrative_ontology:measurement(vers_su_t21, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 21, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__repudiation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(versailles_reparations_clauses__repudiation_reading, 0.25).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses__limited_responsibility_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses__punitive_liability_reading).

% DUAL FORMULATION NOTE:
% The Versailles reparations clauses are a contested kernel with three structurally distinct readings: repudiation_reading (this constraint) holds the treaty is void under duress and Germany owes nothing; limited_responsibility_reading holds reparations are legitimate but bounded by economic capacity; punitive_liability_reading holds Germany bears quasi-unlimited liability for total war costs. Each reading has a different ε (extractiveness), different beneficiary/victim structure, different type. The repudiation reading forecloses the punitive reading (both cannot coexist in one framework) and influences the limited reading (establishes duress as a foundational challenge). Each reading must be authored separately with its own structural data; the kernel relationship is recorded here and in each story's omega variables documenting the alternative readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(versailles_reparations_clauses__repudiation_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
