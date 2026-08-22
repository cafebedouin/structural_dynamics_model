% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__repudiation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: versailles_reparations_clauses__repudiation_reading
 *   human_readable: Versailles Treaty Reparations Clauses (Repudiation Reading)
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   The Treaty of Versailles (1919) imposed war reparations on Germany
 *   through Articles 231 and following, backed by military occupation and the
 *   threat of resumed hostilities. This constraint story instantiates the
 *   REPUDIATION READING: the treaty itself was imposed under duress (Germany
 *   had no viable choice to refuse), and therefore the reparations clauses
 *   lack legitimate binding force. Under this reading, Germany is the victim
 *   of extraction, not a legitimate debtor. The constraint is the reparations
 *   obligation itself — treated as a coercive imposition by this reading, a
 *   justified creditor claim by competing readings.
 *
 * KEY AGENTS:
 *   - German state: forced signatory, trapped debtor under duress reading; powerful actor but no exit
 *   - Allied creditor powers (France, Britain, USA): beneficiaries and enforcers of the reparations extraction
 *   - German population: bears the real cost through taxation and austerity; identity-locked to the state
 *   - League of Nations: observer of treaty legitimacy; mandate undermined if treaty is duress-imposed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__repudiation_reading, 0.92).
domain_priors:suppression_score(versailles_reparations_clauses__repudiation_reading, 0.88).
domain_priors:theater_ratio(versailles_reparations_clauses__repudiation_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, accessibility_collapse, 0.67).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__repudiation_reading, snare).
narrative_ontology:human_readable(versailles_reparations_clauses__repudiation_reading, "Versailles Treaty Reparations Clauses (Repudiation Reading)").
narrative_ontology:topic_domain(versailles_reparations_clauses__repudiation_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__repudiation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__repudiation_reading, '867a3a09-ee7a-4e43-875b-bd139ba3474f').
narrative_ontology:cs_kernel_codification('867a3a09-ee7a-4e43-875b-bd139ba3474f', fixed_text).
narrative_ontology:cs_authority_grounding('867a3a09-ee7a-4e43-875b-bd139ba3474f', extraction).
narrative_ontology:cs_interpretation_layer_present('867a3a09-ee7a-4e43-875b-bd139ba3474f').
narrative_ontology:cs_reading_relation('867a3a09-ee7a-4e43-875b-bd139ba3474f', versailles_reparations_clauses__punitive_liability_reading, forecloses).
narrative_ontology:cs_reading_relation('867a3a09-ee7a-4e43-875b-bd139ba3474f', versailles_reparations_clauses__limited_responsibility_reading, influences).
narrative_ontology:cs_axiom('867a3a09-ee7a-4e43-875b-bd139ba3474f', foundational, treaty_legitimacy_requires_consent).
narrative_ontology:cs_axiom_status(treaty_legitimacy_requires_consent, holdable).
narrative_ontology:cs_axiom_grounding('867a3a09-ee7a-4e43-875b-bd139ba3474f', treaty_legitimacy_requires_consent, deontological).
narrative_ontology:cs_axiom('867a3a09-ee7a-4e43-875b-bd139ba3474f', foundational, duress_voids_obligations).
narrative_ontology:cs_axiom_status(duress_voids_obligations, holdable).
narrative_ontology:cs_axiom_grounding('867a3a09-ee7a-4e43-875b-bd139ba3474f', duress_voids_obligations, deontological).
narrative_ontology:cs_reference_frame('867a3a09-ee7a-4e43-875b-bd139ba3474f', treaty_signed_under_duress).
narrative_ontology:cs_drift_state('867a3a09-ee7a-4e43-875b-bd139ba3474f', post_1923_hyperinflation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('867a3a09-ee7a-4e43-875b-bd139ba3474f', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, allied_creditor_powers).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, german_state).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, german_population).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__repudiation_reading, duress_voids_contracts).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__repudiation_reading, treaty_legitimacy_requires_consent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Forced to sign the Treaty of Versailles under threat of resumed hostilities after military collapse. Under this reading, the imposed reparations clauses (Article 231 and the Schedule of Payments) represent extortion: a defeated power extracting payment through coercion, not legitimate creditor claims. Germany bears legal and financial obligations it did not consent to. Exit would be military default or treaty renunciation — both carry invasion risk. The repudiation reading asserts Germany has no moral or legal obligation to honor payments imposed under these duress conditions.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_state, payer,
    powerful, generational, trapped, continental).

% France, Britain, the United States, and others claim reparations from Germany for war damages and costs. They set the treaty terms, enforce collection through occupation and threats, and benefit directly from reparations payments. This reading identifies them as extractors using military victory to impose financial terms the defeated party never genuinely agreed to.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, allied_creditor_powers, beneficiary,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__repudiation_reading, allied_creditor_powers, agenda_setter).

% Bears the real cost of reparations through taxation, inflation, wage suppression, and austerity. The repudiation reading frames these burdens as illegitimate — the result of a coerced treaty, not a debt the German nation legitimately owes. Exit would require national renunciation of the treaty, which carries geopolitical isolation and military threat risk. The population is identity-locked to the German state and cannot exit the constraint independently.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_population, payer,
    powerless, biographical, identity_locked, continental).

% Would argue that legitimacy requires consent of the governed and that duress-imposed treaties are not binding. Neutral powers in 1919 could not participate in treaty negotiation; German domestic creditors (workers, businesses owed back pay and contracts) have claims subordinated to reparations. Both would benefit from treaty repudiation but are structurally excluded from the reparations negotiation.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, neutral_powers_and_domestic_creditors, excluded,
    moderate, generational, constrained, continental).

% The treaty is the foundation of the League's mandate to prevent future war through collective security. This reading challenges the treaty's legitimacy itself, potentially undermining the League's authority. Observers of the constraint include international law scholars, historians, and legal authorities who assess whether a treaty imposed under military duress is binding.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, league_of_nations, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(versailles_reparations_clauses__repudiation_reading, allied_creditor_powers).
narrative_ontology:fixing_cost_class(versailles_reparations_clauses__repudiation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The reparations clauses attempt to coordinate war-cost allocation among the Allied powers and compensate them for damages through a binding financial obligation imposed on the defeated power. Under this reading, this 'coordination' is one-directional coercion: the victors set terms without genuine negotiation, and the losers comply under threat.
% TRANSFER_FUNCTION: Moves financial assets, raw materials, and industrial goods from Germany to Allied creditor powers (France, Britain, Belgium, etc.) via the reparations schedule and direct occupation. The reading asserts this transfer is extracted under duress, not legitimately owed.
% ABSENT_VOICES: German population (disenfranchised from treaty negotiation), neutral powers and small nations (excluded from peace conference), German domestic creditors (wages, pensions subordinated to reparations), and future German generations (bound by obligations they never approved) have no seat in the reparations negotiation. They would dispute the legitimacy of the obligation if heard.
% DISAPPEARANCE_RATIONALE: If the reparations obligation were repudiated and nullified, Germany would redirect resources from Allied payments to domestic economic recovery, military rearmament would face no financial constraint from war debts, the Allied powers would lose a major revenue stream, and the geopolitical equilibrium would shift sharply. Versailles' reparations architecture would collapse, forcing a new settlement.
% FOUNDING_PROBLEM: How should the costs of total war be allocated among victors and the defeated power? The reparations clauses claim to solve this through binding financial liability. This reading rejects the framing: the 'problem' is not allocation, but the use of military defeat to impose terms one party never consented to.
% FOUNDING_PROBLEM_CORROBORATION: German legal scholars, Weimar-era jurists, and international law experts outside the Allied creditor powers argue the treaty was imposed under duress and thus lacks binding force. Allied powers and war-damage claimants assert the reparations are legitimate compensation for war costs. Historians (e.g., Margaret MacMillan on treaty negotiation dynamics) document the coercive conditions under which Germany signed. No neutral or external authority ratified the legitimacy of the terms; it was imposed by military force.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__repudiation_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__repudiation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__repudiation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(versailles_reparations_clauses__repudiation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__repudiation_reading, 0.92, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is very high (0.92) because the reading asserts the entire reparations obligation is extraction without legitimate foundation — the Allied powers extract maximum value (financial, resources, strategic) from a defeated enemy without genuine consent. Suppression is high (0.88) because enforcement depends on military occupation and the threat of resumed war; without active suppression (troop presence, economic blockade threat), Germany would repudiate. Theater ratio rises from 0.40 to 0.58 over the interval: initial enforcement is primarily military force; by the later period, the justification narrative (war guilt, legitimate compensation) becomes more prominent while actual collection mechanisms (reparations commissions, bond trading) create performative legitimacy around what is fundamentally extraction. Accessibility collapse is moderate (0.67) because Germany has a clear alternative (repudiate, rearm, prepare for war) but the cost is existential — military retaliation by the occupying powers. The measurement series tracks the constraint's hardening: extractiveness plateaus at 0.92 once the payment schedule is fully implemented; theater rises as justification infrastructure (reparations committees, economic arguments) replaces direct military enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (German state, German population) experience this as duress-imposed extraction; they compute this constraint as a snare, with no legitimate foundation. The beneficiary seat (Allied powers) experiences this as legitimate creditor claims and war-cost recovery; they compute it as rope or tangled-rope (coordination of war-cost allocation plus enforcement). The League of Nations observer seat tries to assess legitimacy on principled grounds but faces a fundamental dispute: was the treaty negotiated under duress conditions that void its binding force? This is the core perspectival gap — not observational difference but disagreement on whether the constraint is legitimate at all.
 *
 * DIRECTIONALITY LOGIC:
 *   From Germany's seat: full target (d near 1.0). Germany is forced to pay, has no exit, and bears the cost. From the Allied creditor powers' seat: full beneficiary (d near 0.0). They set terms, enforce collection, and accrue all gains. From the German population's seat: full target (d near 1.0) — they pay through taxation and bear austerity without ever consenting to the obligation. The repudiation reading intensifies the target directionality for all German stakeholders by asserting the obligation itself is illegitimate coercion, not a debt.
 *
 * MANDATROPHY ANALYSIS:
 *   The repudiation reading asserts mandatrophy at inception: the reparations clauses were born with a dead mandate (consent-without-coercion) because the signature was extracted under military duress. The 'founding problem' — allocation of war costs — was framed by the victors without genuine German participation. As time passes, theater rises (justification narratives accumulate) but the mandate does not resurrect; it stays dead. The constraint persists because military force enforces it, not because the underlying obligation is accepted as legitimate. By the 1930s (beyond the interval), Germany's explicit repudiation of the treaty and rearmament make the mandatrophy explicit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    duress_threshold_and_treaty_binding,
    'At what point does military coercion in treaty negotiation void the treaty''s binding force? Did Germany''s situation in June 1919 meet that threshold?',
    'International law doctrine on duress as a ground for treaty invalidity (Vienna Convention Article 52, which codifies duress doctrine in 1969); historical analysis of Germany''s alternatives in June 1919; counterfactual: what could Germany realistically have done other than sign?',
    'If duress is established as the threshold for voidance, and Germany''s 1919 position met it, the entire treaty''s legitimacy is undermined and reparations become illegitimate extraction. If duress is narrowly construed (only outright ''do this or we invade tomorrow''), the treaty''s binding force stands and reparations remain legitimate claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(duress_threshold_and_treaty_binding, conceptual, 'Whether treaty-negotiation conditions met the duress standard for voidance.').

omega_variable(
    war_responsibility_and_cost_allocation,
    'Even if the treaty was negotiated under duress, does Germany bear some legitimate share of war costs, or is all responsibility negated by duress?',
    'Moral philosophy of responsibility under coercion; historical counterfactual: what would a consent-based war-cost settlement have looked like? Comparison with other post-war settlements (e.g., post-WWI Austria-Hungary, post-WWII Germany, post-WWII Japan).',
    'Complete negation of responsibility supports the repudiation reading fully. Acknowledgment of some legitimate German share (even under duress) would move toward the limited_responsibility reading, where reparations are bounded by economic capacity rather than wholly void.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(war_responsibility_and_cost_allocation, preference, 'Whether duress-imposed terms can carry any legitimate obligation or negate all responsibility.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.88) primarily structural (military occupation, blockade threat) or internalized (Germany''s acceptance of war guilt narrative, identification with legal obligation)?',
    'Discourse analysis of German political rhetoric 1919-1933; survey of whether German political factions rejected the treaty in principle (structural dissent) or internalized guilt (internalized suppression). Post-treaty repudiation actions (rearmament, treaty withdrawal) reveal whether suppression persisted after structural mechanisms were weakened.',
    'If suppression is primarily structural, removing the occupation and military threat would dissolve it; Germany''s later treaty repudiation supports this. If significantly internalized, the constraint''s suppression persists psychologically even after structural enforcement weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of reparations obligation is structural (military force) or internalized (accepted guilt).').

omega_variable(
    reading_contest_ground_of_disagreement,
    'Do the three readings of the Versailles kernel disagree primarily about FACTS (Did duress occur? What were the alternatives?), about VALUES (Is responsibility collective or individual? Does military defeat impose payment obligation?), or about INTERPRETATION (What does Article 231 actually say, and what does it bind Germany to)?',
    'Analyze primary sources from each reading''s proponents (German jurists claiming duress, French war-damage claimants, international law scholars); distinguish factual disputes (resolvable by historical evidence) from normative disputes (matters of principle) from interpretive disputes (textual analysis of the treaty).',
    'If disagreement is primarily factual, empirical evidence can resolve which reading is correct. If primarily normative, no amount of evidence resolves it — the readings are irreconcilable commitments. If primarily interpretive, legal scholarship and hermeneutics become the battleground, not history or ethics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_ground_of_disagreement, conceptual, 'What kind of disagreement separates the three readings of the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__repudiation_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t0, versailles_reparations_clauses__repudiation_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement_basis(vers_tr_t0, observed).
narrative_ontology:measurement(vers_tr_t3, versailles_reparations_clauses__repudiation_reading, theater_ratio, 3, 0.45).
narrative_ontology:measurement_basis(vers_tr_t3, observed).
narrative_ontology:measurement(vers_tr_t6, versailles_reparations_clauses__repudiation_reading, theater_ratio, 6, 0.5).
narrative_ontology:measurement_basis(vers_tr_t6, observed).
narrative_ontology:measurement(vers_tr_t12, versailles_reparations_clauses__repudiation_reading, theater_ratio, 12, 0.55).
narrative_ontology:measurement_basis(vers_tr_t12, observed).
narrative_ontology:measurement(vers_tr_t18, versailles_reparations_clauses__repudiation_reading, theater_ratio, 18, 0.58).
narrative_ontology:measurement_basis(vers_tr_t18, observed).
narrative_ontology:measurement(vers_tr_t25, versailles_reparations_clauses__repudiation_reading, theater_ratio, 25, 0.58).
narrative_ontology:measurement_basis(vers_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(vers_be_t0, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement_basis(vers_be_t0, observed).
narrative_ontology:measurement(vers_be_t3, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 3, 0.87).
narrative_ontology:measurement_basis(vers_be_t3, observed).
narrative_ontology:measurement(vers_be_t6, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 6, 0.89).
narrative_ontology:measurement_basis(vers_be_t6, observed).
narrative_ontology:measurement(vers_be_t12, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 12, 0.91).
narrative_ontology:measurement_basis(vers_be_t12, observed).
narrative_ontology:measurement(vers_be_t18, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 18, 0.92).
narrative_ontology:measurement_basis(vers_be_t18, observed).
narrative_ontology:measurement(vers_be_t25, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 25, 0.92).
narrative_ontology:measurement_basis(vers_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t0, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 0, 0.82).
narrative_ontology:measurement_basis(vers_su_t0, observed).
narrative_ontology:measurement(vers_su_t3, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 3, 0.84).
narrative_ontology:measurement_basis(vers_su_t3, observed).
narrative_ontology:measurement(vers_su_t6, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 6, 0.86).
narrative_ontology:measurement_basis(vers_su_t6, observed).
narrative_ontology:measurement(vers_su_t12, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 12, 0.87).
narrative_ontology:measurement_basis(vers_su_t12, observed).
narrative_ontology:measurement(vers_su_t18, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 18, 0.88).
narrative_ontology:measurement_basis(vers_su_t18, observed).
narrative_ontology:measurement(vers_su_t25, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 25, 0.88).
narrative_ontology:measurement_basis(vers_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__repudiation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(versailles_reparations_clauses__repudiation_reading, 0.18).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses__punitive_liability_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses__limited_responsibility_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, german_rearmament_as_strategic_response).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, allied_occupation_enforcement_apparatus).

% DUAL FORMULATION NOTE:
% The Treaty of Versailles reparations clauses form a constraint family with three structurally distinct readings: (1) PUNITIVE_LIABILITY_READING asserts quasi-unlimited German obligation and high justification. (2) LIMITED_RESPONSIBILITY_READING accepts obligation but bounds it by economic capacity. (3) REPUDIATION_READING (this story) asserts the obligation itself is void due to duress. Each reading has a different ε, different beneficiary/victim structure, and different terminal classification. The three readings are linked via network.affects_constraints because each reading's legitimacy depends partly on delegitimizing or constraining the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(versailles_reparations_clauses__repudiation_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
