% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__punitive_liability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_versailles_reparations_clauses__punitive_liability_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: versailles_reparations_clauses__punitive_liability_reading
 *   human_readable: Versailles Article 231 Punitive Liability Reading: German War Guilt and Reparations Obligation
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   The Treaty of Versailles (1919) embedded Article 231, the 'war guilt
 *   clause,' which grounds unlimited German reparations obligations in a
 *   declaration that Germany bears unique moral and financial responsibility
 *   for total war costs. This constraint story instantiates ONE reading of
 *   the contested treaty kernel: the PUNITIVE LIABILITY READING, which treats
 *   Article 231 as a legitimate assertion of culpability and grounds
 *   quasi-unlimited creditor claims on that culpability. The constraint
 *   subordinates German fiscal sovereignty to external Allied claims,
 *   benefits Allied creditor states and the Reparations Commission, and
 *   imposes costs on German workers and taxpayers who did not authorize the
 *   war. The measurement series (years 0–15, representing 1919–1934) track
 *   extractiveness rising and stabilizing, theater rising and plateauing (as
 *   enforcement becomes routine), and suppression requirement rising and
 *   holding steady (indicating the constraint requires active policing of
 *   German fiscal alternatives). Alternative readings
 *   (limited_responsibility_reading, repudiation_reading) are structurally
 *   distinct constraints with different ε values and different
 *   beneficiary/victim sets; they are NOT included here.
 *
 * KEY AGENTS:
 *   - allied_creditor_states (France, Britain, USA): institutional power, near-absolute exit (can restructure or forgive at will), beneficiary and agenda-setter
 *   - german_workers_and_taxpayers: powerless, trapped exit (no escape from taxation/inflation), victims of the extraction
 *   - weimar_republic (German government): moderate power, constrained exit (can negotiate schedule but not repudiate), payer and secondary agenda-setter
 *   - war_reparations_commission: institutional power, analytical exit, agenda-setter and administrator
 *   - excluded German elites: powerful but structurally excluded from reparations discourse; responsible for war decisions but shielded from direct personal cost
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__punitive_liability_reading, 0.82).
domain_priors:suppression_score(versailles_reparations_clauses__punitive_liability_reading, 0.79).
domain_priors:theater_ratio(versailles_reparations_clauses__punitive_liability_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__punitive_liability_reading, tangled_rope).
narrative_ontology:human_readable(versailles_reparations_clauses__punitive_liability_reading, "Versailles Article 231 Punitive Liability Reading: German War Guilt and Reparations Obligation").
narrative_ontology:topic_domain(versailles_reparations_clauses__punitive_liability_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__punitive_liability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__punitive_liability_reading, '809d1ac5-4dc3-4202-9cbd-95352d01c126').
narrative_ontology:cs_kernel_codification('809d1ac5-4dc3-4202-9cbd-95352d01c126', formalized).
narrative_ontology:cs_authority_grounding('809d1ac5-4dc3-4202-9cbd-95352d01c126', extraction).
narrative_ontology:cs_interpretation_layer_present('809d1ac5-4dc3-4202-9cbd-95352d01c126').
narrative_ontology:cs_reading_relation('809d1ac5-4dc3-4202-9cbd-95352d01c126', versailles_reparations_clauses__limited_responsibility_reading, influences).
narrative_ontology:cs_reading_relation('809d1ac5-4dc3-4202-9cbd-95352d01c126', versailles_reparations_clauses__repudiation_reading, coexists_with).
narrative_ontology:cs_axiom('809d1ac5-4dc3-4202-9cbd-95352d01c126', foundational, german_unique_war_guilt).
narrative_ontology:cs_axiom_status(german_unique_war_guilt, holdable).
narrative_ontology:cs_axiom_grounding('809d1ac5-4dc3-4202-9cbd-95352d01c126', german_unique_war_guilt, empirically_contingent).
narrative_ontology:cs_axiom('809d1ac5-4dc3-4202-9cbd-95352d01c126', foundational, perpetual_creditor_claim_validity).
narrative_ontology:cs_axiom_status(perpetual_creditor_claim_validity, holdable).
narrative_ontology:cs_axiom_grounding('809d1ac5-4dc3-4202-9cbd-95352d01c126', perpetual_creditor_claim_validity, deontological).
narrative_ontology:cs_reference_frame('809d1ac5-4dc3-4202-9cbd-95352d01c126', perpetual_german_liability_framework).
narrative_ontology:cs_drift_state('809d1ac5-4dc3-4202-9cbd-95352d01c126', contemporary_mid_1920s, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('809d1ac5-4dc3-4202-9cbd-95352d01c126', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__punitive_liability_reading, war_reparations_commission).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_workers_and_taxpayers).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_fiscal_sovereignty).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_government_weimar_republic).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__punitive_liability_reading, perpetual_war_guilt_doctrine).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__punitive_liability_reading, supremacy_of_creditor_claims_over_sovereign_capacity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% France, Britain, and the United States set the reparations agenda through the Reparations Commission and hold primary authority to adjust rates and schedules. They collect reparations payments directly and justify the amounts as compensation for war damage and assertion of German moral culpability. Their ability to exit is near-absolute: they can forgive, restructure, or enforce at will.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states, beneficiary).

% Bear the direct fiscal burden through taxation and inflation required to service reparations payments. They did not authorize the war, do not control the government's negotiating position, and carry generational debt obligations for costs incurred by military and political elites. Exit consists only of emigration (available to very few) or internal political revolution.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_workers_and_taxpayers, payer,
    powerless, biographical, trapped, national).

% Must extract and remit reparations payments while maintaining minimal domestic fiscal capacity. Claims formal treaty obligation but faces domestic delegitimation for accepting the 'war guilt' clause. Can negotiate schedule restructuring with the Reparations Commission but cannot unilaterally reduce principal or repudiate.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_government_weimar_republic, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__punitive_liability_reading, german_government_weimar_republic, agenda_setter).

% Administers the reparations regime, sets payment schedules, audits German financial capacity, and enforces compliance through occupation and economic sanctions. Acts as the executive body translating Article 231 into concrete obligations. Composed of and accountable primarily to the allied creditor states.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, war_reparations_commission, agenda_setter,
    institutional, generational, analytical, continental).

% Made strategic decisions leading to total war but are largely insulated from direct reparations costs through the state mechanism. Would dispute the war guilt clause if permitted voice in negotiations; instead, the cost falls on workers and taxpayers. Their exclusion from the reparations discourse is structurally necessary to the punitive reading.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_industrial_and_military_elites, excluded,
    powerful, biographical, constrained, national).

% Argue that Article 231 violates principles of sovereign equality and proportionality embedded in centuries of European legal tradition. Would challenge the treaty's legitimacy on grounds of duress and imposed terms, but are excluded from the reparations commission discourse and have no direct leverage over enforcement.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_legal_traditionalists, excluded,
    moderate, generational, constrained, national).

% Economic historians, legal scholars, and nonaligned states assess whether the punitive reading reflects genuine war guilt or is a constructed narrative justifying creditor extraction. This seat has no contemporaneous power to alter the constraint but documents its evolution.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, neutral_and_nonbelligerent_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states).
narrative_ontology:fixing_cost_class(versailles_reparations_clauses__punitive_liability_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified claims-resolution mechanism through the Reparations Commission so that multiple allied creditors present consolidated demands and Germany faces one enforcement body rather than bilateral negotiations with each claimant state. The commission also coordinates among creditors on how to allocate received payments.
% TRANSFER_FUNCTION: Moves approximately 132 billion gold marks (later revised to 50 billion under the Dawes Plan) from German fiscal authority to allied creditor states, nominally in compensation for war damage but operationally structured as quasi-unlimited liability grounded in the perpetual war guilt assertion of Article 231.
% ABSENT_VOICES: German workers and taxpayers are excluded from the treaty negotiation process; German legal and military elites who shaped war strategy are shielded from direct personal liability; the Central Powers' other participants (Austria-Hungary, Ottoman Empire) face different treaty structures and are not part of this specific reparations architecture. Alternative framings of German responsibility (proportional capacity-based, time-limited, context-acknowledging mutual culpability) are structurally excluded from the reparations commission discourse.
% DISAPPEARANCE_RATIONALE: If Article 231 and the reparations obligation disappeared, German fiscal capacity would redirect toward reconstruction and debt retirement; allied states would lose a primary revenue stream and source of German political subordination; the Weimar Republic would face immediate pressure to legitimize itself domestically rather than accept external blame; the entire postwar European balance-of-power arrangement and creditor-debtor hierarchy would reorganize.
% FOUNDING_PROBLEM: World War I produced massive destruction and death; allied creditor states incurred enormous war debts and domestic fiscal strain; a mechanism was needed to assign costs and recover resources from the defeated powers.
% FOUNDING_PROBLEM_CORROBORATION: Allied creditor governments attested in 1919 that the founding problem justified the reparations structure. By 1924 (interval year 5), economic historians (John Maynard Keynes in The Economic Consequences of the Peace) and international economists attested that war-damage compensation was legitimate but the scale and perpetual-liability framing of Article 231 far exceeded proportional cost-recovery. By 1930 (interval year 11), German economists, legal traditionalists, and even some Allied policymakers (acknowledging in Dawes and Young Plan renegotiations) attested that the founding problem of war-damage recovery had been substantially satisfied, but the reparations constraint persisted in pursuit of ongoing German subordination.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__punitive_liability_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__punitive_liability_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__punitive_liability_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(versailles_reparations_clauses__punitive_liability_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(versailles_reparations_clauses__punitive_liability_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(versailles_reparations_clauses__punitive_liability_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(versailles_reparations_clauses__punitive_liability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness reaches 0.82 because the reparations obligation is perpetual (no fixed endpoint under the punitive reading), nominally quasi-unlimited (Article 231 grounds claims to cover 'all costs of war'), and decoupled from German capacity to pay — the creditors can adjust schedules but not the principal assertion of liability. Suppression is high (0.79) because German alternatives are actively closed: the treaty is imposed under military occupation; German legal traditions asserting proportional capacity-based limits are excluded from the reparations commission; repudiation or unilateral renegotiation are treated as treaty violation subject to occupation/sanctions. Theater rises from 0.28 to 0.41 because as time passes and the immediate war trauma recedes, the constraint's operative function shifts from compensation toward enforcing German political subordination and extracting sustained fiscal transfers — the reparations machinery becomes increasingly ritualistic rather than restorative. Accessibility collapse is moderate-high (0.68) because Germany faces occupation, creditor-controlled commission authority, and the threat of military enforcement, but alternative narratives (capacity-based limits, time-bounded settlements, repudiation on duress grounds) remain live in German legal and political discourse and occasionally surface in renegotiation attempts (Dawes Plan, Young Plan). Resistance is high (0.74) because German political factions continuously contest the war guilt clause, the Weimar Republic's legitimacy is damaged by accepting it, and periodic renegotiations reflect real pushback against the constraint.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (Allied creditor states) experiences this as legitimate creditor protection and war-guilt enforcement. The payer seats (German workers, Weimar government) experience it as occupation-backed extraction and perpetual political subordination. The excluded seat (German military/industrial elites) would experience it as unfair externalization of elite decisions onto the population, but they are excluded from the reparations discourse. The observer seat (historians, economists outside the benefiting parties) documents how the punitive reading shifted from post-war compensation toward a vehicle for maintaining German economic and political dependency.
 *
 * DIRECTIONALITY LOGIC:
 *   Allied creditor states are beneficiaries with high institutional power and arbitrage-grade exit (they can forgive, restructure, or escalate enforcement at will) — directionality near 0.0 (full beneficiary), effective extraction inverted to subsidy-adjacent (they are being subsidized by the constraint). German workers/taxpayers are victims with powerless status and trapped exit (no way out of taxation/inflation except emigration or revolution) — directionality near 1.0 (full target), effective extraction amplified by power asymmetry and exit constraint. The Weimar government sits in a peculiar position: they are formally the payer (remitting reparations), but they are partly the agenda-setter (administering the extraction domestically) and face trapped exit (cannot repudiate without military occupation/sanctions). Their directionality is high (~0.75) because they must extract from their own population and bear legitimacy damage for doing so. Excluded German elites have powerful status but constrained exit and are structurally prevented from bearing direct costs — their directionality is anomalous (high d for a powerful actor) because the constraint subordinates power to external claims.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem was legitimate: massive war damage required cost recovery, and a unified claims mechanism prevented bilateral creditor jockeying. However, by the mid-1920s (interval years 6–9), the founding problem had substantially shifted from compensation toward political control: Germany was paying on schedule, war damage was being repaired across Europe, but the reparations architecture persisted and intensified. Theater ratio rises from 0.28 to 0.41, indicating increasing divergence between the stated coordination function (unified claims administration) and the actual operational function (enforcing German subordination and extracting sustained transfers). By year 15, the constraint is almost entirely theater relative to the founding problem — the Reparations Commission continues its enforcement apparatus and schedule adjustments, but the compensation function is secondary to the extraction function. This is a textbook mandatrophy candidate: the founding problem is dead or nearly so (war damage compensation is complete; Europe has stabilized), but the constraint persists and requires active suppression of German alternatives (legal repudiation, unilateral renegotiation, fiscal reallocation) to maintain. The constraint does not meet the Piton criteria (no concentrated beneficiary profit — the reparations are distributed among allied creditors, not captured by a single administrator), so it remains Tangled Rope (genuine founding coordination + asymmetric extraction + active enforcement), but the mandatrophy drift is clear.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    war_guilt_assignment_empirical,
    'Did Germany uniquely initiate World War I, or did the conflict arise from multi-party strategic miscalculation and escalation in which multiple powers share responsibility?',
    'Historical scholarship on the July Crisis, alliance entanglement, and decision-making processes in all major belligerent capitals. The corpus of diplomatic history and contemporaneous correspondence provides the empirical basis.',
    'If Germany''s role was uniquely initiatory, the punitive liability reading has stronger structural grounding; if responsibility was plural, the assertion of unique German guilt becomes a constructed narrative supporting extraction, and the reading shifts toward Snare. The ε value itself does not change (the constraint''s operation is the same), but the legitimacy of that operation is revealed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(war_guilt_assignment_empirical, empirical, 'Empirical question of multi-party war initiation responsibility.').

omega_variable(
    perpetual_vs_time_bounded_liability,
    'Is the moral and financial obligation to bear war costs perpetual, or is it time-bounded and proportional to capacity?',
    'Jurisprudential consensus on principles of sovereign equality, proportionality, and capacity constraints in international law. The lived experience of reparations enforcement (Dawes Plan, Young Plan renegotiations) provides operational evidence of whether time-bounding is treated as legitimate.',
    'If perpetual liability is accepted as legitimate by the broad international legal community, the punitive reading stands as written. If capacity-bounding and time-limitation are recognized as legitimate constraints on reparations, the punitive reading becomes a false summit — it claims to be natural law (perpetual war guilt obligation) but is actually a constructed distribution favoring creditor states. FSM firing would reclassify toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(perpetual_vs_time_bounded_liability, conceptual, 'Whether reparations liability is perpetual or bounded by capacity and time.').

omega_variable(
    duress_invalidation_of_treaty,
    'Does military occupation and the threat of resumed hostilities constitute duress sufficient to void or substantially modify treaty obligations under international law?',
    'International law scholarship on treaties concluded under duress, Vienna Convention on the Law of Treaties provisions, and historical precedent for duress-based treaty modification or repudiation.',
    'If duress is recognized as grounds for invalidation or modification, the entire reparations obligation structure becomes contestable; the repudiation_reading would gain structural legitimacy and the punitive reading would operate as enforced extraction absent genuine contractual consent. The constraint would shift from Tangled Rope (hybrid coordination + extraction) toward Snare (pure extraction masked by legitimacy claims).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(duress_invalidation_of_treaty, conceptual, 'Legality of treaty obligations concluded under military occupation and duress.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.79) primarily structural (external enforcement via occupation, creditor control, sanctions threat) or internalized (German political elites have accepted the war guilt frame and police their own population''s alternatives)?',
    'Comparative analysis of German political discourse and resistance patterns in years of high military occupation versus years of reduced occupation (post-Ruhr withdrawal, 1924 onward); interview/memoir evidence of whether German acceptance of the constraint is coerced externally or internalized.',
    'If suppression is primarily structural, the constraint''s persistence depends on continued occupation and creditor enforcement; if substantially internalized, German acceptance of the war guilt narrative has become self-enforcing even after occupation is reduced, and the constraint''s persistence is more durable. This affects predictions of what happens if external enforcement weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism in the reparations constraint.').

omega_variable(
    kernel_reading_contest_resolution,
    'Which of the three readings (punitive liability, limited responsibility, repudiation) will prevail as the binding interpretation of the Versailles treaty in the 1930s and beyond?',
    'Historical observation of actual reparations enforcement and modification 1924–1933 (Dawes Plan, Young Plan, Nazi repudiation); journal historical scholarship on how the treaty was ultimately treated; international law scholarship on which reading gained legitimacy.',
    'If the punitive reading prevails, the constraint persists as Tangled Rope with high ε. If limited responsibility prevails, the constraint reclassifies to bounded Rope. If repudiation prevails, the constraint dissolves and the reading is superseded. The three readings are structurally independent constraints; which one ''wins'' in historical practice is an empirical open question at the time of authoring (1919–1934).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_resolution, empirical, 'Historical outcome: which reading of the Versailles reparations kernel becomes institutionalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__punitive_liability_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t0, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(vers_tr_t0, observed).
narrative_ontology:measurement(vers_tr_t2, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 2, 0.31).
narrative_ontology:measurement_basis(vers_tr_t2, observed).
narrative_ontology:measurement(vers_tr_t4, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 4, 0.34).
narrative_ontology:measurement_basis(vers_tr_t4, observed).
narrative_ontology:measurement(vers_tr_t6, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 6, 0.37).
narrative_ontology:measurement_basis(vers_tr_t6, observed).
narrative_ontology:measurement(vers_tr_t9, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 9, 0.4).
narrative_ontology:measurement_basis(vers_tr_t9, observed).
narrative_ontology:measurement(vers_tr_t12, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 12, 0.41).
narrative_ontology:measurement_basis(vers_tr_t12, observed).
narrative_ontology:measurement(vers_tr_t15, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 15, 0.41).
narrative_ontology:measurement_basis(vers_tr_t15, observed).

% Extraction over time
narrative_ontology:measurement(vers_be_t0, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 0, 0.76).
narrative_ontology:measurement_basis(vers_be_t0, observed).
narrative_ontology:measurement(vers_be_t2, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 2, 0.78).
narrative_ontology:measurement_basis(vers_be_t2, observed).
narrative_ontology:measurement(vers_be_t4, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 4, 0.8).
narrative_ontology:measurement_basis(vers_be_t4, observed).
narrative_ontology:measurement(vers_be_t6, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 6, 0.81).
narrative_ontology:measurement_basis(vers_be_t6, observed).
narrative_ontology:measurement(vers_be_t9, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 9, 0.82).
narrative_ontology:measurement_basis(vers_be_t9, observed).
narrative_ontology:measurement(vers_be_t12, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 12, 0.82).
narrative_ontology:measurement_basis(vers_be_t12, observed).
narrative_ontology:measurement(vers_be_t15, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 15, 0.82).
narrative_ontology:measurement_basis(vers_be_t15, observed).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t0, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 0, 0.71).
narrative_ontology:measurement_basis(vers_su_t0, observed).
narrative_ontology:measurement(vers_su_t2, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 2, 0.74).
narrative_ontology:measurement_basis(vers_su_t2, observed).
narrative_ontology:measurement(vers_su_t4, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 4, 0.76).
narrative_ontology:measurement_basis(vers_su_t4, observed).
narrative_ontology:measurement(vers_su_t6, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 6, 0.77).
narrative_ontology:measurement_basis(vers_su_t6, observed).
narrative_ontology:measurement(vers_su_t9, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 9, 0.79).
narrative_ontology:measurement_basis(vers_su_t9, observed).
narrative_ontology:measurement(vers_su_t12, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 12, 0.79).
narrative_ontology:measurement_basis(vers_su_t12, observed).
narrative_ontology:measurement(vers_su_t15, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 15, 0.79).
narrative_ontology:measurement_basis(vers_su_t15, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__punitive_liability_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(versailles_reparations_clauses__punitive_liability_reading, 0.12).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses__limited_responsibility_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses__repudiation_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, weimar_republic_political_legitimacy).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, european_postwar_balance_of_power).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, allied_war_debt_servicing_constraint).

% DUAL FORMULATION NOTE:
% This story is one reading of the Versailles reparations clauses kernel. The kernel is the treaty text (Article 231) and the reparations obligation. This reading interprets Article 231 as grounding legitimate perpetual German liability for war costs. Sibling readings (limited_responsibility_reading, repudiation_reading) interpret the same text differently, instantiating different constraints with different ε values and beneficiary/victim structures. All three readings are live positions in 1920s international discourse; which prevails is an empirical open question. The three stories should be read together; network links reflect structural dependencies (punitive reading affects legitimacy conditions for limited reading, both are constrained by the duress question that supports repudiation reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(versailles_reparations_clauses__punitive_liability_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
