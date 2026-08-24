% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__limited_responsibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_versailles_reparations_clauses__limited_responsibility_reading, []).

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
 *   constraint_id: versailles_reparations_clauses__limited_responsibility_reading
 *   human_readable: Versailles Reparations Limited Responsibility Reading
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   The limited_responsibility_reading of the Versailles reparations clauses
 *   asserts that Article 231 (war guilt) is a legal formality establishing
 *   liability for calculation purposes, not a moral judgment justifying
 *   unlimited extraction, and that payment schedules must be bounded by
 *   German economic viability. This reading informed the Dawes Plan (1924),
 *   Young Plan (1929), and the effective termination at Lausanne (1932). It
 *   operates as a constraint on Allied maximalism: the Reparations
 *   Commission's capacity assessments cap the annuity, and the transfer
 *   mechanism (loans + exports) is designed to avoid the 'transfer problem.'
 *   The constraint has genuine coordination function (it prevented total
 *   collapse and kept some payments flowing for a decade) but also asymmetric
 *   extraction: Allied creditor governments and occupied populations bear the
 *   cost of the reduced flow, while German elites gain negotiating leverage.
 *   The claimed_type is tangled_rope — the reading presents it as rope (pure
 *   coordination), but the structural data shows extraction from victims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__limited_responsibility_reading, 0.45).
domain_priors:suppression_score(versailles_reparations_clauses__limited_responsibility_reading, 0.4).
domain_priors:theater_ratio(versailles_reparations_clauses__limited_responsibility_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__limited_responsibility_reading, tangled_rope).
narrative_ontology:human_readable(versailles_reparations_clauses__limited_responsibility_reading, "Versailles Reparations Limited Responsibility Reading").
narrative_ontology:topic_domain(versailles_reparations_clauses__limited_responsibility_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__limited_responsibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__limited_responsibility_reading, 'a477706b-b1b6-4367-9def-e4af427b4d13').
narrative_ontology:cs_kernel_codification('a477706b-b1b6-4367-9def-e4af427b4d13', formalized).
narrative_ontology:cs_authority_grounding('a477706b-b1b6-4367-9def-e4af427b4d13', lineage).
narrative_ontology:cs_interpretation_layer_present('a477706b-b1b6-4367-9def-e4af427b4d13').
narrative_ontology:cs_reading_relation('a477706b-b1b6-4367-9def-e4af427b4d13', versailles_reparations_clauses__punitive_liability_reading, coexists_with).
narrative_ontology:cs_reading_relation('a477706b-b1b6-4367-9def-e4af427b4d13', versailles_reparations_clauses__repudiation_reading, influences).
narrative_ontology:cs_axiom('a477706b-b1b6-4367-9def-e4af427b4d13', foundational, reparations_bounded_by_debtor_capacity).
narrative_ontology:cs_axiom_status(reparations_bounded_by_debtor_capacity, holdable).
narrative_ontology:cs_axiom_grounding('a477706b-b1b6-4367-9def-e4af427b4d13', reparations_bounded_by_debtor_capacity, empirically_contingent).
narrative_ontology:cs_axiom('a477706b-b1b6-4367-9def-e4af427b4d13', foundational, war_guilt_clause_legal_not_moral).
narrative_ontology:cs_axiom_status(war_guilt_clause_legal_not_moral, holdable).
narrative_ontology:cs_axiom_grounding('a477706b-b1b6-4367-9def-e4af427b4d13', war_guilt_clause_legal_not_moral, conventional).
narrative_ontology:cs_reference_frame('a477706b-b1b6-4367-9def-e4af427b4d13', versailles_treaty_framework).
narrative_ontology:cs_drift_state('a477706b-b1b6-4367-9def-e4af427b4d13', dawes_plan_1924, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a477706b-b1b6-4367-9def-e4af427b4d13', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, german_government).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, german_industrial_elites).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, german_civilian_population).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, allied_creditor_governments).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, occupied_territories_civilians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, german_civilian_population).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__limited_responsibility_reading, transfer_problem_analysis).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__limited_responsibility_reading, sustainable_reparations_principle).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__limited_responsibility_reading, war_guilt_clause_legal_formality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiates payment schedules with Allied creditors through the Reparations Commission and subsequent conferences (London, Dawes, Young, Lausanne). Uses the 'capacity to pay' argument to gain leverage and secure downward revisions. Administers the domestic fiscal burden of payments while seeking to avoid economic collapse or political destabilization.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_government, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__limited_responsibility_reading, german_government, beneficiary).

% Major industrial concerns (Krupp, IG Farben, Siemens, etc.) benefit from the viability-bounded framework because it prevents total asset seizure and allows continued operation. They lobby for the 'capacity' interpretation and gain negotiating leverage vis-a-vis Allied creditors. Some conceal assets or shift production to evade the full burden while the principle limits the ceiling.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_industrial_elites, beneficiary,
    powerful, biographical, mobile, national).

% Bears the domestic tax burden and inflationary costs of whatever payments are made. Benefits from the viability bound insofar as it prevents hyperinflationary collapse (1923) and total economic ruin, but still experiences severe immiseration. No meaningful exit; emigration is the only escape and is constrained by destination-country barriers.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_civilian_population, payer,
    moderate, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__limited_responsibility_reading, german_civilian_population, beneficiary).

% France, UK, Belgium, Italy and other creditor states receive reduced reparations flows because the viability bound caps German obligations. They bear the fiscal cost of the constraint: war debts to the US remain unpaid, domestic reconstruction is underfunded, and political legitimacy erodes. They can arbitrage by linking reparations to war debt negotiations (Dawes/Young Plans) or by occupying the Ruhr (1923) to force compliance, but the viability principle structurally limits their take.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, allied_creditor_governments, payer,
    institutional, generational, arbitrage, global).

% Populations in Alsace-Lorraine, the Ruhr (during 1923-25 occupation), and other occupied zones bear the direct costs of enforcement (requisitions, curfews, displacement) and the indirect costs of reduced reparations (underfunded reconstruction, pension shortfalls). They have no voice in the Reparations Commission and no exit from the territory.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, occupied_territories_civilians, payer,
    powerless, biographical, trapped, local).

% The inter-Allied body (with US associate membership) that administers the treaty's reparations clauses. It interprets 'capacity to pay,' sets annual annuities, supervises the German budget, and authorizes sanctions (Ruhr occupation). It is the primary enforcement machinery for the viability-bounded constraint. Its decisions reflect Allied political compromises more than technical economics.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, reparations_commission, agenda_setter,
    institutional, generational, analytical, global).

% Private bankers (J.P. Morgan, Schacht, Norman, etc.) and the Dawes/Young committees design the loan-and-payment structures that operationalize the viability bound. They profit from underwriting German loans and managing the transfer mechanism. Their analyses (the Dawes Report, Young Report) supply the 'expert' legitimacy for the capacity assessments. They are observers of the constraint's operation but also shape its enforcement through financial engineering.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, international_bankers_dawes_young, observer,
    organized, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__limited_responsibility_reading, international_bankers_dawes_young, agenda_setter).

% DNVP, NSDAP, and other völkish parties reject the entire reparations obligation (repudiation reading). They are excluded from the official negotiation process but exert pressure through street violence, parliamentary obstruction, and the 1922 Rathenau assassination. Their exclusion is structural: the viability-bound framework requires a German government willing to pay something, so total rejection is incompatible with the constraint's operation.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_radical_nationalists, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a sustainable payment framework that prevents German economic collapse (which would yield zero reparations) while guaranteeing a predictable, non-zero flow to Allied creditors. Solves the 'transfer problem' (Keynes) by linking annuities to capacity assessments rather to fixed war-cost totals.
% TRANSFER_FUNCTION: Moves a viability-bounded stream of reparations from the German economy (via taxes, exports, and foreign loans) to Allied creditor governments, with the Reparations Commission and private bankers as intermediaries. The bound reduces the gross transfer relative to Allied maximalist claims but secures its continuity.
% ABSENT_VOICES: French and Belgian civilian populations in reconstructed zones who expected full German compensation for war damage; German radical nationalists who deny any obligation; US Congress which refused to ratify Versailles but whose war-debt claims shape the whole structure. The first two are excluded by the viability framework's logic; the third operates outside it.
% DISAPPEARANCE_RATIONALE: Without the viability bound, either (a) German economy collapses into hyperinflation and political chaos (1923 precedent), ending reparations entirely, or (b) Allies enforce maximalist claims through permanent occupation and asset seizure, restructuring European political order. The constraint's disappearance forces a binary: collapse or coercion.
% FOUNDING_PROBLEM: How to extract meaningful reparations from a defeated great power without destroying its capacity to pay — the 'transfer problem' identified by Keynes (1919) and the Reparations Commission's own technical staff. The 1921 London Schedule of Payments (132 billion gold marks) was the initial maximalist answer; the viability principle emerged as the practical correction.
% FOUNDING_PROBLEM_CORROBORATION: Keynes (The Economic Consequences of the Peace, 1919) attested the problem was real and the London Schedule impossible — he was outside the beneficiary set (British Treasury, not German). The Dawes Committee (1924) and Young Committee (1929), composed of international bankers and technicians not directly benefiting from German payments, corroborated the capacity-binding approach. French officials (Poincaré, Caillaux) contested it throughout, arguing the problem was German unwillingness, not inability.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__limited_responsibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__limited_responsibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__limited_responsibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(versailles_reparations_clauses__limited_responsibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__limited_responsibility_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(versailles_reparations_clauses__limited_responsibility_reading_tests).
:- end_tests(versailles_reparations_clauses__limited_responsibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts high (0.55 at 1919 London Schedule) because the initial maximalist claim is the standing arrangement under contest; the limited responsibility constraint then reduces it stepwise (Dawes 0.4, Young 0.35, Lausanne ~0.2). The reading assesses ε from its own lights: the constraint (viability bound) extracts from Allied creditors by limiting their take. Suppression peaks in 1921-23 (Ruhr occupation, sanctions) then declines as the viability framework institutionalizes. Theater rises late (1930-32) as the Young Plan's performative 'final settlement' masks the reality that the Great Depression has made the constraint moot — payments stop not because viability is respected but because the world economy collapses. Accessibility_collapse at 0.6 reflects that full repudiation (exit) triggers occupation and collapse, while full payment triggers ruin; the middle path is narrow. Resistance at 0.5 captures German passive resistance (1923), Allied creditor resistance to reductions, and nationalist opposition to any payment.
 *
 * PERSPECTIVAL GAP:
 *   From the German government seat, the constraint is a hard-won coordination achievement (rope-like). From the Allied creditor seat, it is an extraction-limiting constraint they resist (snare-like from their perspective — they are the victims of reduced flow). From the occupied civilian seat, it is a constraint that denies them reconstruction resources (snare). The engine computes this seat divergence from the structural data; the claimed_type (tangled_rope) reflects the reading's own structural assessment that both coordination and asymmetric extraction are present.
 *
 * DIRECTIONALITY LOGIC:
 *   German government is agenda_setter/beneficiary: it negotiates the bound and gains leverage (d low). German industrial elites are beneficiaries with mobile exit (d very low). German civilians are payers trapped in the system (d high). Allied creditor governments are payers — they bear the cost of the constraint limiting their claims — but with arbitrage-grade exit via war-debt linkage and occupation (d moderate). Occupied civilians are powerless payers trapped (d=1.0). Reparations Commission is agenda_setter with analytical exit (d=0.5). International bankers are observer/agenda_setter with arbitrage exit (d low). Radical nationalists are excluded — their repudiation reading is structurally incompatible with the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (transfer problem) remains contested — Keynes said capacity was near zero; French economists said German unwillingness was the issue. The constraint persists because neither side can force its preferred resolution: Germany cannot repudiate without rupture; Allies cannot collect maximalist sums without destroying the payer. The mandate (reparations) has not atrophied — it was terminated by exogenous crisis (Depression + Hitler), not by internal resolution. Mandatrophy is not resolved; the constraint family dissolved with the kernel's collapse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the limited_responsibility_reading a genuine coordination principle (solving the transfer problem) or a German elite strategy to minimize payments while preserving sovereignty?',
    'Compare the reading''s capacity assessments (Dawes, Young) against independent economic analyses of German potential output, export capacity, and fiscal space in each period. If assessments systematically understate capacity, the reading functions as strategic minimization.',
    'If strategic minimization, the constraint''s claimed coordination function is cover for extraction from Allied creditors; classification shifts toward snare from the Allied seat. If genuine, the tangled_rope classification holds: real coordination with asymmetric distribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, empirical, 'Whether the viability bound reflects economic reality or negotiated minimization.').

omega_variable(
    viability_measurement_contest,
    'What constitutes ''German economic capacity'' — the reading''s technical assessments or the political bargaining outcomes that produced them?',
    'Analyze the Dawes and Young Committee deliberations: were capacity figures derived from macroeconomic models (Keynesian transfer analysis) or from political compromise between creditor and debtor representatives?',
    'If political compromise, the constraint''s ε is endogenous to the bargaining power of the seats, not an exogenous coordination parameter. The engine''s ε-invariance principle would then require decomposing the constraint into a bargaining process and a technical rule.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(viability_measurement_contest, conceptual, 'Whether ''capacity'' is a technical fact or a political settlement.').

omega_variable(
    article_231_interpretation,
    'Does treating Article 231 as ''legal formality not moral judgment'' stabilize the reparations system or delegitimize it in Allied publics, undermining enforcement?',
    'Track Allied public opinion and parliamentary debates (French Chamber, British Commons) on the ''war guilt'' clause from 1919-1932. Correlate shifts in moral framing with willingness to enforce sanctions (e.g., Ruhr occupation support).',
    'If the legal-formality reading erodes Allied enforcement legitimacy, the constraint''s suppression_requirement rises endogenously — the reading''s own framing weakens its enforcement. This feedback loop is not captured in static ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_231_interpretation, preference, 'Feedback between the reading''s moral framing and its enforcement viability.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (Ruhr occupation, sanctions, budget supervision) structural (Allied military/financial power) or internalized (German acceptance of the treaty framework as legitimate)?',
    'Post-exit suppression trajectory: after 1932 (effective end of reparations), did German governments continue to accept the treaty''s legitimacy, or did suppression collapse with the enforcement machinery? Compare with the 1923 passive resistance — was it structural coercion or internalized obligation that ended it?',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — German elites carried the treaty''s legitimacy as a cognitive constraint even when enforcement lapsed. This would amplify χ for the German civilian seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the reparations regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__limited_responsibility_reading, 0, 13).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(versailles_reparations_clauses__limited_responsibility_reading_tr_t0, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(versailles_reparations_clauses__limited_responsibility_reading_tr_t2, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 2, 0.35).
narrative_ontology:measurement(versailles_reparations_clauses__limited_responsibility_reading_tr_t4, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 4, 0.3).
narrative_ontology:measurement(versailles_reparations_clauses__limited_responsibility_reading_tr_t6, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement(versailles_reparations_clauses__limited_responsibility_reading_tr_t8, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(versailles_reparations_clauses__limited_responsibility_reading_tr_t10, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(versailles_reparations_clauses__limited_responsibility_reading_tr_t12, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement(versailles_reparations_clauses__limited_responsibility_reading_tr_t13, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 13, 0.5).

% Extraction over time
narrative_ontology:measurement(versailles_reparations_clauses__limited_responsibility_reading_be_t0, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(versailles_reparations_clauses__limited_responsibility_reading_be_t2, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 2, 0.6).
narrative_ontology:measurement(versailles_reparations_clauses__limited_responsibility_reading_be_t4, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(versailles_reparations_clauses__limited_responsibility_reading_be_t6, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 6, 0.38).
narrative_ontology:measurement(versailles_reparations_clauses__limited_responsibility_reading_be_t8, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(versailles_reparations_clauses__limited_responsibility_reading_be_t10, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(versailles_reparations_clauses__limited_responsibility_reading_be_t12, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 12, 0.25).
narrative_ontology:measurement(versailles_reparations_clauses__limited_responsibility_reading_be_t13, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 13, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(versailles_reparations_clauses__limited_responsibility_reading_su_t0, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(versailles_reparations_clauses__limited_responsibility_reading_su_t2, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 2, 0.65).
narrative_ontology:measurement(versailles_reparations_clauses__limited_responsibility_reading_su_t4, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 4, 0.45).
narrative_ontology:measurement(versailles_reparations_clauses__limited_responsibility_reading_su_t6, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 6, 0.35).
narrative_ontology:measurement(versailles_reparations_clauses__limited_responsibility_reading_su_t8, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 8, 0.3).
narrative_ontology:measurement(versailles_reparations_clauses__limited_responsibility_reading_su_t10, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(versailles_reparations_clauses__limited_responsibility_reading_su_t12, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 12, 0.4).
narrative_ontology:measurement(versailles_reparations_clauses__limited_responsibility_reading_su_t13, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 13, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__limited_responsibility_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(versailles_reparations_clauses__limited_responsibility_reading, 0.12).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses__punitive_liability_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses__repudiation_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, dawes_plan_loan_structure).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, young_plan_loan_structure).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, weimar_fiscal_constraint).

% DUAL FORMULATION NOTE:
% This reading decomposes the versailles_reparations_clauses kernel into a viability-bounded payment constraint. The punitive_liability_reading instantiates the maximalist extraction constraint (high ε, snare from German seat). The repudiation_reading instantiates the null-obligation constraint (ε≈0, mountain from German seat but snare from Allied seat). The three readings form a constraint family linked by affects_constraints. The limited_responsibility_reading sits structurally between them: it acknowledges the kernel's liability (unlike repudiation) but bounds its quantification (unlike punitive).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(versailles_reparations_clauses__limited_responsibility_reading, institutional, 0.55).
constraint_indexing:directionality_override(versailles_reparations_clauses__limited_responsibility_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
