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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: versailles_reparations_clauses__punitive_liability_reading
 *   human_readable: Treaty of Versailles Punitive Reparations Regime (Punitive Liability Reading)
 *   domain: international_law/political_economy/historical_justice
 *
 * SUMMARY:
 *   The Treaty of Versailles imposes reparations on Germany through Article
 *   231, which affirms German moral and financial responsibility for war
 *   damages. Under the PUNITIVE LIABILITY READING instantiated here, this
 *   clause grounds quasi-unlimited extraction of German wealth to Allied
 *   states. The constraint is authorized by the treaty itself, enforced by
 *   the Reparations Commission and occupation forces, and defended by the
 *   principle that the defeated power bears war's costs. German workers,
 *   taxpayers, and the state are the victims; Allied creditor states and the
 *   international enforcement apparatus are the beneficiaries. This reading
 *   frames the extraction as justified punishment rather than negotiated
 *   settlement. The alternative LIMITED RESPONSIBILITY READING (sibling
 *   constraint) argues reparations must align with German economic capacity
 *   and that Article 231 is legal formality, not moral mandate. The
 *   REPUDIATION READING argues the treaty was imposed under duress and is
 *   illegitimate. This JSON instantiates ONLY the punitive reading as a clean
 *   ε-invariant constraint; the siblings are separate stories linked via
 *   network edges.
 *
 * KEY AGENTS:
 *   - Allied creditor states (France, Britain, USA): institutional agenda-setters, capture the reparations transfer, hold arbitrage exit (can adjust rates or forgive debt)
 *   - German workers and taxpayers: powerless payers, trapped by citizenship and tax jurisdiction, bear extraction through wages and reduced services
 *   - Weimar state: institutional payer, fiscal sovereignty subordinated, trapped by international legal obligation and occupation
 *   - Versailles Reparations Commission: institutional agenda-setter, interprets Article 231 as quasi-unlimited liability, adjusts extraction rates
 *   - German industrial base: powerful payers, constrained exit (can relocate capital but state remains liable), forced to export to service debt
 *   - International legal community: observers, debate the reading's validity and post-war norms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__punitive_liability_reading, 0.82).
domain_priors:suppression_score(versailles_reparations_clauses__punitive_liability_reading, 0.71).
domain_priors:theater_ratio(versailles_reparations_clauses__punitive_liability_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__punitive_liability_reading, snare).
narrative_ontology:human_readable(versailles_reparations_clauses__punitive_liability_reading, "Treaty of Versailles Punitive Reparations Regime (Punitive Liability Reading)").
narrative_ontology:topic_domain(versailles_reparations_clauses__punitive_liability_reading, "international_law/political_economy/historical_justice").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__punitive_liability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__punitive_liability_reading, 'a29f25cb-a8bf-4bf8-95d9-7eb74107153e').
narrative_ontology:cs_kernel_codification('a29f25cb-a8bf-4bf8-95d9-7eb74107153e', fixed_text).
narrative_ontology:cs_authority_grounding('a29f25cb-a8bf-4bf8-95d9-7eb74107153e', extraction).
narrative_ontology:cs_interpretation_layer_present('a29f25cb-a8bf-4bf8-95d9-7eb74107153e').
narrative_ontology:cs_reading_relation('a29f25cb-a8bf-4bf8-95d9-7eb74107153e', versailles_reparations_clauses__limited_responsibility_reading, forecloses).
narrative_ontology:cs_reading_relation('a29f25cb-a8bf-4bf8-95d9-7eb74107153e', versailles_reparations_clauses__repudiation_reading, coexists_with).
narrative_ontology:cs_axiom('a29f25cb-a8bf-4bf8-95d9-7eb74107153e', foundational, germany_unique_war_responsibility).
narrative_ontology:cs_axiom_status(germany_unique_war_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('a29f25cb-a8bf-4bf8-95d9-7eb74107153e', germany_unique_war_responsibility, empirically_contingent).
narrative_ontology:cs_axiom('a29f25cb-a8bf-4bf8-95d9-7eb74107153e', foundational, unlimited_liability_from_guilt_clause).
narrative_ontology:cs_axiom_status(unlimited_liability_from_guilt_clause, overridden).
narrative_ontology:cs_axiom_grounding('a29f25cb-a8bf-4bf8-95d9-7eb74107153e', unlimited_liability_from_guilt_clause, conventional).
narrative_ontology:cs_reference_frame('a29f25cb-a8bf-4bf8-95d9-7eb74107153e', allied_victors_right_to_extract_unlimited_reparations).
narrative_ontology:cs_drift_state('a29f25cb-a8bf-4bf8-95d9-7eb74107153e', id_1933_hitlers_repudiation, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('a29f25cb-a8bf-4bf8-95d9-7eb74107153e', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_workers_taxpayers).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_industrial_base).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, weimar_state_fiscal_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, occupation_forces).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% France, Britain, USA, and associated powers wrote Article 231 and set the reparations schedule. They interpret the clause as grounding Germany's unlimited liability for war damages — a moral judgment encoded in treaty law that justifies extracting the maximum sustainable transfer. They set the Reparations Commission, adjust rates, and enforce compliance through occupation and asset seizure. Their interest is recovering war costs from the defeated power. They maintain the punitive framing as long as extraction flows; they revise downward (Dawes, Young Plans) when extraction becomes politically costly.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states, agenda_setter,
    institutional, generational, arbitrage, continental).

% Bear the extraction through taxation, wage suppression, reduced public services, and currency destabilization. They did not author the treaty, have no exit from German citizenship or tax jurisdiction, and cannot refuse the imposed obligation. The extraction accumulates as debt if payments fall short, extending the trap across generations. Their only option is domestic political pressure to renegotiate or repudiate — which risks Allied military intervention. Experience the constraint as compulsory punishment with no voice in its setting or revision.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_workers_taxpayers, payer,
    powerless, biographical, trapped, national).

% Forced to export production to service reparations; capital is diverted to transfer payments rather than reinvestment. They face destruction of productive capacity if they refuse, and market collapse if others in the payment chain fail. They can lobby for reparations revision but cannot exit the national fiscal regime. Accumulate grievance against extraction; support Nazi repudiation policies as exit mechanism.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_industrial_base, payer,
    powerful, biographical, constrained, national).

% Must collect and transfer the reparations while maintaining minimal state functions. The constraint subordinates fiscal sovereignty: the Reparations Commission and occupation authorities dictate payment schedules, tax rates, and budget priorities. Default or non-payment triggers sanctions, occupation expansion, or territorial loss. The state cannot exit the international legal regime without military force or treaty revision. Trapped between external reparations demands and internal demands for social investment.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, weimar_state_fiscal_capacity, payer,
    institutional, biographical, trapped, national).

% Interprets Article 231 as establishing German guilt and quasi-unlimited liability; adjusts reparations schedules, monitors German compliance, and recommends enforcement actions. The Commission embodies the punitive reading: it takes the guilt framing as axiomatic and operates to extract the maximum transfer the German economy can sustain. Its authority derives from the treaty; its function is to enforce the constraint. Maintains the reading even as economic reality forces downward revisions.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_commission, agenda_setter,
    institutional, generational, arbitrage, continental).

% Enforce reparations compliance through military presence and asset seizure. They suppress resistance, collect production, and prevent default. They are sustained by reparations revenue (occupation costs are often charged to Germany). Their dual role: enforcers of the constraint (agenda-setters) and also recipients of extracted value (payers receive occupation-cost reparations).
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, occupation_forces, agenda_setter,
    institutional, biographical, arbitrage, local).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__punitive_liability_reading, occupation_forces, payer).

% Excluded from meaningful negotiation; the Allies drafted Article 231 unilaterally and presented it as non-negotiable. Objections to the moral liability framing were dismissed. The negotiators could sign or face renewed war — no third option was available. Their exclusion is structural to the constraint: they have no voice in defining their state's liability or the punishment's scope.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_peace_negotiators, excluded,
    powerless, biographical, trapped, national).

% Labour unions, socialist parties, and nationalist movements oppose reparations as intolerable extraction. They demand renegotiation or repudiation. They mount strikes, political campaigns, and civil disobedience. Their resistance is acknowledged but overridden by state-level legal obligations. They represent the voices that would reject the punitive reading if they had a seat at the table.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_resistance_movements, excluded,
    organized, biographical, constrained, national).

% Debates whether Article 231's guilt clause is a valid legal instrument or a punitive imposition; whether it establishes moral responsibility or merely allocates liability; whether unlimited reparations can be enforced without state collapse. Their interpretations feed back into reparations revision efforts (Dawes Plan is partly justified by legal analysis of capacity bounds) and post-war jurisprudence. Their seat is analytical: they measure the constraint's structural coherence but do not enforce it.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, international_legal_community, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states).
narrative_ontology:fixing_cost_class(versailles_reparations_clauses__punitive_liability_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates war costs among belligerent powers by imposing liability on the defeated state; establishes a payment mechanism connecting Allied creditors to German fiscal capacity. Solves the problem of who bears war's financial burden — the punitive reading answers: the defeated power bears all or most of it, grounded in unique guilt.
% TRANSFER_FUNCTION: Moves wealth from German workers, taxpayers, industrial output, and state resources to Allied states (France primarily, Britain and USA secondarily, smaller powers tertiarily) as compensation for war damages. Article 231 frames this as liability rather than negotiated settlement, justifying asymmetric extraction. Transfer mechanisms: direct payments, in-kind deliveries (coal, timber, manufactured goods), occupation-cost charges, asset seizure.
% ABSENT_VOICES: German workers and citizens have no seat at the negotiating table; their interests in fiscal autonomy and survival are not represented. Minority creditors (Belgium, smaller Allied powers) have limited influence over rate-setting. Labour movements and socialist parties argue the extraction perpetuates war and impoverishes the working class — their objections are structurally excluded from the treaty-drafting process. German negotiators could protest but had no power to amend the terms. The entire German population is subjected to liability without consent or representation.
% DISAPPEARANCE_RATIONALE: If Article 231 and the reparations regime vanished overnight, German fiscal capacity would be restored to sovereign control; state budgets would reallocate to social investment and economic growth; France and Britain would forgo transfer payments that are otherwise predictable (and that constitute a large fraction of their post-war revenue); Germany would gain the capacity to rearm without the fiscal constraint of reparations; the balance of power in Europe would shift decisively toward Germany; the post-war settlement would be structurally unstable without the extraction mechanism. Every major actor's material situation and strategic position depends on the constraint persisting.
% FOUNDING_PROBLEM: Allied states suffered massive war damages and sought recovery from the defeated power; Germany's military defeat created the opportunity to impose liability on it rather than negotiate mutual cost-sharing or proportional settlement. The founding problem is creditor-state revenue recovery and the assertion of victors' rights over the defeated enemy. The punitive reading adds a second founding problem: the moral judgment that Germany bears unique responsibility for the war's initiation and all its consequences, justifying unlimited extraction as punishment.
% FOUNDING_PROBLEM_CORROBORATION: Allied negotiators and the Reparations Commission affirm the founding problem: Germany caused the war, suffered defeat, and must pay for damages — the punitive reading embeds this assertion as non-negotiable. Independent historians, German economists, and League of Nations technical assessments attest the founding problem is partially live (damages are real; Allied states did suffer losses) but substantially displaced by extraction beyond rational liability. By the 1924 Dawes Plan, even the Allies acknowledge quasi-unlimited reparations are unworkable and revise downward — a tacit admission the founding problem (actual cost recovery) diverged from the extraction mechanism (punitive quasi-unlimited liability under Article 231). The corroboration is mixed: from OUTSIDE the benefiting parties, economic analysis shows the rates exceed documented damages and reflect extraction rather than cost recovery; this contradicts the Allies' and the Commission's framing of the punitive reading as rational liability.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__punitive_liability_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__punitive_liability_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__punitive_liability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(versailles_reparations_clauses__punitive_liability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__punitive_liability_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.82) because the punitive reading decouples reparations from marginal Allied damage costs and grounds them instead in a moral judgment of German uniqueness. The constraint persists because the rate is set by the victors, not negotiated. Suppression is moderate-high (0.71) because it relies partly on occupation forces and asset seizure (structural suppression) but also on the legitimacy of the Treaty and the guilt clause (normative suppression that could erode). Theater is moderate (0.48) reflecting the genuine damage-recovery function overlaid with explicit punitive framing — the security theater is lower here than in pure predation because the guilt justification is transparent and contested. Accessibility collapse (0.64) reflects that exit options for Germany are genuine but costly: they can repudiate the treaty (war risk), default (sanctions), or comply reluctantly (extraction). Resistance (0.73) is high because German workers and politicians actively resist reparations through strikes, reparations revision demands, and eventual repudiation under Nazi regime. The measurement series track the 1919-1933 interval, showing extractiveness spiking in 1923 (peak extraction during hyperinflation) before moderating after Dawes Plan revision (1924). Suppression requirement rises through 1923 then stabilizes as the Weimar state normalizes extraction collection. Theater ratio rises in 1923 (as real extraction becomes unsustainable and performative compliance substitutes) then stabilizes. All measurements share the same time grid (every metric authored at every interval point) enabling lifecycle analysis.
 *
 * PERSPECTIVAL GAP:
 *   From the Allied agenda-setter perspective, the constraint is justified punishment and warranted extraction of defeated enemy assets — the reading makes moral sense and is legally sound. From the German victim perspective, the same constraint is predatory punishment, economically unsustainable, and a threat to state survival — the reading delegitimizes and destabilizes. The engine computes these divergent perceptions from the structural asymmetry: Allied institutional power with arbitrage exit (they can adjust rates, forgive debt, or redirect reparations); German powerless workers and state with trapped exit (subordinated sovereignty, no negotiating power). A powerful institutional payer with arbitrage exit computes toward rope-beneficiary (subsidy, low d). A powerless trapped payer computes toward snare-victim (high d, high χ). The same treaty produces opposite readings because the seats occupy opposite positions in the extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   Allied creditor states: d ≈ 0.1–0.2 (beneficiary end, institutional power with arbitrage exit, set the rates). Weimar state and German workers: d ≈ 0.85–0.95 (full target end, powerless/trapped exit, fiscal sovereignty subordinated, no negotiating power, forced to bear extraction). German industrial base: d ≈ 0.70–0.75 (toward target, powerful but constrained exit, forced to export capital). The directionality derivation flows from beneficiary declaration (Allied states), victim declaration (German workers and state), power atoms (Allied institutional vs. German powerless/trapped), and exit options (arbitrage vs. trapped). The effective extraction χ for German payers is amplified by scope (continental/global, hard to verify payment capacity, scope modulation increases χ) and by the targeting of the powerless (identity-locked citizens cannot exit nationality, amplifies suppression and χ further). No directionality override is needed; the derivation chain from declared structure yields accurate d values for each seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is creditor-state revenue recovery — a live problem (Allied states did suffer damages). But the constraint's persistence diverges from the founding problem by 1924. The Dawes Plan implicitly admits quasi-unlimited reparations are unworkable and substitutes annual payment schedules with implied capacity bounds. By 1929, the Young Plan further reduces the obligation. By 1933, Hitler repudiates reparations unilaterally. This history reveals mandatrophy: the constraint persists not because the founding problem requires it (creditors themselves revise downward in 1924, admitting capacity bounds), but because the extraction mechanism and the punitive framing have become self-perpetuating. The constraint transforms from coordination solution (cost allocation) to extraction mechanism (predatory liability). The punitive reading explicitly embeds the extraction logic — the reading's survival depends on maintaining the fiction that Article 231 grounds unlimited liability. When that fiction erodes (as it does through the revision plans), the reading itself degrades into theater: enforcement depends increasingly on occupation forces and political pressure rather than on principled adherence to the guilt clause. The theater_ratio rises in 1923 and stabilizes afterward, capturing this degradation. Mandatrophy is resolved via repudiation (Nazi regime) or via sustained revision (Dawes/Young Plans), neither of which preserves the original punitive reading intact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_231_moral_guilt_vs_legal_liability,
    'Does Article 231''s affirmation of German guilt establish moral responsibility for unlimited reparations, or is it a legal formality masking creditor-state rent extraction?',
    'Historiographical analysis of negotiators'' intent (primary documents, drafting records); comparison with post-war jurisprudence on state liability and reparations norms; examination of whether the guilt clause was intended to override economic capacity constraints or merely allocate liability.',
    'If the clause genuinely reflects culpability consensus, the extraction is punishment justified by shared judgment; if it is a cover story for creditor seizure, the constraint is pure snare and the reading''s framing collapses into predation. The distinction determines whether the reading is defensible or delegitimizing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_231_moral_guilt_vs_legal_liability, empirical, 'Whether Article 231 expresses shared moral judgment or masks extraction.').

omega_variable(
    bounded_vs_unlimited_liability_semantics,
    'Do the treaty''s economic and legal provisions actually impose unlimited reparations, or do implicit capacity constraints bound the obligation?',
    'Close reading of the treaty text and the Reparations Commission''s mandate; analysis of whether the Commission interpreted Article 231 as open-ended or capacity-constrained; examination of the Dawes and Young Plans as implicit restatement of bounds.',
    'If truly unlimited, the constraint is structurally snare-like (no exit for the payer); if implicitly bounded by capacity, the constraint is tangled-rope (extraction with coordination function and sustainability constraints). The reading''s core framing depends on this distinction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bounded_vs_unlimited_liability_semantics, conceptual, 'Whether Article 231 reparations are economically unlimited or implicitly bounded.').

omega_variable(
    punitive_reading_vs_limited_reading_foreclosure,
    'Do the punitive and limited-responsibility readings occupy the same normative framework, or are they genuinely irreconcilable?',
    'Analysis of whether both readings can be held within a single legal or moral system without contradiction. If the punitive reading asserts Germany is uniquely blameworthy AND the limited reading asserts guilt does not override economic capacity, both cannot be true under the same principle of justice. If they can both be true under different principles (e.g., retributive vs. restorative justice), they coexist; if they logically exclude each other, one forecloses the other.',
    'If they foreclose, the punitive reading''s authority depends on displacing the limited reading — a high-stakes claim. If they coexist, the Treaty embodies an unresolved contest between two frameworks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(punitive_reading_vs_limited_reading_foreclosure, conceptual, 'Whether punitive and limited-liability readings are logically irreconcilable.').

omega_variable(
    weimar_legitimacy_under_extracted_sovereignty,
    'Can a state maintain legitimacy and democratic function while its fiscal sovereignty is subordinated to external creditors'' extraction regimes?',
    'Observation of Weimar state''s capacity to invest in public goods, maintain democratic institutions, and respond to citizen needs while under reparations extraction. Historical record shows state legitimacy eroded as budgets shrank, public services collapsed, and the Reparations Commission''s authority superseded domestic democratic will.',
    'If the extraction undermines state legitimacy, the constraint is not merely extractive but delegitimizing — it transforms the payer state into a hollow vessel managed by external interests. This amplifies the classification toward snare and increases the constraint''s destabilizing force.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(weimar_legitimacy_under_extracted_sovereignty, empirical, 'Whether sovereignty subordination to creditors erodes state legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__punitive_liability_reading, 1919, 1933).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t1919, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1919, 0.35).
narrative_ontology:measurement(vers_tr_t1921, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1921, 0.42).
narrative_ontology:measurement(vers_tr_t1923, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1923, 0.58).
narrative_ontology:measurement(vers_tr_t1924, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1924, 0.51).
narrative_ontology:measurement(vers_tr_t1928, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1928, 0.48).
narrative_ontology:measurement(vers_tr_t1933, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1933, 0.48).

% Extraction over time
narrative_ontology:measurement(vers_be_t1919, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1919, 0.68).
narrative_ontology:measurement(vers_be_t1921, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1921, 0.75).
narrative_ontology:measurement(vers_be_t1923, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1923, 0.88).
narrative_ontology:measurement(vers_be_t1924, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1924, 0.8).
narrative_ontology:measurement(vers_be_t1928, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1928, 0.79).
narrative_ontology:measurement(vers_be_t1933, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1933, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t1919, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1919, 0.55).
narrative_ontology:measurement(vers_su_t1921, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1921, 0.68).
narrative_ontology:measurement(vers_su_t1923, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1923, 0.79).
narrative_ontology:measurement(vers_su_t1924, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1924, 0.71).
narrative_ontology:measurement(vers_su_t1928, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1928, 0.68).
narrative_ontology:measurement(vers_su_t1933, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1933, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__punitive_liability_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(versailles_reparations_clauses__punitive_liability_reading, 0.18).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses__limited_responsibility_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses__repudiation_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, weimar_hyperinflation_fiscal_collapse).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, league_of_nations_dispute_settlement).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, german_industrial_export_forced_transfer).

% DUAL FORMULATION NOTE:
% The Treaty of Versailles reparations regime decomposes into three structurally distinct constraints, each a reading of the same kernel. PUNITIVE LIABILITY READING (this story): Germany bears unique moral guilt; Article 231 grounds quasi-unlimited extraction. ε=0.82, snare. LIMITED RESPONSIBILITY READING (sibling): Reparations bounded by capacity; Article 231 is formality. ε lower, tangled_rope. REPUDIATION READING (sibling): Treaty imposed under duress; Germany has no binding obligation. ε near zero on binding obligation axis, rope or scaffold depending on exit. The readings differ structurally in their interpretation of the treaty's authority, German guilt's scope, and capacity constraints. Each has its own beneficiary/victim structure and ε. They coexist in historical record: Allies maintain punitive reading, Germans shift to limited then repudiation as extraction proves unsustainable. The constraint family models this unresolved contest via network edges and sibling reading relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
