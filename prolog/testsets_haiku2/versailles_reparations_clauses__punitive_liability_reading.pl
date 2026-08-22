% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__punitive_liability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Versailles Article 231 Punitive Reparations Liability
 *   domain: international relations / legal history / political economy
 *
 * SUMMARY:
 *   The Treaty of Versailles, Article 231 (the 'War Guilt Clause'), asserts
 *   unique German responsibility for the outbreak and conduct of World War I,
 *   grounding Allied reparations claims at approximately 132 billion gold
 *   marks. This constraint describes the punitive-liability reading: Germany
 *   bears moral and financial responsibility for total war costs, and Article
 *   231's legal form justifies quasi-unlimited reparations transfers. The
 *   reading subordinates German fiscal sovereignty to external creditor
 *   claims and treats German workers and middle-class taxpayers as proper
 *   bearers of the obligation. This is one of three structurally distinct
 *   readings of the same kernel (the Treaty's text and authority); the other
 *   readings—limited-responsibility and repudiation—would produce different ε
 *   values and different beneficiary/victim structures from the same formal
 *   instrument. The claim/metric gap is authorial independence: claimed type
 *   (tangled rope: genuine coordination function + asymmetric extraction)
 *   reflects the reading's own framing; the metrics (high extractiveness
 *   0.82, substantial suppression 0.71, rising theater ratio) describe the
 *   constraint's observable operation under this reading.
 *
 * KEY AGENTS:
 *   - Allied creditor states (France, Britain, US): structural beneficiaries and agenda-setters; collect reparations directly; frame the obligation as justice and responsibility
 *   - German working class: powerless payers, trapped in citizenship; bear primary burden through inflation, unemployment, wage suppression
 *   - German middle class: moderate-power payers with constrained exit; experience expropriation through currency collapse and professional disruption
 *   - German treasury: institutional payer; obligated to extract and transfer resources; constrained by treaty enforcement and occupation threat
 *   - German political leadership: doubly trapped; must enforce domestically while facing delegitimization; identity-locked to ratified obligation they did not author
 *   - War-guilt dissenters: excluded historians and jurists; their historiographical revisionism cannot reshape reparations policy
 *   - International legal community: analytical observer; produces competing readings of Article 231's meaning and legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__punitive_liability_reading, 0.82).
domain_priors:suppression_score(versailles_reparations_clauses__punitive_liability_reading, 0.71).
domain_priors:theater_ratio(versailles_reparations_clauses__punitive_liability_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, resistance, 0.77).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__punitive_liability_reading, tangled_rope).
narrative_ontology:human_readable(versailles_reparations_clauses__punitive_liability_reading, "Versailles Article 231 Punitive Reparations Liability").
narrative_ontology:topic_domain(versailles_reparations_clauses__punitive_liability_reading, "international relations / legal history / political economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__punitive_liability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__punitive_liability_reading, '82452508-b3d6-421f-987d-de0ab24f02c4').
narrative_ontology:cs_kernel_codification('82452508-b3d6-421f-987d-de0ab24f02c4', fixed_text).
narrative_ontology:cs_authority_grounding('82452508-b3d6-421f-987d-de0ab24f02c4', extraction).
narrative_ontology:cs_interpretation_layer_present('82452508-b3d6-421f-987d-de0ab24f02c4').
narrative_ontology:cs_reading_relation('82452508-b3d6-421f-987d-de0ab24f02c4', versailles_reparations_clauses__limited_responsibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('82452508-b3d6-421f-987d-de0ab24f02c4', versailles_reparations_clauses__repudiation_reading, coexists_with).
narrative_ontology:cs_axiom('82452508-b3d6-421f-987d-de0ab24f02c4', foundational, unique_german_responsibility_for_war_initiation).
narrative_ontology:cs_axiom_status(unique_german_responsibility_for_war_initiation, holdable).
narrative_ontology:cs_axiom_grounding('82452508-b3d6-421f-987d-de0ab24f02c4', unique_german_responsibility_for_war_initiation, empirically_contingent).
narrative_ontology:cs_axiom('82452508-b3d6-421f-987d-de0ab24f02c4', foundational, reparations_commensurate_with_war_damages).
narrative_ontology:cs_axiom_status(reparations_commensurate_with_war_damages, holdable).
narrative_ontology:cs_axiom_grounding('82452508-b3d6-421f-987d-de0ab24f02c4', reparations_commensurate_with_war_damages, instrumental).
narrative_ontology:cs_reference_frame('82452508-b3d6-421f-987d-de0ab24f02c4', versailles_punitive_justice_framework).
narrative_ontology:cs_drift_state('82452508-b3d6-421f-987d-de0ab24f02c4', post_dawes_plan_1925, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('82452508-b3d6-421f-987d-de0ab24f02c4', '2026-06-11T14:00:00Z').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_working_class).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_middle_class).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_treasury).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__punitive_liability_reading, united_states).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_political_leadership).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__punitive_liability_reading, unique_german_war_guilt).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__punitive_liability_reading, victor_justice_doctrine).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__punitive_liability_reading, collective_state_responsibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% France, Britain, and the United States formulated Article 231 and the reparations schedule at Versailles, claiming unprecedented war damages and positioning Germany as the sole responsible party. They collect reparations directly into government treasuries and use reparations leverage to shape German domestic and foreign policy. Their claim: Germany's unique responsibility for the war's outbreak and conduct justifies the breadth and depth of the claim. The constraint operates entirely through their willingness to enforce it.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states, agenda_setter).

% Bears the primary burden through reduced wages, unemployment from industrial contraction, and inflation driven by reparations payments. Has no vote in the treaty's terms and limited political voice in reparations negotiations. Cannot exit German citizenship; cannot renegotiate the obligation. The constraint subordinates household survival to state debt service. By 1923, hyperinflation concentrates the extraction most severely on wage-earners and the elderly.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_working_class, payer,
    powerless, biographical, trapped, national).

% Savings are liquidated by inflation; professional networks are disrupted by industrial restructuring; emigration is available but costly and marginal relative to the national scope of the claim. They experience the reparations burden as expropriation mediated through currency collapse, not as a transparent transfer. The constraint destroys the class's economic security and political stability.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_middle_class, payer,
    moderate, biographical, constrained, national).

% Is obligated by treaty to transfer resources exceeding 6 percent of national income to foreign creditors, constraining domestic investment in infrastructure, education, and welfare. The obligation is legally enforceable through occupation, asset seizure, and diplomatic sanctions. Default or renegotiation triggers military and economic consequences. The constraint subordinates fiscal policy to creditor requirements.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_treasury, payer,
    institutional, generational, constrained, national).

% Must enforce the reparations obligation domestically (collecting taxes, suppressing resistance, accepting occupation terms) while facing domestic resentment and political delegitimization. Their authority is structurally dependent on ratifying a treaty they did not negotiate; refusing to enforce it invites occupation or constitutional crisis. They are simultaneously obligated creditor-agents and politically trapped payers. The constraint fuses state authority with treaty compliance, making exit from governance impossible without exit from the obligation.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_political_leadership, payer,
    institutional, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__punitive_liability_reading, german_political_leadership, excluded).

% Historians, jurists, and political leaders argue the responsibility claim is selective historiography and that shared war guilt across all combatants was effaced to ground unlimited German liability. They are systematically excluded from the treaty-authoring consensus; their voices are read as self-serving German nationalism rather than legitimate historical revisionism. Academic and political dissent from the guilt premise cannot shape reparations policy. By the late 1920s, revisionism becomes the dominant German political position but cannot alter the constraint's enforcement.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_war_guilt_dissenters, excluded,
    moderate, biographical, constrained, national).

% Interprets Article 231 and the reparations scheme as either a binding legal obligation rooted in unique German culpability or as victor's justice incompatible with rule-of-law principles. Different schools of international law produce different readings; the punitive-liability reading is treated as the authoritative interpretation by creditor states and the League of Nations, while dissenters frame it as normative distortion of legal form. The community is split along lines tracking the constraint's beneficiary/victim structure.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, international_legal_community, observer,
    institutional, generational, analytical, global).

% Designs the Dawes Plan (1924) restructuring reparations around US capital loans to Germany, transforming the constraint into a financial engineering mechanism. Withdraws from direct enforcement but remains the financial architect of the system. The constraint serves US interests as much as French and British, though through different mechanisms (capital circulation rather than direct transfers).
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, united_states, beneficiary,
    institutional, generational, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states).
narrative_ontology:fixing_cost_class(versailles_reparations_clauses__punitive_liability_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns war costs and their distribution with a theory of unique responsibility: the reparations mechanism coordinates how societies that bore war damages can be made whole by the state deemed uniquely responsible for inflicting those damages. The coordination claim: uncompensated damage is destabilizing; responsibility-allocation coordinates compensation and reduces zero-sum conflict by making the responsible party bear the costs.
% TRANSFER_FUNCTION: Moves approximately 2-6 percent of German national income annually to Allied governments (primarily France, with significant portions to Britain, Belgium, and Italy) as compensation for war damages. The flow is: German working class and middle class → German treasury (via taxation and inflation) → Allied governments (via treaty obligation). By 1925, the flow is mediated through US capital markets (Dawes Plan): US loans to Germany → Germany reparations payments to Allies → Allies debt service to US.
% ABSENT_VOICES: Historians and jurists arguing that war guilt was shared across European great powers and that the treaty's framing of unique German responsibility was selective historiography designed to exclude Austria-Hungary, the Ottoman Empire, and mutual responsibility from Italian entry-into-war calculations. Societies in Germany that did not vote for the war (the working class, the left-wing parties, women without suffrage) have no seat in the reparations negotiation, though they bear most of the cost. Non-victorious parties are completely excluded from the allocation decisions.
% DISAPPEARANCE_RATIONALE: If Article 231 and the reparations schedule vanished overnight, German fiscal capacity would immediately redirect toward domestic investment and debt reduction; inflation would likely moderate and the currency would stabilize; domestic political legitimacy would shift as the scapegoat mechanism loses force and anti-war-guilt parties gain credibility; Allied treasuries would lose the primary revenue source for servicing their own war debts; France's security strategy would lose its primary tool for keeping Germany economically and politically subordinate; the geopolitical balance in Europe would reorganize around different incentive structures, potentially enabling German rearmament or hegemonic revisionism sooner than occurred historically.
% FOUNDING_PROBLEM: War damages across France, Belgium, and other Allied societies are uncompensated; these societies face massive reconstruction debt, lost productivity, and civilian casualties. The problem: how to distribute the costs of war fairly and ensure accountability for initiating and conducting it aggressively.
% FOUNDING_PROBLEM_CORROBORATION: Allied governments attest that unique German responsibility for aggressive war requires comprehensive reparations to compensate victimized societies; they cite German war aims, the invasion of Belgium, and unrestricted submarine warfare as evidence of culpability. Post-war historiography (Fritz Fischer, Christopher Clark, Margaret MacMillan, David Fromkin) corroborates that war responsibility was distributed across European rivalries and that Germany's war aims, while aggressive, were not uniquely so—multiple powers initiated escalation paths, and the outbreak was driven by alliance structures and arms-race dynamics rather than by German unilateral aggression. This corroboration comes from outside the benefiting parties and contradicts the founding-problem framing that grounds the punitive-liability reading.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__punitive_liability_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__punitive_liability_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__punitive_liability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high and rising (0.68→0.82 over 1919-1932) because the reparations obligation is decoupled from Germany's capacity to pay and from the actual marginal costs of reconstruction. The obligation is framed as moral responsibility rather than as a negotiated transfer, which allows creditors to claim unlimited entitlement while German resistance to payment is reframed as refusal to accept responsibility. Suppression is substantial (0.71 at interval end) because the constraint persists only through occupation, asset seizure threats, and the delegitimization of dissenting voices. Theater ratio rises from 0.25 to 0.42: early reparations transfers are presented as compensation for actual damages; by the mid-1920s, the Dawes Plan restructures reparations as a mechanism for US capital flows into Germany (American loans are recycled as German reparations payments), transforming the constraint into a theater of international financial choreography. The resistance measurement (0.77) reflects sustained German political opposition, hyperinflation-driven refusal to pay (1923), and the rise of revisionist political movements arguing the treaty's legitimacy itself is suspect. Measurements are authored on a single shared time grid (calendar years 1919-1932); every metric is assessed at every time point.
 *
 * PERSPECTIVAL GAP:
 *   From the Allied beneficiary seat, the constraint is genuine coordination with responsibility-aligned extraction: damages were real, someone must pay, and the responsible party should pay. From the German payer seat, the constraint is enforced extraction hiding behind a responsibility myth: the actual war guilt is shared, the framing is selective, and the obligation persists through coercion, not through normative legitimacy. The engine computes per-seat types from the structural data: the beneficiary seat should compute the constraint as rope (genuine coordination benefit, no victim subordination from their position); the payer seats should compute it as snare (extraction with suppressed alternatives, no real coordination benefit visible from those positions). The gap is not reconciled—it is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The Allied creditor states are the structural beneficiaries (d ≈ 0.1-0.2: they collect transfers, set terms, shape outcomes). The German working class are the structural targets (d ≈ 0.85-0.95: they bear costs, have no exit, no voice in negotiation, and are subordinated to the obligation). The German middle class are secondary targets (d ≈ 0.70-0.80: they pay through inflation and disruption but have some emigration option and some political voice). The German treasury sits near symmetric (d ≈ 0.55-0.65: it is both a payer and an institutional actor with some capacity to shape enforcement, though constrained by the occupation and the treaty's legal form). The German political leadership are trapped in identity-lock (d ≈ 0.75-0.80): they must enforce the obligation while their legitimacy depends on the appearance of renegotiating it; the fusion of state authority with treaty obligation means exit from the constraint requires exit from governance authority itself. This reading does not permit directionality overrides because the structural relationships are stable across the interval and coherent with the derived d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (uncompensated war damages requiring responsibility-aligned compensation) and its status (contested) versus the disappearance verdict (world_rearranges) suggests a mandatrophy candidate: if the founding problem is truly contested and the world would rearrange around its removal, then the constraint is persisting beyond its legitimacy justification. However, the punitive-liability reading treats the founding problem as live because it frames the problem as moral responsibility, not as technical damage compensation—as long as the reading's own framework holds (unique German guilt), the problem persists. The reparations system is not purely theatrical (theater_ratio = 0.42, not >0.65), so it is not a piton. The constraint is best classified as tangled rope because the reading does assert a genuine coordination function (aligning compensation with responsibility) while also describing an asymmetric extraction (German fiscal subordination to external creditors). The theater-ratio rise (0.25→0.42) tracks the shift from compensation-framing to financial-engineering-framing (Dawes Plan), which suggests the constraint is accumulating extractive overhead that the coordination function no longer explains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    war_guilt_responsibility_attribution,
    'Was German responsibility for initiating and conducting World War I uniquely concentrated, or was war guilt distributed across multiple European great powers competing for hegemony?',
    'Historical scholarship and source analysis examining imperial rivalries, arms-race dynamics, alliance structures, and the precipitating decisions of all combatants. Post-war historiography (Fischer, Clark, MacMillan) produces evidence for distributed rather than unique responsibility.',
    'If responsibility is distributed, the punitive-liability reading''s core premise collapses and the reparations obligation reverts to a negotiated transfer rather than a justice claim. The constraint would reclassify from tangled rope (genuine coordination + extraction) to snare (pure extraction hiding behind a false moral narrative).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(war_guilt_responsibility_attribution, empirical, 'Whether Article 231''s war guilt attribution reflects historical reality or victor''s historiography.').

omega_variable(
    capacity_vs_unlimited_obligation,
    'Is there a principled relationship between German capacity to pay and the reparations obligation, or does the obligation remain quasi-unlimited regardless of capacity?',
    'Examination of treaty language, creditor-state renegotiation behavior (Dawes Plan, Young Plan), and default or suspension events. The interval shows multiple renegotiations (1924, 1929) that scaled payments to capacity, suggesting the unlimited framing masks conditional extraction.',
    'If the obligation is genuinely capacity-bounded, the punitive-liability reading misrepresents the constraint as unlimited when it is actually conditional; the theater ratio should rise further as the conditional mechanism is obscured by punitive framing. If unlimited, the reading is accurate but increasingly divorced from actual enforcement practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capacity_vs_unlimited_obligation, empirical, 'Whether the reparations obligation is theoretically unlimited or practically bounded by German capacity.').

omega_variable(
    sibling_reading_foreclosure,
    'Do the three readings of the Versailles kernel logically foreclose one another (one framework cannot hold multiple readings) or coexist as different parties'' live commitments?',
    'Analysis of the logical structures: does accepting punitive liability logically require denying the validity of renegotiation-based capacity-bounding (limited-responsibility)? Or can different actors simultaneously hold different readings while remaining in the same institutional framework (the Treaty)?',
    'If the readings foreclose one another, one must be wrong as a matter of logic, and the constraint''s classification depends on which reading the engine privileges. If they coexist, the constraint is genuinely contestable and the engine should flag it as exhibiting high interpretive variability across seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether alternative readings of the Treaty''s meaning logically exclude one another.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.71) primarily structural (occupation threat, enforcement machinery, legal obligation) or internalized (German acceptance of war guilt as moral fact, identity fusion with treaty compliance)?',
    'Post-treaty German political discourse analysis and behavior patterns: does resistance emerge from external constraints or from internalized guilt acceptance? The rise of Nazi revisionism explicitly rejects internalized guilt, suggesting suppression was more structural than internalized.',
    'If suppression is primarily structural, the constraint''s persistence depends on occupation and threat; removal of occupation would reduce suppression and likely destabilize the arrangement. If internalized, the obligation persists even after external enforcement is removed. By the 1930s, the answer appears to be structural—the constraint''s legitimacy collapses as soon as external enforcement weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether measured suppression reflects external coercion or internalized acceptance of responsibility.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__punitive_liability_reading, 1919, 1932).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t1919, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1919, 0.25).
narrative_ontology:measurement_basis(vers_tr_t1919, observed).
narrative_ontology:measurement(vers_tr_t1921, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1921, 0.3).
narrative_ontology:measurement_basis(vers_tr_t1921, observed).
narrative_ontology:measurement(vers_tr_t1923, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1923, 0.38).
narrative_ontology:measurement_basis(vers_tr_t1923, observed).
narrative_ontology:measurement(vers_tr_t1925, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1925, 0.41).
narrative_ontology:measurement_basis(vers_tr_t1925, observed).
narrative_ontology:measurement(vers_tr_t1929, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1929, 0.42).
narrative_ontology:measurement_basis(vers_tr_t1929, observed).
narrative_ontology:measurement(vers_tr_t1932, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1932, 0.42).
narrative_ontology:measurement_basis(vers_tr_t1932, observed).

% Extraction over time
narrative_ontology:measurement(vers_be_t1919, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1919, 0.68).
narrative_ontology:measurement_basis(vers_be_t1919, observed).
narrative_ontology:measurement(vers_be_t1921, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1921, 0.74).
narrative_ontology:measurement_basis(vers_be_t1921, observed).
narrative_ontology:measurement(vers_be_t1923, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1923, 0.79).
narrative_ontology:measurement_basis(vers_be_t1923, observed).
narrative_ontology:measurement(vers_be_t1925, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1925, 0.81).
narrative_ontology:measurement_basis(vers_be_t1925, observed).
narrative_ontology:measurement(vers_be_t1929, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1929, 0.82).
narrative_ontology:measurement_basis(vers_be_t1929, observed).
narrative_ontology:measurement(vers_be_t1932, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1932, 0.82).
narrative_ontology:measurement_basis(vers_be_t1932, observed).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t1919, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1919, 0.58).
narrative_ontology:measurement_basis(vers_su_t1919, observed).
narrative_ontology:measurement(vers_su_t1921, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1921, 0.63).
narrative_ontology:measurement_basis(vers_su_t1921, observed).
narrative_ontology:measurement(vers_su_t1923, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1923, 0.68).
narrative_ontology:measurement_basis(vers_su_t1923, observed).
narrative_ontology:measurement(vers_su_t1925, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1925, 0.7).
narrative_ontology:measurement_basis(vers_su_t1925, observed).
narrative_ontology:measurement(vers_su_t1929, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1929, 0.71).
narrative_ontology:measurement_basis(vers_su_t1929, observed).
narrative_ontology:measurement(vers_su_t1932, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1932, 0.71).
narrative_ontology:measurement_basis(vers_su_t1932, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__punitive_liability_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(versailles_reparations_clauses__punitive_liability_reading, 0.18).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses__limited_responsibility_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses__repudiation_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, german_hyperinflation_1923).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, french_security_strategy_weimar_containment).

% DUAL FORMULATION NOTE:
% The Versailles Treaty's reparations clauses instantiate three structurally distinct constraints depending on the reading's framing of German responsibility and obligation-bindingness. The punitive-liability reading (this file) treats Article 231 as asserting unique responsibility grounding unlimited claims; the limited-responsibility reading interprets the same text as capped by capacity; the repudiation reading rejects the treaty's legitimacy entirely. Each reading produces different ε and different beneficiary/victim structures. The three constraints are linked via network.affects_constraints because they are alternative instantiations of the same kernel (the Treaty text and authority); the engine's multi-seat per-reading analysis should identify this family structure and route the interpretive contest to the commitment-system layer.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
