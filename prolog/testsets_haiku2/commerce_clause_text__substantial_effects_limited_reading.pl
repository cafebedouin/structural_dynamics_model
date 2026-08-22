% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__substantial_effects_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__substantial_effects_limited_reading, []).

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
 *   constraint_id: commerce_clause_text__substantial_effects_limited_reading
 *   human_readable: Commerce Clause Substantial Effects Test with Jurisdictional Nexus
 *   domain: constitutional/federalism
 *
 * SUMMARY:
 *   The Commerce Clause substantial effects test represents a stable but
 *   contested constitutional reading that permits federal regulation of
 *   intrastate economic activity when it substantially affects interstate
 *   commerce, contingent on a jurisdictional nexus and non-pretextual
 *   economic regulation. This is ONE reading of the commerce_clause_text
 *   kernel. The reading instantiates a middle position between the expansive
 *   federal reading (all economic activity with substantial effects is
 *   interstate commerce) and the originalist narrow reading (only trade
 *   crossing borders qualifies). The substantial effects test creates a
 *   hybrid beneficiary structure: federal agencies and national commercial
 *   interests benefit from expanded regulatory reach; state governments lose
 *   autonomy but gain coordination benefits; originalist constitutional
 *   interests are excluded from the framework's interpretation machinery. The
 *   constraint's mechanism is categorical boundary policing — enforcement
 *   activity focuses on distinguishing legitimate commerce regulation from
 *   pretextual police power regulation.
 *
 * KEY AGENTS:
 *   - Federal regulatory agencies: institutional beneficiary, sets and enforces the jurisdictional nexus and economic/non-economic boundary
 *   - National commercial interests: powerful beneficiary, operates under unified national standards
 *   - State legislatures and police power advocates: powerful and organized payers, lose exclusive regulatory authority over intrastate economic activity
 *   - Supreme Court: institutional agenda-setter, polices the boundary between economic and non-economic regulation
 *   - Originalist judiciary: powerful excluded voice, contests the framework as exceeding constitutional bounds
 *   - Local regulatory autonomy interests: moderate/trapped payers, preempted by federal authority over economic activity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__substantial_effects_limited_reading, 0.58).
domain_priors:suppression_score(commerce_clause_text__substantial_effects_limited_reading, 0.52).
domain_priors:theater_ratio(commerce_clause_text__substantial_effects_limited_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__substantial_effects_limited_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__substantial_effects_limited_reading, "Commerce Clause Substantial Effects Test with Jurisdictional Nexus").
narrative_ontology:topic_domain(commerce_clause_text__substantial_effects_limited_reading, "constitutional/federalism").

domain_priors:requires_active_enforcement(commerce_clause_text__substantial_effects_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__substantial_effects_limited_reading, '861259a4-8d1a-43f0-894c-3564af8394d1').
narrative_ontology:cs_kernel_codification('861259a4-8d1a-43f0-894c-3564af8394d1', fixed_text).
narrative_ontology:cs_authority_grounding('861259a4-8d1a-43f0-894c-3564af8394d1', lineage).
narrative_ontology:cs_interpretation_layer_present('861259a4-8d1a-43f0-894c-3564af8394d1').
narrative_ontology:cs_reading_relation('861259a4-8d1a-43f0-894c-3564af8394d1', commerce_clause_text__expansive_federal_reading, forecloses).
narrative_ontology:cs_reading_relation('861259a4-8d1a-43f0-894c-3564af8394d1', commerce_clause_text__originalist_narrow_reading, coexists_with).
narrative_ontology:cs_axiom('861259a4-8d1a-43f0-894c-3564af8394d1', foundational, substantial_effects_nexus_constrains_federal_power).
narrative_ontology:cs_axiom_status(substantial_effects_nexus_constrains_federal_power, holdable).
narrative_ontology:cs_axiom_grounding('861259a4-8d1a-43f0-894c-3564af8394d1', substantial_effects_nexus_constrains_federal_power, deontological).
narrative_ontology:cs_axiom('861259a4-8d1a-43f0-894c-3564af8394d1', foundational, economic_non_economic_boundary_is_meaningful).
narrative_ontology:cs_axiom_status(economic_non_economic_boundary_is_meaningful, holdable).
narrative_ontology:cs_axiom_grounding('861259a4-8d1a-43f0-894c-3564af8394d1', economic_non_economic_boundary_is_meaningful, empirically_contingent).
narrative_ontology:cs_reference_frame('861259a4-8d1a-43f0-894c-3564af8394d1', new_deal_constitutional_settlement).
narrative_ontology:cs_drift_state('861259a4-8d1a-43f0-894c-3564af8394d1', contemporary_post_2000_regulatory_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('861259a4-8d1a-43f0-894c-3564af8394d1', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, federal_regulatory_agencies).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, national_commercial_interests).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, state_police_power_advocates).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, local_regulatory_autonomy_interests).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, state_legislatures).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, interstate_commerce_participants).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, state_legislatures).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Commerce Clause to justify regulatory jurisdiction over intrastate economic activity with substantial aggregate effects on interstate commerce. Sets the jurisdictional nexus requirement and polices the economic/non-economic boundary to distinguish legitimate commerce regulation from pretextual police power. Controls the framework's application and scope.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, federal_regulatory_agencies, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefit from a unified national regulatory regime that permits federal agencies to regulate interstate commerce effects across state lines without requiring explicit state-by-state coordination. Operate under predictable national standards rather than state-by-state variation. Access to national markets is secured by federal power to regulate the aggregated effects of their activity.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, national_commercial_interests, beneficiary,
    powerful, generational, arbitrage, national).

% Lose exclusive regulatory authority over economic activity within their borders when that activity has substantial interstate commerce effects. They retain police power for public health and safety but face federal preemption when federal agencies claim commerce jurisdiction. Benefit incidentally from national coordination on genuinely national problems but bear the cost of reduced autonomy over local economic regulation.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, state_legislatures, payer,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__substantial_effects_limited_reading, state_legislatures, beneficiary).

% Argue that federal agencies use the substantial effects test to regulate intrastate activity that is fundamentally local in character and falls within the state police power (public health, safety, morals). They claim the jurisdictional nexus requirement is unevenly applied and the economic/non-economic boundary policing is pretextual — federal agencies claim commerce jurisdiction to sidestep constitutional limits on police power.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, state_police_power_advocates, payer,
    organized, generational, constrained, regional).

% Face federal regulatory preemption when they attempt to regulate intrastate economic activity, even if the activity has local primary effects. They lack the institutional power to argue constitutional interpretation in federal court and must operate within the framework set by federal agencies and courts.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, local_regulatory_autonomy_interests, payer,
    moderate, biographical, trapped, local).

% Benefit from a framework that permits federal regulation of intrastate activity with substantial interstate effects, eliminating state-level regulatory arbitrage and fragmentation. Can operate across state lines under unified federal standards rather than navigating a patchwork of state and local regulations.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, interstate_commerce_participants, beneficiary,
    powerful, generational, mobile, global).

% Interprets and applies the substantial effects test, sets the boundaries between economic and non-economic regulation, and enforces the jurisdictional nexus requirement. The Court polices the boundary between legitimate commerce regulation and pretextual police power regulation, but the test itself constrains judicial review capacity.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, supreme_court, agenda_setter,
    institutional, generational, arbitrage, national).

% Legislates under the assumption that the Commerce Clause permits federal regulation of any genuinely economic intrastate activity with substantial interstate effects. Exercises delegated authority to agencies to regulate such activity. Could amend the Constitution to clarify its scope but faces high political costs.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, congress, agenda_setter,
    institutional, generational, arbitrage, national).

% Contests the substantial effects framework as a departure from the original meaning of 'commerce among the several states' and argues the test provides insufficient constraint on federal power. Their voice would reframe the constitutional constraint more narrowly, but they are institutionally minoritized on the current Supreme Court.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, originalist_judiciary, excluded,
    powerful, generational, constrained, national).

% Analyze and critique the substantial effects framework, document its historical development, and assess its legitimacy. Produce the academic record that informs judicial and legislative interpretation, but lack enforcement authority.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_text__substantial_effects_limited_reading, federal_regulatory_agencies).
narrative_ontology:fixing_cost_class(commerce_clause_text__substantial_effects_limited_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Permits federal regulation of genuinely economic intrastate activity with substantial aggregate effects on interstate commerce, establishing a unified national regulatory regime that prevents state-level fragmentation and race-to-the-bottom dynamics in economic regulation.
% TRANSFER_FUNCTION: Moves regulatory authority from state governments to federal agencies and the federal judiciary over economic intrastate activity when that activity has substantial interstate commerce effects. The constraint transfers decision-making power upward; those subject to federal regulation pay the cost of reduced state-level voice and autonomy.
% ABSENT_VOICES: Originalist constitutional interpreters and advocates of robust state police power are institutionally minoritized in the current judicial and executive structure. They attest that the substantial effects framework is pretextual and that federal power has exceeded constitutional bounds, but their reading is not represented in the enforcement machinery (the federal agencies and the current Supreme Court composition).
% DISAPPEARANCE_RATIONALE: If this constraint disappeared — if the substantial effects test were abandoned and replaced with a rule that federal authority does not extend to intrastate activity regardless of interstate effects — federal regulatory agencies would immediately lose jurisdiction over vast domains of currently regulated economic activity (labor, environmental, financial, agricultural markets). State-level regulatory variation would resurface, creating competitive dynamics in regulatory standards; national commerce would fragment into state-by-state compliance frameworks.
% FOUNDING_PROBLEM: The original Constitution granted Congress power to regulate 'commerce among the several states' but left ambiguous whether this extended to intrastate economic activity with interstate effects. The New Deal constitutional crisis (1935–1937) arose when the Supreme Court invalidated federal economic regulations on the grounds that intrastate activity fell outside the commerce power, blocking federal response to the Depression. The substantial effects test emerged as a reframing that permitted federal regulation of intrastate economic activity when it substantially affects interstate commerce, resolving the deadlock and permitting the administrative state.
% FOUNDING_PROBLEM_CORROBORATION: The originalist constitutional tradition (attested by originalist scholars and judges, including Justice Thomas in recent Commerce Clause opinions) contests whether the founding problem is still live — they argue the substantial effects test is itself the problem because it has expanded federal power beyond the original constitutional bounds. The administrative law tradition attests the founding problem is live: the regulatory domains (environmental, financial, labor) depend on the substantial effects framework; if it failed, federal regulatory capacity would collapse. Legislative testimony and scholarly consensus in administrative law support the live-problem reading; originalist jurisprudence supports the overreach reading.
narrative_ontology:disappearance_verdict(commerce_clause_text__substantial_effects_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__substantial_effects_limited_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__substantial_effects_limited_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_text__substantial_effects_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__substantial_effects_limited_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__substantial_effects_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_text__substantial_effects_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_text__substantial_effects_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.58) because the constraint operates as asymmetric authority transfer: federal agencies gain regulatory reach over intrastate activity at the cost of state autonomy, and national commercial interests gain unified standards at the cost of state-level competition. The transfer is real and contested — state governments and originalist constitutional theorists argue federal power has exceeded constitutional bounds. Suppression is moderate (0.52) because the constraint's persistence depends on continued federal judicial and executive commitment to the substantial effects framework; that commitment faces ongoing resistance from originalist judges and federalism advocates. Theater ratio rises gradually over the interval (0.28 to 0.41) because the enforcement focus shifts over time: early periods show more substantive jurisdictional-nexus policing; later periods show more categorical boundary-maintenance (distinguishing economic from non-economic regulation), which involves more performative line-drawing. The measurements use one shared time grid (every metric at every time point) and track three series: base extractiveness rising from 0.42 to 0.58 (federal authority accumulating), theater_ratio rising from 0.28 to 0.41 (boundary policing becoming more theatrical), and suppression_requirement rising from 0.38 to 0.52 (originalist resistance intensifying, requiring more enforcement effort). The plateau after t=30 reflects the current stable state: the framework is entrenched but faces sustained constitutional contestation that requires active suppression (judicial coalition-maintenance, originalist appointments resisted, framework reaffirmed in recent opinions).
 *
 * PERSPECTIVAL GAP:
 *   The federal agency and national commercial seat and the state/originalist seat compute radically different types from the same structural data. From the federal perspective this is rope coordination — solving the genuine collective-action problem of national commerce fragmentation — with moderate overhead. From the state and originalist perspective this is tangled rope or snare — extractive redistribution of authority upward, disguised as commerce regulation but functioning as a power grab. The engine computes both seats from the structural data: federal agencies see the coordination benefit (low d, low effective extraction); state advocates see extraction and preemption (high d, high effective extraction). The measured suppression (0.52) indicates sustained effort to maintain the framework against resistance; the theater ratio (0.41) indicates categorical boundary-maintenance performs significant share of the enforcement. The claim (tangled_rope) reflects the structural asymmetry: genuine coordination function (solving national fragmentation) coupled with asymmetric authority transfer (federal gains, states lose).
 *
 * DIRECTIONALITY LOGIC:
 *   Federal agencies and national commercial interests are structurally beneficiary seats: they gain unified regulatory reach and national market access respectively, with low exit costs (arbitrage-positioned). State governments and police power advocates are target seats: they lose regulatory autonomy over intrastate economic activity, with constrained exit (cannot exit federalism, cannot change constitutional meaning unilaterally). The originalist judiciary is excluded (not in the framework's interpretive machinery, minoritized in current court composition), giving them the strongest voice against the constraint but no enforcement role. Directionality for federal agencies: d ≈ 0.2 (full beneficiary, arbitrage-positioned, no suppression needed). National commercial interests: d ≈ 0.1 (beneficiary, mobile, benefit from unified standards). State legislatures: d ≈ 0.7 (target, lose autonomy, powerful but constrained exit). Police power advocates: d ≈ 0.75 (target, their authority is preempted, organized resistance but constrained exit). Local autonomy interests: d ≈ 0.85 (target, trapped exit, no institutional power). No directionality overrides are needed — the structural derivation captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (permitting federal response to national economic crises) was live at the constraint's origin (1937 New Deal constitutional settlement); it remains contested now (2026). The originalist reading contests whether the problem justifies the solution — they argue the substantial effects test has become a pretextual expansion of federal power. The mandatrophy tension: does the constraint exist to solve the founding problem (unified national commerce regulation), or does it persist as a framework for federal power expansion? The theater ratio (0.41) and the measurement trajectory suggest performative maintenance is present but not dominant — the framework still serves genuine coordination functions (preventing state-level regulatory fragmentation), but increasingly justifies federal regulatory expansion into domains where the interstate effect is attenuated. The constraint is NOT resolved mandatrophy (founding problem is still contested), but shows warning signs: theater ratio above 0.35, suppression_requirement rising, originalist judicial coalition mobilizing. The classification (tangled_rope, not piton) is appropriate because the beneficiaries are concrete and concentrated (federal agencies, national commercial interests) and benefit substantially, not diffusely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_boundary_stability,
    'Is the jurisdictional nexus requirement and the economic/non-economic distinction a stable doctrinal boundary that actually constrains federal expansion, or is the boundary itself so malleable that it provides negligible limit on federal authority?',
    'Systematic analysis of federal agency applications of the standard (EPA, OSHA, FTC, etc.) over a decade: do agencies consistently reject claims to jurisdiction when economic/non-economic boundary or nexus requirement is absent? Or does the boundary migration to accommodate agency preferences?',
    'If the boundary is stable and constraining, the substantial effects framework is a genuine limit on federal power (rope-to-tangled_rope transition justified by real coordination). If the boundary is unstable and migrates to accommodate expansion, the framework is a cover story for federal power accumulation and moves toward snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_stability, empirical, 'Whether the economic/non-economic boundary provides meaningful constraint on federal expansion or is pretextual.').

omega_variable(
    reading_normative_authority,
    'Does the substantial effects framework represent legitimate constitutional evolution (necessary adaptation to an increasingly integrated national economy), or constitutional usurpation (federal branch exceeding its Article I bounds)? Which reading of the Founding generates the correct understanding of commerce power?',
    'Historical reconstruction of the original public meaning of ''commerce among the several states'' at ratification; linguistic and institutional evidence from founding documents and early commerce legislation; assessment of whether the substantial effects test is a reasonable reading or a departure from that meaning.',
    'If the framework is legitimate evolution, the constraint is stable tangled_rope (coordination with acceptable asymmetry). If it is usurpation, the framework itself becomes the constraint''s referent (not a reading of it but a false reading), and the originalist narrow reading is the correct constraint. This is a conceptual question about constitutional authority, not an empirical fact that will resolve.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_normative_authority, conceptual, 'Whether the substantial effects framework represents legitimate constitutional interpretation or exceeds Article I bounds.').

omega_variable(
    federal_agency_capture,
    'Do federal regulatory agencies applying the substantial effects test expand their jurisdiction opportunistically, using the framework to regulate domains where the interstate effect is attenuated or pretextual, rather than applying the constraint in good faith?',
    'Regulatory impact analysis and Supreme Court review patterns: does the Court overturn agency jurisdiction claims, and at what rate? Do agencies expand jurisdiction over time as the framework normalizes? Do cases show agencies invoking substantial effects for domains that seem primarily local?',
    'If agencies expand jurisdiction opportunistically, the theater ratio will continue rising and the suppression requirement will rise (to defend the frame); the constraint will exhibit piton characteristics (framework persists despite atrophied justification). If agencies apply the constraint in good faith, the theater ratio plateaus and suppression is stable (current behavior).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_agency_capture, empirical, 'Whether federal agencies apply the substantial effects framework opportunistically or in good faith.').

omega_variable(
    reading_foreclosure_possibility,
    'Can the substantial_effects_limited_reading coexist logically with the originalist_narrow_reading in the same constitutional framework, or does one reading''s acceptance logically preclude the other?',
    'Examine the axioms and grounding types: if both axioms rest on deontological grounds (constitutional authority and originalist semantics), they may foreclose each other. If this reading grounds authority in instrumental pragmatism (governance of integrated economy) and the originalist reading grounds authority in deontological originalism (fidelity to original meaning), they coexist but disagree on foundational commitments.',
    'If readings foreclose each other, the framework is at risk of rapid destabilization if originalist coalition gains court control (foreclosure triggers winner-takes-all dynamics). If readings coexist, contestation is more stable (both remain live options as judicial coalitions shift).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_possibility, conceptual, 'Whether the substantial effects reading and originalist reading logically foreclose each other or can coexist as live constitutional positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__substantial_effects_limited_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(comm_tr_t5, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement(comm_tr_t10, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement(comm_tr_t15, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(comm_tr_t20, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(comm_tr_t25, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(comm_tr_t30, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(comm_tr_t35, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 35, 0.41).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(comm_be_t5, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 5, 0.46).
narrative_ontology:measurement(comm_be_t10, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(comm_be_t15, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(comm_be_t20, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(comm_be_t25, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 25, 0.57).
narrative_ontology:measurement(comm_be_t30, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(comm_be_t35, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 35, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(comm_su_t5, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(comm_su_t10, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(comm_su_t15, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 15, 0.49).
narrative_ontology:measurement(comm_su_t20, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(comm_su_t25, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 25, 0.51).
narrative_ontology:measurement(comm_su_t30, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(comm_su_t35, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 35, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__substantial_effects_limited_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_text__substantial_effects_limited_reading, 0.12).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text__expansive_federal_reading).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text__originalist_narrow_reading).

% DUAL FORMULATION NOTE:
% The commerce_clause_text kernel decomposes into three structurally distinct constraints, each instantiating a different reading with different ε values, beneficiary structures, and persistence mechanisms. This file (substantial_effects_limited_reading) is the middle-ground reading; it influences both sibling readings by setting the default interpretive framework that originalist and expansive readings must contest against. The expansive reading forecloses this reading's boundary constraints; the originalist reading coexists with this reading as competing constitutional positions. Network edges link all three stories via this field to enable constraint-family analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commerce_clause_text__substantial_effects_limited_reading, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
