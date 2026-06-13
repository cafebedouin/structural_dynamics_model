% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__expansive_secondary_objectives
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ecb_mandate_article_127__expansive_secondary_objectives, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ecb_mandate_article_127__expansive_secondary_objectives
 *   human_readable: ECB Mandate Article 127: Expansive Secondary Objectives Reading
 *   domain: monetary_policy/constitutional_law/institutional_governance
 *
 * SUMMARY:
 *   The European Central Bank operates under Article 127 TFEU, which states
 *   the ECB's primary objective is price stability but the ECB must support
 *   the Union's economic objectives 'without prejudice' to price stability.
 *   This constraint describes one reading of that mandate: the expansive
 *   secondary objectives reading, which interprets 'without prejudice' as
 *   permitting the ECB to operationally balance employment and growth goals
 *   alongside the 2% inflation target when price stability is not under
 *   immediate threat. This reading competes with two sibling readings: the
 *   orthodox price-stability reading (secondary objectives are hortatory
 *   only; the ECB must prioritize 2% inflation above all) and the
 *   climate-incorporation reading (the mandate requires climate risk
 *   integration into all asset purchase and collateral decisions). This story
 *   instantiates ONLY the expansive secondary objectives reading as a clean
 *   constraint. The kernel contest—the underlying disagreement about what
 *   Article 127 actually authorizes—is routed to omega variables and
 *   cs_structure.reading_relations, not folded into this constraint's
 *   classification.
 *
 * KEY AGENTS:
 *   - ECB Governing Council: Sets and administers the interpretation; claims the secondary objectives reading is correct and operationally applies it through asset purchases, collateral rules, and forward guidance.
 *   - Employed workers and labor organizations: Benefit from employment-focused policy; their real wages and labor market tightness depend on whether the ECB weights employment operationally.
 *   - Debtors (households, peripheral states): Benefit from higher inflation and lower real debt service; structurally locked in by debt obligations.
 *   - Savers and creditors: Pay through negative real returns and asset erosion; can exit through capital flight or currency rebalancing but face reputational/strategic costs.
 *   - Core eurozone states (Germany, Netherlands): Pay through reserve dilution and real-value loss; constrained from exiting because eurozone exit is economically and politically catastrophic.
 *   - Orthodox price-stability advocates: Excluded from operational influence; their reading (secondary objectives purely hortatory) does not determine policy.
 *   - European Parliament: Observes and can propose amendment; weak leverage because treaty amendment requires unanimity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__expansive_secondary_objectives, 0.62).
domain_priors:suppression_score(ecb_mandate_article_127__expansive_secondary_objectives, 0.54).
domain_priors:theater_ratio(ecb_mandate_article_127__expansive_secondary_objectives, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, extractiveness, 0.62).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 0.54).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__expansive_secondary_objectives, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__expansive_secondary_objectives, "ECB Mandate Article 127: Expansive Secondary Objectives Reading").
narrative_ontology:topic_domain(ecb_mandate_article_127__expansive_secondary_objectives, "monetary_policy/constitutional_law/institutional_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__expansive_secondary_objectives).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__expansive_secondary_objectives, '373b2b87-b865-4c7a-b117-3c5d4419c5a2').
narrative_ontology:cs_kernel_codification('373b2b87-b865-4c7a-b117-3c5d4419c5a2', formalized).
narrative_ontology:cs_authority_grounding('373b2b87-b865-4c7a-b117-3c5d4419c5a2', lineage).
narrative_ontology:cs_interpretation_layer_present('373b2b87-b865-4c7a-b117-3c5d4419c5a2').
narrative_ontology:cs_reading_relation('373b2b87-b865-4c7a-b117-3c5d4419c5a2', ecb_mandate_article_127__orthodox_price_stability, coexists_with).
narrative_ontology:cs_reading_relation('373b2b87-b865-4c7a-b117-3c5d4419c5a2', ecb_mandate_article_127__climate_incorporation, influences).
narrative_ontology:cs_axiom('373b2b87-b865-4c7a-b117-3c5d4419c5a2', foundational, secondary_objectives_operational_discretion).
narrative_ontology:cs_axiom_status(secondary_objectives_operational_discretion, holdable).
narrative_ontology:cs_axiom_grounding('373b2b87-b865-4c7a-b117-3c5d4419c5a2', secondary_objectives_operational_discretion, conventional).
narrative_ontology:cs_axiom('373b2b87-b865-4c7a-b117-3c5d4419c5a2', foundational, without_prejudice_threshold_discretionary).
narrative_ontology:cs_axiom_status(without_prejudice_threshold_discretionary, holdable).
narrative_ontology:cs_axiom_grounding('373b2b87-b865-4c7a-b117-3c5d4419c5a2', without_prejudice_threshold_discretionary, deontological).
narrative_ontology:cs_reference_frame('373b2b87-b865-4c7a-b117-3c5d4419c5a2', mandate_with_operational_secondary_balance).
narrative_ontology:cs_drift_state('373b2b87-b865-4c7a-b117-3c5d4419c5a2', contemporary_post_crisis_institutionalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('373b2b87-b865-4c7a-b117-3c5d4419c5a2', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, employed_workers).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, debtors_households).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, peripheral_eurozone_states).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, ecb_policy_latitude).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, creditors_savers).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, core_eurozone_states).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, inflation_averse_constituencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, progressive_constituencies).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__expansive_secondary_objectives, mandate_flexibility_doctrine).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__expansive_secondary_objectives, secondary_objectives_operational_status).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Article 127 TFEU as permitting operational pursuit of employment/growth goals when price stability is not threatened. Sets the policy framework that defines 'not threatened' and calibrates the balancing weights. Administers the constraint through forward guidance, asset purchase programs, and collateral rules. Controls the interpretation of 'without prejudice' clause.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, ecb_governing_council, agenda_setter,
    institutional, generational, analytical, continental).

% Benefit from accommodative monetary policy that prioritizes employment levels and labor market tightness. When the ECB weights employment concerns operationally, labor markets strengthen, real wages rise, and unemployment falls. Organized through unions and labor movements; their voice in policy is mediated by these institutions rather than direct representation in the ECB.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, employed_workers, beneficiary,
    organized, biographical, constrained, continental).

% Benefit from lower real debt service when policy tolerates higher inflation in service of employment goals. Mortgage holders, consumer creditors benefit from implicit debt erosion. Their exit—debt avoidance—is impossible for the indebted; they are locked into the constraint structurally.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, debtors_households, beneficiary,
    powerless, biographical, trapped, continental).

% Benefit from the secondary-objectives reading because accommodative policy supports higher growth in labor-intensive, periphery-dominated sectors and reduces the real debt burden of high public debt accumulated during the crisis. Exit would require eurozone departure or managed default—both substantially constrained. Their political economies are locked into the eurozone institutional framework.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, peripheral_eurozone_states, beneficiary,
    moderate, generational, constrained, continental).

% Bear the cost of accommodative policy through negative real returns on savings when inflation rises above the stated 2% target. Banks, pension funds, and savers lose purchasing power. Their exit is real—currency rebalancing, asset allocation away from eurozone exposure, or cross-border capital flight—but exercising it signals distrust and faces reputational costs.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, creditors_savers, payer,
    organized, biographical, mobile, global).

% Pay through the erosion of the real value of their accumulated reserves, the reduction of real interest income on official holdings, and the dilution of the nominal anchor that has been central to their monetary policy tradition. Their exit is to exit the eurozone or to override the ECB's independence—both costly and politically difficult. Germany and the Netherlands are the prototype payers.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, core_eurozone_states, payer,
    institutional, generational, constrained, continental).

% Espouse a narrower reading of the mandate in which secondary objectives are purely hortatory and the ECB must maintain an unwavering commitment to 2% inflation targeting regardless of employment costs. Excluded from the operational policy framework under the expansive reading; their preferred interpretation is not directly represented in ECB decision-making structures. Advocates include parts of the German monetary policy establishment and certain ECB board members.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, orthodox_price_stability_advocates, excluded,
    institutional, generational, trapped, continental).

% Holds democratic accountability for ECB decisions through oversight hearings and conditional treaty-amendment authority. Observes the constraint's operation and can propose changes to the underlying mandate through the Article 127 amendment process (requiring unanimity among all member states). Currently holds weak leverage because amendment is procedurally difficult.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, european_parliament, observer,
    institutional, generational, analytical, continental).

% Politically benefit from employment-focused monetary policy and the narrative that the ECB serves broader social goals beyond price stability. They use the expansive reading as evidence that central banks can and should prioritize distributional justice. Their constraint-dependent benefits are narrative/political (legitimacy for redistributive fiscal policy) as much as material.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, progressive_constituencies, beneficiary,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__expansive_secondary_objectives, progressive_constituencies, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ecb_mandate_article_127__expansive_secondary_objectives, ecb_governing_council).
narrative_ontology:fixing_cost_class(ecb_mandate_article_127__expansive_secondary_objectives, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the eurozone's monetary policy around a flexible but bounded framework: permits the ECB to respond to employment shocks and labor-market slack without sacrificing credibility on the 2% inflation anchor. Solves the coordination problem of how to operate a single currency across heterogeneous labor markets and political preferences without freezing policy into a single numerical target.
% TRANSFER_FUNCTION: Transfers real income from savers/creditors to debtors/workers through the mechanism of higher tolerated inflation and lower real interest rates. Transfers nominal purchasing power from the core to the periphery through higher growth and lower real debt service. Transfers policy discretion from statutory text to the ECB's interpretation of 'not threatened' and 'without prejudice.'
% ABSENT_VOICES: Savers and creditors outside the organized financial sector have no direct voice in the ECB's governing council; they register their preferences only through capital flows and political pressure on member states. The orthodox price-stability tradition (parts of German economic thought, classical monetary theory) is structurally excluded from operational influence under this reading. Future generations who would inherit inflation-eroded currency nominal anchors are not represented.
% DISAPPEARANCE_RATIONALE: If this reading disappeared overnight and the ECB reverted to the orthodox price-stability reading (secondary objectives purely hortatory), monetary policy would immediately tighten, real interest rates would rise, debt service would increase for households and peripheral states, and employment-focused policy coordination would collapse. Peripheral eurozone growth would slow; the legitimacy of the ECB in some member states would strengthen; creditors would see improved real returns. The eurozone's political economy of the 2010s crisis would reorganize around a different monetary constitution.
% FOUNDING_PROBLEM: The 2008–2015 eurozone crisis created massive unemployment, particularly in peripheral states, while orthodox price-stability targeting—inherited from the Bundesbank model—offered no operational relief. The expansive secondary objectives reading was developed to permit the ECB to purchase assets and support employment without technically violating a 2% inflation mandate. It legitimated the policy shift (QE, OMT, eventually PEPP) by reinterpreting Article 127 as permitting this balancing act.
% FOUNDING_PROBLEM_CORROBORATION: The ECB itself attests the founding problem (unemployment and deflation risk post-2008) and claims the secondary objectives reading solved it through accommodative policy. Peripheral member states and labor organizations attest the reading is correct and credit it with recovery. The orthodox price-stability advocates and parts of the German establishment attest the founding problem was overstated and the reading is a dangerous reinterpretation that created new problems (asset bubbles, inequality through financial-asset appreciation). Academic economists are split; external corroboration from independent sources (academic monetary economists outside eurozone central banking, international bodies like IMF and OECD) is mixed—no consensus source external to the constraint's beneficiaries unambiguously validates the founding problem or its status.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__expansive_secondary_objectives, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__expansive_secondary_objectives, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__expansive_secondary_objectives, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(ecb_mandate_article_127__expansive_secondary_objectives, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecb_mandate_article_127__expansive_secondary_objectives_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ecb_mandate_article_127__expansive_secondary_objectives, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ecb_mandate_article_127__expansive_secondary_objectives_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.45 to 0.62 over the interval because the constraint's operation systematically transfers real income from one constituency (savers, core states) to another (workers, debtors, peripheral states) without explicit redistribution mechanisms or proportional consent. The transfer is mediated through monetary policy interpretation and asset purchases that are justified on secondary-objectives grounds. Theater rises to 0.41 (moderate-to-high): the constraint's operation increasingly relies on the narrative that employment focus is 'without prejudice to price stability' even as measured inflation repeatedly approaches or exceeds the 2% target. The gap between the stated rationale (balanced mandate) and the executed outcome (subordination of real interest rate concerns to employment/growth) widens over the interval—measuring theater. Suppression rises from 0.38 to 0.54 because alternative interpretations (the orthodox reading) are systematically excluded from governing council voice, and the constraint is maintained through institutional closure rather than consent from savers/creditors. The measurement series sample a shared time grid so every metric is authored at every examined time point. The slight retreat in theater_ratio and suppression_requirement at t=25 reflects a small tightening cycle driven by post-COVID inflation pressure, which temporarily reduced the gap between stated and executed objectives; this is a cyclical variation, not a trend break.
 *
 * PERSPECTIVAL GAP:
 *   The ECB Governing Council experiences this constraint as genuine coordination that it builds and maintains: the secondary objectives reading permits flexible response to labor-market shocks while preserving the 2% anchor as a long-run nominal target. From this seat, the 'without prejudice' clause is real discretion, not a cover story. From the savers/creditors seat and the core eurozone seat, the same constraint operates as enforced transfer: the ECB unilaterally reinterprets the mandate to permit inflationary monetary policy that benefits debtors and peripheral states at their expense. From the excluded orthodox advocates' seat, the constraint is a hermeneutical overreach—a reinterpretation not authorized by the treaty text. The engine computes per-seat classification from the structural data; the authored claim does not adjudicate the gap. The expansive reading sits at moderate-to-high extractiveness (0.62) and moderate suppression (0.54), which produces a tangled_rope classification at most seats: genuine coordination (employment response to slack) plus asymmetric distribution (gains to workers/debtors, losses to savers/core) held in place by ECB institutional closure and the 'without prejudice' clause's interpretive indeterminacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Workers and debtors sit near the beneficiary end (d near 0.0–0.3) because they collect material gains (employment, real-wage growth, debt erosion) without running the constraint; their exit is substantially constrained (employment is trapped; household debt is identity-locked). Savers and core states sit near the target end (d near 0.7–0.9) because they bear extraction through real-value loss without commanding the constraint's interpretation; savers have mobile exit (capital rebalancing) but exercising it is reputationally costly; core states have constrained/identity-locked exit (eurozone membership is institutionally locked in). The ECB Governing Council sits near analytical (d near 0.5): it administers the constraint and sets the bounds on 'without prejudice,' but it also bears institutional legitimacy costs when the reading is contested and the trading of secondary objectives for inflation tolerance becomes visible. The european_parliament sits at analytical observation (d = 0.5): observer status, no direct material gain or loss, but structural capacity to change the framework through treaty amendment.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unemployment and deflation risk post-2008) is contested: the ECB and peripheral member states attest it was live and serious; the orthodox advocates attest it was overstated and a pretext for reinterpretation. By t=25, measured unemployment in the eurozone has fallen substantially (8.5% to 6.2% over the interval) and the deflation risk has receded; inflation has risen above 2% in 2021–2022. The question of whether the founding problem is dead or live reorganizes the mandatrophy check: if the founding problem is dead (unemployment solved, deflation beaten), why does the secondary-objectives reading persist? The constraint survives the removal of its original justification. The theater_ratio tracks this: as measured inflation rises, the gap between the 'without prejudice' narrative and the executed outcome widens, suggesting the constraint's function has shifted from employment-focused coordination to income transfer justified by increasingly strained reinterpretation. The mandatrophy signal is present: the constraint's original mandate (respond to labor-market slack) has been substantially achieved, yet the permissive interpretation persists and becomes increasingly difficult to square with price stability. This does not automatically reclassify the constraint—the engine computes type from structural metrics, not from mandatrophy status—but it flags the constraint as a candidate for reclassification when the next temporal or structural update arrives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    without_prejudice_scope_ambiguity,
    'Does ''without prejudice to the objective of price stability'' require price stability to be in zero danger before secondary objectives can be operationally weighted, or does it permit balancing whenever price stability is merely ''not threatened''?',
    'Compare the ECB''s stated interpretation of the threshold with the treaty''s legislative history and contemporaneous discussions among member states at the time Article 127 was drafted. A narrow reading (zero danger) would be more restrictive; an expansive reading (not threatened) permits more accommodative policy.',
    'A narrow reading would reduce extractiveness and move the constraint toward rope (genuine coordination with minimal transfer). An expansive reading would increase extractiveness and confirm tangled_rope status (coordination with substantial asymmetric distribution). The entire classification hinges on this interpretive threshold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(without_prejudice_scope_ambiguity, conceptual, 'The scope of ''without prejudice'' clause: does it permit secondary-objectives balancing at a lower threat threshold than pure price-stability protection?').

omega_variable(
    founding_problem_obsolescence,
    'Is the founding problem (unemployment and deflation risk post-2008) still live, dead, or contested? If dead, why does the secondary-objectives reading persist?',
    'Measure eurozone unemployment, inflation expectations, and ECB forward guidance over time. If unemployment has fallen to structural levels, inflation expectations are anchored above 2%, and the ECB continues secondary-objectives balancing, the founding problem is dead but the constraint persists—a mandatrophy signature.',
    'If the founding problem is dead and the constraint persists, the expansive reading becomes harder to justify and may reclassify from tangled_rope (coordination + transfer) to piton (inertial persistence) or snare (pure extraction with a decayed coordination story). A dead founding problem + persistent permissive interpretation is the classic marker of institutional capture.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the constraint''s original justification has been made obsolete by economic recovery.').

omega_variable(
    alternative_reading_foreclosure,
    'Is the expansive secondary-objectives reading genuinely coexistent with the orthodox price-stability reading within a single institutional framework, or does the ECB''s adoption of one reading effectively foreclose the other?',
    'Examine whether the orthodox reading remains a live option for ECB policy (could the Governing Council shift to it without treaty amendment?) or whether institutional precedent, communication, and credibility have locked in the expansive reading such that reversal would be constitutionally catastrophic.',
    'If truly coexistent, neither reading forecloses the other; they coexist as competing institutional readings. If the expansive reading has become locked-in through decades of communication and policy precedent, it functionally forecloses the orthodox reading even absent treaty language, suggesting the kernel contest is not primarily about text interpretation but about path-dependent institutional legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_foreclosure, conceptual, 'Whether the two readings remain genuinely coexistent or whether one has foreclosed the other through institutional commitment.').

omega_variable(
    distributional_intentionality,
    'Is the distributional transfer from savers/core states to debtors/workers a foreseen consequence the ECB accepts as part of the secondary-objectives balancing, or an unintended side effect the ECB does not explicitly acknowledge?',
    'Analyze ECB communications, board member statements, and published analyses of distributional impacts. If the ECB explicitly addresses distributional consequences, the transfer is intentional; if it treats distributional outcomes as externalities or refuses to discuss them, the transfer is tacit.',
    'Intentional transfer suggests the ECB has accepted distributional justice as a secondary objective and the constraint is partially a mechanism for wealth redistribution. Unintentional transfer suggests the ECB does not view itself as a redistributive institution and the asymmetric outcomes are artifacts to be minimized—which would reduce the constraint''s legitimacy as tangled_rope and expose it as snare-like (extraction with an eroding coordination justification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributional_intentionality, empirical, 'Whether the constraint''s distributional consequences are intentional objectives or unacknowledged side effects.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__expansive_secondary_objectives, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t0, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(ecb__tr_t0, observed).
narrative_ontology:measurement(ecb__tr_t3, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 3, 0.3).
narrative_ontology:measurement_basis(ecb__tr_t3, observed).
narrative_ontology:measurement(ecb__tr_t7, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 7, 0.38).
narrative_ontology:measurement_basis(ecb__tr_t7, observed).
narrative_ontology:measurement(ecb__tr_t11, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 11, 0.42).
narrative_ontology:measurement_basis(ecb__tr_t11, observed).
narrative_ontology:measurement(ecb__tr_t18, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 18, 0.44).
narrative_ontology:measurement_basis(ecb__tr_t18, observed).
narrative_ontology:measurement(ecb__tr_t25, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(ecb__tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(ecb__be_t0, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(ecb__be_t0, observed).
narrative_ontology:measurement(ecb__be_t3, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 3, 0.51).
narrative_ontology:measurement_basis(ecb__be_t3, observed).
narrative_ontology:measurement(ecb__be_t7, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 7, 0.58).
narrative_ontology:measurement_basis(ecb__be_t7, observed).
narrative_ontology:measurement(ecb__be_t11, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 11, 0.62).
narrative_ontology:measurement_basis(ecb__be_t11, observed).
narrative_ontology:measurement(ecb__be_t18, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 18, 0.64).
narrative_ontology:measurement_basis(ecb__be_t18, observed).
narrative_ontology:measurement(ecb__be_t25, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(ecb__be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t0, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(ecb__su_t0, observed).
narrative_ontology:measurement(ecb__su_t3, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 3, 0.44).
narrative_ontology:measurement_basis(ecb__su_t3, observed).
narrative_ontology:measurement(ecb__su_t7, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 7, 0.5).
narrative_ontology:measurement_basis(ecb__su_t7, observed).
narrative_ontology:measurement(ecb__su_t11, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 11, 0.54).
narrative_ontology:measurement_basis(ecb__su_t11, observed).
narrative_ontology:measurement(ecb__su_t18, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 18, 0.56).
narrative_ontology:measurement_basis(ecb__su_t18, observed).
narrative_ontology:measurement(ecb__su_t25, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 25, 0.54).
narrative_ontology:measurement_basis(ecb__su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__expansive_secondary_objectives, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ecb_mandate_article_127__expansive_secondary_objectives, 0.12).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127__orthodox_price_stability).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127__climate_incorporation).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, eurozone_sovereign_debt_sustainability).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, german_monetary_orthodoxy).

% DUAL FORMULATION NOTE:
% This constraint is one reading of Article 127 TFEU's mandate. The sibling reading (orthodox_price_stability) instantiates the same treaty text but with a narrower interpretation of 'without prejudice': that clause is read as permitting secondary objectives only when price stability is in zero danger, not merely 'not threatened.' The climate_incorporation reading also reads Article 127 but adds a layer of environmental integration obligation from Article 11 TFEU. All three stories are linked by network.affects_constraints. The distinctions in ε values across readings reflect the different structural consequences of each interpretation: orthodox reading has lower ε (narrower beneficiary set, higher suppression of secondary objectives); expansive reading has higher ε (broader beneficiary set, asymmetric distribution, moderate suppression of alternatives); climate reading has highest ε (adds environmental justice beneficiaries and climate-risk externality costs to core constituencies).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
