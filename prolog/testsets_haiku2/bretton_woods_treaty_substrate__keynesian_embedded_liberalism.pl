% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__keynesian_embedded_liberalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_keynesian_embedded_liberalism, []).

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
 *   constraint_id: bretton_woods_treaty_substrate__keynesian_embedded_liberalism
 *   human_readable: Bretton Woods Capital Controls for Embedded Liberalism
 *   domain: international_political_economy/monetary_institutions
 *
 * SUMMARY:
 *   Bretton Woods (1944) establishes an international monetary order fixed on
 *   gold-dollar convertibility and permits member states to maintain capital
 *   controls. The Keynesian-embedded-liberalism reading interprets this as a
 *   legitimate coordinated constraint on international capital flows,
 *   designed to preserve national policy space for full employment and
 *   welfare-state investment. Governments extract the benefit of policy
 *   autonomy from the arrangement; organized labor benefits from full
 *   employment mandates backed by capital-account protections; international
 *   financial actors and foreign investors bear the cost of blocked exit and
 *   rationed repatriation. This reading emphasizes that capital controls are
 *   NOT violations or deviations but core design features — the mechanism by
 *   which sovereignty over domestic demand management is protected. The
 *   constraint bridges coordination (stable monetary order) and extraction
 *   (from mobile capital to immobile labor and national governments); it
 *   requires active enforcement (government monitoring of cross-border
 *   transactions, licensing controls, exchange-rate defense mechanisms) to
 *   hold.
 *
 * KEY AGENTS:
 *   - national_governments_keynes_bloc: Bretton Woods signatories (UK, US, France, Canada, others) that set and enforce capital controls as their chief policy tool for full-employment autonomy — moderate to institutional power, generational time horizon, collective exit option through treaty amendment but individual exit heavily constrained by interdependence.
 *   - domestic_workers_organized_labor: Unions and wage earners in signatory nations whose bargaining power and social insurance are conditional on capital controls preventing external runs — organized power, generational stakes, constrained exit (they cannot leave their national economies).
 *   - international_financial_actors: Banks, investment trusts, and portfolio managers holding cross-border claims who face systematic barriers to repatriation and arbitrage — powerful but trapped within the control regime, biographical horizon, constrained exit (they cannot move capital freely despite holding it).
 *   - foreign_investors: Direct and portfolio investors from outside the national jurisdiction seeking to enter or exit capital accounts, facing government rationing and approval requirements — moderate power, biographical horizon, most severely trapped (their entry and exit are both controlled).
 *   - us_treasury_hegemon: The US government and its central bank, which anchor the dollar-gold parity and hold asymmetric exemptions and veto over IMF policy — institutional power, generational horizon, high exit option (can redefine the parity unilaterally, as it did in 1971).
 *   - colonial_periphery_nations: Nations outside the initial Bretton Woods consensus, later joining but subordinated in rule-setting — powerless, trapped in the system once joining, facing asymmetric enforcement and higher adjustment requirements.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.38).
domain_priors:suppression_score(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.22).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, extractiveness, 0.38).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "Bretton Woods Capital Controls for Embedded Liberalism").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "international_political_economy/monetary_institutions").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__keynesian_embedded_liberalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, '41a53dc3-cbce-48eb-90eb-d73e86f7fbd4').
narrative_ontology:cs_kernel_codification('41a53dc3-cbce-48eb-90eb-d73e86f7fbd4', formalized).
narrative_ontology:cs_authority_grounding('41a53dc3-cbce-48eb-90eb-d73e86f7fbd4', lineage).
narrative_ontology:cs_interpretation_layer_present('41a53dc3-cbce-48eb-90eb-d73e86f7fbd4').
narrative_ontology:cs_reading_relation('41a53dc3-cbce-48eb-90eb-d73e86f7fbd4', bretton_woods_treaty_substrate__neoliberal_convertibility, coexists_with).
narrative_ontology:cs_reading_relation('41a53dc3-cbce-48eb-90eb-d73e86f7fbd4', bretton_woods_treaty_substrate__sovereignty_defense, coexists_with).
narrative_ontology:cs_axiom('41a53dc3-cbce-48eb-90eb-d73e86f7fbd4', foundational, full_employment_mandate_primacy).
narrative_ontology:cs_axiom_status(full_employment_mandate_primacy, holdable).
narrative_ontology:cs_axiom_grounding('41a53dc3-cbce-48eb-90eb-d73e86f7fbd4', full_employment_mandate_primacy, deontological).
narrative_ontology:cs_axiom('41a53dc3-cbce-48eb-90eb-d73e86f7fbd4', foundational, capital_controls_legitimacy_doctrine).
narrative_ontology:cs_axiom_status(capital_controls_legitimacy_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('41a53dc3-cbce-48eb-90eb-d73e86f7fbd4', capital_controls_legitimacy_doctrine, conventional).
narrative_ontology:cs_reference_frame('41a53dc3-cbce-48eb-90eb-d73e86f7fbd4', embedded_liberalism_framework).
narrative_ontology:cs_drift_state('41a53dc3-cbce-48eb-90eb-d73e86f7fbd4', bretton_woods_final_decade, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('41a53dc3-cbce-48eb-90eb-d73e86f7fbd4', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments_keynes_bloc).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, domestic_workers_organized_labor).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_financial_actors).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, foreign_investors).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, capital_controls_legitimate_policy_tool).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, full_employment_mandate_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bretton Woods signatories (UK, US, France, others) set and enforce capital control provisions as their chief mechanism to pursue full employment and welfare-state investment without external capital flight triggering currency crises. The arrangement legitimates capital controls as normal peacetime policy, not emergency deviation. National central banks administer the controls and benefit from policy autonomy they would lack under pure convertibility. They coordinate through the IMF but maintain unilateral control over their own capital accounts.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments_keynes_bloc, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments_keynes_bloc, beneficiary).

% Full employment becomes a constitutional commitment in Keynes-aligned regimes. Capital controls prevent the external runs that would force deflationary austerity and mass unemployment. Organized labor gains bargaining power and welfare-state expansion as capitals cannot exit en masse. They benefit from the constraint indirectly — they do not administer it, but its persistence is the condition for their political power.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, domestic_workers_organized_labor, beneficiary,
    organized, generational, constrained, national).

% Banks, investment trusts, and portfolio managers holding foreign assets or seeking to move capital across borders face systematic barriers: exchange controls, restrictions on repatriation, licensing requirements for foreign direct investment. They pay through forgone arbitrage opportunities, locked capital, and mandatory local-currency holding periods. Their only exit is political change that dismantles the controls or physical relocation of assets to more liquid markets (but even that is blocked by the controls themselves).
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_financial_actors, payer,
    powerful, biographical, constrained, global).

% Direct investors and portfolio holders cannot freely enter or exit national capital accounts; repatriation of profits is rationed; new foreign direct investment requires government approval under explicit industrial criteria, not market signals. They bear the cost of blocked exit and policy unpredictability — they cannot arbitrage interest-rate differentials or respond to political risk by capital flight. The constraint traps their capital in politically selected allocations.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, foreign_investors, payer,
    moderate, biographical, constrained, global).

% The IMF observes and coordinates the system from a technical vantage: it monitors balance-of-payments positions, administers loan facilities when members face temporary imbalances, and conducts surveillance. It takes no direct stake in whether capital controls persist but documents their operation and their boundary conditions — the constraints on external finance that states can collectively enforce.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, imf_technical_administration, observer,
    institutional, generational, analytical, global).

% The US dollar anchors the fixed-parity system and the US Treasury sets the gold floor. The US benefits from seigniorage on dollar reserves and from a stable postwar order; it also accepts capital-account restrictions on its own citizens and on inflows, though with asymmetric exemptions (US capital to other countries is less constrained than external capital to the US). The US position is simultaneously beneficiary (the parity anchors US exports) and administrator (US officials hold agenda-setter veto over IMF decisions and major policy shifts).
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, us_treasury_hegemon, agenda_setter,
    institutional, generational, arbitrage, global).

% Nations outside the Bretton Woods consensus — often newly independent or communist-bloc — are not signatories and are excluded from the coordination frame. They would benefit from capital controls but do not have a seat at the governance table; their exclusion is built into the institutional structure. Later, developing nations that join face asymmetric treatment: their capital accounts are more heavily scrutinized than those of the core bloc; they are expected to defend their parities through adjustment, not through controls.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, colonial_periphery_nations, excluded,
    powerless, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments_keynes_bloc).
narrative_ontology:fixing_cost_class(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable international monetary order anchored on gold-dollar convertibility at fixed parities, with coordinated capital controls permitting each member state to manage its domestic economy and welfare commitments without external capital flight cascading into currency crises. The coordination solves the interwar problem: uncontrolled capital flows had triggered competitive devaluations and currency instability; Bretton Woods replaces that with mutual commitment to parities, IMF lending facilities, and explicit legitimacy for capital account management.
% TRANSFER_FUNCTION: Moves the cost of international monetary stability from national governments (who would otherwise face runs and forced austerity) to international financial actors and foreign investors (who face rationed exit and repatriation blocked). Also redistributes within nations: the constraint enables domestic redistribution from capital to labor by preventing capital from exiting to avoid taxation or wage pressure. Governments capture the policy space they gain; workers benefit from full employment and welfare expansion; foreign capital bears the suppression of exit.
% ABSENT_VOICES: Developing nations outside the negotiated core are excluded from the rule-setting process; they later encounter the system as fixed law, not negotiable structure. Unorganized small savers and residents of capital-importing nations who would benefit from inflows also lack standing — the constraint is set by state treasuries and central banks, not by broad constituencies. Private-sector capital holders (the international financial community) are systematically outside the decision frame and experience the constraint as imposed upon them.
% DISAPPEARANCE_RATIONALE: If Bretton Woods capital controls disappeared overnight, the system would reorganize radically: fixed parities would collapse within months as capital flight resumed, national governments would lose policy autonomy and face deflationary pressure (as in the 1920s), organized labor's bargaining position would deteriorate, and welfare-state expansion would reverse. The constraint is the entire structural condition for embedded liberalism; its removal unmakes the postwar settlement.
% FOUNDING_PROBLEM: Interwar monetary chaos: competitive devaluations, capital flight cascades, currency crises that forced austerity and mass unemployment, and the resulting political instability that led to fascism and war. Keynes and White designed Bretton Woods to prevent recurrence by allowing capital controls and backing them with IMF facilities so nations could pursue full employment without external runs forcing deflation.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians, IMF technical staff, and governments pursuing full employment (UK, Scandinavia, Japan in the 1950s-60s) attest the founding problem of interwar instability motivated the design. The founding problem remains live as long as policymakers fear uncontrolled capital flows would force deflationary adjustment — a concern supported by post-Bretton Woods currency crises (1970s onwards) and by scholarly analysis of the interwar period. Neoliberal economists contest whether capital flows are actually destabilizing or whether controls are efficient responses, but the founding problem itself (preventing competitive devaluations and forced austerity cascades) is corroborated by the historical record and remains a concern in policy circles.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__keynesian_embedded_liberalism_tests).
:- end_tests(bretton_woods_treaty_substrate__keynesian_embedded_liberalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-low-moderate (0.38 at interval end) because the constraint serves a genuine coordination function (stable exchange rates prevent cascading crises) alongside its redistributive function (blocking capital flight to protect labor power and policy space). The coordination benefit is large and real; the extraction is not incidental but is subordinate to the primary function of stability. Suppression is modest (0.22) because enforcement relies chiefly on bureaucratic administration of controls and on peer pressure within the IMF system, not on violent coercion or mass resistance suppression. Capital controls hold because governments see them as legitimate policy tools for full employment, not because they are brutally enforced — though foreign investors who try to circumvent them face legal sanction. Theater is low-to-moderate (0.15) because the stated purpose (stable exchange rates, full employment autonomy) matches the actual function for most of the interval; theater rises toward 1968-71 as the system begins to strain and governments perform compliance more than achieve it. The measurement series run over 1944-1971 (the formal interval of the Bretton Woods regime) on one shared time grid. Extractiveness rises through the 1950s-60s as capital controls become more refined and binding (blocking the growing eurodollar market, etc.) but falls slightly in 1971 as the system begins to unwind and enforcement becomes performative. Suppression and theater track this; the regime is most functionally integrated around 1962-68.
 *
 * PERSPECTIVAL GAP:
 *   The divergence between the reading_relations shows up here: a neoliberal reading of the same Bretton Woods treaty would classify the capital controls as extraction ON governments and foreign investors by protectionist blocs, and would reframe national governments as payers (role: payer instead of beneficiary), labor as a beneficiary of wage-artificialism paid by capital, and financial actors as excluded/threatened parties. The committer structure differences are STRUCTURAL — not just disagreement but competing framings of who is beneficiary and who pays. The engine computes per-seat; this reading's beneficiary/victim declarations route international finance and foreign investors INTO the victim set and national governments INTO the beneficiary set, whereas the neoliberal reading would reverse those positions by declaring capital markets as beneficiaries and governments as constrained extractors. The same constraint text, opposite directionalities.
 *
 * DIRECTIONALITY LOGIC:
 *   National governments are beneficiaries (role: agenda_setter, secondary: beneficiary) — they set the rules, defend the parities, and capture the policy autonomy. d is near 0.2 (strong beneficiary position). Organized labor is beneficiary (d near 0.15-0.25) — they don't run the system but they benefit from full employment and wage-share expansion it enables. International financial actors are payers (role: payer) — they bear the trapped-capital cost, the blocked-arbitrage cost, the rationed-repatriation cost. Their d is near 0.8-0.85 (near-full target). Foreign investors are also payers (d near 0.75-0.8) — they face approval requirements for entry and exit controls for repatriation. The US Treasury occupies a special position: it is both agenda-setter (controls the parity) and beneficiary (seigniorage on dollar holdings) but also has asymmetric exit (can reset the parity unilaterally). Its d should reflect that asymmetry — probably around 0.25-0.35, reflecting that it benefits substantially but is also somewhat constrained by the need to defend the parity. Colonial periphery nations face the highest d (near 0.9-0.95) — they are trapped, powerless, and encounter the system as imposed from outside.
 *
 * MANDATROPHY ANALYSIS:
 *   Founding problem status is LIVE under this reading: the interwar instability problem remains a live concern for full-employment-committed governments; Bretton Woods's capital controls are the answer to that still-present concern. Disappearance verdict is WORLD_REARRANGES: if the controls disappeared, the full-employment mandate would collapse (as it in fact did after 1971). The constraint is NOT mandatrophic — its founding purpose persists and the arrangement still solves it (for the beneficiary seats). However, by 1968-71 the constraint begins to strain: the Triffin dilemma (gold reserves cannot back both expanding dollar supply and dollar-gold convertibility) creates theater (governments perform compliance more than achieve it). A strict Piton reading would emerge if extractiveness and suppression remained high while the coordination function withered and the system persisted only through institutional inertia — but the measured collapse in 1971 shows the system broke under strain, not that it persisted zombified. So: tangled rope throughout, not piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capital_controls_efficacy_vs_protectionism,
    'Are Bretton Woods capital controls efficient tools for macroeconomic stability and full employment, or are they ultimately protective protectionism that distorts resource allocation and suppresses efficiency gains from open capital markets?',
    'Comparative counterfactual: analyze what would have happened to employment, growth, and price stability in signatory nations if capital controls had been absent (1950-1971) versus maintained. Also: post-1971 liberalization trajectories and their employment/growth outcomes.',
    'If controls were genuinely efficient for stability: the constraint is tangled rope — real coordination benefit (stability) alongside extraction (trapped capital). If controls were ultimately protective protectionism with efficiency costs: the classification might shift toward snare or toward pure rope if the coordination benefit was incidental. The reading''s framing as embedded liberalism hinges on the controls being seen as efficient tools, not political cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_controls_efficacy_vs_protectionism, empirical, 'Whether capital controls were effective macroeconomic stabilizers or ultimately protectionist barriers with efficiency costs').

omega_variable(
    labor_benefit_vs_rent_appropriation,
    'Did organized labor actually benefit from full employment and wage-share expansion under Bretton Woods, or did national governments and capital (in the form of protected domestic firms) appropriate the full-employment rents, leaving labor bearing some of the capital-control costs (in the form of constrained investment and lower consumption choice)?',
    'Distributional analysis: track wage shares, unemployment rates, consumption levels, and investment rates under Bretton Woods (1944-71) vs. pre-Bretton Woods (interwar) vs. post-Bretton Woods (1973+). Examine union density, strike activity, and real wage growth per seat.',
    'If labor genuinely benefited: the beneficiary declaration stands. If capital appropriated most rents: labor''s beneficiary role should shift to partial payer; directionality for labor should increase toward 0.5-0.6. The constraint''s classification as tangled rope (benefiting labor alongside harming foreign capital) depends on labor actually extracting real income gains from the embedded liberalism arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_benefit_vs_rent_appropriation, empirical, 'Whether domestic labor actually benefited from the full-employment mandate under capital controls, or whether national governments and protected capital firms captured the rents').

omega_variable(
    nested_kernel_contest_reading_conflict,
    'This reading (keynesian_embedded_liberalism) treats capital controls as legitimate sovereign tools. The sibling neoliberal_convertibility reading treats them as illegitimate constraints on markets. Are these readings LOGICALLY INCOMPATIBLE (one must be false) or COMPATIBLE DISAGREEMENTS about interpretation (both can be true of different aspects or seats)?',
    'Analyze the committing axioms: embedded liberalism''s foundational claim is ''full_employment_mandate_primacy'' (employment is the primary goal, capital mobility secondary); convertibility''s foundational claim is ''capital_market_efficiency_primacy'' (capital efficiency is the goal, employment secondary). These are INCOMPATIBLE PRIMARY GOALS — they coexist in the postwar political landscape (different nations chose differently) but they are not compatible within a single framework. They COEXIST across different parties but do not coexist within one.',
    'If truly incompatible: the reading_relation should be ''forecloses'' (this reading''s primary axiom rules out the neoliberal reading''s axiom within any single framework). If compatible disagreements: the relation is ''coexists_with'' (different parties held different primary goals, both live). The omega documents whether the contest is a logical contradiction or a POLITICAL CONTEST between live alternative framings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nested_kernel_contest_reading_conflict, conceptual, 'Whether the keynesian_embedded_liberalism and neoliberal_convertibility readings are logically incompatible or coexistent political framings').

omega_variable(
    us_asymmetry_beneficiary_or_agenda_setter,
    'Is the US Treasury position best characterized as beneficiary or as agenda_setter with asymmetric privilege? The US sets the gold parity (sets the rule), collects seigniorage (benefits), but also faces pressure to defend the parity and eventually capitulated to it (constrained). What is the structural role?',
    'Analyze the US''s actual degree of freedom: Could the US unilaterally reset the parity, or was it effectively constrained by the system? Did the US profit more from seigniorage than it paid in opportunity cost (forgone devaluation, capital export restrictions)? By 1971, was the US fleeing the system or defending it?',
    'If the US was primarily beneficiary: directionality should reflect d near 0.15-0.25 (benefits substantially). If primarily agenda_setter with asymmetric exit: the role declaration stands as mixed agenda_setter/beneficiary, but d might be higher (0.3-0.4) reflecting partial constraint. The ambiguity affects whether the US is a coordinating party (with interests aligned to stability) or a dominant power using the system as a tool.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_asymmetry_beneficiary_or_agenda_setter, empirical, 'Whether the US position in Bretton Woods is best characterized as beneficiary (capturing seigniorage gains) or as constrained agenda-setter (forced to defend a parity that became expensive)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bw_keynesian_tr_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1944, 0.08).
narrative_ontology:measurement_basis(bw_keynesian_tr_t1944, observed).
narrative_ontology:measurement(bw_keynesian_tr_t1950, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1950, 0.1).
narrative_ontology:measurement_basis(bw_keynesian_tr_t1950, observed).
narrative_ontology:measurement(bw_keynesian_tr_t1956, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1956, 0.13).
narrative_ontology:measurement_basis(bw_keynesian_tr_t1956, observed).
narrative_ontology:measurement(bw_keynesian_tr_t1962, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1962, 0.16).
narrative_ontology:measurement_basis(bw_keynesian_tr_t1962, observed).
narrative_ontology:measurement(bw_keynesian_tr_t1968, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1968, 0.18).
narrative_ontology:measurement_basis(bw_keynesian_tr_t1968, observed).
narrative_ontology:measurement(bw_keynesian_tr_t1971, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1971, 0.15).
narrative_ontology:measurement_basis(bw_keynesian_tr_t1971, observed).

% Extraction over time
narrative_ontology:measurement(bw_keynesian_be_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1944, 0.25).
narrative_ontology:measurement_basis(bw_keynesian_be_t1944, observed).
narrative_ontology:measurement(bw_keynesian_be_t1950, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1950, 0.32).
narrative_ontology:measurement_basis(bw_keynesian_be_t1950, observed).
narrative_ontology:measurement(bw_keynesian_be_t1956, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1956, 0.38).
narrative_ontology:measurement_basis(bw_keynesian_be_t1956, observed).
narrative_ontology:measurement(bw_keynesian_be_t1962, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1962, 0.41).
narrative_ontology:measurement_basis(bw_keynesian_be_t1962, observed).
narrative_ontology:measurement(bw_keynesian_be_t1968, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1968, 0.42).
narrative_ontology:measurement_basis(bw_keynesian_be_t1968, observed).
narrative_ontology:measurement(bw_keynesian_be_t1971, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1971, 0.38).
narrative_ontology:measurement_basis(bw_keynesian_be_t1971, observed).

% Suppression requirement over time
narrative_ontology:measurement(bw_keynesian_su_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1944, 0.18).
narrative_ontology:measurement_basis(bw_keynesian_su_t1944, observed).
narrative_ontology:measurement(bw_keynesian_su_t1950, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1950, 0.2).
narrative_ontology:measurement_basis(bw_keynesian_su_t1950, observed).
narrative_ontology:measurement(bw_keynesian_su_t1956, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1956, 0.22).
narrative_ontology:measurement_basis(bw_keynesian_su_t1956, observed).
narrative_ontology:measurement(bw_keynesian_su_t1962, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1962, 0.24).
narrative_ontology:measurement_basis(bw_keynesian_su_t1962, observed).
narrative_ontology:measurement(bw_keynesian_su_t1968, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1968, 0.26).
narrative_ontology:measurement_basis(bw_keynesian_su_t1968, observed).
narrative_ontology:measurement(bw_keynesian_su_t1971, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1971, 0.22).
narrative_ontology:measurement_basis(bw_keynesian_su_t1971, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.18).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate__neoliberal_convertibility).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate__sovereignty_defense).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, postwar_welfare_state_demand_management).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_collapse_1971).

% DUAL FORMULATION NOTE:
% This constraint story instantiates ONE reading of the contested Bretton Woods kernel. The sibling readings (neoliberal_convertibility, sovereignty_defense) are separate constraint stories in the same family, each with its own ε, beneficiary/victim structure, and classification. All three readings share the same treaty text and institutional machinery but interpret the constraint's PURPOSE and BENEFICIARY differently. This reading emphasizes how capital controls serve national policy autonomy; neoliberal_convertibility emphasizes how they constrain market efficiency; sovereignty_defense emphasizes how they defend against external discipline. The ε values differ substantially because the readings assess different referents: this reading measures extraction ON capital BY governments; convertibility measures extraction ON capital BY protectionist blocs; sovereignty_defense measures extraction ON governments BY external discipline. Per ε-invariance (DP-001), these are THREE separate constraints, not one constraint with measurement ambiguity. Link via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
